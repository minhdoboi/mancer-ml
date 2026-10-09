(* Special house Elementalist — port of priv/sp/house/Elementalist.scala. *)

open Sp
open Engine
open Common

let sylph_phase = "sylph phase"
let golem_ref : card option ref = ref None
let sylph_ref : card option ref = ref None
let is_card opt (c : card) = match opt with Some x -> x.id = c.id | None -> false

let incr_sylph_cost : desc_mod =
 fun house cards ->
  if house.phd_house.h_index < 4 then cards
  else
    Array.map
      (fun c ->
        if is_card !sylph_ref c.cd_card then { c with cd_cost = c.cd_cost + 1 } else c)
      cards

let rec unfreeze chain env =
  let p = player_of_env env in
  Player.remove_desc_mod p skip_turn_mod;
  Player.remove_effect p (fun e -> e == unfreeze_chain || e == unfreeze_end);
  Player.add_desc_mod p hide_special_mod;
  Player.add_effect p (On_end_turn, one_time_player_effect (remove_desc_mod hide_special_mod));
  if chain then (
    let op = other_of_env env in
    Player.add_desc_mod op skip_turn_mod;
    Player.add_effect op (On_end_turn, unfreeze_end))

and unfreeze_chain env = unfreeze true env
and unfreeze_end env = unfreeze false env

let freeze : card_effect =
 fun env ->
  let op = other_of_env env in
  Player.add_desc_mod op skip_turn_mod;
  Player.add_effect op (On_end_turn, unfreeze_chain)

let sylph_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  Player.add_desc_mod p incr_sylph_cost;
  Player.add_desc_mod p hide_basic_mod;
  Player.add_transition p (Wait_player (env.env_player, Some sylph_phase));
  Player.add_effect p (On_end_turn, one_time_player_effect (remove_desc_mod hide_basic_mod))

let salamand : card_effect =
 fun env ->
  let p = player_of_env env in
  if p.houses_update.hu_value.(0).mana > (other_of_env env).houses_update.hu_value.(0).mana then (
    focus_slot p.slots_update.slot_updates.(env.env_selected);
    Player.inflict (other_of_env env) (mk_damage ~is_ability:true 4 (ctx_of_env env)))

let aval : card_effect =
 fun env ->
  let p = player_of_env env in
  let x = p.houses_update.hu_value.(3).mana in
  Slots.inflict_creatures (other_of_env env).slots_update (mk_damage ~is_spell:true (2 * x) (ctx_of_env env));
  Player.heal p (2 * x);
  Houses.incr_mana p.houses_update (-x) [ 3 ]

let destroyed_mod (card : card) : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> card.house_index then cards
  else Array.of_list (List.filter (fun cd -> cd.cd_card.id <> card.id) (Array.to_list cards))

let incinerate : card_effect =
 fun env ->
  let destroy (s : slot_update) =
    let card = (Slot.get s).card.c_card in
    s.overridable_destroy ();
    Player.add_desc_mod s.slots.player (destroyed_mod card)
  in
  Option.iter destroy (reduce_filled (player_of_env env).slots_update lowest_life);
  Option.iter destroy (reduce_filled (other_of_env env).slots_update highest_life)

let phoenix_heal : card_effect =
 fun env ->
  let fire = (player_of_env env).houses_update.hu_value.(0).mana in
  if fire > 0 then (
    Slot.heal (player_of_env env).slots_update.slot_updates.(env.env_selected) fire;
    focus_slot (player_of_env env).slots_update.slot_updates.(env.env_selected))

let frost_x (own : house_state array) (opp : house_state array) =
  let opp_min = Array.fold_left (fun m h -> min m h.mana) max_int opp in
  let own_max = Array.fold_left (fun m h -> max m h.mana) min_int own in
  max 0 (own_max - opp_min)

let frost_light : card_effect =
 fun env ->
  let p = player_of_env env in
  let x = frost_x p.houses_update.hu_value (other_of_env env).houses_update.hu_value in
  Player.inflict (other_of_env env) (mk_damage ~is_spell:true x (ctx_of_env env));
  Player.block_slot (other_of_env env) env.env_selected

let stone_golem_attack =
  Attack_slot_state_func
    (fun atk slot -> if Option.is_some (Slot.opposite slot).value then atk + 4 else atk)

let sylph_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death = (fun dead -> Player.remove_desc_mod dead.d_player incr_sylph_cost);
  }

let sg_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d ->
        let me = Reaction.bound_slot r in
        if Option.is_none (Slot.opposite me).value && is_effect d then { d with amount = 0 } else d);
  }

let cards =
  [
    creature ~key:"elementalist.sylph" ~attack:(attack 5) ~life:15 ~description:"elementalist.sylph.description"
      ~new_reaction:sylph_reaction
      ~effects:(effects [ (Direct, sylph_effect) ])
      ();
    spell ~key:"elementalist.freeze" ~description:"elementalist.freeze.description"
      ~effects:(effects [ (Direct, freeze) ])
      ();
    creature ~key:"elementalist.salamander" ~attack:(attack 6) ~life:14
      ~description:"elementalist.salamander.description" ~effects:(effects [ (On_turn, salamand) ]) ();
    spell ~key:"elementalist.avalanche" ~description:"elementalist.avalanche.description"
      ~effects:(effects [ (Direct, aval) ])
      ();
    spell ~key:"elementalist.incineration" ~description:"elementalist.incineration.description"
      ~effects:(effects [ (Direct, incinerate) ])
      ();
    creature ~key:"elementalist.phoenix" ~attack:(attack 9) ~life:20
      ~description:"elementalist.phoenix.description" ~effects:(effects [ (On_turn, phoenix_heal) ]) ();
    creature ~key:"elementalist.golem" ~attack:{ base = Some 7; sources = [ stone_golem_attack ] } ~life:30
      ~description:"elementalist.golem.description" ~new_reaction:sg_reaction ();
    spell ~key:"elementalist.frost" ~description:"elementalist.frost.description"
      ~input_spec:Select_target_slot
      ~effects:(effects [ (Direct, frost_light) ])
      ();
  ]

let () =
  sylph_ref := Some (List.nth cards 0);
  golem_ref := Some (List.nth cards 6)

let house =
  {
    h_key = "elementalist";
    h_name = "elementalist";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 40;
    h_additional_cards = [];
    h_make_listener = (fun () -> default_listener);
    h_data = No_player_data;
  }
