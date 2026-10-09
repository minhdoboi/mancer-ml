(* Special house Sower — port of priv/sp/house/Sower.scala. *)

open Sp
open Engine
open Common

let house_id = 36
let is_sower (c : card) = c.house_id = house_id
let monster_ref : card option ref = ref None
let is_monster (c : card) = match !monster_ref with Some m -> c.id = m.id | None -> false

let tangle : card_effect =
 fun env ->
  let mine = own_slot_of_env env in
  let opp = Slot.opposite mine in
  match opp.value with
  | None -> ()
  | Some st ->
      let amount = st.attack in
      Slot.inflict opp (spell_damage env amount);
      if Option.is_some mine.value then Slot.heal mine amount

let devour : card_effect =
 fun env ->
  let p = player_of_env env in
  Slot.destroy p.slots_update.slot_updates.(env.env_selected);
  let factor = Attack_func (fun a -> int_of_float (ceil (float_of_int a *. 2.))) in
  Slots.foreach p.slots_update (fun s -> add_attack s factor);
  Player.add_effect p (On_end_turn, one_time_player_effect (remove_attack factor))

let cards_ref : card list ref = ref []

let pollinate : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = p.slots_update.slot_updates.(env.env_selected) in
  match slot.value with
  | None -> ()
  | Some st ->
      let cost = st.card.c_card.cost in
      let best =
        List.fold_left
          (fun acc (c : card) ->
            if c.is_spell || c.cost > cost - 3 then acc
            else match acc with Some (a : card) when a.cost >= c.cost -> acc | _ -> Some c)
          None !cards_ref
      in
      (match best with
      | Some c ->
          Slot.destroy slot;
          ignore (Slots.summon p.slots_update env.env_selected (make_creature c))
      | None -> ());
      Player.heal p cost

let fiery_flower : card_effect =
 fun env ->
  let op = other_of_env env in
  (match
     Slots.foldl op.slots_update None (fun acc s ->
         match acc with
         | None -> Some s
         | Some a -> if (Slot.get s).life >= (Slot.get a).life then Some s else acc)
   with
  | None -> ()
  | Some slot ->
      Updater.focus env.env_updater env.env_selected env.env_player true;
      let life = (Slot.get slot).life in
      Slot.inflict slot (ability_damage env (int_of_float (ceil (float_of_int life /. 2.)))));
  Houses.incr_mana (player_of_env env).houses_update 1 [ 0 ]

let flower_direct : card_effect =
 fun env ->
  Player.inflict (other_of_env env)
    (mk_damage ~is_ability:true (player_of_env env).houses_update.hu_value.(0).mana (ctx_of_env env))

let monster_attack target d player =
  if single_target_attack target d player then
    Slots.foreach player.slots_update (fun slot ->
        match slot.value with
        | Some st when is_monster st.card.c_card -> Slot.heal slot st.card.c_body.b_life
        | _ -> ())

let healing_attack target d player =
  match target with
  | [] -> ()
  | num :: _ ->
      let amount = damage_and_get num d player in
      Slot.heal player.slots_update.slot_updates.(num) amount

let predator_attack target d player =
  match target with
  | [] -> ()
  | num :: _ -> (
      let slot = (other_player player).slots_update.slot_updates.(num) in
      match slot.value with
      | None -> Player.inflict (other_player player) d
      | Some st ->
          let x = st.card.c_body.b_life - st.life in
          Slot.inflict slot { d with amount = d.amount + x })

let drake_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if
          e.s_player.pid = me.slots.player.pid && e.s_num <> me.num && is_sower e.s_card.c_card
          && match me.value with Some st -> has_flag st run_flag | None -> false
        then (
          Updater.focus me.slots.player.updater me.num me.slots.player.pid true;
          match nearest_empty me.num me.slots.player with
          | None -> ()
          | Some pos -> Slots.add_creature me.slots.slot_updates.(pos) (make_creature e.s_card.c_card)));
  }

let cards =
  [
    spell ~key:"sower.tangling" ~description:"sower.tangling.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, tangle) ])
      ();
    spell ~key:"sower.devouring" ~description:"sower.devouring.description" ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, devour) ])
      ();
    creature ~key:"sower.monster" ~attack:(attack 5) ~life:19 ~description:"sower.monster.description"
      ~run_attack:(custom_run_attack monster_attack) ();
    spell ~key:"sower.pollination" ~description:"sower.pollination.description"
      ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, pollinate) ])
      ();
    creature ~key:"sower.sundew" ~attack:(attack 6) ~life:23 ~description:"sower.sundew.description"
      ~run_attack:(custom_run_attack healing_attack) ();
    creature ~key:"sower.predator" ~attack:(attack 5) ~life:33 ~description:"sower.predator.description"
      ~run_attack:(custom_run_attack predator_attack) ();
    creature ~key:"sower.drake" ~attack:(attack 4) ~life:52 ~description:"sower.drake.description"
      ~new_reaction:drake_reaction ();
    creature ~key:"sower.flower" ~attack:(attack 0) ~life:35 ~description:"sower.flower.description"
      ~effects:(effects [ (On_turn, fiery_flower); (Direct, flower_direct) ])
      ();
  ]

let () =
  monster_ref := Some (List.nth cards 2);
  cards_ref := cards

let house = special_house ~key:"sower" ~id:house_id cards
