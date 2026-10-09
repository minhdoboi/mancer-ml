(* Special house Soul Reaper — port of priv/sp/house/SoulReaper.scala. Souls in player data power
   damnation, passage, onslaught and furious; Eternal Rage intercepts lethal hits. *)

open Sp
open Engine
open Common

let empty_data = 0
let get_x p = match p.pstate.ps_data with Reaper_data x -> x | _ -> empty_data
let set_x p x = Player.set_data p (Reaper_data x)
let add_x p n = set_x p (get_x p + n)

let damn : card_effect =
 fun env ->
  let slot = (other_of_env env).slots_update.slot_updates.(env.env_selected) in
  let x = get_x (player_of_env env) in
  Slot.inflict slot (mk_damage ~is_spell:true (min 8 (3 + x)) (ctx_of_env env))

let pass : card_effect =
 fun env ->
  let slot = (player_of_env env).slots_update.slot_updates.(env.env_selected) in
  let x = min 12 (get_x (player_of_env env)) in
  Slot.heal slot x;
  List.iter (fun s -> Slot.heal s x) (Slot.filled_adjacent slot)

let eternal_rage : card_effect =
 fun env -> Slot.set_data (player_of_env env).slots_update.slot_updates.(env.env_selected) Eternal

let onslaught : card_effect =
 fun env ->
  let p = player_of_env env in
  Slots.inflict_creatures (other_of_env env).slots_update (mk_damage ~is_spell:true (get_x p) (ctx_of_env env));
  set_x p empty_data

let tribute : card_effect =
 fun env ->
  let slot = (player_of_env env).slots_update.slot_updates.(env.env_selected) in
  match slot.value with
  | None -> ()
  | Some st ->
      Player.heal (player_of_env env) (min st.life 36);
      Slot.destroy slot

let furious : card_effect =
 fun env ->
  let p = player_of_env env in
  let x = get_x p in
  set_x p empty_data;
  Player.inflict (other_of_env env) (mk_damage ~is_spell:true (x + 7) (ctx_of_env env))

let death_sentence : card_effect =
 fun env ->
  let kill (slot : slot_update) =
    slot.overridable_destroy ();
    add_x (player_of_env env) 1
  in
  Slots.foreach (player_of_env env).slots_update kill;
  Slots.foreach (other_of_env env).slots_update kill

let shackle_effect num : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = p.slots_update.slot_updates.(num) in
  let opp = Slot.opposite slot in
  if Option.is_some opp.value then add_x p 1
  else if Option.is_some slot.value then Slot.inflict slot (mk_damage ~is_ability:true 6 (ctx_of_env env))

let shackle : card_effect =
 fun env ->
  let slot = (other_of_env env).slots_update.slot_updates.(env.env_selected) in
  match slot.value with
  | None -> ()
  | Some _ ->
      let st = slot.remove_hook None in
      Warp.bridle { st with attack_sources = attack 0; attack = 0 } slot;
      Player.block_slot (other_of_env env) env.env_selected;
      Player.add_effect (player_of_env env) (On_turn, shackle_effect env.env_selected)

let card_key_of d = Option.map (fun c -> c.key) d.context.ctx_card

let react_dead player_id (owner : player_update) (dead : dead) =
  let from_owner, key =
    match dead.d_damage with
    | Some d -> (d.context.ctx_player = player_id, card_key_of d)
    | None -> (false, None)
  in
  if not (from_owner && key = Some "reaper.onslaught") then (
    add_x owner 1;
    let gained =
      if from_owner && key = Some "reaper.damnation" then (
        add_x owner dead.d_slot.card.c_card.cost;
        1 + dead.d_slot.card.c_card.cost)
      else 1
    in
    emit owner.updater
      (Ev_ability
         {
           kind = "reaper.soul";
           player = player_id;
           num = dead.d_num;
           target = Some (dead.d_player.pid, dead.d_num);
         });
    ignore gained)

let check_eternal (slot : slot_update) (p : player_update) =
  match slot.value with
  | Some st when st.data = Eternal ->
      if st.life > 0 then (
        Slot.update slot (fun s -> { s with life = 0 });
        let id = st.slot_id in
        Player.add_effect p
          ( On_end_turn,
            countdown 2 (fun env ->
                Option.iter Slot.destroy
                  (Slots.find_slot_id (player_of_env env).slots_update id)) ));
      true
  | _ -> false

let wrap_dead (p : player_update) (target : player_update) =
  let old = target.slots_update.on_dead_hook in
  target.slots_update.on_dead_hook <-
    (fun dead ->
      react_dead p.pid p dead;
      old dead)

let reaper_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        wrap_dead p p;
        wrap_dead p (other_player p);
        Array.iter
          (fun (slot : slot_update) ->
            let old_delayed = slot.delayed_destroy_hook in
            slot.delayed_destroy_hook <-
              (fun d -> if not (check_eternal slot p) then old_delayed d);
            let old_over = slot.overridable_destroy in
            slot.overridable_destroy <- (fun () -> if not (check_eternal slot p) then old_over ()))
          p.slots_update.slot_updates);
  }

let cards =
  [
    spell ~key:"reaper.damnation" ~description:"reaper.damnation.description"
      ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, damn) ])
      ();
    spell ~key:"reaper.passage" ~description:"reaper.passage.description" ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, pass) ])
      ();
    spell ~key:"reaper.rage" ~description:"reaper.rage.description" ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, eternal_rage) ])
      ();
    spell ~key:"reaper.onslaught" ~description:"reaper.onslaught.description"
      ~effects:(effects [ (Direct, onslaught) ])
      ();
    spell ~key:"reaper.tribute" ~description:"reaper.tribute.description" ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, tribute) ])
      ();
    spell ~key:"reaper.furious" ~description:"reaper.furious.description"
      ~effects:(effects [ (Direct, furious) ])
      ();
    spell ~key:"reaper.sentence" ~description:"reaper.sentence.description"
      ~effects:(effects [ (Direct, death_sentence) ])
      ();
    spell ~key:"reaper.shackles" ~description:"reaper.shackles.description"
      ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, shackle) ])
      ();
  ]

let house =
  {
    h_key = "reaper";
    h_name = "reaper";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 35;
    h_additional_cards = [];
    h_make_listener = reaper_listener;
    h_data = Reaper_data empty_data;
  }
