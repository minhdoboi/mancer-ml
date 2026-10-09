(* Earth house — port of priv/sp/house/Earth.scala. *)

open Sp
open Engine
open Common

let forest_spider =
  creature ~key:"earth.forestspider" ~attack:(attack 2) ~life:11 ~description:"earth.forestspider.description"
    ()

let () = forest_spider.cost <- 1

let fury : card_effect =
 fun env ->
  (* Scala reads player.value: the attacks published at the start of the step,
     before Fury's cost is paid (it matters for mana-attack elementals) *)
  let attacks =
    Array.to_list (player_of_env env).pstate.ps_slots
    |> List.filter_map (Option.map (fun st -> st.attack))
    |> List.sort (fun a b -> compare b a)
  in
  let amount = match attacks with a :: b :: _ -> a + b | [ a ] -> a | [] -> 0 in
  Player.inflict (other_of_env env) (mk_damage ~is_spell:true amount (ctx_of_env env))

let therapy : card_effect =
 fun env -> Player.heal (player_of_env env) (2 * (get_mana env 3 + env.env_paid_cost))

let spider : card_effect =
 fun env ->
  let p = player_of_env env in
  let spawn num =
    (* isInSlotRange: the player's open slot list, so blocked slots are skipped *)
    if List.mem num p.pstate.ps_slot_list then
      let slot = p.slots_update.slot_updates.(num) in
      if Option.is_none slot.value then Slots.add_creature slot (make_creature forest_spider)
  in
  spawn (env.env_selected - 1);
  spawn (env.env_selected + 1)

let cards =
  [
    creature ~key:"earth.elven" ~attack:(attack 2) ~life:12 ~description:"earth.elven.description"
      ~effects:(effects [ (On_turn, focus (heal 3)) ])
      ();
    spell ~key:"earth.care" ~description:"earth.care.description" ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, heal 8); (Direct, heal_creature 8) ])
      ();
    creature ~key:"earth.sprite" ~attack:(attack 1) ~life:22 ~description:"earth.sprite.description"
      ~run_attack:multi_run_attack ();
    spell ~key:"earth.therapy" ~description:"earth.therapy.description"
      ~effects:(effects [ (Direct, therapy) ])
      ();
    creature ~key:"earth.hermit" ~attack:(attack 1) ~life:13 ~description:"earth.hermit.description"
      ~new_reaction:(mana_growth_reaction ~owner:true ~amount:2 ~house_indexes:[ 3 ])
      ();
    spell ~key:"earth.fury" ~description:"earth.fury.description" ~effects:(effects [ (Direct, fury) ]) ();
    creature ~key:"earth.hugespider" ~attack:(attack 4) ~life:21 ~description:"earth.hugespider.description"
      ~effects:(effects [ (Direct, spider) ])
      ();
    creature ~key:"earth.troll" ~attack:(attack 6) ~life:26 ~description:"earth.troll.description"
      ~effects:(effects [ (On_turn, focus (heal_creature 4)) ])
      ();
    spell ~key:"earth.shower" ~description:"earth.shower.description"
      ~effects:(effects [ (Direct, mass_damage ~is_spell:true 25) ])
      ();
    creature ~key:"earth.elemental" ~attack:(mana_attack 3) ~life:49
      ~description:"earth.elemental.description"
      ~effects:(effects [ (On_turn, add_mana 1 [ 3 ]) ])
      ();
    creature ~key:"earth.healer" ~attack:(attack 3) ~life:35 ~description:"earth.healer.description"
      ~effects:(effects [ (On_turn, focus (heal 3)); (On_turn, heal_creatures 3) ])
      ();
    creature ~key:"earth.hydra" ~attack:(attack 3) ~life:40 ~description:"earth.hydra.description"
      ~run_attack:multi_run_attack
      ~effects:(effects [ (On_turn, heal_creature 4) ])
      ();
  ]

let house =
  {
    h_key = "earth";
    h_name = "earth";
    h_cards = cards;
    h_index = 3;
    h_effects = [];
    h_house_id = 4;
    h_additional_cards = [ forest_spider ];
    h_make_listener = (fun () -> default_listener);
    h_data = No_player_data;
  }
