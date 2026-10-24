(* Joy house — an earth variant that takes earth's basic slot in standard mode.
   Effects and stats are exactly earth's; only the keys and names differ. *)

open Sp
open Engine
open Common

let young_nymph =
  creature ~key:"joy.nymph" ~attack:(attack 2) ~life:11 ~description:"joy.nymph.description" ()

let () = young_nymph.cost <- 1

let fury_attack (p : player_state) =
  let attacks =
    Array.to_list p.ps_slots
    |> List.filter_map (Option.map (fun st -> st.attack))
    |> List.sort (fun a b -> compare b a)
  in
  match attacks with a :: b :: _ -> a + b | [ a ] -> a | [] -> 0

let fury : card_effect =
 fun env ->
  (* Scala reads player.value: the attacks published at the start of the step,
     before Fury's cost is paid (it matters for mana-attack elementals) *)
  let amount = fury_attack (player_of_env env).pstate in
  Player.inflict (other_of_env env) (spell_damage env amount)

let therapy : card_effect =
 fun env -> Player.heal (player_of_env env) (2 * (get_mana env 3 + env.env_paid_cost))

let nymph : card_effect =
 fun env ->
  let p = player_of_env env in
  let spawn num =
    (* isInSlotRange: the player's open slot list, so blocked slots are skipped *)
    if List.mem num p.pstate.ps_slot_list then
      let slot = p.slots_update.slot_updates.(num) in
      if Option.is_none slot.value then Slots.add_creature slot (make_creature young_nymph)
  in
  spawn (env.env_selected - 1);
  spawn (env.env_selected + 1)

let cards =
  [
    creature ~key:"joy.faery" ~attack:(attack 2) ~life:12 ~description:"joy.faery.description"
      ~effects:(effects [ (On_turn, focus (heal 3)) ])
      ();
    spell ~key:"joy.care" ~description:"joy.care.description" ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, heal 8); (Direct, heal_creature 8) ])
      ();
    creature ~key:"joy.cat" ~attack:(attack 1) ~life:22 ~description:"joy.cat.description"
      ~run_attack:multi_run_attack ();
    spell ~key:"joy.bond" ~description:"joy.bond.description"
      ~desc_args:(fun state pid -> [ string_of_int (2 * state.players.(pid).ps_houses.(3).mana) ])
      ~effects:(effects [ (Direct, therapy) ])
      ();
    creature ~key:"joy.kitsune" ~attack:(attack 1) ~life:13 ~description:"joy.kitsune.description"
      ~new_reaction:(mana_growth_reaction ~owner:true ~amount:2 ~house_indexes:[ 3 ])
      ();
    spell ~key:"joy.eruption" ~description:"joy.eruption.description"
      ~desc_args:(fun state pid -> [ string_of_int (fury_attack state.players.(pid)) ])
      ~effects:(effects [ (Direct, fury) ])
      ();
    creature ~key:"joy.nymphea" ~attack:(attack 4) ~life:21 ~description:"joy.nymphea.description"
      ~effects:(effects [ (Direct, nymph) ])
      ();
    creature ~key:"joy.faune" ~attack:(attack 6) ~life:26 ~description:"joy.faune.description"
      ~effects:(effects [ (On_turn, focus (heal_creature 4)) ])
      ();
    spell ~key:"joy.burst" ~description:"joy.burst.description"
      ~effects:(effects [ (Direct, mass_damage ~is_spell:true 25) ])
      ();
    creature ~key:"joy.sunstonegolem" ~attack:(mana_attack 3) ~life:49
      ~description:"joy.sunstonegolem.description"
      ~effects:(effects [ (On_turn, add_mana 1 [ 3 ]) ])
      ();
    creature ~key:"joy.attachment" ~attack:(attack 3) ~life:35 ~description:"joy.attachment.description"
      ~effects:(effects [ (On_turn, focus (heal 3)); (On_turn, heal_creatures 3) ])
      ();
    creature ~key:"joy.dragon" ~attack:(attack 3) ~life:40 ~description:"joy.dragon.description"
      ~run_attack:multi_run_attack
      ~effects:(effects [ (On_turn, heal_creature 4) ])
      ();
  ]

let house =
  {
    h_key = "joy";
    h_name = "joy";
    h_cards = cards;
    h_index = 3;
    h_effects = [];
    h_house_id = 4;
    h_additional_cards = [ young_nymph ];
    h_make_listener = (fun () -> default_listener);
    h_data = No_player_data;
  }
