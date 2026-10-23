(* Regression tests for the OCaml/Scala house port mismatches. *)

open Mancer
open Sp
open Engine
open Common
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let hit ?(is_ability = false) ?(is_spell = false) u pid num amount =
  step u (fun () -> Slot.inflict (slot u pid num) (mk_damage ~is_ability ~is_spell amount (ctx (1 - pid))))

let settle u = step u (fun () -> ())
let ten = [ 10; 10; 10; 10; 10 ]

let run_at u pid num =
  step u (fun () -> Option.iter (fun st -> Player.run_slot (player u pid) num st) (slot u pid num).value)

let custom_attacks_are_multi () =
  init_all_houses ();
  List.iter
    (fun key -> check_bool key true (is_multi_card (card_by_key key)))
    [ "darkpriest.DarkHydra.name"; "highpriest.sunpriest"; "dreamweaver.sword" ];
  check_bool "apep" true (is_multi_card (card_by_key "highpriest.Apep"));
  check_bool "goblin" false (is_multi_card (card_by_key "fire.goblin"))

let sirin_after_orc_halves_the_bonus () =
  let u = new_game ~special:Fairy.house () in
  place u 1 "fire.goblin" 0;
  place u 1 "fire.orc" 1;
  play u 0 "fairy.sirin" ~input:(Some 0) ~cost:5;
  settle u;
  check_int "oldest first: (4+2+1)/2" 3 (attack_at u 1 0)

let cloud_does_not_trigger_marine () =
  let u = new_game ~mana:ten ~special:Faerie.house () in
  place u 1 "Marine" 0;
  play u 0 "faerie.cloud" ~input:None ~cost:6;
  check_int "rabble full life" 10 (life_at u 0 1)

let crimson_skips_hydra_player_hits () =
  let u = new_game ~mana:ten ~special:Colors.house () in
  play u 0 "colors.Crimson.name" ~input:(Some 0) ~cost:4;
  place u 0 "darkpriest.DarkHydra.name" 1;
  run_at u 0 1;
  check_int "no crimson on multi" 59 (player_life u 1)

let babi_damages_total_growth () =
  let u = new_game ~mana:[ 1; 1; 1; 1; 1 ] ~special:Highpriest.house () in
  place u 0 "highpriest.Babi" 2;
  place u 1 "fire.goblin" 2;
  step u (fun () -> Houses.incr_mana_all (player u 1).houses_update);
  check_int "sum of growths" 11 (life_at u 1 2)

let manipulator_steals_into_own_slot () =
  let u = new_game ~mana:ten ~special:Kinetician.house () in
  List.iter (fun n -> place u 0 "fire.goblin" n) [ 0; 1; 2; 3; 4 ];
  place u 0 "kinetician.manipulator" 5;
  play u 1 "fire.dragon" ~input:(Some 0) ~cost:8;
  check_bool "stolen" true (occupant u 0 5 = Some "fire.dragon");
  check_bool "left the opponent" true (is_empty u 1 0)

let warlock_uses_printed_cost () =
  let u = new_game ~mana:ten ~special:Darksider.house () in
  place u 0 "darksider.warlock.name" 2;
  let printed = (card_by_key "fire.wave").cost in
  step u (fun () ->
      Player.submit (player u 1)
        (Some
           {
             cmd_player = 1;
             cmd_card = card_by_key "fire.wave";
             cmd_input = None;
             cmd_cost = printed + 1;
             cmd_flag = None;
           }));
  check_int "3 * printed" (30 - (3 * printed)) (life_at u 0 2)

let extremophile_spawns_once () =
  let u = new_game ~mana:ten ~special:Meteomancer.house () in
  play u 0 "meteomancer.extremophile" ~input:(Some 2) ~cost:2 ~flag:(Some (Called_element 0));
  play u 1 "fire.goblin" ~input:(Some 0) ~cost:1;
  check_bool "one spawn" true (occupant u 0 5 = Some "meteomancer.extremophile");
  check_bool "no second" true (is_empty u 0 4)

let ring_buffs_when_played () =
  let u = new_game ~mana:ten ~special:Faerie.house () in
  play u 0 "faerie.ring" ~input:(Some 0) ~cost:4;
  check_int "+2 on the ring just played" 4 (attack_at u 0 0)

let limbo_skips_foreign_on_death () =
  let u = new_game ~special:Limbo.house () in
  place u 0 "highpriest.Ammit" 0;
  place u 0 "fire.goblin" 2;
  let life = player_life u 0 in
  hit u 0 2 100;
  check_int "ammit silent" life (player_life u 0)

let warp_skips_colors_listener () =
  let u = new_duel ~special0:Warp.house ~special1:Colors.house () in
  step u (fun () ->
      Player.submit (player u 0)
        (Some
           {
             cmd_player = 0;
             cmd_card = card_by_key "warp.errant";
             cmd_input = Some 0;
             cmd_cost = 1;
             cmd_flag = None;
           }));
  check_bool "no hide mods from Colors" true ((player u 0).pstate.ps_desc.desc_mods = [])

let hp_set_is_indexed () =
  init_all_houses ();
  let croc = card_by_key "highpriest.Ancientcrocodile" in
  check_int "first alt" 0 croc.card_index;
  check_int "apep" 7 (card_by_key "highpriest.Apep").card_index

let hivemind_does_not_clobber_quantum () =
  let u = new_game ~special:Quantumancer.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "entomologist.hivemind" ~input:(Some 2) ~cost:3;
  check_bool "quantum data kept" true
    (match (player u 0).pstate.ps_data with Quantum_data _ -> true | _ -> false)

let () =
  let open Alcotest in
  run "port_fixes"
    [
      ( "engine",
        [
          test_case "custom multi flag" `Quick custom_attacks_are_multi;
          test_case "sirin source order" `Quick sirin_after_orc_halves_the_bonus;
          test_case "cloud no summon" `Quick cloud_does_not_trigger_marine;
        ] );
      ( "houses",
        [
          test_case "crimson skips hydra" `Quick crimson_skips_hydra_player_hits;
          test_case "babi mana all" `Quick babi_damages_total_growth;
          test_case "manipulator full board" `Quick manipulator_steals_into_own_slot;
          test_case "warlock printed cost" `Quick warlock_uses_printed_cost;
          test_case "extremophile snapshot" `Quick extremophile_spawns_once;
          test_case "ring after submit" `Quick ring_buffs_when_played;
          test_case "limbo on_limbo only" `Quick limbo_skips_foreign_on_death;
          test_case "warp vs colors" `Quick warp_skips_colors_listener;
          test_case "hp_set index" `Quick hp_set_is_indexed;
          test_case "hivemind typed data" `Quick hivemind_does_not_clobber_quantum;
        ] );
    ]
