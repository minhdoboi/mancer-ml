(* Fear house: an air variant that takes air's basic slot in standard mode. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)

let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let hit u pid num amount =
  step u (fun () -> Slot.inflict (slot u pid num) (mk_damage amount (ctx (1 - pid))))

let settle u = step u (fun () -> ())
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let growth u pid i = (player u pid).houses_update.hu_value.(i).growth

(* ----------------------------------------------------------------- modes *)

let standard_deals_fear () =
  init_all_houses ();
  let p = Shuffle.create_player ~first:true ~special:Troopers.house Catalog.standard_basics in
  check_string "standard house 2 is fear" "fear" p.desc.pd_houses.(2).phd_house.h_key

let classic_deals_air () =
  init_all_houses ();
  let p = Shuffle.create_player ~first:true ~special:Troopers.house Catalog.classic_basics in
  check_string "classic house 2 is air" "air" p.desc.pd_houses.(2).phd_house.h_key

(* ----------------------------------------------------------------- cards *)

let ghost_boosts_spells () =
  let u = new_game ~mana:[ 10; 4; 4; 4 ] ~house:Fear.house () in
  place u 1 "anger.boar" 1;
  place u 0 "fear.ghost" 0;
  play u 0 "anger.wave" ~input:None ~cost:6;
  check_int "8 + 1 spell damage" 17 (life_at u 1 1)

let harpy_five_fear () =
  let u = new_game ~mana:[ 4; 4; 6; 4 ] ~house:Fear.house () in
  place u 0 "fear.harpy" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "5 damage at 6 fear" (60 - 5) (player_life u 1)

let harpy_below_five () =
  let u = new_game ~mana:[ 4; 4; 4; 4 ] ~house:Fear.house () in
  place u 0 "fear.harpy" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "no damage below 5 fear" 60 (player_life u 1)

let terror_shock_hits_both () =
  let u = new_game ~mana:[ 4; 4; 10; 4 ] ~house:Fear.house () in
  place u 1 "anger.boar" 1;
  play u 0 "fear.strike" ~input:(Some 1) ~cost:3;
  check_int "creature 26 - 6" 20 (life_at u 1 1);
  check_int "opponent 60 - 6" 54 (player_life u 1)

(* The sage reads the joy house, which sits in slot 3 in standard mode. *)
let crow_sage_heals_joy () =
  let u = new_game ~mana:[ 4; 4; 4; 12 ] ~house:Fear.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "fear.sage" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "capped at 10" 50 (player_life u 0)

let crow_sage_reads_joy () =
  let u = new_game ~mana:[ 20; 20; 20; 3 ] ~house:Fear.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "fear.sage" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "joy mana 3" 43 (player_life u 0)

let freezing_fog_burns () =
  let u = new_game ~house:Fear.house () in
  place u 0 "fear.fog" 0;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "opponent 60 - 4" 56 (player_life u 1)

let mimic_rebirths () =
  let u = new_game ~mana:[ 10; 4; 4; 4 ] ~house:Fear.house () in
  place u 0 "fear.mimic" 0;
  hit u 0 0 100;
  check_string "same slot reborn" "fear.mimic" (Option.get (occupant u 0 0))

let mimic_does_not_rebirth_when_destroyed () =
  let u = new_game ~mana:[ 10; 4; 4; 4 ] ~house:Fear.house () in
  place u 0 "fear.mimic" 0;
  step u (fun () -> Slot.destroy (slot u 0 0));
  check_bool "destroyed, not reborn" true (is_empty u 0 0)

let ghost_invasion_hits_board () =
  let u = new_game ~mana:[ 4; 4; 10; 4 ] ~house:Fear.house () in
  place u 1 "anger.boar" 1;
  place u 1 "anger.boar" 3;
  play u 0 "fear.invasion" ~input:None ~cost:8;
  check_int "opponent 60 - 9" 51 (player_life u 1);
  check_int "boar 26 - 9" 17 (life_at u 1 1);
  check_int "second boar 26 - 9" 17 (life_at u 1 3)

let dark_clouds_sweep () =
  let u = new_game ~house:Fear.house () in
  place u 0 "fear.clouds" 0;
  place u 1 "anger.boar" 1;
  place u 1 "anger.boar" 4;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "opponent 60 - 4" 56 (player_life u 1);
  check_int "boar 26 - 4" 22 (life_at u 1 1);
  check_int "second boar 26 - 4" 22 (life_at u 1 4)

let fear_of_fear_destroys () =
  let u = new_game ~mana:[ 4; 4; 12; 4 ] ~house:Fear.house () in
  place u 1 "anger.boar" 1;
  play u 0 "fear.fear" ~input:(Some 1) ~cost:10;
  check_bool "target destroyed" true (is_empty u 1 1)

let medusa_power_and_growth () =
  let u = new_game ~mana:[ 4; 4; 7; 4 ] ~house:Fear.house () in
  let before = growth u 0 2 in
  place u 0 "fear.medusa" 0;
  check_int "fear growth +1" (before + 1) (growth u 0 2);
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  settle u;
  check_int "8 on summon" (60 - 8) (player_life u 1);
  check_int "attack = fear mana" 7 (attack_at u 0 0)

let malediction_kills_opposite () =
  let u = new_game ~house:Fear.house () in
  place u 1 "anger.boar" 1;
  place u 0 "fear.malediction" 1;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "boar 26 - 15" 11 (life_at u 1 1)

let () =
  init_all_houses ();
  Alcotest.run "fear"
    [
      ( "mode",
        [
          Alcotest.test_case "standard deals fear" `Quick standard_deals_fear;
          Alcotest.test_case "classic deals air" `Quick classic_deals_air;
        ] );
      ( "cards",
        [
          Alcotest.test_case "ghost" `Quick ghost_boosts_spells;
          Alcotest.test_case "harpy fires" `Quick harpy_five_fear;
          Alcotest.test_case "harpy waits" `Quick harpy_below_five;
          Alcotest.test_case "terror shock" `Quick terror_shock_hits_both;
          Alcotest.test_case "crow sage" `Quick crow_sage_heals_joy;
          Alcotest.test_case "crow sage reads joy" `Quick crow_sage_reads_joy;
          Alcotest.test_case "freezing fog" `Quick freezing_fog_burns;
          Alcotest.test_case "mimic rebirths" `Quick mimic_rebirths;
          Alcotest.test_case "mimic destroyed" `Quick mimic_does_not_rebirth_when_destroyed;
          Alcotest.test_case "ghost invasion" `Quick ghost_invasion_hits_board;
          Alcotest.test_case "dark clouds" `Quick dark_clouds_sweep;
          Alcotest.test_case "fear of fear" `Quick fear_of_fear_destroys;
          Alcotest.test_case "medusa" `Quick medusa_power_and_growth;
          Alcotest.test_case "malediction" `Quick malediction_kills_opposite;
        ] );
    ]
