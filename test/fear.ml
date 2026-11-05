(* Fear house: an air variant that takes air's basic slot. *)

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

(* ----------------------------------------------------------------- cards *)

let ghost_boosts_spells () =
  let u = new_game ~mana:[ 10; 4; 4; 4 ] ~house:Fear.house () in
  place u 1 "anger.boar" 1;
  place u 0 "fear.ghost" 0;
  play u 0 "anger.wave" ~input:None ~cost:6;
  check_int "9 + 1 spell damage" 16 (life_at u 1 1)

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

(* The sage reads the joy house, which sits in slot 3. *)
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
  let u = new_game ~mana:[ 4; 10; 4; 4 ] ~house:Fear.house () in
  place u 0 "fear.mimic" 0;
  hit u 0 0 100;
  check_string "same slot reborn" "fear.mimic" (Option.get (occupant u 0 0));
  let atk = attack_at u 0 0 and life = life_at u 0 0 in
  check_bool "reborn attack in 6..8" true (atk >= 6 && atk <= 8);
  check_bool "reborn life in 14..19" true (life >= 14 && life <= 19)

(* The roll hook fixes the body while the slot is built, so a real summon
   lands with the rolled attack and life already in place. *)
let mimic_summon_rolls_stats () =
  let u = new_game ~mana:[ 20; 4; 4; 4 ] ~house:Fear.house () in
  play u 0 "fear.mimic" ~input:(Some 0) ~cost:7;
  let atk = attack_at u 0 0 and life = life_at u 0 0 in
  check_bool "summoned attack in 6..8" true (atk >= 6 && atk <= 8);
  check_bool "summoned life in 14..19" true (life >= 14 && life <= 19)

let mimic_does_not_rebirth_when_destroyed () =
  let u = new_game ~mana:[ 4; 10; 4; 4 ] ~house:Fear.house () in
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

let is_stunned u pid num =
  match (slot u pid num).value with Some st -> has_flag st stun_flag | None -> false

let medusa_stuns_opposite_when_wounded () =
  let u = new_game ~mana:[ 4; 4; 7; 4 ] ~house:Fear.house () in
  place u 1 "anger.boar" 1;
  (player u 0).pstate.ps_life <- 15;
  place u 0 "fear.medusa" 1;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_bool "opposite boar stunned" true (is_stunned u 1 1)

let medusa_leaves_opposite_alone_above_fifteen () =
  let u = new_game ~mana:[ 4; 4; 7; 4 ] ~house:Fear.house () in
  place u 1 "anger.boar" 1;
  (player u 0).pstate.ps_life <- 16;
  place u 0 "fear.medusa" 1;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_bool "opposite boar not stunned" false (is_stunned u 1 1)

let ghost_stuns_opposite_when_spelled () =
  let u = new_game ~house:Fear.house () in
  place u 0 "fear.ghost" 1;
  place u 1 "anger.boar" 1;
  (player u 0).pstate.ps_life <- 15;
  hit u 0 1 5;
  check_bool "plain damage does not stun" false (is_stunned u 1 1);
  step u (fun () ->
      Slot.inflict (slot u 0 1)
        (mk_damage ~is_spell:true 5 { ctx_player = 1; ctx_card = None; ctx_selected = 1 }));
  check_bool "spell stuns the opposite creature" true (is_stunned u 1 1)

let ghost_spares_opposite_above_fifteen () =
  let u = new_game ~house:Fear.house () in
  place u 0 "fear.ghost" 1;
  place u 1 "anger.boar" 1;
  (player u 0).pstate.ps_life <- 16;
  step u (fun () ->
      Slot.inflict (slot u 0 1)
        (mk_damage ~is_spell:true 5 { ctx_player = 1; ctx_card = None; ctx_selected = 1 }));
  check_bool "no stun above fifteen" false (is_stunned u 1 1)

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
      ( "basics",
        [ Alcotest.test_case "standard deals fear" `Quick standard_deals_fear ] );
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
          Alcotest.test_case "mimic rolls on summon" `Quick mimic_summon_rolls_stats;
          Alcotest.test_case "mimic destroyed" `Quick mimic_does_not_rebirth_when_destroyed;
          Alcotest.test_case "ghost invasion" `Quick ghost_invasion_hits_board;
          Alcotest.test_case "dark clouds" `Quick dark_clouds_sweep;
          Alcotest.test_case "fear of fear" `Quick fear_of_fear_destroys;
          Alcotest.test_case "medusa" `Quick medusa_power_and_growth;
          Alcotest.test_case "medusa petrifies when wounded" `Quick medusa_stuns_opposite_when_wounded;
          Alcotest.test_case "medusa spares above fifteen" `Quick medusa_leaves_opposite_alone_above_fifteen;
          Alcotest.test_case "ghost stuns when spelled" `Quick ghost_stuns_opposite_when_spelled;
          Alcotest.test_case "ghost spares above fifteen" `Quick ghost_spares_opposite_above_fifteen;
          Alcotest.test_case "malediction" `Quick malediction_kills_opposite;
        ] );
    ]
