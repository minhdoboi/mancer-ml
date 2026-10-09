(* Mechanics for Water, Air, Earth and Troopers, same style as test/mechanics.ml. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

(* ------------------------------------------------------------------ water *)

let golem_ignores_ability () =
  let u = new_game () in
  place u 1 "water.golem" 0;
  place u 0 "fire.wall" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "golem ignores ability damage" 12 (life_at u 1 0)

let golem_takes_combat () =
  let u = new_game () in
  place u 1 "water.golem" 0;
  place u 0 "fire.dragon" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "golem 12 - 9 combat" 3 (life_at u 1 0)

let turtle_absorbs_five () =
  let u = new_game () in
  place u 1 "water.turtle" 0;
  place u 0 "fire.goblin" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "4 attack - 5 absorb" 17 (life_at u 1 0)

let iceguard_halves_player_damage () =
  let u = new_game () in
  place u 1 "water.guard" 1;
  let d = mk_damage ~is_spell:true 9 { ctx_player = 0; ctx_card = None; ctx_selected = 0 } in
  step u (fun () -> Player.inflict (player u 1) d);
  check_int "ceil(9/2) = 5" (60 - 5) (player_life u 1)

let elder_air_growth () =
  let u = new_game () in
  let growth () = (player u 0).houses_update.hu_value.(2).growth in
  let before = growth () in
  place u 0 "water.elder" 0;
  check_int "air growth +1" (before + 1) (growth ())

let apostate_adds_fire () =
  let u = new_game ~mana0:3 () in
  play u 0 "water.apostate" ~input:(Some 0) ~cost:3;
  check_int "fire mana 3 + 2" 5 (player u 0).houses_update.hu_value.(0).mana

let overlord_gives_run () =
  let u = new_game () in
  place ~run:false u 0 "water.lord" 1;
  place ~run:false u 0 "fire.goblin" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "adjacent goblin attacks the turn it lands" (60 - 4) (player_life u 1)

(* -------------------------------------------------------------------- air *)

let griffin_fires_when_air_ge_5 () =
  let u = new_game ~mana:[ 4; 4; 6; 4 ] () in
  play u 0 "air.griffin" ~input:(Some 0) ~cost:2;
  check_int "pre-spend air 6 >= 5" (60 - 5) (player_life u 1)

let griffin_skips_when_air_lt_5 () =
  let u = new_game ~mana:[ 4; 4; 4; 4 ] () in
  play u 0 "air.griffin" ~input:(Some 0) ~cost:2;
  check_int "pre-spend air 4 < 5" 60 (player_life u 1)

let phoenix_reborn () =
  let u = new_game ~mana0:10 () in
  place u 0 "air.phoenix" 0;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 18 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  Alcotest.(check (option string)) "phoenix returns" (Some "air.phoenix") (occupant u 0 0)

let phoenix_stays_dead_without_fire () =
  let u = new_game ~mana0:9 () in
  place u 0 "air.phoenix" 0;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 18 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_bool "mana 9 is not enough" true (is_empty u 0 0)

let dead_hook_override_suppresses_on_my_death () =
  let u = new_game ~mana0:10 () in
  place u 0 "air.phoenix" 0;
  (* SlotsUpdate.onDead is a var: a house that replaces it decides whether
     onMyDeath runs, so the phoenix must not come back here *)
  (player u 0).slots_update.on_dead_hook <- ignore;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 18 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_bool "no rebirth" true (is_empty u 0 0)

let twister_destroys () =
  let u = new_game ~mana:[ 4; 4; 10; 4 ] () in
  place u 1 "fire.dragon" 2;
  play u 0 "air.twister" ~input:(Some 2) ~cost:10;
  check_bool "dragon is destroyed" true (is_empty u 1 2)

let cloud_is_multi () =
  let u = new_game () in
  place u 0 "air.cloud" 0;
  place u 1 "fire.goblin" 2;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "player takes 4" (60 - 4) (player_life u 1);
  check_int "off-lane goblin also takes 4" 12 (life_at u 1 2)

let titan_hits_opposite () =
  let u = new_game ~mana:[ 4; 4; 12; 4 ] () in
  place u 1 "fire.dragon" 1;
  play u 0 "air.titan" ~input:(Some 1) ~cost:12;
  check_int "dragon 41 - 15" 26 (life_at u 1 1)

(* ------------------------------------------------------------------ earth *)

let hermit_earth_growth () =
  let u = new_game () in
  let growth () = (player u 0).houses_update.hu_value.(3).growth in
  let before = growth () in
  place u 0 "earth.hermit" 0;
  check_int "earth growth +2" (before + 2) (growth ())

let fury_sums_top_two () =
  let u = new_game ~mana:[ 4; 4; 4; 6 ] () in
  (* not adjacent, so no orc/bull bonus; no SpellMod on the board *)
  place u 0 "fire.monk" 0;
  place u 0 "fire.drake" 3;
  (* Fury reads the attacks published by the previous step *)
  step u ignore;
  play u 0 "earth.fury" ~input:None ~cost:6;
  check_int "3 + 4" (60 - 7) (player_life u 1)

let fury_counts_elemental_before_payment () =
  let u = new_game ~mana:[ 4; 4; 4; 10 ] () in
  place u 0 "earth.elemental" 0;
  place u 0 "fire.monk" 3;
  step u ignore;
  play u 0 "earth.fury" ~input:None ~cost:6;
  (* elemental attack = earth mana before paying (10), not after (4) *)
  check_int "10 + 3" (60 - 13) (player_life u 1)

let spider_fills_adjacent () =
  let u = new_game ~mana:[ 4; 4; 4; 7 ] () in
  play u 0 "earth.hugespider" ~input:(Some 2) ~cost:7;
  Alcotest.(check (option string)) "left spider" (Some "earth.forestspider") (occupant u 0 1);
  Alcotest.(check (option string)) "right spider" (Some "earth.forestspider") (occupant u 0 3)

let spider_skips_blocked_slot () =
  let u = new_game ~mana:[ 4; 4; 4; 7 ] () in
  Player.block_slot (player u 0) 1;
  play u 0 "earth.hugespider" ~input:(Some 2) ~cost:7;
  check_bool "blocked slot stays empty" true (is_empty u 0 1);
  Alcotest.(check (option string)) "right spider" (Some "earth.forestspider") (occupant u 0 3)

let care_heals_player_and_creature () =
  let u = new_game ~mana:[ 4; 4; 4; 2 ] () in
  place u 0 "fire.goblin" 0;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 8 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  step u (fun () ->
      Player.inflict (player u 0)
        (mk_damage ~is_spell:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  play u 0 "earth.care" ~input:(Some 0) ~cost:2;
  check_int "player +8" (50 + 8) (player_life u 0);
  check_int "goblin 8 + 8" 16 (life_at u 0 0)

(* -------------------------------------------------------------- troopers *)

let marine_pokes_summons () =
  let u = new_game () in
  place u 0 "Marine" 0;
  place u 1 "fire.goblin" 2;
  check_int "goblin 16 - 4" 12 (life_at u 1 2)

let goliath_ignores_spell () =
  let u = new_game () in
  place u 1 "Goliath" 0;
  play u 0 "fire.wave" ~input:None ~cost:6;
  check_int "immune to spell" 20 (life_at u 1 0)

let goliath_absorbs_combat () =
  let u = new_game () in
  place u 1 "Goliath" 0;
  place u 0 "fire.goblin" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "4 - 1 absorb" 17 (life_at u 1 0)

let siege_hits_fattest () =
  let u = new_game () in
  place u 0 "SiegeTank" 0;
  place u 1 "fire.goblin" 1;
  place u 1 "fire.dragon" 3;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "dragon 41 - 8" 33 (life_at u 1 3);
  check_int "goblin untouched" 16 (life_at u 1 1)

let conscription_special_mana () =
  let u = new_game () in
  play u 0 "Conscription" ~input:None ~cost:0;
  check_int "special mana 2 -> 3" 3 (player u 0).houses_update.hu_value.(4).mana

let special_deck_has_one_card_per_quarter () =
  init_all_houses ();
  let d = Game.make_player_desc ~special:Troopers.house Fire.house in
  Alcotest.(check (list string))
    "Troopers deck"
    [ "Conscription"; "Barracks"; "Goliath"; "NuclearMissile" ]
    (Array.to_list d.pd_houses.(1).phd_cards |> List.map (fun cd -> cd.cd_card.key))

let () =
  let open Alcotest in
  run "houses"
    [
      ( "water",
        [
          test_case "golem ignores abilities" `Quick golem_ignores_ability;
          test_case "golem takes combat" `Quick golem_takes_combat;
          test_case "turtle absorb" `Quick turtle_absorbs_five;
          test_case "iceguard" `Quick iceguard_halves_player_damage;
          test_case "elder growth" `Quick elder_air_growth;
          test_case "apostate" `Quick apostate_adds_fire;
          test_case "overlord run" `Quick overlord_gives_run;
        ] );
      ( "air",
        [
          test_case "griffin fires" `Quick griffin_fires_when_air_ge_5;
          test_case "griffin skips" `Quick griffin_skips_when_air_lt_5;
          test_case "phoenix reborn" `Quick phoenix_reborn;
          test_case "phoenix stays dead" `Quick phoenix_stays_dead_without_fire;
          test_case "on_dead_hook override" `Quick dead_hook_override_suppresses_on_my_death;
          test_case "twister" `Quick twister_destroys;
          test_case "cloud multi" `Quick cloud_is_multi;
          test_case "titan" `Quick titan_hits_opposite;
        ] );
      ( "earth",
        [
          test_case "hermit growth" `Quick hermit_earth_growth;
          test_case "fury" `Quick fury_sums_top_two;
          test_case "fury before payment" `Quick fury_counts_elemental_before_payment;
          test_case "spider" `Quick spider_fills_adjacent;
          test_case "spider blocked slot" `Quick spider_skips_blocked_slot;
          test_case "care" `Quick care_heals_player_and_creature;
        ] );
      ( "troopers",
        [
          test_case "marine" `Quick marine_pokes_summons;
          test_case "goliath spell" `Quick goliath_ignores_spell;
          test_case "goliath absorb" `Quick goliath_absorbs_combat;
          test_case "siege" `Quick siege_hits_fattest;
          test_case "conscription" `Quick conscription_special_mana;
          test_case "special deck" `Quick special_deck_has_one_card_per_quarter;
        ] );
    ]
