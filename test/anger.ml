open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)

let settle u = step u (fun () -> ())

(* ----------------------------------------------------------------- modes *)

let standard_deals_anger () =
  init_all_houses ();
  let p = Shuffle.create_player ~first:true ~special:Troopers.house Catalog.standard_basics in
  check_string "standard house 0 is anger" "anger" p.desc.pd_houses.(0).phd_house.h_key

(* ----------------------------------------------------------------- cards *)

let werewolf_rages_adjacent () =
  let u = new_game ~house:Anger.house () in
  place u 0 "anger.werewolf" 1;
  place u 0 "anger.hotspot" 0;
  place u 0 "anger.dragon" 2;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "hotspot 5 - 2" 3 (life_at u 0 0);
  check_int "dragon 41 - 2" 39 (life_at u 0 2)

(* The werewolf's own attack path is the ordinary one: the speed bump is the
   spell that wound it, not a special run attack. *)
let werewolf_counterattacks_when_wounded () =
  let u = new_game ~house:Anger.house () in
  place u 0 "anger.werewolf" 1;
  place u 1 "anger.boar" 1;
  (player u 0).pstate.ps_life <- 15;
  step u (fun () ->
      Slot.inflict (slot u 0 1)
        (mk_damage ~is_spell:true 5 { ctx_player = 1; ctx_card = None; ctx_selected = 1 }));
  check_int "opposite boar 26 - 4" 22 (life_at u 1 1)

let werewolf_strikes_player_when_unopposed () =
  let u = new_game ~house:Anger.house () in
  place u 0 "anger.werewolf" 1;
  (player u 0).pstate.ps_life <- 15;
  let before = player_life u 1 in
  step u (fun () ->
      Slot.inflict (slot u 0 1)
        (mk_damage ~is_spell:true 5 { ctx_player = 1; ctx_card = None; ctx_selected = 1 }));
  check_int "opponent 60 - 4" (before - 4) (player_life u 1)

(* The wound lands on the opponent's turn, when the ordinary attack phase has
   had no say: the swing must not wait for the werewolf's run flag. *)
let werewolf_strikes_before_being_readied () =
  let u = new_game ~house:Anger.house () in
  place ~run:false u 0 "anger.werewolf" 1;
  (player u 0).pstate.ps_life <- 15;
  let before = player_life u 1 in
  step u (fun () ->
      Slot.inflict (slot u 0 1)
        (mk_damage ~is_spell:true 5 { ctx_player = 1; ctx_card = None; ctx_selected = 1 }));
  check_int "opponent 60 - 4" (before - 4) (player_life u 1);
  check_bool
    "the lent run flag is given back"
    false
    (match (slot u 0 1).value with Some st -> has_flag st run_flag | None -> true)

let werewolf_holds_its_swing_above_fifteen () =
  let u = new_game ~house:Anger.house () in
  place u 0 "anger.werewolf" 1;
  place u 1 "anger.boar" 1;
  (player u 0).pstate.ps_life <- 16;
  step u (fun () ->
      Slot.inflict (slot u 0 1)
        (mk_damage ~is_spell:true 5 { ctx_player = 1; ctx_card = None; ctx_selected = 1 }));
  check_int "boar untouched above fifteen" 26 (life_at u 1 1)

let werewolf_ignores_plain_damage () =
  let u = new_game ~house:Anger.house () in
  place u 0 "anger.werewolf" 1;
  place u 1 "anger.boar" 1;
  (player u 0).pstate.ps_life <- 15;
  step u (fun () ->
      Slot.inflict (slot u 0 1)
        (mk_damage ~is_ability:true 5 { ctx_player = 1; ctx_card = None; ctx_selected = 1 }));
  check_int "ability does not enrage it" 26 (life_at u 1 1)

let hotspot_hits_opponent_board () =
  let u = new_game ~house:Anger.house () in
  place u 1 "anger.dragon" 0;
  place u 1 "anger.werewolf" 2;
  place u 0 "anger.hotspot" 3;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "dragon 41 - 5" 36 (life_at u 1 0);
  check_int "werewolf 16 - 5" 11 (life_at u 1 2)

let sentinel_grows_anger () =
  let u = new_game ~house:Anger.house () in
  let growth () = (player u 0).houses_update.hu_value.(0).growth in
  let before = growth () in
  place u 0 "anger.sentinel" 0;
  check_int "anger growth +1" (before + 1) (growth ())

let gorilla_buffs_allies () =
  let u = new_game ~house:Anger.house () in
  check_int "gorilla costs 7" 7 (card_by_key "anger.gorilla").cost;
  place u 0 "anger.dragon" 1;
  settle u;
  place u 0 "anger.gorilla" 0;
  settle u;
  check_int "dragon 9 + 1" 10 (attack_at u 0 1)

let chainbreaker_buffs_adjacent () =
  let u = new_game ~house:Anger.house () in
  check_int "chain breaker costs 5" 5 (card_by_key "anger.chainbreaker").cost;
  place u 0 "anger.dragon" 1;
  place u 0 "anger.boar" 2;
  settle u;
  place u 0 "anger.chainbreaker" 0;
  settle u;
  check_int "dragon 9 + 2" 11 (attack_at u 0 1);
  check_int "boar not adjacent" 8 (attack_at u 0 2)

let boar_mass_damage () =
  let u = new_game ~house:Anger.house () in
  place u 0 "anger.dragon" 0;
  place u 1 "anger.dragon" 1;
  place u 0 "anger.boar" 2;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "own dragon 41 - 5" 36 (life_at u 0 0);
  check_int "foe dragon 41 - 5" 36 (life_at u 1 1);
  check_int "boar immune" 26 (life_at u 0 2)

let oni_attacks_on_summon () =
  let u = new_game ~mana0:4 ~house:Anger.house () in
  play u 0 "anger.oni" ~input:(Some 0) ~cost:4;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "oni hits for 4 the turn it lands" (60 - 4) (player_life u 1)

let normal_creature_waits_a_turn () =
  let u = new_game ~mana0:1 ~house:Anger.house () in
  play u 0 "anger.werewolf" ~input:(Some 0) ~cost:1;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "werewolf does not attack yet" 60 (player_life u 1)

let scorching_breath_nine () =
  let u = new_game ~mana0:6 ~house:Anger.house () in
  place u 1 "anger.dragon" 1;
  play u 0 "anger.wave" ~input:None ~cost:6;
  check_int "dragon 41 - 9" 32 (life_at u 1 1)

let scorching_voice_18_10 () =
  let u = new_game ~mana0:20 ~house:Anger.house () in
  place u 1 "anger.dragon" 1;
  place u 1 "anger.werewolf" 3;
  play u 0 "anger.inferno" ~input:(Some 1) ~cost:9;
  check_int "target 41 - 18" 23 (life_at u 1 1);
  check_int "other 16 - 10" 6 (life_at u 1 3)

let nemesis_attack_for_mana () =
  let u = new_game ~mana0:7 ~house:Anger.house () in
  place u 0 "anger.nemesis" 0;
  settle u;
  check_int "attack = anger mana" 7 (attack_at u 0 0)

let nemesis_hits_on_summon () =
  let u = new_game ~mana0:7 ~house:Anger.house () in
  place u 1 "anger.dragon" 0;
  place u 0 "anger.nemesis" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "dragon 41 - 3" 38 (life_at u 1 0);
  check_int "opponent 60 - 3" 57 (player_life u 1)

let nemesis_mana_each_turn () =
  let u = new_game ~mana0:7 ~house:Anger.house () in
  place u 0 "anger.nemesis" 0;
  let mana () = (player u 0).houses_update.hu_value.(0).mana in
  let before = mana () in
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "anger mana +1" (before + 1) (mana ())

let nemesis_sweep_when_wounded () =
  let u = new_game ~mana0:7 ~house:Anger.house () in
  place u 1 "anger.dragon" 0;
  (player u 0).pstate.ps_life <- 9;
  place u 0 "anger.nemesis" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "dragon 41 - 5" 36 (life_at u 1 0);
  check_int "opponent still 60 - 3" 57 (player_life u 1)

let nemesis_sweep_at_fifteen () =
  let u = new_game ~mana0:7 ~house:Anger.house () in
  place u 1 "anger.dragon" 0;
  (player u 0).pstate.ps_life <- 15;
  place u 0 "anger.nemesis" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "dragon 41 - 5 at exactly 15" 36 (life_at u 1 0)

let nemesis_sweep_at_sixteen () =
  let u = new_game ~mana0:7 ~house:Anger.house () in
  place u 1 "anger.dragon" 0;
  (player u 0).pstate.ps_life <- 16;
  place u 0 "anger.nemesis" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "dragon 41 - 3 at sixteen" 38 (life_at u 1 0)

let incarnation_boosts_spells () =
  let u = new_game ~mana0:20 ~house:Anger.house () in
  place u 0 "anger.dragon" 0;
  place u 1 "anger.dragon" 1;
  play u 0 "anger.wave" ~input:None ~cost:6;
  check_int "9 * 1.5 = 14" (41 - 14) (life_at u 1 1)

let outburst_scales_with_anger () =
  let u = new_game ~mana0:11 ~house:Anger.house () in
  place u 0 "anger.werewolf" 0;
  place u 1 "anger.werewolf" 0;
  play u 0 "anger.apocalypse" ~input:None ~cost:11;
  check_int "11 paid + 8" (60 - 19) (player_life u 1);
  check_bool "enemy werewolf dies" true (is_empty u 1 0);
  check_bool "caster's werewolf dies" true (is_empty u 0 0)

let () =
  init_all_houses ();
  Alcotest.run "anger"
    [
      ( "basics",
        [ Alcotest.test_case "standard deals anger" `Quick standard_deals_anger ] );
      ( "cards",
        [
          Alcotest.test_case "werewolf rages" `Quick werewolf_rages_adjacent;
          Alcotest.test_case "werewolf counterattacks when wounded"
            `Quick werewolf_counterattacks_when_wounded;
          Alcotest.test_case "werewolf strikes the player when unopposed"
            `Quick werewolf_strikes_player_when_unopposed;
          Alcotest.test_case "werewolf strikes before being readied"
            `Quick werewolf_strikes_before_being_readied;
          Alcotest.test_case "werewolf holds its swing above fifteen"
            `Quick werewolf_holds_its_swing_above_fifteen;
          Alcotest.test_case "werewolf ignores plain damage" `Quick werewolf_ignores_plain_damage;
          Alcotest.test_case "fire hotspot" `Quick hotspot_hits_opponent_board;
          Alcotest.test_case "sentinel" `Quick sentinel_grows_anger;
          Alcotest.test_case "revolt gorilla" `Quick gorilla_buffs_allies;
          Alcotest.test_case "chain breaker" `Quick chainbreaker_buffs_adjacent;
          Alcotest.test_case "wild boar" `Quick boar_mass_damage;
          Alcotest.test_case "oni" `Quick oni_attacks_on_summon;
          Alcotest.test_case "summoning sickness" `Quick normal_creature_waits_a_turn;
          Alcotest.test_case "scorching breath" `Quick scorching_breath_nine;
          Alcotest.test_case "scorching voice" `Quick scorching_voice_18_10;
          Alcotest.test_case "nemesis" `Quick nemesis_attack_for_mana;
          Alcotest.test_case "nemesis hits on summon" `Quick nemesis_hits_on_summon;
          Alcotest.test_case "nemesis mana each turn" `Quick nemesis_mana_each_turn;
          Alcotest.test_case "nemesis sweeps harder when wounded" `Quick nemesis_sweep_when_wounded;
          Alcotest.test_case "nemesis sweeps 5 at fifteen life" `Quick nemesis_sweep_at_fifteen;
          Alcotest.test_case "nemesis sweeps 3 at sixteen life" `Quick nemesis_sweep_at_sixteen;
          Alcotest.test_case "incarnation" `Quick incarnation_boosts_spells;
          Alcotest.test_case "outburst" `Quick outburst_scales_with_anger;
        ] );
    ]
