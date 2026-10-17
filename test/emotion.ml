(* Mechanics for the Emotion house. *)

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

let kill u pid num = hit u pid num 1000
let ten = [ 10; 10; 10; 10; 10 ]
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)

let coward_dies_when_alone () =
  let u = new_game ~mana:ten ~special:Emotion.house () in
  play u 0 "emotion.coward" ~input:(Some 2) ~cost:1;
  check_bool "alone on summon" true (is_empty u 0 2)

let coward_dies_when_the_last_ally_falls () =
  let u = new_game ~mana:ten ~special:Emotion.house () in
  place u 0 "fire.goblin" 1;
  play u 0 "emotion.coward" ~input:(Some 2) ~cost:1;
  check_bool "not alone yet" false (is_empty u 0 2);
  kill u 0 1;
  check_bool "coward dies" true (is_empty u 0 2)

let revolt_scales_with_the_opponent_board () =
  let u = new_game ~mana:ten ~special:Emotion.house () in
  List.iter (fun n -> place u 1 "fire.goblin" n) [ 0; 1; 2 ];
  play u 0 "emotion.revolt" ~input:None ~cost:2;
  check_int "2 + 2 * 3" 52 (player_life u 1)

let loneliness_hits_only_creatures_with_no_opposite () =
  let u = new_game ~mana:ten ~special:Emotion.house () in
  place u 0 "water.lord" 0;
  place u 0 "water.lord" 1;
  place u 1 "water.lord" 0;
  place u 1 "water.lord" 2;
  play u 0 "emotion.loneliness" ~input:None ~cost:3;
  check_int "protected owner" 34 (life_at u 0 0);
  check_int "lonely owner" 18 (life_at u 0 1);
  check_int "protected opponent" 34 (life_at u 1 0);
  check_int "lonely opponent" 18 (life_at u 1 2)

let courage_triples_at_twelve_life () =
  let u = new_game ~mana:ten ~special:Emotion.house () in
  place u 0 "emotion.courage" 2;
  step u (fun () -> (player u 0).pstate.ps_life <- 13);
  step u (fun () -> Player.refresh_slots (player u 0));
  check_int "normal" 5 (attack_at u 0 2);
  step u (fun () -> (player u 0).pstate.ps_life <- 12);
  step u (fun () -> Player.refresh_slots (player u 0));
  check_int "tripled" 15 (attack_at u 0 2)

let love_heals_by_the_number_of_allies () =
  let u = new_game ~mana:ten ~special:Emotion.house () in
  place u 0 "water.lord" 1;
  place u 0 "emotion.love" 2;
  place u 0 "water.lord" 3;
  hit u 0 2 10;
  check_int "damaged" 15 (life_at u 0 2);
  on_turn u 0;
  check_int "three allies" 18 (life_at u 0 2)

let hope_heals_allies_and_hurts_enemies () =
  let u = new_game ~mana:ten ~special:Emotion.house () in
  place u 0 "emotion.hope" 0;
  place u 0 "fire.goblin" 1;
  place u 1 "fire.goblin" 0;
  hit u 0 1 10;
  check_int "ally before" 6 (life_at u 0 1);
  kill u 0 0;
  check_int "ally healed" 13 (life_at u 0 1);
  check_int "enemy damaged" 9 (life_at u 1 0)

let teamwork_doubles_when_flanked () =
  let u = new_game ~mana:ten ~special:Emotion.house () in
  place u 0 "water.lord" 1;
  place u 0 "water.lord" 3;
  place u 0 "emotion.teamwork" 2;
  check_int "both sides" 12 (attack_at u 0 2);
  let v = new_game ~mana:ten ~special:Emotion.house () in
  place v 0 "water.lord" 1;
  place v 0 "emotion.teamwork" 2;
  check_int "one side" 6 (attack_at v 0 2)

let legend_halves_friendly_life_rounded_up () =
  let u = new_game ~mana:ten ~special:Emotion.house () in
  place u 0 "emotion.legend" 0;
  place u 0 "water.lord" 1;
  place u 0 "fire.goblin" 2;
  hit u 0 1 14;
  hit u 0 2 1;
  check_int "lord before" 20 (life_at u 0 1);
  check_int "goblin before" 15 (life_at u 0 2);
  kill u 0 0;
  check_int "lord halved" 10 (life_at u 0 1);
  check_int "goblin rounded up" 8 (life_at u 0 2)

let () =
  let open Alcotest in
  run "emotion"
    [
      ( "houses",
        [
          test_case "coward on summon" `Quick coward_dies_when_alone;
          test_case "coward after ally death" `Quick coward_dies_when_the_last_ally_falls;
          test_case "revolt" `Quick revolt_scales_with_the_opponent_board;
          test_case "loneliness" `Quick loneliness_hits_only_creatures_with_no_opposite;
          test_case "courage" `Quick courage_triples_at_twelve_life;
          test_case "love" `Quick love_heals_by_the_number_of_allies;
          test_case "hope" `Quick hope_heals_allies_and_hurts_enemies;
          test_case "teamwork" `Quick teamwork_doubles_when_flanked;
          test_case "legend" `Quick legend_halves_friendly_life_rounded_up;
        ] );
    ]
