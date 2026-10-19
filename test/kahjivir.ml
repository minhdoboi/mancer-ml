(* Mechanics for the Kahjivir house. *)

open Mancer
open Sp
open Engine
open Common
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }
let ten = [ 10; 10; 10; 10; 10 ]

let hit ?(is_ability = false) ?(is_spell = false) u pid num amount =
  step u (fun () -> Slot.inflict (slot u pid num) (mk_damage ~is_ability ~is_spell amount (ctx (1 - pid))))

let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)
let settle u = step u (fun () -> ())
let run_at u pid num =
  step u (fun () -> Option.iter (fun st -> Player.run_slot (player u pid) num st) (slot u pid num).value)

let stalker_strikes_a_creature () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "kahjivir.stalker" ~input:(Some 2) ~cost:3;
  check_int "not on summon" 16 (life_at u 1 2);
  run_at u 0 2;
  check_int "one attack this turn" 11 (life_at u 1 2)

let stalker_punishes_an_empty_lane () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  play u 0 "kahjivir.stalker" ~input:(Some 2) ~cost:3;
  check_int "player hit" 57 (player_life u 1)

let javelin_throws_at_a_creature () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 0 "kahjivir.javelin" 2;
  check_int "open lane" 4 (attack_at u 0 2);
  place u 1 "fire.goblin" 2;
  settle u;
  check_int "creature opposite" 7 (attack_at u 0 2)

let dancer_reduces_creature_damage () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 0 "kahjivir.dancer" 2;
  place u 1 "fire.goblin" 2;
  hit u 0 2 10;
  check_int "two less" 19 (life_at u 0 2)

let dancer_ignores_spell_damage_in_an_empty_lane () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 0 "kahjivir.dancer" 2;
  hit ~is_spell:true u 0 2 10;
  check_int "no spell damage" 27 (life_at u 0 2);
  place u 1 "fire.goblin" 2;
  hit ~is_spell:true u 0 2 10;
  check_int "spell still hurts opposite a creature" 17 (life_at u 0 2)

let shaman_grows_with_each_attack () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 0 "kahjivir.shaman" 2;
  run_at u 0 2;
  check_int "player hit once" 57 (player_life u 1);
  check_int "one attack gained" 4 (attack_at u 0 2)

let shaman_attacks_twice_against_a_creature () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 0 "kahjivir.shaman" 2;
  place u 1 "fire.goblin" 2;
  run_at u 0 2;
  check_int "3 then 4" 9 (life_at u 1 2);
  check_int "two attack gains" 5 (attack_at u 0 2)

let archer_shoots_from_an_empty_lane () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 0 "kahjivir.archer" 2;
  check_int "plus six" 9 (attack_at u 0 2)

let archer_rains_on_creatures () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 0 "kahjivir.archer" 2;
  place u 1 "fire.goblin" 2;
  place u 1 "fire.goblin" 4;
  on_turn u 0;
  check_int "opposite" 13 (life_at u 1 2);
  check_int "elsewhere" 13 (life_at u 1 4)

let head_huntress_executes_the_weak () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "kahjivir.headhuntress" ~input:(Some 2) ~cost:8;
  check_bool "gone" true (is_empty u 1 2)

let head_huntress_razes_an_empty_lane () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 1 "fire.goblin" 0;
  play u 0 "kahjivir.headhuntress" ~input:(Some 2) ~cost:8;
  check_int "player" 52 (player_life u 1);
  check_int "creature" 8 (life_at u 1 0)

let hunger_adds_an_attack_and_heals () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 0 "kahjivir.javelin" 2;
  place u 1 "fire.goblin" 2;
  hit u 0 2 10;
  play u 0 "kahjivir.hunger" ~input:(Some 2) ~cost:1;
  run_at u 0 2;
  check_int "goblin took two throws" 2 (life_at u 1 2);
  check_int "healed the damage dealt" 29 (life_at u 0 2);
  on_end u 0;
  check_int "extra attack expired" 7 (attack_at u 0 2)

let ambush_buffs_the_target_and_neighbours () =
  let u = new_game ~mana:ten ~special:Kahjivir.house () in
  place u 0 "fire.goblin" 1;
  place u 0 "kahjivir.javelin" 2;
  place u 0 "fire.goblin" 3;
  play u 0 "kahjivir.ambush" ~input:(Some 2) ~cost:2;
  check_int "permanent +2" 6 (attack_at u 0 2);
  run_at u 0 1;
  check_int "left neighbour attacked twice" 52 (player_life u 1);
  run_at u 0 3;
  check_int "right neighbour attacked twice" 44 (player_life u 1)

let () =
  Alcotest.run "kahjivir"
    [
      ( "houses",
        [
          Alcotest.test_case "stalker creature" `Quick stalker_strikes_a_creature;
          Alcotest.test_case "stalker empty lane" `Quick stalker_punishes_an_empty_lane;
          Alcotest.test_case "javelin" `Quick javelin_throws_at_a_creature;
          Alcotest.test_case "dancer creature" `Quick dancer_reduces_creature_damage;
          Alcotest.test_case "dancer empty lane" `Quick dancer_ignores_spell_damage_in_an_empty_lane;
          Alcotest.test_case "shaman grows" `Quick shaman_grows_with_each_attack;
          Alcotest.test_case "shaman doubles" `Quick shaman_attacks_twice_against_a_creature;
          Alcotest.test_case "archer empty lane" `Quick archer_shoots_from_an_empty_lane;
          Alcotest.test_case "archer volley" `Quick archer_rains_on_creatures;
          Alcotest.test_case "head huntress kills" `Quick head_huntress_executes_the_weak;
          Alcotest.test_case "head huntress empty lane" `Quick head_huntress_razes_an_empty_lane;
          Alcotest.test_case "hunger" `Quick hunger_adds_an_attack_and_heals;
          Alcotest.test_case "ambush" `Quick ambush_buffs_the_target_and_neighbours;
        ] );
    ]
