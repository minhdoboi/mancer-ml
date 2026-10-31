(* Joy house: an earth variant that takes earth's basic slot in standard mode.
   Effects and stats are meant to be exactly earth's. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let check_opt = Alcotest.(check (option string))

let settle u = step u (fun () -> ())
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let growth u pid i = (player u pid).houses_update.hu_value.(i).growth

let hit u pid num amount =
  step u (fun () -> Slot.inflict (slot u pid num) (mk_damage amount { ctx_player = 1 - pid; ctx_card = None; ctx_selected = 0 }))

(* ----------------------------------------------------------------- modes *)

let standard_deals_joy () =
  init_all_houses ();
  let p = Shuffle.create_player ~first:true ~special:Troopers.house Catalog.standard_basics in
  check_string "standard house 3 is joy" "joy" p.desc.pd_houses.(3).phd_house.h_key

let classic_deals_earth () =
  init_all_houses ();
  let p = Shuffle.create_player ~first:true ~special:Troopers.house Catalog.classic_basics in
  check_string "classic house 3 is earth" "earth" p.desc.pd_houses.(3).phd_house.h_key

(* Every cost, attack, life and multi flag matches earth's card of the same slot. *)
let same_stats () =
  init_all_houses ();
  let z = Array.of_list (Joy.house.h_cards @ Joy.house.h_additional_cards) in
  let e = Array.of_list (Earth.house.h_cards @ Earth.house.h_additional_cards) in
  check_int "same card count" (Array.length e) (Array.length z);
  Array.iteri
    (fun i (ec : card) ->
      let jc = z.(i) in
      check_int (Printf.sprintf "cost %d" i) ec.cost jc.cost;
      check_bool (Printf.sprintf "spell %d" i) ec.is_spell jc.is_spell;
      match (ec.creature_body, jc.creature_body) with
      | Some eb, Some jb ->
          check_int (Printf.sprintf "attack %d" i)
            (Option.value eb.b_attack.base ~default:(-1))
            (Option.value jb.b_attack.base ~default:(-1));
          check_int (Printf.sprintf "life %d" i) eb.b_life jb.b_life;
          check_bool (Printf.sprintf "multi %d" i) eb.b_run_attack.is_multi_target
            jb.b_run_attack.is_multi_target
      | None, None -> ()
      | _ -> check_bool (Printf.sprintf "kind %d" i) true false)
    e

(* ----------------------------------------------------------------- cards *)

let faery_heals_owner () =
  let u = new_game ~house:Joy.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "joy.faery" 0;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "owner 40 + 3" 43 (player_life u 0)

let care_heals_player_and_creature () =
  let u = new_game ~mana:[ 4; 4; 4; 2 ] ~house:Joy.house () in
  place u 0 "fire.goblin" 0;
  hit u 0 0 8;
  step u (fun () ->
      Player.inflict (player u 0)
        (mk_damage ~is_spell:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  play u 0 "joy.care" ~input:(Some 0) ~cost:2;
  check_int "player 50 + 8" (50 + 8) (player_life u 0);
  check_int "goblin 8 + 8" 16 (life_at u 0 0)

let cat_sweeps () =
  let u = new_game ~house:Joy.house () in
  place u 0 "joy.cat" 0;
  place u 1 "anger.boar" 1;
  place u 1 "anger.boar" 4;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "opponent 60 - 1" 59 (player_life u 1);
  check_int "boar 26 - 1" 25 (life_at u 1 1);
  check_int "second boar 26 - 1" 25 (life_at u 1 4)

let bond_heals_per_joy () =
  let u = new_game ~mana:[ 4; 4; 4; 7 ] ~house:Joy.house () in
  (player u 0).pstate.ps_life <- 40;
  play u 0 "joy.bond" ~input:None ~cost:4;
  check_int "2 * 7 joy" (40 + 14) (player_life u 0)

let kitsune_grows_joy () =
  let u = new_game ~house:Joy.house () in
  let before = growth u 0 3 in
  place u 0 "joy.kitsune" 0;
  check_int "joy growth +2" (before + 2) (growth u 0 3)

let fusion_sums_top_two () =
  let u = new_game ~mana:[ 4; 4; 4; 6 ] ~house:Joy.house () in
  place u 0 "fire.monk" 0;
  place u 0 "fire.drake" 3;
  settle u;
  play u 0 "joy.fusion" ~input:None ~cost:6;
  check_int "3 + 4" (60 - 7) (player_life u 1)

let nymphea_spawns_two () =
  let u = new_game ~mana:[ 4; 4; 4; 7 ] ~house:Joy.house () in
  play u 0 "joy.nymphea" ~input:(Some 2) ~cost:7;
  check_opt "left nymph" (Some "joy.nymph") (occupant u 0 1);
  check_opt "right nymph" (Some "joy.nymph") (occupant u 0 3)

let faune_heals_self () =
  let u = new_game ~house:Joy.house () in
  place u 0 "joy.faune" 0;
  hit u 0 0 8;
  check_int "26 - 8" 18 (life_at u 0 0);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "18 + 4" 22 (life_at u 0 0)

let solar_burst_burns () =
  let u = new_game ~mana:[ 4; 4; 4; 9 ] ~house:Joy.house () in
  place u 0 "anger.boar" 0;
  place u 1 "anger.boar" 1;
  play u 0 "joy.burst" ~input:None ~cost:9;
  check_int "own boar 26 - 25" 1 (life_at u 0 0);
  check_int "foe boar 26 - 25" 1 (life_at u 1 1)

let sunstonegolem_mana_attack () =
  let u = new_game ~mana:[ 4; 4; 4; 7 ] ~house:Joy.house () in
  place u 0 "joy.sunstonegolem" 0;
  settle u;
  check_int "attack = joy mana" 7 (attack_at u 0 0);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "joy mana 7 + 1" 8 (mana u 0 3)

let sunstonegolem_heals_neighbours () =
  let u = new_game ~mana:[ 4; 4; 4; 7 ] ~house:Joy.house () in
  place u 0 "joy.sunstonegolem" 1;
  place u 0 "fire.goblin" 0;
  hit u 0 0 8;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "goblin 8 + 3" 11 (life_at u 0 0)

let attachment_heals_all () =
  let u = new_game ~house:Joy.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "joy.attachment" 0;
  place u 0 "fire.goblin" 1;
  hit u 0 1 8;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "owner 40 + 3" 43 (player_life u 0);
  check_int "goblin 8 + 3" 11 (life_at u 0 1)

let dragon_sweeps_and_heals () =
  let u = new_game ~house:Joy.house () in
  place u 0 "joy.dragon" 0;
  place u 1 "anger.boar" 1;
  hit u 0 0 8;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "dragon 32 + 4" 36 (life_at u 0 0);
  step u (fun () -> Player.run_slots (player u 0));
  check_int "boar 26 - 3" 23 (life_at u 1 1)

let () =
  init_all_houses ();
  Alcotest.run "joy"
    [
      ( "mode",
        [
          Alcotest.test_case "standard deals joy" `Quick standard_deals_joy;
          Alcotest.test_case "classic deals earth" `Quick classic_deals_earth;
          Alcotest.test_case "same stats as earth" `Quick same_stats;
        ] );
      ( "cards",
        [
          Alcotest.test_case "playful faery" `Quick faery_heals_owner;
          Alcotest.test_case "care" `Quick care_heals_player_and_creature;
          Alcotest.test_case "drunken cat" `Quick cat_sweeps;
          Alcotest.test_case "sexual bond" `Quick bond_heals_per_joy;
          Alcotest.test_case "kitsune" `Quick kitsune_grows_joy;
          Alcotest.test_case "fusion" `Quick fusion_sums_top_two;
          Alcotest.test_case "nymphea" `Quick nymphea_spawns_two;
          Alcotest.test_case "faune" `Quick faune_heals_self;
          Alcotest.test_case "solar burst" `Quick solar_burst_burns;
          Alcotest.test_case "sun stone golem" `Quick sunstonegolem_mana_attack;
          Alcotest.test_case "sun stone golem heals" `Quick sunstonegolem_heals_neighbours;
          Alcotest.test_case "attachment healer" `Quick attachment_heals_all;
          Alcotest.test_case "oriental dragon" `Quick dragon_sweeps_and_heals;
        ] );
    ]
