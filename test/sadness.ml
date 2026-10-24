(* Sadness house: a water variant that takes water's basic slot in standard mode. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)

let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let hit ?(is_ability = false) u pid num amount =
  step u (fun () -> Slot.inflict (slot u pid num) (mk_damage ~is_ability amount (ctx (1 - pid))))

let settle u = step u (fun () -> ())
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let growth u pid i = (player u pid).houses_update.hu_value.(i).growth

(* ----------------------------------------------------------------- modes *)

let standard_deals_sadness () =
  init_all_houses ();
  let p = Shuffle.create_player ~first:true ~special:Troopers.house Catalog.standard_basics in
  check_string "standard house 0 is anger" "anger" p.desc.pd_houses.(0).phd_house.h_key;
  check_string "standard house 1 is sadness" "sadness" p.desc.pd_houses.(1).phd_house.h_key

let classic_deals_water () =
  init_all_houses ();
  let p = Shuffle.create_player ~first:true ~special:Troopers.house Catalog.classic_basics in
  check_string "classic house 1 is water" "water" p.desc.pd_houses.(1).phd_house.h_key

(* ----------------------------------------------------------------- cards *)

let retirement_mana () =
  let u = new_game ~mana:[ 6; 6; 4; 4 ] ~house:Sadness.house () in
  play u 0 "sadness.retirement" ~input:None ~cost:1;
  check_int "anger +1" 7 (mana u 0 0);
  check_int "sadness paid 1" 5 (mana u 0 1);
  check_int "air +1" 5 (mana u 0 2);
  check_int "earth +1" 5 (mana u 0 3)

let living_shadow_hurts_owner () =
  let u = new_game ~house:Sadness.house () in
  place u 0 "sadness.shadow" 0;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "owner 60 - 2" 58 (player_life u 0)

let scratched_victim_gives_anger () =
  let u = new_game ~mana:[ 6; 6; 4; 4 ] ~house:Sadness.house () in
  place u 0 "sadness.victim" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "anger 6 + 2" 8 (mana u 0 0)

let blizzard_survivor_ignores_effects () =
  let u = new_game ~house:Sadness.house () in
  place u 0 "sadness.blizzard" 0;
  hit ~is_ability:true u 0 0 5;
  check_int "ability ignored" 12 (life_at u 0 0);
  hit u 0 0 8;
  check_int "combat lands" 4 (life_at u 0 0)

(* The wolf grows the fear house, which sits in slot 2 in standard mode. *)
let grey_wolf_grows_fear () =
  let u = new_game ~house:Sadness.house () in
  let before = growth u 0 2 in
  place u 0 "sadness.greywolf" 0;
  check_int "fear growth +1" (before + 1) (growth u 0 2)

let stag_halves_owner_damage () =
  let u = new_game ~house:Sadness.house () in
  place u 0 "sadness.stag" 0;
  let d = mk_damage ~is_spell:true 9 { ctx_player = 1; ctx_card = None; ctx_selected = 0 } in
  step u (fun () -> Player.inflict (player u 0) d);
  check_int "ceil(9/2) = 5" (60 - 5) (player_life u 0)

let shellycoat_absorbs_five () =
  let u = new_game ~house:Sadness.house () in
  place u 0 "sadness.shellycoat" 0;
  hit u 0 0 8;
  check_int "8 - 5 absorb" 13 (life_at u 0 0)

let tears_damage_and_drain () =
  let u = new_game ~mana:[ 4; 20; 4; 4 ] ~house:Sadness.house () in
  place u 0 "anger.boar" 0;
  place u 1 "anger.boar" 1;
  let before = mana u 1 0 in
  play u 0 "sadness.tears" ~input:None ~cost:8;
  check_int "own boar 26 - 15" 11 (life_at u 0 0);
  check_int "foe boar 26 - 15" 11 (life_at u 1 1);
  check_int "opponent anger -1" (before - 1) (mana u 1 0)

let banshee_gives_run () =
  let u = new_game ~house:Sadness.house () in
  place ~run:false u 0 "sadness.banshee" 1;
  place ~run:false u 0 "anger.werewolf" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "neighbour attacks on landing" (60 - 4) (player_life u 1)

let paindruid_heals_and_grows () =
  let u = new_game ~mana:[ 4; 7; 4; 4 ] ~house:Sadness.house () in
  (player u 0).pstate.ps_life <- 40;
  let before = growth u 0 1 in
  place u 0 "sadness.paindruid" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  settle u;
  check_int "heals 10" 50 (player_life u 0);
  check_int "sadness growth +1" (before + 1) (growth u 0 1);
  check_int "attack = sadness mana" 7 (attack_at u 0 0)

let fallen_angel_grows_everything () =
  let u = new_game ~house:Sadness.house () in
  let before = Array.init 4 (fun i -> growth u 0 i) in
  place u 0 "sadness.fallenangel" 0;
  Array.iteri
    (fun i b -> check_int (Printf.sprintf "house %d +1" i) (b + 1) (growth u 0 i))
    before

let soul_anchor_shrinks_opponent () =
  let u = new_game ~house:Sadness.house () in
  let before = growth u 1 0 in
  place u 0 "sadness.soulanchor" 0;
  check_int "opponent house 0 -1" (before - 1) (growth u 1 0)

let () =
  init_all_houses ();
  Alcotest.run "sadness"
    [
      ( "mode",
        [
          Alcotest.test_case "standard deals sadness" `Quick standard_deals_sadness;
          Alcotest.test_case "classic deals water" `Quick classic_deals_water;
        ] );
      ( "cards",
        [
          Alcotest.test_case "retirement" `Quick retirement_mana;
          Alcotest.test_case "living shadow" `Quick living_shadow_hurts_owner;
          Alcotest.test_case "scratched victim" `Quick scratched_victim_gives_anger;
          Alcotest.test_case "blizzard survivor" `Quick blizzard_survivor_ignores_effects;
          Alcotest.test_case "grey wolf" `Quick grey_wolf_grows_fear;
          Alcotest.test_case "ghostly stag" `Quick stag_halves_owner_damage;
          Alcotest.test_case "shellycoat" `Quick shellycoat_absorbs_five;
          Alcotest.test_case "rain of tears" `Quick tears_damage_and_drain;
          Alcotest.test_case "banshee" `Quick banshee_gives_run;
          Alcotest.test_case "pain druid" `Quick paindruid_heals_and_grows;
          Alcotest.test_case "fallen angel" `Quick fallen_angel_grows_everything;
          Alcotest.test_case "soul anchor" `Quick soul_anchor_shrinks_opponent;
        ] );
    ]
