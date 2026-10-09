(* Mechanics for the Technomage, the Sinist engineer.  The house plays as
   active defence: its bodies are towers that fire back when the opponent
   strikes them, and every Workpiece it summons is swapped for the tower of
   its owner's strongest element. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let growth u pid i = (player u pid).houses_update.hu_value.(i).growth
let special = special_house_index

let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)

(* A plain, non-ability hit from the other side of the board, the kind a
   creature's swing is.  The counter-firing walls also answer ability damage
   (a Boar's summon, a spell); see the guard tests. *)
let hit u owner num amount =
  step u (fun () ->
      Slot.inflict (slot u owner num)
        (mk_damage ~is_ability:false amount
           { ctx_player = 1 - owner; ctx_card = None; ctx_selected = num }))

let blast u owner num amount =
  step u (fun () ->
      Slot.inflict (slot u owner num)
        (mk_damage ~is_ability:true amount
           { ctx_player = 1 - owner; ctx_card = None; ctx_selected = num }))

let hurt_player u pid amount =
  step u (fun () ->
      Player.inflict (player u pid)
        (mk_damage ~is_ability:true amount { ctx_player = 1 - pid; ctx_card = None; ctx_selected = -1 }))

let kill u pid num = step u (fun () -> Slot.destroy (slot u pid num))

(* ------------------------------------------------------------ deck order *)

let cost_order () =
  init_all_houses ();
  let keys = List.map (fun (c : card) -> c.key) Technomage.house.h_cards in
  Alcotest.(check (list string))
    "card order"
    [
      "technomage.workpiece";
      "technomage.workpiece";
      "technomage.daggers";
      "technomage.repair";
      "technomage.distortion";
      "technomage.battery";
      "technomage.fortification";
      "technomage.doom";
    ]
    keys;
  Alcotest.(check (list int)) "costs" [ 2; 2; 3; 4; 5; 6; 7; 8 ]
    (List.map (fun (c : card) -> c.cost) Technomage.house.h_cards);
  Alcotest.(check (list int)) "cost function" [ 2; 2; 3; 4; 5; 6; 7; 8 ]
    (List.init 8 Technomage.cost_fn);
  (* The four towers are the body a Workpiece becomes, so they carry its price. *)
  Alcotest.(check (list int)) "tower costs" [ 2; 2; 2; 2 ]
    (List.map (fun (c : card) -> c.cost) Technomage.house.h_additional_cards)

(* ------------------------------------------------------------- the passive *)

let workpiece_becomes_the_strongest_tower () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.workpiece" 2;
  check_bool "the body turned at once" true (occupant u 0 2 = Some "technomage.flame");
  check_int "and keeps its fire life" 15 (life_at u 0 2);
  check_int "towers do not hit by themselves" 0 (attack_at u 0 2)

let the_tower_follows_the_mana () =
  let u = new_game ~special:Technomage.house ~mana:[ 4; 7; 5; 4 ] () in
  place u 0 "technomage.workpiece" 1;
  check_bool "the strongest power wins" true (occupant u 0 1 = Some "technomage.ice")

let playing_a_workpiece_transforms_it () =
  let u = new_game ~special:Technomage.house () in
  play u 0 "technomage.workpiece" ~input:(Some 4) ~cost:2;
  check_bool "turned on the way in" true (occupant u 0 4 = Some "technomage.flame")

let workpiece_builds_a_tower () =
  let u = new_game ~special:Technomage.house () in
  play u 0 "technomage.workpiece" ~input:(Some 3) ~cost:2;
  check_bool "the creature left a tower" true (occupant u 0 3 = Some "technomage.flame")

(* ----------------------------------------------------------------- towers *)

let ice_shell_softens_its_own_wounds () =
  let u = new_game ~special:Technomage.house ~mana:[ 4; 7; 4; 4 ] () in
  place u 0 "technomage.workpiece" 0;
  check_bool "water tower" true (occupant u 0 0 = Some "technomage.ice");
  let before = life_at u 0 0 in
  blast u 0 0 10;
  check_int "it shrugs off three" (before - 7) (life_at u 0 0)

let ice_shell_softens_the_owner () =
  let u = new_game ~special:Technomage.house ~mana:[ 4; 7; 4; 4 ] () in
  place u 0 "technomage.workpiece" 0;
  let before = player_life u 0 in
  hurt_player u 0 10;
  check_int "the owner is covered too" (before - 7) (player_life u 0)

let forest_heals_the_weaker_side () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.forest" 2;
  place u 0 "anger.werewolf" 1;
  blast u 0 1 5;
  check_int "the neighbour is hurt" 11 (life_at u 0 1);
  hit u 0 2 5;
  check_int "a bigger owner pays the neighbour" 16 (life_at u 0 1)

let forest_patches_the_owner_when_low () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.forest" 2;
  place u 0 "anger.dragon" 1;
  hurt_player u 0 20;
  let low = player_life u 0 in
  hit u 0 2 5;
  check_int "the wound heals the owner" (low + 5) (player_life u 0)

let raging_flame_burns_the_weakest () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.workpiece" 0;
  place u 1 "anger.werewolf" 0;
  place u 1 "anger.dragon" 1;
  on_end u 0;
  check_int "the weakest attacker is scorched" 11 (life_at u 1 0);
  check_int "the strong one is spared" 41 (life_at u 1 1)

let wall_of_daggers_bites_back () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.daggers" 2;
  place u 1 "anger.dragon" 2;
  place u 1 "anger.dragon" 4;
  let foe2 = life_at u 1 2 and foe4 = life_at u 1 4 and foe_life = player_life u 1 in
  hit u 0 2 3;
  check_int "every foe creature bleeds" (foe2 - 2) (life_at u 1 2);
  check_int "even the far one" (foe4 - 2) (life_at u 1 4);
  check_int "and the opponent takes the wall's attack" (foe_life - 2) (player_life u 1)

(* An ability wound is a wound too: the wall used to ignore is_ability damage
   to dodge a loop, which also swallowed a Boar's summon. *)
let wall_of_daggers_answers_abilities () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.daggers" 2;
  place u 1 "anger.dragon" 2;
  let foe = life_at u 1 2 and foe_life = player_life u 1 in
  blast u 0 2 3;
  check_int "an ability also draws blood" (foe - 2) (life_at u 1 2);
  check_int "and the opponent" (foe_life - 2) (player_life u 1)

(* The Boar's real summon, not a synthetic blast: its mass damage must make
   the wall bite. *)
let wall_of_daggers_answers_a_boar () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.daggers" 2;
  let wall = life_at u 0 2 and foe_life = player_life u 1 in
  play u 1 "anger.boar" ~input:(Some 4) ~cost:8;
  check_int "the boar's 5 is a wound" (wall - 5) (life_at u 0 2);
  check_int "and the wall answers the opponent" (foe_life - 2) (player_life u 1)

(* Wall of Daggers and Rocket Battery carry base attack, so they must NOT
   carry the base run flag: a wall that lands this turn waits for its owner's
   end-of-turn toggle_run.  Only the 0-attack towers keep the flag, so a tower
   that has been fed attack can still fire the turn it is built. *)
let walls_wait_a_turn () =
  let fresh key num =
    let u = new_game ~special:Technomage.house () in
    play u 0 key ~input:(Some num) ~cost:(card_by_key key).cost;
    match (slot u 0 num).value with Some st -> Slot.is_runnable st | None -> false
  in
  check_bool "a fresh wall cannot run" false (fresh "technomage.daggers" 2);
  check_bool "nor a fresh battery" false (fresh "technomage.battery" 3);
  check_bool "a tower keeps the run flag" true (fresh "technomage.workpiece" 4)

(* Two walls facing each other used to be the loop the ability guard bought;
   now the re-entrancy flag cuts the exchange after one answer. *)
let wall_of_daggers_stops_the_mirror () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.daggers" 0;
  place u 1 "technomage.daggers" 0;
  let mine = life_at u 0 0 and foe = life_at u 1 0 in
  let my_life = player_life u 0 and foe_life = player_life u 1 in
  blast u 0 0 3;
  check_int "my wall takes the blow and one counter" (mine - 3 - 2) (life_at u 0 0);
  check_int "the foe wall takes the 2" (foe - 2) (life_at u 1 0);
  check_int "both players bleed once" (my_life - 2) (player_life u 0);
  check_int "and only once" (foe_life - 2) (player_life u 1)

let wall_of_distortion_echoes () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.distortion" 2;
  place u 1 "anger.dragon" 0;
  place u 1 "anger.dragon" 3;
  place u 1 "anger.dragon" 5;
  let before = List.map (fun n -> life_at u 1 n) [ 0; 3; 5 ] in
  hit u 0 2 7;
  List.iteri
    (fun i n -> check_int "the whole enemy board echoes" (List.nth before i - 7) (life_at u 1 n))
    [ 0; 3; 5 ]

let wall_of_distortion_answers_abilities () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.distortion" 2;
  place u 1 "anger.dragon" 0;
  let foe = life_at u 1 0 in
  blast u 0 2 7;
  check_int "an ability echoes too" (foe - 7) (life_at u 1 0)

(* A lethal blow empties the slot before on_my_damage runs, so the reaction
   must not read the wall back off the board -- it fires with the refs. *)
let wall_of_daggers_dies_swinging () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.daggers" 2;
  place u 1 "anger.dragon" 2;
  step u (fun () -> ());
  let foe = life_at u 1 2 and foe_life = player_life u 1 in
  hit u 0 2 999;
  check_bool "the wall is gone" true (is_empty u 0 2);
  check_int "but still bleeds the foe" (foe - 2) (life_at u 1 2);
  check_int "and the opponent" (foe_life - 2) (player_life u 1)

let wall_of_distortion_dies_echoing () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.distortion" 2;
  place u 1 "anger.dragon" 0;
  step u (fun () -> ());
  let foe = life_at u 1 0 and wall = life_at u 0 2 in
  hit u 0 2 999;
  check_bool "the wall is gone" true (is_empty u 0 2);
  check_int "the echo still lands" (foe - wall) (life_at u 1 0)

let rocket_battery_ages () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.battery" 0;
  check_int "starts at nine" 9 (attack_at u 0 0);
  on_turn u 0;
  check_int "loses one a turn" 8 (attack_at u 0 0);
  on_turn u 0;
  check_int "and another" 7 (attack_at u 0 0)

let rocket_battery_strafes_whoever_strikes_a_neighbour () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.battery" 1;
  place u 0 "anger.dragon" 2;
  place u 1 "anger.dragon" 2;
  let foe = life_at u 1 2 in
  let bat = attack_at u 0 1 in
  hit u 0 2 3;
  check_int "the striker is punished" (foe - bat) (life_at u 1 2)

(* --------------------------------------------------------- Tower of Doom *)

let tower_of_doom_charges_on_death () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.doom" 0;
  List.iter
    (fun n ->
      place u 1 "anger.werewolf" n;
      kill u 1 n)
    [ 1; 2; 3; 4 ];
  check_int "four deaths, eight attack" 8 (attack_at u 0 0)

let tower_of_doom_cashes_in_at_end_of_turn () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.doom" 0;
  List.iter
    (fun n ->
      place u 1 "anger.werewolf" n;
      kill u 1 n)
    [ 1; 2; 3; 4 ];
  let before = player_life u 1 in
  on_end u 0;
  check_int "triple the charge" (before - 24) (player_life u 1);
  check_int "and it starts over" 0 (attack_at u 0 0)

let tower_of_doom_charges_on_spells () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "technomage.doom" 0;
  play u 1 "chaospriest.deepmeditation" ~input:None ~cost:0;
  check_int "a card is two charges" 2 (attack_at u 0 0)

let tower_of_doom_fills_the_workshop () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "anger.dragon" 5;
  play u 0 "technomage.doom" ~input:(Some 0) ~cost:8;
  check_bool "the doom itself landed" true (occupant u 0 0 = Some "technomage.doom");
  List.iter
    (fun n ->
      check_bool (Printf.sprintf "slot %d was built up" n) true
        (occupant u 0 n = Some "technomage.flame"))
    [ 1; 2; 3; 4 ];
  check_bool "the tenant stayed" true (occupant u 0 5 = Some "anger.dragon")

(* ------------------------------------------------------------ the spells *)

let mass_repair_heals_and_quickens () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "anger.dragon" 0;
  blast u 0 0 20;
  check_int "the dragon is hurt" 21 (life_at u 0 0);
  let before = Array.init 4 (fun i -> growth u 0 i) in
  play u 0 "technomage.repair" ~input:None ~cost:4;
  check_int "back to full" 41 (life_at u 0 0);
  Array.iteri
    (fun i g -> check_int (Printf.sprintf "power %d grows faster" i) (before.(i) + 1) (growth u 0 i))
    before

let fortification_rebuilds_the_elementals () =
  let u = new_game ~special:Technomage.house () in
  place u 0 "anger.dragon" 0;
  place u 0 "sadness.shadow" 1;
  let before = mana u 0 special in
  play u 0 "technomage.fortification" ~input:None ~cost:0;
  check_bool "fire becomes a flame tower" true (occupant u 0 0 = Some "technomage.flame");
  check_bool "water becomes an ice tower" true (occupant u 0 1 = Some "technomage.ice");
  check_int "one special mana per body" (before + 2) (mana u 0 special)

(* ------------------------------------------------------------------ run *)

let () =
  Alcotest.run "technomage"
    [
      ( "technomage",
        [
          Alcotest.test_case "cost order" `Quick cost_order;
          Alcotest.test_case "workpiece transforms" `Quick workpiece_becomes_the_strongest_tower;
          Alcotest.test_case "tower follows mana" `Quick the_tower_follows_the_mana;
          Alcotest.test_case "playing a workpiece" `Quick playing_a_workpiece_transforms_it;
          Alcotest.test_case "workpiece builds" `Quick workpiece_builds_a_tower;
          Alcotest.test_case "ice shell" `Quick ice_shell_softens_its_own_wounds;
          Alcotest.test_case "ice shell on the owner" `Quick ice_shell_softens_the_owner;
          Alcotest.test_case "green forest" `Quick forest_heals_the_weaker_side;
          Alcotest.test_case "green forest on the owner" `Quick forest_patches_the_owner_when_low;
          Alcotest.test_case "raging flame" `Quick raging_flame_burns_the_weakest;
          Alcotest.test_case "wall of daggers" `Quick wall_of_daggers_bites_back;
          Alcotest.test_case "wall of daggers answers abilities" `Quick wall_of_daggers_answers_abilities;
          Alcotest.test_case "wall of daggers answers a boar" `Quick wall_of_daggers_answers_a_boar;
          Alcotest.test_case "walls wait a turn" `Quick walls_wait_a_turn;
          Alcotest.test_case "wall of daggers mirror" `Quick wall_of_daggers_stops_the_mirror;
          Alcotest.test_case "wall of distortion" `Quick wall_of_distortion_echoes;
          Alcotest.test_case "wall of distortion answers abilities" `Quick wall_of_distortion_answers_abilities;
          Alcotest.test_case "wall of daggers dies swinging" `Quick wall_of_daggers_dies_swinging;
          Alcotest.test_case "wall of distortion dies echoing" `Quick wall_of_distortion_dies_echoing;
          Alcotest.test_case "rocket battery ages" `Quick rocket_battery_ages;
          Alcotest.test_case "rocket battery strafes" `Quick rocket_battery_strafes_whoever_strikes_a_neighbour;
          Alcotest.test_case "tower of doom charges" `Quick tower_of_doom_charges_on_death;
          Alcotest.test_case "tower of doom cashes in" `Quick tower_of_doom_cashes_in_at_end_of_turn;
          Alcotest.test_case "tower of doom hears spells" `Quick tower_of_doom_charges_on_spells;
          Alcotest.test_case "tower of doom builds" `Quick tower_of_doom_fills_the_workshop;
          Alcotest.test_case "mass repair" `Quick mass_repair_heals_and_quickens;
          Alcotest.test_case "fortification" `Quick fortification_rebuilds_the_elementals;
        ] );
    ]
