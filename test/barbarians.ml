(* Mechanics for the Barbarians house: every card wakes up below its owner's
   life threshold. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let ten = [ 10; 10; 10; 10; 10 ]
let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let hurt_player u pid n =
  step u (fun () ->
      Player.inflict (player u pid)
        (mk_damage ~mortal:false n { ctx_player = 1 - pid; ctx_card = None; ctx_selected = -1 }))

let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)

let run_at u pid num =
  step u (fun () -> Option.iter (fun st -> Player.run_slot (player u pid) num st) (slot u pid num).value)

let hit ?(is_spell = false) ?(cost = 0) u pid num amount =
  (* a copy, so the shared catalog card is never mutated *)
  let src = { (card_by_key "fire.goblin") with cost } in
  let ctx = { ctx_player = 1 - pid; ctx_card = Some src; ctx_selected = 0 } in
  step u (fun () -> Slot.inflict (slot u pid num) (mk_damage ~is_spell amount ctx))

(* ---------------------------------------------------------- axe warrior *)

let axe_warrior_wakes_below_eight () =
  let u = new_game ~special:Barbarians.house () in
  place u 0 "barbarians.axewarrior" 2;
  check_int "calm" 3 (attack_at u 0 2);
  hurt_player u 0 52;
  step u (fun () -> ());
  check_int "raging" 8 (attack_at u 0 2)

(* --------------------------------------------------------- dwarven hammer *)

let banned u pid key =
  let pd = (player u pid).pstate.ps_desc in
  let h =
    Option.get
      (Array.find_opt
         (fun h -> List.exists (fun c -> c.Sp.key = key) h.phd_house.Sp.h_cards)
         pd.init_desc.pd_houses)
  in
  let probe = [| { cd_card = card_by_key key; cd_cost = 0; cd_enabled = true; cd_extra_mana = 0 } |] in
  let out = List.fold_left (fun cs m -> m h cs) probe pd.desc_mods in
  not out.(0).cd_enabled

let hammer_hits_and_locks_spells () =
  let u = new_game ~special:Barbarians.house () in
  place u 1 "fire.goblin" 3;
  play u 0 "barbarians.dwarvenhammer" ~input:(Some 3) ~cost:0;
  check_int "goblin took 8" (16 - 8) (life_at u 1 3);
  check_bool "opponent locked out of spells" true (banned u 1 "fire.wave");
  check_bool "own spells untouched" false (banned u 0 "fire.wave");
  on_end u 1;
  check_bool "lock lifts at their end of turn" false (banned u 1 "fire.wave")

(* -------------------------------------------------------------- goliath *)

let goliath_grows_and_breaks () =
  let u = new_game ~special:Barbarians.house () in
  play u 0 "barbarians.goliath" ~input:(Some 2) ~cost:0;
  check_int "calm" 5 (attack_at u 0 2);
  hurt_player u 0 47;
  step u (fun () -> ());
  check_int "raging" 9 (attack_at u 0 2);
  (* a cheap opponent source doubles its damage *)
  hit ~cost:2 u 0 2 4;
  check_int "double from a level-2 source" (26 - 8) (life_at u 0 2)

(* The doubling only bites cheap opponent sources: a level-6 attacker passes
   through once. *)
let goliath_ignores_expensive_sources () =
  let u = new_game ~special:Barbarians.house () in
  play u 0 "barbarians.goliath" ~input:(Some 2) ~cost:0;
  hit ~cost:6 u 0 2 4;
  check_int "full damage, once" (26 - 4) (life_at u 0 2)

(* ------------------------------------------------------------- cerberus *)

let cerberus_sweeps_below_ten () =
  let u = new_game ~special:Barbarians.house () in
  place u 0 "barbarians.cerberus" 2;
  place u 1 "fire.goblin" 1;
  place u 1 "fire.goblin" 3;
  place u 1 "water.sprite" 4;
  hurt_player u 0 51;
  run_at u 0 2;
  check_int "first lane hit" (16 - 5) (life_at u 1 1);
  check_int "middle lane hit" (16 - 5) (life_at u 1 3);
  check_int "far lane hit" (22 - 5) (life_at u 1 4);
  check_int "player untouched" 60 (player_life u 1)

let cerberus_picks_one_above_ten () =
  let u = new_game ~special:Barbarians.house () in
  place u 0 "barbarians.cerberus" 2;
  place u 1 "fire.goblin" 1;
  place u 1 "fire.goblin" 2;
  run_at u 0 2;
  check_int "opposite lane hit" (16 - 5) (life_at u 1 2);
  check_int "other lane spared" 16 (life_at u 1 1)

(* -------------------------------------------------------- steaming bull *)

let bull_strikes_twice_below_fifteen () =
  let u = new_game ~special:Barbarians.house () in
  place u 0 "barbarians.steamingbull" 2;
  place u 1 "fire.dragon" 2;
  hurt_player u 0 46;
  run_at u 0 2;
  check_int "two blows" (41 - 12) (life_at u 1 2)

let bull_slows_above_fifteen () =
  let u = new_game ~special:Barbarians.house () in
  place u 0 "barbarians.steamingbull" 2;
  place u 1 "fire.dragon" 2;
  run_at u 0 2;
  check_int "one blow" (41 - 6) (life_at u 1 2)

(* ------------------------------------------------------------ ogre mage *)

let ogre_mage_heals_and_grows () =
  let u = new_game ~special:Barbarians.house () in
  play u 0 "barbarians.ogremage" ~input:(Some 2) ~cost:0;
  hit u 0 2 6;
  check_int "wounded" 22 (life_at u 0 2);
  hurt_player u 0 52;
  on_turn u 0;
  check_int "healed 8, capped at max" 28 (life_at u 0 2);
  check_int "attack +1" 6 (attack_at u 0 2);
  on_turn u 0;
  check_int "permanent, keeps growing" 7 (attack_at u 0 2)

let ogre_mage_sleeps_above_eight () =
  let u = new_game ~special:Barbarians.house () in
  play u 0 "barbarians.ogremage" ~input:(Some 2) ~cost:0;
  hit u 0 2 6;
  on_turn u 0;
  check_int "no heal" 22 (life_at u 0 2);
  check_int "no growth" 5 (attack_at u 0 2)

(* ------------------------------------------------------------- behemoth *)

let behemoth_hits_the_player_below_twenty () =
  let u = new_game ~special:Barbarians.house () in
  place u 0 "barbarians.behemoth" 2;
  place u 1 "fire.goblin" 2;
  hurt_player u 0 41;
  run_at u 0 2;
  check_int "player takes it" (60 - 7) (player_life u 1);
  check_int "creature spared" 16 (life_at u 1 2)

let behemoth_fights_normally_above_twenty () =
  let u = new_game ~special:Barbarians.house () in
  place u 0 "barbarians.behemoth" 2;
  place u 1 "fire.goblin" 2;
  run_at u 0 2;
  check_int "player spared" 60 (player_life u 1);
  check_int "creature takes it" (16 - 7) (life_at u 1 2)

(* ------------------------------------------------------- eldritch giant *)

let eldritch_giant_kills_a_big_one () =
  let u = new_game ~special:Barbarians.house () in
  place u 0 "barbarians.eldritchgiant" 2;
  place u 1 "water.golem" 1;
  place u 1 "fire.goblin" 2;
  hurt_player u 0 41;
  on_end u 0;
  check_bool "the big creature died" true (is_empty u 1 1);
  check_bool "the small one lives" false (is_empty u 1 2)

let eldritch_giant_sleeps_above_twenty () =
  let u = new_game ~special:Barbarians.house () in
  place u 0 "barbarians.eldritchgiant" 2;
  place u 1 "water.golem" 1;
  on_end u 0;
  check_bool "nothing died" false (is_empty u 1 1)

(* -------------------------------------------------------------- the deck *)

(* One card per quarter of the list is dealt, so with eight cards the deck is
   indices 0, 2, 4, 6 (Axe Warrior, Goliath, Steaming Bull, Behemoth). *)
let deck_is_ascending () =
  let keys = List.map (fun (c : card) -> c.key) Barbarians.house.h_cards in
  let costs = List.map (fun (c : card) -> c.cost) Barbarians.house.h_cards in
  check_int "eight cards" 8 (List.length keys);
  Alcotest.(check (list int)) "costs 1..8" [ 1; 2; 3; 4; 5; 6; 7; 8 ] costs;
  let rec asc = function a :: (b :: _ as rest) -> a <= b && asc rest | _ -> true in
  check_bool "ascending costs" true (asc costs);
  let u = new_game ~special:Barbarians.house () in
  let pd = (player u 0).pstate.ps_desc.init_desc in
  let h = Option.get (Array.find_opt (fun h -> h.phd_house.h_key = "barbarians") pd.pd_houses) in
  let dealt = Array.to_list h.phd_cards |> List.map (fun cd -> cd.cd_card.key) in
  Alcotest.(check (list string))
    "quarter picks"
    [ "barbarians.axewarrior"; "barbarians.goliath"; "barbarians.steamingbull"; "barbarians.behemoth" ]
    dealt

let () =
  Alcotest.run "barbarians"
    [
      ( "barbarians",
        [
          Alcotest.test_case "axe warrior" `Quick axe_warrior_wakes_below_eight;
          Alcotest.test_case "dwarven hammer" `Quick hammer_hits_and_locks_spells;
          Alcotest.test_case "goliath" `Quick goliath_grows_and_breaks;
          Alcotest.test_case "goliath cheap only" `Quick goliath_ignores_expensive_sources;
          Alcotest.test_case "cerberus sweep" `Quick cerberus_sweeps_below_ten;
          Alcotest.test_case "cerberus single" `Quick cerberus_picks_one_above_ten;
          Alcotest.test_case "steaming bull twice" `Quick bull_strikes_twice_below_fifteen;
          Alcotest.test_case "steaming bull once" `Quick bull_slows_above_fifteen;
          Alcotest.test_case "ogre mage" `Quick ogre_mage_heals_and_grows;
          Alcotest.test_case "ogre mage asleep" `Quick ogre_mage_sleeps_above_eight;
          Alcotest.test_case "behemoth player" `Quick behemoth_hits_the_player_below_twenty;
          Alcotest.test_case "behemoth normal" `Quick behemoth_fights_normally_above_twenty;
          Alcotest.test_case "eldritch giant" `Quick eldritch_giant_kills_a_big_one;
          Alcotest.test_case "eldritch giant asleep" `Quick eldritch_giant_sleeps_above_twenty;
          Alcotest.test_case "deck" `Quick deck_is_ascending;
        ] );
    ]
