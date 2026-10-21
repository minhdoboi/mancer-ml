(* Mechanics for the Trappers. *)

open Mancer
open Sp
open Engine
open Common
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

let flagged u pid num = has_flag (Option.get (slot u pid num).value) stun_flag
let stunned u pid num = flagged u pid num
let slot_state_data u pid num = Option.map (fun st -> st.data) (slot u pid num).value
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let run_at u pid num = step u (fun () -> Option.iter (fun st -> Player.run_slot (player u pid) num st) (slot u pid num).value)
let kill u pid num =
  step u (fun () ->
      Slot.inflict (slot u pid num)
        (mk_damage 1000 { ctx_player = 1 - pid; ctx_card = None; ctx_selected = num }))

(* ------------------------------------------------------------- whip master *)

let whip_master_punishes_summons () =
  let u = new_game ~special:Trappers.house () in
  play u 0 "trappers.whipmaster" ~input:(Some 2) ~cost:2;
  check_int "no damage on own summon" 60 (player_life u 1);
  place u 1 "fire.goblin" 2;
  check_int "opponent hit for 5" 55 (player_life u 1);
  place u 1 "fire.monk" 3;
  check_int "hit again" 50 (player_life u 1)

let whip_master_stops_when_dead () =
  let u = new_game ~special:Trappers.house () in
  play u 0 "trappers.whipmaster" ~input:(Some 2) ~cost:2;
  kill u 0 2;
  place u 1 "fire.goblin" 2;
  check_int "no damage" 60 (player_life u 1)

(* --------------------------------------------------------- psychic shaman *)

let psychic_shaman_stuns_opposite () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "trappers.psychicshaman" ~input:(Some 2) ~cost:1;
  check_bool "opposite stunned" true (stunned u 1 2);
  run_at u 1 2;
  check_int "goblin did not attack" 60 (player_life u 0);
  check_bool "stun still there after the skipped run" true (stunned u 1 2);
  step u (fun () -> Slots.toggle_run (player u 1).slots_update);
  check_bool "stun gone after the owner's end of turn" true (not (stunned u 1 2))

let psychic_shaman_attacks_immediately () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "trappers.psychicshaman" ~input:(Some 2) ~cost:1;
  run_at u 0 2;
  check_int "shaman attacks the turn it is summoned" 12 (life_at u 1 2);
  step u (fun () -> Slots.toggle_run (player u 0).slots_update);
  run_at u 0 2;
  check_int "shaman hits for 4 again next turn" 8 (life_at u 1 2)

(* -------------------------------------------------------- underworld rose *)

let rose_stuns_and_hits_for_half_life () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.blargl" 3;
  play u 0 "trappers.rose" ~input:(Some 3) ~cost:3;
  check_int "blargl (26 life) takes ceil(26/2) = 13" 13 (life_at u 1 3);
  check_bool "blargl stunned" true (stunned u 1 3)

let rose_damage_follows_the_life () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.drake" 2;
  play u 0 "trappers.rose" ~input:(Some 2) ~cost:3;
  check_int "drake (18 life) takes ceil(18/2) = 9" 9 (life_at u 1 2)

(* ------------------------------------------------------------ dark pixie *)

let pixie_locks_opposite () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "trappers.darkpixie" ~input:(Some 2) ~cost:4;
  check_bool "opposite stunned as soon as the pixie lands" true (stunned u 1 2);
  run_at u 1 2;
  check_int "goblin could not attack" 60 (player_life u 0);
  (* the opponent's own end-of-turn toggle_run wipes the stun, but the Pixie
     puts it back on its owner's turn, before the opponent's creatures run *)
  step u (fun () -> Slots.toggle_run (player u 1).slots_update);
  check_bool "stun gone after the owner's end of turn" true (not (stunned u 1 2));
  on_turn u 0;
  check_bool "stunned again by the pixie" true (stunned u 1 2);
  run_at u 1 2;
  check_int "still held next turn" 60 (player_life u 0)

let pixie_holds_newcomer () =
  let u = new_game ~special:Trappers.house () in
  play u 0 "trappers.darkpixie" ~input:(Some 2) ~cost:4;
  place u 1 "fire.goblin" 2;
  check_bool "newcomer in the opposing slot is held" true (stunned u 1 2);
  run_at u 1 2;
  check_int "newcomer could not attack" 60 (player_life u 0)

let pixie_lock_ends_with_death () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "trappers.darkpixie" ~input:(Some 2) ~cost:4;
  kill u 0 2;
  check_bool "the hold is gone once the pixie is gone" true (not (stunned u 1 2));
  step u (fun () -> Slots.toggle_run (player u 1).slots_update);
  run_at u 1 2;
  check_int "goblin attacks again" 56 (player_life u 0)

(* The whole round: the hold is the same as the pause it replaces, the
   creature in the opposing slot never gets a run. *)
let pixie_holds_through_a_round () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "trappers.darkpixie" ~input:(Some 2) ~cost:4;
  (* the chain of a turn's end, as Session.drain runs it *)
  let turn pid =
    step u (fun () -> Player.run_slots (player u pid));
    step u (fun () -> Player.apply_effects (player u pid) On_end_turn);
    step u (fun () -> Slots.toggle_run (player u pid).slots_update);
    step u (fun () -> Player.apply_effects (player u (1 - pid)) On_turn)
  in
  turn 1;
  check_int "no attack on the opponent's turn" 60 (player_life u 0);
  turn 0;
  turn 1;
  check_int "no attack on the next opponent turn either" 60 (player_life u 0)

(* ---------------------------------------------------- underground poison *)

let poison_destroys_and_refunds () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.elemental" 2;
  place u 1 "fire.goblin" 4;
  (* cost 10, refund 7 *)
  check_int "elemental alive" 36 (life_at u 1 2);
  play u 0 "trappers.undergroundpoison" ~input:(Some 2) ~cost:5;
  check_bool "elemental destroyed" true (is_empty u 1 2);
  check_int "opponent gains cost - 3" 13 (mana u 1 0);
  check_int "the other creature is untouched" 16 (life_at u 1 4)

let poison_refund_is_never_negative () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.goblin" 2;
  (* cost 1, refund max 0 (1 - 3) = 0 *)
  play u 0 "trappers.undergroundpoison" ~input:(Some 2) ~cost:5;
  check_bool "goblin destroyed" true (is_empty u 1 2);
  check_int "no mana granted" 6 (mana u 1 0)

(* ------------------------------------------------------ poisonous roots *)

let roots_hold_one_turn_and_reduce_powers () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.goblin" 1;
  place u 1 "fire.monk" 4;
  play u 0 "trappers.roots" ~input:None ~cost:6;
  check_bool "slot 1 held" true (flagged u 1 1);
  check_bool "slot 4 held" true (flagged u 1 4);
  check_int "fire power reduced by 1" 5 (mana u 1 0);
  check_int "water power reduced by 1" 3 (mana u 1 1);
  run_at u 1 1;
  run_at u 1 4;
  check_int "nobody attacked" 60 (player_life u 0);
  on_end u 1;
  check_bool "released after the turn" true (not (flagged u 1 1) && not (flagged u 1 4));
  step u (fun () -> Slots.toggle_run (player u 1).slots_update);
  run_at u 1 1;
  run_at u 1 4;
  check_int "goblin 4 + monk 3 next turn" 53 (player_life u 0)

(* ----------------------------------------------------------- mind witch *)

let witch_silences_opposite () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "trappers.mindwitch" ~input:(Some 2) ~cost:7;
  check_bool "goblin bridled" true (is_bridled (Option.get (slot u 1 2).value))

let witch_silences_later_arrivals () =
  let u = new_game ~special:Trappers.house () in
  play u 0 "trappers.mindwitch" ~input:(Some 2) ~cost:7;
  place u 1 "fire.goblin" 2;
  check_bool "newcomer bridled too" true (is_bridled (Option.get (slot u 1 2).value))

let witch_release_on_death () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "trappers.mindwitch" ~input:(Some 2) ~cost:7;
  kill u 0 2;
  check_bool "goblin's ability restored"
    true
    (match slot_state_data u 1 2 with Some No_data -> true | _ -> false)

(* -------------------------------------------------------- psychic medusa *)

let medusa_stuns_highest_life () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.goblin" 1;
  place u 1 "fire.blargl" 4;
  play u 0 "trappers.psychicmedusa" ~input:(Some 2) ~cost:8;
  on_end u 0;
  check_bool "goblin free" true (not (stunned u 1 1));
  check_bool "blargl (26 life) stunned" true (stunned u 1 4)

let medusa_lock_is_each_turn () =
  let u = new_game ~special:Trappers.house () in
  place u 1 "fire.blargl" 2;
  play u 0 "trappers.psychicmedusa" ~input:(Some 2) ~cost:8;
  on_end u 0;
  check_bool "stunned first turn" true (stunned u 1 2);
  (* the opponent's end of turn clears it; the Medusa re-stuns next round *)
  step u (fun () -> Slots.toggle_run (player u 1).slots_update);
  check_bool "cleared by the owner's end of turn" true (not (stunned u 1 2));
  on_end u 0;
  check_bool "stunned again next turn" true (stunned u 1 2)

let () =
  Alcotest.run "houses_h"
    [
      ( "trappers",
        [
          Alcotest.test_case "whip master" `Quick whip_master_punishes_summons;
          Alcotest.test_case "whip master dies" `Quick whip_master_stops_when_dead;
          Alcotest.test_case "psychic shaman stun" `Quick psychic_shaman_stuns_opposite;
          Alcotest.test_case "psychic shaman attacks at once" `Quick psychic_shaman_attacks_immediately;
          Alcotest.test_case "rose" `Quick rose_stuns_and_hits_for_half_life;
          Alcotest.test_case "rose follows the life" `Quick rose_damage_follows_the_life;
          Alcotest.test_case "pixie locks" `Quick pixie_locks_opposite;
          Alcotest.test_case "pixie holds newcomer" `Quick pixie_holds_newcomer;
          Alcotest.test_case "pixie dies" `Quick pixie_lock_ends_with_death;
          Alcotest.test_case "pixie holds a round" `Quick pixie_holds_through_a_round;
          Alcotest.test_case "poison refund" `Quick poison_destroys_and_refunds;
          Alcotest.test_case "poison min refund" `Quick poison_refund_is_never_negative;
          Alcotest.test_case "roots" `Quick roots_hold_one_turn_and_reduce_powers;
          Alcotest.test_case "witch silences" `Quick witch_silences_opposite;
          Alcotest.test_case "witch silences newcomers" `Quick witch_silences_later_arrivals;
          Alcotest.test_case "witch releases" `Quick witch_release_on_death;
          Alcotest.test_case "medusa picks highest" `Quick medusa_stuns_highest_life;
          Alcotest.test_case "medusa repeats" `Quick medusa_lock_is_each_turn;
        ] );
    ]
