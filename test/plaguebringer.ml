(* Mechanics for the Plaguebringers: the disease counters, the producers that
   pile them up and the consumers that spend them.  A counter lives in the
   infecting player's data, keyed by the victim's slot_id, so [counter_on]
   reads the infector's map through the living victim's slot. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

let pcounters u pid = match (player u pid).pstate.ps_data with Plague_data xs -> xs | _ -> []

let slot_id u pid num = Option.map (fun st -> st.slot_id) (slot u pid num).value

(* The counters player [infector] has put on the creature at [victim]/[num]. *)
let counter_on u infector victim num =
  match slot_id u victim num with
  | None -> 0
  | Some id -> Option.value (List.assoc_opt id (pcounters u infector)) ~default:0

(* 1 damage from player 0's Plaguebringer board, so the passive marks the
   creature.  The card context is what tells the house apart. *)
let plague_card = card_by_key "plaguebringer.locustcarrier"

let hurt_enemy u amount num =
  step u (fun () ->
      Slot.inflict (slot u 1 num)
        (mk_damage ~is_ability:true amount
           { ctx_player = 0; ctx_card = Some plague_card; ctx_selected = -1 }))

let run_at u pid num =
  step u (fun () ->
      Option.iter (fun st -> Player.run_slot (player u pid) num st) (slot u pid num).value)

let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)

let special_mana u pid = (player u pid).houses_update.hu_value.(special_house_index).mana

(* ---------------------------------------------------------------- passive *)

(* A basic-class creature carries no plague: its blow leaves no counter. *)
let basic_creature_leaves_no_mark () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 0 "anger.oni" 2;
  place u 1 "anger.oni" 2;
  run_at u 0 2;
  check_int "the victim lost 4 life" 15 (life_at u 1 2);
  check_int "but no disease counter" 0 (counter_on u 0 1 2)

(* A Plaguebringer creature does. *)
let plaguebringer_creature_marks () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 0 "plaguebringer.locustcarrier" 2;
  place u 1 "anger.oni" 2;
  run_at u 0 2;
  check_int "the victim lost 3 life" 16 (life_at u 1 2);
  check_int "and gained one counter" 1 (counter_on u 0 1 2)

(* Spells mark whatever their house: a basic Anger spell spreads disease. *)
let basic_spell_marks () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 1 "anger.gorilla" 2;
  play u 0 "anger.inferno" ~input:(Some 2) ~cost:0;
  check_int "the gorilla lost 18 life" 2 (life_at u 1 2);
  check_int "one disease counter" 1 (counter_on u 0 1 2)

let spell_marks () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 1 "anger.oni" 2;
  play u 0 "plaguebringer.infect" ~input:(Some 2) ~cost:0;
  check_int "6 damage" 13 (life_at u 1 2);
  check_int "passive + Infect bonus" 2 (counter_on u 0 1 2)

let zero_damage_marks_nothing () =
  let u = new_game ~special:Plaguebringer.house () in
  (* the Conjurer's golem turns every effect to 0 damage *)
  place u 1 "conjurer.golem" 2;
  play u 0 "plaguebringer.infect" ~input:(Some 2) ~cost:0;
  check_int "untouched" 18 (life_at u 1 2);
  check_int "no counter for a wound that never was" 0 (counter_on u 0 1 2)

(* ---------------------------------------------------------------- infest *)

let infest_low_sets_three () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 1 "anger.oni" 2;
  play u 0 "plaguebringer.infect" ~input:(Some 2) ~cost:0;
  play u 0 "plaguebringer.infest" ~input:(Some 2) ~cost:0;
  check_int "12 more damage" 1 (life_at u 1 2);
  check_int "the counters are set to 3" 3 (counter_on u 0 1 2);
  check_bool "stunned" true
    (match (slot u 1 2).value with Some st -> has_flag st stun_flag | None -> false)

let infest_high_consumes () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 1 "anger.oni" 2;
  hurt_enemy u 1 2;
  hurt_enemy u 1 2;
  hurt_enemy u 1 2;
  check_int "3 counters built up" 3 (counter_on u 0 1 2);
  check_int "16 life" 16 (life_at u 1 2);
  play u 0 "plaguebringer.infest" ~input:(Some 2) ~cost:0;
  check_int "5 per counter" 1 (life_at u 1 2);
  check_int "the burn left its own mark" 1 (counter_on u 0 1 2)

(* ------------------------------------------------------------- consumers *)

let soul_leech_bites () =
  let u = new_game ~special:Plaguebringer.house () in
  play u 0 "plaguebringer.soulleech" ~input:(Some 2) ~cost:0;
  place u 1 "anger.oni" 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  check_int "3 counters" 3 (counter_on u 0 1 1);
  let before = player_life u 1 in
  hurt_enemy u 20 1;
  check_bool "the creature is gone" true (is_empty u 1 1);
  check_int "the leech drank the 4 counters (3 + the fatal one)" (before - 4) (player_life u 1)

let soul_leech_reads_fatal_counters () =
  (* The Priest of Fire example: the killing blow's counters count before the
     creature dies, so the leech reads 3 + 2 = 5. *)
  let u = new_game ~special:Plaguebringer.house () in
  play u 0 "plaguebringer.soulleech" ~input:(Some 2) ~cost:0;
  place u 1 "antimancer.0.name" 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  check_int "3 counters on 6 life" 3 (counter_on u 0 1 1);
  check_int "6 life left" 6 (life_at u 1 1);
  let before = player_life u 1 in
  play u 0 "plaguebringer.infect" ~input:(Some 1) ~cost:0;
  check_bool "Infect killed it" true (is_empty u 1 1);
  check_int "3 + 2 counters dealt" (before - 5) (player_life u 1)

let broodmother_power () =
  let u = new_game ~special:Plaguebringer.house () in
  play u 0 "plaguebringer.broodmother" ~input:(Some 2) ~cost:0;
  place u 1 "anger.oni" 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  let before = special_mana u 0 in
  hurt_enemy u 20 1;
  check_int "4 counters became Disease power" (before + 4) (special_mana u 0)

let desecrator_consumes () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 1 "anger.oni" 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  check_int "2 counters" 2 (counter_on u 0 1 1);
  play u 0 "plaguebringer.desecrator" ~input:(Some 3) ~cost:0;
  check_int "17 - 10 life" 7 (life_at u 1 1);
  check_int "only the burn's own mark stays" 1 (counter_on u 0 1 1)

let titanclaw_crushes () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 1 "anger.oni" 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  place u 1 "anger.oni" 2;
  play u 0 "plaguebringer.titanclaw" ~input:(Some 3) ~cost:0;
  check_bool "3 counters is enough" true (is_empty u 1 1);
  check_bool "the clean one lives" false (is_empty u 1 2)

(* --------------------------------------------------------------- minders *)

let injector_turn_cloud () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 0 "plaguebringer.flyinginjector" 0;
  place u 1 "anger.oni" 1;
  on_turn u 0;
  check_int "1 damage" 18 (life_at u 1 1);
  check_int "and a counter" 1 (counter_on u 0 1 1)

let injector_end_burns () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 0 "plaguebringer.flyinginjector" 0;
  place u 1 "anger.dragon" 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  hurt_enemy u 1 1;
  check_int "4 counters on the dragon" 4 (counter_on u 0 1 1);
  check_int "37 life before the burn" 37 (life_at u 1 1);
  on_end u 0;
  check_int "4 per counter, 16 off 37" 21 (life_at u 1 1);
  check_int "the burn re-marks it" 1 (counter_on u 0 1 1)

let locust_carrier_strafes () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 1 "anger.oni" 1;
  place u 1 "anger.oni" 3;
  play u 0 "plaguebringer.locustcarrier" ~input:(Some 1) ~cost:0;
  check_int "the blocked creature takes 3" 16 (life_at u 1 1);
  check_int "and so does its neighbour" 16 (life_at u 1 3)

let spread_on_infect_death () =
  let u = new_game ~special:Plaguebringer.house () in
  place u 1 "anger.hotspot" 1;
  place u 1 "anger.oni" 3;
  play u 0 "plaguebringer.infect" ~input:(Some 1) ~cost:0;
  check_bool "the 5-life creature died" true (is_empty u 1 1);
  check_int "its 2 counters moved to the survivor" 2 (counter_on u 0 1 3)

let () =
  Alcotest.run "plaguebringer"
    [
      ( "plaguebringer",
        [
          Alcotest.test_case "basic creature" `Quick basic_creature_leaves_no_mark;
          Alcotest.test_case "plaguebringer creature" `Quick plaguebringer_creature_marks;
          Alcotest.test_case "basic spell" `Quick basic_spell_marks;
          Alcotest.test_case "spell marks" `Quick spell_marks;
          Alcotest.test_case "zero damage" `Quick zero_damage_marks_nothing;
          Alcotest.test_case "infest low" `Quick infest_low_sets_three;
          Alcotest.test_case "infest high" `Quick infest_high_consumes;
          Alcotest.test_case "soul leech" `Quick soul_leech_bites;
          Alcotest.test_case "soul leech fatal counters" `Quick soul_leech_reads_fatal_counters;
          Alcotest.test_case "broodmother" `Quick broodmother_power;
          Alcotest.test_case "desecrator" `Quick desecrator_consumes;
          Alcotest.test_case "titanclaw" `Quick titanclaw_crushes;
          Alcotest.test_case "injector turn" `Quick injector_turn_cloud;
          Alcotest.test_case "injector end" `Quick injector_end_burns;
          Alcotest.test_case "locust carrier" `Quick locust_carrier_strafes;
          Alcotest.test_case "infect spread" `Quick spread_on_infect_death;
        ] );
    ]
