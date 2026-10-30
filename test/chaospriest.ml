(* Mechanics for the Chaos Priest: the dice-rolled spells and creatures. The
   engine's test rng is seeded, but the cards pick random slots, so the checks
   assert the shape of the outcome (exactly one victim, one slot blocked, …)
   rather than one particular throw. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let manas u pid = Array.init 5 (mana u pid) |> Array.to_list
let mana_sum u pid = List.fold_left ( + ) 0 (manas u pid)
let is_stunned u pid num =
  match (slot u pid num).value with Some st -> has_flag st stun_flag | None -> false
let custom_data u pid num =
  match (slot u pid num).value with
  | Some st -> ( match st.data with Custom s -> int_of_string_opt s | _ -> None)
  | None -> None
let hurt_creature u pid num amount =
  step u (fun () ->
      Slot.inflict
        (slot u pid num)
        (mk_damage ~is_ability:true amount { ctx_player = 1 - pid; ctx_card = None; ctx_selected = num }))
let hurt_player u pid amount =
  step u (fun () ->
      Player.inflict
        (player u pid)
        (mk_damage ~is_ability:true amount { ctx_player = 1 - pid; ctx_card = None; ctx_selected = -1 }))

(* ---------------------------------------------------------------- spells *)

let deep_meditation_blocks_one_growth () =
  let u = new_game ~special:Chaospriest.house () in
  let growths () =
    Array.to_list (Array.map (fun h -> h.growth) (player u 0).houses_update.hu_value)
  in
  let before = manas u 0 in
  play u 0 "chaospriest.deepmeditation" ~input:None ~cost:0;
  let g = growths () in
  check_int "one house stops growing" 1 (List.length (List.filter (fun x -> x = 0) g));
  List.iteri
    (fun i x ->
      if x = 0 then check_int "the chosen power keeps its mana" (List.nth before i) (mana u 0 i)
      else
        check_int (Printf.sprintf "power %d gains 1" i) (List.nth before i + 1) (mana u 0 i))
    g

let deep_meditation_skips_one_tick_then_expires () =
  let u = new_game ~special:Chaospriest.house () in
  let growths () =
    Array.to_list (Array.map (fun h -> h.growth) (player u 0).houses_update.hu_value)
  in
  let before = manas u 0 in
  play u 0 "chaospriest.deepmeditation" ~input:None ~cost:0;
  let g = growths () in
  on_end u 0;
  on_end u 1;
  step u (fun () -> Player.prepare_next_turn (player u 0));
  (* the blocked power skips the tick, the others grew once more on top of the
     direct +1 they already received *)
  List.iteri
    (fun i m ->
      if List.nth g i = 0 then check_int "blocked power skips a tick" (List.nth before i) m
      else check_int "others gain 2" (List.nth before i + 2) m)
    (manas u 0);
  on_turn u 0;
  check_bool "growth is back to normal" true (List.for_all (fun x -> x = 1) (growths ()))

let unstable_healing_heals_one_completely () =
  let u = new_game ~special:Chaospriest.house () in
  place u 0 "fire.dragon" 1;
  place u 0 "fire.goblin" 2;
  hurt_creature u 0 1 20;
  hurt_creature u 0 2 5;
  let deficit num =
    match (slot u 0 num).value with Some st -> st.max_life - st.life | None -> 0
  in
  let d1, d2 = (deficit 1, deficit 2) in
  play u 0 "chaospriest.unstablehealing" ~input:None ~cost:0;
  let d1', d2' = (deficit 1, deficit 2) in
  check_bool "one creature is fully healed" true
    ((d1', d2') = (0, d2) || (d1', d2') = (d1, 0))

let shockwave_stuns_all_but_one () =
  let u = new_game ~special:Chaospriest.house () in
  place u 1 "fire.goblin" 0;
  place u 1 "fire.goblin" 2;
  place u 1 "fire.dragon" 4;
  play u 0 "chaospriest.shockwave" ~input:None ~cost:0;
  let stunned = List.length (List.filter (fun n -> is_stunned u 1 n) [ 0; 2; 4 ]) in
  check_int "two of three stunned" 2 stunned;
  check_bool "nobody died" true (List.for_all (fun n -> Option.is_some (slot u 1 n).value) [ 0; 2; 4 ])

(* ------------------------------------------------------------- creatures *)

let healer_shrugs_off_damage () =
  let u = new_game ~special:Chaospriest.house () in
  play u 0 "chaospriest.selftaughthealer" ~input:(Some 2) ~cost:0;
  on_turn u 0;
  let r =
    match custom_data u 0 2 with
    | Some x when x >= 1 && x <= 5 -> x
    | _ -> Alcotest.fail "the healer rolled no reduction"
  in
  let before = player_life u 0 in
  hurt_player u 0 30;
  check_int "the wound is smaller" (30 - r) (before - player_life u 0)

let healer_needs_its_turn () =
  let u = new_game ~special:Chaospriest.house () in
  play u 0 "chaospriest.selftaughthealer" ~input:(Some 2) ~cost:0;
  let before = player_life u 0 in
  hurt_player u 0 30;
  check_int "no roll, no reduction" 30 (before - player_life u 0)

let healer_stops_when_gone () =
  let u = new_game ~special:Chaospriest.house () in
  play u 0 "chaospriest.selftaughthealer" ~input:(Some 2) ~cost:0;
  on_turn u 0;
  step u (fun () -> Slot.destroy (slot u 0 2));
  let before = player_life u 0 in
  hurt_player u 0 30;
  check_int "a dead healer heals nothing" 30 (before - player_life u 0)

let glider_moves_one_neighbour () =
  let u = new_game ~special:Chaospriest.house () in
  place u 0 "chaospriest.glider" 2;
  place u 0 "fire.goblin" 1;
  place u 0 "fire.monk" 3;
  on_turn u 0;
  let filled = List.length (List.filter (fun n -> Option.is_some (slot u 0 n).value) [ 0; 1; 2; 3; 4; 5 ]) in
  check_int "nobody was lost" 3 filled;
  check_bool "one neighbour left home" true
    (Option.is_none (slot u 0 1).value || Option.is_none (slot u 0 3).value)

let glider_keeps_the_board_legal () =
  let u = new_game ~special:Chaospriest.house () in
  place u 0 "chaospriest.glider" 2;
  place u 0 "fire.goblin" 1;
  step u (fun () -> Player.block_slot (player u 0) 4);
  on_turn u 0;
  check_bool "the blocked slot stayed empty" true (Option.is_none (slot u 0 4).value)

let witcher_drains_a_power () =
  let u = new_game ~special:Chaospriest.house () in
  play u 0 "chaospriest.witcher" ~input:(Some 2) ~cost:0;
  place u 1 "fire.dragon" 2;
  place u 1 "fire.goblin" 4;
  let sum = mana_sum u 1 in
  hurt_creature u 1 2 5;
  check_int "the wound costs a power point" (sum - 1) (mana_sum u 1);
  let sum = mana_sum u 1 in
  hurt_creature u 1 4 5;
  check_int "a wound elsewhere costs nothing" sum (mana_sum u 1)

let witcher_stops_when_gone () =
  let u = new_game ~special:Chaospriest.house () in
  play u 0 "chaospriest.witcher" ~input:(Some 2) ~cost:0;
  place u 1 "fire.dragon" 2;
  step u (fun () -> Slot.destroy (slot u 0 2));
  let sum = mana_sum u 1 in
  hurt_creature u 1 2 5;
  check_int "a dead witcher drains nothing" sum (mana_sum u 1)

let distorter_redirects_summons () =
  let u = new_game ~special:Chaospriest.house () in
  play u 0 "chaospriest.distorter" ~input:(Some 2) ~cost:0;
  play u 1 "fire.goblin" ~input:(Some 2) ~cost:0;
  let goblins =
    List.length
      (List.filter (fun n -> occupant u 1 n = Some "fire.goblin") [ 0; 1; 2; 3; 4; 5 ])
  in
  check_int "the summon landed somewhere" 1 goblins;
  check_bool "the unblocked board is intact" true (6 = List.length (player u 1).pstate.ps_slot_list)

let distorter_respects_blocked_slots () =
  let u = new_game ~special:Chaospriest.house () in
  play u 0 "chaospriest.distorter" ~input:(Some 2) ~cost:0;
  step u (fun () -> Player.block_slot (player u 1) 3);
  play u 1 "fire.goblin" ~input:(Some 2) ~cost:0;
  check_bool "the sealed slot stayed sealed" true (Option.is_none (slot u 1 3).value);
  check_bool "the summon stayed unblocked" true (5 = List.length (player u 1).pstate.ps_slot_list)

let distorter_redirects_spells () =
  let u = new_game ~special:Chaospriest.house () in
  place u 0 "fire.dragon" 0;
  place u 0 "fire.goblin" 3;
  place u 0 "fire.monk" 5;
  let life num = match (slot u 0 num).value with Some st -> st.life | None -> 0 in
  let before = List.map life [ 0; 3; 5 ] in
  play u 1 "trappers.rose" ~input:(Some 0) ~cost:0;
  let stunned = List.filter (fun n -> is_stunned u 0 n) [ 0; 3; 5 ] in
  check_int "one creature took the rose" 1 (List.length stunned);
  match stunned with
  | [ n ] ->
      let expected = List.nth before n - (List.nth before n + 1) / 2 in
      check_int "half its life, rounded up" expected (life n)
  | _ -> Alcotest.fail "the rose found no target"

let cannon_hits_three_on_entry () =
  let u = new_game ~special:Chaospriest.house () in
  place u 1 "fire.dragon" 0;
  place u 1 "fire.goblin" 3;
  place u 1 "fire.goblin" 4;
  place u 1 "fire.dragon" 5;
  play u 0 "chaospriest.unstablecannon" ~input:(Some 2) ~cost:0;
  let life num = match (slot u 1 num).value with Some st -> st.life | None -> 0 in
  let full num = match (slot u 1 num).value with Some st -> st.max_life | None -> 0 in
  let damage num = full num - life num in
  check_int "ten damage, three times" 30 (List.fold_left (fun acc n -> acc + damage n) 0 [ 0; 3; 4; 5 ]);
  check_bool "nobody died" true (List.for_all (fun n -> Option.is_some (slot u 1 n).value) [ 0; 3; 4; 5 ])

let cannon_seals_one_slot_per_turn () =
  let u = new_game ~special:Chaospriest.house () in
  play u 0 "chaospriest.unstablecannon" ~input:(Some 2) ~cost:0;
  on_turn u 0;
  check_int "one slot sealed" 5 (List.length (player u 1).pstate.ps_slot_list);
  on_turn u 0;
  check_int "still exactly one" 5 (List.length (player u 1).pstate.ps_slot_list);
  step u (fun () -> Slot.destroy (slot u 0 2));
  check_int "the seal dies with the cannon" 6 (List.length (player u 1).pstate.ps_slot_list)

let cannon_seal_blocks_summons () =
  let u = new_game ~special:Chaospriest.house () in
  play u 0 "chaospriest.unstablecannon" ~input:(Some 2) ~cost:0;
  on_turn u 0;
  let sealed =
    List.find (fun n -> not (List.mem n (player u 1).pstate.ps_slot_list)) [ 0; 1; 2; 3; 4; 5 ]
  in
  (* the summon chooser only ever offers open slots *)
  let open_slots = Slots.get_open (player u 1).slots_update in
  (* the summon chooser only ever offers open slots *)
  check_bool "the seal is never offered" true (not (List.mem sealed open_slots))

let cannon_seal_only_lands_on_an_empty_slot () =
  let u = new_game ~special:Chaospriest.house () in
  play u 0 "chaospriest.unstablecannon" ~input:(Some 2) ~cost:0;
  (* every slot but 3 is taken, so the dice have only one choice *)
  List.iter (fun n -> if n <> 3 then place u 1 "fire.dragon" n) [ 0; 1; 2; 4; 5 ];
  on_turn u 0;
  let sealed =
    List.filter (fun n -> not (List.mem n (player u 1).pstate.ps_slot_list)) [ 0; 1; 2; 3; 4; 5 ]
  in
  check_int "exactly one slot sealed" 1 (List.length sealed);
  check_int "the empty slot is the one sealed" 3 (List.hd sealed);
  check_bool "every tenant is alive" true
    (List.for_all (fun n -> Option.is_some (slot u 1 n).value) [ 0; 1; 2; 4; 5 ])

let cannon_leaves_a_full_board_alone () =
  let u = new_game ~special:Chaospriest.house () in
  play u 0 "chaospriest.unstablecannon" ~input:(Some 2) ~cost:0;
  List.iter (fun n -> place u 1 "fire.dragon" n) [ 0; 1; 2; 3; 4; 5 ];
  on_turn u 0;
  check_int "a full foe keeps all six slots" 6 (List.length (player u 1).pstate.ps_slot_list)

(* The life the opponent's creatures have left, dead ones counting as gone. *)
let lives u pid nums =
  List.map (fun n -> match (slot u pid n).value with Some st -> st.life | None -> 0) nums

(* The entry damage is announced for the animation: the front end lobs a bomb
   at each of the creatures the cannon hit. *)
let cannon_entry_announces_its_bombs () =
  let u = new_game ~special:Chaospriest.house () in
  place u 1 "fire.dragon" 0;
  place u 1 "fire.goblin" 3;
  place u 1 "fire.goblin" 4;
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  play u 0 "chaospriest.unstablecannon" ~input:(Some 2) ~cost:0;
  let bombs =
    List.filter_map
      (function
        | Ev_ability { kind; player = 0; num = 2; target = None } when String.starts_with ~prefix:"chaospriest.cannon.bomb" kind ->
            Some kind
        | _ -> None)
      !events
  in
  check_int "one salvo" 1 (List.length bombs);
  let hit =
    match bombs with
    | [ kind ] -> kind |> String.split_on_char '.' |> List.tl |> List.filter_map int_of_string_opt
    | _ -> []
  in
  (* the three creatures the cannon hit, in the order the dice picked them *)
  check_int "three targets" 3 (List.length hit);
  check_bool "the targets stand on the foe's board" true
    (List.for_all (fun n -> List.mem n [ 0; 3; 4 ]) hit);
  check_bool "no target twice" true (List.length (List.sort_uniq compare hit) = 3)

let cannon_entry_says_nothing_when_the_board_is_empty () =
  let u = new_game ~special:Chaospriest.house () in
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  play u 0 "chaospriest.unstablecannon" ~input:(Some 2) ~cost:0;
  check_bool "no bombs" false
    (List.exists
       (function Ev_ability { kind; _ } -> String.starts_with ~prefix:"chaospriest.cannon.bomb" kind | _ -> false)
       !events)

let () =
  Alcotest.run "chaospriest"
    [
      ( "chaospriest",
        [
          Alcotest.test_case "deep meditation blocks growth" `Quick deep_meditation_blocks_one_growth;
          Alcotest.test_case "deep meditation skips a tick" `Quick deep_meditation_skips_one_tick_then_expires;
          Alcotest.test_case "unstable healing" `Quick unstable_healing_heals_one_completely;
          Alcotest.test_case "shockwave" `Quick shockwave_stuns_all_but_one;
          Alcotest.test_case "healer" `Quick healer_shrugs_off_damage;
          Alcotest.test_case "healer needs its turn" `Quick healer_needs_its_turn;
          Alcotest.test_case "healer stops when gone" `Quick healer_stops_when_gone;
          Alcotest.test_case "glider" `Quick glider_moves_one_neighbour;
          Alcotest.test_case "glider legality" `Quick glider_keeps_the_board_legal;
          Alcotest.test_case "witcher" `Quick witcher_drains_a_power;
          Alcotest.test_case "witcher stops when gone" `Quick witcher_stops_when_gone;
          Alcotest.test_case "distorter summons" `Quick distorter_redirects_summons;
          Alcotest.test_case "distorter blocked slots" `Quick distorter_respects_blocked_slots;
          Alcotest.test_case "distorter spells" `Quick distorter_redirects_spells;
          Alcotest.test_case "cannon entry" `Quick cannon_hits_three_on_entry;
          Alcotest.test_case "cannon entry bombs" `Quick cannon_entry_announces_its_bombs;
          Alcotest.test_case "cannon entry without a foe" `Quick cannon_entry_says_nothing_when_the_board_is_empty;
          Alcotest.test_case "cannon seal" `Quick cannon_seals_one_slot_per_turn;
          Alcotest.test_case "cannon seal blocks" `Quick cannon_seal_blocks_summons;
          Alcotest.test_case "cannon seal empty only" `Quick cannon_seal_only_lands_on_an_empty_slot;
          Alcotest.test_case "cannon full foe" `Quick cannon_leaves_a_full_board_alone;
        ] );
    ]
