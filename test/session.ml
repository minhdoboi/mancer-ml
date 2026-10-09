(* Session API and the event stream a UI listens to. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)

let fire_desc () =
  init_all_houses ();
  let d = Game.make_player_desc Fire.house in
  { gd_players = [| d; d |] }

(* Records every event; [events ()] returns them oldest first *)
let recorder () =
  let log = ref [] in
  ((fun e -> log := e :: !log), fun () -> List.rev !log)

let greedy s =
  match Session.status s with
  | Session.To_move pid -> Game.bot_move (Session.updater s) (Updater.player (Session.updater s) pid)
  | Session.Over _ -> None

let rec play_out s =
  match Session.status s with
  | Session.Over _ -> ()
  | Session.To_move _ ->
      Session.submit s (greedy s);
      play_out s

(* ---------------------------------------------------------------- session *)

let announces_turns () =
  let on_event, events = recorder () in
  let s = Session.create ~on_event (fire_desc ()) in
  (match events () with
  | [ Ev_to_move { t = 1; player = 0 } ] -> ()
  | _ -> Alcotest.fail "first event should announce player 0, move 1");
  Session.submit s (greedy s);
  (match List.find_opt (function Ev_submitted _ -> true | _ -> false) (events ()) with
  | Some (Ev_submitted { t = 1; player = 0; command = Some _ }) -> ()
  | _ -> Alcotest.fail "player 0's move 1 is reported");
  (match List.rev (events ()) with
  | Ev_to_move { t = 2; player = 1 } :: _ -> ()
  | _ -> Alcotest.fail "the hand-over is the last event");
  check_int "one move" 1 (Session.turns s);
  check_int "one record" 1 (List.length (Session.moves s))

let game_over_once () =
  let on_event, events = recorder () in
  let s = Session.create ~max_turns:5 ~on_event (fire_desc ()) in
  play_out s;
  (match Session.status s with
  | Session.Over { winner = None; reason } -> check_string "reason" "maxTurns" reason
  | _ -> Alcotest.fail "capped game should end without a winner");
  check_int "game over sent once" 1
    (List.length (List.filter (function Ev_game_over _ -> true | _ -> false) (events ())));
  check_bool "no legal moves" true (Session.legal_commands s = []);
  Alcotest.check_raises "submit after the end" (Invalid_argument "Session.submit: the game is over") (fun () ->
      Session.submit s None)

let rejects_wrong_player () =
  let s = Session.create (fire_desc ()) in
  match Session.legal_commands s with
  | [] -> Alcotest.fail "player 0 should have moves"
  | c :: _ ->
      Alcotest.check_raises "player 1's command on player 0's move"
        (Invalid_argument "Session.submit: player 0 to move, got a command for 1") (fun () ->
          Session.submit s (Some { c with cmd_player = 1 }))

(* A UI that reads the board on every event must not change the game: peek
   does not flush deaths, so this seeded Disciples game plays out the same *)
let listener_does_not_change_game () =
  Catalog.init ();
  let seed = 1 in
  let r = Replay.generate ~special:"disciples" ~max_turns:60 ~seed () in
  let desc = Replay.desc_of (Catalog.default ()) (Game_record.of_json (Game_record.to_json r)) in
  let peeks = ref 0 in
  let s_ref = ref None in
  let on_event = function
    | Ev_refresh _ | Ev_die _ ->
        Option.iter (fun s -> ignore (Session.peek s)) !s_ref;
        incr peeks
    | _ -> ()
  in
  let s = Session.create ~mana0:r.mana.(0) ~mana1:r.mana.(1) ~rng:(Dice.make seed) ~max_turns:60 ~on_event desc in
  s_ref := Some s;
  play_out s;
  check_bool "peeked" true (!peeks > 0);
  check_int "same length" (List.length r.moves) (List.length (Session.moves s));
  check_bool "same moves" true (r.moves = Session.moves s)

(* Greedy used to submit Simoom and Scarecrow without a slot (index -1) *)
let custom_targets_get_a_slot () =
  Catalog.init ();
  List.iter
    (fun special ->
      for seed = 1 to 5 do
        ignore (Replay.generate ~special ~seed ())
      done)
    [ "wind"; "disciples" ]

(* Bot.scala:229: a target-slot card is offered the opponent's open slots.
   No ported card uses this input yet, so the test brings its own spell. *)
let target_slot_offers_open_slots () =
  init_all_houses ();
  let probe =
    Common.spell ~key:"test.probe" ~description:"" ~input_spec:Select_target_slot ~effects:Engine.no_effects ()
  in
  probe.cost <- 1;
  probe.house_index <- 0;
  let cd = { cd_card = probe; cd_cost = 1; cd_enabled = true; cd_extra_mana = 0 } in
  let d = { pd_houses = [| { phd_house = Fire.house; phd_cards = [| cd |] } |] } in
  let s = Session.create { gd_players = [| d; d |] } in
  let u = Session.updater s in
  place u 1 "fire.goblin" 0;
  place u 1 "fire.goblin" 3;
  let slots = List.filter_map (fun c -> c.cmd_input) (Session.legal_commands s) in
  Alcotest.(check (list int)) "opponent's open slots" [ 1; 2; 4; 5 ] (List.sort compare slots)

(* ------------------------------------------------------------ engine events *)

let listen u =
  let on_event, events = recorder () in
  Updater.listen u on_event;
  events

let run_slot_and_die () =
  let u = new_game () in
  place u 0 "fire.goblin" 0;
  place u 1 "fire.goblin" 2;
  let events = listen u in
  step u (fun () -> Player.run_slots (player u 0));
  check_bool "run slot" true (List.mem (Ev_run_slot { num = 0; player = 0 }) (events ()));
  step u (fun () ->
      Slot.inflict (slot u 1 2) (mk_damage 100 { ctx_player = 0; ctx_card = None; ctx_selected = no_selection }));
  check_bool "die" true (List.mem (Ev_die { num = 2; player = 1 }) (events ()))

let move_event () =
  let u = new_game () in
  place u 0 "fire.goblin" 0;
  let events = listen u in
  step u (fun () -> Slots.move (player u 0).slots_update 0 3);
  check_bool "move" true (List.mem (Ev_move { num = 0; dest = 3; player = 0; dest_player = 0 }) (events ()))

let summon_and_refresh () =
  let u = new_game () in
  let events = listen u in
  play u 0 "fire.goblin" ~input:(Some 1) ~cost:1;
  let evs = events () in
  check_bool "summon" true
    (List.exists (function Ev_summon { num = 1; player = 0; creature } -> creature.c_card.key = "fire.goblin" | _ -> false) evs);
  match List.rev evs with
  | Ev_refresh { silent = false } :: _ -> ()
  | _ -> Alcotest.fail "a submit ends with a loud refresh"

let zen_spark () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.dreamer" 0;
  place u 1 "fire.goblin" 2;
  let events = listen u in
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_bool "spark at the goblin" true
    (List.mem (Ev_ability { kind = "zen.spark"; player = 0; num = 0; target = Some (1, 2) }) (events ()))

let simulations_are_silent () =
  let desc = fire_desc () in
  let on_event, events = recorder () in
  let s = Session.create ~on_event desc in
  let before = List.length (events ()) in
  ignore (Game.simulate desc (Session.state s) 0 (greedy s));
  check_int "no events from a simulation" before (List.length (events ()))

let () =
  let open Alcotest in
  run "session"
    [
      ( "session",
        [
          test_case "announces turns" `Quick announces_turns;
          test_case "game over once" `Quick game_over_once;
          test_case "wrong player" `Quick rejects_wrong_player;
          test_case "listener keeps game" `Quick listener_does_not_change_game;
          test_case "custom targets" `Quick custom_targets_get_a_slot;
          test_case "target slot" `Quick target_slot_offers_open_slots;
          test_case "simulations silent" `Quick simulations_are_silent;
        ] );
      ( "events",
        [
          test_case "run slot and die" `Quick run_slot_and_die;
          test_case "move" `Quick move_event;
          test_case "summon and refresh" `Quick summon_and_refresh;
          test_case "zen spark" `Quick zen_spark;
        ] );
    ]
