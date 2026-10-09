(* Recorded-game replay: parse, determinism, snapshots. *)

open Mancer
open Support

let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)

let parse_roundtrip () =
  Catalog.init ();
  let rec_ = Replay.generate ~special:"zen" ~max_turns:12 ~seed:7 () in
  let json = Game_record.to_json rec_ in
  let p = Game_record.of_json json in
  check_string "game" rec_.game p.game;
  check_int "seed" rec_.rng_seed p.rng_seed;
  check_int "moves" (List.length rec_.moves) (List.length p.moves);
  check_int "mana0" (List.length rec_.mana.(0)) (List.length p.mana.(0));
  check_bool "has decks" true (Array.length p.decks = 2 && p.decks.(0) <> [])

let replay_is_deterministic () =
  Catalog.init ();
  let rec_ = Replay.generate ~special:"Troopers" ~max_turns:20 ~seed:3 () in
  let p = Game_record.of_json (Game_record.to_json rec_) in
  let cat = Catalog.default () in
  let a = Replay.run ~cat p and b = Replay.run ~cat p in
  match Replay.diff a b with
  | None -> check_int "snaps" (1 + List.length rec_.moves) (List.length a)
  | Some m -> Alcotest.fail (Printf.sprintf "t=%d %s" m.t m.message)

let start_board_is_empty () =
  Catalog.init ();
  let rec_ = Replay.generate ~special:"wind" ~max_turns:4 ~seed:11 () in
  let snaps = Replay.run ~cat:(Catalog.default ()) (Game_record.of_json (Game_record.to_json rec_)) in
  let start = List.hd snaps in
  check_int "t0" 0 start.t;
  check_string "after" "start" start.after;
  check_int "life" 60 start.sides.(0).life;
  let filled =
    Array.fold_left
      (fun n (s : Replay.side) -> n + Array.fold_left (fun k o -> k + if o = None then 0 else 1) 0 s.slots)
      0 start.sides
  in
  check_int "empty board" 0 filled

let snap_json_roundtrip () =
  Catalog.init ();
  let rec_ = Replay.generate ~special:"chronomaster" ~max_turns:8 ~seed:5 () in
  let snaps = Replay.run ~cat:(Catalog.default ()) (Game_record.of_json (Game_record.to_json rec_)) in
  let again = Replay.snaps_of_json (Replay.snaps_to_json snaps) in
  match Replay.diff snaps again with
  | None -> check_int "count" (List.length snaps) (List.length again)
  | Some m -> Alcotest.fail m.message

let () =
  let open Alcotest in
  run "replay"
    [
      ( "record",
        [
          test_case "parse generated json" `Quick parse_roundtrip;
          test_case "replay is deterministic" `Quick replay_is_deterministic;
          test_case "start snap" `Quick start_board_is_empty;
          test_case "snapshot json" `Quick snap_json_roundtrip;
        ] );
    ]
