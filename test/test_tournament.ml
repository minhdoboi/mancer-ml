(* Elo, pairing order and JSON logs. *)

open Mancer
open Sp
open Support

let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)

let ordered () =
  let seats = List.init 4 (fun i -> fst (Tournament.ordered_pair "greedy" "oracle" i)) in
  Alcotest.(check (list string)) "alternating start" [ "greedy"; "oracle"; "greedy"; "oracle" ] seats

let parse_flags () =
  let a = Tournament.parse [| "main.exe"; "--games"; "4"; "--bots"; "greedy,oracle"; "--budget"; "200" |] in
  check_int "games" 4 a.games;
  Alcotest.(check (list string)) "bots" [ "greedy"; "oracle" ] a.bots;
  check_int "budget" 200 a.budget_ms

let elo_win () =
  let elos = Hashtbl.create 4 in
  let stats = Hashtbl.create 4 in
  Tournament.update_stats "a" "b" (Some "a") elos stats;
  Alcotest.(check (float 1e-6)) "winner elo" 1508.0 (Hashtbl.find elos "a");
  Alcotest.(check (float 1e-6)) "loser elo" 1492.0 (Hashtbl.find elos "b");
  check_int "wins" 1
    (let w, _, _ = Hashtbl.find stats "a" in
     w);
  check_int "losses" 1
    (let _, l, _ = Hashtbl.find stats "b" in
     l)

let elo_draw () =
  let elos = Hashtbl.create 4 in
  let stats = Hashtbl.create 4 in
  Tournament.update_stats "a" "b" None elos stats;
  Alcotest.(check (float 1e-6)) "draw elo a" 1500.0 (Hashtbl.find elos "a");
  check_int "draws" 1
    (let _, _, d = Hashtbl.find stats "a" in
     d)

let json_roundtrip () =
  init_all_houses ();
  let d = Game.make_player_desc ~special:Troopers.house Fire.house in
  let r : Game_record.t =
    {
      game = "1";
      bot_names = ("greedy", "oracle");
      starting_player = 0;
      special_names = ("Troopers", "Troopers");
      decks = [| d; d |];
      winner_bot = Some "greedy";
      winner_player = Some 0;
      draw = false;
      turns = 12;
      reason = "ended";
      moves =
        [
          Game_record.move_of ~t:1 ~player:0 None
            {
              players =
                [| Game.initial_player_state [ 2; 2; 2; 2 ] d; Game.initial_player_state [ 1; 1; 1; 1 ] d |];
            }
            { gd_players = [| d; d |] };
        ];
    }
  in
  let json = Game_record.to_json r in
  check_bool "has bots" true (String.length json > 0 && true);
  let dir =
    Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "mancer-ml-t-%d" (Random.bits ()))
  in
  Unix.mkdir dir 0o755;
  let oc = open_out (Filename.concat dir "game-001.json") in
  output_string oc json;
  close_out oc;
  let loaded = Tournament.load_dir dir in
  check_int "one game" 1 (List.length loaded);
  let g = List.hd loaded in
  check_string "first" "greedy" g.first;
  check_string "second" "oracle" g.second;
  check_string "winner" "greedy" (Option.value g.winner ~default:"");
  check_int "winnerPlayer" 0 (Option.value g.winner_player ~default:(-1));
  Sys.remove (Filename.concat dir "game-001.json");
  Unix.rmdir dir

let summary_orders_by_elo () =
  let entries : Tournament.standing_game list =
    [
      {
        num = 1;
        first = "oracle";
        second = "greedy";
        winner = Some "oracle";
        specials = ("Troopers", "Troopers");
        winner_player = Some 0;
      };
      {
        num = 2;
        first = "greedy";
        second = "oracle";
        winner = Some "oracle";
        specials = ("Troopers", "Troopers");
        winner_player = Some 1;
      };
    ]
  in
  let stats, elos = Tournament.compute_standings entries ~include_:[] ~exclude:[] in
  let elo n = List.assoc n elos in
  check_bool "oracle ahead" true (elo "oracle" > elo "greedy");
  let text = Tournament.summary_text stats elos [] [] in
  check_bool "summary names oracle" true (String.length text > 10)

let () =
  let open Alcotest in
  run "tournament"
    [
      ( "harness",
        [
          test_case "ordered pair" `Quick ordered;
          test_case "parse" `Quick parse_flags;
          test_case "elo win" `Quick elo_win;
          test_case "elo draw" `Quick elo_draw;
          test_case "json roundtrip" `Quick json_roundtrip;
          test_case "summary" `Quick summary_orders_by_elo;
        ] );
    ]
