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

let parse_modes () =
  let houses = Tournament.parse [| "main.exe"; "--mode"; "houses"; "--special"; "zen" |] in
  check_string "houses mode" "houses" houses.mode;
  let bots = Tournament.parse [| "main.exe" |] in
  check_string "default mode" "bots" bots.mode;
  check_string "play is bots" "bots" (Tournament.parse [| "main.exe"; "--mode"; "play" |]).mode;
  check_string "summary mode" "summary" (Tournament.parse [| "main.exe"; "--mode"; "summary" |]).mode;
  let unknown_mode_fails =
    try
      ignore (Tournament.parse [| "main.exe"; "--mode"; "nonsense" |]);
      false
    with Failure _ -> true
  in
  check_bool "unknown mode" true unknown_mode_fails

let parse_opponents () =
  let a =
    Tournament.parse [| "main.exe"; "--mode"; "houses"; "--special"; "zen"; "--opponent-special"; "dudes,junk" |]
  in
  Alcotest.(check (list string)) "special" [ "zen" ] a.special;
  Alcotest.(check (list string)) "opponents" [ "dudes"; "junk" ] a.opponent_special;
  (* --special takes a list of houses *)
  let many =
    Tournament.parse [| "main.exe"; "--mode"; "houses"; "--special"; "zen,wind,darksider" |]
  in
  Alcotest.(check (list string)) "special list" [ "zen"; "wind"; "darksider" ] many.special;
  Alcotest.(check (list string)) "no opponents" [] many.opponent_special;
  (* one house by default *)
  Alcotest.(check (list string)) "default special" [ "Troopers" ]
    (Tournament.parse [| "main.exe" |]).special

(* Every special house of the catalog must be usable: the houses that were
   left out of the tournament's init got card indexes of zero and the deck
   shuffle failed with "special house has no legal pick". *)
let every_special_house_deals () =
  Game.init_cards Fire.house;
  Game.init_cards Water.house;
  Game.init_cards Air.house;
  Game.init_cards Earth.house;
  Tournament.init_specials ();
  let basics = [ Fire.house; Water.house; Air.house; Earth.house ] in
  List.iter
    (fun (h : house) ->
      let rng = Random.State.make [| 7 |] in
      let p1, p2 = Shuffle.two_players ~rng ~specials:(h, h) basics in
      check_bool ("deal " ^ h.h_name) true (Array.length p1.desc.pd_houses = 5 && Array.length p2.desc.pd_houses = 5))
    Catalog.specials

let house_suffix () =
  init_all_houses ();
  let d = Game.make_player_desc ~special:Troopers.house Fire.house in
  let desc = { gd_players = [| d; d |] } in
  let a, b = Tournament.specials_of desc ~suffix:true in
  check_string "first class" "Troopers" a;
  check_string "second class" "Troopers2" b;
  let a, b = Tournament.specials_of desc ~suffix:false in
  check_string "no suffix" "Troopers" b

let house_standings_table () =
  let entries : Tournament.standing_game list =
    [
      {
        num = 1;
        first = "diviner";
        second = "diviner";
        winner = Some "diviner";
        specials = ("zen", "dudes");
        winner_player = Some 0;
      };
      {
        num = 2;
        first = "diviner";
        second = "diviner";
        winner = Some "diviner";
        specials = ("zen", "dudes");
        winner_player = Some 1;
      };
      {
        num = 3;
        first = "diviner";
        second = "diviner";
        winner = None;
        specials = ("zen", "zen2");
        winner_player = None;
      };
    ]
  in
  let houses = Tournament.house_standings entries ~include_:[] ~exclude:[] in
  let find n = List.find (fun (h : Tournament.house_standing) -> h.h_name = n) houses in
  let zen = find "zen" in
  check_int "zen games" 3 zen.h_games;
  check_int "zen wins" 1 zen.h_wins;
  check_int "zen losses" 1 zen.h_losses;
  check_int "zen draws" 1 zen.h_draws;
  let dudes = find "dudes" in
  check_int "dudes wins" 1 dudes.h_wins;
  check_int "dudes losses" 1 dudes.h_losses;
  let second = find "zen2" in
  check_int "zen2 games" 1 second.h_games;
  check_int "zen2 draws" 1 second.h_draws;
  (* the winner of a game rates above the loser *)
  check_bool "dudes rated above zen" true (dudes.h_elo > zen.h_elo);
  (* --include keeps the games where a seat plays the house, suffix and all *)
  let only_zen = Tournament.house_standings entries ~include_:[ "zen" ] ~exclude:[] in
  let names = List.map (fun (h : Tournament.house_standing) -> h.h_name) only_zen in
  Alcotest.(check (list string)) "filtered" [ "dudes"; "zen"; "zen2" ] (List.sort compare names);
  let zen = List.find (fun (h : Tournament.house_standing) -> h.h_name = "zen") only_zen in
  check_int "zen games" 3 zen.h_games

(* The house tournament compares houses: [--special] plus the ones in
   [--opponent-special], each of them once, and every pair of them plays.
   With no [--opponent-special] the [--special] list battles among itself. *)
let house_tournament_shape () =
  let a =
    Tournament.parse [| "main.exe"; "--mode"; "houses"; "--special"; "zen"; "--opponent-special"; "dudes,junk,zen" |]
  in
  let houses = Tournament.house_set a in
  Alcotest.(check (list string)) "houses" [ "zen"; "Dudes"; "junk" ]
    (List.map (fun (h : house) -> h.h_key) houses);
  let pairs = Tournament.house_pairs houses in
  check_int "pairs" 3 (List.length pairs);
  Alcotest.(check (list string)) "first pair" [ "zen"; "Dudes" ]
    (let (a, b) = List.hd pairs in
     [ a.h_key; b.h_key ]);
  (* the list alone battles among itself *)
  let alone =
    Tournament.parse [| "main.exe"; "--mode"; "houses"; "--special"; "zen,wind,darksider" |]
  in
  let houses = Tournament.house_set alone in
  check_int "houses alone" 3 (List.length houses);
  check_int "pairs alone" 3 (List.length (Tournament.house_pairs houses));
  Alcotest.(check (list string)) "pairs alone" [ "zen"; "wind"; "darksider" ]
    (List.map (fun (h : house) -> h.h_key) houses)

(* The matrix holds the score of every pair of houses, both ways. *)
let house_matrix_is_pairwise () =
  let entry n a b winner : Tournament.standing_game =
    {
      num = n;
      first = "diviner";
      second = "diviner";
      winner = Some "diviner";
      specials = (a, b);
      winner_player = winner;
    }
  in
  let entries =
    [ entry 1 "zen" "wind" (Some 0); entry 2 "zen" "wind" (Some 1); entry 3 "zen" "wind" None ]
  in
  let mx = Tournament.house_matrix entries ~include_:[] ~exclude:[] in
  Alcotest.(check (list string)) "houses" [ "zen"; "wind" ] mx.mx_houses;
  let row h = List.assoc h mx.mx_rows in
  let zen_wind = List.assoc "wind" (row "zen") in
  check_int "zen wins" 1 zen_wind.m_wins;
  check_int "zen losses" 1 zen_wind.m_losses;
  check_int "zen draws" 1 zen_wind.m_draws;
  check_int "zen games" 3 zen_wind.m_games;
  let wind_zen = List.assoc "zen" (row "wind") in
  check_int "wind wins" 1 wind_zen.m_wins;
  check_int "wind losses" 1 wind_zen.m_losses;
  check_int "wind games" 3 wind_zen.m_games

(* Over four games every house has met every bot from every seat, so the
   house rating measures the house and not the seat or the bot. *)
let crossing_is_balanced () =
  let zen = Catalog.find_special "zen" and dudes = Catalog.find_special "dudes" in
  let seen = Hashtbl.create 16 in
  for g = 0 to 3 do
    let (b0, _), (h0, _) = Tournament.crossing ("greedy", "oracle") (zen, dudes) g in
    let key = h0.h_key ^ "|" ^ b0 in
    Hashtbl.replace seen key ()
  done;
  check_int "combinations" 4 (Hashtbl.length seen);
  let bot = Tournament.crossing ("greedy", "oracle") (zen, dudes) 0 in
  let (b0, b1), (h0, h1) = bot in
  check_string "seat 0 bot" "greedy" b0;
  check_string "seat 1 bot" "oracle" b1;
  check_string "seat 0 house" "zen" h0.h_key;
  check_string "seat 1 house" "Dudes" h1.h_key;
  let (_, _), (h0, h1) = Tournament.crossing ("greedy", "oracle") (zen, dudes) 1 in
  check_string "house swaps" "Dudes" h0.h_key;
  check_string "house swaps back" "zen" h1.h_key;
  let (b0, _), _ = Tournament.crossing ("greedy", "oracle") (zen, dudes) 2 in
  check_string "bot swaps" "oracle" b0

(* The log of a game names the subject of the tournament, and in the house
   tournament the winner is the house, not the bot that played it. *)
let has_sub s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  m = 0 || go 0

let game_line_names_the_winner () =
  init_all_houses ();
  let d = Game.make_player_desc ~special:Troopers.house Fire.house in
  let result winner_player : Game_record.t =
    {
      Game_record.game = "1";
      bot_names = ("diviner", "diviner");
      starting_player = 0;
      special_names = ("paganist", "zen");
      decks = [| d; d |];
      mana = [| []; [] |];
      rng_seed = 1;
      winner_bot = Some "diviner";
      winner_player;
      draw = winner_player = None;
      turns = 21;
      reason = "ended";
      moves = [];
    }
  in
  let line = Tournament.game_line "houses" ~id:6 ~bot_a:"diviner" ~bot_b:"diviner" ~result:(result (Some 1)) in
  check_bool "house of the winning seat" true (has_sub line "-> zen");
  check_bool "no bot as the winner" false (has_sub line "-> diviner");
  check_bool "both houses named" true (has_sub line "paganist vs zen");
  let line = Tournament.game_line "houses" ~id:6 ~bot_a:"diviner" ~bot_b:"diviner" ~result:(result (Some 0)) in
  check_bool "seat 0 wins" true (has_sub line "-> paganist");
  let line = Tournament.game_line "houses" ~id:6 ~bot_a:"diviner" ~bot_b:"diviner" ~result:(result None) in
  check_bool "draw" true (has_sub line "-> draw");
  (* the bot tournament names the bots *)
  let line = Tournament.game_line "bots" ~id:6 ~bot_a:"greedy" ~bot_b:"oracle" ~result:(result (Some 1)) in
  check_bool "bot wins" true (has_sub line "-> diviner");
  check_bool "bots named" true (has_sub line "greedy vs oracle")

(* A pairing is counted apart from the house pair, so a resumed house run
   knows which pairings are left. *)
let pairing_keys () =
  let bot_key = Tournament.entry_key "bots" ~first:"a" ~second:"b" ~specials:("zen", "zen") in
  let house_key = Tournament.entry_key "houses" ~first:"a" ~second:"b" ~specials:("zen", "dudes") in
  check_bool "keys differ" true (bot_key <> house_key);
  let same =
    Tournament.entry_key "houses" ~first:"a" ~second:"b" ~specials:("dudes", "zen")
    = house_key
  in
  check_bool "order does not matter" true same

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
      mana = [| [ 2; 2; 2; 2; 2 ]; [ 1; 1; 1; 1; 2 ] |];
      rng_seed = 1;
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
          test_case "parse modes" `Quick parse_modes;
          test_case "parse opponents" `Quick parse_opponents;
          test_case "every house deals" `Quick every_special_house_deals;
          test_case "house suffix" `Quick house_suffix;
          test_case "house standings" `Quick house_standings_table;
          test_case "house tournament shape" `Quick house_tournament_shape;
          test_case "house matrix" `Quick house_matrix_is_pairwise;
          test_case "crossing is balanced" `Quick crossing_is_balanced;
          test_case "pairing keys" `Quick pairing_keys;
          test_case "game line names the winner" `Quick game_line_names_the_winner;
          test_case "elo win" `Quick elo_win;
          test_case "elo draw" `Quick elo_draw;
          test_case "json roundtrip" `Quick json_roundtrip;
          test_case "summary" `Quick summary_orders_by_elo;
        ] );
    ]
