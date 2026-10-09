(* Headless bot-vs-bot tournament.  Port of priv/sp/bot/Tournament.scala:
   one JSON file per game plus a summary with win/loss/draw and Elo. *)

open Sp

type args = {
  games : int;
  bots : string list;
  budget_ms : int;
  max_turns : int;
  out : string;
  mode : string;
  dir : string;
  resume : string;
  include_ : string list;
  exclude : string list;
  min_plays : int;
  min_plays_set : bool;
  seed : int option;
  verbose : bool;
  special : string;
}

type standing_game = {
  num : int;
  first : string;
  second : string;
  winner : string option;
  specials : string * string;
  winner_player : int option;
}

type card_rate = { card : string; house : string; plays : int; wins : int; draws : int; rate : float }

let default_args =
  {
    games = 20;
    bots = [ "greedy" ];
    budget_ms = 1000;
    max_turns = 300;
    out = "tournament-results";
    mode = "play";
    dir = "";
    resume = "";
    include_ = [];
    exclude = [];
    min_plays = 8;
    min_plays_set = false;
    seed = None;
    verbose = false;
    special = "Troopers";
  }

let value_simple args key =
  let rec go = function
    | a :: rest when a = key -> (
        let rec take acc = function
          | [] -> List.rev acc
          | x :: _ when String.starts_with ~prefix:"--" x -> List.rev acc
          | x :: xs -> take (x :: acc) xs
        in
        match take [] rest with [] -> None | vs -> Some (String.concat " " vs))
    | _ :: xs -> go xs
    | [] -> None
  in
  go (Array.to_list args)

let csv_list s = String.split_on_char ',' s |> List.map String.trim |> List.filter (fun x -> x <> "")

let parse argv =
  let v = value_simple argv in
  let has f = Array.exists (( = ) f) argv in
  {
    games = Option.fold ~none:default_args.games ~some:int_of_string (v "--games");
    bots = Option.fold ~none:default_args.bots ~some:csv_list (v "--bots");
    budget_ms = Option.fold ~none:default_args.budget_ms ~some:int_of_string (v "--budget");
    max_turns = Option.fold ~none:default_args.max_turns ~some:int_of_string (v "--maxTurns");
    out = Option.value (v "--out") ~default:default_args.out;
    mode =
      Option.value (v "--mode")
        ~default:(if has "mine" then "mine" else if has "summary" then "summary" else "play");
    dir = Option.value (v "--dir") ~default:"";
    resume = Option.value (v "--resume") ~default:"";
    include_ = Option.fold ~none:[] ~some:csv_list (v "--include");
    exclude = Option.fold ~none:[] ~some:csv_list (v "--exclude");
    min_plays = Option.fold ~none:8 ~some:int_of_string (v "--minPlays");
    min_plays_set = Option.is_some (v "--minPlays");
    seed = Option.map int_of_string (v "--seed");
    verbose = has "-v";
    special = Option.value (v "--special") ~default:default_args.special;
  }

let kept specials include_ exclude =
  let a, b = specials in
  if List.mem a exclude || List.mem b exclude then false
  else if include_ <> [] then List.mem a include_ && List.mem b include_
  else true

let update_stats first second w elos stats =
  let bump b =
    let wins, losses, draws = Hashtbl.find_opt stats b |> Option.value ~default:(0, 0, 0) in
    let row =
      match w with
      | Some x when x = b -> (wins + 1, losses, draws)
      | None -> (wins, losses, draws + 1)
      | Some _ -> (wins, losses + 1, draws)
    in
    Hashtbl.replace stats b row
  in
  bump first;
  bump second;
  let elo n = Hashtbl.find_opt elos n |> Option.value ~default:1500.0 in
  let ra = elo first and rb = elo second in
  let sa, sb = match w with None -> (0.5, 0.5) | Some x -> if x = first then (1.0, 0.0) else (0.0, 1.0) in
  Hashtbl.replace elos first (ra +. (16.0 *. (sa -. (1.0 /. (1.0 +. (10. ** ((rb -. ra) /. 400.0)))))));
  Hashtbl.replace elos second (rb +. (16.0 *. (sb -. (1.0 /. (1.0 +. (10. ** ((ra -. rb) /. 400.0)))))))

let compute_standings entries ~include_ ~exclude =
  let elos = Hashtbl.create 8 in
  let stats = Hashtbl.create 8 in
  List.iter
    (fun e -> if kept e.specials include_ exclude then update_stats e.first e.second e.winner elos stats)
    entries;
  let stats_map = Hashtbl.fold (fun k v acc -> (k, v) :: acc) stats [] in
  let elos_map = Hashtbl.fold (fun k v acc -> (k, v) :: acc) elos [] in
  (stats_map, elos_map)

let elo_of elos n = List.assoc_opt n elos |> Option.value ~default:1500.0

let summary_text stats elos cards special_cards =
  let standings =
    stats
    |> List.sort (fun (a, _) (b, _) -> compare (elo_of elos b) (elo_of elos a))
    |> List.map (fun (b, (w, l, d)) ->
        Printf.sprintf {|    "%s": {"wins": %d, "losses": %d, "draws": %d, "elo": %.1f}|} (Game_record.esc b)
          w l d (elo_of elos b))
    |> String.concat ",\n"
  in
  let card_json cs =
    cs
    |> List.map (fun c ->
        Printf.sprintf
          {|    {"card": "%s", "house": "%s", "plays": %d, "wins": %d, "draws": %d, "rate": %.3f}|}
          (Game_record.esc c.card) (Game_record.esc c.house) c.plays c.wins c.draws c.rate)
    |> String.concat ",\n"
  in
  Printf.sprintf {|{
  "standings": {
%s
  },
  "cards": [
%s
  ],
  "specialCards": [
%s
  ]
}
|} standings
    (card_json cards) (card_json special_cards)

(* --- PlayMiner.cardRates ----------------------------------------------- *)

let bots_re = Str.regexp {|"bots":[ \t]*\["\([^"]+\)",[ \t]*"\([^"]+\)"\]|}
let winner_re = Str.regexp {|"winner":[ \t]*\(null\|"\([^"]+\)"\)|}
let winner_player_re = Str.regexp {|"winnerPlayer":[ \t]*\(null\|\([0-9]+\)\)|}
let specials_re = Str.regexp {|"specials":[ \t]*\["\([^"]+\)",[ \t]*"\([^"]+\)"\]|}

let play_re =
  Str.regexp
    {|"t":[ \t]*\([0-9]+\),[ \t]*"player":[ \t]*\([0-9]+\),[ \t]*"card":[ \t]*"\([^"]+\)",[ \t]*"house":[ \t]*"\([^"]+\)"|}

let file_re = Str.regexp {|^game-\([0-9]+\)\.json$|}

let parse_entry n text =
  if
    try
      ignore (Str.search_forward (Str.regexp {|"error":|}) text 0);
      true
    with Not_found -> false
  then None
  else
    try
      ignore (Str.search_forward bots_re text 0);
      let first = Str.matched_group 1 text and second = Str.matched_group 2 text in
      ignore (Str.search_forward winner_re text 0);
      let winner = try Some (Str.matched_group 2 text) with Not_found -> None in
      ignore (Str.search_forward specials_re text 0);
      let s0 = Str.matched_group 1 text and s1 = Str.matched_group 2 text in
      let wp =
        try
          ignore (Str.search_forward winner_player_re text 0);
          try Some (int_of_string (Str.matched_group 2 text)) with Not_found -> None
        with Not_found -> None
      in
      Some { num = n; first; second; winner; specials = (s0, s1); winner_player = wp }
    with Not_found -> None

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let load_dir dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter_map (fun name ->
        if Str.string_match file_re name 0 then
          let n = int_of_string (Str.matched_group 1 name) in
          let text = read_file (Filename.concat dir name) in
          parse_entry n text
        else None)
    |> List.sort (fun a b -> compare a.num b.num)

type play_move = { player : int; card : string option; house : string option }

let parse_play_game text =
  if
    try
      ignore (Str.search_forward (Str.regexp {|"error":|}) text 0);
      true
    with Not_found -> false
  then None
  else
    try
      ignore (Str.search_forward bots_re text 0);
      let first = Str.matched_group 1 text and second = Str.matched_group 2 text in
      ignore (Str.search_forward winner_re text 0);
      let winner = try Some (Str.matched_group 2 text) with Not_found -> None in
      let wp =
        try
          ignore (Str.search_forward winner_player_re text 0);
          try Some (int_of_string (Str.matched_group 2 text)) with Not_found -> None
        with Not_found -> None
      in
      ignore (Str.search_forward specials_re text 0);
      let spec = (Str.matched_group 1 text, Str.matched_group 2 text) in
      let moves = ref [] in
      let rec scan pos =
        try
          let p = Str.search_forward play_re text pos in
          moves :=
            {
              player = int_of_string (Str.matched_group 2 text);
              card = Some (Str.matched_group 3 text);
              house = Some (Str.matched_group 4 text);
            }
            :: !moves;
          scan (p + 1)
        with Not_found -> ()
      in
      scan 0;
      Some (first, second, winner, wp, spec, List.rev !moves)
    with Not_found -> None

let outcome player ~winner ~winner_player ~bots =
  match (winner, winner_player) with
  | None, None -> 0.5
  | _, Some w -> if player = w then 1.0 else 0.0
  | Some w, None ->
      let b0, b1 = bots in
      if b0 = b1 then 0.5 else if (player = 0 && w = b0) || (player = 1 && w = b1) then 1.0 else 0.0

let card_min_plays a = if a.min_plays_set then a.min_plays else 4

let card_rates games min_plays =
  let acc = Hashtbl.create 32 in
  List.iter
    (fun (bots, winner, winner_player, moves) ->
      List.iter
        (fun pid ->
          let y = outcome pid ~winner ~winner_player ~bots in
          let seen = Hashtbl.create 8 in
          List.iter
            (fun (m : play_move) ->
              if m.player = pid then
                match (m.card, m.house) with
                | Some c, Some h -> if not (Hashtbl.mem seen c) then Hashtbl.add seen c h
                | _ -> ())
            moves;
          Hashtbl.iter
            (fun card house ->
              let key = house ^ "\t" ^ card in
              let plays, wins, draws = Hashtbl.find_opt acc key |> Option.value ~default:(0, 0, 0) in
              let plays = plays + 1 in
              let wins, draws =
                if y >= 1.0 then (wins + 1, draws) else if y > 0.0 then (wins, draws + 1) else (wins, draws)
              in
              Hashtbl.replace acc key (plays, wins, draws))
            seen)
        [ 0; 1 ])
    games;
  Hashtbl.fold
    (fun key (plays, wins, draws) acc ->
      if plays < min_plays then acc
      else
        let i = String.index key '\t' in
        let house = String.sub key 0 i in
        let card = String.sub key (i + 1) (String.length key - i - 1) in
        {
          card;
          house;
          plays;
          wins;
          draws;
          rate = (float_of_int wins +. (0.5 *. float_of_int draws)) /. float_of_int plays;
        }
        :: acc)
    acc []
  |> List.sort (fun a b ->
      let c = compare b.rate a.rate in
      if c <> 0 then c
      else
        let c = compare b.plays a.plays in
        if c <> 0 then c
        else
          let c = compare a.house b.house in
          if c <> 0 then c else compare a.card b.card)

let load_card_games dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter_map (fun name ->
        if Str.string_match file_re name 0 then
          let text = read_file (Filename.concat dir name) in
          match parse_play_game text with
          | Some (b0, b1, w, wp, spec, moves) -> Some ((b0, b1), w, wp, spec, moves)
          | None -> None
        else None)

let load_card_rates dir include_ exclude min_plays =
  load_card_games dir
  |> List.filter_map (fun (bots, w, wp, spec, moves) ->
      if kept spec include_ exclude then Some (bots, w, wp, moves) else None)
  |> fun gs -> card_rates gs min_plays

let special_card_rates (cards : card_rate list) special_names =
  List.filter (fun (c : card_rate) -> List.mem c.house special_names) cards

let cards_report ?(title = "top cards") ?(top = 12) cards =
  if cards = [] then title ^ ": (none with enough plays)\n"
  else
    let lines =
      cards
      |> (fun xs ->
      let rec take n = function [] -> [] | _ when n <= 0 -> [] | x :: xs -> x :: take (n - 1) xs in
      take top xs)
      |> List.map (fun c ->
          Printf.sprintf "    %.3f  %2d-%d-%d / %-3d  %-16s %s" c.rate c.wins
            (c.plays - c.wins - c.draws)
            c.draws c.plays c.house c.card)
    in
    Printf.sprintf "%s (%d with enough plays):\n%s\n" title (List.length cards) (String.concat "\n" lines)

let rec mkdir_p path =
  if path = "." || path = "/" || Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write_file path text =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc

let write_summary path stats elos (cards : card_rate list) special_names =
  let text = summary_text stats elos cards (special_card_rates cards special_names) in
  write_file path text;
  text

let ordered_pair first second game_index = if game_index mod 2 = 0 then (first, second) else (second, first)

type bot_kind = Greedy | Oracle | Lecturer

let bot_kind = function
  | "greedy" | "apprentice" -> Greedy
  | "oracle" -> Oracle
  | "lecturer" -> Lecturer
  | id -> failwith ("unknown bot '" ^ id ^ "' (want greedy, oracle or lecturer)")

let bot_ids = [ "greedy"; "oracle"; "lecturer" ]

let make_mover ?(stats_log = true) kind budget =
  match kind with
  | Greedy -> Game.greedy_mover
  | Oracle ->
      let s = Oracle.default_settings () in
      s.duration <- budget;
      s.logging <- false;
      s.stats_log <- stats_log;
      Oracle.mover s
  | Lecturer ->
      let s = Lecturer.default_settings () in
      s.duration <- budget;
      s.logging <- false;
      s.stats_log <- stats_log;
      Lecturer.mover s

let mover ?stats_log ~budget id = make_mover ?stats_log (bot_kind id) budget

let pairings bots =
  match bots with
  | [] -> failwith "Tournament: --bots needs at least one id"
  | [ b ] -> [ (b, b) ]
  | bs ->
      let rec comb acc = function
        | x :: xs -> comb (List.fold_left (fun a y -> (x, y) :: a) acc xs) xs
        | [] -> List.rev acc
      in
      comb [] bs

let specials_of desc =
  (Game_record.special_name desc.gd_players.(0), Game_record.special_name desc.gd_players.(1))

let play_one ~game_id ~bot_a ~bot_b ~budget ~max_turns ~verbose ~rng ~basics ~special =
  let kind_a = bot_kind bot_a and kind_b = bot_kind bot_b in
  let p1, p2 = Shuffle.two_players ~rng ~specials:(special, special) basics in
  let desc = { gd_players = [| p1.desc; p2.desc |] } in
  let dice_seed = Random.State.bits rng in
  let r =
    Session.play_game desc ~mana0:(Array.to_list p1.mana) ~mana1:(Array.to_list p2.mana)
      ~rng:(Dice.make dice_seed) ~max_turns ~verbose
      ~bots:(make_mover kind_a budget, make_mover kind_b budget)
  in
  let winner_bot = match r.winner with Some 0 -> Some bot_a | Some _ -> Some bot_b | None -> None in
  let result : Game_record.t =
    {
      game = string_of_int game_id;
      bot_names = (bot_a, bot_b);
      starting_player = owner;
      special_names = specials_of desc;
      decks = desc.gd_players;
      mana = [| Array.to_list p1.mana; Array.to_list p2.mana |];
      rng_seed = dice_seed;
      winner_bot;
      winner_player = r.winner;
      draw = Option.is_none r.winner;
      turns = r.turns;
      reason = r.reason;
      moves = r.moves;
    }
  in
  ( result,
    {
      num = game_id;
      first = bot_a;
      second = bot_b;
      winner = winner_bot;
      specials = result.special_names;
      winner_player = r.winner;
    } )

let pairing_key g = String.concat "\x00" (List.sort compare [ g.first; g.second ])

let run_summary a =
  if a.dir = "" then failwith "summary mode needs --dir <results dir>";
  let entries = load_dir a.dir in
  Printf.printf "loaded %d games from %s\n%!" (List.length entries) a.dir;
  let stats, elos = compute_standings entries ~include_:a.include_ ~exclude:a.exclude in
  let cards = load_card_rates a.dir a.include_ a.exclude (card_min_plays a) in
  let name = if a.include_ <> [] || a.exclude <> [] then "summary-filtered.json" else "summary.json" in
  let special_names = [ Troopers.house.h_name ] in
  let text = write_summary (Filename.concat a.dir name) stats elos cards special_names in
  Printf.printf "wrote %s\n%s%!" name text;
  print_string (cards_report cards);
  print_string (cards_report ~title:"top special cards" (special_card_rates cards special_names))

let init_specials () =
  Game.init_cards ~cost_fn:Troopers.conscription_cost Troopers.house;
  Game.init_cards Zen.house;
  Game.init_cards Wind.house;
  Game.init_cards Chronomaster.house;
  Game.init_cards Disciples.house;
  Game.init_cards Darksider.house;
  Game.init_cards Faerie.house;
  Game.init_cards Fairy.house;
  Game.init_cards Dudes.house;
  Game.init_cards Vampires.house;
  Game.init_cards Warp.house;
  Game.init_cards Snowblood.house;
  Game.init_cards Dreamweaver.house;
  Game.init_cards Junk.house;
  Game.init_cards Battlemage.house;
  Game.init_cards ~cost_fn:Antimancer.cost_fn Antimancer.house;
  Game.init_cards Powermancer.house;
  Game.init_cards Shadowmage.house;
  Game.init_cards ~cost_fn:Colors.cost_fn Colors.house;
  Game.init_cards Limbo.house;
  Game.init_cards Rogue.house;
  Game.init_cards Tinker.house;
  Game.init_cards Entomologist.house;
  Game.init_cards Soulbinder.house;
  Game.init_cards Shadowcaster.house;
  Game.init_cards Spiritshaman.house;
  Game.init_cards Conjurer.house;
  Game.init_cards Quantumancer.house;
  Game.init_cards Meteomancer.house;
  Game.init_cards Seasons.house;
  Game.init_cards Soulreaper.house;
  Game.init_cards Sower.house;
  Game.init_cards Shaman.house;
  Game.init_cards Mountainking.house;
  Game.init_cards Darkpriest.house;
  Game.init_cards Elementalist.house;
  Game.init_cards Highpriest.house;
  Game.init_cards Kinetician.house

let run_play a =
  let basics = [ Fire.house; Water.house; Air.house; Earth.house ] in
  Game.init_cards Fire.house;
  Game.init_cards Water.house;
  Game.init_cards Air.house;
  Game.init_cards Earth.house;
  init_specials ();
  let special = Catalog.find_special a.special in
  let seed = match a.seed with Some n -> n | None -> Random.State.bits (Random.State.make_self_init ()) in
  Printf.printf "seed %d\n%!" seed;
  let rng = Random.State.make [| seed |] in
  let existing, out_dir =
    if a.resume <> "" then (
      if not (Sys.file_exists a.resume && Sys.is_directory a.resume) then
        failwith ("--resume dir not found: " ^ a.resume);
      (load_dir a.resume, a.resume))
    else
      let d = Filename.concat a.out (Printf.sprintf "%.0f" (Unix.gettimeofday () *. 1000.)) in
      mkdir_p d;
      ([], d)
  in
  let game_id = ref (List.fold_left (fun m e -> max m e.num) 0 existing) in
  let new_entries = ref [] in
  let done_per_pair =
    let t = Hashtbl.create 8 in
    List.iter
      (fun e ->
        let k = pairing_key e in
        Hashtbl.replace t k (1 + (Hashtbl.find_opt t k |> Option.value ~default:0)))
      existing;
    t
  in
  List.iter
    (fun (bot_a, bot_b) ->
      let key = String.concat "\x00" (List.sort compare [ bot_a; bot_b ]) in
      let done_ = Hashtbl.find_opt done_per_pair key |> Option.value ~default:0 in
      for k = 0 to max 0 (a.games - done_) - 1 do
        let first, second = ordered_pair bot_a bot_b (done_ + k) in
        incr game_id;
        try
          let result, entry =
            play_one ~game_id:!game_id ~bot_a:first ~bot_b:second ~budget:a.budget_ms ~max_turns:a.max_turns
              ~verbose:(a.verbose && !game_id = 1)
              ~rng ~basics ~special
          in
          let path = Filename.concat out_dir (Printf.sprintf "game-%03d.json" !game_id) in
          write_file path (Game_record.to_json result);
          let winner = Option.value result.winner_bot ~default:"draw" in
          Printf.printf "game %d: %s vs %s -> %s in %d turns\n%!" !game_id first second winner result.turns;
          new_entries := entry :: !new_entries
        with e ->
          let err =
            Printf.sprintf
              {|{
  "game": "%d",
  "bots": ["%s", "%s"],
  "specials": ["%s", "%s"],
  "error": "%s",
  "at": "%s"
}
|}
              !game_id first second special.h_name special.h_name
              (Game_record.esc (Printexc.to_string e))
              (Game_record.esc (Printexc.get_backtrace ()))
          in
          write_file (Filename.concat out_dir (Printf.sprintf "game-%03d-error.json" !game_id)) err;
          Printf.printf "game %d: %s vs %s -> ERROR %s\n%!" !game_id first second (Printexc.to_string e)
      done)
    (pairings a.bots);
  let entries = existing @ List.rev !new_entries |> List.sort (fun a b -> compare a.num b.num) in
  let special_names = [ special.h_name ] in
  let full_stats, full_elos = compute_standings entries ~include_:[] ~exclude:[] in
  let full_cards = load_card_rates out_dir [] [] (card_min_plays a) in
  let filtered = a.include_ <> [] || a.exclude <> [] in
  let text =
    write_summary (Filename.concat out_dir "summary.json") full_stats full_elos full_cards special_names
  in
  let text =
    if filtered then
      let stats, elos = compute_standings entries ~include_:a.include_ ~exclude:a.exclude in
      let cards = load_card_rates out_dir a.include_ a.exclude (card_min_plays a) in
      write_summary (Filename.concat out_dir "summary-filtered.json") stats elos cards special_names
    else text
  in
  let cards =
    if filtered then load_card_rates out_dir a.include_ a.exclude (card_min_plays a) else full_cards
  in
  Printf.printf "wrote %d games to %s\n%s%!" (List.length entries) out_dir text;
  print_string (cards_report cards);
  print_string (cards_report ~title:"top special cards" (special_card_rates cards special_names))

let run a = if a.mode = "summary" then run_summary a else run_play a
