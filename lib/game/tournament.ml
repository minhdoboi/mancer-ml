(* Headless tournament.  Port of priv/sp/bot/Tournament.scala: one JSON file
   per game plus a summary with win/loss/draw and Elo.

   Two tournaments share the driver:

   - "bots"   the bot tournament: the bots are the variable, every seat
              plays [--special], and the standings rank the bots.
   - "houses" the house tournament: the houses are the variable, they play a
              round robin against each other, and the standings rank the
              houses.  The bots are only the instrument that plays them.

   Mixing the two in one run would rank a bot by the houses it happened to
   draw, which measures neither. *)

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
  special : string list;
  opponent_special : string list;
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

type house_standing = {
  h_name : string;
  h_games : int;
  h_wins : int;
  h_losses : int;
  h_draws : int;
  h_elo : float;
}

type matchup = { m_games : int; m_wins : int; m_losses : int; m_draws : int }

type house_matrix = {
  mx_houses : string list;  (** The houses, in the order of the run. *)
  mx_rows : (string * (string * matchup) list) list;  (** House -> its record against each other house. *)
}
(** The houses against each other: the score of every pair, both ways. *)

let default_args =
  {
    games = 20;
    bots = [ "greedy" ];
    budget_ms = 1000;
    max_turns = 300;
    out = "tournament-results";
    mode = "bots";
    dir = "";
    resume = "";
    include_ = [];
    exclude = [];
    min_plays = 8;
    min_plays_set = false;
    seed = None;
    verbose = false;
    special = [ "Troopers" ];
    opponent_special = [];
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
  let mode =
    let m = Option.value (v "--mode") ~default:(if has "summary" then "summary" else "bots") in
    match String.lowercase_ascii m with
    | "bots" | "bot" | "play" | "mine" -> "bots"
    | "houses" | "house" -> "houses"
    | "summary" -> "summary"
    | other -> failwith ("Tournament: unknown mode '" ^ other ^ "' (want bots, houses or summary)")
  in
  let special =
    let l = Option.fold ~none:default_args.special ~some:csv_list (v "--special") in
    if l = [] then default_args.special else l
  in
  {
    games = Option.fold ~none:default_args.games ~some:int_of_string (v "--games");
    bots = Option.fold ~none:default_args.bots ~some:csv_list (v "--bots");
    budget_ms = Option.fold ~none:default_args.budget_ms ~some:int_of_string (v "--budget");
    max_turns = Option.fold ~none:default_args.max_turns ~some:int_of_string (v "--maxTurns");
    out = Option.value (v "--out") ~default:default_args.out;
    mode;
    dir = Option.value (v "--dir") ~default:"";
    resume = Option.value (v "--resume") ~default:"";
    include_ = Option.fold ~none:[] ~some:csv_list (v "--include");
    exclude = Option.fold ~none:[] ~some:csv_list (v "--exclude");
    min_plays = Option.fold ~none:8 ~some:int_of_string (v "--minPlays");
    min_plays_set = Option.is_some (v "--minPlays");
    seed = Option.map int_of_string (v "--seed");
    verbose = has "-v";
    special;
    opponent_special = Option.fold ~none:[] ~some:csv_list (v "--opponent-special");
  }

(* A game may label both seats with the same house ("zen" and "zen2"), so a
   filter matches on the base name as well.  [include_] keeps a game when
   either seat plays one of the houses: both seats shared a house until the
   house tournament, so it used to be the same thing. *)
let base_name s =
  let n = String.length s in
  let rec digits i = if i > 0 && s.[i - 1] >= '0' && s.[i - 1] <= '9' then digits (i - 1) else i in
  let i = digits n in
  if i < n && i > 0 then String.sub s 0 i else s

let mem_house name list = List.exists (fun x -> x = name || base_name x = base_name name) list

(* The houses a run used, without the seat suffix. *)
let special_names_of (entries : standing_game list) =
  entries
  |> List.concat_map (fun e ->
         let a, b = e.specials in
         [ base_name a; base_name b ])
  |> List.sort_uniq compare

let kept specials include_ exclude =
  let a, b = specials in
  if mem_house a exclude || mem_house b exclude then false
  else if include_ <> [] then mem_house a include_ || mem_house b include_
  else true

(* The online rating update, on its own: the house tournament rates houses
   with the same formula the bot tournament rates bots. *)
let update_elo first second w elos =
  let elo n = Hashtbl.find_opt elos n |> Option.value ~default:1500.0 in
  let ra = elo first and rb = elo second in
  let sa, sb = match w with None -> (0.5, 0.5) | Some x -> if x = first then (1.0, 0.0) else (0.0, 1.0) in
  Hashtbl.replace elos first (ra +. (16.0 *. (sa -. (1.0 /. (1.0 +. (10. ** ((rb -. ra) /. 400.0)))))));
  Hashtbl.replace elos second (rb +. (16.0 *. (sb -. (1.0 /. (1.0 +. (10. ** ((ra -. rb) /. 400.0)))))))

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
  update_elo first second w elos

(* The names of the two seats.  A self-play has one bot on both seats, so the
   second seat gets a suffix and the table shows the seat split — the only
   thing a mirror match can measure. *)
let seat_names (e : standing_game) =
  if e.first = e.second then (e.first, e.first ^ "2") else (e.first, e.second)

let compute_standings entries ~include_ ~exclude =
  let elos = Hashtbl.create 8 in
  let stats = Hashtbl.create 8 in
  List.iter
    (fun e ->
      if kept e.specials include_ exclude then
        let a, b = seat_names e in
        let w = match e.winner_player with Some 0 -> Some a | Some 1 -> Some b | _ -> None in
        update_stats a b w elos stats)
    entries;
  let stats_map = Hashtbl.fold (fun k v acc -> (k, v) :: acc) stats [] in
  let elos_map = Hashtbl.fold (fun k v acc -> (k, v) :: acc) elos [] in
  (stats_map, elos_map)

let elo_of elos n = List.assoc_opt n elos |> Option.value ~default:1500.0

(* Per-house record: a game is credited to the house of the seat that won it.
   A game where both seats play the same house labels them "zen" and "zen2",
   so the two seats stay apart.  The Elo is the same online rating the bot
   tournament gives the bots. *)
let house_standings (entries : standing_game list) ~include_ ~exclude =
  let acc = Hashtbl.create 8 in
  let elos = Hashtbl.create 8 in
  let row name = Hashtbl.find_opt acc name |> Option.value ~default:(0, 0, 0, 0) in
  let put name g w l d = Hashtbl.replace acc name (g, w, l, d) in
  List.iter
    (fun e ->
      if kept e.specials include_ exclude then
        let a, b = e.specials in
        let ag, aw, al, ad = row a and bg, bw, bl, bd = row b in
        put a (ag + 1) aw al ad;
        put b (bg + 1) bw bl bd;
        let winner = match e.winner_player with Some 0 -> Some a | Some _ -> Some b | None -> None in
        update_elo a b winner elos;
        match e.winner_player with
        | Some 0 ->
            let g, w, l, d = row a in
            put a g (w + 1) l d;
            let g, w, l, d = row b in
            put b g w (l + 1) d
        | Some _ ->
            let g, w, l, d = row b in
            put b g (w + 1) l d;
            let g, w, l, d = row a in
            put a g w (l + 1) d
        | None ->
            let g, w, l, d = row a in
            put a g w l (d + 1);
            let g, w, l, d = row b in
            put b g w l (d + 1))
    entries;
  Hashtbl.fold
    (fun name (g, w, l, d) acc ->
      { h_name = name; h_games = g; h_wins = w; h_losses = l; h_draws = d;
        h_elo = Hashtbl.find_opt elos name |> Option.value ~default:1500.0 }
      :: acc)
    acc []
  |> List.sort (fun a b ->
         let c = compare b.h_elo a.h_elo in
         if c <> 0 then c else compare a.h_name b.h_name)

(* The houses of a run, in the order they first appear. *)
let matrix_houses (entries : standing_game list) =
  let seen = Hashtbl.create 8 in
  let out = ref [] in
  List.iter
    (fun e ->
      let a, b = e.specials in
      List.iter
        (fun n ->
          let n = base_name n in
          if not (Hashtbl.mem seen n) then (
            Hashtbl.add seen n ();
            out := n :: !out))
        [ a; b ])
    entries;
  List.rev !out

(* The record of every house against every other house, both ways.  A game
   between two houses is one win for one of them. *)
let house_matrix (entries : standing_game list) ~include_ ~exclude : house_matrix =
  let acc = Hashtbl.create 16 in
  let get a b = Hashtbl.find_opt acc (a ^ "\t" ^ b) |> Option.value ~default:(0, 0, 0, 0) in
  let put a b (g, w, l, d) = Hashtbl.replace acc (a ^ "\t" ^ b) (g, w, l, d) in
  List.iter
    (fun e ->
      if kept e.specials include_ exclude then
        let a = base_name (fst e.specials) and b = base_name (snd e.specials) in
        if a <> b then (
          let ga, aw, al, ad = get a b and gb, bw, bl, bd = get b a in
          let (wa, la, da), (wb, lb, db) =
            match e.winner_player with
            | Some 0 -> (1, 0, 0), (0, 1, 0)
            | Some _ -> (0, 1, 0), (1, 0, 0)
            | None -> (0, 0, 1), (0, 0, 1)
          in
          put a b (ga + 1, aw + wa, al + la, ad + da);
          put b a (gb + 1, bw + wb, bl + lb, bd + db)))
    entries;
  let houses = matrix_houses entries in
  let row h =
    List.filter_map
      (fun other ->
        if h = other then None
        else
          let g, w, l, d = get h other in
          if g = 0 then None else Some (other, { m_games = g; m_wins = w; m_losses = l; m_draws = d }))
      houses
  in
  { mx_houses = houses; mx_rows = List.map (fun h -> (h, row h)) houses }

let house_rate (h : house_standing) =
  if h.h_games = 0 then 0.0
  else (float_of_int h.h_wins +. (0.5 *. float_of_int h.h_draws)) /. float_of_int h.h_games

let summary_text ?(houses = []) ?(matrix = { mx_houses = []; mx_rows = [] }) stats elos cards special_cards =
  let standings =
    stats
    |> List.sort (fun (a, _) (b, _) -> compare (elo_of elos b) (elo_of elos a))
    |> List.map (fun (b, (w, l, d)) ->
        Printf.sprintf {|    "%s": {"wins": %d, "losses": %d, "draws": %d, "elo": %.1f}|} (Game_record.esc b)
          w l d (elo_of elos b))
    |> String.concat ",\n"
  in
  let houses_json =
    houses
    |> List.map (fun (h : house_standing) ->
           Printf.sprintf
             {|    "%s": {"games": %d, "wins": %d, "losses": %d, "draws": %d, "rate": %.3f, "elo": %.1f}|}
             (Game_record.esc h.h_name) h.h_games h.h_wins h.h_losses h.h_draws (house_rate h) h.h_elo)
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
  let matrix_json =
    matrix.mx_rows
    |> List.map (fun (h, row) ->
           let cells =
             row
             |> List.map (fun (other, m) ->
                    Printf.sprintf
                      {|      "%s": {"games": %d, "wins": %d, "losses": %d, "draws": %d, "rate": %.3f}|}
                      (Game_record.esc other) m.m_games m.m_wins m.m_losses m.m_draws
                      (if m.m_games = 0 then 0.0
                       else (float_of_int m.m_wins +. (0.5 *. float_of_int m.m_draws)) /. float_of_int m.m_games))
             |> String.concat ",\n"
           in
           if cells = "" then Printf.sprintf {|    "%s": {}|} (Game_record.esc h)
           else Printf.sprintf {|    "%s": {
%s
    }|} (Game_record.esc h) cells)
    |> String.concat ",\n"
  in
  Printf.sprintf {|{
  "standings": {
%s
  },
  "houses": {
%s
  },
  "houseMatrix": {
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
    houses_json
    matrix_json
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

let read_file = Util.read_file

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

let standings_report ?(title = "bots") (stats : (string * (int * int * int)) list) (elos : (string * float) list) =
  if stats = [] then title ^ ": (none)\n"
  else
    let lines =
      stats
      |> List.sort (fun (a, _) (b, _) -> compare (elo_of elos b) (elo_of elos a))
      |> List.map (fun (b, (w, l, d)) ->
             Printf.sprintf "    %2d-%d-%d / %-3d  %-10s elo %6.1f" w l d (w + l + d) b (elo_of elos b))
      |> String.concat "\n"
    in
    Printf.sprintf "%s (%d):\n%s\n" title (List.length stats) lines

let houses_report ?(title = "houses") (houses : house_standing list) =
  if houses = [] then title ^ ": (none)\n"
  else
    let lines =
      houses
      |> List.map (fun (h : house_standing) ->
             Printf.sprintf "    %.3f  %2d-%d-%d / %-3d  %-8s elo %6.1f" (house_rate h) h.h_wins h.h_losses
               h.h_draws h.h_games h.h_name h.h_elo)
      |> String.concat "\n"
    in
    Printf.sprintf "%s (%d):\n%s\n" title (List.length houses) lines

(* The houses against each other: rows are the houses, columns the houses
   they played, and a cell is the row's score against the column. *)
let matrix_report ?(title = "house matrix (row against column)") (mx : house_matrix) =
  if mx.mx_houses = [] then title ^ ": (none)\n"
  else
    let cell m = if m.m_games = 0 then "-" else Printf.sprintf "%d-%d" m.m_wins m.m_losses in
    let width = List.fold_left (fun w h -> max w (String.length h)) 0 mx.mx_houses + 2 in
    let pad text = Printf.sprintf "%-*s" width text in
    let trim_right s =
      let n = ref (String.length s) in
      while !n > 0 && s.[!n - 1] = ' ' do
        decr n
      done;
      String.sub s 0 !n
    in
    let row_of h =
      mx.mx_rows |> List.find_opt (fun (x, _) -> x = h) |> Option.value ~default:(h, []) |> snd
    in
    let header = mx.mx_houses |> List.map pad |> String.concat "" |> trim_right in
    let line h =
      let matchups = row_of h in
      let cells =
        mx.mx_houses
        |> List.map (fun other ->
               let text = match List.assoc_opt other matchups with Some m -> cell m | None -> "-" in
               pad text)
        |> String.concat ""
      in
      trim_right (pad h ^ cells)
    in
    let lines = mx.mx_houses |> List.map line in
    Printf.sprintf "%s:\n%s\n%s\n" title header (String.concat "\n" lines)

let rec mkdir_p path =
  if path = "." || path = "/" || Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write_file path text =
  mkdir_p (Filename.dirname path);
  Util.write_file path text

let write_summary path stats elos (cards : card_rate list) special_names ~houses ~matrix =
  let text = summary_text ~houses ~matrix stats elos cards (special_card_rates cards special_names) in
  write_file path text;
  text

let ordered_pair first second game_index = if game_index mod 2 = 0 then (first, second) else (second, first)

type bot_kind = Greedy | Summoner | Strategist | Oracle | Lecturer | Diviner

let bot_kind = function
  | "greedy" -> Greedy
  | "apprentice" -> Lecturer
  | "summoner" -> Summoner
  | "strategist" -> Strategist
  | "oracle" -> Oracle
  | "lecturer" | "gambler" -> Lecturer
  | "diviner" -> Diviner
  | id ->
      failwith ("unknown bot '" ^ id ^ "' (want greedy, summoner, strategist, lecturer, oracle or diviner)")

let bot_ids = [ "greedy"; "summoner"; "strategist"; "lecturer"; "oracle"; "diviner" ]

let make_mover ?(stats_log = true) kind budget =
  match kind with
  | Greedy -> Game.greedy_mover
  | Summoner ->
      let s = Summoner.default_settings () in
      s.duration <- budget;
      s.logging <- false;
      s.stats_log <- stats_log;
      Summoner.mover s
  | Strategist ->
      let s = Strategist.default_settings () in
      s.duration <- budget;
      s.logging <- false;
      s.stats_log <- stats_log;
      Strategist.mover s
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
  | Diviner ->
      let s = Diviner.default_settings () in
      s.duration <- budget;
      s.logging <- false;
      s.stats_log <- stats_log;
      Diviner.mover s

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

(* The recorded class of each seat.  When both seats play the same house the
   second one gets a suffix so the two stay apart in the standings. *)
let specials_of desc ~suffix =
  let a = Game_record.special_name desc.gd_players.(0)
  and b = Game_record.special_name desc.gd_players.(1) in
  if suffix && a = b then (a, a ^ "2") else (a, b)

let play_one ~game_id ~bot_a ~bot_b ~budget ~max_turns ~verbose ~rng ~basics ~special_a ~special_b ~suffix =
  let kind_a = bot_kind bot_a and kind_b = bot_kind bot_b in
  let p1, p2 = Shuffle.two_players ~rng ~specials:(special_a, special_b) basics in
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
      special_names = specials_of desc ~suffix;
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

(* What a run needs to keep between games. *)
type run_ctx = {
  ctx_basics : house list;
  ctx_rng : Random.State.t;
  ctx_out : string;
  ctx_game_id : int ref;
  ctx_entries : standing_game list ref;
  ctx_done : (string, int) Hashtbl.t;
}

(* The key a pairing is counted under, so a resumed run knows what is left.
   The house tournament counts a house pair apart from the bot pair: the
   same bots against another house is another pairing. *)
let entry_key mode ~first ~second ~specials =
  let bots = String.concat "\x00" (List.sort compare [ first; second ]) in
  match mode with
  | "houses" ->
      let houses = String.concat "\x00" (List.sort compare [ fst specials; snd specials ]) in
      bots ^ "\x00" ^ houses
  | _ -> bots

(* The log names the subject of the tournament: the bots in the bot
   tournament, the houses in the house tournament.  There the winner is the
   house of the winning seat, not the bot that played it. *)
let game_line mode ~id ~bot_a ~bot_b ~(result : Game_record.t) =
  let houses = mode = "houses" in
  let label0, label1 = result.special_names in
  let first = if houses then label0 else bot_a in
  let second = if houses then label1 else bot_b in
  let winner =
    if houses then
      match result.winner_player with
      | Some 0 -> Some label0
      | Some _ -> Some label1
      | None -> None
    else result.winner_bot
  in
  let bots = if houses && bot_a <> bot_b then Printf.sprintf " (%s vs %s)" bot_a bot_b else "" in
  Printf.sprintf "game %d: %s vs %s%s -> %s in %d turns" id first second bots
    (Option.value winner ~default:"draw") result.turns

let play_and_record ctx ~mode ~budget ~max_turns ~verbose ~(bot_a : string) ~(bot_b : string) ~(special_a : house)
    ~(special_b : house) =
  incr ctx.ctx_game_id;
  let id = !(ctx.ctx_game_id) in
  try
    let result, entry =
      play_one ~game_id:id ~bot_a ~bot_b ~budget ~max_turns ~verbose:(verbose && id = 1) ~rng:ctx.ctx_rng
        ~basics:ctx.ctx_basics ~special_a ~special_b ~suffix:true
    in
    let path = Filename.concat ctx.ctx_out (Printf.sprintf "game-%03d.json" id) in
    write_file path (Game_record.to_json result);
    Printf.printf "%s\n%!" (game_line mode ~id ~bot_a ~bot_b ~result);
    ctx.ctx_entries := entry :: !(ctx.ctx_entries)
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
        id bot_a bot_b special_a.h_name special_b.h_name (Game_record.esc (Printexc.to_string e))
        (Game_record.esc (Printexc.get_backtrace ()))
    in
    write_file (Filename.concat ctx.ctx_out (Printf.sprintf "game-%03d-error.json" id)) err;
    let houses = mode = "houses" in
    let first = if houses then special_a.h_name else bot_a in
    let second = if houses then special_b.h_name else bot_b in
    Printf.printf "game %d: %s vs %s -> ERROR %s\n%!" id first second (Printexc.to_string e)

let run_summary a =
  if a.dir = "" then failwith "summary mode needs --dir <results dir>";
  let entries = load_dir a.dir in
  Printf.printf "loaded %d games from %s\n%!" (List.length entries) a.dir;
  let stats, elos = compute_standings entries ~include_:a.include_ ~exclude:a.exclude in
  let cards = load_card_rates a.dir a.include_ a.exclude (card_min_plays a) in
  let houses = house_standings entries ~include_:a.include_ ~exclude:a.exclude in
  let matrix = house_matrix entries ~include_:a.include_ ~exclude:a.exclude in
  let name = if a.include_ <> [] || a.exclude <> [] then "summary-filtered.json" else "summary.json" in
  let special_names = special_names_of entries in
  let text = write_summary (Filename.concat a.dir name) stats elos cards special_names ~houses ~matrix in
  Printf.printf "wrote %s\n%s%!" name text;
  print_string (matrix_report matrix);
  print_string (houses_report houses);
  print_string (standings_report ~title:"bots" stats elos);
  print_string (cards_report cards);
  print_string (cards_report ~title:"top special cards" (special_card_rates cards special_names))

(* Every special house the catalog knows, so [--special] never lands on a
   house whose cards were never initialized (that used to leave the card
   indexes at zero and the deck shuffle failed).  House keys are not all
   lower case ("Troopers", "Dudes", "reaper"), so the cost overrides match
   on the lower-cased key. *)
let init_specials () =
  List.iter
    (fun (h : house) ->
      match String.lowercase_ascii h.h_key with
      | "troopers" -> Game.init_cards ~cost_fn:Troopers.conscription_cost h
      | "antimancer" -> Game.init_cards ~cost_fn:Antimancer.cost_fn h
      | "colors" -> Game.init_cards ~cost_fn:Colors.cost_fn h
      | "trappers" -> Game.init_cards ~cost_fn:Trappers.cost_fn h
      | _ -> Game.init_cards h)
    Catalog.specials

(* The houses a house tournament compares: the ones in [--special] and the
   ones in [--opponent-special], each of them once.  With no
   [--opponent-special] the [--special] list battles among itself. *)
let house_set a : house list =
  let names = a.special @ a.opponent_special in
  let found : house list ref = ref [] in
  List.iter
    (fun n ->
      let h = Catalog.find_special n in
      if not (List.exists (fun (x : house) -> x.h_key = h.h_key) !found) then found := h :: !found)
    names;
  List.rev !found

(* The house the bot tournament plays on both seats. *)
let single_house a =
  match a.special with
  | [ n ] -> Catalog.find_special n
  | [ ] -> Catalog.find_special "Troopers"
  | names ->
      failwith
        ("Tournament: the bot tournament plays one house for both seats, but --special lists "
       ^ String.concat "," names
       ^ " (use --mode houses to compare several)")

let house_pairs (houses : house list) : (house * house) list =
  (* every unordered pair, in the order the houses were given *)
  let rec go acc = function
    | [] -> acc
    | x :: rest -> go (acc @ List.map (fun y -> (x, y)) rest) rest
  in
  go [] houses

(* Which bot and which house sit at seat 0 in game [g] of a pairing.  The
   house alternates every game and the bots every other one, so over four
   games every house has met every bot from every seat. *)
let crossing (b1, b2) (h1, h2) g =
  let bots = if (g / 2) mod 2 = 0 then (b1, b2) else (b2, b1) in
  let houses = if g mod 2 = 0 then (h1, h2) else (h2, h1) in
  (bots, houses)

(* The house tournament: the houses are the variable and play a round robin
   against each other, the bots are the instrument.  Within a pairing the
   house at seat 0 alternates every game and the bots every other one, so
   every house meets every bot from every seat and the house rating measures
   the house, not the seat or the bot. *)
let run_houses a ctx =
  let houses : house list = house_set a in
  if List.length houses < 2 then
    failwith "Tournament: the house tournament needs at least two houses (--special and --opponent-special)";
  let bot_pairs = pairings a.bots in
  List.iter
    (fun ((h1, h2) : house * house) ->
      List.iter
        (fun (b1, b2) ->
          let key = entry_key "houses" ~first:b1 ~second:b2 ~specials:(h1.h_name, h2.h_name) in
          let done_ = Hashtbl.find_opt ctx.ctx_done key |> Option.value ~default:0 in
          for k = 0 to max 0 (a.games - done_) - 1 do
            let (bot_a, bot_b), (special_a, special_b) = crossing (b1, b2) (h1, h2) (done_ + k) in
            play_and_record ctx ~mode:"houses" ~budget:a.budget_ms ~max_turns:a.max_turns ~verbose:a.verbose
              ~bot_a ~bot_b ~special_a ~special_b
          done)
        bot_pairs)
    (house_pairs houses)

(* The bot tournament: the bots are the variable, every seat plays
   [--special], and the seat order alternates so neither bot has the first
   turn more often. *)
let run_bots a ctx =
  let special = single_house a in
  List.iter
    (fun (bot_a, bot_b) ->
      let key = entry_key "bots" ~first:bot_a ~second:bot_b ~specials:(special.h_name, special.h_name) in
      let done_ = Hashtbl.find_opt ctx.ctx_done key |> Option.value ~default:0 in
      for k = 0 to max 0 (a.games - done_) - 1 do
        let first, second = ordered_pair bot_a bot_b (done_ + k) in
        play_and_record ctx ~mode:"bots" ~budget:a.budget_ms ~max_turns:a.max_turns ~verbose:a.verbose
          ~bot_a:first ~bot_b:second ~special_a:special ~special_b:special
      done)
    (pairings a.bots)

let run_play a =
  let basics = [ Fire.house; Water.house; Air.house; Earth.house ] in
  Game.init_cards Fire.house;
  Game.init_cards Water.house;
  Game.init_cards Air.house;
  Game.init_cards Earth.house;
  init_specials ();
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
  let ctx =
    {
      ctx_basics = basics;
      ctx_rng = rng;
      ctx_out = out_dir;
      ctx_game_id = ref (List.fold_left (fun m e -> max m e.num) 0 existing);
      ctx_entries = ref [];
      ctx_done = Hashtbl.create 8;
    }
  in
  List.iter
    (fun e ->
      let k = entry_key a.mode ~first:e.first ~second:e.second ~specials:e.specials in
      Hashtbl.replace ctx.ctx_done k (1 + (Hashtbl.find_opt ctx.ctx_done k |> Option.value ~default:0)))
    existing;
  if a.mode = "houses" then run_houses a ctx else run_bots a ctx;
  let entries = existing @ List.rev !(ctx.ctx_entries) |> List.sort (fun a b -> compare a.num b.num) in
  let special_names = special_names_of entries in
  let full_stats, full_elos = compute_standings entries ~include_:[] ~exclude:[] in
  let full_cards = load_card_rates out_dir [] [] (card_min_plays a) in
  let full_houses = house_standings entries ~include_:[] ~exclude:[] in
  let full_matrix = house_matrix entries ~include_:[] ~exclude:[] in
  let filtered = a.include_ <> [] || a.exclude <> [] in
  let text =
    write_summary (Filename.concat out_dir "summary.json") full_stats full_elos full_cards special_names
      ~houses:full_houses ~matrix:full_matrix
  in
  let text =
    if filtered then
      let stats, elos = compute_standings entries ~include_:a.include_ ~exclude:a.exclude in
      let cards = load_card_rates out_dir a.include_ a.exclude (card_min_plays a) in
      let houses = house_standings entries ~include_:a.include_ ~exclude:a.exclude in
      let matrix = house_matrix entries ~include_:a.include_ ~exclude:a.exclude in
      write_summary (Filename.concat out_dir "summary-filtered.json") stats elos cards special_names ~houses
        ~matrix
    else text
  in
  let cards =
    if filtered then load_card_rates out_dir a.include_ a.exclude (card_min_plays a) else full_cards
  in
  let houses = if filtered then house_standings entries ~include_:a.include_ ~exclude:a.exclude else full_houses in
  let matrix = if filtered then house_matrix entries ~include_:a.include_ ~exclude:a.exclude else full_matrix in
  Printf.printf "wrote %d games to %s\n%s%!" (List.length entries) out_dir text;
  if a.mode = "houses" then (
    print_string (matrix_report matrix);
    print_string (houses_report ~title:"houses" houses);
    print_string (standings_report ~title:"bots (the instrument)" full_stats full_elos))
  else (
    print_string (standings_report ~title:"bots" full_stats full_elos);
    print_string (houses_report ~title:"house of the run" houses));
  print_string (cards_report cards);
  print_string (cards_report ~title:"top special cards" (special_card_rates cards special_names))

let run a = if a.mode = "summary" then run_summary a else run_play a
