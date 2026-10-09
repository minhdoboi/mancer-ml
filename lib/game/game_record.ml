(* Tournament / headless JSON schema.  Mirrors priv/sp/GameRecord.scala. *)

open Sp

type move = {
  t : int;
  player : player_id;
  card : string option;
  house : string option;
  cost : int option;
  slot : int option;
  side : string option;
  on : string option;
  vs : string option;
}

type t = {
  game : string;
  bot_names : string * string;
  starting_player : player_id;
  special_names : string * string;
  decks : player_desc array;
  mana : int list array;
  rng_seed : int;
  winner_bot : string option;
  winner_player : player_id option;
  draw : bool;
  turns : int;
  reason : string;
  moves : move list;
}

(* JSON as read from disk. House/card strings are looked up later. *)
type raw_deck = (string * string list) list

type parsed = {
  game : string;
  bot_names : string * string;
  starting_player : player_id;
  special_names : string * string;
  decks : raw_deck array;
  mana : int list array;
  rng_seed : int;
  winner_bot : string option;
  winner_player : player_id option;
  draw : bool;
  turns : int;
  reason : string;
  moves : move list;
}

let esc s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let occupant (state : game_state) pid num =
  if not (in_slot_range num) then None
  else match state.players.(pid).ps_slots.(num) with None -> None | Some st -> Some st.card.c_card.key

let vs_of player (cmd : command) state =
  match cmd.cmd_input with
  | None -> None
  | Some num -> (
      match cmd.cmd_card.input_spec with
      | Select_owner_creature -> occupant state player num
      | Select_any_creature ->
          let side = if cmd.cmd_flag = Some Target_other then other player else player in
          occupant state side num
      | _ -> occupant state (other player) num)

let house_name (desc : player_desc) (card : card) =
  match Array.find_opt (fun h -> h.phd_house.h_index = card.house_index) desc.pd_houses with
  | Some h -> h.phd_house.h_name
  | None -> card.name

let side_on (cmd : command) =
  match cmd.cmd_card.input_spec with
  | Select_any_creature ->
      let side = if cmd.cmd_flag = Some Target_other then "opp" else "own" in
      (Some side, Some "creature")
  | Select_owner_creature -> (Some "own", Some "creature")
  | Select_owner_slot | Select_owner _ -> (Some "own", Some "slot")
  | Select_target_creature -> (Some "opp", Some "creature")
  | Select_target_slot | Select_target _ -> (Some "opp", Some "slot")
  | No_input -> (None, None)

let move_of ~t ~player cmd (state : game_state) (desc : game_desc) =
  match cmd with
  | None ->
      { t; player; card = None; house = None; cost = None; slot = None; side = None; on = None; vs = None }
  | Some c ->
      let side, on = side_on c in
      {
        t;
        player;
        card = Some c.cmd_card.key;
        house = Some (house_name desc.gd_players.(player) c.cmd_card);
        cost = Some c.cmd_cost;
        slot = c.cmd_input;
        side;
        on;
        vs = vs_of player c state;
      }

let special_name (d : player_desc) =
  match Array.find_opt (fun h -> h.phd_house.h_index = 4) d.pd_houses with
  | Some h -> h.phd_house.h_name
  | None -> ( match Array.length d.pd_houses with 0 -> "none" | n -> d.pd_houses.(n - 1).phd_house.h_name)

let winner_class (r : t) =
  match r.winner_player with
  | Some p -> Some (if p = owner then fst r.special_names else snd r.special_names)
  | None -> None

let to_json (r : t) =
  let deck_json (d : player_desc) =
    Array.to_list d.pd_houses
    |> List.map (fun h ->
        let cards =
          Array.to_list h.phd_cards
          |> List.map (fun cd -> Printf.sprintf {|"%s"|} (esc cd.cd_card.key))
          |> String.concat ", "
        in
        Printf.sprintf {|      "%s": [%s]|} (esc h.phd_house.h_key) cards)
    |> String.concat ",\n"
  in
  let extra m =
    (match m.slot with None -> "" | Some s -> Printf.sprintf {|, "slot": %d|} s)
    ^ (match m.side with None -> "" | Some s -> Printf.sprintf {|, "side": "%s"|} (esc s))
    ^ (match m.on with None -> "" | Some o -> Printf.sprintf {|, "on": "%s"|} (esc o))
    ^ match m.vs with None -> "" | Some v -> Printf.sprintf {|, "vs": "%s"|} (esc v)
  in
  let moves_json =
    r.moves
    |> List.map (fun m ->
        match m.card with
        | Some card ->
            Printf.sprintf {|    {"t": %d, "player": %d, "card": "%s", "house": "%s", "cost": %d%s}|} m.t
              m.player (esc card)
              (esc (Option.value m.house ~default:""))
              (Option.value m.cost ~default:0) (extra m)
        | None -> Printf.sprintf {|    {"t": %d, "player": %d, "pass": true}|} m.t m.player)
    |> String.concat ",\n"
  in
  let winner = match r.winner_bot with None -> "null" | Some b -> Printf.sprintf {|"%s"|} (esc b) in
  let winner_player = match r.winner_player with None -> "null" | Some p -> string_of_int p in
  let ints xs = "[" ^ String.concat ", " (List.map string_of_int xs) ^ "]" in
  let mana0 = if Array.length r.mana > 0 then r.mana.(0) else [] in
  let mana1 = if Array.length r.mana > 1 then r.mana.(1) else [] in
  Printf.sprintf
    {|{
  "game": "%s",
  "bots": ["%s", "%s"],
  "startingPlayer": %d,
  "specials": ["%s", "%s"],
  "decks": [
    {
%s
    },
    {
%s
    }
  ],
  "mana": [%s, %s],
  "rngSeed": %d,
  "winner": %s,
  "winnerPlayer": %s,
  "draw": %b,
  "turns": %d,
  "reason": "%s",
  "moves": [
%s
  ]
}
|}
    (esc r.game)
    (esc (fst r.bot_names))
    (esc (snd r.bot_names))
    r.starting_player
    (esc (fst r.special_names))
    (esc (snd r.special_names))
    (deck_json r.decks.(0))
    (deck_json r.decks.(1))
    (ints mana0) (ints mana1) r.rng_seed winner winner_player r.draw r.turns (esc r.reason) moves_json

(* ----------------------------------------------------------------- parser *)

let skip_ws s i =
  let n = String.length s in
  let rec go i = if i >= n then i else match s.[i] with ' ' | '\n' | '\r' | '\t' -> go (i + 1) | _ -> i in
  go i

let matching_brace s start =
  let n = String.length s in
  if start >= n || s.[start] <> '{' then failwith "Game_record: expected object";
  let rec go i depth in_str escape =
    if i >= n then failwith "Game_record: unterminated object";
    let c = s.[i] in
    if in_str then
      if escape then go (i + 1) depth true false
      else if c = '\\' then go (i + 1) depth true true
      else if c = '"' then go (i + 1) depth false false
      else go (i + 1) depth true false
    else
      match c with
      | '"' -> go (i + 1) depth true false
      | '{' -> go (i + 1) (depth + 1) false false
      | '}' -> if depth = 1 then i + 1 else go (i + 1) (depth - 1) false false
      | _ -> go (i + 1) depth false false
  in
  go (start + 1) 1 false false

let matching_bracket s start =
  let n = String.length s in
  if start >= n || s.[start] <> '[' then failwith "Game_record: expected array";
  let rec go i depth in_str escape =
    if i >= n then failwith "Game_record: unterminated array";
    let c = s.[i] in
    if in_str then
      if escape then go (i + 1) depth true false
      else if c = '\\' then go (i + 1) depth true true
      else if c = '"' then go (i + 1) depth false false
      else go (i + 1) depth true false
    else
      match c with
      | '"' -> go (i + 1) depth true false
      | '[' -> go (i + 1) (depth + 1) false false
      | ']' -> if depth = 1 then i + 1 else go (i + 1) (depth - 1) false false
      | '{' ->
          let j = matching_brace s i in
          go j depth false false
      | _ -> go (i + 1) depth false false
  in
  go (start + 1) 1 false false

let find_key s key =
  let tok = "\"" ^ key ^ "\"" in
  let rec go from =
    match String.index_from_opt s from tok.[0] with
    | None -> None
    | Some i ->
        if i + String.length tok <= String.length s && String.sub s i (String.length tok) = tok then Some i
        else go (i + 1)
  in
  go 0

let after_colon s key =
  match find_key s key with
  | None -> None
  | Some i ->
      let j = skip_ws s (i + String.length key + 2) in
      if j < String.length s && s.[j] = ':' then Some (skip_ws s (j + 1)) else None

let parse_string_at s i =
  let i = skip_ws s i in
  if i >= String.length s || s.[i] <> '"' then failwith "Game_record: expected string";
  let b = Buffer.create 16 in
  let rec go i escape =
    if i >= String.length s then failwith "Game_record: unterminated string";
    let c = s.[i] in
    if escape then (
      (match c with
      | 'n' -> Buffer.add_char b '\n'
      | 'r' -> Buffer.add_char b '\r'
      | 't' -> Buffer.add_char b '\t'
      | '"' | '\\' -> Buffer.add_char b c
      | _ -> Buffer.add_char b c);
      go (i + 1) false)
    else if c = '\\' then go (i + 1) true
    else if c = '"' then (Buffer.contents b, i + 1)
    else (
      Buffer.add_char b c;
      go (i + 1) false)
  in
  go (i + 1) false

let parse_int_at s i =
  let i = skip_ws s i in
  let n = String.length s in
  let neg = i < n && s.[i] = '-' in
  let i0 = if neg then i + 1 else i in
  let rec go i = if i < n && s.[i] >= '0' && s.[i] <= '9' then go (i + 1) else i in
  let i1 = go i0 in
  if i1 = i0 then failwith "Game_record: expected int";
  let v = int_of_string (String.sub s i0 (i1 - i0)) in
  ((if neg then -v else v), i1)

let field_string obj key =
  match after_colon obj key with
  | None -> None
  | Some i ->
      let i = skip_ws obj i in
      if i < String.length obj && obj.[i] = 'n' then None else Some (fst (parse_string_at obj i))

let field_int obj key =
  match after_colon obj key with None -> None | Some i -> Some (fst (parse_int_at obj i))

let field_bool obj key =
  match after_colon obj key with
  | None -> None
  | Some i ->
      let i = skip_ws obj i in
      if i + 4 <= String.length obj && String.sub obj i 4 = "true" then Some true
      else if i + 5 <= String.length obj && String.sub obj i 5 = "false" then Some false
      else None

let parse_string_list s =
  let i = skip_ws s 0 in
  if i >= String.length s || s.[i] <> '[' then failwith "Game_record: expected string list";
  let rec go i acc =
    let i = skip_ws s i in
    if i >= String.length s then failwith "Game_record: unterminated list";
    match s.[i] with
    | ']' -> List.rev acc
    | '"' ->
        let v, j = parse_string_at s i in
        let j = skip_ws s j in
        let j = if j < String.length s && s.[j] = ',' then j + 1 else j in
        go j (v :: acc)
    | ',' -> go (i + 1) acc
    | _ -> failwith "Game_record: expected string in list"
  in
  go (i + 1) []

let parse_int_list s =
  let i = skip_ws s 0 in
  if i >= String.length s || s.[i] <> '[' then failwith "Game_record: expected int list";
  let rec go i acc =
    let i = skip_ws s i in
    if i >= String.length s then failwith "Game_record: unterminated list";
    match s.[i] with
    | ']' -> List.rev acc
    | ',' -> go (i + 1) acc
    | '-' | '0' .. '9' ->
        let v, j = parse_int_at s i in
        go j (v :: acc)
    | _ -> failwith "Game_record: expected int in list"
  in
  go (i + 1) []

let parse_deck obj =
  let rec go i acc =
    let i = skip_ws obj i in
    if i >= String.length obj then List.rev acc
    else
      match obj.[i] with
      | '{' | ',' -> go (i + 1) acc
      | '}' -> List.rev acc
      | '"' ->
          let name, j = parse_string_at obj i in
          let j = skip_ws obj j in
          if j >= String.length obj || obj.[j] <> ':' then failwith "Game_record: expected colon in deck";
          let j = skip_ws obj (j + 1) in
          if j >= String.length obj || obj.[j] <> '[' then failwith "Game_record: expected card list";
          let k = matching_bracket obj j in
          let cards = parse_string_list (String.sub obj j (k - j)) in
          go k ((name, cards) :: acc)
      | _ -> go (i + 1) acc
  in
  go 0 []

let parse_move obj : move =
  let pass = field_bool obj "pass" = Some true in
  {
    t = Option.value (field_int obj "t") ~default:0;
    player = Option.value (field_int obj "player") ~default:0;
    card = (if pass then None else field_string obj "card");
    house = (if pass then None else field_string obj "house");
    cost = (if pass then None else field_int obj "cost");
    slot = field_int obj "slot";
    side = field_string obj "side";
    on = field_string obj "on";
    vs = field_string obj "vs";
  }

let parse_object_list s =
  let i = skip_ws s 0 in
  if i >= String.length s || s.[i] <> '[' then failwith "Game_record: expected object list";
  let rec go i acc =
    let i = skip_ws s i in
    if i >= String.length s then failwith "Game_record: unterminated list";
    match s.[i] with
    | ']' -> List.rev acc
    | ',' -> go (i + 1) acc
    | '{' ->
        let j = matching_brace s i in
        go j (String.sub s i (j - i) :: acc)
    | _ -> failwith "Game_record: expected object in list"
  in
  go (i + 1) []

let of_json text : parsed =
  let game = Option.value (field_string text "game") ~default:"" in
  let bot_names =
    match after_colon text "bots" with
    | None -> ("", "")
    | Some i -> (
        let i = skip_ws text i in
        let k = matching_bracket text i in
        match parse_string_list (String.sub text i (k - i)) with a :: b :: _ -> (a, b) | _ -> ("", ""))
  in
  let special_names =
    match after_colon text "specials" with
    | None -> ("", "")
    | Some i -> (
        let i = skip_ws text i in
        let k = matching_bracket text i in
        match parse_string_list (String.sub text i (k - i)) with a :: b :: _ -> (a, b) | _ -> ("", ""))
  in
  let starting_player = Option.value (field_int text "startingPlayer") ~default:0 in
  let decks =
    match after_colon text "decks" with
    | None -> [| []; [] |]
    | Some i -> (
        let i = skip_ws text i in
        let k = matching_bracket text i in
        match parse_object_list (String.sub text i (k - i)) with
        | a :: b :: _ -> [| parse_deck a; parse_deck b |]
        | _ -> [| []; [] |])
  in
  let mana =
    match after_colon text "mana" with
    | None -> [| []; [] |]
    | Some i ->
        let i = skip_ws text i in
        let k = matching_bracket text i in
        let body = String.sub text i (k - i) in
        (* [[ints], [ints]] *)
        let inner = skip_ws body 1 in
        let k0 = matching_bracket body inner in
        let a = parse_int_list (String.sub body inner (k0 - inner)) in
        let rest = skip_ws body k0 in
        let rest = if rest < String.length body && body.[rest] = ',' then skip_ws body (rest + 1) else rest in
        let k1 = matching_bracket body rest in
        let b = parse_int_list (String.sub body rest (k1 - rest)) in
        [| a; b |]
  in
  let rng_seed = Option.value (field_int text "rngSeed") ~default:0 in
  let winner_bot =
    match after_colon text "winner" with
    | None -> None
    | Some i ->
        let i = skip_ws text i in
        if i + 4 <= String.length text && String.sub text i 4 = "null" then None
        else Some (fst (parse_string_at text i))
  in
  let winner_player =
    match after_colon text "winnerPlayer" with
    | None -> None
    | Some i ->
        let i = skip_ws text i in
        if i + 4 <= String.length text && String.sub text i 4 = "null" then None
        else Some (fst (parse_int_at text i))
  in
  let draw = Option.value (field_bool text "draw") ~default:false in
  let turns = Option.value (field_int text "turns") ~default:0 in
  let reason = Option.value (field_string text "reason") ~default:"" in
  let moves =
    match after_colon text "moves" with
    | None -> []
    | Some i ->
        let i = skip_ws text i in
        let k = matching_bracket text i in
        parse_object_list (String.sub text i (k - i)) |> List.map parse_move
  in
  {
    game;
    bot_names;
    starting_player;
    special_names;
    decks;
    mana;
    rng_seed;
    winner_bot;
    winner_player;
    draw;
    turns;
    reason;
    moves;
  }
