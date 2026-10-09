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
  match Array.find_opt (fun h -> h.phd_house.h_index = special_house_index) d.pd_houses with
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

open Yojson.Basic.Util

let opt_list = function `Null -> [] | j -> to_list j
let pair f j = match opt_list j with a :: b :: _ -> (f a, f b) | _ -> (f `Null, f `Null)
let str_or_empty j = to_string_option j |> Option.value ~default:""

let parse_deck j : raw_deck =
  to_assoc j |> List.map (fun (house, cards) -> (house, to_list cards |> filter_string))

let parse_move j : move =
  let pass = member "pass" j |> to_bool_option = Some true in
  let str k = member k j |> to_string_option in
  {
    t = member "t" j |> to_int_option |> Option.value ~default:0;
    player = member "player" j |> to_int_option |> Option.value ~default:0;
    card = (if pass then None else str "card");
    house = (if pass then None else str "house");
    cost = (if pass then None else member "cost" j |> to_int_option);
    slot = member "slot" j |> to_int_option;
    side = str "side";
    on = str "on";
    vs = str "vs";
  }

let of_json text : parsed =
  let j =
    match Yojson_five.Basic.from_string text with Ok j -> j | Error e -> failwith ("Game_record: " ^ e)
  in
  let int k = member k j |> to_int_option |> Option.value ~default:0 in
  let decks = pair (function `Null -> [] | d -> parse_deck d) (member "decks" j) in
  let mana = pair (fun m -> opt_list m |> filter_int) (member "mana" j) in
  {
    game = member "game" j |> str_or_empty;
    bot_names = pair str_or_empty (member "bots" j);
    starting_player = int "startingPlayer";
    special_names = pair str_or_empty (member "specials" j);
    decks = [| fst decks; snd decks |];
    mana = [| fst mana; snd mana |];
    rng_seed = int "rngSeed";
    winner_bot = member "winner" j |> to_string_option;
    winner_player = member "winnerPlayer" j |> to_int_option;
    draw = member "draw" j |> to_bool_option |> Option.value ~default:false;
    turns = int "turns";
    reason = member "reason" j |> str_or_empty;
    moves = member "moves" j |> opt_list |> List.map parse_move;
  }
