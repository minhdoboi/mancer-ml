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
  else match state.players.(pid).ps_slots.(num) with None -> None | Some st -> Some st.card.c_card.name

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
        card = Some c.cmd_card.name;
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

let winner_class r =
  match r.winner_player with
  | Some p -> Some (if p = owner then fst r.special_names else snd r.special_names)
  | None -> None

let to_json r =
  let deck_json (d : player_desc) =
    Array.to_list d.pd_houses
    |> List.map (fun h ->
        let cards =
          Array.to_list h.phd_cards
          |> List.map (fun cd -> Printf.sprintf {|"%s"|} (esc cd.cd_card.name))
          |> String.concat ", "
        in
        Printf.sprintf {|      "%s": [%s]|} (esc h.phd_house.h_name) cards)
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
    winner winner_player r.draw r.turns (esc r.reason) moves_json
