(* Two seats around a Session: the setup a UI offers (who plays, which
   specials), dealing as Tournament.play_one does, and per-card move choices
   for a human. *)

open Sp

type seat = Human | Bot of string

type setup = {
  seats : seat * seat;
  specials : string * string;
  budget_ms : int;
  seed : int option;
  max_turns : int;
}

let default_setup =
  {
    seats = (Human, Bot "oracle");
    specials = ("Troopers", "Troopers");
    budget_ms = 1000;
    seed = None;
    max_turns = 300;
  }

let seat_name = function Human -> "human" | Bot id -> id

(* Game.init_cards must run once per house *)
let catalog = lazy (Catalog.init ())

let special_houses () =
  Lazy.force catalog;
  Catalog.specials

type t = {
  setup : setup;
  seed : int;
  session : Session.t;
  movers : Game.mover option array;
  mana : int list array;
  rng_seed : int;
}

let start ?on_event (setup : setup) =
  Lazy.force catalog;
  let seed =
    match setup.seed with Some n -> n | None -> Random.State.bits (Random.State.make_self_init ())
  in
  let rng = Random.State.make [| seed |] in
  let s0, s1 = setup.specials in
  let specials = (Catalog.find_special s0, Catalog.find_special s1) in
  let p0, p1 = Shuffle.two_players ~rng ~specials Catalog.basics in
  let desc = { gd_players = [| p0.desc; p1.desc |] } in
  let rng_seed = Random.State.bits rng in
  let mover = function
    | Human -> None
    | Bot id -> Some (Tournament.mover ~stats_log:false ~budget:setup.budget_ms id)
  in
  let movers = [| mover (fst setup.seats); mover (snd setup.seats) |] in
  let mana = [| Array.to_list p0.mana; Array.to_list p1.mana |] in
  let session =
    Session.create desc ~mana0:mana.(0) ~mana1:mana.(1) ~rng:(Dice.make rng_seed) ~max_turns:setup.max_turns
      ?on_event
  in
  { setup; seed; session; movers; mana; rng_seed }

let session t = t.session
let setup t = t.setup
let seed t = t.seed
let seat t pid = if pid = owner then fst t.setup.seats else snd t.setup.seats
let to_move t = match Session.status t.session with To_move pid -> Some pid | Over _ -> None
let hand t pid = Game.player_houses pid (Session.desc t.session) (Session.state t.session)

let playable t cd h =
  match to_move t with
  | None -> false
  | Some pid -> is_available cd (Session.state t.session).players.(pid).ps_houses.(h.phd_house.h_index)

let choices t (card : card) =
  match to_move t with
  | None -> []
  | Some pid ->
      Game.legal_commands pid (Session.desc t.session) (Session.state t.session)
      |> List.filter (fun c -> c.cmd_card.key = card.key)
      |> List.sort (fun a b -> compare (a.cmd_flag, a.cmd_input) (b.cmd_flag, b.cmd_input))

let target_side (c : command) =
  match c.cmd_input with
  | None -> None
  | Some _ -> (
      match c.cmd_card.input_spec with
      | Select_target_slot | Select_target_creature | Select_target _ -> Some (other c.cmd_player)
      | Select_any_creature when c.cmd_flag = Some Target_other -> Some (other c.cmd_player)
      | _ -> Some c.cmd_player)

let submit t cmd = Session.submit t.session cmd

let bot_command t =
  match to_move t with
  | None -> invalid_arg "Table.bot_command: the game is over"
  | Some pid -> (
      match t.movers.(pid) with
      | None -> invalid_arg "Table.bot_command: a human is to move"
      | Some mover ->
          let u = Session.updater t.session in
          let p = Engine.Updater.player u pid in
          if is_disabled p.pstate then None else mover u pid)

let play_bot t = submit t (bot_command t)

let record t : Game_record.t =
  let desc = Session.desc t.session in
  let winner, reason =
    match Session.status t.session with
    | Over { winner; reason } -> (winner, reason)
    | To_move _ -> (None, "unfinished")
  in
  let name pid = seat_name (seat t pid) in
  {
    game = Printf.sprintf "tui-%d" t.seed;
    bot_names = (name owner, name opponent);
    starting_player = owner;
    special_names =
      (Game_record.special_name desc.gd_players.(0), Game_record.special_name desc.gd_players.(1));
    decks = desc.gd_players;
    mana = t.mana;
    rng_seed = t.rng_seed;
    winner_bot = Option.map name winner;
    winner_player = winner;
    draw = Option.is_none winner;
    turns = Session.turns t.session;
    reason;
    moves = Session.moves t.session;
  }

let event_line ?(name = fun (c : card) -> c.name) =
  let p pid = Printf.sprintf "P%d" (pid + 1) in
  function
  | Ev_to_move { t; player } -> Some (Printf.sprintf "-- turn %d: %s to move" t (p player))
  | Ev_submitted { player; command = None; _ } -> Some (Printf.sprintf "%s passes" (p player))
  | Ev_submitted { player; command = Some c; _ } ->
      let at =
        match (c.cmd_input, target_side c) with
        | Some n, Some side -> Printf.sprintf " at %s slot %d" (p side) n
        | _ -> ""
      in
      Some (Printf.sprintf "%s plays %s (%d)%s" (p player) (name c.cmd_card) c.cmd_cost at)
  | Ev_summon { num; player; creature } ->
      Some (Printf.sprintf "%s summons %s in slot %d" (p player) (name creature.c_card) num)
  | Ev_replaced { num; player; creature } ->
      Some (Printf.sprintf "%s slot %d becomes %s" (p player) num (name creature.c_card))
  | Ev_die { num; player } -> Some (Printf.sprintf "%s slot %d dies" (p player) num)
  | Ev_move { num; dest; player; dest_player } ->
      Some (Printf.sprintf "%s slot %d moves to %s slot %d" (p player) num (p dest_player) dest)
  | Ev_swap { num; dest; player; dest_player } ->
      Some (Printf.sprintf "%s slot %d swaps with %s slot %d" (p player) num (p dest_player) dest)
  | Ev_ability { kind; player; num; target } ->
      let on = match target with Some (tp, tn) -> Printf.sprintf " on %s slot %d" (p tp) tn | None -> "" in
      Some (Printf.sprintf "%s slot %d: %s%s" (p player) num kind on)
  | Ev_game_over { winner = Some w; reason } -> Some (Printf.sprintf "== %s wins (%s)" (p w) reason)
  | Ev_game_over { winner = None; reason } -> Some (Printf.sprintf "== draw (%s)" reason)
  | Ev_spell_played _ | Ev_run_slot _ | Ev_focus _ | Ev_refresh _ -> None

let save t ~dir =
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let path = Filename.concat dir (Printf.sprintf "game-%d-%.0f.json" t.seed (Unix.gettimeofday ())) in
  Out_channel.with_open_text path (fun oc -> output_string oc (Game_record.to_json (record t)));
  path
