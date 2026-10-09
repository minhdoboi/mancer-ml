(* A live game a UI or a bot loop can drive one move at a time: HeadlessGame's
   turn loop (Tournament.scala) turned inside out. Events reach the listener
   given at creation, Scala's UpdateListener plus three turn-loop events. *)

open Sp
open Engine

type status = To_move of player_id | Over of { winner : player_id option; reason : string }

type t = {
  u : updater;
  desc : game_desc;
  max_turns : int;
  mutable next : player_id;
  mutable turns : int;
  mutable moves : Game_record.move list; (* newest first *)
  mutable over_sent : bool;
}

(* Resolve what [pid]'s submit queued: a Wait_player transition hands the turn
   over at once; otherwise run attacks and the end-of-turn chain. Returns who
   moves next. *)
let drain u pid =
  let running () = Option.is_none (Updater.ended u) in
  let rec loop pid =
    if not (running ()) then pid
    else
      let popped = ref None in
      Game.step u (fun _ -> popped := Player.pop_transition (Updater.player u pid));
      match !popped with
      | Some (Wait_player (next, _)) -> next
      | None ->
          let p = Updater.player u pid in
          let op = Player.other p in
          List.iter
            (fun f -> if running () then Game.step u (fun _ -> f ()))
            [
              (fun () -> Player.run_slots p);
              (fun () -> Player.apply_effects p On_end_turn);
              (fun () -> Slots.toggle_run p.slots_update);
              (fun () -> Player.prepare_next_turn op);
              (fun () -> Player.apply_effects op On_turn);
            ];
          other pid
  in
  loop pid

let start u = Game.step u (fun u -> Array.iter (fun p -> Player.apply_effects p On_start) u.player_updates)

let status s =
  match Updater.ended s.u with
  | Some _ as winner -> Over { winner; reason = "ended" }
  | None when s.turns >= s.max_turns -> Over { winner = None; reason = "maxTurns" }
  | None -> To_move s.next

let announce s =
  match status s with
  | To_move player -> emit s.u (Ev_to_move { t = s.turns + 1; player })
  | Over { winner; reason } ->
      if not s.over_sent then (
        s.over_sent <- true;
        emit s.u (Ev_game_over { winner; reason }))

let create ?mana0 ?mana1 ?rng ?(max_turns = max_int) ?(on_event = ignore) desc =
  let u = Updater.create ?rng (Game.initial_game_state ?mana0 ?mana1 desc) desc in
  Updater.listen u on_event;
  start u;
  let s = { u; desc; max_turns; next = owner; turns = 0; moves = []; over_sent = false } in
  announce s;
  s

let updater s = s.u
let desc s = s.desc
let state s = s.u.state
let peek s = Updater.peek s.u
let turns s = s.turns
let moves s = List.rev s.moves

let legal_commands s =
  match status s with To_move pid -> Game.available_commands s.u (Updater.player s.u pid) | Over _ -> []

let submit s cmd =
  match status s with
  | Over _ -> invalid_arg "Session.submit: the game is over"
  | To_move pid ->
      Option.iter
        (fun c ->
          if c.cmd_player <> pid then
            invalid_arg
              (Printf.sprintf "Session.submit: player %d to move, got a command for %d" pid c.cmd_player))
        cmd;
      let t = s.turns + 1 in
      s.moves <- Game_record.move_of ~t ~player:pid cmd s.u.state s.desc :: s.moves;
      emit s.u (Ev_submitted { t; player = pid; command = cmd });
      Game.step s.u (fun _ -> Player.submit (Updater.player s.u pid) cmd);
      s.next <- drain s.u pid;
      s.turns <- t;
      announce s

(* The -v trace: the four lines the engine printed before events were typed *)
let trace_line = function
  | Ev_summon { num; creature; _ } -> Some (Printf.sprintf "summon %d %s" num creature.c_card.name)
  | Ev_spell_played c -> Some (Printf.sprintf "spell %s" c.cmd_card.name)
  | Ev_focus { num; _ } -> Some (Printf.sprintf "focus %d" num)
  | Ev_replaced { num; creature; _ } ->
      Some (Printf.sprintf "slot %d refilled with %s" num creature.c_card.name)
  | _ -> None

(* ------------------------------------------------------------------- driver *)

type result = { winner : player_id option; turns : int; reason : string; moves : Game_record.move list }

let play_game ?mana0 ?mana1 ?rng ?(bots = (Game.greedy_mover, Game.greedy_mover)) (desc : game_desc)
    ~max_turns ~verbose =
  let on_event = if verbose then fun e -> Option.iter print_endline (trace_line e) else ignore in
  let s = create ?mana0 ?mana1 ?rng ~max_turns ~on_event desc in
  let mover pid = if pid = owner then fst bots else snd bots in
  let rec loop () =
    match status s with
    | Over { winner; reason } -> { winner; turns = s.turns; reason; moves = moves s }
    | To_move pid ->
        let p = Updater.player s.u pid in
        submit s (if is_disabled p.pstate then None else mover pid s.u pid);
        loop ()
  in
  loop ()
