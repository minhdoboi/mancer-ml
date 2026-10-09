(* Lecturer: UCT Monte-Carlo tree search.
   Port of priv/sp/bot/lecturer/LecturerBot.scala, with these changes:

   - Negamax values. Each node stores rewards from the side of the player who
     played its move, so the opponent picks its own best reply. Scala kept
     every node on the bot's side and only gave the opponent the exploration
     term ("fair" selection).
   - Heuristic priors. A node is expanded all at once and each child starts
     with [prior_visits] virtual visits at its {!Oracle.Eval.score}. Terminal
     children are exact. This replaces Scala's card-stat weighting.
   - Branching below the root comes from {!Oracle.base_moves} (pass plus a
     couple of placements per card). The root keeps every legal command.
   - Depth is capped by [max_depth] (Scala: 2) and the move with the most
     visits wins (Scala: best average reward).
   - A life margin, like the Summoner's. The score is a verdict in a decided
     position: the rollout kills the bot in every line, every leaf says -1 and
     the root children tie, so the search ranked nothing and fell back on the
     visit count, which is noise. The margin says what the line costs the bot
     — the life it has left after the opponent's answer — and it separates the
     lines the score cannot. The move that gets played is the best value, not
     the most visited one: in a decided position the visit count is noise.

   The engine is deterministic for the ported houses, so a node keeps the
   state its move produced and there are no chance nodes (Scala added up to 4
   duplicate children for random effects). *)

open Sp

type settings = {
  mutable logging : bool;
  mutable stats_log : bool;
  mutable duration : int;
  mutable rng : Random.State.t;
}

let default_settings () =
  { logging = false; stats_log = false; duration = 1000; rng = Random.State.make_self_init () }

module Params = struct
  let exploration = ref 0.7
  let max_depth = ref 4
  let prior_visits = ref 2.0
  let rollout_plies = ref 6
  let project_plies = ref 4
  let pass_weight = ref 0.5
  let life_weight = ref 0.1
end

let apply_overrides spec =
  let set k v =
    match k with
    | "exploration" -> Params.exploration := float_of_string v
    | "maxDepth" -> Params.max_depth := int_of_string v
    | "priorVisits" -> Params.prior_visits := float_of_string v
    | "rolloutPlies" -> Params.rollout_plies := int_of_string v
    | "projectPlies" -> Params.project_plies := int_of_string v
    | "passWeight" -> Params.pass_weight := float_of_string v
    | "lifeWeight" -> Params.life_weight := float_of_string v
    | other -> Printf.printf "lecturer: unknown option %s\n%!" other
  in
  spec |> String.split_on_char ','
  |> List.iter (fun kv ->
      let kv = String.trim kv in
      match String.index_opt kv '=' with
      | None -> ()
      | Some i -> set (String.sub kv 0 i) (String.sub kv (i + 1) (String.length kv - i - 1)));
  Printf.printf "lecturer: options %s\n%!" spec

let () = Option.iter apply_overrides (Sys.getenv_opt "LECTURER_OPTS")
let now_ms () = int_of_float (Unix.gettimeofday () *. 1000.)
let cmd_label = function None -> "pass" | Some c -> command_to_string c

type node = {
  move : command option;
  mover : player_id;  (** who played [move]; rewards are from this side *)
  state : game_state;
  next : player_id;
  ended : player_id option;
  depth : int;
  mutable children : node array option;
  mutable visits : float;
  mutable reward : float;  (** the score, summed over the visits *)
  margin : float;  (** the life margin of the line: what it costs the bot *)
}

type stats = { mutable sims : int; mutable iterations : int; mutable wins : int; mutable losses : int }

let mean n = if n.visits > 0.0 then n.reward /. n.visits else 0.0

(* The value the search ranks by: the score first, the life margin to break
   the ties the score cannot.  The score is a verdict in a decided position —
   the bot dies in every line, every leaf says -1, and the root children tie —
   so without the margin the search falls back on the visit count, which is
   noise, and plays an arbitrary move.  The summoner ranks the same way: the
   race decides, the life margin separates the lines the race cannot. *)
let value n = mean n +. (!Params.life_weight *. n.margin)
let signed ~bot_id v pid = if pid = bot_id then v else -.v

(* Terminal reward from the bot's side. *)
let terminal ~bot_id winner = if winner = bot_id then 1.0 else -1.0

(* How much of its life the bot still has, relative to the position the search
   started from.  The score says how the position looks; this says what the
   line costs the bot, and it is what separates the lines of a decided game —
   the summoner calls it the life margin. *)
let life_margin ~bot_id ~root (state : game_state) =
  let full = float_of_int root.players.(bot_id).ps_life in
  let left = float_of_int state.players.(bot_id).ps_life in
  if full <= 0. then 0. else max 0. (left /. full)

let simulate ~desc ~stats state pid cmd =
  stats.sims <- stats.sims + 1;
  Game.simulate desc state pid cmd

(* Let both sides pass for [project_plies] so pending attacks land, then score
   against the root position. *)
let leaf ~desc ~stats ~bot_id ~root (s : Game.sim) =
  match s.ended with
  | Some w -> terminal ~bot_id w
  | None -> (
      let st = ref s.state and pid = ref s.next and ended = ref None and r = ref 0 in
      while !r < !Params.project_plies && Option.is_none !ended do
        let s = simulate ~desc ~stats !st !pid None in
        st := s.state;
        pid := s.next;
        ended := s.ended;
        incr r
      done;
      match !ended with
      | Some w -> terminal ~bot_id w
      | None -> Oracle.Eval.score ~projected:!st ~before:root ~bot_id)

(* The margin of a line: the life the bot has left once the opponent has
   answered it.  One ply, not the whole projection: the question the margin
   answers is "does this line keep me alive this turn", and a longer horizon
   answers a different one — the bot dies a ply later in every line of a lost
   game, and the margin would read zero everywhere.

   It is measured on the line the tree walked, not on the random continuation
   the rollout plays: a random continuation kills the bot in a decided game,
   and a margin read there is zero in every line. *)
let margin_of ~desc ~stats ~bot_id ~root (s : Game.sim) =
  let st = ref s.state and pid = ref s.next and ended = ref s.ended in
  if Option.is_none !ended then (
    let s = simulate ~desc ~stats !st !pid None in
    st := s.state;
    pid := s.next;
    ended := s.ended);
  life_margin ~bot_id ~root !st

let expand ~desc ~stats ~bot_id ~root (parent : node) =
  let pid = parent.next in
  let moves =
    if parent.depth = 0 then None :: List.map (fun c -> Some c) (Game.legal_commands pid desc parent.state)
    else Oracle.base_moves ~desc pid parent.state
  in
  let child move =
    let s = simulate ~desc ~stats parent.state pid move in
    let prior =
      match s.ended with
      | Some w -> terminal ~bot_id w
      | None -> Oracle.Eval.score ~projected:s.state ~before:root ~bot_id
    in
    (* The margin is what the root move costs: only the root's children are
       priced, the search below the root works on the score alone. *)
    let margin = if parent.depth = 0 then margin_of ~desc ~stats ~bot_id ~root s else 0.0 in
    let pv = !Params.prior_visits in
    {
      move;
      mover = pid;
      state = s.state;
      next = s.next;
      ended = s.ended;
      depth = parent.depth + 1;
      children = None;
      visits = pv;
      reward = pv *. signed ~bot_id prior pid;
      margin;
    }
  in
  let cs = Array.of_list (List.map child moves) in
  parent.children <- Some cs;
  cs

let uct parent_visits (c : node) =
  value c +. (!Params.exploration *. sqrt (log (max 1.0 parent_visits) /. c.visits))

let select (parent : node) cs =
  let best = ref cs.(0) and best_v = ref neg_infinity in
  Array.iter
    (fun c ->
      let v = uct parent.visits c in
      if v > !best_v then (
        best := c;
        best_v := v))
    cs;
  !best

(* Random playout: pass with weight [pass_weight], otherwise a uniform pick
   among the base policy's placements. *)
let rollout ~desc ~stats ~rng ~bot_id ~root (n : node) =
  let rec go (s : Game.sim) ply =
    if Option.is_some s.ended || ply >= !Params.rollout_plies then s
    else
      let cmds = List.filter_map Fun.id (Oracle.base_moves ~desc s.next s.state) in
      let cmd =
        match cmds with
        | [] -> None
        | _ ->
            let k = List.length cmds in
            let total = !Params.pass_weight +. float_of_int k in
            let x = Random.State.float rng total in
            if x < !Params.pass_weight then None
            else Some (List.nth cmds (min (k - 1) (int_of_float (x -. !Params.pass_weight))))
      in
      go (simulate ~desc ~stats s.state s.next cmd) (ply + 1)
  in
  let s = go { state = n.state; next = n.next; ended = n.ended } 0 in
  Option.iter
    (fun w -> if w = bot_id then stats.wins <- stats.wins + 1 else stats.losses <- stats.losses + 1)
    s.ended;
  leaf ~desc ~stats ~bot_id ~root s

let iterate ~desc ~stats ~rng ~bot_id ~root (root_node : node) =
  let rec descend path (n : node) =
    let path = n :: path in
    if Option.is_some n.ended || n.depth >= !Params.max_depth then (path, n)
    else
      match n.children with
      | Some cs when Array.length cs > 0 -> descend path (select n cs)
      | Some _ -> (path, n)
      | None ->
          let cs = expand ~desc ~stats ~bot_id ~root n in
          if Array.length cs = 0 then (path, n)
          else
            let c = select n cs in
            (c :: path, c)
  in
  let path, leaf_node = descend [] root_node in
  let r =
    match leaf_node.ended with
    | Some w -> terminal ~bot_id w
    | None -> rollout ~desc ~stats ~rng ~bot_id ~root leaf_node
  in
  List.iter
    (fun n ->
      n.visits <- n.visits +. 1.0;
      n.reward <- n.reward +. signed ~bot_id r n.mover)
    path;
  stats.iterations <- stats.iterations + 1

let make_root ~pid state =
  {
    move = None;
    mover = other pid;
    state;
    next = pid;
    ended = None;
    depth = 0;
    children = None;
    visits = 0.0;
    reward = 0.0;
    margin = 0.0;
  }

let search_with_stats settings ~pid ~desc start =
  let t0 = now_ms () in
  let deadline = t0 + settings.duration in
  let stats = { sims = 0; iterations = 0; wins = 0; losses = 0 } in
  let root = make_root ~pid start in
  let go () = iterate ~desc ~stats ~rng:settings.rng ~bot_id:pid ~root:start root in
  go ();
  (* Stop early on a winning move: it is exact and visits will not change it. *)
  let lethal () =
    match root.children with Some cs -> Array.exists (fun c -> c.ended = Some pid) cs | None -> false
  in
  while (not (lethal ())) && now_ms () < deadline do
    go ()
  done;
  (root, stats, t0)

let search settings ~pid ~desc start =
  let root, _, _ = search_with_stats settings ~pid ~desc start in
  root

let root_children root =
  match root.children with
  | None -> []
  | Some cs -> Array.to_list cs |> List.map (fun c -> (c.move, int_of_float c.visits, mean c))

(* [(move, visits, score mean, value)] for each root move: the value is the
   score plus the life margin, the number the search ranks by. *)
let root_values root =
  match root.children with
  | None -> []
  | Some cs ->
      Array.to_list cs |> List.map (fun c -> (c.move, int_of_float c.visits, mean c, value c))

(* The move the search plays: the one that wins outright, else the best value.
   The visit count used to decide, but in a decided position every line scores
   alike and the visits are noise — the value is the signal.  The margin is
   deterministic, so it separates the lines even where the score cannot. *)
let best_child root =
  match root.children with
  | None | Some [||] -> None
  | Some cs -> (
      match Array.to_list cs |> List.find_opt (fun c -> c.ended = Some root.next) with
      | Some c -> Some c
      | None ->
          Some
            (Array.fold_left
               (fun a c ->
                 if value c > value a || (value c = value a && c.visits > a.visits) then c else a)
               cs.(0) cs))

let best_move root = Option.bind (best_child root) (fun c -> c.move)

let execute settings ~pid ~desc start =
  if is_disabled start.players.(pid) then None
  else
    let root, stats, t0 = search_with_stats settings ~pid ~desc start in
    match best_child root with
    | None -> None
    | Some best ->
        if settings.logging || settings.stats_log then
          Printf.printf
            "lecturer-stats iterations=%d wins=%d losses=%d sims=%d spentMs=%d/budgetMs=%d best=%.3f n=%.0f \
             cmd=%s\n\
             %!"
            stats.iterations stats.wins stats.losses stats.sims
            (now_ms () - t0)
            settings.duration (mean best) best.visits (cmd_label best.move);
        if settings.logging || Option.is_some (Sys.getenv_opt "LECTURER_DEBUG") then (
          print_endline (state_to_string start);
          root_values root
          |> List.sort (fun (_, _, _, a) (_, _, _, b) -> compare b a)
          |> List.map (fun (m, n, v, w) -> Printf.sprintf "%s n=%d mean=%.3f value=%.3f" (cmd_label m) n v w)
          |> String.concat " | " |> Printf.printf "  root: %s\n%!");
        best.move

let mover settings = Belief.blind (execute settings)
