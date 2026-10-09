(* Summoner: iterative-deepening best-first settle of a shallow game tree.
   Port of priv/sp/bot/summoner/SummonerBot.scala.

   Each pass rebuilds from scratch at depth 2, 3, … while budget remains. Only a
   fully-settled pass may overwrite the chosen command. Leaves are scored by
   {!Heuris.of_state} (race) unless [heuris] is [lifemana]. *)

open Sp

type settings = {
  mutable logging : bool;
  mutable stats_log : bool;
  mutable duration : int;
  mutable heuris : string;
}

let default_settings () = { logging = false; stats_log = false; duration = 1000; heuris = "race" }

let apply_overrides spec =
  (* Applied on the next [default_settings] via env; execute reads settings. *)
  Printf.printf "summoner: options %s\n%!" spec

let () = Option.iter apply_overrides (Sys.getenv_opt "SUMMONER_OPTS")
let now_ms () = int_of_float (Unix.gettimeofday () *. 1000.)
let cmd_label = function None -> "pass" | Some c -> command_to_string c
let max_pass_depth = 8

type node = {
  id : int;
  from : game_state;
  player_id : player_id;
  ended : player_id option;
  command : command option;
  mutable pending : command option list;
  mutable expanded : int;
  mutable survey : int;
  mutable children : node list;
  mutable value : float;
  mutable scored : bool;
  mutable num_sim : float;
  mutable wins : int;
  mutable losses : int;
}

let fresh_id =
  let n = ref 0 in
  fun () ->
    incr n;
    !n

let is_leaf n = Option.is_some n.ended

let fill_pending ~desc n =
  if not (is_leaf n) then (
    let moves = Game.legal_commands n.player_id desc n.from in
    let n_cards =
      let seen = Hashtbl.create 8 in
      List.iter (fun c -> Hashtbl.replace seen c.cmd_card.id ()) moves;
      Hashtbl.length seen
    in
    let rec take k = function
      | xs when k <= 0 -> ([], xs)
      | [] -> ([], [])
      | x :: xs ->
          let a, b = take (k - 1) xs in
          (x :: a, b)
    in
    let first, rest = take n_cards moves in
    n.pending <- List.map (fun c -> Some c) first @ (None :: List.map (fun c -> Some c) rest);
    n.survey <- n_cards + 1)

let make ~desc state pid ended cmd =
  let n =
    {
      id = fresh_id ();
      from = state;
      player_id = pid;
      ended;
      command = cmd;
      pending = [];
      expanded = 0;
      survey = 0;
      children = [];
      value = 0.;
      scored = false;
      num_sim = 0.;
      wins = 0;
      losses = 0;
    }
  in
  fill_pending ~desc n;
  n

let value_of n = n.value

let rec pull ~bot_id n =
  match n.children with
  | [] -> ()
  | kids ->
      let maximizing = n.player_id = bot_id in
      let rec go v sims wins losses = function
        | [] -> (v, sims, wins, losses)
        | c :: rest ->
            let better = if maximizing then c.value > v else c.value < v in
            go (if better then c.value else v) (sims +. c.num_sim) (wins + c.wins) (losses + c.losses) rest
      in
      let hd = List.hd kids in
      let v, sims, wins, losses = go hd.value hd.num_sim hd.wins hd.losses (List.tl kids) in
      n.scored <- true;
      n.value <- v;
      n.num_sim <- sims;
      n.wins <- wins;
      n.losses <- losses

let rec score_leaf ~heuris ~start ~bot_id ~desc n parents =
  let reward =
    match n.ended with
    | Some p -> if p = bot_id then 1. else -1.
    | None -> heuris n.from n.player_id
  in
  n.scored <- true;
  n.value <- reward;
  n.num_sim <- 1.;
  n.wins <- (if n.ended = Some bot_id then 1 else 0);
  n.losses <- (if Option.is_some n.ended && n.ended <> Some bot_id then 1 else 0);
  List.iter (pull ~bot_id) parents;
  ignore desc;
  ignore start

let needs_answer ~bot_id n =
  n.pending <> [] && (not (is_leaf n)) && value_of n > -1.

let rec max_by f = function
  | [] -> invalid_arg "Summoner.max_by"
  | x :: xs -> List.fold_left (fun a b -> if f b > f a then b else a) x xs

let expand ~desc parent =
  match parent.pending with
  | [] -> invalid_arg "Summoner.expand: empty pending"
  | cmd :: rest ->
      parent.pending <- rest;
      parent.expanded <- parent.expanded + 1;
      let s = Game.simulate desc parent.from parent.player_id cmd in
      let child = make ~desc s.state s.next s.ended cmd in
      (* Don't fill below the current max depth: caller clears pending when at cap. *)
      parent.children <- child :: parent.children;
      child

type loc = { node : node; parents : node list; depth : int }

let should_open ~bot_id loc =
  let n = loc.node in
  if is_leaf n || n.pending = [] then false
  else if n.player_id = bot_id && List.exists (fun c -> c.ended = Some bot_id) n.children then false
  else if n.expanded = 0 then true
  else if n.player_id <> bot_id then value_of n > -1.
  else if n.expanded < n.survey then true
  else
    match n.children with
    | [] -> false
    | kids ->
        let leader = max_by value_of kids in
        leader.expanded >= 2 || leader.pending = []

let contender ~bot_id loc =
  let trees = loc.node.children in
  if trees = [] || List.exists (fun c -> c.ended = Some bot_id) trees then None
  else
    let open_ = List.filter (needs_answer ~bot_id) trees in
    if open_ = [] then None
    else
      let floor =
        List.filter (fun t -> not (needs_answer ~bot_id t)) trees
        |> List.fold_left (fun m t -> max m (value_of t)) (-1.1)
      in
      match List.filter (fun t -> value_of t >= floor) open_ with
      | [] -> None
      | live -> Some (max_by value_of live)

let rec focus ~bot_id ~heuris ~start ~desc ~max_depth loc =
  let n = loc.node in
  if is_leaf n || loc.depth >= max_depth then if n.scored then None else Some loc
  else if n.player_id = bot_id && List.exists (fun c -> c.ended = Some bot_id) n.children then None
  else if should_open ~bot_id loc then (
    let child = expand ~desc loc.node in
    if loc.depth + 1 >= max_depth then child.pending <- [];
    focus ~bot_id ~heuris ~start ~desc ~max_depth
      { node = child; parents = loc.node :: loc.parents; depth = loc.depth + 1 })
  else if n.player_id <> bot_id then None
  else
    match contender ~bot_id loc with
    | Some child ->
        focus ~bot_id ~heuris ~start ~desc ~max_depth
          { node = child; parents = loc.node :: loc.parents; depth = loc.depth + 1 }
    | None -> None

let pick_best root =
  match root.children with
  | [] -> None
  | kids ->
      let rec go acc = function
        | [] -> acc.command
        | c :: rest ->
            let dv = c.value -. acc.value in
            let next =
              if dv > 0. then c
              else if dv < 0. then acc
              else if Option.is_some c.command && Option.is_none acc.command then c
              else acc
            in
            go next rest
      in
      go (List.hd kids) (List.tl kids)

let all_scored n = List.for_all (fun c -> c.scored) n.children && n.children <> []

let search_pass ~settings ~heuris ~start ~desc ~bot_id ~depth ~deadline =
  let saved_duration = settings.duration in
  settings.duration <- max 1 (deadline - now_ms ());
  Fun.protect
    ~finally:(fun () -> settings.duration <- saved_duration)
    (fun () ->
      let root = make ~desc start bot_id None None in
      let loc = { node = root; parents = []; depth = 0 } in
      let rec loop () =
        if now_ms () >= deadline then ()
        else
          match focus ~bot_id ~heuris ~start ~desc ~max_depth:depth loc with
          | Some selected when not selected.node.scored ->
              score_leaf ~heuris ~start ~bot_id ~desc selected.node selected.parents;
              loop ()
          | None when root.pending <> [] ->
              let child = expand ~desc root in
              if 1 >= depth then child.pending <- [];
              (match
                 focus ~bot_id ~heuris ~start ~desc ~max_depth:depth
                   { node = child; parents = [ root ]; depth = 1 }
               with
              | Some selected when not selected.node.scored ->
                  score_leaf ~heuris ~start ~bot_id ~desc selected.node selected.parents
              | _ -> ());
              loop ()
          | _ -> ()
      in
      loop ();
      (root, pick_best root, all_scored root))

let execute settings ~pid ~desc start =
  let t0 = now_ms () in
  let deadline = t0 + settings.duration in
  let bot_id = pid in
  if is_disabled start.players.(bot_id) then None
  else
    let heuris state to_move =
      if settings.heuris = "lifemana" then Heuris.life_mana ~bot_id ~start state *. 2. -. 1.
      else Heuris.of_state ~bot_id ~to_move state
    in
    let depth = ref 2 in
    let best = ref None in
    let last = ref None in
    let deepening = ref true in
    while !deepening && now_ms () < deadline do
      let root, cmd, settled = search_pass ~settings ~heuris ~start ~desc ~bot_id ~depth:!depth ~deadline in
      last := Some root;
      if settled && Option.is_some cmd then (
        best := cmd;
        if List.for_all is_leaf root.children then deepening := false);
      if now_ms () >= deadline || !depth >= max_pass_depth then deepening := false else incr depth
    done;
    if Option.is_none !best then best := Option.bind !last pick_best;
    if settings.logging || settings.stats_log then
      Printf.printf "summoner-stats depth=%d spentMs=%d/budgetMs=%d cmd=%s\n%!" !depth (now_ms () - t0)
        settings.duration (cmd_label !best);
    !best

let mover settings u pid = execute settings ~pid ~desc:u.desc u.state
