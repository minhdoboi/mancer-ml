(* Oracle: rollout policy improvement with sequential halving.
   Port of priv/sp/bot/oracle/OracleBot.scala.

   No game tree. A root move is valued by playing it and then letting a cheap
   greedy player (the base policy) play both sides for a few plies through the
   real engine, then scoring the leaf. The opponent's first reply is a
   fixed-weight average over its best few replies. *)

open Sp

type settings = { mutable logging : bool; mutable stats_log : bool; mutable duration : int }

let default_settings () = { logging = false; stats_log = false; duration = 1000 }

module Params = struct
  let root_cards = ref 8
  let horizon = ref 5
  let placements_per_card = ref 2
  let reply_weights = ref [| 0.6; 0.25; 0.15 |]
  let first_round_samples = ref 6
  let project_plies = ref 4
  let win_discount = ref 0.02
  let screen_weight = ref 2.0
end

module Eval = struct
  let life_w = ref 1.0
  let life_curve = ref 1.0
  let attack_w = ref 0.8
  let creature_life_w = ref 0.4
  let mana_w = ref 1.0
  let growth_w = ref 6.0
  let scale = ref 60.0
  let extrapolate = ref 1.0
  let held_factor = ref 0.3

  let life_term life =
    let lost = min 60.0 (max 0.0 (60.0 -. life)) in
    (!life_w *. life) -. (!life_curve *. lost *. lost /. 60.0)

  let board (p : player_state) =
    Array.fold_left
      (fun v -> function
        | None -> v
        | Some s ->
            let held = if has_flag s on_hold then !held_factor else 1.0 in
            v +. (!attack_w *. float_of_int s.attack *. held) +. (!creature_life_w *. float_of_int s.life))
      0.0 p.ps_slots

  let stock (p : player_state) = Array.fold_left (fun a h -> a +. float_of_int h.mana) 0.0 p.ps_houses
  let income (p : player_state) = Array.fold_left (fun a h -> a +. float_of_int h.growth) 0.0 p.ps_houses

  let score ~projected ~before ~bot_id =
    let bot = projected.players.(bot_id) in
    let opp = projected.players.(other bot_id) in
    let b_now = float_of_int before.players.(bot_id).ps_life in
    let o_now = float_of_int before.players.(other bot_id).ps_life in
    let b_life = float_of_int bot.ps_life +. (!extrapolate *. (float_of_int bot.ps_life -. b_now)) in
    let o_life = float_of_int opp.ps_life +. (!extrapolate *. (float_of_int opp.ps_life -. o_now)) in
    let raw =
      life_term b_life -. life_term o_life +. board bot -. board opp
      +. (!mana_w *. (stock bot -. stock opp))
      +. (!growth_w *. (income bot -. income opp))
    in
    tanh (raw /. !scale)
end

let apply_overrides spec =
  let set k v =
    match k with
    | "rootCards" -> Params.root_cards := int_of_string v
    | "horizon" -> Params.horizon := int_of_string v
    | "placementsPerCard" -> Params.placements_per_card := int_of_string v
    | "replyWeights" ->
        Params.reply_weights := Array.of_list (List.map float_of_string (String.split_on_char ';' v))
    | "firstRoundSamples" -> Params.first_round_samples := int_of_string v
    | "projectPlies" -> Params.project_plies := int_of_string v
    | "winDiscount" -> Params.win_discount := float_of_string v
    | "screenWeight" -> Params.screen_weight := float_of_string v
    | "lifeW" -> Eval.life_w := float_of_string v
    | "lifeCurve" -> Eval.life_curve := float_of_string v
    | "attackW" -> Eval.attack_w := float_of_string v
    | "creatureLifeW" -> Eval.creature_life_w := float_of_string v
    | "manaW" -> Eval.mana_w := float_of_string v
    | "growthW" -> Eval.growth_w := float_of_string v
    | "scale" -> Eval.scale := float_of_string v
    | "extrapolate" -> Eval.extrapolate := float_of_string v
    | "heldFactor" -> Eval.held_factor := float_of_string v
    | other -> Printf.printf "oracle: unknown option %s\n%!" other
  in
  spec |> String.split_on_char ','
  |> List.iter (fun kv ->
      let kv = String.trim kv in
      match String.index_opt kv '=' with
      | None -> ()
      | Some i -> set (String.sub kv 0 i) (String.sub kv (i + 1) (String.length kv - i - 1)));
  Printf.printf "oracle: options %s\n%!" spec

let () = Option.iter apply_overrides (Sys.getenv_opt "ORACLE_OPTS")
let now_ms () = int_of_float (Unix.gettimeofday () *. 1000.)
let cmd_label = function None -> "pass" | Some c -> command_to_string c

type step = { cmd : command option; state : game_state; next : player_id; ended : player_id option }
type cand = { move : command option; screen : float; sum : float array; cnt : int array; mutable n : int }

let cand_mean c =
  let weights = !Params.reply_weights in
  let acc = ref 0.0 and w = ref 0.0 in
  Array.iteri
    (fun k wt ->
      if c.cnt.(k) > 0 then (
        acc := !acc +. (wt *. (c.sum.(k) /. float_of_int c.cnt.(k)));
        w := !w +. wt))
    weights;
  let played = !acc +. (c.screen *. (1.0 -. !w)) in
  ((!Params.screen_weight *. c.screen) +. played) /. (!Params.screen_weight +. 1.0)

let add_sample c rank v =
  c.sum.(rank) <- c.sum.(rank) +. v;
  c.cnt.(rank) <- c.cnt.(rank) + 1;
  c.n <- c.n + 1

let slot_of (p : player_state) n = if in_slot_range n then p.ps_slots.(n) else None
let defined p n = Option.is_some (slot_of p n)
let attack_of p n = match slot_of p n with Some st -> st.attack | None -> 0
let life_of p n = match slot_of p n with Some st -> st.life | None -> 0
let max_life_of p n = match slot_of p n with Some st -> st.max_life | None -> 1
let cmd_num c = Option.value c.cmd_input ~default:(-1)
let cmd_key c = (c.cmd_card.id, c.cmd_input, c.cmd_flag)

let distinct_cmds cs =
  let rec go acc = function
    | [] -> List.rev acc
    | c :: rest ->
        if List.exists (fun a -> cmd_key a = cmd_key c) acc then go acc rest else go (c :: acc) rest
  in
  go [] cs

let max_by f = function
  | [] -> invalid_arg "Oracle.max_by: empty"
  | x :: xs -> List.fold_left (fun a b -> if f b > f a then b else a) x xs

let min_by f = function
  | [] -> invalid_arg "Oracle.min_by: empty"
  | x :: xs -> List.fold_left (fun a b -> if f b < f a then b else a) x xs

let friends (p : player_state) n =
  List.fold_left (fun a i -> if defined p i then a + 1 else a) 0 (adjacents n)

let own_side (c : command) =
  match c.cmd_card.input_spec with
  | Select_owner_slot | Select_owner_creature | Select_owner _ -> true
  | _ -> false

let placements cs (p : player_state) (enemy : player_state) =
  if List.length cs <= !Params.placements_per_card then List.map (fun c -> Some c) cs
  else
    let ranked =
      if own_side (List.hd cs) then
        let creatures, slots = List.partition (fun c -> defined p (cmd_num c)) cs in
        if slots = [] then
          [
            max_by (fun c -> attack_of p (cmd_num c)) creatures;
            min_by
              (fun c ->
                let n = cmd_num c in
                float_of_int (life_of p n) /. float_of_int (max 1 (max_life_of p n)))
              creatures;
          ]
        else
          let blocking, free = List.partition (fun c -> defined enemy (cmd_num c)) slots in
          let best_block =
            if blocking = [] then [] else [ max_by (fun c -> attack_of enemy (cmd_num c)) blocking ]
          in
          let best_free =
            if free = [] then [] else [ min_by (fun c -> (-friends p (cmd_num c), cmd_num c)) free ]
          in
          best_block @ best_free @ slots
      else
        let targets = List.filter (fun c -> defined enemy (cmd_num c)) cs in
        if targets = [] then cs
        else
          [
            max_by (fun c -> attack_of enemy (cmd_num c)) targets;
            min_by (fun c -> life_of enemy (cmd_num c)) targets;
          ]
          @ targets
    in
    distinct_cmds ranked |> fun xs ->
    let rec take n = function [] -> [] | _ when n <= 0 -> [] | x :: xs -> Some x :: take (n - 1) xs in
    take !Params.placements_per_card xs

let base_moves ~desc pid state =
  let p = state.players.(pid) in
  if is_disabled p then [ None ]
  else
    let enemy = state.players.(other pid) in
    let all = Game.legal_commands pid desc state in
    let order =
      let seen = Hashtbl.create 16 in
      List.filter_map
        (fun (c : command) ->
          if Hashtbl.mem seen c.cmd_card.id then None
          else (
            Hashtbl.add seen c.cmd_card.id ();
            Some c.cmd_card.id))
        all
    in
    let by_card = Hashtbl.create 16 in
    List.iter
      (fun (c : command) ->
        let id = c.cmd_card.id in
        let prev = Option.value (Hashtbl.find_opt by_card id) ~default:[] in
        Hashtbl.replace by_card id (prev @ [ c ]))
      all;
    None :: List.concat_map (fun id -> placements (Hashtbl.find by_card id) p enemy) order

type stats = {
  mutable sims : int;
  mutable playouts : int;
  mutable rounds : int;
  mutable wins : int;
  mutable losses : int;
}

let make_stats () = { sims = 0; playouts = 0; rounds = 0; wins = 0; losses = 0 }

let play ~desc ~stats state pid cmd =
  let s = Game.simulate desc state pid cmd in
  stats.sims <- stats.sims + 1;
  { cmd; state = s.state; next = s.next; ended = s.ended }

let terminal ~bot_id winner ply =
  let v = max 0.5 (1.0 -. (!Params.win_discount *. float_of_int ply)) in
  if winner = bot_id then v else -.v

let rec project ~desc ~stats ~bot_id step ply =
  match step.ended with
  | Some p -> terminal ~bot_id p ply
  | None ->
      let st = ref step.state in
      let pid = ref step.next in
      let ended = ref None in
      let r = ref 0 in
      while !r < !Params.project_plies && Option.is_none !ended do
        let s = Game.simulate desc !st !pid None in
        ended := s.ended;
        stats.sims <- stats.sims + 1;
        st := s.state;
        pid := s.next;
        incr r
      done;
      Eval.score ~projected:!st ~before:step.state ~bot_id

let signed ~bot_id v pid = if pid = bot_id then v else -.v

let nth_best scored rank =
  let top = List.sort (fun (_, a) (_, b) -> compare b a) scored in
  fst (List.nth top (min rank (List.length top - 1)))

let log_line settings t0 how cmd v n stats =
  if settings.logging || settings.stats_log then
    Printf.printf
      "oracle-stats %s playouts=%d wins=%d losses=%d sims=%d rounds=%d spentMs=%d/budgetMs=%d best=%.3f n=%d \
       cmd=%s\n\
       %!"
      how stats.playouts stats.wins stats.losses stats.sims stats.rounds
      (now_ms () - t0)
      settings.duration v n (cmd_label cmd)

let execute settings ~pid ~desc start =
  let t0 = now_ms () in
  let deadline = t0 + settings.duration in
  let stats = make_stats () in
  let bot_id = pid in
  if is_disabled start.players.(bot_id) then None
  else
    let play = play ~desc ~stats in
    let value step ply = project ~desc ~stats ~bot_id step ply in
    let all = None :: List.map (fun c -> Some c) (Game.legal_commands bot_id desc start) in
    let screened : (step * float) list =
      List.map
        (fun c ->
          let s = play start bot_id c in
          (s, value s 1))
        all
    in
    match List.find_opt (fun ((s : step), _) -> s.ended = Some bot_id) screened with
    | Some (s, _) ->
        log_line settings t0 "lethal" s.cmd 1.0 0 stats;
        s.cmd
    | None ->
        let tbl : (int option, (step * float) list) Hashtbl.t = Hashtbl.create 16 in
        let keys = ref [] in
        List.iter
          (fun (s, v) ->
            let k = Option.map (fun (c : command) -> c.cmd_card.id) s.cmd in
            let row = (s, v) in
            match Hashtbl.find_opt tbl k with
            | Some xs -> Hashtbl.replace tbl k (row :: xs)
            | None ->
                Hashtbl.add tbl k [ row ];
                keys := k :: !keys)
          screened;
        let per_card : (step * float) list =
          List.rev !keys
          |> List.map (fun k -> max_by (fun (_, v) -> v) (Hashtbl.find tbl k))
          |> List.sort (fun (_, a) (_, b) -> compare b a)
        in
        let pass = List.filter (fun (s, _) -> Option.is_none s.cmd) per_card in
        let cards =
          List.filter (fun (s, _) -> Option.is_some s.cmd) per_card |> fun xs ->
          let rec take n = function [] -> [] | _ when n <= 0 -> [] | x :: xs -> x :: take (n - 1) xs in
          take !Params.root_cards xs
        in
        let mk ((s : step), v) : cand =
          {
            move = s.cmd;
            screen = v;
            sum = Array.make (Array.length !Params.reply_weights) 0.0;
            cnt = Array.make (Array.length !Params.reply_weights) 0;
            n = 0;
          }
        in
        let survivors = ref (List.map mk (cards @ pass)) in
        let rec playout root reply_rank =
          stats.playouts <- stats.playouts + 1;
          let step = ref (play start bot_id root) in
          let ply = ref 1 in
          let opponent_replied = ref false in
          while Option.is_none !step.ended && !ply <= !Params.horizon do
            let pid = !step.next in
            let state = !step.state in
            let steps = List.map (fun c -> play state pid c) (base_moves ~desc pid state) in
            let scored = List.map (fun s -> (s, signed ~bot_id (value s (!ply + 1)) pid)) steps in
            step :=
              if pid <> bot_id && not !opponent_replied then (
                opponent_replied := true;
                nth_best scored reply_rank)
              else fst (max_by snd scored);
            incr ply
          done;
          Option.iter
            (fun p -> if p = bot_id then stats.wins <- stats.wins + 1 else stats.losses <- stats.losses + 1)
            !step.ended;
          value !step !ply
        in
        let per_round = ref !Params.first_round_samples in
        let time_left = ref true in
        while !time_left && List.length !survivors > 1 do
          stats.rounds <- stats.rounds + 1;
          let i = ref 0 in
          while !time_left && !i < !per_round do
            List.iter
              (fun c ->
                if !time_left then (
                  let rank = c.n mod Array.length !Params.reply_weights in
                  add_sample c rank (playout c.move rank);
                  time_left := now_ms () < deadline))
              !survivors;
            incr i
          done;
          if !time_left then (
            let n = List.length !survivors in
            let keep = if n > 2 then (n + 1) / 2 else 2 in
            let rec take k = function [] -> [] | _ when k <= 0 -> [] | x :: xs -> x :: take (k - 1) xs in
            survivors := !survivors |> List.sort (fun a b -> compare (cand_mean b) (cand_mean a)) |> take keep;
            if List.length !survivors = 2 then per_round := 1 else per_round := !per_round * 2)
        done;
        let best = max_by cand_mean !survivors in
        log_line settings t0 "search" best.move (cand_mean best) best.n stats;
        if settings.logging || Option.is_some (Sys.getenv_opt "ORACLE_DEBUG") then (
          print_endline (state_to_string start);
          let screen_line =
            cards @ pass
            |> List.map (fun (s, v) -> Printf.sprintf "%s screen=%.3f" (cmd_label s.cmd) v)
            |> String.concat " | "
          in
          Printf.printf "  screen: %s\n%!" screen_line;
          let final_line =
            !survivors
            |> List.sort (fun a b -> compare (cand_mean b) (cand_mean a))
            |> List.map (fun c -> Printf.sprintf "%s mean=%.3f n=%d" (cmd_label c.move) (cand_mean c) c.n)
            |> String.concat " | "
          in
          Printf.printf "  final: %s\n%!" final_line);
        best.move

let leaf_value ~desc ?(bot_id = owner) state =
  let stats = make_stats () in
  project ~desc ~stats ~bot_id { cmd = None; state; next = opponent; ended = None } 1

let mover settings u pid = execute settings ~pid ~desc:u.desc u.state
