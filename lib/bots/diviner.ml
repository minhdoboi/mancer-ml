(* Diviner: Oracle's playouts inside several sampled opponent decks.
   Port of priv/sp/bot/diviner/DivinerBot.scala.

   Scala draws beliefs with Choco CardGuess. Here each belief is a legal
   four-cards-per-house deal from {!Shuffle}, excluding the bot's cards and
   containing every card seen so far (see {!Belief}). The real deck is not a
   world, as in Scala. *)

open Sp

type settings = { mutable logging : bool; mutable stats_log : bool; mutable duration : int }

let default_settings () = { logging = false; stats_log = false; duration = 1000 }

module Params = struct
  let belief_count = ref 3
  let risk_aversion = ref 0.35
  let first_round_samples = ref 1
end

let apply_overrides spec =
  let set k v =
    match k with
    | "beliefCount" -> Params.belief_count := int_of_string v
    | "riskAversion" -> Params.risk_aversion := float_of_string v
    | "firstRoundSamples" -> Params.first_round_samples := int_of_string v
    | other -> Printf.printf "diviner: unknown option %s\n%!" other
  in
  spec |> String.split_on_char ','
  |> List.iter (fun kv ->
      let kv = String.trim kv in
      match String.index_opt kv '=' with
      | None -> ()
      | Some i -> set (String.sub kv 0 i) (String.sub kv (i + 1) (String.length kv - i - 1)));
  Printf.printf "diviner: options %s\n%!" spec

let () = Option.iter apply_overrides (Sys.getenv_opt "DIVINER_OPTS")
let now_ms () = int_of_float (Unix.gettimeofday () *. 1000.)
let cmd_label = function None -> "pass" | Some c -> command_to_string c

module Eval = struct
  let world_mean ~screen ~sum ~n =
    if n <= 0 then screen
    else
      ((!Oracle.Params.screen_weight *. screen) +. (sum /. float_of_int n))
      /. (!Oracle.Params.screen_weight +. 1.)

  let score ?(aversion = !Params.risk_aversion) means =
    match means with
    | [] -> neg_infinity
    | xs ->
        let n = float_of_int (List.length xs) in
        let mean = List.fold_left ( +. ) 0. xs /. n in
        mean -. (aversion *. (mean -. List.fold_left min infinity xs))
end

type world = { start : game_state; desc : game_desc; ai : Oracle.Search.t }

type cand = {
  cmd : command option;
  screen : float array;
  screened : bool array;
  sum : float array;
  cnt : int array;
}

let add c w v =
  c.sum.(w) <- c.sum.(w) +. v;
  c.cnt.(w) <- c.cnt.(w) + 1

let world_mean_of c w =
  if c.cnt.(w) <= 0 then c.screen.(w)
  else if not c.screened.(w) then c.sum.(w) /. float_of_int c.cnt.(w)
  else Eval.world_mean ~screen:c.screen.(w) ~sum:c.sum.(w) ~n:c.cnt.(w)

let cand_score c =
  let means =
    List.init (Array.length c.screen) (fun w -> w)
    |> List.filter (fun w -> c.screened.(w) || c.cnt.(w) > 0)
    |> List.map (fun w -> world_mean_of c w)
  in
  Eval.score means

let max_by = Util.max_by
let rec take n = function [] -> [] | _ when n <= 0 -> [] | x :: xs -> x :: take (n - 1) xs
let card_key = function None -> None | Some (c : command) -> Some c.cmd_card.id

let search settings ~pid worlds ~deadline =
  let bot_id = pid in
  if worlds = [] || is_disabled (List.hd worlds).start.players.(bot_id) then None
  else
    let primary = List.hd worlds in
    let nw = List.length worlds in
    let moves = None :: List.map (fun c -> Some c) (Game.legal_commands bot_id primary.desc primary.start) in
    let rows = ref [] in
    let rec screen = function
      | [] -> ()
      | _ when !rows <> [] && now_ms () >= deadline -> ()
      | cmd :: rest ->
          let values = Array.make nw 0. in
          let seen = Array.make nw false in
          let all_won = ref true in
          List.iteri
            (fun w world ->
              if now_ms () < deadline then (
                let step = Oracle.Search.play world.ai world.start bot_id cmd in
                if step.ended <> Some bot_id then all_won := false;
                values.(w) <- Oracle.Search.value world.ai step ~ply:1;
                seen.(w) <- true))
            worlds;
          if !all_won && Array.for_all Fun.id seen then rows := [ `Lethal cmd ]
          else (
            if Array.exists Fun.id seen then
              rows :=
                `Cand { cmd; screen = values; screened = seen; sum = Array.make nw 0.; cnt = Array.make nw 0 }
                :: !rows;
            screen rest)
    in
    screen moves;
    match !rows with
    | [ `Lethal cmd ] -> cmd
    | _ -> (
        let cands = List.filter_map (function `Cand c -> Some c | `Lethal _ -> None) (List.rev !rows) in
        if cands = [] then None
        else
          let per_card =
            let tbl = Hashtbl.create 16 in
            let keys = ref [] in
            List.iter
              (fun c ->
                let k = card_key c.cmd in
                match Hashtbl.find_opt tbl k with
                | Some xs -> Hashtbl.replace tbl k (c :: xs)
                | None ->
                    Hashtbl.add tbl k [ c ];
                    keys := k :: !keys)
              cands;
            List.rev !keys
            |> List.map (fun k -> max_by cand_score (Hashtbl.find tbl k))
            |> List.sort (fun a b -> compare (cand_score b) (cand_score a))
          in
          let pass = List.filter (fun c -> Option.is_none c.cmd) per_card in
          let cards =
            List.filter (fun c -> Option.is_some c.cmd) per_card |> take !Oracle.Params.root_cards
          in
          let survivors = ref (cards @ pass) in
          let per_round = ref (max 1 !Params.first_round_samples) in
          let time_left = ref (now_ms () < deadline) in
          let rounds = ref 0 in
          while !time_left && List.length !survivors > 1 do
            incr rounds;
            let i = ref 0 in
            while !time_left && !i < !per_round do
              List.iteri
                (fun w world ->
                  List.iter
                    (fun c ->
                      if !time_left then (
                        let rank = c.cnt.(w) mod Array.length !Oracle.Params.reply_weights in
                        add c w
                          (Oracle.Search.playout world.ai ~start:world.start ~root:c.cmd ~reply_rank:rank);
                        time_left := now_ms () < deadline))
                    !survivors)
                worlds;
              incr i
            done;
            if !time_left then (
              let n = List.length !survivors in
              let keep = if n > 2 then (n + 1) / 2 else 2 in
              survivors :=
                List.sort (fun a b -> compare (cand_score b) (cand_score a)) !survivors |> take keep;
              if List.length !survivors = 2 then per_round := 1 else per_round := !per_round * 2)
          done;
          match !survivors with
          | [] -> None
          | xs ->
              let best = max_by cand_score xs in
              if settings.logging || settings.stats_log then
                Printf.printf "diviner-stats beliefs=%d spentMs leftover cmd=%s score=%.3f\n%!" nw
                  (cmd_label best.cmd) (cand_score best);
              best.cmd)

(* Sampled decks, kept while the seen cards and the game stay the same (Scala
   [cachedBeliefs]), so the bot's picture of the opponent does not jump
   between moves. *)
let cached : (game_desc * player_id * string * player_desc list) list ref = ref []

let beliefs ~desc ~pid ~seen rng =
  let key = Belief.seen_key seen in
  match List.find_opt (fun (d, p, k, _) -> d == desc && p = pid && k = key) !cached with
  | Some (_, _, _, b) -> b
  | None ->
      let b =
        match Belief.sample ~desc ~bot_id:pid ~seen ~n:!Params.belief_count rng with
        | [] -> [ desc.gd_players.(other pid) ] (* no legal deck at all: nothing better to guess *)
        | l -> l
      in
      cached := (desc, pid, key, b) :: List.filter (fun (d, p, _, _) -> d == desc && p <> pid) !cached;
      b

let execute ?(seen = []) settings ~pid ~desc start =
  if is_disabled start.players.(pid) then None
  else
    let t0 = now_ms () in
    let deadline = t0 + settings.duration in
    let rng = Random.State.make_self_init () in
    let beliefs = beliefs ~desc ~pid ~seen rng in
    let worlds =
      List.map
        (fun belief ->
          let st, d = Belief.with_belief start desc pid belief in
          { start = st; desc = d; ai = Oracle.Search.create ~desc:d ~bot_id:pid })
        beliefs
    in
    search settings ~pid worlds ~deadline

let mover settings (u : updater) pid = execute ~seen:(Belief.seen_cards u ~pid) settings ~pid ~desc:u.desc u.state
