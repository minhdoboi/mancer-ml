(* Strategist: Summoner's search plus a tempo plan and a UCB bandit over
   nearby plans. Port of priv/sp/bot/strategist. *)

open Sp

type settings = { mutable logging : bool; mutable stats_log : bool; mutable duration : int }

let default_settings () = { logging = false; stats_log = false; duration = 1000 }

type roles = { heal : float; damage : float; presence : float; mana : float }
type weights = roles

type profile = {
  is_spell : bool;
  attack : int;
  mana_attack : bool;
  life : int;
  cost : int;
  hasted : bool;
  multi : bool;
  invincible : bool;
  altar : bool;
}

type focus = Short_game | Long_game | Balanced_focus
type archetype = Tempo | Value | Balanced_arch

type plan = {
  tempo : float;
  confidence : float;
  gamma : float;
  race_w : float;
  synergy_w : float;
  weights : weights;
  max_depth : int;
  branch_top_k : int;
  focus : focus;
}

let contains t sub =
  let n = String.length t and m = String.length sub in
  let rec go i =
    if i + m > n then false else if String.sub t i m = sub then true else go (i + 1)
  in
  if m = 0 then true else go 0

let hits t words = List.fold_left (fun n w -> if contains t w then n + 1 else n) 0 words

let numbers t =
  let n = String.length t in
  let rec go i acc cur =
    if i >= n then (match cur with Some x -> List.rev (x :: acc) | None -> List.rev acc)
    else
      match t.[i] with
      | '0' .. '9' as c ->
          let d = Char.code c - Char.code '0' in
          go (i + 1) acc (Some (match cur with None -> d | Some x -> (x * 10) + d))
      | _ -> go (i + 1) (match cur with Some x -> x :: acc | None -> acc) None
  in
  go 0 [] None

let number_bonus t words =
  if hits t words = 0 then 0.
  else min 0.6 (float_of_int (List.fold_left max 0 (numbers t)) /. 30.)

let capped x maxv = min maxv (max 0. x)

let score_creature (p : profile) t =
  let heal =
    capped (float_of_int (hits t [ "heal" ]) *. 0.6) 1.5
    +. (if contains t "heal" && contains t "every turn" then 0.4 else 0.)
    +. number_bonus t [ "heal" ]
  in
  let damage =
    capped (float_of_int (hits t [ "damage"; "deals" ]) *. 0.35) 1.2
    +. (if contains t "destroy" then 0.8 else 0.)
    +. capped (float_of_int (hits t [ "stun"; "block" ]) *. 0.25) 0.5
    +. number_bonus t [ "damage"; "deals" ]
    +. capped (float_of_int p.attack /. float_of_int (p.cost + 2)) 1.
    +. (if p.multi then 0.4 else 0.)
    +. if p.hasted then 0.2 else 0.
  in
  let presence =
    capped (float_of_int (p.attack + p.life) /. float_of_int ((5 * p.cost) + 5)) 1.2
    +. capped (float_of_int (hits t [ "summon"; "spawn"; "reborn"; "rebirth" ]) *. 0.5) 1.
    +. (if contains t "immune" || contains t "invulnerab" || contains t "absorb" || contains t "redirect"
        then 0.3
        else 0.)
    +. (if p.invincible then 0.3 else 0.)
    +. if p.altar then 0.4 else 0.
  in
  let mana =
    capped (float_of_int (hits t [ "mana" ]) *. 0.5) 1.2
    +. (if contains t "growth" then 0.6 else 0.)
    +. (if contains t "cost" then 0.3 else 0.)
    +. if p.mana_attack then 0.5 else 0.
  in
  { heal; damage; presence; mana }

let score_spell t =
  let heal = capped (float_of_int (hits t [ "heal" ]) *. 0.7) 1.5 +. number_bonus t [ "heal" ] in
  let damage =
    capped (float_of_int (hits t [ "damage"; "deals" ]) *. 0.4) 1.4
    +. (if contains t "destroy" then 1. else 0.)
    +. capped (float_of_int (hits t [ "stun"; "block" ]) *. 0.3) 0.6
    +. number_bonus t [ "damage"; "deals" ]
  in
  let presence = capped (float_of_int (hits t [ "summon"; "spawn"; "reborn"; "rebirth" ]) *. 0.7) 1.4 in
  let mana =
    capped (float_of_int (hits t [ "mana" ]) *. 0.6) 1.4
    +. if contains t "growth" || contains t "cost" then 0.5 else 0.
  in
  { heal; damage; presence; mana }

let score_profile p text =
  let t = String.lowercase_ascii text in
  if p.is_spell then score_spell t else score_creature p t

let profile_of (c : card) =
  match c.creature_body with
  | None ->
      {
        is_spell = true;
        attack = 0;
        mana_attack = false;
        life = 0;
        cost = max 1 c.cost;
        hasted = false;
        multi = false;
        invincible = false;
        altar = false;
      }
  | Some b ->
      {
        is_spell = false;
        attack = (match b.b_attack.base with Some a -> a | None -> 0);
        mana_attack = Option.is_none b.b_attack.base;
        life = b.b_life;
        cost = max 1 c.cost;
        hasted = b.b_status land run_flag <> 0;
        multi = Common.is_multi_run b.b_run_attack;
        invincible = b.b_status land invincible_flag <> 0;
        altar = b.b_is_altar;
      }

let score_card c text = score_profile (profile_of c) text
let weighted (r : roles) (w : weights) = (r.heal *. w.heal) +. (r.damage *. w.damage) +. (r.presence *. w.presence) +. (r.mana *. w.mana)

module Params = struct
  let cost_tie_break = 0.05
  let gamma_value = 0.97
  let gamma_tempo = 0.70
  let race_w_value = 0.5
  let race_w_tempo = 0.9
  let syn_w_value = 0.9
  let syn_w_tempo = 0.4
  let depth_value = 3
  let depth_tempo = 2
  let width_value = 3.
  let width_tempo = 8.
  let tempo_high = 0.66
  let tempo_low = 0.33
  let bandit_arms = 3
  let bandit_delta = 0.25
  let bandit_alpha = 0.7
  let bandit_c = 1.4
  let probe_top_k = 5
  let balanced_weights = { heal = 0.6; damage = 0.7; presence = 0.8; mana = 0.5 }
  let value_weights = { heal = 0.5; damage = 0.5; presence = 1.0; mana = 0.8 }
  let tempo_weights = { heal = 0.7; damage = 1.0; presence = 0.6; mana = 0.3 }
  let tempo_threshold = 0.15
  let value_threshold = -0.15
end

let clamp01 x = max 0. (min 1. x)

let for_tempo_plan t confidence =
  let tc = clamp01 t and c = clamp01 confidence in
  let mix value balanced tempo =
    let x = value +. ((tempo -. value) *. tc) in
    balanced +. ((x -. balanced) *. c)
  in
  let mix_w v b e =
    {
      heal = mix v.heal b.heal e.heal;
      damage = mix v.damage b.damage e.damage;
      presence = mix v.presence b.presence e.presence;
      mana = mix v.mana b.mana e.mana;
    }
  in
  let focus =
    if tc > Params.tempo_high then Short_game else if tc < Params.tempo_low then Long_game else Balanced_focus
  in
  {
    tempo = tc;
    confidence = c;
    gamma = mix Params.gamma_value ((Params.gamma_value +. Params.gamma_tempo) /. 2.) Params.gamma_tempo;
    race_w = mix Params.race_w_value ((Params.race_w_value +. Params.race_w_tempo) /. 2.) Params.race_w_tempo;
    synergy_w = mix Params.syn_w_value ((Params.syn_w_value +. Params.syn_w_tempo) /. 2.) Params.syn_w_tempo;
    weights = mix_w Params.value_weights Params.balanced_weights Params.tempo_weights;
    max_depth = (if tc < Params.tempo_low then Params.depth_value else Params.depth_tempo);
    branch_top_k =
      int_of_float
        (Float.round (mix Params.width_value ((Params.width_value +. Params.width_tempo) /. 2.) Params.width_tempo));
    focus;
  }

let for_tempo t c =
  let p = for_tempo_plan t c in
  (p.gamma, p.max_depth, p.branch_top_k)

let arms t confidence =
  List.map
    (fun x -> for_tempo_plan (clamp01 x) confidence)
    [ t -. Params.bandit_delta; t; t +. Params.bandit_delta ]

let tempo_of r = (r.damage *. 0.6) -. (r.heal *. 0.4) -. (r.mana *. 0.5) -. (r.presence *. 0.2)

let read_tempo ~house ~known =
  let avg rs = if rs = [] then 0. else List.fold_left (fun a r -> a +. tempo_of r) 0. rs /. float_of_int (List.length rs) in
  let house_score = avg house and known_score = avg known in
  let known_weight = min 1. (float_of_int (List.length known) /. 4.) in
  let blended = (house_score *. (1. -. (0.5 *. known_weight))) +. (known_score *. (0.5 *. known_weight)) in
  let tempo =
    max 0. (min 1. ((blended -. Params.value_threshold) /. (Params.tempo_threshold -. Params.value_threshold)))
  in
  let confidence = min 1. (0.55 +. (0.1 *. float_of_int (List.length known))) in
  (tempo, confidence)

type bandit = { n : int; counts : int array; values : float array; mutable total : int }

let bandit_make () =
  {
    n = Params.bandit_arms;
    counts = Array.make Params.bandit_arms 0;
    values = Array.make Params.bandit_arms 0.;
    total = 0;
  }

let bandit_reset b =
  Array.fill b.counts 0 b.n 0;
  Array.fill b.values 0 b.n 0.;
  b.total <- 0

let bandit_update b arm reward =
  b.counts.(arm) <- b.counts.(arm) + 1;
  b.total <- b.total + 1;
  b.values.(arm) <- b.values.(arm) +. ((reward -. b.values.(arm)) /. float_of_int b.counts.(arm))

let bandit_bonus b arm =
  Params.bandit_c *. sqrt (log (float_of_int (b.total + b.n)) /. float_of_int (b.counts.(arm) + 1))

let now_ms () = int_of_float (Unix.gettimeofday () *. 1000.)
let synergy_of w bot_id state = Heuris.synergy ~weights:(w.heal, w.damage, w.presence, w.mana) ~bot_id state

let text_of c = try Assets.describe c with _ -> c.name

let order_strategic moves plan role_of =
  let score (c : command) = weighted (role_of c.cmd_card) plan.weights +. (Params.cost_tie_break *. float_of_int c.cmd_card.cost) in
  let tbl = Hashtbl.create 16 in
  let cards = ref [] in
  List.iter
    (fun (c : command) ->
      match Hashtbl.find_opt tbl c.cmd_card.id with
      | Some xs -> Hashtbl.replace tbl c.cmd_card.id (c :: xs)
      | None ->
          Hashtbl.add tbl c.cmd_card.id [ c ];
          cards := c.cmd_card :: !cards)
    moves;
  let groups =
    List.rev !cards
    |> List.map (fun card -> List.rev (Hashtbl.find tbl card.id))
    |> List.sort (fun a b ->
        let ca = List.hd a and cb = List.hd b in
        let c = compare (score cb) (score ca) in
        if c <> 0 then c
        else
          let d = compare cb.cmd_card.cost ca.cmd_card.cost in
          if d <> 0 then d else compare ca.cmd_card.id cb.cmd_card.id)
  in
  let rec round acc gs =
    match List.filter (fun g -> g <> []) gs with
    | [] -> acc
    | keep -> round (acc @ List.map List.hd keep) (List.map List.tl keep)
  in
  round [] groups

type node = {
  mutable pending : command option list;
  mutable expanded : int;
  mutable survey : int;
  mutable children : node list;
  mutable value : float;
  mutable scored : bool;
  mutable num_sim : float;
  from : game_state;
  player_id : player_id;
  ended : player_id option;
  command : command option;
  depth : int;
}

let rec pull ~bot_id n =
  match n.children with
  | [] -> ()
  | kids ->
      let maximizing = n.player_id = bot_id in
      let rec go v sims = function
        | [] -> (v, sims)
        | c :: rest ->
            let better = if maximizing then c.value > v else c.value < v in
            go (if better then c.value else v) (sims +. c.num_sim) rest
      in
      let hd = List.hd kids in
      let v, sims = go hd.value hd.num_sim (List.tl kids) in
      n.scored <- true;
      n.value <- v;
      n.num_sim <- sims

let score_leaf ~bot_id ~plan ~start_syn ~heuris n parents =
  let raw =
    match n.ended with
    | Some p -> if p = bot_id then 1. else -1.
    | None -> heuris n.from n.player_id
  in
  let reward = raw *. (plan.gamma ** float_of_int n.depth) in
  n.scored <- true;
  n.value <- reward;
  n.num_sim <- 1.;
  List.iter (pull ~bot_id) parents;
  ignore start_syn

let fill ~desc ~plan ~role_of n =
  if Option.is_none n.ended then (
    let moves = Game.legal_commands n.player_id desc n.from in
    let ordered = order_strategic moves plan role_of in
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
    let first, rest = take n_cards ordered in
    n.pending <- List.map (fun c -> Some c) first @ (None :: List.map (fun c -> Some c) rest);
    n.survey <- min (n_cards + 1) (plan.branch_top_k + 1))

let rec max_by f = function [] -> invalid_arg "empty" | x :: xs -> List.fold_left (fun a b -> if f b > f a then b else a) x xs

let needs n = n.pending <> [] && Option.is_none n.ended && n.value > -1.

let search settings ~pid ~desc start plan role_of =
  let bot_id = pid in
  let start_syn = synergy_of plan.weights bot_id start in
  let heuris state to_move =
    let race = Heuris.of_state ~bot_id ~to_move state in
    let syn = synergy_of plan.weights bot_id state -. start_syn in
    max (-1.) (min 1. ((plan.race_w *. race) +. (plan.synergy_w *. syn)))
  in
  let make state pid ended cmd depth =
    let n =
      {
        pending = [];
        expanded = 0;
        survey = 0;
        children = [];
        value = 0.;
        scored = false;
        num_sim = 0.;
        from = state;
        player_id = pid;
        ended;
        command = cmd;
        depth;
      }
    in
    if depth < plan.max_depth then fill ~desc ~plan ~role_of n;
    n
  in
  let expand parent =
    match parent.pending with
    | [] -> invalid_arg "expand"
    | cmd :: rest ->
        parent.pending <- rest;
        parent.expanded <- parent.expanded + 1;
        let s = Game.simulate desc parent.from parent.player_id cmd in
        let child = make s.state s.next s.ended cmd (parent.depth + 1) in
        parent.children <- child :: parent.children;
        child
  in
  let should_open n =
    if Option.is_some n.ended || n.pending = [] then false
    else if n.player_id = bot_id && List.exists (fun c -> c.ended = Some bot_id) n.children then false
    else if n.expanded = 0 then true
    else if n.player_id <> bot_id then n.value > -1.
    else if n.expanded < n.survey then true
    else
      match n.children with
      | [] -> false
      | kids ->
          let leader = max_by (fun c -> c.value) kids in
          leader.expanded >= 2 || leader.pending = []
  in
  let contender n =
    let trees = n.children in
    if trees = [] || List.exists (fun c -> c.ended = Some bot_id) trees then None
    else
      let open_ = List.filter needs trees in
      if open_ = [] then None
      else
        let floor =
          List.filter (fun t -> not (needs t)) trees |> List.fold_left (fun m t -> max m t.value) (-1.1)
        in
        match List.filter (fun t -> t.value >= floor) open_ with [] -> None | live -> Some (max_by (fun t -> t.value) live)
  in
  let rec focus n parents =
    if Option.is_some n.ended || n.depth >= plan.max_depth then if n.scored then None else Some (n, parents)
    else if n.player_id = bot_id && List.exists (fun c -> c.ended = Some bot_id) n.children then None
    else if should_open n then focus (expand n) (n :: parents)
    else if n.player_id <> bot_id then None
    else match contender n with Some child -> focus child (n :: parents) | None -> None
  in
  let root = make start bot_id None None 0 in
  let deadline = now_ms () + settings.duration in
  let rec loop () =
    if now_ms () >= deadline then ()
    else
      match focus root [] with
      | Some (n, parents) when not n.scored ->
          score_leaf ~bot_id ~plan ~start_syn ~heuris n parents;
          loop ()
      | _ -> ()
  in
  loop ();
  match root.children with
  | [] -> None
  | kids -> (max_by (fun c -> c.value) kids).command

let probe ~desc ~pid start plan role_of =
  let moves = order_strategic (Game.legal_commands pid desc start) plan role_of in
  let rec take k = function [] -> [] | _ when k <= 0 -> [] | x :: xs -> x :: take (k - 1) xs in
  let top = take Params.probe_top_k moves in
  if top = [] then -1.
  else
    let start_syn = synergy_of plan.weights pid start in
    List.fold_left
      (fun best cmd ->
        let s = Game.simulate desc start pid (Some cmd) in
        let race = Heuris.of_state ~bot_id:pid ~to_move:s.next s.state in
        let syn = synergy_of plan.weights pid s.state -. start_syn in
        let v = ((plan.race_w *. race) +. (plan.synergy_w *. syn)) *. plan.gamma in
        max best v)
      neg_infinity top

let is_fresh st = Array.for_all (fun p -> p.ps_life = 60 && Array.for_all Option.is_none p.ps_slots) st.players

let baseline ~bot_id state =
  (0.7 *. Heuris.of_state ~bot_id ~to_move:bot_id state)
  +. (0.3 *. synergy_of Params.balanced_weights bot_id state)

let execute_with settings ~bandit ~pending ~pid ~desc start =
  if is_disabled start.players.(pid) then None
  else (
    if is_fresh start then (
      bandit_reset bandit;
      pending := None);
    let base = baseline ~bot_id:pid start in
    (match !pending with Some (arm, prev) -> bandit_update bandit arm (base -. prev) | None -> ());
    let special = start.players.(other pid).ps_desc.init_desc.pd_houses.(4).phd_house in
    let house_roles =
      (special.h_cards @ special.h_additional_cards) |> List.map (fun c -> score_card c (text_of c))
    in
    let tempo, confidence = read_tempo ~house:house_roles ~known:[] in
    let role_of c = score_card c (text_of c) in
    let arm_plans = arms tempo confidence in
    let scored =
      List.mapi
        (fun a p ->
          let probe_v = probe ~desc ~pid start p role_of in
          let score =
            (Params.bandit_alpha *. probe_v) +. ((1. -. Params.bandit_alpha) *. bandit.values.(a)) +. bandit_bonus bandit a
          in
          (p, a, score))
        arm_plans
    in
    let win_plan, win_arm, _ = List.fold_left (fun ((_, _, s) as acc) ((_, _, s2) as x) -> if s2 > s then x else acc) (List.hd scored) (List.tl scored) in
    pending := Some (win_arm, base);
    if settings.logging then
      Printf.printf "strategist tempo=%.2f conf=%.2f arm=%d gamma=%.2f depth=%d width=%d\n%!" tempo confidence
        win_arm win_plan.gamma win_plan.max_depth win_plan.branch_top_k;
    search settings ~pid ~desc start win_plan role_of)

let execute settings ~pid ~desc start =
  execute_with settings ~bandit:(bandit_make ()) ~pending:(ref None) ~pid ~desc start

let mover settings =
  let bandit = bandit_make () and pending = ref None in
  fun u pid -> execute_with settings ~bandit ~pending ~pid ~desc:u.desc u.state
