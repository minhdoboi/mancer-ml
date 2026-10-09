(* Race and life/mana leaf scores. Port of priv/sp/bot/Heuristic.scala. *)

open Sp
open Engine

type fighter = {
  mutable life : int;
  attack : int;
  mutable active : bool;
  multi : bool;
  invincible : bool;
  wakes : bool;
}

let horizon = 12

let harm d amount =
  match d with Some f when f.life > 0 && not f.invincible -> f.life <- f.life - amount | _ -> ()

let strike attackers defenders life0 =
  let life = ref life0 in
  for i = 0 to nb_slots - 1 do
    if !life > 0 then
      match attackers.(i) with
      | Some a when a.active && a.life > 0 && a.attack > 0 -> (
          if a.multi then (
            life := !life - a.attack;
            for j = 0 to nb_slots - 1 do
              harm defenders.(j) a.attack
            done)
          else
            match defenders.(i) with
            | Some d when d.life > 0 -> harm (Some d) a.attack
            | _ -> life := !life - a.attack)
      | _ -> ()
  done;
  !life

let wake attackers =
  for i = 0 to nb_slots - 1 do
    match attackers.(i) with Some a when (not a.active) && a.wakes -> a.active <- true | _ -> ()
  done

let score ~bot_life ~human_life ~bot ~human ~bot_to_move =
  if bot_life <= 0 && human_life <= 0 then 0.
  else if human_life <= 0 then 1.
  else if bot_life <= 0 then -1.
  else
    let b = ref bot_life and h = ref human_life in
    let bot_turn = ref bot_to_move in
    let step = ref 0 in
    while !step < horizon && !b > 0 && !h > 0 do
      if !bot_turn then (
        h := strike bot human !h;
        wake bot)
      else (
        b := strike human bot !b;
        wake human);
      bot_turn := not !bot_turn;
      incr step
    done;
    if !h <= 0 && !b <= 0 then 0.
    else if !h <= 0 then 1.
    else if !b <= 0 then -1.
    else float_of_int (!b - !h) /. float_of_int (!b + !h)

let income houses = Array.fold_left (fun a h -> a +. float_of_int h.growth) 0. houses
let stock houses = Array.fold_left (fun a h -> a +. float_of_int h.mana) 0. houses
let growth_turns = 8.
let stock_weight = 0.15
let attack_weight = 1.5
let board_life_weight = 0.1

let long_term ~bot ~human ~bot_attack ~human_attack ~bot_board ~human_board =
  let raw =
    ((income bot -. income human) *. growth_turns)
    +. (stock_weight *. (stock bot -. stock human))
    +. (attack_weight *. (bot_attack -. human_attack))
    +. (board_life_weight *. (bot_board -. human_board))
  in
  tanh (raw /. 10.)

let mana_lead ~bot ~human =
  long_term ~bot ~human ~bot_attack:0. ~human_attack:0. ~bot_board:0. ~human_board:0.

let blend ~race ~bot ~human ?(bot_attack = 0.) ?(human_attack = 0.) ?(bot_board = 0.) ?(human_board = 0.) () =
  race +. ((1. -. abs_float race) *. long_term ~bot ~human ~bot_attack ~human_attack ~bot_board ~human_board)

let line (p : player_state) =
  Array.init nb_slots (fun i ->
      match p.ps_slots.(i) with
      | None -> None
      | Some (s : slot_state) ->
          let held = has_flag s blocked_flag || has_flag s paused_flag in
          let runnable = Slot.is_runnable s in
          let multi = Common.is_multi_run s.card.c_body.b_run_attack in
          Some
            {
              life = s.life;
              attack = s.attack;
              active = runnable;
              multi;
              invincible = has_flag s invincible_flag;
              wakes = (not runnable) && not held;
            })

let attack_of (p : player_state) =
  Array.fold_left
    (fun a -> function Some (s : slot_state) -> a +. float_of_int s.attack | None -> a)
    0. p.ps_slots

let board_life (p : player_state) =
  Array.fold_left
    (fun a -> function Some (s : slot_state) -> a +. float_of_int s.life | None -> a)
    0. p.ps_slots

let of_state ~bot_id ~to_move state =
  let bot = state.players.(bot_id) and human = state.players.(other bot_id) in
  let race =
    score ~bot_life:bot.ps_life ~human_life:human.ps_life ~bot:(line bot) ~human:(line human)
      ~bot_to_move:(to_move = bot_id)
  in
  blend ~race ~bot:bot.ps_houses ~human:human.ps_houses ~bot_attack:(attack_of bot)
    ~human_attack:(attack_of human) ~bot_board:(board_life bot) ~human_board:(board_life human) ()

let mana_ratio (p : player_state) =
  let basics = Array.fold_left ( +. ) 0. (Array.init 4 (fun i -> float_of_int p.ps_houses.(i).mana /. 12.)) in
  (basics +. (float_of_int p.ps_houses.(4).mana /. 8.)) /. 5.

let board_ratio (p : player_state) =
  Array.fold_left
    (fun a -> function
      | None -> a
      | Some (s : slot_state) -> a +. (float_of_int s.life /. float_of_int (max 1 s.card.c_body.b_life)))
    0. p.ps_slots
  /. 6.

let life_mana ~bot_id ~start state =
  let value st =
    let bot = st.players.(bot_id) and human = st.players.(other bot_id) in
    let life_delta = float_of_int (bot.ps_life - human.ps_life) in
    let mana_delta = mana_ratio bot -. mana_ratio human in
    let board_delta = board_ratio bot -. board_ratio human in
    let pressure =
      min 1. (float_of_int (min start.players.(bot_id).ps_life start.players.(other bot_id).ps_life) /. 60.)
    in
    ((1. -. pressure) *. (max 0. (40. +. life_delta) /. 100.))
    +. (pressure *. ((pressure *. mana_delta) +. ((1. -. pressure) *. board_delta)))
  in
  max 0. ((1. +. (value state -. value start)) /. 2.)

let synergy ~weights:(heal, damage, presence, mana) ~bot_id state =
  let bot = state.players.(bot_id) and human = state.players.(other bot_id) in
  let sustain = max heal (damage *. 0.5) in
  (presence *. (board_ratio bot -. board_ratio human))
  +. (mana *. (mana_ratio bot -. mana_ratio human))
  +. (sustain *. (float_of_int (bot.ps_life - human.ps_life) /. 60.))
