# 2. The bot's view of the game

Every bot in `lib/bots/` is the same four parts wired differently:

```text
  +------------------+   +-------------------+   +------------------+   +---------------+
  | 1. move generator |-->| 2. simulator      |-->| 3. leaf evaluator |-->| 4. controller |
  | what can I play? |   | try it, fork it   |   | how good is this? |   | which one?    |
  +------------------+   +-------------------+   +------------------+   +---------------+
        Oracle.base_moves     Game.simulate        Oracle.Eval.score      successive halving
        Game.legal_commands    Updater.create      Heuris.of_state        UCT (Lecturer)
        Game.available_commands                     Heuris.synergy        best-first (Summoner)
                                                                     UCB bandit (Strategist)
```

This document is about parts 1–3, which are shared. Document 3 covers part 4;
documents 4 and 5 cover one controller each.

## 1. The bot's view is a plain value

A bot receives `game_state` — an ordinary record of two `player_state`s. There
is no observer interface, no visibility filter. The bots read the same board
the engine keeps:

```ocaml
let execute settings ~pid ~desc start = ...      (* oracle.ml:268 *)
let mover settings u pid = execute settings ~pid ~desc:u.desc u.state   (* oracle.ml:390 *)
```

`u.state` is `game_state`, and from it a bot gets:

| field | meaning |
|---|---|
| `ps_life` | the player's life, 60 at the start |
| `ps_houses.(i)` | `house_state { mana; growth }` for house `i` |
| `ps_slots.(0..5)` | the board: `slot_state option` |
| `ps_slot_list` | which slots are usable |
| `ps_desc.desc_mods` | the live cost/effect modifiers on this player's cards |
| `ps_data` | per-house scratch space (Zen cocoons, chrono snapshots, …) |
| `ps_effects` | registered player-level phase effects |

and from a `slot_state` (all in `lib/core/sp.ml:207-218`): `card`, `life`,
`max_life`, `status` (a bitmask: `run`, `stun`, `invincible`, `blocked`,
`paused`, `cursed`), `attack_sources`, the folded `attack`, `target`,
`slot_id`, `reaction`, `data`.

`ps_houses` is one array of five — indexes 0–3 are Fire, Water, Air, Earth and
index 4 is the special house (`special_house_index`, `sp.ml:425`). So
`h_index >= 4` is the test for "this is a special-house creature", used
everywhere in the engine and in house code.

### The honest caveat

The bots see everything, including what a human player cannot see. The README
documents two cases; both are real:

- An unrevealed **Shade Stalker** (Shadowmage) is `Stealthed`. The engine
  refuses to target it and the move generators hide it
  (`lib/game/game.ml:127,232-238`), but it is still sitting in
  `ps_slots.(n)` with its attack and life, so a bot scoring that position will
  play around it.
- Strategist, Lecturer, Oracle and Summoner explore the opponent's replies with
  that side's **real** hand. Diviner is the only bot that samples guesses
  instead — which is exactly document 5.

So when a bot plays "as if" it knew something, that is usually the first
explanation. The second is the shared evaluation function.

## 2. Move generation

There are three move generators, and choosing between them is most of what
distinguishes the bots.

### `Game.legal_commands pid desc state` — everything, ordered

`lib/game/game.ml:191-203`. Pure: state in, list of commands out. It walks the
five houses, filters by `is_available`, expands each card into one command per
legal target via `input_spec` (`sp.ml:160-168`), then orders the result:

```ocaml
let order_moves ~enemy commands =
  group_by_card commands
  |> List.map (List.stable_sort (fun a b -> compare (place_rank ~enemy a) (place_rank ~enemy b)))
  |> List.sort (fun a b -> ... compare cb.cost ca.cost ... then card id ...)
  |> interleave
```

Three steps, each doing something useful:

- `group_by_card` puts every command for the same card together, in first-seen
  order.
- `place_rank` orders a creature's placements: blocking an occupied enemy slot
  first (`blocked` is the primary key for `Select_owner_slot`), and for a
  targeted spell, lowest-enemy-life first.
- `interleave` takes the groups round-robin — first placement of every card,
  then second, and so on. This is the port of Scala's `Choices.getNexts`, and
  it exists so a search meets every card before it retries slots.

Card groups are ordered cheapest-first. The `cmd_input` is the target slot
number; `None` means the card needs no input.

`input_spec` is the whole targeting vocabulary:

```ocaml
type input_spec =
  | No_input | Select_owner_slot | Select_owner_creature
  | Select_target_slot | Select_target_creature | Select_any_creature
  | Select_owner of (player_id -> game_state -> int list)   (* house computes it *)
  | Select_target of (player_id -> game_state -> int list)
```

The two functional variants let a house decide for itself — see
`Select_target non_special` in `lib/houses/special/wind.ml:137`.

### `Game.available_commands u p` — the greedy bot's view

`lib/game/game.ml:205-253`. Takes the **live** `player_update` rather than a
state, so it sees pending slot changes, and it sorts purely cheapest-first.
`Game.bot_move` takes the head of the list and `Game.greedy_mover` is that
wrapped in a `Game.mover`. Greedy is "always play the cheapest legal card, as
far forward as it can" — one line of code, and the baseline everything else is
measured against.

### `Oracle.base_moves ~desc pid state` — the pruned set

`lib/bots/oracle.ml:190-213`. This is the interesting one, because it is the
branching factor of a rollout:

```ocaml
None :: List.concat_map (fun id -> placements (Hashtbl.find by_card id) p enemy) order
```

**Pass**, plus, for each playable card, at most `placements_per_card` (default
2) targets. Which two is a small hand-written heuristic (`placements`,
`oracle.ml:152-188`):

- **Own-side targets** (`Select_owner_slot` / `Select_owner_creature`):
  block the enemy creature with the highest attack in that slot; then the
  least-damaged own creature (highest `life / max_life`); then the emptiest
  free slot (`-friends`, `friends` counting filled neighbours — stay packed).
- **Enemy targets**: the enemy creature with the highest attack, then the one
  with the lowest life.
- **Spells with no input** just take the card.

Two placements per card instead of six open slots is what makes a rollout
affordable; the heuristic biases each toward the interaction a human would
consider first. Lecturer reuses this set for both its expansion and its
rollout (`lecturer.ml:110,156`).

## 3. The simulator

Part 2 is one function:

```ocaml
val simulate : ?rng:Dice.t -> game_desc -> game_state -> player_id -> command option -> sim
type sim = { state : game_state; next : player_id; ended : player_id option }
```

It applies `cmd` for `pid` and then runs the **whole rest of the turn** —
creature attacks, `On_end_turn` effects, the run-flag toggle, the opponent's
growth, the opponent's `On_turn` effects — unless a `Wait_player` transition
hands the turn over mid-effect (the house pushed one). See §4 of document 1
for the exact chain.

Three properties matter for search:

1. **It forks.** `Updater.create` copies the state first (`engine.ml:667`), so
   the live game and any sibling simulation are untouched. Tests assert this by
   string-comparing the board after a full search.
2. **It is silent.** A fresh updater's listener is `ignore`, so simulations
   emit no events and cannot animate anything.
3. **Its dice are private.** Without `~rng` it draws from a self-seeded stream
   (`game.ml:267`), so search never perturbs a seeded game's replay.

`ended : player_id option` is `Some winner` when the game finished (the engine sets it to `other` of the player who died). Both Oracle
and Lecturer check it before anything else, and a bot that finds a move which
ends the game wins immediately — there is nothing to evaluate.

The cost is not free: every call copies both players' states and rebuilds the
update graph. Bots count them (`stats.sims`) and print the total, which is a
more honest cost meter than elapsed milliseconds.

## 4. Pass is a move

`command option` — `None` is the pass. Every bot that does not deal with `None`
explicitly has a bug: passing is often correct (saving mana for a big card,
waiting out a stun, holding a spell for the right turn).

And passing is sometimes forced: `is_disabled` (`sp.ml:464`) asks "is nothing at
all playable", and every driver short-circuits to `None` before consulting the
bot — `Table.bot_command` (`table.ml:250`), `Session.play_game`
(`session.ml:132`), and all five search bots' `execute`.

## 5. Evaluating a position

Two evaluation modules exist and they are **not** interchangeable.

### `Oracle.Eval.score` — smooth, resource-weighted

`lib/bots/oracle.ml:26-66`. Returns a value in `(-1, 1)`:

```ocaml
let score ~projected ~before ~bot_id =
  let bot = projected.players.(bot_id) in
  let opp = projected.players.(other bot_id) in
  ...
  let raw =
    life_term b_life -. life_term o_life +. board bot -. board opp
    +. (!mana_w *. (stock bot -. stock opp))
    +. (!growth_w *. (income bot -. income opp))
  in
  tanh (raw /. !scale)
```

Six terms: a **concave life curve** (`life_term` squares the life already
lost, so being behind hurts more the further behind you are), **board
attack and creature life** (with a `held_factor` discount for stunned, blocked
or paused creatures), **mana stock**, and **growth income** — the last weighted
6× by default, because in this game the engine that wins is usually the one
that out-grows the other. `tanh (raw / 60)` compresses everything to `(-1, 1)`
so playout means can be averaged.

The `projected` / `before` pair is unusual and worth understanding:
`before` is the position at the root of the rollout, `projected` is where the
leaf evaluation happens, and

```ocaml
let b_life = float_of_int bot.ps_life +. (!extrapolate *. (float_of_int bot.ps_life -. b_now)) in
```

**doubles the life delta**. The leaf is reached by simulating several *passes*
(`project_plies`, default 4), and a pass is not a neutral action: it advances
the turn chain, creatures attack, mana grows. So the raw leaf life is a
mid-transition reading, and extrapolating from the root damps the noise. With
`extrapolate = 0` the score would use the leaf's life untouched.

Oracle, Diviner and Lecturer all use it. Document 4 derives it term by term.

### `Heuris` — the race forecast

`lib/bots/heuris.ml`, used by Summoner and Strategist only. It predicts the
fight rather than counting resources:

```ocaml
let of_state ~bot_id ~to_move state =            (* heuris.ml:117 *)
  let race = score ~bot_life ~bot.life ~bot:(line bot) ~human:(line human) ~bot_to_move:(to_move = bot_id) in
  blend ~race ~bot:bot.ps_houses ~human:human.ps_houses ...
```

`score` (`heuris.ml:44-65`) runs up to `horizon = 12` alternating combat swings
with a faithful copy of the strike rules (multi-run attackers hit every defender
*and* the face; single-run attackers hit the blocker or leak through; `wake`
brings sleeping creatures online). Twelve is two turns for a full board of six,
and the loop stops early the moment someone dies. The side to move swings first,
so first-strike positions are read correctly.

#### The ratio, and why it trades for you

The score is not a difference, it is a **normalised difference**:

```text
s(b, h) = (b − h) / (b + h)          ∈ (−1, 1)
```

which is antisymmetric, scale-free, and saturated at the ends. The marginal
value of a point of life is what makes it interesting:

```text
∂s/∂b = 2h/(b+h)²        ∂s/∂h = −2b/(b+h)²        ratio = b/h
```

A point of your own life and a point of theirs are worth the *same* at parity,
and their exchange rate is exactly your life lead. So the rule "trade when ahead,
turtle when behind" is not written anywhere — it falls out of the ratio, with
the switch exactly at parity and no threshold. Worked, with a 5-for-5 trade:

| position | take 5 | deal 5 | net |
|---|---|---|---|
| ahead, `(b,h) = (40,30)`, `s = +0.143` | `(35,30)` → `0.077` = **−0.066** | `(40,25)` → `0.231` = **+0.088** | **+0.022** |
| behind, `(b,h) = (30,40)`, `s = −0.143` | `(25,40)` → `−0.231` = **−0.088** | `(30,35)` → `−0.077` = **+0.066** | **−0.022** |

The two rows are exact mirror images, and so is the answer: the same trade is
worth `+0.022` at a ten-point lead and `−0.022` at a ten-point deficit. Nothing
is clamped and nothing is tuned.

#### The blend

```ocaml
let blend ~race ~bot ~human ... () =
  race +. ((1. -. abs_float race) *. long_term ~bot ~human ...)
```

The long-term weight is a hat function of `|race|`: 1 when the race is dead
even, 0 when it is decided. The long-term term can only ever matter where the
fight is still open. One subtlety: this is an exact convex interpolation only
when `race ≥ 0`. A race score of `−0.6` with a long-term score of `+0.9` blends
to `−0.6 + 0.4·0.9 = −0.24`, still negative — the resource advantage cannot
outvote a losing fight until the fight is nearly even.

#### The long-term term, term by term

```ocaml
let raw =
  ((income bot -. income human) *. growth_turns)          (* growth_turns = 8 *)
  +. (stock_weight *. (stock bot -. stock human))        (* 0.15 *)
  +. (attack_weight *. (bot_attack -. human_attack))     (* 1.5 *)
  +. (board_life_weight *. (bot_board -. human_board))   (* 0.1 *)
in
tanh (raw /. 10.)
```

`growth_turns = 8` is the horizon over which a per-turn income difference is
assumed to persist: one extra point of growth is booked as eight extra mana.
At the origin the slope is `d/dr tanh(r/10) = 0.1`, so each raw point is worth
`0.1` of score and the terms compare like this:

| term | raw per unit | score per unit (at 0) |
|---|---|---|
| growth | 8.0 | **+0.80** |
| board attack | 1.5 | +0.15 |
| mana in hand | 0.15 | +0.015 |
| board creature life | 0.1 | +0.010 |

One point of growth is worth 5.3 points of board attack, 53 mana in hand or 80
points of creature life. Development dominates the position, exactly as
`growth_w = 6` does in `Oracle.Eval` — this is the same opinion expressed in two
different currencies. And the slope decays: at `raw = 10` the multiplier is
`0.1·(1 − tanh²1) = 0.042`, so the tenth raw point is worth 42% of the first.
It is the same saturation as `tanh(raw/60)` in Oracle, at a scale of 10.

Worked: a one-growth lead and a two-attack lead, nothing else:

```text
raw   = 8·1 + 1.5·2 = 11.0
long_term = tanh(11/10) = tanh(1.1) = 0.800
race = 0.2  ->  blend = 0.2 + (1 − 0.2)·0.800 = 0.2 + 0.64 = 0.840
```

#### The two ratios

```ocaml
let mana_ratio p  = (Σ_{i<4} mana_i/12  +  mana_4/8) / 5      (* heuris.ml:126 *)
let board_ratio p = Σ_n (life_n / max_life_n) / 6            (* heuris.ml:130 *)
```

Both divide by the game's natural maxima — 12 mana per basic house, 8 for the
special, 6 slots — so each house and each slot contributes at most 1 and the
ratios land in `[0, 1]` in an ordinary game. Neither is clamped by the code and
the engine does not clamp mana either, so a house that banks above 12 pushes
`mana_ratio` past 1. Treat both as *ratios*, not as bounded quantities, when
you combine them.

#### `life_mana` — the Summoner alternative

```ocaml
let pressure = min 1. (float_of_int (min start_life_bot start_life_opp) /. 60.) in
value = (1 − pressure)·max(0, (40 + Δlife)/100)
      + pressure·(pressure·Δmana + (1 − pressure)·Δboard)
score = max 0. ((1 + (value state − value start)) / 2)
```

`pressure` is the fraction of the *opening* life that the worse-placed player
still has, and it interpolates the whole evaluation: at `pressure = 1` (the
opening) the score is pure mana difference, at `pressure = 0` (someone is nearly
dead) it is pure life, and at `0.5` it is half of each. The final line is a
**progress** measure, not a position measure: it compares the position against
the state before the move, so `0.5` means "this move changed nothing". Note the
`40` offset in the life term — parity reads `0.4`, and a 20-point deficit reads
`0.2`, so small deficits are real but large ones are not penalised twice.

#### `synergy` — the Strategist term

```ocaml
let synergy ~weights:(heal, damage, presence, mana) ~bot_id state =
  let sustain = max heal (damage *. 0.5) in
  presence·Δboard_ratio + mana·Δmana_ratio + sustain·(Δlife/60)
```

Healing counts double if the deck actually heals, otherwise damage counts at
half — a pure damage deck gets credit for closing a life gap, but only half
credit. With the balanced weights (`heal 0.6, damage 0.7, presence 0.8, mana
0.5`, `strategist.ml:167`) a position with a 0.2 board-ratio lead, a 0.1
mana-ratio lead and ten life:

```text
sustain = max(0.6, 0.7·0.5) = 0.6
synergy = 0.8·0.2 + 0.5·0.1 + 0.6·(10/60) = 0.16 + 0.05 + 0.10 = 0.31
```

Strategist reads it as a **delta** (`synergy_of plan.weights pid state −
start_syn`, `strategist.ml:342-346`) and blends it with the race under the
tempo plan's weights — `race_w` 0.5 (value-oriented) to 0.9 (tempo-oriented),
`syn_w` 0.9 to 0.4.

`Heuris.mana_lead` is `long_term` with the attack and board terms zeroed; it is
exported and nothing calls it.

## 6. Writing a bot

The shape is fixed. Here is a complete, compilable skeleton:

```ocaml
(* lib/bots/mybot.ml *)
open Sp

type settings = { mutable logging : bool; mutable stats_log : bool; mutable duration : int }
let default_settings () = { logging = false; stats_log = false; duration = 1000 }

let now_ms () = int_of_float (Unix.gettimeofday () *. 1000.)

let execute settings ~pid ~desc start =
  let t0 = now_ms () in
  if is_disabled start.players.(pid) then None
  else
    let moves = None :: List.map (fun c -> Some c) (Game.legal_commands pid desc start) in
    let score cmd =
      let sim = Game.simulate desc start pid cmd in
      match sim.ended with
      | Some winner -> if winner = pid then 1.0 else -1.0
      | None -> Oracle.Eval.score ~projected:sim.state ~before:start ~bot_id:pid
    in
    let best, best_v =
      List.fold_left
        (fun (b, s) c -> let v = score c in if v > s then (Some c, v) else (b, s))
        (None, -.infinity) moves
    in
    if settings.stats_log then
      Printf.printf "mybot-stats spentMs=%d/budgetMs=%d best=%.3f\n%!" (now_ms () - t0) settings.duration best_v;
    best

let mover settings u pid = execute settings ~pid ~desc:u.desc u.state
```

Then register it: add `"mybot"` to `Tournament.bot_kind` and `bot_ids`
(`lib/game/tournament.ml:372,384`), give it a `case` in `make_mover`
(`:386`), add `ai.mybot=Mybot ({0})` to the five `assets/i18n/*.properties`
files, and — if it should show in the duel lobby — a row in
`Table.duel_opponents` (`lib/game/table.ml:58`).

Checklist while you build it:

- [ ] `None` is a legal answer and must be in your candidate list.
- [ ] Check `sim.ended` before trusting any score.
- [ ] Check the clock **inside** the loop, not just around it, or you will
      overrun the budget by a whole rollout.
- [ ] Do not mutate anything you were handed; `start` is the live game's
      state.
- [ ] Assume your search will be cut off at an arbitrary moment and must
      return its best-so-far.

Next: [03-search-concepts.md](03-search-concepts.md).