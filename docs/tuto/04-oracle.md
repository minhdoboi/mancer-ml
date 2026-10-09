# 4. The Oracle bot

`lib/bots/oracle.ml`, 423 lines. Port of `priv/sp/bot/oracle/OracleBot.scala`.

> **The idea in one sentence:** do not build a tree — score every root move by
> playing it and then letting a cheap greedy player play *both* sides for five
> plies through the real engine, and pick the winner by **successive halving**
> under a wall clock.

That is the whole design. Everything below is detail.

## 1. The pipeline

```text
  execute settings ~pid ~desc start            oracle.ml:268
    |
    |-- is_disabled? -> None (pass)                                :273
    |
    |-- SCREEN   play every legal command once, project, score
    |              -> [(step, screen_value)]                       :277-284
    |
    |-- LETHAL?  any command whose step.ended = Some bot_id
    |              -> return it, zero rollouts spent               :285-288
    |
    |-- SHORTLIST best screen value per card, top root_cards (8),
    |              plus pass                                        :290-312
    |
    |-- ROLL     successive halving until the clock dies           :345-366
    |              each sample = one playout                       :323-344
    |
    `-- max_by cand_mean survivors -> the command
```

`None` (pass) is a first-class candidate at every stage. It gets its own
screen entry and its own group in the shortlist.

### Stage 1: screening

```ocaml
let all = None :: List.map (fun c -> Some c) (Game.legal_commands bot_id desc start) in
let screened : (step * float) list =
  List.map (fun c -> let s = play start bot_id c in (s, value s 1)) all
```

Every legal command is played once in a fork, then `value` pushes the result
four passes forward and scores it. This costs `(|legal| + 1) * 5` simulations —
the whole opening move above took 15 sims in one run — and it is what makes
everything after it affordable.

### Stage 2: the lethal shortcut

```ocaml
match List.find_opt (fun ((s : step), _) -> s.ended = Some bot_id) screened with
| Some (s, _) -> log_line settings t0 "lethal" s.cmd 1.0 0 stats; s.cmd
```

If any move ends the game immediately, take it and spend nothing else. Note
this can report `cmd=pass`: a real run ends with

```text
oracle-stats lethal playouts=0 wins=0 losses=0 sims=15 rounds=0 spentMs=0/budgetMs=60 best=1.000 n=0 cmd=pass
```

because the *end-of-turn chain* triggered by passing (a death-rattle, a house
effect) killed the opponent. `Game.simulate` runs the whole chain, so the bot
sees wins that are not the direct result of the card.

### Stage 3: the shortlist

```ocaml
let per_card =
  List.rev !keys
  |> List.map (fun k -> max_by (fun (_, v) -> v) (Hashtbl.find tbl k))
  |> List.sort (fun (_, a), (_, b) -> compare b a)
```

Candidates are grouped by **card id**, and only the single best-scoring
*placement* of each card survives. Since `Game.legal_commands` returns a
separate command per target slot, Oracle is really choosing both *which card*
and *where to put it* from the screen alone. That is the single biggest
heuristic shortcut in the bot, and it is why a move whose best target looks
mediocre but whose other targets look great never gets explored.

The top `root_cards` (8) plus the pass become `survivors`.

### Stage 4: successive halving

```ocaml
let per_round = ref !Params.first_round_samples in
while !time_left && List.length !survivors > 1 do
  while !time_left && !i < !per_round do
    List.iter (fun c ->
      if !time_left then (
        let rank = c.n mod Array.length !Params.reply_weights in
        add_sample c rank (playout c.move rank);
        time_left := now_ms () < deadline)) !survivors;
    incr i done;
  if !time_left then (
    let n = List.length !survivors in
    let keep = if n > 2 then (n + 1) / 2 else 2 in
    survivors := ... |> take keep;
    if List.length !survivors = 2 then per_round := 1 else per_round := !per_round * 2)
done;
```

Two details in twelve lines:

- `rank = c.n mod 3` cycles the opponent-reply rank with each sample, so the
  first samples are spread across best / second-best / third-best replies
  rather than piling onto one.
- `keep = 2` when `n <= 2`. The last two candidates are never eliminated; they
  just accumulate one sample per round until the clock stops. **The final
  stage is a head-to-head duel, and its result is monotonic** — more samples
  can only improve the estimate.

## 2. The evaluation function

`lib/bots/oracle.ml:26-66`. Six weighted terms, squashed into `(-1, 1)`.

```ocaml
let life_term life =
  let lost = min 60.0 (max 0.0 (60.0 -. life)) in
  (!life_w *. life) -. (!life_curve *. lost *. lost /. 60.0)

let board (p : player_state) =
  Array.fold_left (fun v -> function
    | None -> v
    | Some s ->
        let held = if has_flag s on_hold then !held_factor else 1.0 in
        v +. (!attack_w *. float_of_int s.attack *. held)
          +. (!creature_life_w *. float_of_int s.life)) 0.0 p.ps_slots

let score ~projected ~before ~bot_id =
  let raw =
    life_term b_life -. life_term o_life +. board bot -. board opp
    +. (!mana_w *. (stock bot -. stock opp))
    +. (!growth_w *. (income bot -. income opp))
  in
  tanh (raw /. !scale)
```

### Term by term

**`life_term` is concave, so a life point is worth more when you have fewer of
them.** Life is worth its face value, plus a quadratic penalty for the life
already lost. With the default weights (`life_w = life_curve = 1.0`,
`life_term x = x - (60-x)²/60`, second derivative −1/30):

| life | `lost` | `life_term` | value of the next point of life |
|---|---|---|---|
| 60 | 0 | 60.00 | 1.08 |
| 50 | 10 | 48.33 | 1.42 |
| 40 | 20 | 33.33 | 1.75 |
| 30 | 30 | 15.00 | 2.08 |
| 20 | 40 | −6.67 | 2.42 |

The last column is the cost of the next five points of damage:
`life_term x - life_term (x-5)`. Losing five life at 60 costs **5.4** raw;
losing five at 30 costs **10.4**, nearly twice as much. A linear life term
would treat both identically.

Note what this is *not*: the difference `life_term bot - life_term opp` is
automatically antisymmetric, so a lead and the mirror-image deficit always
cancel exactly. The curvature does not tilt the scale toward caution — it
makes a slow bleed progressively more expensive to sustain, which is the
behaviour you want: the bot stops taking trades that only look affordable while
it is still healthy.

**`board` discounts held creatures.** A stunned, blocked or paused creature
contributes its attack times `held_factor` (0.3), because it will not swing
next turn. Its life still counts in full. Note this reads the *leaf* board, so
it is a statement about the position as projected, not about intentions.

**`stock` and `income`** are mana in hand and growth per turn. Growth is
weighted **6×** mana by default — `growth_w = 6.0` against `mana_w = 1.0`. In
this game the engine that wins is usually the one that develops faster, and
mana on hand is worth much less than mana arriving every turn. This single
weight is the most important number in the function; see the tuning table.

**`tanh (raw / 60)`** is the normaliser. `raw = 60` scores `0.76`, `raw = 120`
scores `0.96`. Nothing saturates to ±1, which keeps the average of many
samples informative — a rollout that wins the race but does not win the game
still scores well above a draw.

### The `projected` / `before` pair

```ocaml
let b_life = float_of_int bot.ps_life +. (!extrapolate *. (float_of_int bot.ps_life -. b_now)) in
```

`before` is the root of the rollout; `projected` is where we are scoring after
four simulated passes. The **life delta is doubled** with `extrapolate = 1.0`.
If the bot entered the rollout at 50 and is at 45 at the leaf, the score uses
`45 + (45 - 50) = 40`. The rationale: a pass is not a null move, it advances
the turn chain, so a mid-chain leaf life reading lags the real position.
Extrapolating damps that noise. Setting `extrapolate=0` scores the raw leaf.

### A worked evaluation

Bot: life 50, one creature 4/3 on board, mana 6+5+4+3+2 = 20, growth 6.
Opponent: life 40, one creature 2/2, mana 16, growth 5.

```text
life_term 50 - life_term 40  = 48.33 - 33.33 =  +15.00
board bot - board opp        = 4.40  - 2.40  =   +2.00   (0.8*4 + 0.4*3 vs 0.8*2 + 0.4*2)
mana_w  * (20 - 16)          = 1.0 * 4     =   +4.00
growth_w * (6 - 5)           = 6.0 * 1     =   +6.00
                                       raw =  +27.00
score = tanh(27 / 60) = tanh(0.45)          ~=  +0.42
```

A clear but not absurd advantage reads as about four tenths. That is the scale
every rollout sample lives on.

### Terminal values

```ocaml
let terminal ~bot_id winner ply =
  let v = max 0.5 (1.0 -. (!Params.win_discount *. float_of_int ply)) in
  if winner = bot_id then v else -.v
```

A finished rollout is **not** scored by `Eval.score`; it gets a flat ±v where
v decays with the ply the game ended on. Winning on ply 1 is `0.98`, on ply 5
is `0.90`, and the floor is `0.5` (reached at ply 25). Speed has value, but
never more than double.

## 3. The base policy

A rollout is five plies of greedy play for both sides, from Oracle's own
pruned move set (`oracle.ml:190-213`):

```ocaml
None :: List.concat_map (fun id -> placements (Hashtbl.find by_card id) p enemy) order
```

Pass, plus at most `placements_per_card` (2) targets per playable card. Which
two is chosen by `placements` (`oracle.ml:152-188`):

- **Own-side target** (`Select_owner_slot`, `Select_owner_creature`):
  1. the enemy creature with the **highest attack** in that slot — block it;
  2. the **least damaged** own creature, `life / max_life` maximal;
  3. the **emptiest** free slot, `-friends` maximal, where `friends` counts
     filled neighbours — stay packed so a creature has company.
- **Enemy target**: the highest-attack enemy, then the lowest-life enemy.
- **No input**: the card itself.

Inside a rollout the side to move takes the argmax of the leaf score
(`max_by snd scored`, `oracle.ml:337`), except for the opponent's first reply,
which is taken at a chosen rank.

## 4. Modelling the opponent

This is Oracle's most distinctive trick, and it is worth reading twice
(`oracle.ml:323-344`).

```ocaml
let scored = List.map (fun s -> (s, signed ~bot_id (value s (!ply + 1)) pid)) steps in
step := if pid <> bot_id && not !opponent_replied then (
          opponent_replied := true; nth_best scored reply_rank)
        else fst (max_by snd scored);
```

`signed ~bot_id v pid` negates when the side to move is not the bot, so
`scored` always ranks moves **from the perspective of the player about to
move**. `nth_best scored 0` is therefore the opponent's *best* reply, rank 1
its second best, rank 2 its third.

Samples cycle `rank = n mod 3`, weights `[0.60; 0.25; 0.15]`, and the
candidate's value is the weighted average of the per-rank means, with unplayed
ranks falling back to the screen value (`oracle.ml:108-118`):

```ocaml
let acc = Σ over ranks k with samples of  w_k * mean_k
let w   = Σ over ranks k with samples of  w_k
let played     = acc +. (c.screen *. (1.0 -. !w))
let cand_mean  = ((!screen_weight *. c.screen) +. played) /. (!screen_weight +. 1.0)
```

### Worked `cand_mean`

Candidate with `screen = 0.30`, rank 0 sampled 3× averaging `0.50`, rank 1
sampled 2× averaging `0.20`, rank 2 never sampled, `screen_weight = 2`:

```text
acc    = 0.60*0.50 + 0.25*0.20            = 0.350
w      = 0.60 + 0.25                      = 0.85
played = 0.350 + 0.30*(1 - 0.85)          = 0.395
mean   = (2.0*0.30 + 0.395) / (2.0 + 1.0) = 0.332
```

And when the rollouts disagree with the screen — say `screen = 0.10` but the
three ranks come back `0.55, 0.40, 0.30`, one sample each:

```text
acc    = 0.60*0.55 + 0.25*0.40 + 0.15*0.30 = 0.475
w      = 1.0
played = 0.475
mean   = (2.0*0.10 + 0.475) / 3.0          = 0.225
```

So the rollouts moved the candidate from `0.10` to `0.225` — the screen is
pulled toward the sampled evidence with weight `screen_weight : 1`, and with
enough samples it stops mattering at all.

The point of the ranked replies: **a move that is only good because the
opponent's single best reply is bad should not get full credit.** Weighting
rank 0 at 0.60 rather than 1.0 prices in that the opponent has other options.

## 5. Watching it work

`ORACLE_DEBUG=1` prints the board and both rankings before the move is chosen.
Real output, 60 ms budget, seed 7:

```text
oracle-stats search playouts=14 wins=0 losses=0 sims=6800 rounds=1 spentMs=63/budgetMs=60 best=0.354 n=3 cmd=0> water.sprite at 5
houses 3,5,5,6,2, life 60, slots .,.,.,.,.,.
houses 6,5,5,2,2, life 60, slots .,.,.,.,.,.
  screen: 0> water.sprite at 5 screen=0.386 | 0> fire.monk at 5 screen=0.366 | 0> air.fairy at 5 screen=0.364 | 0> water.golem at 5 screen=0.325 | 0> Conscription screen=-0.050 | pass screen=-0.067
  final:  0> water.sprite at 5 mean=0.354 n=3 | 0> water.golem at 5 mean=0.342 n=2 | 0> air.fairy at 5 mean=0.331 n=2 | 0> fire.monk at 5 mean=0.316 n=3 | 0> Conscription mean=-0.055 n=2 | pass mean=-0.083 n=2
```

How to read it:

- `screen:` is one entry per card (best placement each), plus pass, in screen
  order. Six cards in hand → six entries.
- `final:` is the survivors, sorted by `cand_mean`. Here all six survived —
  the clock expired during round 1, so **no halving ever happened**. That is
  normal at 60 ms and it is the anytime property in action: a usable ranking
  exists from the first sample onwards.
- `n=` is how many rollouts each candidate received. Values of 1–3 mean the
  budget was the binding constraint, not the algorithm.

Now a decision where the two stages disagreed. Same run, later ply:

```text
  screen: 0> fire.wave screen=0.983 | 0> fire.monk at 0 screen=0.956 | 0> air.call at 2 screen=0.956 | ... | pass screen=0.820
  final:  0> earth.care at 1 mean=0.874 n=1 | 0> air.call at 2 mean=0.860 n=2 | 0> air.lightnin mean=0.849 n=2 | ... | 0> fire.wave mean=0.655 n=2 | 0> fire.monk at 0 mean=0.477 n=2
```

`fire.wave` screened **first** at `0.983` and finished **eighth** at `0.655`;
`fire.monk` screened second at `0.956` and finished last at `0.477`. Whatever
the engine does in the five plies after the card lands, four passes of
projection do not see it. (The board in that frame had the bot ahead on
creatures and level on life, and every screen value was above 0.9 — a regime
where the leaf terms are nearly saturated and small differences in what
happens next are what separate the candidates.) This reversal is the entire
reason the two-stage design exists.

## 6. Cost

One playout is not one simulation. Per ply, every candidate move in the pruned
set is played (a fork each) and then projected forward four passes:

```text
  playout cost  ~=  1 (root) + 4 (its projection)
                +  horizon * branching * 5        (1 fork + 4 projection passes each)
```

With `horizon = 5` and branching 7 (pass + 2 placements × 3 playable cards)
that is about **180 simulations per rollout**, before the screen phase. Real
numbers from a 200 ms budget on this machine:

```text
oracle-stats search playouts=60  wins=0 losses=0 sims=28640 rounds=2  spentMs=200/budgetMs=200 best=0.354 n=14 cmd=0> water.sprite at 5
oracle-stats search playouts=273 wins=273 losses=0 sims=21701 rounds=129 spentMs=200/budgetMs=200 best=0.976 n=134 cmd=1> earth.care at 5
```

Roughly 10⁵ simulations/second. The first line did two halving rounds and gave
the winner 14 samples; the second ran out of candidates down to the final duel
and then spent 129 further rounds sampling that pair — `rounds=129` with
`n=134` is the head-to-head stage doing its job.

Use `sims=` as the real cost meter when you change parameters. If you double
`horizon`, sims per rollout doubles and the number of completed rounds halves.

## 7. Tuning

`ORACLE_OPTS` is a comma-separated `key=value` list, read once at module load
(`oracle.ml:68-99`). Unknown keys just print a warning.

```sh
ORACLE_OPTS=horizon=3,rootCards=4,projectPlies=2 \
  dune exec ./bin/main.exe -- --bots oracle,greedy --games 4 --budget 200 --seed 1
```

| key | default | effect |
|---|---|---|
| `rootCards` | 8 | candidates entering the halving, plus pass |
| `horizon` | 5 | plies per rollout |
| `placementsPerCard` | 2 | branches per card in the base policy |
| `replyWeights` | `0.6;0.25;0.15` | opponent reply ranks, `;`-separated |
| `firstRoundSamples` | 6 | rollouts per candidate in round 1 |
| `projectPlies` | 4 | passes before the leaf is scored |
| `winDiscount` | 0.02 | how fast a terminal's ±1 decays with ply |
| `screenWeight` | 2.0 | how much the shallow screen counts in `cand_mean` |
| `lifeW` | 1.0 | life term |
| `lifeCurve` | 1.0 | quadratic penalty for life already lost |
| `attackW` | 0.8 | board attack |
| `creatureLifeW` | 0.4 | board creature life |
| `manaW` | 1.0 | mana in hand |
| `growthW` | 6.0 | growth — the biggest lever in the evaluation |
| `scale` | 60.0 | raw value that maps to `tanh(1)` |
| `extrapolate` | 1.0 | life-delta doubling at the leaf |
| `heldFactor` | 0.3 | attack weight of a stunned/blocked/paused creature |

Two things to know before you tune:

- `Params` are `ref`s, so `ORACLE_OPTS` affects **Diviner too** — it reuses
  `Oracle.Params.root_cards`, `reply_weights` and `screen_weight`.
- These are process-global. There is no per-seat setting.

Cheap experiments worth running:

```sh
# depth vs breadth, same budget
ORACLE_OPTS=horizon=2 dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --budget 500 --seed 1
ORACLE_OPTS=horizon=8 dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --budget 500 --seed 1

# greedy evaluation vs the smooth one
ORACLE_OPTS=scale=8 dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --budget 500 --seed 1

# a candidate that only survives if the opponent's best reply is bad
ORACLE_OPTS=replyWeights=1;1;1 dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --budget 500 --seed 1
```

## 8. Limits

- **Only the best placement of each card is ever rolled out.** The shortlist
  is chosen by screen value, so a card whose good target reveals itself only
  after the opponent replies is invisible to the search.
- **Greedy rollouts have correlated error.** Every sample of a move shares the
  same base-policy bias, so the estimate is confident about the wrong thing.
  A uniform-weight reply scheme (`replyWeights=1;1;1`) mixes this a little;
  a random base policy, as Lecturer uses, mixes it more.
- **No reuse between moves.** Each `execute` starts from nothing. A bot that
  kept the previous search's statistics across turns would be sharper, at the
  cost of a stateful `mover`.
- **It plays against one oversized guessed deck.** Like Scala's
  `BotKnowledge`, the mover never searches the opponent's real deck.
  `Belief.blind` swaps it for the union of a few legal deals
  (`Belief.superset_samples`, 4), each containing every opponent card seen so
  far, and keeps that guess until a new card shows up. The opponent may then
  answer with more cards than a real hand holds, which errs on the side of
  caution, but a card none of the deals drew is still invisible. Diviner takes
  the other route and keeps the deals apart as separate worlds
  ([Diviner](05-diviner.md)). (Earlier versions of the port searched the real deck, which made
  every bot an upper bound; the board is still real, see Diviner §8.)

Next: [05-diviner.md](05-diviner.md).