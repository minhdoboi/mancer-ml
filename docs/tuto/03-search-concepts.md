# 3. The search concepts

The algorithms, taught generically. Nothing here is specific to a file; the
last section maps each idea onto the bot that implements it. Read this before
documents 4 and 5, or skip it if you already know the vocabulary.

## 1. Why not just search the tree?

The obvious design — build the game tree, do minimax or MCTS to a fixed depth —
dies here for two reasons.

**The branching factor is huge.** A hand is four basics plus a special; each
card has up to six targets. With three playable cards, `Game.legal_commands`
returns twenty-odd commands. Depth 5 would be 20⁵ ≈ 3.2 million positions, and
each position costs a full `Game.simulate` — a state copy plus the whole
end-of-turn chain. Even a perfect implementation would take minutes.

**The leaves are only half-known.** Life totals are not the outcome of this
game; nobody wins at turn 3. A leaf position's value has to be *judged*, and
judging needs an opinion about what happens next.

So every bot here does the same trade: **do not search deeply, search wide and
shallow, and be honest about the leaf.** That is the whole design space, and
the bots differ only in how they spend the budget inside it.

## 2. Leaf evaluation

A **leaf score** maps a position to a number. Requirements in this game:

- **Bounded.** `(-1, 1)` or `[0, 1]`, so averages of many samples stay
  interpretable and one wild outlier cannot dominate.
- **Continuous and monotone.** Tiny differences must produce tiny score
  differences, otherwise averaging is meaningless.
- **Zero-centred.** Bot-advantage, not "good for player 0" — otherwise the
  search has to negate by turn, which is exactly where sign bugs live.

`Oracle.Eval.score` satisfies all three with `tanh (raw / 60)`: the raw sum of
six weighted terms is unbounded, `tanh` squashes it into `(-1, 1)`, and the
`/ 60` is the scale at which a clearly winning position saturates. A raw value
of +60 scores +0.76, not +1.0; you need a runaway to get near 1.

The alternative is a **shallow projection**: instead of guessing from the
position, push it a few deterministic plies forward (here, four passes) and
score the result. Passes are almost free — each is one `Game.simulate` — and
they force pending attacks and mana growth to resolve, which removes a lot of
the noise a mid-transition leaf carries. `Heuris.score` takes the opposite
route: it simulates a 12-turn *combat* inside the evaluation, ignoring cards
entirely and asking "who wins the race from here".

## 3. Rollouts

A **rollout** is a short game played by a cheap **base policy**. Its value is
the leaf score at the end. Rollouts are how these bots buy depth without
paying for it.

```text
   root move
     |
     +--> opponent reply        |
     |    bot move              |  <- each simulated through the real engine
     |    opponent reply        |
     |    ... horizon plies     |
     |                          |
     +--> leaf score in (-1, 1)
```

Three parameters define a rollout, and every bot sets all three differently:

| parameter | Oracle / Diviner | Lecturer | Summoner / Strategist |
|---|---|---|---|
| base policy | **greedy** — the argmax of the leaf score over pruned moves | **random**, with a pass weight | **greedy** on `Heuris` |
| horizon | 5 plies | 6 plies | bounded by depth, 2–3 for Strategist |
| leaf | 4 passes, then `Oracle.Eval.score` | 4 passes, then `Oracle.Eval.score` | the node's `Heuris` value directly |

Greedy base policies give *correlated* rollouts — all samples of a move share
the same biases — which is cheap and fine for a one-ply judgement. Random
policies give *unbiased* noise, which is what MCTS needs so that visit counts
mean something.

## 4. Anytime algorithms and the wall clock

Every bot here gets a budget in milliseconds and is checked against
`now_ms () < deadline` inside its loop. That makes them **anytime**: they must
return a usable answer no matter when they are cut off.

The consequence is that the *algorithm*, not the *depth*, is the knob. You
cannot ask "search to depth 5"; you ask "keep sampling until the clock runs
out, and make every sample count". This favours designs where accuracy
improves smoothly with sample count — Monte Carlo methods — over designs where
you either finish or you have nothing.

## 5. Successive halving

The controller Oracle and Diviner use. It is a multi-armed bandit schedule, and
it is the cleanest idea in this codebase.

Start with *k* candidates. Give each the same small budget. Discard the worst
half. Double the per-candidate budget. Repeat until one or two remain, then keep
sampling the final pair until the clock dies.

```text
  round 1:  [A B C D E F G H | pass]   9 candidates, 6 samples each   -> 54 rollouts
            keep 5
  round 2:  5 candidates, 12 samples each                          -> 60 rollouts
            keep 3
  round 3:  3 candidates, 24 samples each                          -> 72 rollouts
            keep 2
  round 4+: 2 candidates, 1 sample per round, until the deadline
```

In code, Oracle's loop is `lib/bots/oracle.ml:347-366`:

```ocaml
let keep = if n > 2 then (n + 1) / 2 else 2 in
survivors := !survivors |> List.sort (fun a b -> compare (cand_mean b) (cand_mean a)) |> take keep;
if List.length !survivors = 2 then per_round := 1 else per_round := !per_round * 2
```

Note `keep = 2` when `n <= 2`: the last two are **never** eliminated, they just
keep accumulating samples. Anytime by construction — you always have a
ranking, and the last round is a pure head-to-head duel.

Why not just give every move the same budget? Because the bad moves
outnumber the good ones: with 9 candidates and a fixed 100 rollouts, you spend
about 11 on each and most of them are obvious losers. Halving spends the
budget where the decision is still open. If you already know the answer — the
screening pass — the whole schedule is cheap.

The one thing successive halving is *not* good at is reuse: it throws away the
statistics of eliminated candidates entirely. That is the trade against MCTS,
which keeps everything it learned in a tree.

## 6. Modelling the opponent without solving the game

You cannot afford to ask "what is the opponent's best reply?" — that is a
search of its own. Oracle approximates it three ways at once.

**Screening.** Before any rollout, every legal move is played once and scored
(`oracle.ml:277-284`). This is the cheap first cut that makes the rest
affordable, and it is where most of the discarded candidates die. The screen
value is also kept and mixed into the final mean, which is why a move that
looks terrible on the shallow read is not instantly thrown away.

**Ranked replies.** Inside a rollout, the opponent's *first* reply is not
taken at random and not taken greedily — the rollout is run at a chosen rank
(`nth_best scored reply_rank`, `oracle.ml:336`). Consecutive samples cycle
`reply_rank = n mod 3` over weights `[0.60; 0.25; 0.15]`, and the candidate's
score is the weighted average of the per-rank means, with the unplayed ranks
falling back to the screen value:

```ocaml
let played = !acc +. (c.screen *. (1.0 -. !w))     (* oracle.ml:117 *)
```

So "the opponent's best reply is bad for us" is only worth full credit if it
survives the opponent's 2nd and 3rd best replies too. It is a cheap
approximation of a soft minimax: mostly play the opponent's best line, but
discount the move if only that one line is bad for them.

**Greedy afterwards.** The bot's own moves and every later opponent move are
the argmax of the leaf score (`max_by snd scored`, `oracle.ml:337`). Nobody
models a deep opponent strategy; they model the next one.

## 7. UCT and MCTS

Lecturer's controller, and the only real tree in the codebase. Four repeating
steps:

```text
  SELECT     walk down from the root, always taking the child that maximises
             UCT = mean(child) + c * sqrt( ln N(parent) / n(child) )
  EXPAND    add the children of the first node with none
  SIMULATE  play the rest randomly to the horizon, score the leaf
  BACKPROP   add one visit to every node on the path, add the (signed) reward
```

The actual formula (`lecturer.ml:136-137`):

```ocaml
let uct parent_visits (c : node) = mean c +. (!Params.exploration *. sqrt (log (max 1.0 parent_visits) /. c.visits))
```

The exploitation term is the child's mean reward; the exploration term decays
with the child's visit count, so a well-tried move stops being re-tried and a
neglected one gets another look. `exploration` defaults to 0.7.

Three details make it work here:

- **Priors.** Every new child is created with `prior_visits = 2.0` virtual
  visits at its heuristic value (`lecturer.ml:112-130`), so the very first UCT
  comparison is informed instead of arbitrary. Without priors, early visits are
  noise.
- **Negamax backup.** Each node stores rewards from the perspective of whoever
  *played* its move (`n.mover`), and `signed ~bot_id r n.mover` flips the sign
  per node on the way up. One number serves both players.
- **Branching from `base_moves`.** Only the root expands to the full
  `Game.legal_commands`; everything deeper uses Oracle's pruned set. The tree
  stays affordable, and the code is shared.

Lecturer picks its move by **visit count**, not by mean value — the most
visited root child is the one the tree agrees on.

## 8. Best-first search and iterative deepening

Summoner (and Strategist, which copies its shape) is a plainer tree search:

- **Iterative deepening**: rebuild from scratch at depth 2, 3, 4, … while the
  budget lasts, and only accept a pass's answer if that pass *fully settled*
  (`summoner.ml:242-269`). An unfinished pass is discarded rather than trusted.
- **Best-first within a pass**: instead of a fixed order, `should_open` and
  `contender` pick the most promising unexpanded node, so easy wins are
  resolved first.
- **Min/max backup by side to move** (`pull`, `summoner.ml:96-113`), the
  classic negamax-alternative.
- **Gamma discounting** on non-terminal values (`plan.gamma ** depth`,
  `strategist.ml:308`), so "two moves from winning" is worth less than "one
  move from winning".

Rebuilding every pass is wasteful and Summoner is the weakest search bot for
it, but it has a property the others lack: it knows whether it finished.

## 9. Bandits over plans

Strategist adds one more layer: the search parameters themselves are treated as
arms of a UCB1 bandit. It first reads a **tempo** score from the matchup's
character — how fast the decks in play tend to end the game — and maps that to
a *plan*: a `gamma` discount (0.97 value-oriented to 0.70 tempo-oriented), a
max depth (3 or 2) and a branching width (3.0 to 8.0), interpolated by tempo
and by how confident the read is (`strategist.ml:148-210`).

Three arms are then built along that dial (`strategist.ml:214-215`):

```ocaml
let arms t confidence =
  List.map (fun x -> for_tempo_plan (clamp01 x) confidence)
    [ t -. Params.bandit_delta; t; t +. Params.bandit_delta ]      (* delta = 0.25 *)
```

Each turn it probes each arm one ply deep over the top 5 ordered moves, picks
the best by

```text
score = alpha * probe_value + (1 - alpha) * bandit_mean + bandit_bonus
```

(`strategist.ml:469-473`, with `alpha = 0.7`, `bandit_c = 1.4`), runs the real
search with the winner, and feeds the outcome back into the band's running
means. Over a game the bandit learns which tempo suits the matchup.

The idea generalises: when the parameters are the hard part, search over the
parameters with the same budget machinery.

## 10. Beliefs: playing without full information

Everything above assumed the bot knows the whole state. For this game it does
(see document 2), which makes the problem *much* easier than it should be. So
what would a principled version look like? This is exactly what Diviner
implements, and the vocabulary is worth learning on its own.

**Partially observable.** The opponent's twenty cards are hidden. The bot
knows their special house and the cards they have revealed, nothing else. Full-information search is
optimistic: it evaluates "the opponent has exactly the cards they have",
which is one sample out of an enormous space.

**Belief space.** Instead of a single hidden state, keep a set of *possible*
hidden states — here, possible opponent deals — called **worlds** or
particles. Search each world separately. The three assumptions that make this
tractable:

1. Sample worlds by **rejection**: draw a random deal from the game's own deck
   builder (`Shuffle.create_player`), so every world is a *legal* deck, not an
   arbitrary hand. Rejection is cheap here and never produces an impossible
   position. Cards the opponent has already revealed are passed as `~require`,
   so every world contains them.
2. Keep one world that is the **truth** (or the best available guess), so the
   search is never uniformly pessimistic.
3. Search each world with an *independent* controller — in Diviner's case,
   Oracle's, with its own statistics.

**Expected value is not enough.** Averaging over worlds is the obvious
aggregation and it is wrong for play: a move that wins 2 worlds out of 3 and
loses badly in the third is much worse than a move that is mildly good in all
three, because the third world might be the real one. So:

```text
score(move) = mean(world values) - risk_aversion * (mean - worst world)
```

The penalty term grows with the spread and vanishes when all worlds agree. With
`risk_aversion = 0` this collapses to plain expectation — the optimistic,
Oracle-like bot. With `risk_aversion = 1` it becomes pure minimax over worlds,
which is far too timid. `0.35` is a playable middle.

Note also that worlds are **resampled only when a new card is revealed**; no
weighting is carried across turns.
They are constrained to contain the revealed cards, but nothing is inferred
from what the opponent did *not* play. Full particle filtering (weight worlds
by how likely the observed play was, then resample) would be strictly better
and is the obvious next step if you want Diviner to get sharper.

## 11. How the six bots differ, in one table

| | greedy | summoner | strategist | lecturer | oracle | diviner |
|---|---|---|---|---|---|---|
| **generator** | first of `available_commands` | full at the root | full, role-ordered | full at the root, `base_moves` below | `base_moves` | `base_moves` |
| **depth** | 0 | 2–8, iterative deepening | 2–3 | 4 (`maxDepth`) | 5 (`horizon`) | 5 |
| **rollout** | none | none (tree only) | none (tree only) | random, 6 plies | greedy | greedy |
| **leaf** | none | `Heuris.of_state` | race + synergy, gamma-discounted | 4 passes + `Eval.score` | 4 passes + `Eval.score` | per-world `Eval.score` |
| **controller** | none | best-first | best-first + UCB bandit | UCT / MCTS | **successive halving** | successive halving × worlds |
| **opponent model** | none | guessed deck | guessed deck | guessed deck | ranked replies | ranked replies, per world |
| **hidden info** | none | one guess | one guess | one guess | one guess | **samples several** |
| **tree between moves** | no | no | bandit only | no (rebuilt) | no | no |
| **default budget** | — | 1000 ms | 1000 ms | 1000 ms | 1000 ms | 1000 ms |

## 12. How many samples is enough

Every bot here decides by averaging: the candidate's value is the mean of *n*
leaf scores. The mean is unbiased but its **standard error is `σ/√n`**, and
almost every surprise in a low-budget search is that error being larger than the
gap you are trying to read.

Leaf scores live in `(-1, 1)`, but they are not uniform over it. In practice a
5-ply greedy rollout lands within a few tenths of its own candidate's mean, with
a heavy tail when the rollout sometimes ends the game outright. Take
`σ ≈ 0.4` as a realistic per-sample figure and treat the tables below as "what a
gap has to be to be worth anything at this budget".

Two candidates with `n` independent samples each have
`SE_diff = σ·√(2/n)`, so the smallest gap worth calling at 95% confidence
(`z = 1.96`) is

```text
resolvable gap  =  1.96 · σ · √(2/n)  =  0.78 · √(2/n)     for σ = 0.4
```

| samples per candidate `n` | `SE` of one mean | resolvable gap |
|---|---|---|
| 1 | 0.40 | 1.11 |
| 3 | 0.23 | 0.64 |
| 6 | 0.16 | 0.45 |
| 14 | 0.11 | 0.30 |
| 60 | 0.052 | 0.14 |
| 250 | 0.025 | 0.07 |

Inverted, that is the rule for designing a budget: to separate two moves whose
true values differ by `Δ`, you need

```text
n  ≥  2 · (1.96 · σ / Δ)²      samples per move
```

so `Δ = 0.3` costs 14 rollouts per move, `Δ = 0.1` costs 123, and `Δ = 0.02`
costs 3 000. **No budget in this game's UI can defend the last two.** The
consequence is not that the bots are broken — it is that *the right move is
usually not the second-best move*, so coarse estimates are fine. It also means a
real log line can be honest and useless at the same time:

```text
  final:  0> water.sprite at 5 mean=0.354 n=3 | 0> water.golem at 5 mean=0.342 n=2 | ...
```

A gap of 0.012 at `n = 3`, where 0.64 would be needed to call it. The correct
reading of that line is *"these candidates are indistinguishable, play any of
them"* — not *"sprite was the best"*. Undoing the screen/playout blend of
[document 4](04-oracle.md) §4 with `screen_weight = 2` shows who really decided
it:

```text
cand_mean = (2·screen + played) / 3

sprite:  (2·0.386 + played) / 3 = 0.354  ->  played = 0.290   (3 rollouts)
golem:   (2·0.325 + played) / 3 = 0.342  ->  played = 0.376   (2 rollouts)
```

The rollouts actually preferred the golem by 0.086, and the screen's 0.061 lead
overrode it. That is the design working as intended at a 60 ms budget — the
shallow read is trusted, the expensive read only gets to break ties — but it is
worth being clear that at this budget the rollouts are not choosing the move.

### Correlation makes it worse

With a greedy base policy every sample of a move shares the same bias, so the
samples are not independent. For equicorrelated samples the standard error is

```text
SE = σ · √( (1 + (n−1)·ρ) / n )       →  σ·√ρ   as n → ∞
```

which is the useful way to state it: **a correlated estimator has a noise floor
that more samples cannot pass.** `ρ = 0.3` already floors the standard error at
`0.4·√0.3 = 0.22`, so doubling the budget buys nothing. This is exactly the
limitation [document 4](04-oracle.md) §8 states qualitatively, and it is why
Lecturer's random rollouts are worth their extra noise: for them `ρ ≈ 0` and the
error really does fall as `1/√n`.

### One-sided damage

Successive halving does not make symmetric mistakes: an unlucky round *eliminates
the best candidate* and no later sample can bring it back. The same is true of
the shortlist. This asymmetry is the whole reason the screen value is mixed back
into `cand_mean` with weight `screen_weight`, and the reason the lethal shortcut
comes before the search rather than after it — if you can see a win, take it
without spending a single rollout on the possibility of being wrong.

### The schedule's budget arithmetic

The default halving schedule spends, per round, `k_j · s_j` rollouts where
`k_j` is the number of survivors and `s_j` the samples per candidate:

```text
round 1:   9 candidates ×  6 =  54
round 2:   5 candidates × 12 =  60
round 3:   3 candidates × 24 =  72
round 4+:  2 candidates ×  1 =   2 per round, forever
```

Reaching the final duel costs 186 rollouts; at the ~180 simulations per rollout
of [document 4](04-oracle.md) §6 that is ~33 k simulations, which is why the
200 ms runs log `rounds=1` or `2` and never get near the duel. The last line is
the important one: two candidates are never eliminated, so the search degrades to
a head-to-head rather than to a guess, and every further second of budget makes
that duel more reliable.

## 13. Vocabulary

| term | meaning here |
|---|---|
| ply | one move by one side |
| move | one command (or pass) by the side to move |
| root | the position the bot must choose a move in |
| rollout / playout | a short game from a move, played by the base policy |
| base policy | the cheap player used inside a rollout |
| leaf | the position a rollout ends in |
| screen | one shallow evaluation before any rollout |
| world / belief | one guess about the opponent's hidden hand |
| anytime | must produce a valid answer if interrupted at any instant |
| prior | a virtual visit given to a new MCTS child from a heuristic value |
| negamax | storing rewards from the mover's perspective, flipping sign per ply |
| standard error | `σ/√n`, how far the sample mean is expected to sit from the true value |
| resolvable gap | the value difference `n` samples can defend; below it, the ordering is noise |
| `ρ` | correlation between samples; positive `ρ` puts a floor under the standard error |

Next: [04-oracle.md](04-oracle.md) — successive halving and the Oracle
evaluation, line by line.