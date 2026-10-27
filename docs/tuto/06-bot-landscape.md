# 6. The bot landscape

All six bots, side by side, with the knobs each one exposes and a method for
measuring a change rather than guessing at one.

## In one minute

- There are **six bots**, from `greedy` (plays the cheapest card, no thinking)
  to `diviner` (imagines several possible opponent decks and plays the move
  that holds up best in all of them).
- They are **not six programs**. Each one is a different mix of the same four
  parts: a *move generator*, a *simulator*, a *scoring function* and a
  *controller* that decides where to spend the thinking time.
- The lobby's six "difficulty levels" just pick one of these bots. The
  **budget** (milliseconds of thinking per move) is the only other dial.
- To know whether a change helped, **play many games and compare**
  (`bin/main.exe`, §5). Twenty games is a smoke test, not proof.

| If you want to… | Go to |
|---|---|
| know which bot is which | §1 and §2 |
| change a bot's behaviour from the shell | §3 |
| understand a `*-stats` log line | §4 |
| check that a change really made a bot stronger | §5 |
| make a bot stronger, faster or deliberately weaker | §6 |

### Words used in this document

| word | plain meaning |
|---|---|
| **ply** | one move by one side (two plies = I move, you answer) |
| **rollout** | a quick imaginary game played to the end (or to a fixed depth) by a cheap policy, to see who is ahead |
| **leaf / evaluation** | the score given to a position once the imagining stops; "how good does this look for me?" |
| **budget** | the thinking time allowed per move, in ms (the lobby slider) |
| **successive halving** | try every candidate move a little, drop the worst half, try the rest more, repeat |
| **UCT / MCTS** | a search that grows a tree and keeps visiting the branches that look most promising |
| **world** (diviner) | one guess of what the opponent's hidden deck could be |
| **Elo** | a rating that goes up when you beat opponents, scaled so a 400-point gap means "10 times better" (§5) |

Document 3 defines these properly; this table is only so you can read on.

## 1. Six bots, four building blocks

Every bot in `lib/bots/` is a different setting of the same four components
described in [document 2](02-game-view.md):

| | 1. generator | 2. simulator | 3. evaluation | 4. controller |
|---|---|---|---|---|
| **greedy** | first of `available_commands` | — | — | none |
| **summoner** | full `legal_commands` | `Game.simulate` | `Heuris.of_state` + life margin | best-first + iterative deepening |
| **strategist** | full, role-ordered | `Game.simulate` | `Heuris.of_state` + `synergy`, gamma-discounted | best-first + UCB bandit |
| **lecturer** | full at the root, `base_moves` below | `Game.simulate` | `Oracle.Eval.score` after 4 passes + life margin | UCT / MCTS |
| **oracle** | `Oracle.base_moves` | `Game.simulate` | `Oracle.Eval.score` after 4 passes | successive halving |
| **diviner** | `Oracle.base_moves` | `Game.simulate`, one engine per world | `Oracle.Eval.score` per world | successive halving × worlds |

In plain words: **greedy** grabs the first thing it can; **summoner** and
**strategist** look a few moves ahead with a hand-made "who is winning the
race" formula; **lecturer** plays thousands of random-ish games from the
current position and trusts the averages; **oracle** does the same but wastes
less time on bad moves; **diviner** is oracle that keeps several guesses of
the opponent's deck instead of one.

Two evaluation families, and they are not interchangeable:

- **`Oracle.Eval.score`** (`oracle.ml:26-66`) — smooth, resource-weighted,
  `(life curve, board, mana, growth)`, squashed with `tanh`. Used by oracle,
  diviner and lecturer.
- **`Heuris`** (`heuris.ml`) — a 12-swing **race forecast** blended with a
  long-term resource term, or `life_mana`, or a weighted `synergy`. Used by
  summoner and strategist only.

They answer different questions. `Eval` asks "how does the position look if
we stop now". `Heuris.score` asks "who wins the fight". `Heuris.blend` gives
the long-term term weight only where the race is undecided, so a bot using it
will happily take a bad trade when it is already winning. Which one is better
for this game is an empirical question — measure it.

The summoner adds a third number of its own, the **life margin**: the race is
a verdict (`±1` once the forecast decides the fight), so in a position the bot
is losing every line ties and the search falls back on an arbitrary move — in
practice the cheapest card it holds, which is what made the duel bot look like
it did not care about its life at all. The margin (how much life the line
leaves the bot) breaks those ties, and a pass that ranks nothing no longer
overwrites the choice of a pass that does. Document 2 §5 has the details; the
Scala's `SummonerBot` has the same flaw, so this is a fix the port made on its
own. Measured over 30 games per pairing at a 100 ms budget (seed 7): against
`oracle` 0/30 before and 5/30 after, against `lecturer` 1/30 before and 6/30
after.

The lecturer had the same flaw, for the same reason: its leaf is a verdict
too, only the verdict comes from the engine — a line that ends the game is
`±1`. In a position the bot is losing, the random rollout kills the bot in
every line, every root child ties, and the search falls back on the visit
count, which is noise. The symptom is a bot that stops trying: it drops a
creature in a free slot and dies, while the blocking placement was legal all
along. The lecturer therefore scores a root move with a **life margin** too
(how much life the line leaves the bot after the opponent's answer) and plays
the best value rather than the most visited one. On the board of
`test/test_lecturer.ml` — a bot at 8 life against a blargl, with a deck of
two creatures and no spell — the blocking move came out of 2 searches out of
12 before the fix and 12 out of 12 after. The win rate did not move: over 100
games per pairing (100 ms, seed 7) the bot won 346 games before the change
and 336 after, inside the noise of a 500-game sample (§5 has the arithmetic).
The change buys behaviour, not Elo.

## 2. The difficulty ladder

The duel lobby offers six levels (`lib/game/table.ml:58-73`), in the Scala
`AiLevel` order:

| level | face | bot | search |
|---|---|---|---|
| 1 | summoner | `summoner` | best-first, depth 2–8 |
| 2 | strategist | `strategist` | best-first + bandit, depth 2–3 |
| 3 | gambler | `lecturer` | UCT — the port kept the faces, not the brain |
| 4 | lecturer | `lecturer` | UCT, the same bot as level 3 |
| 5 | oracle | `oracle` | successive halving |
| 6 | diviner | `diviner` | successive halving × 4 worlds |

Levels 3 and 4 are the same engine with different portraits: *"Gambler is not
ported, so those faces still play lecturer"* (`table.ml:56-57`). The display
name comes from `opp_kind` and the engine from `opp_bot`, which is why an
`ai.gambler` i18n key exists with no Gambler behind it. Levels carry no
difficulty parameter of their own — the difference is entirely which module
runs, at whatever budget the lobby slider is on (`budget_options =
[100; 250; 500; 1000; 2000; 5000]`, `table.ml:324`).

The bot order in the ladder is not a strength ranking, and it does not follow
measured strength either. At 200 ms per move (12 games per pairing,
`--seed 7`), Lecturer beat Oracle 7–5 and Diviner 8–4, and Oracle beat
Diviner 7–5. Letting the bots peek at the real deck (`BELIEF_SAMPLES=0`)
did not change the order: Lecturer still beat Oracle 11–1. Diviner splits its
budget across its worlds, so at small budgets it completes fewer playouts than
Oracle. Summoner and Strategist were not part of that run. Twelve games is a
small sample: re-measure before reordering the levels.

## 3. The knobs

| bot | env var | keys |
|---|---|---|
| lecturer | `LECTURER_OPTS` | `exploration` 0.7, `maxDepth` 4, `priorVisits` 2.0, `rolloutPlies` 6, `projectPlies` 4, `passWeight` 0.5, `lifeWeight` 0.1 |
| oracle | `ORACLE_OPTS` | 8 search + 9 evaluation keys, see [document 4](04-oracle.md) |
| diviner | `DIVINER_OPTS` | `beliefCount` 3, `riskAversion` 0.35, `firstRoundSamples` 1 |
| summoner | `SUMMONER_OPTS` | **parses nothing** — `apply_overrides` only echoes the string (`summoner.ml:19-23`) |
| strategist | *(none)* | no `apply_overrides` at all; its parameters are plain constants (`strategist.ml:148-172`) |

Format is `key=value,key=value`, applied once at module load, and unknown keys
print a warning. All are process-global `ref`s, so there is no per-seat
setting, and `ORACLE_OPTS` also reconfigures Diviner and Lecturer's leaf.

Two debugging variables exist: `ORACLE_DEBUG` (full screen and final ranking,
[document 4](04-oracle.md)) and `LECTURER_DEBUG` (board plus every root child
with its visit count and mean, `lecturer.ml:267-273`). Diviner has no debug
flag of its own.

## 4. Reading the stats lines

Every search bot prints one line per move when `stats_log` is on — which is the
default in tournaments and off in the front ends, where the TUI redirects bot
stdout to `/dev/null` so the screen stays clean.

```text
oracle-stats search playouts=60 wins=0 losses=0 sims=28640 rounds=2 spentMs=200/budgetMs=200 best=0.354 n=14 cmd=0> water.sprite at 5
lecturer-stats iterations=... wins=... losses=... sims=... spentMs=.../budgetMs=... best=... n=... cmd=...
diviner-stats beliefs=4 spentMs leftover cmd=0> fire.wave score=0.818
summoner-stats depth=3 spentMs=1000/budgetMs=1000 cmd=0> water.turtle at 0
```

Fields worth knowing:

- `sims=` is the honest cost. It is the count of `Game.simulate` calls, each
  of which copies the state and runs the end-of-turn chain. Compare it against
  `spentMs` to see whether a change bought accuracy or just moved work around.
- `rounds=` is the successive-halving stage. `rounds=1` means the clock expired
  during the first round and **no candidates were eliminated** — normal at low
  budgets. `rounds=129` means the search reached the final duel and spent the
  rest of the budget sampling the last two candidates.
- `best=` is the winning value, in `(−1, 1)` for oracle/diviner/lecturer, and
  for summoner the `Heuris` blend of race and long-term.
- `n=` is the winner's rollout count.
- `cmd=pass` on an oracle line is legal: passing can win the game through the
  end-of-turn chain.

Two known rough edges in these lines: `diviner-stats` prints the literal words
`spentMs` and `leftover` where the elapsed-time placeholders should be, and
`strategist` gates its line on `settings.logging` rather than `stats_log`, so
it never prints in a duel.

## 5. Measuring a change

The tournament driver is the measuring instrument. It plays complete games and
reports W/L/D plus Elo (K = 16) in `summary.json`.

```sh
# the baseline: greedy always takes the cheapest legal card, so greedy vs
# greedy is not a signal.  Oracle vs greedy is.
dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --budget 1000 --seed 1

# the interesting question for Diviner: what is honest information worth?
dune exec ./bin/main.exe -- --bots oracle,diviner --games 20 --budget 1000 --seed 1

# the full round robin
dune exec ./bin/main.exe -- --bots greedy,summoner,strategist,lecturer,oracle,diviner --games 4 --seed 1

# house matchups: both seats take the special
dune exec ./bin/main.exe -- --bots greedy,oracle --games 10 --special zen --seed 1

# the house tournament: the houses play each other, the diviner is the instrument
dune exec ./bin/main.exe -- --mode houses --special zen,wind,darksider \
  --bots diviner --games 12 --seed 1

# replay a run's card and Elo tables without playing
dune exec ./bin/main.exe -- --mode summary --dir tournament-results/<timestamp>
```

**Two tournaments.** `--mode bots` (the default) ranks the bots: every seat
plays `--special`, so a bot's record measures the bot. `--mode houses` ranks
the houses: they play a round robin against each other and the bots are only
the instrument that plays them. Running both at once would rate a bot by the
houses it happened to draw, which measures neither — that is why they are
separate modes and not one mode with a flag.

`--special` takes a list of houses and every pair of them plays, `--games`
games per pair; `--opponent-special` adds challengers to that list. Inside a
pairing the house at seat 0 alternates every game and the bots every other
one, so over four games every house has met every bot from every seat.
`summary.json` then carries a `houses` table:

```json
"houses": {
  "zen":  {"games": 12, "wins": 7, "losses": 4, "draws": 1, "rate": 0.625, "elo": 1516.0},
  "dudes": {"games": 12, "wins": 3, "losses": 8, "draws": 1, "rate": 0.292, "elo": 1485.3}
}
```

and a `houseMatrix` with the score of every pair, both ways, printed as a
table with the houses in rows and columns:

```text
house matrix (row against column):
zen        wind       darksider
zen        -          2-0        1-1
wind       0-2        -          3-0
darksider  1-1        0-3        -
```

A game is credited to the house of the seat that won it, so the table reads
"how did this house do when it was played", and the Elo ranks them. The log
follows the same rule — it names the houses and the winning house, not the
bot that played it:

```text
game 1: zen vs wind -> zen in 23 turns
game 3: zen vs darksider -> darksider in 24 turns
```

With two bots the crossing needs four games per pairing to cancel the bot
difference as well as the seat: the house at seat 0 alternates every game,
the bots every other one. When both
seats play the same house the second one is labelled `zen2` (seat 2), and
`--include`/`--exclude` match on the house name either way. The same trick
splits a self-play of one bot into `diviner` and `diviner2`, which is the
first-player advantage — the only thing a mirror match can measure. Read the
rates as a smoke test: the arithmetic in "How many games a claim needs"
applies to them as well.

House keys are the game's own names, which are not always the module names:
`Troopers`, `Dudes` and `reaper` (Soul Reaper, also accepted as `soulreaper`).

**Short version of the next two subsections.** Elo is not a win rate: +70 Elo
is about 60% wins, +170 is about 73%. And a result from 20 games can be off by
±22 points, so use it only to catch big differences; for small ones, play
hundreds of games or compare `sims=` instead. The arithmetic follows for
whoever wants to check it.

### What the two numbers in `summary.json` mean

W/L/D is a proportion and needs no explanation. Elo is the standard online
rating, and it is worth ten seconds of arithmetic because the number is easy to
misread as a win rate.

```ocaml
ra' = ra +. (16.0 *. (sa -. (1.0 /. (1.0 +. (10. ** ((rb -. ra) /. 400.0))))))
```

`tournament.ml:166-167`, i.e. `R' = R + K·(S − E)` with `K = 16`,
`S ∈ {1, ½, 0}` and

```text
E = 1 / (1 + 10^(−Δ/400))        the expected score against this opponent
```

`E` is a **logistic expectation, not a win rate**, and the scale is deliberately
compressive:

| rating gap Δ | expected score `E` | linear guess `50% + Δ/8` |
|---|---|---|
| 0 | 0.500 | 50% |
| 25 | 0.536 | 53% |
| 100 | 0.640 | 62.5% |
| 200 | 0.760 | 75% |
| 400 | 0.909 | 100% |

A 400-point gap is the 10:1 point, hence the divisor: it makes "ten times better"
mean 400 rating points. Everything below that is squeezed, so **a large Elo
advantage reads as a smaller win rate than the same number suggests when you
eyeball it** — `summary.json` should never be quoted as a percentage.

Worked, from two fresh bots at 1500:

```text
game 1, A wins:  E = 0.5,  ΔR = 16·(1 − 0.5)  = +8   ->  A 1508, B 1492
game 2, draw:    E_A = 1/(1 + 10^(−16/400)) = 0.5229
                 ΔR_A = 16·(0.5 − 0.5229)   = −0.37 ->  A 1507.6, B 1492.4
```

Two things fall out of the second line. A draw costs the winner almost nothing —
the update is self-limiting — and the pair drifts towards the rating difference
that actually matches the observed score rate:

```text
ΔR_steady = 400 · log10(p / (1 − p))
```

so a bot winning **60%** of its games settles about **70 Elo** above its
opponent, not 200. Read backwards, a summary saying `+170` means an expected
score of `1/(1 + 10^(−170/400)) = 0.727`, not the 71% the linear reading
suggests.

### How many games a claim needs

A win rate from `N` games has `SE = √(p(1−p)/N)`. Comparing two runs of `N`
games each gives `SE_diff = √(2p(1−p)/N)`, so to call a difference `Δ` real at
95% confidence you need

```text
N  ≥  2 · 1.96² · p(1−p) / Δ²  =  1.92 / Δ²       at p = 0.5
```

| real difference | games **per arm** | Elo that corresponds |
|---|---|---|
| 10 points | ~192 | ~35 |
| 5 points | ~768 | ~17 |
| 2 points | ~4 800 | ~7 |

This is the arithmetic behind "twenty games at least". Twenty games at `p = 0.5`
have `SE = 0.112`, so the 95% band on the printed percentage is roughly
**50% ± 22%** — a smoke test, nothing more. Two hundred games resolves a
10-point change; you need eight hundred for a 5-point one. And because games
inside one run share bot pairs, decks and seat patterns, treat these as lower
bounds.

The practical consequence for retuning: a change worth less than about 20 Elo
cannot be measured by a quick tournament at all. Either run the games it takes,
or measure something cheaper and more sensitive — `sims=` per move, the number
of halving rounds completed at a fixed budget, or the leaf values in
`ORACLE_DEBUG` (document 4 §5). Those are the metrics to iterate on; the Elo
number is the last check, not the first.

The method, because bot-vs-bot results are noisy (the checklist to follow):

1. **Fix `--seed`.** The seed drives the deals and the dice. The same seed
   gives the same decks for both bots, which removes deck luck from the
   comparison. Seat order alternates per game (`ordered_pair`,
   `tournament.ml:635`).
2. **Use `--games`, not 1.** Twenty games at least. A 6-game sample routinely
   puts a genuine ±5% change inside the noise.
3. **Change one thing.** Especially: do not retune `Eval` and the controller in
   the same experiment.
4. **Check `sims=`, not just wins.** If a change makes the bot spend twice the
   simulations for the same result, it is a regression in disguise.
5. **Watch the budget curve.** Run at 100 ms and 1000 ms. A bot that wins big
   at 1000 ms and badly at 100 ms has a search that needs a full halving round
   to show anything — worth knowing before it shows up as a stall in the UI.

The bot test suites pin the properties that are easy to break:

```sh
dune test                       # everything
dune exec ./test/test_oracle.exe        # eval preferences + search sanity
dune exec ./test/test_diviner.exe       # risk aversion, world aggregation
dune exec ./test/test_lecturer.exe      # tree structure, budget, no mutation
dune exec ./test/heuris.exe             # race forecast, antisymmetry
```

`test/test_oracle.ml`'s `eval` group is the most useful of them: it asserts
*orderings* of `Oracle.leaf_value` (sprites hurt more than bulls, blocking the
titan is good, a wide board is scarier than a narrow one) rather than exact
numbers, so retuning the weights does not break it. Keep that style if you add
evaluation tests.

## 6. Making a bot stronger, weaker, or faster

**Stronger, same budget.** Buy rollouts: raise `horizon`, lower
`firstRoundSamples` so the first halving round is cheap and more rounds
complete. `rootCards` down from 8 to 4 halves the width and roughly doubles
the depth of the search for the same time.

**Faster.** `horizon` 5 → 3, `projectPlies` 4 → 2, `placementsPerCard` 2 → 1.
Each roughly halves the per-rollout cost; check the strength cost at a matched
budget before assuming it is free.

**Weaker on purpose.** Reduce `rootCards` to 1 — the search degenerates to
"screening value", which is a reasonable beginner. Or set `horizon=0`, which
leaves only the four passes of projection.

**A different opponent.** Change the base policy, not the evaluation.
`Oracle.base_moves` (`placements`, `oracle.ml:152-188`) is where the bot
decides what an opponent would plausibly do; making the greedy rollout
occasionally take the second-best move makes the bot more robust, and the
`reply_weights` change in document 4 does the same thing more cheaply.

## 7. Gotchas

- **`strategist` is stateful.** `Strategist.mover` allocates the bandit once
  and keeps it for the whole game (`strategist.ml:491-493`). Give each seat
  its own mover, which `Table.movers_of` does (`table.ml:179`).
- **`summoner` mutates its own settings.** `search_pass` temporarily rewrites
  `settings.duration` around the inner search and restores it in a
  `Fun.protect` (`summoner.ml:212-215`). Sharing a `settings` record between
  threads would be a bug.
- **`Tournament.mover`'s doc comment is stale.** It says `[apprentice]` is
  greedy; the code maps `"apprentice"` and `"gambler"` to `Lecturer`
  (`tournament.ml:641,645`), and `bot_ids` (`:650`) does not list them.
- **`Heuris.mana_lead` is exported and never called.**
- **Do not compare on a single seed.** See §5.
- **The board is visible to everything.** Before you conclude that a bot plays
  nonsensically against a hidden card, check whether it could see it. The two
  documented cases are in the README's *Limitations* and in
  [document 2](02-game-view.md) §1.

## Where to go next

- Adding a bot: the skeleton in [document 2](02-game-view.md) §6, plus the
  registration checklist.
- Making a bot understand a house better: the evaluation weights
  ([document 4](04-oracle.md) §7) and the tempo tables in
  `strategist.ml:148-210`.
- Making a bot handle hidden information properly: [document 5](05-diviner.md)
  §8, which lists what Diviner does not yet do.
- The underlying ideas: [document 3](03-search-concepts.md).

And the project docs next door: [migration status](../tech/migration.md),
[house port mismatches](../tech/house-port-mismatches.md),
[spell-effect port plan](../tech/effects-port-plan.md),
[packaging](../tech/packaging.md).