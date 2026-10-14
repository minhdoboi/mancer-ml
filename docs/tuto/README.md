# Tutorials

Eight documents about how `mancer-ml` is put together, how its AI
opponents think, how the spell effects are drawn, and how the lobby ocean is
synthesised. Read them in order the
first time; after that they are reference.

| # | File | What you get out of it |
|---|---|---|
| 1 | [01-architecture.md](01-architecture.md) | The code map: libraries, layers, the `desc`/`state` split, the update graph, and what happens during one move. |
| 2 | [02-game-view.md](02-game-view.md) | The four building blocks every bot shares: move generation, the simulator, leaf evaluation, and the search controller — with the `Heuris` race forecast derived term by term. |
| 3 | [03-search-concepts.md](03-search-concepts.md) · [fr](03-search-concepts.fr.md) | The algorithms, taught generically: leaf scores, rollouts, successive halving, UCT, POMDP beliefs, risk aversion, and how many samples a decision actually needs. |
| 4 | [04-oracle.md](04-oracle.md) · [fr](04-oracle.fr.md) | The Oracle bot, line by line: screen → shortlist → successive halving, with worked numbers. |
| 5 | [05-diviner.md](05-diviner.md) · [fr](05-diviner.fr.md) | The Diviner bot: belief worlds over the opponent's hidden deck, and why it plays differently from Oracle. |
| 6 | [06-bot-landscape.md](06-bot-landscape.md) · [fr](06-bot-landscape.fr.md) | All six bots side by side, the tuning knobs, the Elo and sampling arithmetic, and how to measure a change. |
| 7 | [07-visual-effects.md](07-visual-effects.md) · [fr](07-visual-effects.fr.md) | The effects layer: the timeline, and the maths behind the lightning fractal, the fire sheet, and the madness eye. |
| 8 | [08-ocean-sound.md](08-ocean-sound.md) · [fr](08-ocean-sound.fr.md) | The lobby ocean: value noise, swell and wind envelopes, the cutoff-following low-pass, and raylib's streaming buffers. |

## Conventions

- References are `path/file.ml:line`, clickable from most editors. Line numbers
  drift; if a pointer looks wrong, search for the quoted identifier instead.
- Everything quoted is real code from this repo, not pseudocode. The one
  exception is the arithmetic in the worked examples, which I did by hand and
  checked.
- Search bots speak in **plies** (one move by one side), **rollouts** (a short
  game played by a cheap policy), and **leaves** (the score at the end of a
  rollout). Document 3 defines the vocabulary properly.

## Run things yourself

Nothing below is theoretical; every example can be reproduced.

```sh
eval $(opam env --switch=mancer)
dune build

# Watch a search bot think. stats_log prints one line per move.
dune exec ./bin/main.exe -- --bots oracle,greedy --games 4 --budget 200 --seed 1

# Oracle's own screen and final candidate ranking, printed per move.
ORACLE_DEBUG=1 dune exec ./bin/main.exe -- --bots oracle,oracle --games 1 --budget 300

# Retune the bots from the shell.
ORACLE_OPTS=horizon=3,rootCards=4 DIVINER_OPTS=riskAversion=0.8 \
  dune exec ./bin/main.exe -- --bots oracle,diviner --games 4 --budget 200 --seed 1
```

Useful one-liners when you want the machine to answer a question instead of
you:

```sh
# How strong is the bot at this budget?
dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --budget 1000 --seed 1

# Which house is the bot bad at?  (--special picks it for both seats)
dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --special zen --seed 1

# Card-level win rates from a directory of finished runs.
dune exec ./bin/main.exe -- --mode summary --dir tournament-results/<timestamp>
```

## Where the code lives

The bots are six files in one directory, and they are short:

```text
lib/bots/lecturer.ml     UCT / MCTS, the only one with a real tree
lib/bots/oracle.ml       successive halving over rollouts   <- document 4
lib/bots/diviner.ml      Oracle inside sampled opponent decks <- document 5
lib/bots/summoner.ml     iterative-deepening best-first search
lib/bots/strategist.ml   Summoner's search + a UCB bandit over tempo plans
lib/bots/heuris.ml       leaf evaluation used by Summoner and Strategist
```

Anything in this folder that quotes a bot quotes one of those files.

The effects layer is seven files in `gui/`, split by technique rather than by
spell:

```text
gui/fx_core.ml         types, transform, timeline, noise, shaders, shakes, looks
gui/fx_lightning.ml    fractal bolts, hop schedule, colour styles   <- document 7
gui/fx_flames.ml       flame column, rolling fire sheet, embers      <- document 7
gui/fx_particles.ml    the generic emitter and the hover smoke
gui/fx_dedicated.ml    seventeen one-off choreographies
gui/fx_scenes.ml       madness, shockwave, confetti, victory
gui/fx_stage.ml        the table: card key -> effect
```

Press **F8** in a duel to cycle through `Fx.demo_names` and watch every effect
without playing the card that causes it.