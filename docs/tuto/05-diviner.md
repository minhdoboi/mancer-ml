# 5. The Diviner bot

`lib/bots/diviner.ml`, about 220 lines, plus the deck-guessing helpers it shares
with every other bot in `lib/bots/belief.ml`. Port of
`priv/sp/bot/diviner/DivinerBot.scala`.

> **The idea in one sentence:** run the Oracle search several times over
> several *guesses* about the opponent's hidden hand, and pick the move that is
> best on average **and safe in the worst guess**.

Diviner is under 300 lines because it is Oracle plus one idea: what happens when the
position contains information you do not have.

## 1. The problem

Oracle's search forks the state and plays the opponent's twenty cards — the one
thing the player in front of the bot genuinely cannot see. It uses a single
guessed deck (`Belief.guess`, the union of a few deals), so it optimises
against one merged picture of a huge space and reports the result as if it
were certain.

Diviner keeps the guesses apart instead. Real output from an Oracle vs Diviner
game, 100 ms per move:

```text
diviner-stats beliefs=3 spentMs leftover cmd=1> earth.hugespider at 4 score=0.115
diviner-stats beliefs=3 spentMs leftover cmd=1> Wraith at 2 score=0.370
diviner-stats beliefs=3 spentMs leftover cmd=1> air.cloud at 1 score=0.571
diviner-stats beliefs=3 spentMs leftover cmd=1> fire.orc at 0 score=0.737
```

`beliefs=3` is the default `belief_count`: three sampled deals, and never the
real deck (§3).

(`spentMs leftover` with no numbers is a small formatting bug in the stats
line: the format string has the literal words where the elapsed-time
placeholders should be. It costs nothing at runtime.)

## 2. Belief worlds

A **world** is a complete, self-consistent game state that differs only in the
guessed part. Concretely, a world is a `(game_state, game_desc)` pair in which
the opponent's `player_desc` has been swapped for a sampled legal deal, while
the board, the mana, the life totals, the bot's own hand and every house effect
stay exactly as they are.

```text
                    the real position
                          |
        +-----------------+------------------+
        |                 |                  |
     world 0          world 1    ...      world 2
   sampled deal     sampled deal          sampled deal
        |                 |                  |
   Oracle.Search      Oracle.Search        Oracle.Search
        |                 |                  |
        +--------+--------+---------+--------+
                 |   one score per world    |
                 +--------------------------+
                            |
                   mean minus risk penalty
```

Each world gets its own `Oracle.Search.t` — an independent Oracle engine over
a fixed `desc` (`Diviner.execute`):

```ocaml
let worlds =
  List.map (fun belief ->
      let st, d = Belief.with_belief start desc pid belief in
      { start = st; desc = d; ai = Oracle.Search.create ~desc:d ~bot_id:pid })
    beliefs
```

That is the whole reason `Oracle.Search` is a separate public module
(`module Search` in `oracle.mli`): it packages "one Oracle over one fixed desc" as a value,
so Diviner can have several running at once without any global state.

## 3. Sampling the beliefs

Sampling has three parts: remembering which opponent cards have been revealed,
drawing legal decks that contain them, and keeping those decks until something
new is learned. All of it except the cache lives in `Belief`, because every bot
now needs it. The difference is what each bot does with the deals:

| bot | deals per guess | how they are used |
|---|---|---|
| Summoner, Strategist, Lecturer, Oracle | `Belief.superset_samples` (4) | merged house by house into one oversized deck (`Belief.superset`), like Scala's `CardGuess.createAIPlayer` |
| Diviner | `beliefCount` (3) | kept apart, one world each |

Neither ever searches the opponent's real deck.

### Remembering what was seen

The game state keeps no history of what the opponent played, so the bot keeps
its own. `mover` calls `Belief.seen_cards`, which wraps `u.listener` once per
game and seat (`track_opponent`) and records every opponent card that appears in an
`Ev_summon`, `Ev_spell_played` or `Ev_submitted` event:

```ocaml
u.listener <-
  (fun e ->
    (match e with
    | Ev_summon { player; creature; _ } when player <> pid -> note creature.c_card
    | Ev_spell_played c when c.cmd_player <> pid -> note c.cmd_card
    | Ev_submitted { player; command = Some c; _ } when player <> pid -> note c.cmd_card
    | _ -> ());
    prev e)
```

Recording events (rather than reading the board) is what keeps a creature that
has since died, or a spell that has resolved, in the set. `seen_cards` also adds
whatever is on the opponent's board right now, as a fallback. There is one
tracker per seat, so when two bots share a game each one records its own
opponent; all trackers are dropped when the updater changes, i.e. for each new
game.

`required_indexes` then turns those cards into per-house 1-based card indexes
for the opponent's deck. Cards that are not part of it — summoned tokens, for
instance — match no house and drop out.

### Drawing consistent decks

```ocaml
let sample ~desc ~bot_id ~seen ~n rng =
  let bot = desc.gd_players.(bot_id) in
  let opp = desc.gd_players.(other bot_id) in
  let special = opp.pd_houses.(4).phd_house in
  let require = required_indexes opp seen in
  let deal require =
    try Some (Shuffle.create_player ~rng ~exclude:bot ~require ~first:false ~special Catalog.basics)
    with Failure _ -> None
  in
  ...
```

Five decisions, each doing something important:

**Rejection sampling from the real deck builder.** Rather than inventing hands,
Diviner calls `Shuffle.create_player` — the same function that built the game
in the first place — and catches its `Failure` when it cannot build a legal
deck. Every world is therefore a **legal** deck: four cards from each basic
house plus four from the special, satisfying the mana-generator, finisher and
defence-count rules in `lib/game/shuffle.ml`.

**`~require`: seen cards are forced in.** `Shuffle.create_player` takes an
optional `require`, one list of card indexes per house (fire, water, air,
earth, special). Basic-house candidates and special-house picks that do not
contain all of them are filtered out before the search, so every sampled deck
holds every card the opponent has shown. If the seen cards cannot form a legal
deck at all, `Belief.sample` falls back to unconstrained sampling rather than
returning no worlds.

**The opponent's actual special house.** The bot reads it straight out of the
desc (`opp.pd_houses.(4)`) — it is genuinely public information, chosen in the
lobby. That is what makes the belief space finite enough to sample. (The
`~special` in that call is an OCaml pun: `~special:special`, with
`Catalog.basics` as the trailing `house list` argument.)

**`~exclude:bot`.** Card indexes the bot itself holds are removed from the
pool, so no world contains a card that is in both hands.

**Distinctness, and never the real deck.** Duplicates are detected by the
sorted list of card ids in the deal and resampled. The real opponent desc is
*not* a world, as in Scala: the worlds are exactly the sampled deals, so the
stats line says `beliefs=3` with the default `belief_count = 3`. (Earlier
versions of the port prepended the real deck as `opp :: sampled`, which made
Diviner peek; it was removed.) The `primary` world, which supplies the legal
move list, is the first sampled deck. Only if no legal deck can be dealt at all
does `execute` fall back to the real one.

### Keeping the worlds

Sampling again on every move would make the opponent's imagined hand jump
around between turns. `Diviner.beliefs` caches the deals per game desc, seat
and seen-card set (`Belief.seen_key`), as Scala's `cachedBeliefs` does:

```ocaml
let beliefs ~desc ~pid ~seen rng =
  let key = Belief.seen_key seen in
  match List.find_opt (fun (d, p, k, _) -> d == desc && p = pid && k = key) !cached with
  | Some (_, _, _, b) -> b
  | None -> (* Belief.sample, store, return *)
```

The worlds change only when the opponent reveals a new card. `Belief.guess`
caches the single-deck bots' superset the same way.

## 4. Grafting a world

```ocaml
let with_belief visible desc bot_id belief =
  let opp = other bot_id in
  let p = visible.players.(opp) in
  let players = Array.copy visible.players in
  players.(opp) <- { p with ps_desc = { init_desc = belief; desc_mods = p.ps_desc.desc_mods } };
  let gd = { gd_players = Array.copy desc.gd_players } in
  gd.gd_players.(opp) <- belief;
  ({ players }, gd)
```

Two records must change together, and this is worth understanding rather than
copying:

- the **state** half, so `Game.legal_commands` and `Game.simulate` enumerate the
  guessed cards when it is the opponent's turn;
- the **desc** half, so `Game.player_houses` resolves the guessed cards'
  costs. The desc is otherwise fixed for the whole game — a belief world is the
  one place where it changes.

Everything else is untouched, including `desc_mods` (the live cost modifiers
accumulated during the real game) and the whole board. The bot's own hand is
never guessed.

Two honest limits of this approach:

- **The board is never guessed.** Diviner shares every bot's blind spot on the board:
  an unrevealed Shade Stalker is still in `ps_slots` with its attack and life.
- **Beliefs are kept until a card is revealed, and constrained.** `execute`
  reuses the same samples while the seen cards stay the same and calls
  `Belief.sample` again when a new card shows up, with no weighting carried
  over. Every sample is forced to contain the opponent cards seen so far (§3).
  A world can no longer contradict a card the opponent has played; it can still be
  inconsistent in subtler ways (a card they chose *not* to play carries no
  information here). Full particle filtering — weight worlds by how likely
  the observed play was, carry them across turns — is the next improvement.

(A small inefficiency, harmless: `create_player` also computes the deal's
opening mana, which `Belief.sample` discards. The `first:false` argument is
therefore irrelevant.)

## 5. Searching the worlds

The controller is Oracle's, unchanged. What differs is the axis.

**Screening runs per world** (the `screen` loop in `Diviner.search`):

```ocaml
List.iteri (fun w world ->
    if now_ms () < deadline then (
      let step = Oracle.Search.play world.ai world.start bot_id cmd in
      if step.ended <> Some bot_id then all_won := false;
      values.(w) <- Oracle.Search.value world.ai step ~ply:1;
      seen.(w) <- true))
  worlds;
if !all_won && Array.for_all Fun.id seen then rows := [ `Lethal cmd ]
```

The same root move is played once **in each world**, so a screen value is a
vector, not a scalar. Two consequences:

- **Lethality must hold in every world** — `all_won` and `Array.for_all Fun.id
  seen`. Winning in three guesses out of four is not a win; it is a gamble,
  and the risk penalty below will bury it.
- **The budget can expire mid-command.** The deadline is checked inside the
  world loop, so `screened.(w)` is a per-world flag and a command may be
  scored in two worlds and blind in the third. `world_mean_of` handles all
  three cases (§6).

**Shortlisting and halving** are Oracle's verbatim, including
`Oracle.Params.root_cards`. The inner loop simply iterates worlds
(the halving loop at the end of `Diviner.search`):

```ocaml
List.iteri (fun w world ->
    List.iter (fun c ->
      if !time_left then (
        let rank = c.cnt.(w) mod Array.length !Oracle.Params.reply_weights in
        add c w (Oracle.Search.playout world.ai ~start:world.start ~root:c.cmd ~reply_rank:rank);
        time_left := now_ms () < deadline))
      !survivors)
  worlds
```

One sample means *one rollout in one world*, so the per-candidate accounting is
a matrix (`sum`, `cnt`, `screen`, `screened` — all arrays indexed by world)
rather than Oracle's scalar-plus-three-ranks. The reply rank cycles
independently per world, so the same candidate can be probed with the
opponent's best reply in world 0 and its third-best in world 1.

**Cost.** With `W` worlds, one sample of one candidate costs `W` rollouts.
Against the same 200 ms budget, Diviner therefore completes roughly a quarter
as many halving rounds as Oracle and ends up comparing fewer candidates. That
is the entire trade: the same budget buys either confidence in one world or
breadth across several.

## 6. Aggregating worlds

Per-world mean, three cases (`world_mean_of`):

```ocaml
let world_mean_of c w =
  if c.cnt.(w) <= 0 then c.screen.(w)                                   (* never rolled out *)
  else if not c.screened.(w) then c.sum.(w) /. float_of_int c.cnt.(w)   (* blind: playouts only *)
  else Eval.world_mean ~screen:c.screen.(w) ~sum:c.sum.(w) ~n:c.cnt.(w) (* both: blend them *)
```

and `Eval.world_mean` is Oracle's screen/playout blend, unchanged:

```ocaml
let world_mean ~screen ~sum ~n =
  if n <= 0 then screen else ((!Oracle.Params.screen_weight *. screen) +. (sum /. float_of_int n))
                              /. (!Oracle.Params.screen_weight +. 1.)
```

Then the risk-aware score (`Eval.score`):

```ocaml
let score ?(aversion = !Params.risk_aversion) means =
  match means with
  | [] -> neg_infinity
  | xs ->
      let n = float_of_int (List.length xs) in
      let mean = List.fold_left ( +. ) 0. xs /. n in
      mean -. (aversion *. (mean -. List.fold_left min infinity xs))
```

**mean − aversion × (mean − worst world).** The penalty is proportional to the
spread and vanishes when every world agrees, so worlds that tell the same story
cost nothing to add.

### Worked examples (aversion 0.35)

| world means | mean | worst | score |
|---|---|---|---|
| `[0.9; −1.0]` | −0.05 | −1.0 | −0.05 − 0.35 × 0.95 = **−0.383** |
| `[0.2; 0.2]` | 0.2 | 0.2 | 0.2 − 0 = **0.200** |
| `[0.5; 0.5; −1.0]` | 0.0 | −1.0 | 0 − 0.35 × 1.0 = **−0.350** |
| `[0.5; 0.5; 0.5]` | 0.5 | 0.5 | **0.500** |

The first two rows are the test `eval_prefers_stable` in `test/test_diviner.ml`: a move that is
excellent in one guess and disastrous in another loses to a mediocre move that
is mediocre everywhere. That is the whole point — the third world might be
the real one, and a move that relies on the opponent holding exactly one thing
is not a move, it is a guess.

Note the interaction between `belief_count` and `risk_aversion`. One bad world
out of four is diluted:

```text
[0.9; 0.9; 0.9; -1.0]   mean 0.675  worst -1.0   score 0.675 - 0.35*1.675 = 0.089
[0.9; -1.0]              mean -0.05  worst -1.0   score -0.383
```

Same disagreement, different worlds, four times the penalty. More worlds make
Diviner more robust *and* more tolerant of any single scary guess; raising
`riskAversion` while raising `beliefCount` partly cancels itself.

And `riskAversion = 0` collapses the whole thing back to plain expectation —
which is Oracle with wasted work. `1.0` is pure minimax over worlds, far too
timid to play. The default `0.35` is the compromise.

## 7. Tuning

```sh
DIVINER_OPTS=riskAversion=0.7,beliefCount=5 \
  dune exec ./bin/main.exe -- --bots greedy,diviner --games 20 --budget 500 --seed 1
```

| key | default | effect |
|---|---|---|
| `beliefCount` | 3 | sampled deals; one world each, never the real deck |
| `riskAversion` | 0.35 | weight of the penalty toward the worst world |
| `firstRoundSamples` | 1 | rollouts per candidate per world in round 1 (Oracle uses 6, because a Diviner sample costs `W` rollouts) |

Diviner also inherits `ORACLE_OPTS` for `rootCards`, `replyWeights` and
`screenWeight` — they are read out of `Oracle.Params` at run time, so setting `ORACLE_OPTS` changes Diviner's
behaviour. `horizon`, `projectPlies` and every evaluation weight apply to Diviner
as well, through `Oracle.Search` and `Oracle.Eval`.

Experiments worth running:

```sh
# one merged guess vs several separate ones  (Oracle vs Diviner, same budget)
dune exec ./bin/main.exe -- --bots oracle,diviner --games 20 --budget 500 --seed 1

# timid vs trusting
DIVINER_OPTS=riskAversion=0.0 dune exec ./bin/main.exe -- --bots greedy,diviner --games 20 --budget 500 --seed 1
DIVINER_OPTS=riskAversion=1.0 dune exec ./bin/main.exe -- --bots greedy,diviner --games 20 --budget 500 --seed 1

# more worlds at a bigger budget
DIVINER_OPTS=beliefCount=7 dune exec ./bin/main.exe -- --bots greedy,diviner --games 10 --budget 2000 --seed 1
```

There is no `DIVINER_DEBUG`. To watch its reasoning, turn on Oracle's stats
from the outside instead:

```sh
ORACLE_DEBUG=1 dune exec ./bin/main.exe -- --bots diviner,greedy --games 1 --budget 100
```

`ORACLE_DEBUG` is read inside `Oracle.execute`, which Diviner does not call, so
this prints nothing. To instrument Diviner, print from `cand_score` — it is one
function and it sees the full per-world matrix.

## 8. Limits

- **The board is still real.** Beliefs cover the opponent's hand only. The
  hidden Stalker blind spot is shared by every bot.
- **Only revealed cards update beliefs.** Seen cards are forced into every
  world, but nothing is inferred from what the opponent held back or from how
  they played. The seen-card tracker also needs `u.listener` to stay wrapped: a
  caller that replaces it mid-game loses the event history and falls back to
  the cards on the board.
- **Uniform prior over legal decks.** `Shuffle` encodes legality and a few
  house-specific rules, not how *players* build decks. A player who always
  takes the strongest creature would be badly mis-sampled.
- **A bad sample sticks.** Because worlds are cached until a card is revealed,
  an unlucky set of deals lasts for several turns when the opponent plays
  cards the bot has already seen. Raising `beliefCount` is the remedy.
- **Cost is linear in worlds.** Doubling `beliefCount` halves the number of
  halving rounds completed in the same budget. Whether that is a win depends on
  how much the position actually turns on hidden cards.
- **It is a bot, not a study.** Whether separate worlds beat one merged guess
  is worth measuring rather than assuming — that is what the Oracle-vs-Diviner
  matchup above is for.

Next: [06-bot-landscape.md](06-bot-landscape.md).