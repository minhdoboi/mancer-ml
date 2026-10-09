# mancer-ml — OCaml prototype

Feasibility prototype for porting `freespectrogdx` (Scala 3 + libGDX) to OCaml.

**Status:** the game engine, the four basic houses, Troopers, and the deck
builder are ported. `test/` covers house mechanics and shuffle constraints.
`bin/main.ml` deals two legal decks and plays them headlessly.

```sh
eval $(opam env --switch=mancer)   # OCaml 5.2.0 + dune 3.24.2
dune build
dune test                          # Alcotest suites + expect tests
dune promote                       # accept a reviewed expect-test diff
dune exec ./bin/main.exe           # 20 greedy-vs-greedy games; writes tournament-results/<ts>/
dune exec ./bin/main.exe -- -v     # first game with an event trace
dune exec ./bin/main.exe -- --bots greedy,oracle --games 4 --budget 200 --seed 1
dune exec ./bin/main.exe -- --mode summary --dir tournament-results/<ts>
```

## What is ported

| Scala | OCaml |
|---|---|
| `State.scala` | `lib/sp.ml` |
| `CardSpec.scala` | `lib/sp.ml` |
| `GameCardEffect.scala` | `lib/engine.ml` (effect combinators) |
| `update/{SlotUpdate,SlotsUpdate,PlayerUpdate,GameUpdate}.scala` | `lib/engine.ml` |
| `house/Common.scala`, `house/package.scala` (shared helpers) | `lib/common.ml` |
| `house/Fire.scala` | `lib/fire.ml` |
| `house/Water.scala` | `lib/water.ml` |
| `house/Air.scala` | `lib/air.ml` |
| `house/Earth.scala` | `lib/earth.ml` |
| `house/Troopers.scala` | `lib/troopers.ml` |
| `CardShuffle.scala` | `lib/shuffle.ml` |
| `Tournament.scala` (HeadlessGame) | `lib/game.ml` |
| `GameRecord.scala` | `lib/game_record.ml` |
| `Tournament.scala` (CLI, Elo, JSON logs) | `lib/tournament.ml` |
| `bot/oracle/OracleBot.scala` | `lib/oracle.ml` |
| `bot/lecturer/LecturerBot.scala` | `lib/lecturer.ml` |

## How the Scala translated

**The card DSL was the easy part, exactly as expected.** Scala's
`type Effect = Env ⇒ Unit` is `env -> unit`, and `effects(Direct -> f, OnTurn -> g)`
became a ~10-line builder over a 4-variant phase:

```ocaml
creature ~key:"fire.blargl" ~attack:(attack 8) ~life:26
  ~effects:(effects [ Direct, mass_damage ~is_ability:true ~immune_self:true 4 ]) ()
```

versus the Scala:

```scala
new Creature("fire.blargl", Attack(8), 26, I18n("fire.blargl.description"),
  effects = effects(Direct -> massDamage(4, isAbility = true, immuneSelf = true)))
```

The OCaml card list is ~60 lines for 12 cards and reads at the same level. No
translation friction here at all.

**`Reaction` was the interesting one.** Scala uses `class Reaction extends Actions`
with self-typed inheritance; 44 house files subclass it. OCaml has no equivalent
(mutually recursive modules need explicit signatures here, which is not worth it),
so reactions became a record of function fields with defaults:

```ocaml
type reaction = {
  selected : slot_update option ref;
  inflict  : slot_update -> damage -> unit;
  self_protect : damage -> damage;
  on_my_death : dead -> unit;
  ...
}
```

Houses override single fields with `{ r with on_my_damage = ... }`. `AttackBonusReaction`
(a 2-method abstract class) became a closure factory:

```ocaml
~new_reaction:
  (attack_bonus_reaction
     ~cond:(fun pos num -> abs (pos - num) = 1)
     ~get_bonus:(fun pos -> Attack_bonus { tag = "orc"; pos; bonus = (fun a -> a + 2) }))
```

This is the part I flagged as "a design pass over 44 houses, not a translation",
and that held up. It is per-house work, but each is a small local rewrite, not a
rewrite of the layer the rules sit on.

**The cyclic graph cost nothing**, as predicted. `updater → player → slots → slot
→ slots` builds with one `let rec p = { slots_update = { player = p; ... } }`,
because OCaml allows a record literal to refer to the value being defined.
(`slots_update.slot_updates` is `mutable` only to fill the slot array after; no
`Rc<RefCell<_>>`, no `Obj.magic` in the final code.)

**Two simplifications the port absorbed for free:**

- `FieldUpdate`'s tid/dirty/valuedirty cache exists to make `value` a lazily
  rebuilt view of the parent. Mutating in place removes the need for it.
  `PlayerUpdate.dirtyElementals` disappears too: attack sources are re-folded on
  every read, so `ManaAttack` and attack bonuses are always current. That deleted
  roughly 80 lines of cache-invalidation bookkeeping *and* the class of bug where
  a stale flag makes an elemental attack wrong.
- `AnyRef data` (37 nulls across the tree) became `slot_data`/`player_data` sum
  types. Fire doesn't need them, but every other house that does will have to
  enumerate its cases instead of punting on null.

## Two real bugs the port surfaced

Both were found by the mechanics test, and both are the kind that only show up
after 200 games in Scala:

1. **`Player.mod_damage` did not terminate.** Scala's `PlayerUpdate.mod` folds
   over the *attacker's* slots starting from `d`; a faithful-looking translation
   that recursed `otherPlayer mod d` as the fold seed looped forever. `SpellMod`
   scaling is now correct and tested (dragon 10 → 15).

2. **`Reaction.default`'s `inflict` recursed into itself.** Scala's `Actions.inflict`
   calls `damageSlot`; routing it back through `SlotUpdate.inflict` meant the
   default reaction called itself. Only surfaced as a hang, not a wrong number.

Both argue for keeping the Scala Elo harness as the oracle rather than trusting
the port by inspection.

## Code layout

`lib/` is the wrapped library `Mancer`. `engine.mli`, `common.mli`, the house
interfaces (`fire.mli`, `water.mli`, `air.mli`, `earth.mli`, `troopers.mli`)
and `game.mli` are the public surface; `sp.ml` is types only and has no
interface because all of it is meant to be public.

`engine.ml` is a sequence of modules in dependency order (`Houses`, `Slot`,
`Slots`, `Player`, `Reaction`, `Updater`) followed by the card effect
combinators, so there is no large `let rec` group. House code reads like the
Scala: `Slot.inflict s d`, `Player.inflict op d`, `Slots.summon su num c`.

Record fields that would clash across types carry a short prefix (`b_` creature
body, `cd_` card desc, `h_` house, `ps_` player state, `cmd_` command, `env_`
env, `hl_` house listener), so literals never need a type annotation.

Format with `dune fmt`.

Tests: `test/mechanics.ml` holds the Alcotest suites, `test/expect/` holds
ppx_expect board snapshots and a short game trace, and `test/support/` the
fixtures both share. A rules change shows up as an expect diff: review it, then
`dune promote`.

## What is not ported

- **39 of 44 houses.** Fire, Water, Air, Earth and Troopers are in; the other
  specials are not.
- **Most bots.** Summoner, Strategist, Gambler and Diviner are not
  ported. The greedy bot in `game.ml` picks the cheapest legal card into the
  frontmost open slot. Oracle (`lib/oracle.ml`) is the sequential-halving
  rollout bot from Scala; it searches with perfect information of both decks
  (Scala's Choco card-guesser is not ported). Lecturer (`lib/lecturer.ml`) is
  the UCT tree search, reworked: per-player (negamax) node values, Oracle's
  eval as expansion priors and leaf score, depth 4 instead of 2. Tune with
  `LECTURER_OPTS=exploration=0.7,maxDepth=4,...`.
- **Networking, campaign, rendering, i18n.**

## Reading the numbers

`bin/main.exe` is the Scala tournament driver: it shuffles legal decks, plays
the requested bots, and writes `game-NNN.json` plus `summary.json` (W/L/D and
Elo, K=16) under `tournament-results/<timestamp>/`. Default is greedy vs
greedy. `--bots greedy,oracle,lecturer` is the interesting matchup; `--budget` is the
search bots' milliseconds per move (default 1000, same as Scala).

The greedy bot always takes the cheapest legal card, so greedy-vs-greedy win
counts are not a balance signal. Oracle vs greedy is.

Tests that need a fixed hand still use `Game.make_player_desc`, which keeps the
old cost-slice (and one special card per quarter).