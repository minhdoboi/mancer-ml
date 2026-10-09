# Migration from freespectrogdx

Status and design notes for the OCaml port of `freespectrogdx` (Scala 3 +
libGDX). House-by-house diffs live in
[house-port-mismatches.md](house-port-mismatches.md). The GUI spell-effect
plan is [effects-port-plan.md](effects-port-plan.md).

## Status

Ported:

- Game engine (`lib/core/`)
- All 42 houses (`lib/houses/`)
- Deck shuffle constraints (`lib/game/shuffle.ml`)
- Session, tournament driver, replay, campaign
- Bots: greedy, Summoner, Strategist, Lecturer, Oracle, Diviner
- Terminal UI (`bin/tui.ml`) and raylib UI (`bin/gui.ml`)

Not ported:

- **Gambler.** Campaign and duel faces for it still play Lecturer.
- **Oracle card-guesser.** Scala's Choco guesser is absent. Diviner samples
  opponent decks with `Shuffle` instead. Oracle searches with perfect
  information of both decks.
- **Networking.**

House mechanics are meant to match Scala. Remaining intentional or UI-only
differences are listed in [house-port-mismatches.md](house-port-mismatches.md).
`compare_engines` is the check for new house work: it replays one recorded
game in both engines and reports the first turn the boards differ.

## Scala → OCaml

| Scala | OCaml |
|---|---|
| `State.scala` | `lib/core/sp.ml` |
| `CardSpec.scala` | `lib/core/sp.ml` |
| `GameCardEffect.scala` | `lib/core/engine.ml` (effect combinators) |
| `update/{SlotUpdate,SlotsUpdate,PlayerUpdate,GameUpdate}.scala` | `lib/core/engine.ml` |
| `house/Common.scala`, `house/package.scala` (shared helpers) | `lib/core/common.ml` |
| `house/Fire.scala` | `lib/houses/basic/fire.ml` |
| `house/Water.scala` | `lib/houses/basic/water.ml` |
| `house/Air.scala` | `lib/houses/basic/air.ml` |
| `house/Earth.scala` | `lib/houses/basic/earth.ml` |
| `house/Troopers.scala` | `lib/houses/special/troopers.ml` |
| `house/Zen.scala` | `lib/houses/special/zen.ml` |
| `house/MasterOfWind.scala` | `lib/houses/special/wind.ml` |
| `house/Chronomaster.scala` | `lib/houses/special/chronomaster.ml` |
| `house/LostChurch.scala` | `lib/houses/special/disciples.ml` |
| `house/DarkSider.scala` | `lib/houses/special/darksider.ml` |
| `house/FaerieDruid.scala` | `lib/houses/special/faerie.ml` |
| `house/FairyKnight.scala` | `lib/houses/special/fairy.ml` |
| `house/DudeMancer.scala` | `lib/houses/special/dudes.ml` |
| `house/Vampire.scala` | `lib/houses/special/vampires.ml` |
| `house/Warp.scala` | `lib/houses/special/warp.ml` |
| `house/SB.scala` | `lib/houses/special/snowblood.ml` |
| `house/Dreamweaver.scala` | `lib/houses/special/dreamweaver.ml` |
| `house/JunkMage.scala` | `lib/houses/special/junk.ml` |
| `house/Battlemage.scala` | `lib/houses/special/battlemage.ml` |
| `house/Antimancer.scala` | `lib/houses/special/antimancer.ml` |
| `house/Powermancer.scala` | `lib/houses/special/powermancer.ml` |
| `house/Shadowmage.scala` | `lib/houses/special/shadowmage.ml` |
| `house/Colors.scala` | `lib/houses/special/colors.ml` |
| `house/Limbo.scala` | `lib/houses/special/limbo.ml` |
| `house/Rogue.scala` | `lib/houses/special/rogue.ml` |
| `house/Tinker.scala` | `lib/houses/special/tinker.ml` |
| `house/Entomologist.scala` | `lib/houses/special/entomologist.ml` |
| `house/Soulbinder.scala` | `lib/houses/special/soulbinder.ml` |
| `house/Shadowcaster.scala` | `lib/houses/special/shadowcaster.ml` |
| `house/SpiritShaman.scala` | `lib/houses/special/spiritshaman.ml` |
| `house/Conjurer.scala` | `lib/houses/special/conjurer.ml` |
| `house/Quantumancer.scala` | `lib/houses/special/quantumancer.ml` |
| `house/Meteomancer.scala` | `lib/houses/special/meteomancer.ml` |
| `house/Seasons.scala` | `lib/houses/special/seasons.ml` |
| `house/SoulReaper.scala` | `lib/houses/special/soulreaper.ml` |
| `house/Sower.scala` | `lib/houses/special/sower.ml` |
| `house/Shaman.scala` | `lib/houses/special/shaman.ml` |
| `house/MountainKing.scala` | `lib/houses/special/mountainking.ml` |
| `house/DarkPriest.scala` | `lib/houses/special/darkpriest.ml` |
| `house/Elementalist.scala` | `lib/houses/special/elementalist.ml` |
| `house/HighPriest.scala` | `lib/houses/special/highpriest.ml` |
| `house/Kinetician.scala` | `lib/houses/special/kinetician.ml` |
| `CardShuffle.scala` | `lib/game/shuffle.ml` |
| `Tournament.scala` (HeadlessGame) | `lib/game/session.ml` (turn loop), `lib/game/game.ml` (setup, bots) |
| `update/GameUpdate.scala` (UpdateListener) | `Sp.game_event`, `Updater.listen` |
| `GameRecord.scala` | `lib/game/game_record.ml` |
| `Replay.scala` | `lib/game/replay.ml` |
| `Tournament.scala` (CLI, Elo, JSON logs) | `lib/game/tournament.ml` |
| `bot/oracle/OracleBot.scala` | `lib/bots/oracle.ml` |
| `bot/lecturer/LecturerBot.scala` | `lib/bots/lecturer.ml` |

## How the Scala translated

**The card DSL was the easy part.** Scala's `type Effect = Env ⇒ Unit` is
`env -> unit`, and `effects(Direct -> f, OnTurn -> g)` became a short builder
over a 4-variant phase:

```ocaml
creature ~key:"fire.blargl" ~attack:(attack 8) ~life:26
  ~effects:(effects [ Direct, mass_damage ~is_ability:true ~immune_self:true 4 ]) ()
```

versus the Scala:

```scala
new Creature("fire.blargl", Attack(8), 26, I18n("fire.blargl.description"),
  effects = effects(Direct -> massDamage(4, isAbility = true, immuneSelf = true)))
```

The OCaml card list is about 60 lines for 12 cards and reads at the same
level.

**`Reaction` was the interesting one.** Scala uses `class Reaction extends
Actions` with self-typed inheritance; 44 house files subclass it. OCaml has
no equivalent (mutually recursive modules need explicit signatures here,
which is not worth it), so reactions became a record of function fields with
defaults:

```ocaml
type reaction = {
  selected : slot_update option ref;
  inflict  : slot_update -> damage -> unit;
  self_protect : damage -> damage;
  on_my_death : dead -> unit;
  ...
}
```

Houses override single fields with `{ r with on_my_damage = ... }`.
`AttackBonusReaction` (a 2-method abstract class) became a closure factory:

```ocaml
~new_reaction:
  (attack_bonus_reaction
     ~cond:(fun pos num -> abs (pos - num) = 1)
     ~get_bonus:(fun pos -> Attack_bonus { tag = "orc"; pos; bonus = (fun a -> a + 2) }))
```

This is per-house work, but each is a small local rewrite, not a rewrite of
the layer the rules sit on.

**The cyclic graph cost nothing.** `updater → player → slots → slot → slots`
builds with one `let rec p = { slots_update = { player = p; ... } }`, because
OCaml allows a record literal to refer to the value being defined.
(`slots_update.slot_updates` is `mutable` only to fill the slot array after;
no `Rc<RefCell<_>>`, no `Obj.magic` in the final code.)

**Two simplifications the port absorbed:**

- `FieldUpdate`'s tid/dirty/valuedirty cache exists to make `value` a lazily
  rebuilt view of the parent. Mutating in place removes the need for it.
  `PlayerUpdate.dirtyElementals` disappears too: attack sources are re-folded
  on every read, so `ManaAttack` and attack bonuses are always current. That
  deleted roughly 80 lines of cache-invalidation bookkeeping and the class of
  bug where a stale flag makes an elemental attack wrong.
- `AnyRef data` (37 nulls across the tree) became `slot_data`/`player_data`
  sum types. Every house that needs them enumerates its cases instead of
  punting on null.

## Bugs the port surfaced

Both were found by the mechanics test, and both are the kind that only show
up after many games in Scala:

1. **`Player.mod_damage` did not terminate.** Scala's `PlayerUpdate.mod`
   folds over the *attacker's* slots starting from `d`; a faithful-looking
   translation that recursed `otherPlayer mod d` as the fold seed looped
   forever. `SpellMod` scaling is now correct and tested (dragon 10 → 15).

2. **`Reaction.default`'s `inflict` recursed into itself.** Scala's
   `Actions.inflict` calls `damageSlot`; routing it back through
   `SlotUpdate.inflict` meant the default reaction called itself. Only
   surfaced as a hang, not a wrong number.

Both argue for keeping the Scala Elo harness as the oracle rather than
trusting the port by inspection.

## Cross-engine harness

Hand-written house tests missed real bugs. `compare_engines` plays one
recorded game in both engines and reports the first turn where the boards
differ.

Game JSON carries opening `mana` and `rngSeed`. In-game dice use a
`java.util.Random` LCG (`lib/core/dice.ml`, Scala `GameStateUpdater.rng`) so
both sides draw the same integers. Shuffle deals stay on OCaml
`Random.State`.

```sh
# generate a greedy-vs-greedy game, replay it in OCaml and Scala
dune exec ./bin/compare_engines.exe -- --seed 1 --special zen --turns 80

# replay an existing tournament file (needs mana + rngSeed)
dune exec ./bin/compare_engines.exe -- --game tournament-results/<ts>/game-001.json

# keep the JSON and both snapshot dumps
dune exec ./bin/compare_engines.exe -- --seed 1 --special disciples --keep /tmp/mancer-diff

# Scala replay only (from the freespectrogdx repo)
./gradlew :core:replay -Ptargs="--in /tmp/game.json --out /tmp/scala.json"
```

`--scala` defaults to `../freespectrogdx` (or `$FREESPECTROGDX`). New houses
should pass this before they rely on tests written from reading the Scala.
