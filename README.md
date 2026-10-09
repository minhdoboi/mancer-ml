# mancer-ml — OCaml prototype

Feasibility prototype for porting `freespectrogdx` (Scala 3 + libGDX) to OCaml.

**Status:** the game engine, all 42 houses, and the deck builder are ported.
`test/` covers house mechanics and shuffle constraints.
`bin/main.ml` deals two legal decks and plays them headlessly.

```sh
eval $(opam env --switch=mancer)   # OCaml 5.2.0 + dune 3.24.2
opam install notty raylib          # terminal UI, raylib UI
dune build
dune test                          # Alcotest suites + expect tests
dune promote                       # accept a reviewed expect-test diff
dune exec ./bin/main.exe           # 20 greedy-vs-greedy games; writes tournament-results/<ts>/
dune exec ./bin/main.exe -- -v     # first game with an event trace
dune exec ./bin/main.exe -- --bots greedy,oracle --games 4 --budget 200 --seed 1
dune exec ./bin/main.exe -- --special zen --games 4 --seed 1
dune exec ./bin/main.exe -- --mode summary --dir tournament-results/<ts>
dune exec ./bin/compare_engines.exe -- --seed 1 --special zen --turns 80
dune exec ./bin/tui.exe            # play or watch a game in the terminal
dune exec ./bin/gui.exe            # the same in a raylib window, with card art
dune exec ./bin/gui.exe -- --shot out.png --bots --seed 1 --wait 8   # screenshot, then exit
dune exec ./bin/sync_assets.exe    # rebuild ../mancer-assets from ../freespectrogdx
```

## What is ported

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

This is the part I flagged as "a design pass over the houses, not a translation",
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

`session.mli` is the API for driving a live game, from a UI or a bot loop:
`Session.create`, `status`, `legal_commands`, `submit`, plus an `on_event`
listener that receives every `Sp.game_event`. The engine events mirror Scala's
`UpdateListener` (focus, move, swap, run slot, summon, die, replaced, refresh,
spell played, ability); the session adds to-move, submitted and game-over.
Listeners run mid-step, so they read the board with `Session.peek`, which does
not flush pending deaths. `Session.play_game` is the headless bot-vs-bot loop
built on it.

`table.mli` sits on top of the session for front ends: two seats (human or a bot
id), seeded deals, bot dispatch, the legal targets for each card, log lines and the
game record. `assets.mli` finds card text and art in the assets checkout. `bin/tui.ml`
([notty](https://github.com/pqwy/notty)) and `bin/gui.ml` (raylib) are front ends
on both and only differ in drawing and input.

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

- **All 42 houses** are in. Tournament `--special` selects one of them
  (default Troopers).
- **Most bots.** Summoner, Strategist, Gambler and Diviner are not
  ported. The greedy bot in `game.ml` picks the cheapest legal card into the
  frontmost open slot. Oracle (`lib/bots/oracle.ml`) is the sequential-halving
  rollout bot from Scala; it searches with perfect information of both decks
  (Scala's Choco card-guesser is not ported). Lecturer (`lib/bots/lecturer.ml`) is
  the UCT tree search, reworked: per-player (negamax) node values, Oracle's
  eval as expansion priors and leaf score, depth 4 instead of 2. Tune with
  `LECTURER_OPTS=exploration=0.7,maxDepth=4,...`.
- **Networking, campaign, rendering, i18n.**

## Reading the numbers

`bin/main.exe` is the Scala tournament driver: it shuffles legal decks, plays
the requested bots, and writes `game-NNN.json` plus `summary.json` (W/L/D and
Elo, K=16) under `tournament-results/<timestamp>/`. Default is greedy vs
greedy. `--bots greedy,oracle,lecturer` is the interesting matchup; `--budget` is the
search bots' milliseconds per move (default 1000, same as Scala). `--special`
picks the special house for both players (`Troopers`, `zen`, `wind`,
`chronomaster`, `disciples`).

The greedy bot always takes the cheapest legal card, so greedy-vs-greedy win
counts are not a balance signal. Oracle vs greedy is.

Tests that need a fixed hand still use `Game.make_player_desc`, which keeps the
old cost-slice (and one special card per quarter).

## Terminal UI

`dune exec ./bin/tui.exe` opens the lobby: campaign (grayed out, not ported),
duel and settings. Duel picks each seat (`human`, `greedy`, `oracle` or
`lecturer`) with its portrait, then fights; settings picks both special houses,
the bot budget, an optional seed and the language. Two humans is hot-seat; two
bots is a game to watch (space pauses). On your move, pick a card with the arrows
and press enter, then choose a highlighted slot (arrows or 0-5) and enter again;
`p` passes. `w` saves the game
to `tui-games/` in the tournament format, so it feeds `replay.exe` and
`compare_engines`. After a game, `r` replays the same seed and `n` deals again.

Card names and descriptions come from `i18n/<lang>.properties` in the assets
checkout (see below), with `$MANCER_LANG` (`fr`, `es`, `ru`, `vi`; default `en`)
falling back to English per key; without it the UI shows i18n keys. Bots print stats to stdout, so the UI sends stdout to
`/dev/null` and draws on a duplicate of the terminal descriptor.

Like the Scala lobby, both UIs remember their setup in `~/.mancer/user.prefs`
(the same Java-properties file as `Storage.scala`, overridable with
`$MANCER_USER_PREFS`): seats, specials, bot budget, portraits, plus the shared
`language`, `sound.level` and `player.portrait` keys. `$MANCER_LANG` still wins
over the saved language when set; a language picked in the GUI settings takes
effect on restart.

## Raylib UI

`dune exec ./bin/gui.exe` opens the same lobby, with the duel portrait pickers
and the game-settings dialog (classes checked per side from a scrolling grid,
an empty set playing every house, a game tab with bot time and seed, sound,
language). Click a lit card to play it; when it needs a target the
legal slots light up, and right click or Esc cancels. Hovering a card shows its
text. Saved games go to `gui-games/`. The window scales a fixed 1440x900 layout.

Art, sounds and text come from the
[mancer-assets](https://github.com/minhdoboi/mancer-assets) submodule at `assets/`
(`git submodule update --init`, or point `$MANCER_ASSETS` at another checkout).
Files are keyed for direct lookup: `cards/<card key>.jpg`,
`backgrounds/<house key>.jpg`, `sounds/<sound key>.mp3`. `bin/sync_assets.ml`
rebuilds it from the Scala repo, like `resources/pack_atlas.py` without the
libGDX packing: vibe card art first, then `resources/Images/Cards`, matched by
English name ignoring case and apostrophes. TGAs become PNGs and portraits are
cut to 256². It fails listing any card without art, and `manifest.tsv` records
each file's source. Hand-made files go in the checkout's `overrides/` and are
copied over the result, so keep hand edits there: `sync_assets` wipes the
generated folders. `scripts/push_assets.sh "message"` commits what changed in
`assets/`, pushes it, and stages the new submodule pointer here. Bots search on a second domain so
the window keeps drawing; their move is submitted on the main one, and leaving a
game waits for a running search to finish.

## Cross-engine harness

Hand-written house tests missed real bugs. `compare_engines` plays one recorded
game in both engines and reports the first turn where the boards differ.

Game JSON now carries opening `mana` and `rngSeed`. In-game dice use a
`java.util.Random` LCG (`lib/core/dice.ml`, Scala `GameStateUpdater.rng`) so
both sides draw the same integers. Shuffle deals stay on OCaml `Random.State`.

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