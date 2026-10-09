# Plan: porting spell effects from Scala to the raylib GUI

## Decision

We port the effects that already exist in freespectrogdx instead of designing
new ones. They are not translated line by line: they go into a small effects
module that runs on the GUI's existing animation timeline.

Why:

- Raylib has no particle system. What it offers (shaders, blend modes, render
  textures) is what the Scala effects already use, so starting over would
  mean redrawing the same kinds of shapes without their tuning.
- Most of the Scala effects are shapes drawn with libGDX's `ShapeRenderer`,
  often with additive blending. Each of those calls has a direct raylib
  equivalent (`draw_triangle`, `draw_circle`, `draw_line_ex`, `draw_ring`,
  `begin_blend_mode Additive`).
- The most valuable part is the table that maps about 90 spells to 13 shared
  styles, each with its own color, intensity and target. Porting the 13 styles
  gives most cards an effect at once.
- The hover smoke ([bin/gui.ml](../bin/gui.ml), `draw_smoke`) already shows
  the approach: `smoke.p` was rewritten by hand as OCaml code.

## What exists in Scala

All paths below are under `freespectrogdx/core/src/main/`.

| Kind | Where | Count | Technique |
|---|---|---|---|
| Shared styles | `scala/.../effects/SpellEffects.scala` | 13 styles, ~77 spells | Procedural shapes, some additive |
| Dedicated effects | `effects/Lightning, Flames, Madden, PoisonFlower, SoulHarvest, BatFlock, Phosphor, GreenFlux, BlackMass, Victory` | 10 | Procedural shapes; Madden and Victory also use Scene2D Actions |
| Shader effects | `effects/Shockwave.scala`, `effects/Shaders.scala` | 1 (+ UI) | GLSL on a custom mesh |
| Particle effects | `resources/particle/*.p` | 8 | libGDX `ParticleEffect` |
| UI effects | `gui/EffectActors.scala` | 4 | Selection glow (`sel` shader), grey cards (`grey`), board smoke (`smoke`), campaign night tint (`night`) |
| Which effect a card gets | `gui/SpellCast.scala` | 19 custom cases | `if card == …` chain, then falls back to `SpellLooks` |

Shared styles: Stones, Sprout, Eyes, Rise, Strike, Swirl, Ice, Shadow, Rays,
Drops, Fireburst, Ring, Spikes. Where the effect lands (`SpellAim`): AllSlots,
FoeSlots, OwnSlots, OwnLife, TargetAim, OwnTarget.

Particle files: `acid_rain`, `blood_burst`, `faerie_fire`, `fire_wall`,
`nature_heal`, `nether_grasp`, `redemption`, `smoke` (`smoke` is done).

Shaders that are loaded: sel, shockwave, smoke, night, grey, flat. The rest
(circle, nz, ripple, test) and the `.gdp` files are unused and are not ported.

### Which effects block the game

In Scala, an effect returns a `Promise`, and `awaitEffect` holds game logic
until it completes. Spell casts, ZenSpark and PhosphorRays block the game;
BatFlock and SoulHarvest do not. We keep this split.

## Design

### Module

`bin/fx.ml` (and `fx.mli`), added to the `gui` executable in `bin/dune`.
`gui.ml` is about 1,500 lines already, so effects stay out of it apart from
the code that starts and draws them.

```ocaml
type spot = { x : float; y : float }          (* board coordinates, before scaling *)

type t = {
  start : float;                              (* R.get_time () based *)
  duration : float;
  draw : float -> unit;                       (* progress 0..1, draws in board coords *)
}

val active : t list ref
val spawn : t -> unit
val draw_all : now:float -> unit              (* drops finished effects *)
```

Each effect is a constructor that receives where things are on the board
(source, target, slot centers, life positions) and returns a `t`. Like the
hover smoke, effects work in board coordinates and are drawn through the same
`off_x`/`off_y`/`scale` transform. Those helpers move into a small module
shared by `gui.ml` and `fx.ml`, or are passed in.

### Timeline

The GUI already replays the engine's results on a timeline: `schedule ui
duration` reserves time after `ui.anim_at`, and board snapshots (`frames`) are
shown only when their time comes.

- A blocking effect calls `schedule ui duration` and uses the start time it
  returns. Board refreshes that come after it wait until the effect ends,
  which is what Scala's `awaitEffect` does, but without blocking a thread.
- A non-blocking effect starts at `max now ui.anim_at` and does not move
  `anim_at` forward (as a non-blocking `Ev_focus` does today).

### Triggers

`queue_slides` already handles `game_event`. Two cases are added:

- `Ev_spell_played cmd` → `Fx.Cast.for_command`, which follows `SpellCast.cast`:
  the dedicated cases first, matched by card key rather than by
  `House.cards(i)`, then the `SpellLooks` table, then nothing.
- `Ev_ability { kind; … }` → one handler per kind (`"zen_spark"`,
  `"phosphor_rays"`, `"dark_flock_bats"`, `"soul_taken"`).

Today only `lib/houses/special/zen.ml` emits `Ev_ability`. The Vampire flock,
Soul Reaper harvest and Phosphor abilities must emit it at the same points as
Scala (`Vampire.scala:55`, `SoulReaper.scala:133`). This is the only change
outside `bin/`, and it does not change game logic.

### The spell look table

`SpellLooks.looks` becomes OCaml data, written as a `(key, look)` list,
not as code:

```ocaml
type style = Stones | Sprout | Eyes | Rise | Strike | Swirl | Ice | Shadow
           | Rays | Drops | Fireburst | Ring | Spikes
type aim = All_slots | Foe_slots | Own_slots | Own_life | Target | Own_target
type look = { style : style; aim : aim; color : R.Color.t; power : float }
```

A test checks that every spell key in the table exists in the OCaml card
database, so the table and the cards stay in sync.

### Particles

A generic emitter generalises the hover smoke. Its settings: emission rate,
lifetime, spawn shape, velocity and angle ranges, gravity, size over life,
color/alpha over life, blend mode and texture. The smoke is rewritten to use
it.

The remaining seven `.p` files are written out by hand as emitter records
instead of parsed. A parser for libGDX's format costs more than writing seven
files' settings by hand, and records are easier to tweak.

Textures: `particle.png`, `thin.png` and `smoke.png` are already in
`assets/particle/`. If a `.p` file uses a texture that only exists in the
Scala atlas, it is extracted with `sync_assets`.

### Shaders

The shaders are ported to raylib's names (`vertexPosition`,
`vertexTexCoord`, `mvp`, `texture0`, `colDiffuse`) and stored in
`assets/shaders/`. The custom libGDX meshes become textured quads
(`draw_texture_pro` on a render texture) or `rlgl` triangle strips where the
mesh shape matters (Shockwave).

### Tweens

Madden (shakes cards) and Victory use Scene2D Actions. A few easing functions
(`ease_out_quad`, `swing_out`, …) in `fx.ml` are enough. Madden's shake is
an offset applied to slot drawing, the same way the attack animation moves
a card.

## Steps

Each step can be merged on its own and shows something in `gui --shot`.

1. **Framework.** `fx.ml` with `t`, `spawn`, `draw_all`, easings; board
   transform helpers shared; `Ev_spell_played` hook that does nothing yet;
   `draw_all` called after the board and before the hand/HUD.
2. **Shared styles.** The 13 styles, the `look` table, and target positions
   (`pointsOf` in Scala). After this step most spells have an effect.
3. **Lightning.** The recursive bolt and its plain, chain, ball and frost
   variants. Covers Air call/lightning/chain, Ball Lightning, Frost and
   ZenSpark.
4. **Particle emitter.** Generalise the smoke, then port the 7 `.p` effects
   (earth.care, faerie.fire, junk.poison/acid rain, limbo.nether,
   limbo.redemption, vampires.ties/blood burst, fire wall).
5. **Dedicated procedural effects.** Flames (wave, inferno), BlackMass,
   PoisonFlower, GreenFlux, Phosphor.
6. **Abilities.** Emit `Ev_ability` from Vampire, Soul Reaper and Phosphor;
   BatFlock and SoulHarvest, which do not block.
7. **Shaders.** Shockwave (warp.quake), the selection glow, grey for cards
   that can't be played.
8. **Madden and Victory.** The card shake and the end-of-game screen.
9. **Bloom pass (optional).** Draw additive effects into a render texture,
   blur and add it back. This is the one thing raylib makes easy that
   Scala didn't have; we decide on it after comparing with the original.

## Checking the work

- `gui --shot` at a fixed time after a cast, for a screenshot of each style
  next to the Scala version.
- A debug key or flag that casts each effect in turn on a fixed board, so
  every effect can be reviewed without playing.
- A test that every key in the look table and every custom case is a real
  card, and that no spell in `SpellLooks.covered` is left without an effect.

## Out of scope

- Unused Scala shaders and the commented-out EarthQuake.
- The campaign map night shader, until there is a campaign screen.
- Sounds: they are already played on `Ev_spell_played`.
