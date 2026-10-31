# 7. Visual effects

Everything that draws *on top of* the board: spell animations, screen and slot
shakes, particle emitters, the selection/night/grey shaders. Seven files in
`gui/`, and one idea underneath them all.

| file | what lives there |
|---|---|
| `fx_core.ml` | types, transform, timeline, noise, shaders, shakes, the 13 shared "looks" |
| `fx_lightning.ml` | fractal bolt geometry, the hop schedule, five colour styles |
| `fx_flames.ml` | the flame column, the rolling fire sheet, embers, inferno |
| `fx_particles.ml` | the generic emitter and the hover smoke |
| `fx_dedicated.ml` | seventeen one-off choreographies (poison, scythe, aurora, …) |
| `fx_scenes.ml` | madness, shockwave, confetti, banner, victory |
| `fx_stage.ml` | the table: card key → effect |

Documents 1–6 are about the game and its bots; this one is about pixels. It is
also the only document in this folder with nothing to tune in an environment
variable — everything here is a constant in a function body.

## In one minute

- An effect is **a function of its age**: every frame the game calls
  `draw ~age` again with "seconds since the effect started" and the function
  paints the whole picture from scratch. Nothing is remembered between frames,
  so there is nothing to get out of sync (§1).
- Light is drawn by **stacking** translucent shapes (*additive blending*): a
  wide faint halo, a narrower brighter body, a thin white core. The white-hot
  centre is the sum of the layers, not a colour somebody chose (§1.5, §2.6).
- Movement looks natural because of **two clocks at unrelated speeds**
  (a slow sway and a fast flicker) and **stable per-element randomness**
  (`noise`), never a fresh random number each frame (§1.6).
- Most spells need no code: they pick one of **13 ready-made looks** from a data
  table (§5.1). Only special spells have their own function (§6).
- An effect is **blocking** if the next animation must wait for it; its
  `duration` must then match what the player actually sees (§1.2).

**Where to start.**

| you want to… | read |
|---|---|
| understand the model, whatever you do next | §1 |
| add or tweak a spell animation | §5.1 (looks table), §6, §7 |
| see how a realistic effect is built, step by step | §4 (Maddening) — the best worked example |
| learn the maths behind bolts and flames | §2 and §3 — **optional deep dives**, skip on a first read |
| debug "why is it under the cards / why does the turn hang" | §8 |

### Words used in this document

| word | plain meaning |
|---|---|
| **age** | seconds since the effect started; the only input to `draw` |
| **recipe** | a ready-to-spawn effect: its duration, whether it blocks, an optional sound clip, and a `make` function |
| **blocking** | the next animation (summon, death, slide…) waits until this effect ends |
| **alpha** | opacity, 0 (invisible) to 1 (solid) |
| **additive blending** | colours are *added* to what is underneath, so overlapping light gets brighter, like spotlights |
| **easing** | a curve that makes a 0→1 motion start or end gently (`ease_out` = fast then slow) |
| **envelope** | rise / hold / fade shape of an effect's strength over time |
| **layout** | the object that tells effects where slots, portraits and floors are on screen |
| **look** | a ready-made effect style (rocks, rays, ice…) parameterised by colour, target and power |
| **shake** | a small, timed offset applied to a slot so it trembles |
| **fractal / midpoint displacement** | build a jagged line by repeatedly splitting segments and nudging the middle point |


## 1. The pipeline: an effect is a function of its age

### 1.1 The timeline

```ocaml
type t = { start : float; duration : float; draw : age:float -> unit }
type recipe = { duration : float; blocking : bool; sound : string option; make : start:float -> t }
```

`fx_core.ml:9-10`. An effect is spawned once and then *re-evaluated from
scratch every frame* with the elapsed time. There is no state machine, no
`update` phase, and nothing that remembers what was true last frame — with two
deliberate exceptions, the lightning geometry cache and the particle emitters,
both of which want their own memory.

The whole timeline is nine lines (`fx_core.ml:84-97`):

```ocaml
let draw_all ~now =
  active :=
    List.filter
      (fun e ->
        let age = now -. e.start in
        if age >= 0. && age < e.duration then e.draw ~age;
        now < e.start +. e.duration)
      !active
```

Three consequences worth internalising before reading any effect:

- **An effect is reproducible from `age` alone.** Every position in an effect is
  a closed-form function of time, not an accumulation. This is why the whole
  system survives a window resize, a frame-rate change and a stalled frame
  without a correction step.
- **Lifetime is `duration`.** There is no leak path, and `age` outside
  `[0, duration)` draws nothing. The two exceptions are the victory banner and
  the portrait halo, which use `Float.infinity` and are cleared by `Fx.clear`
  when a rematch starts (`fx_core.ml:255-257`).
- **The duration in the `recipe` is a promise to the animation timeline**, not
  just a cutoff. See below.

### 1.2 Blocking, and the animation cursor

`gui_board.ml:542-546`:

```ocaml
let run_fx ui recipe =
  let start =
    if recipe.Fx.blocking then schedule ui recipe.duration
    else Float.max (R.get_time ()) ui.anim_at
  in
  Fx.spawn (recipe.make ~start)
```

`schedule` (`gui_board.ml:405-410`) is a one-field cursor:

```ocaml
let schedule ui duration =
  let now = R.get_time () in
  if ui.anim_at < now then ui.anim_at <- now;
  let start = ui.anim_at in
  ui.anim_at <- start +. duration;
  start
```

So `blocking = true` does not merely delay the effect — it **pushes the start of
every later animation** by its own duration. Summons, deaths and card slides
all go through the same cursor, which is why a blocking spell visibly holds the
turn. The house of the effect decides the convention: an effect that *resolves
something* blocks (`fire.wave`, `disciples.madden`), and an effect that is
atmosphere on top of an already-finished move does not (`vampires.flock`,
`reaper.soul` — both `blocking:false` in `fx_stage.ml:306-317`).

**A blocking effect must set `duration` to what the player sees.** If the draw
code finishes at 0.8 s but the recipe says 1.5, the turn waits half a second
after the picture is gone.

### 1.3 The fade convention

Nearly every effect ends with the same three lines:

```ocaml
let fade = Float.min 1. ((duration -. age) /. 0.2) in
if fade > 0.02 then ...
```

A linear ramp to zero over the last 0.2 s, with a hard cut at 2% alpha — which
is also where the primitives stop drawing anything (§1.4). Effects have no
common fade-*in*: pop-in is done per element instead, by giving each element a
`born` offset and letting it be invisible until then (`born = index · 0.07` for
a lightning hop, `born = noise e 2 5 · something` for an ember). The available
easings are `clamp01`, `ease_out u = 1 − (1 − u)²`, and `swing_out`, a
back-out overshoot used only by the victory banner's scale pop.

### 1.4 Coordinates, scaling and culling

The board is authored in an 832×768 design space (`gui_draw.ml:9-10`) and
mapped by two functions:

```ocaml
let sx x = !off_x +. (x *. !scale)
let sy y = !off_y +. (y *. !scale)
```

`fx_core.ml:38-39`, set once per frame from the window size (`bin/gui.ml:246`).
Every effect above that level is resolution-independent; only text multiplies by
`!scale` by hand.

The five primitives each refuse sub-pixel work, which is what keeps the flame
wave (§3) affordable:

```ocaml
let circle x y rad c = if rad *. !scale > 0.4 then R.draw_circle_v (v x y) (rad *. !scale) c
let line x0 y0 x1 y1 thick c = if thick *. !scale > 0.3 then ...
let ellipse x y rx ry c = if rx *. !scale > 0.4 && ry *. !scale > 0.4 then ...
```

### 1.5 Additive blending is how "energy" is drawn

`additive f` wraps a draw in `BlendMode.Additive`, which in Raylib is
`(SRC_ALPHA, ONE)`: the framebuffer gains `colour · α`. A single alpha-blended
pass can never be brighter than its own colour, but N stacked translucent passes
sum. Every glow in this codebase is therefore three or four passes with
progressively smaller radius and larger alpha — see the lightning table in §2.6
and the flame table in §3.2. `additive_style` (`fx_core.ml:274-276`) is the one
place where this is turned *off*: rocks, sprouts, eyes and spikes are solid
objects, and adding them would blow them out to white, so they draw normally.

Draw order matters the other way for alpha-blended passes: Raylib draws later
calls over earlier ones, and `bin/gui.ml:398` calls `Fx.draw_all` after the
board, the slots and the hands precisely so bolts land *on top of* cards. The
comment in that file says so.

### 1.6 Two kinds of randomness

`noise a b c` (`fx_core.ml:72-76`) is a stateless integer hash returning a float
in `[0, 1)`:

```ocaml
let n = (a * 374761393) + (b * 668265263) + (c * 1442695041) in
let n = (n ^ (n >> 13)) * 1274126177 in
(n & 0xffffff) /. 16777215.
```

It is used for properties that must be **stable while a frame lasts but differ
between neighbours**: the raggedness of one column of fire, whether one card
gets a glitch slice, where one ember is born. Frame-varying things use
`sin (t·ω + …)` or a `Random.State` instead. Getting this backwards is the
classic bug in an effects rewrite: an effect that reseeds its RNG every frame
turns into television static, and one that hashes only the time index turns into
a frozen picture.

Colours go through one clamp so that an effect can overshoot its design range
without wrapping:

```ocaml
let byte x = int_of_float (Float.max 0. (Float.min 255. (x *. 255.)))
```

## 2. Lightning

*Deep dive: the numbers in §2.2–2.6 explain why the constants are what they are; you can use `spawn_lightning` without them.*


`fx_lightning.ml`, 157 lines, used by five spells (`air.call`, `air.lightnin`,
`air.chain`, `wind.BallLightning.name`, `elementalist.frost`) and one ability
(`zen.spark`). The signature is deliberately dumb: a list of points to travel
through, and an optional style.

```ocaml
spawn_lightning ?(style = electric) [ own_life; foe.(2); foe_life ] ~start
```

Three points is a two-hop chain: the bolt strikes the first pair 70 ms in, the
second pair 70 ms after that, and then whatever has been struck stays alight
for 0.7 s while the whole thing fades.

### 2.1 The path is a midpoint-displacement fractal

The classic midpoint displacement algorithm, in 28 lines (`fx_lightning.ml:25-52`):

```ocaml
let jag rng a b generations sway =
  let pts = ref [| a; b |] in
  let disp = ref sway in
  for _ = 1 to generations do
    ...
    let mx = (p.x +. q.x) *. 0.5 and my = (p.y +. q.y) *. 0.5 in
    let dx = q.x -. p.x and dy = q.y -. p.y in
    let len2 = (dx *. dx) +. (dy *. dy) in
    let mid =
      if len2 > 64. then
        let inv = 1. /. sqrt len2 in
        let off = (Random.State.float rng 1. -. 0.5) *. 2. *. !disp in
        { x = mx +. (-.dy *. inv *. off); y = my +. (dx *. inv *. off) }
      else { x = mx; y = my }
    in
    ...
    disp := !disp *. 0.55
  done;
  !pts
```

Reading the math:

- **Perpendicular, not random direction.** The displacement is
  `off · (−dy, dx)/|d|`, i.e. the unit normal of the segment times a uniform
  offset in `(−disp, +disp)`. Displacing along the normal keeps the endpoints
  fixed, which is what makes the recursion composable — the parent segment's two
  halves always join.
- **The ratio is 0.55.** Each generation's amplitude is 0.55 of its parent's.
  This is the "H" in fractal Brownian motion, and 0.55 is a hand-tuned visual
  compromise: larger ratios give an obviously geometric zigzag, smaller ones
  give noise with no large-scale direction.
- **The 8 px floor.** `len2 > 64.` skips the offset when a segment is shorter
  than 8 px. A displacement on a 2 px segment is not a wiggle, it is a
  discontinuity.

### 2.2 How far it wanders, and why

Take one generation's offset as `U(−d_k, d_k)` with `d_k = sway · 0.55^k`. A
variance of `U(−d, d)` is `d²/3`, so if all the offsets pointed along the same
normal, the deepest vertex's lateral deviation would have

```text
Var = Σ_k d_k² / 3 = sway²/3 · 1/(1 − 0.55²) = 0.478 · sway²
σ   = 0.69 · sway
```

They do not point the same way: at every level the normal is the normal of a
polyline that the previous level already bent, so a large tilt eats part of the
next offset's vertical component. Simulating the actual code (6 generations,
`sway = 62`, chord 300 px, 1.3 M vertices):

| statistic | measured | aligned-normal ideal |
|---|---|---|
| RMS over all vertices | 24.2 px = **0.39 · sway** | — |
| RMS at the deepest vertex | 36.0 px = **0.58 · sway** | 0.69 · sway |
| largest \|offset\| seen | 81 px = **1.31 · sway** | bound 2.16 · sway |
| hard bound (all offsets at their extremes, same direction) | — | `sway / 0.45 = 2.22 · sway` |

So: plan a bolt with `sway = 40` and expect it to wander about 16 px typically
and 50 px at worst. The `2.22 · sway` figure is the geometric sum
`sway(1 + 0.55 + 0.55² + …)` and is not reachable — it needs every one of six
independent uniforms to land on its extreme, in the same direction.

### 2.3 The span chooses the resolution

`build` (`fx_lightning.ml:72-78`) is where the geometry is chosen:

```ocaml
let sway = Float.min 62. (Float.max 14. (len *. 0.22)) in
let gens = if len > 220. then 6 else if len > 100. then 5 else 4 in
```

| chord | `sway = clamp(0.22·len, 14, 62)` | gens | points = 2^g+1 | segment = len/2^g | RMS (0.39·sway) |
|---|---|---|---|---|---|
| 60 px | 14.0 (0.22·60 = 13.2 → floored) | 4 | 17 | 3.75 px | 5.5 px |
| 120 px | 26.4 | 5 | 33 | 3.75 px | 10.3 px |
| 240 px | 52.8 | 6 | 65 | 3.75 px | 20.6 px |
| 400 px | 62.0 (0.22·400 = 88 → capped) | 6 | 65 | 6.25 px | 24.2 px |
| 700 px | 62.0 | 6 | 65 | 10.9 px | 24.2 px |

Two things fall out of this table:

- **The final segment length is nearly constant at 3.75 px.** The generation
  count is chosen exactly so that `len / 2^g` lands just under the 8 px
  displacement cutoff. That is the whole reason for the 220/100 thresholds — a
  fourth threshold would be needed for really long hops, and there isn't one, so
  from 512 px of chord (`64 × 8`) onward the finest generation contributes
  vertices but no jitter.
- **Amplitude saturates.** Beyond 282 px of chord, `0.22·len` wants more than
  the 62 px ceiling, so a board-wide bolt and a 400 px bolt jitter the same
  amount. `62 / 0.22 ≈ 282` is the span at which a lightning bolt stops getting
  angrier as it gets longer.

A **secondary** path is generated alongside for long hops only: same
endpoints, one generation fewer, half the sway (`fx_lightning.ml:77`). It is
drawn *under* the primary and is what makes a long bolt look like a bolt rather
than a wire — two independent cracks that never meet.

### 2.4 Forks

`forks` (`fx_lightning.ml:54-68`) grows one or two side branches:

```ocaml
let count = if span > 220. then 2 else 1 in
let ang = atan2 (p.y -. prev.y) (p.x -. prev.x)
          +. (if Random.State.bool rng then 0.85 else -0.85)
          +. ((Random.State.float rng 1. -. 0.5) *. 0.5) in
let bl = Float.min 78. (span *. (0.16 +. Random.State.float rng 0.18)) in
jag rng p { ... } 3 (bl *. 0.4)
```

- **±0.85 rad = ±49°** off the local direction, with a further ±14° of jitter.
  Roughly perpendicular, jittered — a fork that leaves at a shallow angle reads
  as a kink, not a fork.
- **Length is 16–34% of the span, capped at 78 px**, drawn with 3 generations
  and a sway of 0.4·length. That is a *large* relative sway for a 40 px branch,
  so short forks curl tightly.
- **No forks under 8 points or a 50 px span.** A short hop has too few
  vertices to pick a believable branch point from.

### 2.5 The hop schedule and its alpha

`hop_delay = 0.07`, `lightning_hold = 0.7`
(`fx_lightning.ml:22-23`), and

```text
duration = (points − 1) · 0.07 + 0.7
```

| spell | points | hops | duration | last hop begins |
|---|---|---|---|---|
| `air.lightnin` | 2 | 1 | 0.77 s | 0.00 s |
| `air.call` | 3 | 2 | 0.84 s | 0.07 s |
| `elementalist.frost` | 3 | 2 | 0.84 s | 0.07 s |
| `air.chain` | 7 | 6 | 1.12 s | 0.42 s |

Alpha per hop (`hop_alpha`, `fx_lightning.ml:94-102`) is the product of three
independent factors:

```ocaml
let strike = if age < 0.045 then 1. else 0.8 in
let flick = if (int_of_float (t *. 20.) + index) mod 4 = 0 then 0.4 else 1. in
let fade = Float.min 1. ((duration -. t) /. 0.22) in
strike *. flick *. Float.max 0. fade
```

- **`strike`** — the first 45 ms of a hop are drawn at full brightness, then it
  drops to 0.8 for the rest of its life. A real strike is a flash and a
  afterglow; this is the flash.
- **`flick`** — a 20 Hz clock, one tick in four dimmed to 0.4. That is a 5 Hz
  pulse at 25% duty, mean multiplier 0.85. The `+ index` is the important part:
  a hop dims on the tick where `tick ≡ −index (mod 4)`, so hop 0 dims on tick 0,
  hop 1 on tick 3, hop 2 on tick 2, hop 3 on tick 1 — the chain cycles
  backwards through the four ticks and never pulses as a whole. It crackles.
- **`fade`** — the house 0.2 s ramp, done over 0.22 s.

Worked: hop 3 of the seven-point `air.chain` (duration 1.12) at `t = 0.26`.

```text
born  = 3 · 0.07 = 0.21,  age = 0.05
strike = 0.8                       (age > 0.045)
flick  = (int(0.26 · 20) + 3) mod 4 = (5 + 3) mod 4 = 0  -> 0.4
fade   = min 1 ((1.12 − 0.26)/0.22) = 1
alpha  = 0.8 · 0.4 · 1 = 0.32
```

Twenty milliseconds earlier (`t = 0.24`, tick 4) the same hop draws at 0.8. That
alternation at 20 Hz is the entire crackle.

**The geometry is rebuilt at the same 20 Hz**, which is a separate decision from
the flicker and the reason they are coded as two clocks
(`fx_lightning.ml:142-148`):

```ocaml
let next = int_of_float (age /. 0.05) in
if next <> !frame then (frame := next; strikes := Array.mapi ... (build rng a b) ...)
```

Rebuilding every frame would cost 6× more `Random.State` draws for a difference
nobody can see at 60 fps; rebuilding less often than the flicker makes the bolt
look like it is strobing as a whole. 20 Hz is the point where the two read as
one phenomenon. The bolts stay lit for the rest of the effect with *frozen*
geometry, which is correct — a channel of lightning is a briefly-lit path, not a
moving one.

### 2.6 Painting: three nested strokes

`paint_strike` (`fx_lightning.ml:104-125`) draws the same polyline eight times.

| pass | width | alpha | colour |
|---|---|---|---|
| secondary glow | 0.65 · glow_w | 0.28 | `glow` |
| primary glow | glow_w | 0.42 | `glow` |
| fork glow | 0.45 · glow_w | 0.30 | `glow` |
| secondary body | 4.2 | 0.55 | `body` |
| primary body | 5.2 | 0.80 | `body` |
| fork body | 2.4 | 0.70 | `body` |
| primary core | 2.0 | **1.00** | `core` |
| fork core | 1.3 | 0.90 | `core` |

with

```text
glow_w = min(18, 8 + 0.015 · span)      -> 14 px for a 400 px hop, capped at 18
```

Three properties of that table matter:

- **Glow is proportional to the span, body and core are not.** A long bolt gets
  a proportionally fatter halo — physically right (more air to light) and it
  keeps the halo from looking like a rope drawn around a wire.
- **The core is drawn last, at full alpha.** Additively, the centre pixel
  receives `0.42 + 0.80 + 1.00 = 2.22` times the alpha, so it saturates to the
  core colour while the edges stay in the `glow` hue. The white-hot centre is not
  drawn anywhere; it is the sum.
- **`stroke` draws a circle at every vertex when `caps` is true**
  (`fx_lightning.ml:80-92`), so joins are round and the polyline has no gaps at
  the kinks. Only the three glow passes ask for caps: a 14 px cap is a round
  blob that hides the join, whereas a 2 px cap on the core would just be the
  same width as the line again.

Then two circles at the target (radius 11 at 0.45 body, 4.5 at 1.0 core) for
the strike point.

**Cost.** A 6-generation hop is 64 segments × 8 passes ≈ 512 `line` calls, plus
one cap circle per vertex on the three glow passes (~195 more), so ~700 draw
calls per hop per frame; `air.chain` runs six of them. This is the second most
expensive effect in the codebase (§3.5 has the first).

### 2.7 The five styles

```ocaml
let electric = { glow = (0.48, 0.22, 1.); body = (0.35, 0.78, 1.); core = (0.95, 0.98, 1.); orb = false }
let frost    = { glow = (0.25, 0.55, 1.); body = (0.7, 0.95, 1.);  core = (0.92, 1., 1.);    orb = false }
let ball     = { glow = (0.55, 0.3, 1.);  body = (0.45, 0.8, 1.);  core = (1., 0.97, 0.85);   orb = true }
let spark    = { glow = (1., 0.82, 0.12); body = (1., 0.92, 0.28); core = (1., 0.98, 0.82);   orb = false }
let drain    = { glow = (0.82, 0.88, 1.); body = (0.95, 0.97, 1.); core = (1., 1., 1.);      orb = false }
```

Four of the five are pure colour. `ball` also sets `orb`, which adds a travelling
sphere along the hop (`fx_lightning.ml:120-125`):

```ocaml
let u = clamp01 ((t -. 0.02) /. 0.28) in
let x = from_p.x +. ((to_p.x -. from_p.x) *. u) in
circle x y 18. (rgb gr gg gb (alpha *. 0.55));
circle x y 7.  (rgb cr cg cb alpha)
```

A 0.30 s constant-velocity transit with a 20 ms head start, drawn as the same
halo-plus-core pair as everything else. Note the transit is in *effect* time,
not per hop, so a seven-point ball chain has all six orbs in flight together.

## 3. Fire

*Deep dive: skim the code listings, keep the idea — a front moving across the row, tall and white at the head, a low permanent bed behind it.*


`fx_flames.ml`, 185 lines, for `fire.wave` and `fire.inferno`. Three layers:

1. `flame_column` — one vertical flame, drawn as a stack of circles.
2. `fire_sheet` — a wavefront that runs across the row, calling the column
   35 times per frame.
3. `sheet_embers` — sparks trailing the front.

### 3.1 The column

`flame_column` (`fx_flames.ml:12-48`) draws `licks` (3 in a sheet, 5 and 4 in
inferno) flames side by side. For lick *k* of a column of height *h*:

```ocaml
let lean = sin ((t *. 3.2) +. (float id *. 0.73) +. (float k *. 1.4)) *. 0.45 +. push in
let h = height *. (0.78 +. (0.22 *. (0.5 +. (0.5 *. sin ((t *. 11.) +. ...))))) in
let steps = max 6 (min 14 (int_of_float (h /. 7.))) in
let side = (float k -. mid) *. fat *. 6.5 in
```

Four decisions in six lines:

- **Lateral wander is slow and per-lick** — `3.2 rad/s` ≈ 0.5 Hz, with the
  per-lick phase offset `1.4·k` so the licks do not move as one object.
- **Height is modulated at `11 rad/s` ≈ 1.75 Hz** between 78% and 100%. Fire
  flickers faster than it breathes; the sway and the flicker are deliberately at
  different frequencies for the same reason the lightning has two clocks.
- **`steps` is a resolution constant**: one circle every 7 px of flame, clamped
  to 6–14 steps. A 150 px inferno column gets 14 steps of ~11 px; a 40 px one
  gets 6 of ~7 px.
- **`side` fans the licks out** by `fat · 6.5` px per step from the centre —
  three licks span ±6.5 px of a ~17 px radius column, which is what stops a
  column from looking like one fat sausage.

The vertical profile of one lick, at step fraction `v` from base (0) to tip (1):

```ocaml
let pinch = (1. -. vv) ** 0.62 in
let wob = sin (phase +. (vv *. 4.6)) *. (2.5 +. (9. *. vv)) in
px.(s) <- x +. side +. (lean *. vv *. 34.) +. wob;
py.(s) <- y -. (vv *. h);
pr.(s) <- fat *. (6.2 +. (11. *. pinch))
```

- **The taper is `(1 − v)^0.62`, not `(1 − v)`.** Radius at the base is
  `17.2 · fat`; at the tip `6.2 · fat`. With exponent 1 the profile would be a
  straight cone; below 1 it stays fat through the lower third and then drops
  away — the silhouette of a flame rather than a triangle. The measured values:
  `pinch(0.25) = 0.84`, `pinch(0.5) = 0.65`, `pinch(0.75) = 0.42`,
  `pinch(0.9) = 0.24`.
- **Wobble amplitude grows linearly to the tip** (`2.5 + 9v` px), for the same
  reason the fractal in §2 gets rougher as it gets finer: the tip is the least
  constrained part of the shape.
- **`lean · v · 34`** turns the sway into a bend — the offset grows with height,
  so the flame leans instead of sliding sideways.

### 3.2 Four passes, and why the bright part is short

```ocaml
paint 1.    0.2  0.02 (alpha *. 0.26) 1.2   1.;                 (* outer red  *)
paint 1.    0.5  0.06 (alpha *. 0.38) 0.74  0.84;                (* orange     *)
paint 1.    0.88 0.32 (alpha *. 0.48) 0.42  0.58;                (* yellow     *)
paint 1.    0.97 0.9  (alpha *. 0.4 *. hot) 0.24 (0.28 +. 0.18 *. hot)  (* white core *)
```

Each pass is `radius · scale` over the first `steps · reach` steps. The radius
scales 1.2 → 0.24 and the reach shrinks 1.0 → 0.28, additively. **The white-hot
core covers only the bottom 28% of the flame**, which is what makes it read as
heat rising rather than a gradient. The fourth pass is conditional on `hot`,
which is the sheet's crest value (§3.3): a column at the front of the wave has a
white core, a column 300 px behind it is dull red all the way up.

### 3.3 The sheet: a travelling front

`fire_sheet` (`fx_flames.ml:53-93`) samples 35 columns across the row and asks,
for each, *how far behind the advancing front is this column?*

```ocaml
let reach = t *. speed in
let behind = reach -. dist in
let ignite = clamp01 ((behind +. 12.) /. 30.) in
let body = exp (-.Float.max 0. (behind -. 40.) /. 230.) in
let heat = ignite *. Float.max (ignite *. 0.35) body in
let crest = exp (-.(behind -. 25.) *. (behind -. 25.) /. 2400.) in
let h = (24. +. (56. *. heat)) *. (0.8 +. (0.9 *. crest)) *. flick *. ragged *. power in
```

Four functions, each with a job:

- **`ignite`** — a 30 px ramp from "not yet reached" to "fully alight", offset by
  12 px so a column fades *in* just before the front touches it.
- **`body`** — an exponential decay with a **230 px characteristic length in
  space** (the front moves at constant speed, so this is also a time constant of
  `230 / speed` seconds). At 948 px/s that is 0.24 s of burn behind the front.
- **`heat = ignite · max(ignite · 0.35, body)`** — the inner `max` is a floor:
  a column that has been burning for a long time still sits at 35% heat, so the
  sheet leaves a permanent low bed of flame instead of ending abruptly. It is
  also why the far tail is dim *and* short rather than invisible.
- **`crest`** — a **Gaussian bump centred 25 px behind the front**, `σ² =
  2400/2 = 1200`, so `σ ≈ 35 px`. This is the visual "head" of the wave: it is
  where the columns are tallest and where `hot` lets the white core in.

Height, worked (`power = 1`, flicker mean 0.94, ragged mean 1.0):

| `behind` | `heat` | `crest` | height |
|---|---|---|---|
| −12 px | 0.00 | 0.00 | 18 px |
| 0 | 0.40 | 0.77 | 65 px |
| 25 (crest peak) | 1.00 | 1.00 | **128 px** |
| 100 | 0.77 | 0.10 | 56 px |
| 300 | 0.35 (the floor) | ~0 | 33 px |
| 700 | 0.35 (the floor) | ~0 | 33 px |

The last two rows are the interesting pair. The exponential crosses the floor
where `exp(−(b−40)/230) = 0.35`, i.e. at `b = 40 + 230·ln(1/0.35) ≈ 282 px`
behind the front — and after that the bed **stops decaying altogether** and
sits at a constant 33 px of dull flame until the effect ends. The wave is
therefore a bright leading edge over a long, permanent, low bed: the fire
covers the whole row and stays, which is what "the ground is on fire" looks
like, rather than a wave that visibly stops.

`flick` is a per-column `0.88 + 0.12 sin(30t + i)` — 4.8 Hz, phase-offset per
column, which is what stops 35 columns from pulsating in lockstep. `ragged` is
`0.8 + 0.4 · noise i seed 7`, the per-column permanent variation of §1.6.

The columns also **lean into their direction of travel**: `push = side · 0.95 ·
crest`, where `side` is +1 or −1 depending on which way the front came from.
Only at the crest, so the head of the wave bends over and the tail stands
upright.

### 3.4 Speed is derived from the geometry

```ocaml
let bounds bases = (min_x -. 44., max_x +. 44.)        (* fx_flames.ml:116 *)
let speed = span /. 0.62                                 (* flame wave *)
let speed = span /. 0.55                                 (* inferno *)
```

Six slots at a 100 px pitch span 500 px of centres, and `bounds` pads by 44 px on
each side, so `span = 588 px`. The wave therefore crosses the whole padded row
in exactly 0.62 s regardless of the board layout, at **948 px/s** (inferno:
1069 px/s), and the column pitch on screen is `588 / 34 = 17.3 px` — comfortably
inside the ~17–27 px column radius, which is why adjacent columns merge into a
sheet instead of reading as a picket fence.

`spawn_inferno` (`fx_flames.ml:139-185`) is the same machinery with a different
choreography:

1. **Engulf**, first: two columns at the target (150 px and 80 px tall, `fat`
   1.9 and 2.5, five and four licks) under an ADSR envelope
   `rise 0.08, hold 0.7, fade 0.4`, plus a 34 px glow disc. The wider, shorter
   column in front of the taller one is what makes the middle read as a solid
   mass of fire.
2. **Then the wave**, after a 0.16 s delay, at 0.85 power in *both* directions
   (`dirs: 0`), so it runs outward from the target rather than across the row.
3. **A shock ring** during the first 0.45 s: 20 points on a circle of radius
   `16 + 150t`, squashed to 0.42 in *y* so it reads as lying on the ground.
4. **Sparks**: 30 ballistic puffs, `x = tx + cos(ang) · speed · t`,
   `y = ty − 8 + sin(ang) · speed · t − 40t`, with `speed = 80 + 170 · noise`,
   fading as `(1 − p)`.

Embers (`sheet_embers`, `fx_flames.ml:96-114`) are the cheap version of the same
idea — no trigonometry, just `p = a/life`:

```ocaml
let born = Float.abs d /. speed in          (* born when the front passes *)
let ey = y -. 14. -. ((70. +. (120. *. noise e 4 6)) *. p) in
let col = if noise e 9 3 > 0.4 then rgb 1. 0.92 0.55 ((1. -. p) ** 2. *. fade)
          else rgb 1. 0.45 0.05 ((1. -. p) ** 2. *. fade *. 0.9) in
```

`born` is derived from the *same* front position as the sheet, so sparks appear
under the flame exactly when the fire reaches them rather than on an independent
timer. The quadratic alpha `(1 − p)²` front-loads the brightness, and the
`noise > 0.4` split gives a mix of pale ash and deep ember.

### 3.5 Cost

35 columns × 3 licks × up to 14 steps × 4 passes ≈ **5 900 circles per frame**,
plus 70 embers, plus the three-line coal bed. This is the most expensive effect
in the game, and it is the reason the primitives cull sub-pixel work and the
`steps` count is derived from the height rather than fixed. On a slow machine
this is the effect that drops frames; it is also the only one with a `dirs`
parameter, and the columns the wave is not travelling towards are skipped before
any drawing.

## 4. Maddening

`spawn_madden` (`fx_scenes.ml:14-90`), the Disciples' mass hypnosis. It is the
one effect that touches the *board* rather than painting on it, and it is worth
reading in full because it is the clearest example of an effect with three
independent clocks driving three different parts of the same picture.

### 4.1 What it is handed

```ocaml
spawn_madden ~foes:foe_ids ~allies:ally_ids ~eye ~start
```

from `fx_stage.ml:102-117`. `foes` is every occupied enemy slot (or all six if
the enemy board is empty), `allies` is every occupied own slot, and `eye` is
computed from the layout:

```ocaml
let eye = {
  x = Array.fold_left (fun s p -> s +. p.x) 0. foe_row /. float nb_slots;
  y = (foe_row.(0).y +. own_row.(0).y) *. 0.5;
}
```

— the centroid *x* of the opponent's row and the midpoint between the two rows.
With six evenly pitched slots that is the horizontal centre of the board, between
slots 2 and 3, which is where the eye belongs: the middle of the thing being
hypnotised.

`spawn_madden` also registers **shakes** immediately, before any drawing
(§4.5).

### 4.2 Three clocks

```ocaml
let fade    = Float.min 1. ((madden_duration -. age) /. 0.2) in   (* 1.28 s life *)
let open_   = ease_out (clamp01 (t /. 0.28)) in
let madness = ease_out (clamp01 ((t -. 0.1) /. 0.35)) in
let ramp    = clamp01 (t /. madden_duration) in
let shake   = ramp *. ramp in
```

- `fade` — the standard 0.2 s tail.
- `open_` — the **eyelid**, 0 → 1 over 0.28 s with `ease_out`, so half the
  opening is done in the first 0.07 s and it settles long before the eye has
  finished glowing.
- `madness` — the **glitch**, delayed by 0.1 s and finished at 0.45 s, so the
  eye opens on a still board and the corruption arrives after.
- `shake = ramp²` — the **board violence**, a quadratic ramp over the whole
  1.28 s, used both for the glitch shear and for the slot shakes.

The separation matters: eye first, then corruption, then a long crescendo. Three
clocks let all three happen without a state machine.

### 4.3 The glitch

Per card, drawn as a `78 × 88` rectangle (the slot is 90 × 102, so the frame
sits just inside it):

```ocaml
let split = (3. +. (shake_m *. 10.)) *. (0.6 +. (0.4 *. sin (t *. (18. +. float id)))) in
rect (x +. split)  y w h (rgb 0.95 0.08 0.1  (fade *. 0.22));   (* red copy   *)
rect (x -. split) y w h (rgb 0.05 0.75 0.85 (fade *. 0.16));   (* cyan copy  *)
```

That is **chromatic aberration**: two copies of the card silhouette in
complementary colours, displaced in opposite directions. The offset is
`(3 + 10·shake)` px scaled by a per-card `0.4 sin(18t + id)` wobble, so at rest
it is 1.8–3 px (a barely-there double image) and at full shake up to 13 px. The
`+ float id` inside the frequency means neighbouring cards beat against each
other rather than wobbling in unison.

On top of the copies, four more layers:

- **A scan band** travelling down the card: `u = mod(1.6t + noise)`, speed
  `1.6 + 4.5·shake` heights per second, thickness `5 + 7·shake` px, pale pink.
- **A torn slice** on 55% of cards (`noise id 8 4 > 0.45`): a dark bar of height
  `8 + 16n` drifting at `2.4 + id` heights per second, offset sideways by
  `(n − 0.5)·18` px.
- **Nine horizontal scan lines**, 1.4–2.6 px thick, blinking at
  `sin(30t + 1.7k + id)` — about 4.8 cycles a second, with the per-line phase
  offset so the blink looks like noise.
- Allies get the same treatment at `0.38` of the alpha and `0.4` of the shake,
  so the hypnotic effect visibly concentrates on the enemy.

### 4.4 The eye

Six nested ellipses (sclera, sclera rim, mid, inner, iris, pupil), ten radial
iris fibres, a pupil and a highlight dot (`fx_scenes.ml:64-85`):

```ocaml
let rx = 40. and ry = 4. +. (22. *. open_) in
let irx = rx *. 0.48 and iry = ry *. 0.78 in
...
for k = 0 to 9 do
  let ang = (float k *. (Float.pi /. 5.)) +. (t *. 0.4) in
  let len = irx *. (0.55 +. (0.35 *. noise k 2 1)) in
  line x y (x +. (cos ang *. len)) (y +. (sin ang *. len *. (iry /. Float.max irx 1.)))
done;
```

| `t` | `open_` | `ry` | eye aspect `ry/rx` | what happens |
|---|---|---|---|---|
| 0.00 | 0.00 | 4.0 | 0.10 | a closed slit; the red/cyan copies are barely offset |
| 0.10 | 0.59 | 16.9 | 0.42 | the eye is most of the way open |
| 0.28 | 1.00 | 26.0 | 0.65 | fully open; the glitch is at 0.76 |
| 0.45 | 1.00 | 26.0 | 0.65 | glitch at full strength; **pupil starts dilating** |
| 0.55 | 1.00 | 26.0 | 0.65 | pupil at maximum width |
| 1.08 | 1.00 | 26.0 | 0.65 | last frame before the fade; peak shake |
| 1.28 | — | — | — | `fade = 0`, nothing drawn |

Two details in the eye geometry:

- **The vertical squash of the iris fibres** — `len · (iry / irx)` — is what
  makes ten lines radiating from the centre read as a *circular* iris pattern
  inside an *elliptical* eye. Without it, the fibres would form a flattened
  starburst.
- **The pupil dilates on its own schedule**: `slit = 1 + 0.8 sin(22t + 1.2) ·
  clamp01((t − 0.45)/0.1)`, so the pupil is a fixed 5.2 px slit for the first
  0.45 s and then dilates to 9.4 px over 0.1 s. It is the one thing in the
  effect that gets *worse* as the spell succeeds.

The last layer is additive: two discs of radius `28 + 10·open_` and 14, pulsing
at `0.55 + 0.45 sin(11t)`.

### 4.5 The board shakes

Effects can register a shake for a slot, and the board rendering asks for the
current offset every frame (`fx_core.ml:240-251`, used at `gui_board.ml:913`):

```ocaml
let percent = clamp01 ((now -. s.start) /. s.duration) in
let ramp = percent *. percent in
let freq = 10. +. (ramp *. (28. +. (42. *. s.strength))) in
let amp = (0.7 +. (ramp *. 6.2)) *. s.strength in
let phase = percent *. s.duration *. freq in
(sin (phase *. 2.17) *. amp, cos (phase *. 3.41) *. amp *. 0.42)
```

Note `percent · duration = elapsed`, so `phase` is elapsed time × frequency and
the displacement is a two-axis Lissajous:

- **`ramp²`** — the same quadratic the glitch uses, so the shake and the visual
  corruption crescendo together.
- **Amplitude ≤ 6.9 px** at full ramp, scaled by `strength` (1.0 for enemies,
  0.38 for allies).
- **Frequency rises from 10 to 80 Hz** as the ramp grows.
- **The `2.17` and `3.41` multipliers are coprime-ish and irrational enough**
  that the pattern does not visibly repeat — the classic trick for a shake that
  looks hand-animated rather than looped.

A nice consequence of the interaction with the fade: the amplitude never actually
reaches its 6.9 px ceiling, because at `ramp = 1` (`t = 1.28`) the effect has
already faded to nothing. The **visible peak is at `t = 1.08`**, where
`ramp = 0.84`, `shake = 0.71`, so the largest shake a player ever sees is

```text
amp  = (0.7 + 0.71 · 6.2) = 5.1 px      freq = 10 + 0.71 · 70 = 59.7 Hz
```

— and 59.7 Hz is at the edge of what a 60 fps display can show, which is why the
shake reads as a high-frequency buzz rather than a rumble. The amplitude was
tuned down to match.

## 5. The rest of the family

### 5.1 The thirteen shared looks

Most spells do not have bespoke code. They pick a `look` from the table in
`lib/game/spell_looks.ml` — `{ style; aim; r; g; b; power }` — and
`draw_style` (`fx_core.ml:334-561`) paints it. `aim` decides *which points*
(`points_of`, `fx_core.ml:261-270`): `All_slots`, `Foe_slots`, `Own_slots`,
`Own_life`, `Target`, `Own_target`.

| style | shape | notes |
|---|---|---|
| `Stones` | 9 tumbling rock chunks per point, impact chips and dust | `u^1.7` fall, 1.5 s, **not** additive, faceted shading from a virtual upper-left light |
| `Sprout` | three stems with opening leaves and a bud | `open_²` spreads the leaves |
| `Eyes` | two tiger eyes at the life portrait | iris, pupil, specular dot |
| `Rise` | 7 motes per point rising on an ease-out | `(1−u)²` alpha |
| `Strike` | a beam from your portrait to theirs, then an impact puff | 0.28 s travel |
| `Swirl` | 3 arms × 6 dots, y squashed 0.55 | the wind spells |
| `Ice` | falling shards with a motion streak | 0.4 s fall |
| `Shadow` | hovering blobs that rise and fade | 5 per point |
| `Rays` | pulsing disc plus 6 rotating spokes | `Own_slots` / heals |
| `Drops` | 6 drops with splash rings | 0.32 s fall |
| `Fireburst` | 7 stacked discs tapering `1 − 0.72v` | the generic fire |
| `Ring` | two expanding rings of 14 dots, `0.7 s` apart | y squashed 0.7 |
| `Spikes` | 3 triangles per point growing on `ease_out` | aimed at the slot **floors**, not centres |

Every style lasts `1.05 s`, except `Stones` at `1.5 s`
(`fx_core.ml:272`), and `power` scales the size of most of them — the
exceptions are `Sprout`, `Eyes`, `Strike` and `Ice`, which are fixed-size (so
`~power:1.1` on an Ice entry in the table has no effect at all).

`rock` is worth a line because it is the only shaded primitive in the codebase:
`6 + (seed mod 3)` jittered vertices, and each facet shaded by
`0.85 + 0.6 · (−mx − my)/(1.4 · rad)` clamped to `[0.35, 1.45]` — a fake light
from the upper left, which is what stops a blob of triangles from reading as a
sphere.

### 5.2 Particles

`emitter_cfg` (`fx_particles.ml:15-32`) is a declarative record: spawn shape,
speed range, angle range in degrees, gravity, wind range, size range, colour and
alpha gradient stops, texture, rate and cap. Six emitters are defined this way
(`faerie_fire`, `acid_rain`, `nether_grasp`, `redemption`, `blood_burst`, and
`fire_wall` as a recolour of `faerie_fire`), and `nature_heal` / `pollen_*` /
`smoke_cfg` are record updates of each other.

The integrator (`tick_emitter`, `fx_particles.ml:116-130`) is plain explicit
Euler with a rate accumulator:

```ocaml
let dt = if e.last = 0. then 0.016 else Float.max 0. (age -. e.last) in
e.emit <- e.emit +. (dt *. e.cfg.rate);
while e.emit >= 1. && List.length e.puffs < e.cfg.max_particles do ...
p.vy <- p.vy -. (e.cfg.gravity *. dt);
p.px <- p.px +. ((p.vx +. p.wind) *. dt);
p.py <- p.py +. (p.vy *. dt)
```

Accumulating `rate · dt` rather than spawning per frame is what makes the
particle count independent of frame rate — at 250/s the shower spawns ~4 per
frame at 60 fps and ~8 per frame at 30 fps, and the picture is identical.
Gravity is folded in *before* the position update, which is semi-implicit Euler
and therefore stable for the large `dt` of a stalled frame.

Colour and alpha come from piecewise-linear stops over `u = (age − born)/life`;
with a texture the puff is drawn with `draw_texture_pro`, without one it is two
additive circles (a 1.35× halo at 0.28 α and the core at 0.9 α) — the same
halo/core pairing as the lightning core (§2.6), which is the visual grammar of
this codebase: **a bright small centre inside a larger dim halo**.

The emitter's lifetime is `min 3. (duration + max life)`, so the last particle
always gets to die.

### 5.3 Dedicated scenes and shaders

`fx_dedicated.ml` holds seventeen effects that do not fit a look table —
`spawn_poison` (a bezier stem, a six-petal bloom, travelling smoke),
`spawn_scythe`, `spawn_aurora`, `spawn_apocalypse`, `spawn_burial`,
`spawn_heal_haze` and so on. They share the same `mk`/`additive`/`fade`
vocabulary and the same layout vocabulary; there is nothing structurally new in
them, which is the point of having made the vocabulary small.

Four GLSL shaders are loaded from `assets/shaders/` (`fx_core.ml:147-183`):
`grey` (a desaturating post-process for the menu), `night` (`u_night` blend for
campaign), `sel` (the animated card-selection ring) and `shockwave`. Each
effect that uses one **falls back to a primitive-only version if the shader
failed to load**, which is why `spawn_shockwave` has two implementations. Uniform
locations are cached per shader in a hashtable, and a missing uniform is skipped
rather than fatal — a shader may be recompiled by a driver and lose names.

## 6. How a spell gets its effect

`fx_stage.ml` is the table, and it is the file to edit when a spell needs
something the looks table cannot express.

`for_spell layout (c : command)` runs a `match c.cmd_card.key` over 28 bespoke
keys, each returning `Some (recipe ~duration ~blocking (fun ~start -> …))`, and
falls through to the looks table:

```ocaml
| _ -> Option.map (look_recipe layout c) (Spell_looks.get c.cmd_card.key)
```

So the resolution order is: **bespoke → looks table → nothing**. A card with no
entry anywhere returns `None` and plays silently, which is the correct default
for a creature summon.

`layout` (`fx_core.ml:12-25`, filled in by `gui_board.ml:475-540`) is the only
bridge between board geometry and effects. It exposes slot centres, slot
**floors** (the bottom of the slot well, where fire and spikes belong), the
`flame_base` (38 px above the bottom of the card), the life portrait, house
header positions, the cards in a house in hand, and the queries an effect needs
(`occupied`, `slot_life`, `slot_card`, `scarecrow`). Effects never compute board
geometry themselves; if you need a new anchor, add it here.

Three spell-specific rewrites are worth knowing, because they are the effect
choosing to *disagree* with the card's aim:

- **`disciples.wind`** is aimed at the scarecrow's neighbours rather than the
  whole enemy row (`oppress_foe`, `fx_stage.ml:17-22`).
- **`Spikes` looks** are retargeted at the enemy **floors** and `Stones` at
  both floors (`fx_stage.ml:34-43`), because a rock lands on the ground and a
  spike grows out of it.
- **`elementalist.incineration`** computes its own `own`/`foe` arrays by picking
  the weakest friendly and the strongest enemy creature, and hands them to the
  generic `Fireburst` look (`fx_stage.ml:147-174`).

`for_ability layout ~kind ~player ~num ~target` is the same idea for card
abilities, keyed on a free-form `kind` string, and — notably — two of its five
cases are non-blocking.

### 6.1 A spell whose target is only known when it resolves

Most spells name their target before they are cast: the player picks a slot, and
the effect is aimed at it. A dice-rolled spell does not. The Chaos Priest's
**Unstable Healing** picks one of the caster's own creatures at random *when the
card resolves*, so the front end cannot know at cast time which creature the
healing will land on.

Such a spell must not use a `Target` look. `target_spot` (`fx_stage.ml:27`) reads
`c.cmd_input`, which is `None` for a spell with no input, and `points_of`
(`fx_core.ml:673`) then falls back to `foe.(2)` — the *opponent's* middle slot:

```ocaml
| Spell_looks.Target -> (
    match target with Some p -> [| p |] | None -> if Array.length foe > 2 then [| foe.(2) |] else own)
```

So the green motes rise on the opponent's creature while the caster's creature is
healed in silence. The look table cannot express this, which is exactly what
`custom` is for.

The spell announces the creature instead. The house effect picks the slot, heals
it and emits an `Ev_ability` naming it (`lib/houses/special/chaospriest.ml`):

```ocaml
| Some st when st.life > 0 && st.life < st.max_life ->
    focus_slot s;
    Slot.heal s (st.max_life - st.life);
    emit env.env_updater
      (Ev_ability
         {
           kind = "chaospriest.unstablehealing";
           player = env.env_player;
           num = s.num;
           target = Some (env.env_player, s.num);
         })
```

and the board draws it there (`gui/fx_stage.ml`):

```ocaml
| "chaospriest.unstablehealing" ->
    Option.map
      (fun (tp, tn) ->
        recipe ~sound:"naturescare" ~duration:nature_care_duration ~blocking:true (fun ~start ->
            spawn_nature_care [ layout.slot_center tp tn ] ~start))
      target
```

Two things follow from this shape. The key moves from `looks` to `custom` in
`lib/game/spell_looks.ml`, because the spell no longer has a cast overlay at
all: the card is drawn when the effect resolves, not when it is played. And the
announcement is conditional — the event is emitted only when a creature was
actually healed, so a spell that finds nothing to heal is silent rather than
flashing on a creature that did not change.

An ability is where an effect gets a **sound of its own**. A spell's clip comes
from the card's English name (`gui/gui_draw.ml:266`), but an ability fires on a
creature and has no card to be named after, so the recipe carries the clip name
instead and the board plays it when the effect starts:

```ocaml
| "meteomancer.grenadier" ->
    Option.map
      (fun (tp, tn) ->
        recipe ~sound:"scatterbomb" ~duration:grenade_duration ~blocking:true (fun ~start ->
            spawn_grenade ~src:(layout.slot_center player num) ~target:(layout.slot_center tp tn)
              ~pid:tp ~num:tn ~start))
      target
```

The name is a file in `assets/sounds/`, so `~sound:"scatterbomb"` is
`assets/sounds/scatterbomb.mp3` — the tinker's Scatterbomb clip, reused here
because a grenade landing and a bomb landing are the same noise.

## 7. Writing a new effect

A complete effect is about ten lines. To make a spell called `fire.tide` burn
the foe's leftmost occupied slot:

```ocaml
(* gui/fx_flames.ml, or a new file opened from gui/dune *)
let tide_duration = 1.1

let spawn_fire_tide ~at ~start =
  mk ~start ~duration:tide_duration (fun ~age ->
      let fade = Float.min 1. ((tide_duration -. age) /. 0.2) in
      if fade > 0.02 then
        additive (fun () ->
            let env = envelope age 0.1 0.5 0.3 in
            flame_column ~x:at.x ~y:at.y ~height:(90. *. env) ~fat:1.2 ~hot:1.
              ~alpha:(fade *. env) ~t:age ~id:1 ~licks:4 ~push:0.))
```

and the binding, in `fx_stage.ml`:

```ocaml
| "fire.tide" ->
    Option.map (fun n -> recipe ~duration:tide_duration ~blocking:true (fun ~start ->
        spawn_fire_tide ~at:(layout.slot_floor other n) ~start)) c.cmd_input
```

Then add `"tide"` to `demo_names` and a line to `spawn_demo`, so it can be
inspected without setting up a game.

Checklist, in the order that actually catches mistakes:

- [ ] **Take every position from `layout`.** `slot_center` for a bolt, `slot_floor`
      for ground effects, `life` for anything aimed at a player, `source_of` for
      "from the card I just played".
- [ ] **Use `fade` for the tail and per-element `born` for the head.** A single
      `fade` over the whole thing is flat; the layering is what sells it.
- [ ] **`additive` for anything that is light**, and only for that. `Stones`,
      `Sprout`, `Eyes` and `Spikes` are opaque objects.
- [ ] **Give the effect a `duration` constant and use it for both the timeline
      and the fade.** No literals in two places.
- [ ] **Set `blocking` honestly.** `true` iff the picture should delay the next
      animation. Blocking effects stack: three blocking spells in one turn is
      four seconds of nothing happening.
- [ ] **Add it to `demo_names`.** Press **F8** in a duel to cycle through every
      demo effect (`bin/gui.ml:317-320`) — it works in any game state, which is
      the only practical way to check timings.
- [ ] **Name a clip if the effect makes a noise.** `~sound:"<file>"` on the
      recipe, for an ability (a spell's sound comes from the card's name).
      The clip starts with the animation, so its own lead-in is the delay.
- [ ] **Check the target is known when you draw.** A `Target` look aims at
      `c.cmd_input`, and a spell with no input falls back to the *opponent's*
      middle slot (§6.1). If the card rolls its target, have the house emit an
      `Ev_ability` naming it and draw from that instead.

Two cheap performance rules learned from the existing code: derive any step
count from a length (`steps = clamp(6, h/7, 14)`) rather than hard-coding a
loop bound, and let the primitives' sub-pixel culling do the rest.

## 8. Gotchas

- **Two clocks are a feature, not a bug.** Both the lightning flicker and the
  flame column use a slow sway plus a fast flicker at unrelated frequencies. A
  single frequency reads as a loop.
- **`noise` is for stable variation, `Random.State` for per-frame change.**
  Hashing the frame index makes noise; using an RNG for a per-column property
  makes static.
- **Additive passes sum, so watch the alpha arithmetic.** Several places
  deliberately pass an alpha above 1 (the flame core, `paint … 1.2 1.`);
  `byte` clamps it, which is the intent.
- **`draw_all` is called after the cards** (`bin/gui.ml:398`) so bolts overlay
  them. If a new effect looks like it is *under* the cards, it is being drawn
  from the wrong place, not with the wrong blend mode.
- **Shake frequency can exceed the frame rate** (up to 80 Hz at `strength = 1`,
  though §4.5 shows the visible peak is ~60 Hz). Do not raise `amp` expecting a
  bigger rumble; you will get aliasing.
- **A blocking effect's `duration` is a promise.** Get it wrong and the game
  waits on an invisible frame.
- **`Fx.clear` exists because two effects have infinite duration.** Any new
  effect that never expires is a rematch bug.
- **Longest hop caveat:** `gens` saturates at 6, so past 512 px of chord the
  finest generation only adds vertices and the bolt looks calmer than the design
  intends (§2.3).
- **The look table is data, not code.** `lib/game/spell_looks.ml` is the cheapest
  way to give a spell a decent animation; write bespoke code only when the card
  needs something the table cannot express, and check §6 for the three cases
  where that distinction is already blurred.

## Where to go next

- The spell choreography, effect by effect, against the Scala original:
  [effects port plan](../tech/effects-port-plan.md).
- The engine events that start all of this (`Ev_spell_played`, `Ev_ability`,
  `Ev_focus`): [document 1](01-architecture.md).
- The board layout the effects are anchored to:
  [document 2](02-game-view.md) for state, `gui_board.ml` for geometry.
- And back to the bots: [document 3](03-search-concepts.md).