# 10. The Falconer's bird: the maths of a curved path

`spawn_falcon` (`gui/fx_dedicated.ml`) is the Disciples' **Falconer** ability.
The bird leaves the falconer's stone, sweeps a wide loop through the turn,
then crosses the whole enemy row, stooping at every creature it passes, and
flies off the board. There is no return leg.

Document 7 §5.3 lists this function among the seventeen one-off
"choreographies". This document takes it apart, because it is a small,
complete example of something you will want again: **making a moving thing
look like it has weight** — one path, no physics engine, only a curve and a
clock.

The function is about 120 lines and answers four questions:

1. **Where is the bird?** A path built from a cubic and a circle (§3, §4).
2. **Does the path have kinks?** No — the pieces meet tangent to each other
   (§4).
3. **How fast does it move?** Not at constant distance: it slows through the
   turn and accelerates on the straight (§6).
4. **When is it over a card?** The stoop rings are timed by inverting the
   straight run (§7).

## In one minute

- The path is drawn in a **local frame** where the falconer is always below
  the foe row; one sign flip (`sy`) mirrors it for a caster on the other side
  (§2).
- The turn is a **circle of radius 85** hung below the left foe slot, swept
  **270°** so the bird arrives heading down and leaves heading right (§3).
- The lead-in from the falconer is a **cubic Bézier** whose two tangents are
  chosen at the ends; the shared handle length makes it meet the circle
  smoothly (§4).
- The bird does **not** move at constant speed. Each sample carries a *time
  cost* `ds / speed`, and `speed` rises as the heading stops changing — so
  equal time steps are worth fewer pixels in the turn (§6).
- Everything else hashes off this: `at_cost` turns a cost into a position,
  `crossing` turns a card's x into a cost (§7).

### Words used in this document

| word | plain meaning |
|---|---|
| **path parameter** `u` | a number from 0 to 1 that picks a point on a curve |
| **Bézier** | the smooth curve through a start, an end and two control points; a point on it is a weighted average of the four |
| **tangent direction** | the direction the curve is heading at a point; if two pieces share it at a join, the join is invisible |
| **arc length** | distance measured along the curve, not straight through the air |
| **local frame** | coordinates where a problem is simple; a transform maps them back to the screen |
| **time cost** | a fake length where a slow segment is counted as longer; equal steps of cost take equal time |

## 1. What the board hands in, and an example

`fx_stage.ml` calls the function from its `"disciples.falconer"` case. It
measures the foe row once and passes plain geometry, no card objects:

```ocaml
let left  = Array.fold_left (fun a s -> Float.min a s.x) row.(0).x row in
let right = Array.fold_left (fun a s -> Float.max a s.x) row.(0).x row in
let foe_span = (right -. left) /. 2. in
```

| argument | meaning |
|---|---|
| `from_` | the falconer's own slot centre — where the bird starts |
| `foe_x`, `foe_y` | the centre of the foe row |
| `foe_span` | half the row's width, so `foe_x − foe_span` is the **left** slot |
| `targets` | the centres of the enemy creatures, which get a stoop ring |

`foe_span` is measured from the row centre to a slot centre, so
`foe_x − foe_span` is the leftmost slot — even if it is empty. The bird always
enters the row at that first *slot*, not at the first occupied creature.

The numbers below use the real board layout. Cards are `90×102` with a `10`
gap, so a 5-slot row has centres 100 apart, the row's left edge is
`832 − 6 − 490 = 336` (`vw = 832`, `pane_margin = 6`,
`gui_board.ml:396-408`), and the row centres sit at `66 + 102/2 = 117`
(opponent) and `224 + 102/2 = 275` (player).

| quantity | value |
|---|---|
| falconer `from_` | `(581, 275)` — the middle player slot |
| foe row | `foe_x = 581`, `foe_y = 117` |
| `foe_span` | `200` (five slots) |
| `entry = foe_x − foe_span` | `(381, 117)` |
| `exit_x = world_w + 70` | `902` |
| `world_w` | `832` (`fx_core.ml:43`) |

## 2. A local frame, so one geometry serves both seats

The falconer is either on the player's side (below the row) or the opponent's
(above it). Rather than write two paths, the code mirrors one:

```ocaml
let sy = if home.y >= foe_y then 1. else -1. in
let hx = home.x -. entry.x in
let hy = (home.y -. foe_y) *. sy in
let to_world x y = (entry.x +. x, foe_y +. (sy *. y)) in
```

In this frame the **entry is the origin**, and `y` grows *toward the falconer*.
For the player `sy = +1` and the falconer is below; for the opponent `sy = −1`
and it is above. The loop is drawn once and always hangs on the caster's side
of the row, which is what you want from the player's seat.

Numbers for the example (`sy = +1`):

| world | local |
|---|---|
| `from_ (581, 275)` | `home (200, 158)` |
| `entry (381, 117)` | `(0, 0)` |

From here on, everything is local. `to_world` maps a local point back, and it
is applied to every sample before drawing.

## 3. The turn: a circle, and 270° of it

The turn is a circle of radius `r = 85` whose centre is one radius *below* the
entry:

```ocaml
let r = 85. in
let t = { x = r; y = r } in
let loop u =
  let ph = 1.5 *. Float.pi *. u in
  to_world (r *. cos ph) (r +. (r *. sin ph))
in
```

A point at angle `ph` is `(r cos ph, r + r sin ph)`: the centre `(0, r)` plus
`r` in the direction `ph`.

| `ph` | local point | world point | what it is |
|---|---|---|---|
| `0` | `(r, r)` | `(466, 202)` | right of the centre — the loop **starts** here |
| `π/2` | `(0, 2r)` | `(381, 287)` | bottom, under the entry |
| `π` | `(−r, r)` | `(296, 202)` | left of the centre |
| `3π/2` | `(0, 0)` | `(381, 117)` | the **entry** |

`loop u` runs `ph = 1.5π·u`, from `0` to `3π/2` — a **270°** sweep. Calling
`t = (r, r)`, the bird arrives at the circle's right point and leaves at the
entry.

The tangent of a circle point is `P′(ph) = r(−sin ph, cos ph)`, so:

- at the start, `ph = 0`: tangent `(0, r)` — straight *down* in the local
  frame, away from the row and toward the caster's side;
- at the entry, `ph = 3π/2`: tangent `(r, 0)` — straight **+x**, exactly the
  direction of the straight run that follows.

That last fact is the reason for the 270°: the bird leaves the loop *already*
pointing along the run, so the join into the straight is seamless. A 180°
half-circle would leave the entry pointing +x too, but it would have to start
at the loop's *bottom* moving left, and the lead-in from the falconer would
arrive moving left — a sharper hand-off. The extra quarter buys the bird a dip
below the row and a downward arrival, which is what makes the turn read as a
bank rather than a snap.

## 4. The lead-in: a cubic with two chosen tangents

The bird has to get from the falconer to `t = (r, r)`, arriving *tangent to the
circle*. A cubic Bézier gives exactly that control:

```
B(u) = (1-u)^3 P0 + 3(1-u)^2 u P1 + 3(1-u) u^2 P2 + u^3 P3 .
```

In the code it is the helper `bezier` (`fx_dedicated.ml:130`):

```ocaml
let bezier a b c d u =
  let o = 1. -. u in
  (o *. o *. o *. a) +. (3. *. o *. o *. u *. b) +. (3. *. o *. u *. u *. c) +. (u *. u *. u *. d)
```

called once per axis. The endpoints are fixed, `P0 = home` and `P3 = t`. The
two inner control points set the two end tangents, because

```
B'(0) = 3(P1 - P0),   B'(1) = 3(P3 - P2).
```

The code picks the tangents and then one shared handle length:

```ocaml
let l = Float.max 1e-3 (hypot (t.x -. hx) (t.y -. hy)) in
let sd = hypot hx hy in
let sdx, sdy = if sd <= 0. then (0., -1.) else (-.hx /. sd, -.hy /. sd) in
let p1 = { x = hx +. (sdx *. l /. 3.); y = hy +. (sdy *. l /. 3.) } in
let p2 = { x = t.x; y = t.y -. (l /. 3.) } in
```

- **Start:** `(sdx, sdy)` is the unit direction from the falconer to the
  **entry** (`(−hx, −hy) / |home|`), and the handle is `l/3` long:
  `P1 = home + (l/3) · dir`. So the bird leaves aiming at the slot.
- **End:** the circle's tangent at `t` is `(0, r)` — straight down — and the
  handle is again `l/3`: `P2 = t − (l/3) · (0,1) = (r, r − l/3)`. So it arrives
  moving straight down, which is what the circle expects.
- `l = |home − t|` is the **chord** between the endpoints. Using the chord as
  the handle scale, rather than the two real tangent distances, is the cheap
  Hermite choice: it is stable for any input and never doubles the curve back
  on itself.

The `if sd <= 0.` guard is the degenerate case of a falconer directly above
the entry — a vertical approach whose x-component would divide by zero.

Example numbers (local): `home (200, 158)`, `t (85, 85)`, `l = 136.2`,
`P1 = (164.4, 129.9)`, `P2 = (85, 39.6)`. In world coordinates the control
polygon runs `P0 (581, 275) → P1 (545, 247) → P2 (466, 157) → P3 (466, 202)`;
the circle then takes over at `P3`.

## 5. Sampling: three pieces, one array

The three pieces are sampled to fixed steps and stitched into one array `pts`:

```ocaml
let lead_steps = 64 and loop_steps = 80 and run_steps = 64 in
let curve_steps = lead_steps + loop_steps in
let n = curve_steps + run_steps in
let pts = Array.init (n + 1) (fun i -> ... ) in
```

`lead_steps = 64`, `loop_steps = 80`, `run_steps = 64`, so `n = 208` and there
are 209 points. The straight run is a uniform line in *x*:

```ocaml
let u = float (i - curve_steps) /. float run_steps in
(entry.x +. ((exit_x -. entry.x) *. u), foe_y)
```

which is what later lets a card's **x** be converted straight into a time (§7).
`loop_steps = 80` gives circle segments of `1.5π/80 ≈ 0.0589 rad` each — not an
accident; it is the number that makes the circle the slow zone in §6.

## 6. Timing: constant time, not constant distance

If the bird moved one fixed distance per frame, the circle would fly by and
the straight would crawl. Instead each sample carries a **time cost**:

```ocaml
let wrap a =
  let a = Float.rem a (2. *. Float.pi) in
  if a > Float.pi then a -. (2. *. Float.pi)
  else if a < -.Float.pi then a +. (2. *. Float.pi)
  else a
in
let cum = Array.make (n + 1) 0. in
let prev = ref 0. and first = ref true in
for i = 1 to n do
  let (x0, y0) = pts.(i - 1) and (x1, y1) = pts.(i) in
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let ds = hypot dx dy in
  let ang = atan2 dy dx in
  let turn = if !first then 0. else Float.abs (wrap (ang -. !prev)) in
  prev := ang;
  first := false;
  let speed = 1. +. (0.9 *. (1. -. Float.min 1. (turn /. 0.06))) in
  cum.(i) <- cum.(i - 1) +. (ds /. speed)
done;
```

Read it as three steps:

1. **How much did the heading change?** `ang` is the direction of the segment;
   `turn = |wrap(ang − prev)|` is how much the bird turned, folded into
   `(−π, π]` so a wrap past `π` does not read as a huge turn.
2. **Turn that into a speed.** `speed = 1 + 0.9·(1 − min 1 (turn/0.06))`. A
   straight (`turn = 0`) runs at `1.9`; anything that turns by `0.06 rad`
   (≈ 3.4°) or more runs at `1.0`. `0.06` is the *cornering scale*.
3. **Charge the time.** `cum.(i) = cum.(i−1) + ds/speed`. A short slow segment
   and a long fast one can cost the same.

Because the segments are short, `turn ≈ curvature · ds`, so `speed` is really
a curvature detector: high-curvature spots are slow. The straight has
`turn = 0` with `ds = 8.1 px`; the circle turns `0.0589 rad` every `5.0 px` —
just under the `0.06` threshold, so the whole loop stays at ≈ `1.0` while the
straight runs at `1.9`.

Now `cum` is a monotone "fake length" from `0` to `total = cum.(n)`. Two
helpers finish the job:

```ocaml
let at_cost s =
  ... while !i < n && cum.(!i + 1) < s do incr i done;
  (* then linear interpolation between pts.(!i) and pts.(!i+1) *)
```

finds the point at a given cost, and

```ocaml
let at age =
  let s = total *. Float.max 0. (Float.min 1. (age /. falcon_duration)) in
  at_cost s
in
```

maps the clock to a position. The bird travels the whole `total` of cost in
exactly `falcon_duration` seconds, so equal slices of time are equal slices of
*cost* — hence few pixels where turning is expensive.

Worked numbers for the example, `falcon_duration = 1.0 s`:

| piece | length | cost | share of the time |
|---|---|---|---|
| lead cubic | 155 px | 98 | 13 % |
| circle | 402 px | 392 | 51 % |
| straight run | 521 px | 275 | 36 % |
| **total** | **1078 px** | **765** | **1.0 s** |

The straight is the longest piece (48 % of the distance) but costs the least
time (36 %), because its segments run at `1.9` while the whole loop runs at
`1.0`. On average the bird covers ≈ 870 px/s through the turn and ≈ 1450 px/s
on the straight — a ratio below the per-segment `1.9`, because the lead cubic's
speed ramps.

## 7. The stoop rings: inverting the straight run

Each enemy creature gets a ring the moment the bird passes over it. The bird
is over a slot only on the straight run, and the straight run is uniform in
`x`, so the index of the sample above a card is linear in its `x`:

```ocaml
let crossing (t : spot) =
  if t.x < entry.x -. 1. || t.x > exit_x then None
  else
    let k =
      float curve_steps +. ((t.x -. entry.x) /. (exit_x -. entry.x) *. float run_steps)
    in
    let i = int_of_float (Float.floor k) in
    let f = k -. float i in
    let c = if i >= n then cum.(n) else cum.(i) +. (f *. (cum.(i + 1) -. cum.(i))) in
    Some (c /. total)
in
```

`k` is the fractional sample index, `c` interpolates its cost from `cum`, and
the result is a **time fraction** `c / total`. The draw loop turns it into a
moment:

```ocaml
let strike = p *. falcon_duration in
let window =
  Float.min 0.42 (Float.max 0.14 (falcon_duration -. strike -. 0.02))
in
```

`strike` is the age at which the bird is above that slot. `window` is how long
the ring lives, clamped so a ring near the end still fits:
`falcon_duration − strike − 0.02` is the time left (minus a small tail),
floored at `0.14 s` so it is visible and capped at `0.42 s` so it does not
linger. The five foe slots of the example strike at ages
`0.64, 0.71, 0.78, 0.85, 0.92 s` — the spacing is the straight run's constant
speed.

If a target is not on the run — an imaginary card left of the entry, or beyond
the exit — `crossing` returns `None` and no ring is drawn. The `entry.x − 1`
slack absorbs the round-off at the very first slot.

## 8. The trail, the fade and the bird

The bird is drawn at `at age`, and its heading from a point one tick ahead:

```ocaml
let (px, py) = at age in
let (qx, qy) = at (age +. 0.03) in
let ang = atan2 (qy -. py) (qx -. px) in
```

so the sprite always points where it is going. Behind it, five ghost circles
sample *past* positions, `at (age − k·0.032)` for `k = 1…5`, each smaller and
fainter — a motion smear made of the same path function.

The whole effect fades over its last `0.18 s`:

```ocaml
let fade = Float.min 1. ((falcon_duration -. age) /. 0.18) in
if fade > 0.02 then ( ... )
```

and the wing beat is just `sin (age *. 19.)` — about 3 Hz — passed to
`draw_falcon`, which uses it to widen the wings and lighten the grey.

## 9. The knobs

| change | where | effect |
|---|---|---|
| `falcon_duration` | top of the falcon block | total blocking time (1.0 s) |
| `r = 85.` | `spawn_falcon` | loop radius; larger = wider, longer turn |
| `1.5 *. Float.pi` | `loop` | sweep; `1.0` would be a 180° half-loop |
| `turn /. 0.06` | timing | cornering scale: bigger = fewer spots count as a turn |
| `0.9` in `speed` | timing | how much faster a straight is than a turn |
| `lead_steps`, `loop_steps`, `run_steps` | sampling | resolution of each piece |
| `world_w +. 70.` | `exit_x` | how far past the edge the bird flies |
| `0.42` / `0.14` | ring `window` | longest / shortest stoop ring |
| `0.18` | `fade` | length of the fade-out tail |

Two warnings. Changing `loop_steps` changes the circle's per-segment turn
`1.5π/loop_steps`: it is `0.0589` at the default, so the whole loop reads as
one slow arc; sample it much more finely and the loop's share of the time
starts to drift away from the straight's. And `falcon_duration` is also the
`recipe`'s blocking duration (`fx_stage.ml`), so the next animation waits
exactly this long: if you shorten the path, shorten the constant with it, or
the turn will hang at the end.

## Where to go next

- Document 7 §5.3 for the rest of `fx_dedicated.ml` and the shared vocabulary.
- Document 7 §1 for the timeline and what `mk ~start ~duration` means.
- `gui/fx_core.ml` for `spot`, the `spot`-shaped `bezier` at the bottom,
  `circle`, `ring`, `additive` and the transform.
