# 8. The lobby ocean

The lobby can play an endless, procedural beach: waves, gusting wind, no audio
file anywhere. It is one file, `gui/ocean.ml`, about 150 lines, and it is a port
of a small browser toy (the "beach wave loop generator"). This document walks
through it in the order the signal flows: noise → shaping → filter → buffer →
stream.

Turn it on or off with **Settings → Sound → Ocean ambience in the lobby**. It is on by
default (`sound.ocean` in the prefs file, `Prefs.ambient_sound`) and plays only
while the lobby is on screen.

## 1. The big idea: a sound is a function of time

There is no sample to loop. Every output sample is computed from one number, the
time `t` in seconds:

```text
sample(t) = lowpass( waves(t) + wind(t) )
```

and every ingredient of `waves` and `wind` is a smooth function of `t`. So the
sound can run forever with no join, and "where are we in the loop" is just an
integer frame counter (`frame`, 44 100 per second).

The browser version also had a WAV export that needed a crossfade to be
seamless. We do not export, so that whole part is gone.

## 2. Ingredient one: value noise

Waves are noise, so we need noise that is the same on every run and smooth
enough not to hiss like static. `make_noise seed` builds it:

1. Fill a 4096-entry table with random numbers in [-1, 1] from a tiny linear
   congruential generator (`s ← s·1664525 + 1013904223 mod 2³²`). Same seed,
   same table, every run.
2. To read the noise at position `x = t · 4096`, take the two neighbouring table
   entries `a` and `b` and blend them with a **smoothstep**, `u = f²(3 − 2f)`
   where `f` is the fractional part of `x`.

```ocaml
let u = f *. f *. (3. -. (2. *. f)) in
a +. ((b -. a) *. u)
```

Smoothstep has zero slope at both ends, so the curve joins its neighbours
without a kink. That interpolation is a gentle low-pass in itself: the result is
random but rounded, not a stream of independent random samples.

The table index is taken `land 4095`, so it **wraps**. The noise repeats; how
fast depends on how quickly we walk through it (next section).

Each channel and each wind layer gets its own table (seeds 12345, 67890, 24680,
13579, 55555, 77777), so left and right are not copies of each other.

## 3. Ingredient two: waves

```ocaml
let l = o.n1 ((t *. 3.1) +. (sin (t *. 0.02) *. 0.5)) *. env *. surge *. 0.5
```

Three pieces multiply together.

**Noise, read fast.** The argument is `t · 3.1`, so we walk the table at
3.1 × 4096 ≈ 12 700 entries per second. That is about 3.5 output samples per
table entry, which makes the noise a soft rushing hiss. The table has 4096
entries, so it wraps every 4096 / 12 700 ≈ **0.32 s**. The extra
`sin (t · 0.02) · 0.5` wobbles the read position slowly, so the repeat drifts
instead of locking to a fixed beat. The right channel reads at 2.9 instead of
3.1 and has its own phase, which keeps the two ears decorrelated.

**`envelope t`: the swell.** Three slow sines with periods of about 14.1 s
(0.071 Hz), 23.3 s (0.043 Hz) and 8.5 s (0.117 Hz). Their frequencies are not
multiples of each other, so the sum never repeats in any useful time:

```ocaml
let e = (((a +. b +. c) /. 3.) +. 1.) /. 2. in   (* squash into 0..1 *)
Float.pow e 1.6
```

The power 1.6 bends the curve so that low values get lower: long near-silent
lulls, shorter strong surges. When `e = 0.5` the envelope is only
0.5^1.6 ≈ 0.33.

**`surge`: the push and pull.** `0.6 + 0.4·sin(2π · 0.35 · t)`: a 0.35 Hz wobble
(one cycle every 2.9 s) that never goes below 0.2 or above 1. That is the
individual wave breaking and draining inside the slow swell.

A worked sample, at `t = 0`:

| term | value |
|---|---|
| sin 1.3, sin 4.1, sin 0.7 | 0.964, −0.818, 0.644 |
| mean of the three | 0.263 |
| rescaled to 0..1 | 0.632 |
| `envelope` = 0.632^1.6 | **0.479** |
| `surge` = 0.6 + 0.4·sin 0 | **0.6** |

so the noise is scaled by 0.479 × 0.6 × 0.5 ≈ 0.14.

## 4. Ingredient three: wind

`wind_sample lo mid t` mixes two noise layers under a gust envelope:

```text
gust(t)   three sines (19 s, 11 s, 43 s), rescaled to 0..1, power 1.3
whoosh    lo  read at 0.55·t   -> table wraps every ~1.8 s: a deep, slow body
whistle   mid read at 4.7·t    -> wraps every ~0.21 s: a higher hiss
wind = (0.75·whoosh + 0.25·whistle·gust) · (0.35 + 0.65·gust) · 0.45
```

Two things make it sound like wind and not a second wave track:

- the **whistle** is multiplied by the gust, so the high hiss only appears when
  a gust is up;
- the **whole thing** is scaled by `0.35 + 0.65·gust`, so even in a lull there
  is a faint breeze, and it never goes silent.

The result is multiplied by `wind_level` (0.35). The right channel is evaluated
at `t + 0.37`, so a gust reaches one ear 0.37 s after the other, which is where
the stereo width comes from.

## 5. The filter: dark when calm, bright when the surf is up

The sum then goes through a low-pass whose cutoff follows the swell:

```ocaml
let k = pole (500. +. (2500. *. env)) in
```

At `env = 0` the cutoff is 500 Hz (muffled, distant); at `env = 1` it is 3 kHz
(crisp surf). A real wave sounds brighter as it breaks, and this reproduces that
with one line.

The filter is two **one-pole low-pass** stages in series. One stage is

```ocaml
s ← s + k · (x − s)            with   k = 1 − exp(−2π · fc / sample_rate)
```

which means "move the state a fraction `k` of the way toward the input". Two in
series give a 12 dB-per-octave roll-off, which is close to the Web Audio biquad
the original used. It is an approximation, not an identical filter.

At `t = 0` the envelope is 0.479, so the cutoff is 500 + 2500 × 0.479 ≈ 1 700 Hz
and `k` = 1 − exp(−2π · 1700 / 44100) ≈ 0.215. At the two extremes `k` is 0.069
(500 Hz) and 0.348 (3 kHz). Each channel keeps its own pair of states
(`lp_l`, `lp_r`), and the state carries across chunks. If it did not, there would
be a click at every chunk boundary.

`k` is recomputed for **every sample**. Computing it once per chunk (what the
first version did) steps the cutoff every 186 ms, and the steps are audible.

## 6. Volume, headroom and fades

```ocaml
let gain = 0.9 *. intensity *. volume          (* 0.9 · 0.8 · sound level *)
let out x = ... x *. gain *. f *. 32767. *. 2.
```

`volume` is the Level from the sound tab (0..1), `f` the fade (below), and the
final `2.` is a make-up gain. The worst-case signal before the filter is
0.5 (waves) + 0.158 (wind: 1.0 · 0.45 · 0.35) = 0.658, and 0.658 × 0.72 × 2 =
0.95, so full volume stays just under full scale. The `max`/`min` clamp is there
only as a guard.

**Fade.** Entering the lobby with the sound suddenly at full is jarring.
`fade` moves toward its target (1 when active, 0 when not) by 0.3 per chunk up
and 0.5 per chunk down, and is **interpolated linearly across each chunk**, so
the gain ramps smoothly instead of stepping. Fade-in takes about 0.75 s,
fade-out about 0.4 s.

**Level refresh.** The sound level lives in the prefs file. Reading a file on
every chunk would be wasteful, so `update` re-reads it every fifth chunk
(about once a second).

## 7. Getting the samples to the speakers

Synthesis is the easy half. The other half is raylib's streaming API, and it has
one trap that cost a few rounds of debugging.

```ocaml
R.set_audio_stream_buffer_size_default chunk;
let stream = R.load_audio_stream sample_rate 16 2 in   (* 44.1 kHz, 16 bit, stereo *)
R.play_audio_stream stream
```

A raylib stream owns **two sub-buffers** and plays one while you refill the
other. `is_audio_stream_processed` turns true when the device has finished one
and wants the next. Then you hand it exactly one sub-buffer of audio:

```ocaml
R.update_audio_stream o.stream (Ctypes.to_voidp (Ctypes.CArray.start o.buf)) chunk
```

The trap: `set_audio_stream_buffer_size_default` sets the size of **one
sub-buffer**, not of the whole stream. If the two numbers disagree you do not
get an error. Update with half a sub-buffer and the rest is stale or silent,
which sounds exactly like "different samples with a pause between". Hence
`chunk` appears in both calls.

`chunk = 8192` frames is 8192 / 44100 ≈ 186 ms. That is also the whole
tolerance for a slow frame: the game loop runs at 60 fps and refills only when
raylib asks, so one slow frame (a bot search finishing, a heavy draw) must take
less than the audio already queued. The first version used 2048-frame chunks
(46 ms), too short to ride out a stall.

`buf` is a `Ctypes.CArray` of `int16` holding the left and right samples
interleaved (`2·i` and `2·i + 1`), which is the layout raylib expects for stereo.

Everything is driven from the main loop, not from an audio thread:

```ocaml
(* bin/gui.ml, once per frame *)
Ocean.update ~active:(match !screen with Lobby l -> l.gs.ocean | _ -> false)
             ~volume:sound_level
```

`update` creates the stream lazily the first time it is wanted, keeps filling it
while `active` or while the fade-out is still audible, and goes quiet (no more
refills) once the fade reaches 0. `unload` frees the stream at shutdown, before
the audio device closes.

## 8. What to change, and what it costs

All the knobs are constants at the top of `gui/ocean.ml`:

| constant | effect |
|---|---|
| `intensity` (0.8) | overall level of the whole mix |
| `wind_level` (0.35) | wind against waves; 0 removes the wind |
| `speed` (1.0) | stretches the swell: 2.0 doubles the wave tempo |
| `chunk` (8192) | latency vs. robustness; do not go below ~4096 |

Things worth knowing before you tune:

- `speed` affects the wave swell and surge only. The gust envelope ignores it,
  as in the original.
- Because each noise table wraps, the texture has a faint 0.32 s periodicity in
  the waves. The slow phase wobble hides most of it. A longer table (8192 or
  16384 entries) would push the repeat out further; both `land 4095` and the
  `4096.` would change together.
- The synthesis costs a handful of `sin`, one `pow`, one `exp` and the noise
  reads per sample, about 44 100 times a second. That is far below a frame's
  budget, which is why it can live on the render thread.

## 9. Differences from the HTML original

| original | here |
|---|---|
| `ScriptProcessor`, 4096-frame blocks | raylib `AudioStream`, 8192-frame blocks |
| Web Audio biquad low-pass, Q 0.4, cutoff smoothed with a 0.2 s time constant | two one-pole stages, cutoff recomputed per sample |
| sliders for intensity, wind, speed, length | constants |
| WAV export with a 3 s crossfade | none: the stream is endless |
| spectrum bars | none |

## Try it

```sh
dune build
dune exec ./bin/gui.exe
# lobby -> Settings -> Sound -> tick "Ocean ambience in the lobby"
```

To hear what one ingredient contributes, set `wind_level` to `0.` (waves only),
or set the `env` used for the cutoff to a constant in `fill` (no brightening).
