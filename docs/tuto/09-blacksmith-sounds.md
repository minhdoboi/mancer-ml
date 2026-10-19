# 9. The blacksmith sounds

Two Blacksmith cards — **Bargulian Gauntlet** and **Excalibur** — have sound
clips whose whole point is to line up with a drawn animation: the punch has to
land on the frame the fist reaches the far lane, and the tear has to happen
when the blade comes out the other side. Nothing you can download fits that, so
the clips are **synthesised from noise and filters** by a small Python script,
`scripts/gen_blacksmith_sounds.py` (167 lines):

```sh
python3 scripts/gen_blacksmith_sounds.py
# wrote .../assets/sounds/barguliangauntlet.mp3 1.65s
# wrote .../assets/sounds/excalibur.mp3 2.05s
```

It needs `python3` with **numpy**, and **ffmpeg** with `libmp3lame` (there is no
mp3 encoder in the Python standard library, so the script writes a plain 16-bit
WAV and lets ffmpeg compress it). Nothing else in the build depends on either —
the script is run by hand, and the game only ever sees two `.mp3` files.

## 1. What the game looks for

A card sound is not keyed by the card key. On `Ev_spell_played` the board plays
`play_card_sound` (`gui/gui_board.ml:126`), and the name is built from the
**English** card name — lowercased, spaces stripped
(`gui/gui_draw.ml:261`):

```ocaml
let card_sound_name (c : card) =
  Assets.english_card_name c |> String.lowercase_ascii |> String.split_on_char ' ' |> String.concat ""

let play_card_sound (c : card) = play_sound ~volume:0.9 (card_sound_name c)
```

`Assets.sound` then looks for `assets/sounds/<name>` with the extensions
`.mp3`, `.ogg`, `.wav` (`lib/game/assets.ml:109`). So:

| card | English name | file |
|---|---|---|
| `blacksmith.excalibur` | `Excalibur` (`assets/i18n/en.properties:1275`) | `assets/sounds/excalibur.mp3` |
| `blacksmith.barguliangauntlet` | `Bargulian Gauntlet` (`:1277`) | `assets/sounds/barguliangauntlet.mp3` |

Two consequences worth remembering. A name that only exists in another locale
never plays, because the lookup is the English string; and the rule is crude —
only spaces are stripped, so the Blacksmith's `Wizard's Hat` would look for
`wizard'shat.mp3`, apostrophe and all. The rest of the house (Mithril Spear,
Mithril Bow, Mithril Shield, Flying Chariot, Pegasus, Cloak and Dagger) has no
clip at all: `Assets.sound` returns `None` and the play is skipped, so casting
those cards is simply silent.

The volume is `0.9 · sound_level ()`, the Level slider from Settings → Sound
read from the prefs file. Sounds are decoded once into a `Hashtbl` cache
(`gui/gui_draw.ml:233`) and unloaded at shutdown (`bin/gui.ml:499`).

Both cards fire from `card_effect`, when the wielder **comes into play**
(`lib/houses/special/blacksmith.ml:276` and `:299`), so to hear a clip you play
the card. **F8** cycles `Fx.demo_names`, but `Fx.spawn_demo` only spawns the
visuals — it emits no event, so F8 shows the animation in silence.

## 2. Why the clip is not a hit

The sound and the animation both start on the same `Ev_spell_played` event, so
the clip is written as a **short piece with a wind-up**, not as a single impact.
The wind-up is not free time: the gauntlet draws the fist back for 0.42 s and
punches for 0.16, and the sword is forged for 0.85 s, held, let go at 1.15 s,
and needs 0.22 s to cross the lane.

Every number the script uses is one of those constants, copied by hand:

| visual (`gui/fx_dedicated.ml`) | value | audio (`scripts/gen_blacksmith_sounds.py`) |
|---|---|---|
| `wind +. punch` (`:613`) | 0.58 | `hit = 0.58`, every impact layer of the gauntlet |
| `gauntlet_duration` (`:609`) | 1.65 | `d = 1.65`, the length of the gauntlet buffer |
| `forge` (`:440`) | 0.85 | the four bell partials last 0.85 s, and the 0.5 s shimmer starts at 0.35 |
| `excalibur_lunge` (`:436`) | 1.15 | the swing `swoosh` is `put` at 1.10, so it ends at 1.36 |
| `excalibur_lunge +. drive` | 1.37 | `hit = 1.37`, the strike |
| `excalibur_duration` (`:435`) | 2.05 | `d = 2.05` |

Nothing checks this. Change `drive` in `fx_dedicated.ml` and you must change
`hit` in the script, or the tear stops arriving with the blade.

Note also that the gauntlet does not hit every lane at once:

```ocaml
let hit i = wind +. punch +. (Float.abs (p.x -. wielder.x) /. far_of p.x *. wave)
```

The nearest lane is struck at 0.58 s and the far ones up to 0.62 s later, which
is where the shock front's travel comes from. The audio impact sits at 0.58 s —
on the punch itself, not on the last lane — so the sound drives the animation
rather than following it.

## 3. The toolkit

A handful of helpers do all the work; the rest of the script is layers mixed
into a buffer.

**Time axis and noise.** `t_(d)` is `arange(d · 44100) / 44100`, `noise(d)` draws
`d` seconds of white noise from the seeded generator, `env_exp(d, tau)` is the
usual `exp(-t/tau)` decay.

**`fft_filter(x, lo, hi)`** filters noise by frequency band. It takes the real
FFT, multiplies every bin by a brick-wall-ish mask, and comes back:

```python
if lo: m *= 1 / (1 + (lo / np.maximum(f, 1e-3)) ** 4)   # high-pass
if hi: m *= 1 / (1 + (f / hi) ** 4)                      # low-pass
```

The fourth power gives roughly 24 dB per octave of roll-off. The band numbers
in the script are not subtle EQ — they are the *colour* of each layer: a 22–160
Hz quake, a 1200–7500 Hz tear. Filtering noise (not a tone) also means the
transients stay transients.

**`svf_sweep` / `swoosh(d, f0, f1, peak)`** make the whooshes. `svf_sweep` is a
state-variable band-pass whose centre frequency is a different value at every
sample, so `swoosh` walks noise through a band that sweeps exponentially from
`f0` to `f1` — 700 → 5200 Hz for the sword being let go. The envelope

```python
e = np.sin(np.pi * u ** peak) ** 2
```

decides where the energy sits inside the sweep: `peak = 0.7` puts it early
(`u = 0.5^(1/0.7) ≈ 0.37` of the way through), `peak = 1.9` puts it late
(`≈ 0.69`), which is what makes the sword's swing arrive fast and the gauntlet's
whoosh spill into the impact. The loop is per-sample Python, so swooshes are
kept at 0.2–0.5 s; the whole script runs in a couple of seconds.

**`put(buf, x, at, gain)`** mixes a layer into the buffer at a time in seconds.
It clips at the end (`j = min(len(buf), i + len(x))`), which is why the 1.07 s
quake layer that starts at 0.60 s in a 1.65 s buffer loses its last 20 ms —
at `exp(-1.07/0.5) ≈ 0.12` of the peak, so it does not matter.

**`norm(x, peak=0.89)`** is the saturator and the level:

```python
x = np.tanh(x * 1.0)
return x / (np.max(np.abs(x)) + 1e-9) * peak
```

`tanh` is a soft clip: everything pushed through it grows the same quiet
harmonics instead of squaring off, which is most of why a mix of nine or ten
layers sounds like one object. The final peak of 0.89 is −1.0 dBFS, and it is
applied *after* the tanh, so the soft clip is what decides the loudness, not the
sum.

**`save(name, x, drive)`** optionally scales the sum to `drive` (so it can
exceed full scale on purpose, saturate, and come back down), runs `norm`,
applies a 30 ms fade-out on the last samples, writes 16-bit mono 44.1 kHz WAV
into a temp file, and shells out to ffmpeg:

```python
subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-i", p, "-codec:a", "libmp3lame",
                "-b:a", "128k", out], check=True)
```

The fade-out is not decoration: without it the clip ends on whatever sample the
last layer happened to land on, which is a click.

## 4. Bargulian Gauntlet, layer by layer

The brief is in `gauntlet()`'s own docstring: *a fist driven into the ground
hard enough to set off an earthquake: all low end — a dull, deep earth thud,
never anything bright or metallic*. Every layer obeys it.

| at | layer | what it is |
|---|---|---|
| 0.00 | pressure | sine sweeping 40 → 95 Hz over 0.55 s, amplitude `u²·0.5`, `tanh(·2)` |
| 0.38 | whoosh | `swoosh(0.2, 120, 900, 1.8)`, gain 0.9 — ends exactly on 0.58 |
| 0.58 | boom | 120 → 30 Hz, pitch sagging with `exp(-t/0.06)`, `tanh(·2.5)·exp(-t/0.35)` |
| 0.58 | thud | noise 35–380 Hz, `exp(-t/0.07)`, `tanh(·6)·1.6` — packed dirt |
| 0.58 | slap | noise 90–650 Hz, `exp(-t/0.025)` — fist on the surface, gone in ~75 ms |
| 0.58 | body | 78 Hz sine, `exp(-t/0.09)` — the part you feel |
| 0.60 | quake | noise 22–160 Hz, 30 ms attack, `exp(-t/0.5)`, amplitude-modulated at 7.5 Hz |
| 0.64 | crackle | 40 snaps/s in 60–420 Hz, each smoothed by a 50 ms exponential, whole layer under `exp(-t/0.4)` |
| 0.66 | grind | noise 70–500 Hz, 13 Hz tremolo raised to the power 1.5, `exp(-t/0.4)` |
| — | one line | `b = fft_filter(b, hi=1400)` — *nothing above the low mids anywhere* |

That last line is the one that makes the punch read as earth and not as a metal
impact: at 2.8 kHz the mask is already 1/17 (−24 dB) below 1, so every
consequence of an over-bright layer — the hiss, the crackle's sparkle, the 78 Hz
harmonics of the saturation — is gone. It is also the only line in the file
that no layer can substitute for: the saturation adds harmonics, and this
removes them.

The quake's modulation is what makes it shudder rather than rumble:

```python
quake *= 0.6 + 0.4 * np.sin(2 * np.pi * 7.5 * qt + 1.0)
```

7.5 Hz is under the ~20 Hz where you stop hearing pitch and start feeling
movement, so the ground breathes in and out about seven times a second without
ever becoming a tone.

`save("barguliangauntlet", b, drive=1.7)` pushes the sum to 1.7 × full scale
before the tanh. The gauntlet is deliberately over-driven: it is a dull sound
made out of very quiet layers, and the saturation is what fuses them into one
heavy object instead of a pile of thuds.

## 5. Excalibur, layer by layer

The brief is the mirror image: *a ragged, crackling tear rather than a clang*.
There is no bell partial in the impact and no ring-out — a sword that cuts
people is not a bell.

| at | layer | what it is |
|---|---|---|
| 0.00 | forge | four sines at 1318 / 1760 / 2637 / 3520 Hz, 0.85 s, envelope `u²(1-u)^0.5`, gain 0.05 each |
| 0.35 | shimmer | `swoosh(0.5, 900, 4500, 0.9)`, gain 0.1 — ends on `forge` |
| 1.10 | swing | `swoosh(0.26, 700, 5200, 1.9)`, gain 0.85 — ends at 1.36 |
| 1.35 | tear | noise 1200–7500 Hz, jagged amplitude, 3–12 kHz snaps, 6 ms attack, `exp(-(t/0.17)^1.5)` |
| 1.37 | low tear | noise 200–1400 Hz, `tanh(·3)`, `exp(-t/0.09)`, gain 0.55 |
| 1.37 | bite | 140 → 50 Hz, pitch sagging with `exp(-t/0.025)`, `exp(-t/0.07)` |
| 1.49 | slither | noise 2500–9000 Hz, `exp(-t/0.1)`, gain 0.18 |

The tear is the interesting layer. Two products make it:

```python
grit = fft_filter(noise(n), lo=1200, hi=7500)                 # the body of the rip
jag  = np.abs(fft_filter(noise(n), hi=260)); jag = (jag / jag.max()) ** 1.6
snaps = (rng.random(len(tt)) < 0.004) * rng.uniform(0.6, 1.4, len(tt))
tear = grit * (0.25 + 0.75 * jag) + fft_filter(noise(n), lo=3000, hi=12000) * snaps * 1.5
```

The first product is the rip itself, amplitude-modulated by `jag`: low-passed
noise, normalised and raised to the 1.6 power, which turns smooth blobs into
hard-edged lumps — fibres snapping one after another rather than a continuous
hiss. The second is the sparkle on top: `snaps` fires on 0.4 % of samples,
about **176 a second**, each convolved with a 40-sample (0.9 ms) exponential so
it is a click rather than a step, and band-limited to 3–12 kHz. The decay
`(t/0.17)^1.5` falls faster than exponential: a power above 1 in the exponent
kills the tail early, which is what makes the tear *end* instead of fading out.

The tear starts at 1.35 s, 20 ms before the visual strike at 1.37 s, because
its own 6 ms attack plus the time the eye needs puts the perceived onset on the
frame. The `bite` layer at 1.37 s is the blade landing in something: a sine
dropping from 140 Hz to 50 Hz in 25 ms, the same trick as the gauntlet's boom,
one octave and a half higher.

Note that `excalibur` is saved without a `drive` argument. With the forge
partials at 0.05 gain the sum never gets hot, and the sword is meant to be
crisp; over-driving it turns the tear into the sort of buzzy, harmonically
laden sound that reads as a guitar string.

## 6. The mp3 layer

Two details to know before you measure anything in a waveform editor:

- **Encoder delay.** ffmpeg reports `start=0.025057` for these files: LAME adds
  ~25 ms of silence at the front of every mp3. Against a 0.58 s wind-up that is
  nothing, but it is why the numbers in the script are the *mix* times and a
  measured transient lands 25 ms later than the code says.
- **Mono, 44.1 kHz, 128 kbps.** The game decodes the file once through raylib
  and plays it on the same device as everything else; mono is deliberate, since
  a spell sound belongs in the middle of the mix, not in a corner.

The WAV in between is not an artefact of the tooling — Python can write a WAV
with the standard `wave` module and nothing else, and it keeps the 16-bit
quantisation visible. It is also the last step: the script scales the normalised
floats by 32767 and writes them as `<i2`, so quantisation never feeds back into
the synthesis.

## 7. Reproducibility, and why it is fragile

One line makes the whole script deterministic:

```python
rng = np.random.default_rng(7)
```

and it is a *single* stream, consumed in call order. Re-running the script
reproduces both files byte for byte — I checked, by regenerating over the
committed assets and comparing checksums (numpy 2.3.5, ffmpeg 8.0.1). Same
numpy, same samples; the mp3 *bytes* also depend on the encoder build, so a
different ffmpeg gives different bytes for identical audio.

The fragility is the price: add one `noise()` call in the middle of a function
and every later draw shifts, so the gauntlet you tuned is not the gauntlet you
get. That is not a bug to fix — with a seeded stream and no version control for
the sound design, "small tweak" simply does not exist here. You listen, you
change two or three numbers, you listen again.

The other consequence: **there is no diff**. A git diff of an mp3 tells you
nothing about what moved. Keep the intent in the comments, the way the script
already does.

## 8. Tuning, and the traps

The knobs that matter, in the order you would reach for them:

| knob | effect |
|---|---|
| `hit` in each function | when the impact lands; must follow `fx_dedicated.ml` |
| `d` | clip length; must equal `*_duration`, or the tail is cut or outlives the animation |
| `fft_filter` bands | the colour of each layer; the single biggest lever on "what is this" |
| `tanh` gains (`·2.5`, `·6`, `·7`, `·3`) | how hard each layer is driven before saturation |
| `exp` taus | how long the clip keeps going after the hit |
| `drive=1.7` (gauntlet only) | overall saturation; drop it and the punch turns thin |
| `peak` in `swoosh` | where the energy sits inside a sweep: lower is earlier |
| `hi=1400` (gauntlet only) | the safety net; raise it and any bright artefact comes back |

Traps, all of them paid for:

- **Names, not keys.** The file is `assets/sounds/barguliangauntlet.mp3`, not
  `blacksmith.barguliangauntlet.mp3`. Rename the card in
  `assets/i18n/en.properties` and you must rename the file with it.
- **The sound starts where the effect starts.** If the board timeline is still
  busy, `run_fx` delays the animation to `ui.anim_at`
  (`gui/gui_board.ml:579`) while `play_card_sound` has already fired. Only
  `NuclearMissile` uses `Fx.sound_delay` (`gui/fx_stage.ml:62`); these two
  do not, which is correct here because the clip's own lead-in is the delay.
- **Never leave a layer running past `d`.** `put` truncates it silently, so a
  `swoosh` or a noise bed that overruns gets cut at the buffer edge with no
  error — check that `at + len(layer) <= d` for every layer.
- **The assets are a submodule.** The script writes straight into
  `assets/sounds/`, so commit with `scripts/push_assets.sh "message"` like any
  other asset change.

## Try it

```sh
python3 scripts/gen_blacksmith_sounds.py
dune build && dune exec ./bin/gui.exe
# play Blacksmith, put Excalibur or Bargulian Gauntlet on a creature,
# let it enter play (F8 previews the visual only, in silence)
```

To hear one layer, comment out the other `put` calls — the gauntlet has nine,
the sword seven, and each is self-contained. To check the sync without playing
the card, count: the gauntlet must hit at 0.58 s and the sword at 1.37 s, so
watch the animation with a stopwatch the first time. To hear the gauntlet
without the earth (and notice how much the low-pass is doing), comment out
`b = fft_filter(b, hi=1400)`.
