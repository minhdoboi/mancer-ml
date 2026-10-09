#!/usr/bin/env python3
"""Synthesise the Solar Burst sound (joy house).

    python3 scripts/gen_solar_burst_sound.py

Writes assets/sounds/solarburst.mp3 (mono 44.1k).  The spell rolls a curtain
of solar light across the board and leaves every slot it touches burning, so
the clip is a rising solar wave that breaks into a broad swell at ~1.1s (when
the wave reaches the field in gui/fx_dedicated.ml), then a bed of roaring,
crackling flame that licks and dies down over the rest of the 3.0s effect.
"""
import subprocess, tempfile, os, wave
import numpy as np

SR = 44100
rng = np.random.default_rng(41)
ROOT = os.path.join(os.path.dirname(__file__), "..", "assets", "sounds")


def t_(d): return np.arange(int(d * SR)) / SR
def noise(d): return rng.standard_normal(int(d * SR))
def env_exp(d, tau): return np.exp(-t_(d) / tau)


def fft_filter(x, lo=None, hi=None):
    X = np.fft.rfft(x)
    f = np.fft.rfftfreq(len(x), 1 / SR)
    m = np.ones_like(f)
    if lo: m *= 1 / (1 + (lo / np.maximum(f, 1e-3)) ** 4)   # high-pass
    if hi: m *= 1 / (1 + (f / hi) ** 4)                      # low-pass
    return np.fft.irfft(X * m, len(x))


def svf_sweep(x, fc, q=0.15):
    """band-pass state-variable filter with per-sample centre frequency fc"""
    out = np.zeros_like(x); lp = bp = 0.0
    for i in range(len(x)):
        f = 2 * np.sin(np.pi * min(fc[i], SR * 0.45) / SR)
        lp += f * bp
        hp = x[i] - lp - q * bp
        bp += f * hp
        out[i] = bp
    return out


def swoosh(d, f0, f1, peak=0.7):
    n = int(d * SR); u = np.linspace(0, 1, n)
    y = svf_sweep(noise(d), f0 * (f1 / f0) ** u)
    y /= np.max(np.abs(y)) + 1e-9
    return y * np.sin(np.pi * u ** peak) ** 2


def put(buf, x, at, gain=1.0):
    i = int(at * SR); j = min(len(buf), i + len(x))
    if j > i: buf[i:j] += x[: j - i] * gain


def norm(x, peak=0.89):
    x = np.tanh(x * 1.0)
    return x / (np.max(np.abs(x)) + 1e-9) * peak


def save(name, x, drive=None):
    if drive:  # scale so the loudest peak sits at `drive` before the soft clip
        x = x / (np.max(np.abs(x)) + 1e-9) * drive
    x = norm(x)
    fade = int(0.03 * SR); x[-fade:] *= np.linspace(1, 0, fade)
    with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tf:
        p = tf.name
    with wave.open(p, "wb") as w:
        w.setnchannels(1); w.setsampwidth(2); w.setframerate(SR)
        w.writeframes((x * 32767).astype("<i2").tobytes())
    out = os.path.join(ROOT, name + ".mp3")
    subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-i", p, "-codec:a", "libmp3lame",
                    "-b:a", "128k", out], check=True)
    os.unlink(p)
    print("wrote", out, f"{len(x) / SR:.2f}s")


def crackle(d, rate, tau, lo, hi, gain=1.0):
    """sparse random snaps -- the pops of a fire."""
    n = int(d * SR)
    ev = (rng.random(n) < rate / SR) * rng.uniform(0.3, 1.0, n)
    ev = np.convolve(ev, np.exp(-np.arange(int(tau * SR * 6)) / (tau * SR)))[:n]
    return fft_filter(noise(d), lo=lo, hi=hi) * ev * gain


def flicker(d, rate, seed=0.0):
    """a restless envelope: fire never holds still"""
    t = t_(d)
    f = (0.55 + 0.28 * np.sin(2 * np.pi * rate * t + seed)
         + 0.17 * np.sin(2 * np.pi * rate * 1.7 * t + seed * 2.3))
    return np.clip(f, 0, 1)


def fire(d, f0, f1, seed=0.0, rate=7.0, grow=0.5):
    """a body of flame: a sub rumble, a broad low-mid roar and a breath whose
    centre climbs, all licking under a restless flicker"""
    n = int(d * SR); u = np.linspace(0, 1, n)
    sub = fft_filter(noise(d), lo=40, hi=260)
    sub /= np.max(np.abs(sub)) + 1e-9
    roar = fft_filter(noise(d), lo=45, hi=1400)
    roar /= np.max(np.abs(roar)) + 1e-9
    breath = svf_sweep(noise(d), f0 * (f1 / f0) ** u, q=0.5)
    breath /= np.max(np.abs(breath)) + 1e-9
    return (0.7 * sub + 0.5 * roar + 0.5 * breath) * flicker(d, rate, seed) * (u ** grow)


def solar_burst():
    d = 3.0
    travel = 1.15   # seconds for the wave to cross the board
    ignite = 1.15   # when the first slots catch fire

    b = np.zeros(int(d * SR))

    # -- the solar wave rolling across: a low rumble gathering into a bright roar --
    put(b, swoosh(1.4, 130, 2600, 1.15) * 0.75, 0.0)
    wave_u = np.linspace(0, 1, int(1.4 * SR))
    put(b, np.tanh(fft_filter(noise(1.4), lo=50, hi=950) * (wave_u ** 2) * 4) * 0.5, 0.0)
    # a shimmer of sun particles riding the crest
    shimmer = svf_sweep(noise(1.0), 1600 * np.exp(1.3 * np.linspace(0, 1, int(1.0 * SR))), q=0.4)
    shimmer /= np.max(np.abs(shimmer)) + 1e-9
    put(b, shimmer * np.sin(np.pi * np.clip(np.linspace(0, 1, int(1.0 * SR)) / 1.0, 0, 1)) ** 2 * 0.35, 0.2)

    # -- the wave breaks over the field: a broad solar swell --
    tt = t_(0.8)
    flick = np.clip(0.6 + 0.4 * np.sin(2 * np.pi * 6 * tt), 0, 1)
    swell = fft_filter(noise(0.8), lo=50, hi=2600) * (1 - np.exp(-tt / 0.01)) * np.exp(-tt / 0.45)
    put(b, np.tanh(swell * flick * 6) * 1.2, ignite - 0.05)

    # -- the slots burn: two restless bodies of flame, crackling as they eat --
    burn = d - ignite
    u = np.linspace(0, 1, int(burn * SR))
    env = np.clip(u / 0.25, 0, 1) * (1 - np.clip((u - 0.78) / 0.22, 0, 1))
    put(b, np.tanh(fire(burn, 150, 520, seed=2.1, rate=7.5, grow=0.4) * 3.5) * env * 0.8, ignite)
    put(b, np.tanh(fire(burn, 95, 360, seed=5.3, rate=5.0, grow=0.3) * 3.5) * env * 0.55, ignite)
    # crackling wood and chitin as each creature burns
    put(b, crackle(burn, 42, 0.012, 500, 3600) * (0.45 + 0.55 * u) * env * 0.75, ignite)
    # a last scatter of embers as the sun dies down
    put(b, crackle(0.9, 22, 0.02, 400, 2600) * np.exp(-t_(0.9) / 0.35) * 0.5, d - 1.0)

    rel = int(0.05 * SR); b[-rel:] *= np.linspace(1, 0, rel)
    # a touch of warmth so the fire roars rather than hisses
    b += 0.4 * fft_filter(b, lo=40, hi=500)
    b = fft_filter(b, lo=35, hi=6500)
    save("solarburst", b, drive=1.5)


if __name__ == "__main__":
    solar_burst()
