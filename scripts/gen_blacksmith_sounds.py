#!/usr/bin/env python3
"""Synthesise the Blacksmith spell sounds (Bargulian Gauntlet, Excalibur).

    python3 scripts/gen_blacksmith_sounds.py

Writes assets/sounds/{barguliangauntlet,excalibur}.mp3 (mono 44.1k).  Timing
follows gui/fx_dedicated.ml: the gauntlet lands at ~0.58s (clip 1.65s), the
sword is let go at 1.15s and strikes at ~1.37s (clip 2.05s).
"""
import subprocess, tempfile, os, wave
import numpy as np

SR = 44100
rng = np.random.default_rng(7)
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
    fc = f0 * (f1 / f0) ** u
    y = svf_sweep(noise(d), fc)
    y /= np.max(np.abs(y)) + 1e-9
    e = np.sin(np.pi * u ** peak) ** 2
    return y * e


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


# ------------------------------------------------------------- gauntlet
def crackle(d, rate, tau, lo, hi, gain=1.0):
    """sparse random snaps (splitting rock, falling debris)"""
    n = int(d * SR)
    ev = (rng.random(n) < rate / SR) * rng.uniform(0.3, 1.0, n)
    ev = np.convolve(ev, np.exp(-np.arange(int(tau * SR * 6)) / (tau * SR)))[:n]
    return fft_filter(noise(d), lo=lo, hi=hi) * ev * gain


def gauntlet():
    """a fist driven into the ground hard enough to set off an earthquake: all
    low end -- a dull, deep earth thud, never anything bright or metallic --
    then the ground heaves, grinds and rolls"""
    d = 1.65
    b = np.zeros(int(d * SR))
    hit = 0.58
    # pressure building as the fist draws back: rising low drone
    n = int(0.55 * SR); u = np.linspace(0, 1, n)
    drone = np.sin(2 * np.pi * np.cumsum(40 + 55 * u ** 2) / SR)
    put(b, np.tanh(drone * 2) * u ** 2 * 0.5, 0.0)
    # the fist comes down: a heavy, low, airy whoosh (no hiss)
    put(b, swoosh(0.2, 120, 900, 1.8), 0.38, 0.9)

    # -- the impact --
    # deep boom of the ground taking the blow, pitch sagging
    tt = t_(1.0)
    ph = 2 * np.pi * np.cumsum(30 + 90 * np.exp(-tt / 0.06)) / SR
    put(b, np.tanh(np.sin(ph) * 2.5) * np.exp(-tt / 0.35) * 2.0, hit)
    # the earth thud: packed dirt and bag-like body, dull and saturated
    thud = fft_filter(noise(0.4), lo=35, hi=380) * env_exp(0.4, 0.07)
    put(b, np.tanh(thud * 6) * 1.6, hit)
    # fist on the surface: a muffled slap, low-mid only
    slap = fft_filter(noise(0.15), lo=90, hi=650) * env_exp(0.15, 0.025)
    put(b, np.tanh(slap * 5), hit, 1.0)
    mid = np.sin(2 * np.pi * 78 * t_(0.3)) * env_exp(0.3, 0.09)
    put(b, np.tanh(mid * 4), hit, 0.8)
    # the quake: the ground heaves, a long grinding rumble that shudders
    qt = t_(1.07)
    quake = fft_filter(noise(1.07), lo=22, hi=160) * (1 - np.exp(-qt / 0.03)) * np.exp(-qt / 0.5)
    quake *= 0.6 + 0.4 * np.sin(2 * np.pi * 7.5 * qt + 1.0)
    put(b, np.tanh(quake * 7), hit + 0.02, 1.1)
    # heavy rock and earth shifting: dull low thuds, not sharp snaps
    put(b, crackle(0.9, 40, 0.05, 60, 420) * np.exp(-t_(0.9) / 0.4) * 5, hit + 0.06, 1.0)
    # grinding of the ground, kept low and slow
    gr = fft_filter(noise(0.95), lo=70, hi=500) * (0.5 + 0.5 * np.sin(2 * np.pi * 13 * t_(0.95) + 2)) ** 1.5
    put(b, gr * np.exp(-t_(0.95) / 0.4), hit + 0.08, 0.7)
    # nothing above the low mids anywhere in the clip
    b = fft_filter(b, hi=1400)
    save("barguliangauntlet", b, drive=1.7)


# ------------------------------------------------------------- excalibur
def excalibur():
    d = 2.05
    b = np.zeros(int(d * SR))
    # the blade is drawn in light: a soft rising shimmer of bell partials
    for k, fr in enumerate((1318, 1760, 2637, 3520)):
        n = int(0.85 * SR); u = np.linspace(0, 1, n)
        put(b, np.sin(2 * np.pi * fr * t_(0.85)) * (u ** 2) * (1 - u) ** 0.5 * 0.05, 0.0)
    put(b, swoosh(0.5, 900, 4500, 0.9), 0.35, 0.1)
    # let go: a fast, bright swing through the wielder
    put(b, swoosh(0.26, 700, 5200, 1.9), 1.1, 0.85)

    hit = 1.37
    # the blade rips through: a ragged, crackling tear rather than a clang
    n = 0.34; tt = t_(n); u = tt / n
    grit = fft_filter(noise(n), lo=1200, hi=7500)
    # jagged amplitude: fibres snapping one after another
    jag = np.abs(fft_filter(noise(n), hi=260)); jag = (jag / jag.max()) ** 1.6
    snaps = (rng.random(len(tt)) < 0.004) * rng.uniform(0.6, 1.4, len(tt))
    snaps = np.convolve(snaps, np.exp(-np.arange(300) / 40.0))[: len(tt)]
    tear = grit * (0.25 + 0.75 * jag) + fft_filter(noise(n), lo=3000, hi=12000) * snaps * 1.5
    tear *= (1 - np.exp(-tt / 0.006)) * np.exp(-(tt / 0.17) ** 1.5)
    put(b, tear, hit - 0.02, 1.3)
    # a coarser low tear underneath gives it weight
    low = np.tanh(fft_filter(noise(0.3), lo=200, hi=1400) * (0.3 + jag[: int(0.3 * SR)]) * 3)
    put(b, low * env_exp(0.3, 0.09), hit, 0.55)
    # wet thud as it bites in
    tt2 = t_(0.3)
    ph = 2 * np.pi * np.cumsum(50 + 90 * np.exp(-tt2 / 0.025)) / SR
    put(b, np.sin(ph) * np.exp(-tt2 / 0.07), hit, 0.9)
    # the cut finishes: a dying slither of air
    put(b, fft_filter(noise(0.4), lo=2500, hi=9000) * env_exp(0.4, 0.1) * 0.5, hit + 0.12, 0.18)
    save("excalibur", b)


if __name__ == "__main__":
    gauntlet(); excalibur()
