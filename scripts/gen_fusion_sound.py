#!/usr/bin/env python3
"""Synthesise the Fusion sound (joy house).

    python3 scripts/gen_fusion_sound.py

Writes assets/sounds/fusion.mp3 (mono 44.1k).  The spell fuses the caster's
two strongest creatures into one strike, so the clip is fire: two restless
flames braid and are drawn together, roar into a single fireball at ~0.9s, and
burst on the opponent at ~1.35s (the fx timings in gui/fx_dedicated.ml).
"""
import subprocess, tempfile, os, wave
import numpy as np

SR = 44100
rng = np.random.default_rng(21)
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


def fire(d, f0, f1, seed=0.0, rate=7.0, grow=1.6):
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


def fusion():
    d = 2.2
    b = np.zeros(int(d * SR))
    merge = 0.9  # the two flames become one fireball
    hit = 1.35   # the fireball bursts on the opponent

    # -- two fires drawn together, trading prominence as they gather --
    n = max(1, int(merge * SR)); u = np.linspace(0, 1, n)
    phase = 2 * np.pi * (1.4 * u + 1.2 * 1.4 * u ** 2) * merge
    a_env = 0.5 + 0.5 * np.sin(phase)
    b_env = 0.5 - 0.5 * np.sin(phase)
    put(b, np.tanh(fire(merge, 170, 750, seed=0.0, rate=6.5) * a_env * 1.6) * 0.95, 0.0)
    put(b, np.tanh(fire(merge, 120, 560, seed=1.7, rate=8.5) * b_env * 1.6) * 0.95, 0.0)
    # both fires crackle as they burn
    put(b, crackle(merge, 32, 0.010, 400, 2600) * (u ** 2) * 0.55, 0.0)
    # the air being pulled into the gathering flame
    put(b, swoosh(0.45, 260, 1700, 1.2) * 0.5, merge - 0.45)
    put(b, np.tanh(fft_filter(noise(merge), lo=80, hi=900) * (u ** 2.5) * 4) * 0.5, 0.0)

    # -- they fuse: one fireball --
    tt = t_(1.0)
    flick9 = np.clip(0.6 + 0.4 * np.sin(2 * np.pi * 9 * tt), 0, 1)
    burst = fft_filter(noise(1.0), lo=70, hi=3200) * (1 - np.exp(-tt / 0.004)) * np.exp(-tt / 0.3)
    put(b, np.tanh(burst * flick9 * 6) * 1.4, merge)
    grow = np.clip(tt / 0.5, 0, 1)
    put(b, np.tanh(fft_filter(noise(1.0), lo=60, hi=1800) * flick9 * (0.35 + 0.65 * grow) * 4) * 0.75, merge)
    # the fireball's low rumble, swelling under the roar
    put(b, np.tanh(fft_filter(noise(1.0), lo=40, hi=300) * grow * 5) * 0.6, merge)
    put(b, crackle(1.0, 55, 0.010, 600, 4000) * np.exp(-tt / 0.35) * 0.6, merge)

    # -- the fireball bursts on the opponent --
    flick13 = np.clip(0.6 + 0.4 * np.sin(2 * np.pi * 13 * tt), 0, 1)
    imp = fft_filter(noise(1.0), lo=60, hi=3800) * (1 - np.exp(-tt / 0.003)) * np.exp(-tt / 0.26)
    put(b, np.tanh(imp * flick13 * 8) * 1.7, hit)
    # the low boom of the burst
    ph = 2 * np.pi * np.cumsum(48 + 90 * np.exp(-tt / 0.05)) / SR
    put(b, np.tanh(np.sin(ph) * 2.4) * np.exp(-tt / 0.32) * 1.3, hit)
    # a storm of sparks
    put(b, crackle(1.0, 85, 0.009, 900, 6000) * np.exp(-tt / 0.22) * 0.9, hit)
    # embers dying after the blast
    put(b, crackle(0.8, 16, 0.02, 400, 2400) * np.exp(-t_(0.8) / 0.3) * 0.6, hit + 0.3, 0.6)

    rel = int(0.3 * SR); b[-rel:] *= np.linspace(1, 0, rel)
    # a touch of warmth so the flame roars rather than hisses
    b += 0.5 * fft_filter(b, lo=40, hi=500)
    b = fft_filter(b, lo=35, hi=6500)
    save("fusion", b, drive=1.6)


if __name__ == "__main__":
    fusion()
