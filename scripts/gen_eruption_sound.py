#!/usr/bin/env python3
"""Synthesise the Eruption sound (joy house).

    python3 scripts/gen_eruption_sound.py

Writes assets/sounds/eruption.mp3 (mono 44.1k).  The spell throws the two
strongest creatures of the caster at the opponent, so the clip is a volcano
coughing and spitting out stone projectiles: a deep roar, three rocks tearing
through the air, and the dull knocks of them landing.
"""
import subprocess, tempfile, os, wave
import numpy as np

SR = 44100
rng = np.random.default_rng(21)
ROOT = os.path.join(os.path.dirname(__file__), "..", "assets", "sounds")


def t_(d): return np.arange(int(d * SR)) / SR
def noise(d): return rng.standard_normal(int(d * SR))
def env_exp(d, tau): return np.exp(-t_(d) / tau)


def attack(d, atk=0.006):
    """a short fade-in, so a burst of noise starts without a click"""
    return 1 - np.exp(-t_(d) / atk)


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


def crackle(d, rate, tau, lo, hi, gain=1.0):
    """sparse random snaps (grit and small stones bouncing off the ground)"""
    n = int(d * SR)
    ev = (rng.random(n) < rate / SR) * rng.uniform(0.3, 1.0, n)
    ev = np.convolve(ev, np.exp(-np.arange(int(tau * SR * 6)) / (tau * SR)))[:n]
    return fft_filter(noise(d), lo=lo, hi=hi) * ev * gain


def projectile(d, f0, f1):
    """a stone tearing through the air: a gritty band of noise that rises as
    it flies, with the low body of the rock under it"""
    y = swoosh(d, f0, f1, 1.5) * attack(d)
    body = fft_filter(noise(d), lo=60, hi=420) * env_exp(d, d * 0.5) * attack(d)
    body /= np.max(np.abs(body)) + 1e-9
    return y + (body * 0.55)


def impact(d):
    """a rock landing: a dull knock, packed earth and grit, never a crack"""
    knock_t = t_(d)
    ph = 2 * np.pi * np.cumsum(120 + 60 * np.exp(-knock_t / 0.02)) / SR
    knock = np.sin(ph) * env_exp(d, 0.05) * attack(d, 0.002)
    thud = fft_filter(noise(d), lo=45, hi=520) * env_exp(d, 0.045) * attack(d, 0.003)
    return np.tanh(knock * 2.0) * 0.8 + np.tanh(thud * 4.0) * 1.1


def eruption():
    d = 1.55
    b = np.zeros(int(d * SR))

    # -- the volcano opens --
    # a deep roar swelling up out of the ground
    n = int(0.45 * SR); u = np.linspace(0, 1, n)
    roar = fft_filter(noise(0.45), lo=25, hi=340) * (u ** 1.7) * env_exp(0.45, 0.55)
    put(b, np.tanh(roar * 2.4) * 1.3, 0.0)
    # the shove of the throat, sagging as the shot goes off
    tt = t_(0.62)
    ph = 2 * np.pi * np.cumsum(62 + 46 * np.exp(-tt / 0.11)) / SR
    put(b, np.tanh(np.sin(ph) * 2.4) * env_exp(0.62, 0.28) * 2.0, 0.0)

    # -- the stone projectiles --
    put(b, projectile(0.30, 340, 1150) * 0.7, 0.14, 0.9)
    put(b, projectile(0.36, 260, 760) * 0.85, 0.34, 1.0)
    put(b, projectile(0.26, 620, 1900) * 0.6, 0.52, 0.8)

    # -- the rocks land --
    put(b, impact(0.5) * 0.9, 0.6, 1.0)
    put(b, impact(0.45) * 0.75, 0.85, 0.9)
    put(b, impact(0.4) * 0.6, 1.05, 0.8)
    # grit bouncing after each hit
    put(b, crackle(0.45, 90, 0.03, 150, 1100) * 1.2, 0.64, 0.6)
    put(b, crackle(0.4, 70, 0.03, 150, 1100) * 1.0, 0.89, 0.45)

    # -- the rumble dies away --
    tail = fft_filter(noise(0.55), lo=20, hi=220) * env_exp(0.55, 0.26) * attack(0.55, 0.02)
    put(b, np.tanh(tail * 4) * 0.8, 0.95)

    # no hiss above the stones: the clip stays as dark as the rocks it throws
    b = fft_filter(b, lo=28, hi=2600)
    save("eruption", b, drive=1.6)


if __name__ == "__main__":
    eruption()
