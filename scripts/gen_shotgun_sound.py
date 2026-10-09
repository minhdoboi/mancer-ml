#!/usr/bin/env python3
"""Synthesise the Elite Soldiers shotgun blast.

    python3 scripts/gen_shotgun_sound.py

Writes assets/sounds/shotgun.mp3 (mono 44.1k).  The Shotgunner's summon volley
and Sniper Bullets both fire it (gui/fx_dedicated.ml spawn_buckshot), and the
pellets land buckshot_flight = 0.24s after the muzzle flash, so the clip is a
tight, saturated blast: a razor-sharp crack, a deep boom with a sub thump under
it, and a report that rolls away over ~0.7s -- a 12-gauge going off, not a
suppressed pop.
"""
import subprocess, tempfile, os, wave
import numpy as np

SR = 44100
rng = np.random.default_rng(13)
ROOT = os.path.join(os.path.dirname(__file__), "..", "assets", "sounds")


def t_(d): return np.arange(int(d * SR)) / SR
def noise(d): return rng.standard_normal(int(d * SR))


def fft_filter(x, lo=None, hi=None):
    X = np.fft.rfft(x)
    f = np.fft.rfftfreq(len(x), 1 / SR)
    m = np.ones_like(f)
    if lo: m *= 1 / (1 + (lo / np.maximum(f, 1e-3)) ** 4)   # high-pass
    if hi: m *= 1 / (1 + (f / hi) ** 4)                      # low-pass
    return np.fft.irfft(X * m, len(x))


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


def shotgun():
    d = 1.0
    b = np.zeros(int(d * SR))

    # -- the blast: the muzzle letting go.  A near-instant broadband crack
    #    with a hard attack, and a supersonic snap on the very front edge. --
    n = 0.08; tt = t_(n)
    crack = fft_filter(noise(n), lo=320, hi=15000)
    crack *= (1 - np.exp(-tt / 0.00025)) * np.exp(-tt / 0.015)
    put(b, np.tanh(crack * 8) * 1.7, 0.0)
    snap = fft_filter(noise(0.03), lo=4000, hi=16000) * np.exp(-t_(0.03) / 0.0035)
    put(b, np.tanh(snap * 5) * 0.85, 0.0)

    # -- the boom: the low body of the blast, the air slamming back out of the
    #    barrel, pitch sagging as the pressure drops, and a sub thump under it
    #    for the chest.  This is what the old clip was missing. --
    n = 0.6; tt = t_(n)
    ph = 2 * np.pi * np.cumsum(150 * np.exp(-tt / 0.05) + 48) / SR
    put(b, np.tanh(np.sin(ph) * 4) * np.exp(-tt / 0.12) * 1.8, 0.0)
    sub = np.sin(2 * np.pi * np.cumsum(72 * np.exp(-tt / 0.09) + 38) / SR)
    put(b, np.tanh(sub * 3) * np.exp(-tt / 0.22), 0.0)

    # -- the punch: a midrange bark that gives the shot its size --
    n = 0.3; tt = t_(n)
    punch = fft_filter(noise(n), lo=150, hi=1500)
    punch *= (1 - np.exp(-tt / 0.0012)) * np.exp(-tt / 0.06)
    put(b, np.tanh(punch * 5) * 1.6, 0.0)

    # -- the report: the shot rolling away across the field, with a couple of
    #    early reflections so it sits in a space instead of a vacuum --
    n = 0.9; tt = t_(n)
    report = fft_filter(noise(n), lo=150, hi=4000)
    report *= (1 - np.exp(-tt / 0.004)) * np.exp(-tt / 0.2)
    put(b, report, 0.012, 0.55)
    put(b, report, 0.07, 0.3)
    put(b, report, 0.15, 0.16)

    # keep the weight, lose the hiss
    b += 0.6 * fft_filter(b, lo=30, hi=160)
    b = fft_filter(b, lo=28, hi=15000)
    save("shotgun", b, drive=1.8)


if __name__ == "__main__":
    shotgun()
