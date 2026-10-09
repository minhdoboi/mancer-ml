#!/usr/bin/env python3
"""Generate Vietnamese voice-over with ZeroTTS (https://github.com/zeroweight-ai/ZeroTTS).

The text comes from the Vietnamese i18n bundle, `assets/i18n/vi.properties`, so
the game and the audio never drift apart.  Files land in `assets/audio/vi/`,
named after the property key.  The `site.*` entries belong to the website
rather than the game, so `--site` speaks them into `docs/audio/vi/` instead
(the Pages workflow copies `docs/` as-is, so they publish with the page).

Setup
-----
ZeroTTS is ONNX-only (no PyTorch) and runs on the CPU, but it pulls in
`numpy`/`onnxruntime`/`soundfile`, so install it in a virtualenv instead of the
system Python.  The model weights (~1 GB) download on first run and are cached
under `HF_HOME`.

    python3 -m venv .venv
    .venv/bin/pip install --upgrade pip
    .venv/bin/pip install zerotts "huggingface_hub<1"

    # then use the venv's python for every run below
    .venv/bin/python scripts/gen_vi_tts.py --key lobby.title

The `huggingface_hub<1` pin matters on a slow link.  Hub 2.x downloads each
file to a process-unique temporary file and throws it away on any error, so an
interrupted 1 GB model restarts from zero every run; 0.36.x appends to
`<blob>.incomplete` and picks up where it stopped.  If you already installed
2.x, downgrade it:

    .venv/bin/pip install "huggingface_hub<1"

The script itself disables the `hf-xet` backend (`HF_HUB_DISABLE_XET=1`),
whose transfer errors are what usually aborts that big download, and falls back
to resumable plain HTTP.  Override it by exporting the variable yourself if you
want Xet back.  Files are fetched one at a time (`--workers 1`), which is what
a slow link wants; pass `--workers 4` to pull them in parallel instead.

The weights are materialised as real files under `~/.cache/zerotts/<repo>`
(`--model-dir` / `$ZEROTTS_MODEL_DIR` to move it).  The first run copies them
out of the Hugging Face cache — no second download — because ONNX Runtime
rejects the codec's external-data file when it is an HF-cache symlink.  Once
that directory is complete the script uses it as-is and does not contact the
Hub again; pass `--refresh` to check for a newer revision anyway.

`pip install "zerotts[webui]"` adds the Gradio/web UI, which is handy for
picking a voice by ear; it is not needed to generate the files.

Usage
-----
    # one key, for a quick listen
    .venv/bin/python scripts/gen_vi_tts.py --key lobby.title

    # a shell-glob pattern; *?[] work and * crosses the dots
    .venv/bin/python scripts/gen_vi_tts.py --key 'campaign.conjurer.*'

    # a few keys at a time (patterns allowed here too)
    .venv/bin/python scripts/gen_vi_tts.py --keys lobby.title,button.duel

    # every string (slow: minutes on a CPU)
    .venv/bin/python scripts/gen_vi_tts.py --all

    # every campaign line the game can play (messages, win/lose, interludes)
    .venv/bin/python scripts/gen_vi_tts.py --campaign

    # every site.* line the landing page can speak (into docs/audio/vi/)
    .venv/bin/python scripts/gen_vi_tts.py --site

    # which keys are there, and which voices
    .venv/bin/python scripts/gen_vi_tts.py --list-keys
    .venv/bin/python scripts/gen_vi_tts.py --list-voices

By default a string's `{0}`-style placeholders are dropped before speaking,
because there is no value to read.  `--keep-placeholders` leaves them in.

Campaign characters each get their own voice.  `campaign_voices.json`,
beside this script, records the tags that decide a voice (gender, age, tone)
for every campaign node and describes each preset the same way; the script's
resolver matches them and picks the best pack.  `campaign.<id>.*` keys are
spoken with their character's resolved voice, while `campaign.intro`,
`campaign.epilogue`, `campaign.end.*` and the other characterless campaign
keys use the map's `narrator_voice`; child characters have no preset and are
skipped.  `--campaign` generates every such spoken line in one pass (the
`.name` labels and `.choice` buttons are left out); `--all` still does the
whole bundle.  `--resolve-voices` prints the table the map resolves to.
Pass `-v/--voice` to force one voice for everything instead (which also
voices the skipped children), or `--voice-map` to point at a different map.
`--voice narrator_voice` (or `--voice default_voice`) is shorthand for the
matching field of the map, so the narrator can be forced without repeating the
pack name; `narrator` and `default` work too.

The presets the model bundles are not the only ones.  The repo also carries
its own packs under `assets/voices/vi/` — the `.zip` files zeroweight.ai hands
down — and every run loads them beside the bundled ones, so a voice named in
the map works without unpacking it by hand.  A zip is installed once into
ZeroTTS's `~/.zerotts/voices` (and re-installed when it changes); a pack that
is already unpacked, a folder with a `voice.npz` in it, is used where it lies.
`--voices-dir` points at a different directory.

Output is MP3 by default, matching `assets/sounds/`, and needs ffmpeg with
libmp3lame on PATH.  `--bitrate` sets the compression (128k by default); pass
`--format wav` for an uncompressed take.  `--compress` transcodes any `.wav` or
`.ogg` clips already sitting in the output directory in place, so a batch
recorded before the default changed does not have to be synthesised again:

    .venv/bin/python scripts/gen_vi_tts.py --key lobby.title
    .venv/bin/python scripts/gen_vi_tts.py --compress

Intermediates live in `.tmp/vi_tts/`, the way the other asset scripts do it.
The sampling defaults are the model's benchmarked settings and are meant to be
left alone — there is no slider worth raising for medium-quality voice-over.
"""
from __future__ import annotations

import argparse
import fnmatch
import json
import os
import re
import subprocess
import sys
from pathlib import Path

# The hf-xet transfer backend aborts large downloads with opaque CAS errors and
# cannot resume; plain HTTP resumes from the `.incomplete` blob.  Must be set
# before huggingface_hub is imported (see build_tts).  HF_HUB_DOWNLOAD_TIMEOUT
# is raised because the 1 GB model can trickle in at well under 1 MB/s.
os.environ.setdefault("HF_HUB_DISABLE_XET", "1")
os.environ.setdefault("HF_HUB_DOWNLOAD_TIMEOUT", "60")

ROOT = Path(__file__).resolve().parent.parent
PROPERTIES = ROOT / "assets" / "i18n" / "vi.properties"
OUT_DIR = ROOT / "assets" / "audio" / "vi"
SITE_OUT_DIR = ROOT / "docs" / "audio" / "vi"
TMP_DIR = ROOT / ".tmp" / "vi_tts"
VOICE_MAP = ROOT / "scripts" / "campaign_voices.json"
VOICES_DIR = ROOT / "assets" / "voices" / "vi"
DEFAULT_VOICE = "maichi"
CAMPAIGN_KEY = re.compile(r"^campaign\.([a-z_]+)(?:\.|$)")

COMMENT = re.compile(r"^\s*[#!]")
PLACEHOLDER = re.compile(r"\{[^}]*\}")
EMPTY_BRACKETS = re.compile(r"\s*[([][\s]*[)\]]")
UNSAFE = re.compile(r"[^A-Za-z0-9._-]")
MULTISPACE = re.compile(r"\s{2,}")
SPACE_BEFORE_PUNCT = re.compile(r"\s+([,.;:!?…])")


# --- properties ------------------------------------------------------------

def _unescape(s: str) -> str:
    out, i = [], 0
    while i < len(s):
        c = s[i]
        if c == "\\" and i + 1 < len(s):
            d = s[i + 1]
            if d == "u" and i + 5 < len(s):
                out.append(chr(int(s[i + 2:i + 6], 16)))
                i += 6
                continue
            out.append({"n": "\n", "t": "\t", "r": "\r", "f": "\f"}.get(d, d))
            i += 2
            continue
        out.append(c)
        i += 1
    return "".join(out)


def load_properties(path: Path) -> dict[str, str]:
    """Parse a Java .properties file: continuations, comments, \\escapes."""
    text = path.read_text(encoding="utf-8")
    props: dict[str, str] = {}
    pending: str | None = None
    for raw in text.splitlines():
        if pending is not None:
            raw = pending + raw.lstrip()
            pending = None
        if COMMENT.match(raw) or not raw.strip():
            continue
        if raw.endswith("\\") and not raw.endswith("\\\\"):
            pending = raw[:-1]
            continue
        sep = next((i for i, ch in enumerate(raw) if ch in "=:"), -1)
        if sep == -1:
            continue
        key = _unescape(raw[:sep]).strip()
        value = _unescape(raw[sep + 1:].lstrip())
        props[key] = value
    return props


def spoken_text(value: str, keep_placeholders: bool) -> str:
    """Turn a UI string into something a voice can read."""
    if not keep_placeholders:
        value = PLACEHOLDER.sub("", value)
        value = EMPTY_BRACKETS.sub("", value)
    value = value.replace("\n", " ").replace("\t", " ")
    value = SPACE_BEFORE_PUNCT.sub(r"\1", value)
    return MULTISPACE.sub(" ", value).strip()


def output_name(key: str, fmt: str) -> str:
    return UNSAFE.sub("_", key) + "." + fmt


def encode_mp3(src: Path, dest: Path, bitrate: str) -> None:
    """Transcode `src` to `dest` with ffmpeg's libmp3lame at `bitrate`."""
    dest.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        ["ffmpeg", "-y", "-loglevel", "error", "-i", str(src),
         "-codec:a", "libmp3lame", "-b:a", bitrate, str(dest)],
        check=True,
    )


def drop_other_encodings(path: Path) -> None:
    """Delete the .wav/.ogg twins of `path` so the game cannot pick a heavier
    or stale file (Assets.voice tries .wav before .mp3)."""
    for ext in (".wav", ".ogg", ".mp3"):
        other = path.with_suffix(ext)
        if other != path and other.exists():
            other.unlink()


# --- per-character voices --------------------------------------------------

AGE_ORDER = ("young", "mature", "older")


def load_voice_map(path: Path | None) -> dict | None:
    """Read the campaign character → voice tag map, or None when absent."""
    if path is None:
        return None
    path = Path(path)
    if not path.exists():
        return None
    with path.open(encoding="utf-8") as fh:
        return json.load(fh)


def map_default_voice(vmap: dict | None) -> str:
    if vmap:
        return vmap.get("default_voice") or DEFAULT_VOICE
    return DEFAULT_VOICE


# `-v/--voice` values that name a field of the voice map instead of a pack.
VOICE_ALIASES = {
    "default": "default_voice",
    "default_voice": "default_voice",
    "narrator": "narrator_voice",
    "narrator_voice": "narrator_voice",
}


def resolve_voice_alias(name: str | None, vmap: dict | None) -> str | None:
    """Turn `--voice` shorthand such as `narrator_voice` into a pack name.

    `default`/`default_voice` and `narrator`/`narrator_voice` refer to the
    matching fields of the voice map, so the narrator can be spoken with the
    same voice the characterless campaign keys use.  Anything else is already a
    pack name and passes through unchanged.
    """
    if not name:
        return name
    field = VOICE_ALIASES.get(name.strip().lower())
    if field is None:
        return name
    value = (vmap or {}).get(field)
    if not value:
        sys.exit(f"--voice {name}: the voice map has no {field}")
    return value


def _age_distance(age: str, ages) -> int:
    """How far a character's age is from a voice's age range (0 = exact)."""
    ages = [str(a).strip().lower() for a in ages or []]
    if age in ages:
        return 0
    known = [a for a in ages if a in AGE_ORDER]
    if not known or age not in AGE_ORDER:
        return len(AGE_ORDER)
    return min(abs(AGE_ORDER.index(age) - AGE_ORDER.index(a)) for a in known)


def resolve_voice(gender, age, tone, voices: dict) -> str | None:
    """Pick a voice pack from the tags the map records for a character.

    Voices are filtered to the character's gender, then ranked by age fit
    first (an old man should still sound old) and tone second.  A character
    may pin `"voice"` in the map to bypass this.
    """
    gender = (gender or "").strip().lower()
    age = (age or "").strip().lower()
    tone = (tone or "").strip().lower()
    best, best_key = None, None
    for name, spec in voices.items():
        if (spec.get("gender") or "").strip().lower() != gender:
            continue
        voice_tone = (spec.get("tone") or "").strip().lower()
        key = (
            _age_distance(age, spec.get("ages")),
            0 if voice_tone == tone else 1,
        )
        if best_key is None or key < best_key:
            best, best_key = name, key
    return best


def voice_for_key(key: str | None, vmap: dict | None) -> str | None:
    """The voice a property key should be spoken with, or None for the CLI one.

    Campaign character keys (`campaign.<id>.…`) resolve the character's tags to
    a voice; characterless campaign keys (the intro, the epilogue, the end
    cards, the night interludes) use the map's narrator voice.  Every other key
    returns None, which leaves the caller on `--voice` / the map default.
    """
    if not key or not vmap:
        return None
    m = CAMPAIGN_KEY.match(key)
    if not m:
        return None
    cid = m.group(1)
    entry = (vmap.get("characters") or {}).get(cid)
    if isinstance(entry, str):
        return entry
    if isinstance(entry, dict):
        pinned = entry.get("voice")
        if pinned:
            return pinned
        voice = resolve_voice(entry.get("gender"), entry.get("age"),
                              entry.get("tone"), vmap.get("voices") or {})
        if voice:
            return voice
    if cid in (vmap.get("skipped") or {}):
        return None
    return vmap.get("narrator_voice") or None


def skipped_character(key: str | None, vmap: dict | None) -> str | None:
    """The reason a key was skipped (a character with no available voice)."""
    if not key or not vmap:
        return None
    m = CAMPAIGN_KEY.match(key)
    if not m:
        return None
    return (vmap.get("skipped") or {}).get(m.group(1))


# --- synthesis -------------------------------------------------------------

def _model_ready(path: Path) -> bool:
    """True once every graph ZeroTTS needs has actually landed on disk."""
    try:
        from zerotts.hub import REQUIRED_FILES
    except Exception:
        REQUIRED_FILES = ("onnx/text_encoder.onnx", "onnx/prefix_step.onnx",
                          "onnx/local_frame_decode.onnx")
    return all((path / f).exists() for f in REQUIRED_FILES)


def _cache_complete(path: Path) -> bool:
    """True when the model is already fully on disk, so nothing need be fetched.

    Checks the graphs ZeroTTS loads (`REQUIRED_FILES`) and that every voice the
    index lists has its `voice.npz` and `meta.json`, so a directory that only
    half downloaded still goes through `snapshot_download` to fill the gaps.
    """
    if not _model_ready(path):
        return False
    index = path / "voices" / "index.json"
    try:
        voices = json.loads(index.read_text(encoding="utf-8")).get("voices") or []
    except (OSError, ValueError):
        return False
    names = [v.get("name") for v in voices
             if isinstance(v, dict) and v.get("name")]
    if not names:
        return False
    return all((path / "voices" / name / "voice.npz").exists()
               and (path / "voices" / name / "meta.json").exists()
               for name in names)


def default_model_dir(model: str) -> Path:
    """Where a repo id gets materialised as real files (not HF-cache symlinks)."""
    env = os.environ.get("ZEROTTS_MODEL_DIR")
    if env:
        return Path(env).expanduser()
    return Path.home() / ".cache" / "zerotts" / UNSAFE.sub("_", str(model))


def add_asset_voices(tts, voices_dir: Path) -> list[str]:
    """Register the packs the repo ships under `assets/voices/`.

    Mirrors the two shapes a voice comes in: the `.zip` download, which
    `add_voices` installs (replacing an earlier copy of the same name), and an
    already-unpacked pack directory, which is used where it lies.  A missing
    directory is not an error — the assets submodule may not be checked out.
    """
    voices_dir = Path(voices_dir).expanduser()
    if not voices_dir.is_dir():
        return []
    added: list[str] = []
    unpacked = ((voices_dir / "voice.npz").exists()
                or any(d.is_dir() and (d / "voice.npz").exists()
                       for d in voices_dir.iterdir()))
    if unpacked:
        added += tts.add_voices(voices_dir)
    for archive in sorted(voices_dir.glob("*.zip")):
        added += tts.add_voices(archive)
    return added


def build_tts(args):
    try:
        from zerotts import DEFAULT_REPO_ID, ZeroTTS
        from zerotts.hub import _ALLOW_PATTERNS, DEFAULT_REVISION
    except ImportError:
        sys.exit("zerotts is not installed — see the setup notes at the top "
                 "of this file (pip install zerotts \"huggingface_hub<1\")")

    model = args.model or DEFAULT_REPO_ID
    local = Path(model).expanduser()
    if local.is_dir():
        model_dir = local
    else:
        model_dir = (args.model_dir or default_model_dir(model)).expanduser()
        # `snapshot_download` reaches the Hub to resolve `main` to a commit even
        # when every file is already on disk.  A complete local directory is
        # used as-is, so a second run never touches the network; `--refresh`
        # (or a pinned `--revision`) still forces the check.
        if args.revision is None and not args.refresh and _cache_complete(model_dir):
            print(f"model: {model_dir} (cached)", file=sys.stderr)
        else:
            from huggingface_hub import snapshot_download
            revision = args.revision or DEFAULT_REVISION
            # A plain HF-cache download symlinks every file into `blobs/<xx>/`,
            # and ONNX Runtime then rejects the codec's external-data `.data`
            # file as escaping the model directory (it is a symlink into a
            # different blob dir).  `local_dir` copies real files side by side;
            # it reuses blobs already in the HF cache, so this does not
            # re-download anything.
            mode = ("one file at a time" if args.workers == 1
                    else f"{args.workers} workers")
            print(f"fetching {model}@{revision} into {model_dir} ({mode})…",
                  file=sys.stderr)
            try:
                model_dir = Path(snapshot_download(
                    repo_id=str(model), revision=revision,
                    allow_patterns=_ALLOW_PATTERNS, local_dir=str(model_dir),
                    max_workers=args.workers))
            except Exception as e:
                sys.exit(f"download interrupted: {e}\n"
                         "Run the same command again — the partial file is on "
                         "disk and the download resumes where it stopped.")

    if not _model_ready(model_dir):
        sys.exit(f"{model_dir} is incomplete (missing ONNX graphs); re-run to resume.")
    print(f"model dir: {model_dir}", file=sys.stderr)
    tts = ZeroTTS(model_dir, intra_op_num_threads=args.threads)
    added = add_asset_voices(tts, args.voices_dir)
    if added:
        print(f"asset voices: {', '.join(added)}", file=sys.stderr)
    return tts


def synthesize_one(tts, text: str, args, dest: Path, voice: str | None = None) -> float:
    """Synthesize `text` (long text is chunked) into `dest`.  Returns seconds."""
    import numpy as np
    from zerotts.audio import concat_with_silence
    from zerotts.chunking import (chunk_text, clean_segment_punctuation,
                                  normalize_punctuation, split_sentences)
    from zerotts.text_norm import normalize_vi_text

    if args.seed is not None:
        np.random.seed(args.seed)

    # Normalize first: expansions are longer and must fit the chunk budget.
    if not args.no_normalize:
        text = normalize_vi_text(text)

    # Group the text by sentence, then chunk any sentence that is still over
    # the duration budget.  `chunk_text` alone finds the sentence boundaries
    # but then greedily packs short sentences back into one chunk, so the gap
    # between them is just whatever the model emits at an interior `.`; by
    # synthesising each sentence as its own utterance we get a real pause
    # there, `--sentence-gap-sec`, distinct from the `--gap-sec` used between
    # the chunks of one long sentence.
    if args.chunk:
        groups = [
            [c for c in (clean_segment_punctuation(s) for s in
                         chunk_text(sent, max_chunk_sec=args.max_chunk_sec)) if c]
            for sent in split_sentences(normalize_punctuation(text))
        ]
        groups = [g for g in groups if g]
    else:
        groups = [[text]]

    sampling = dict(
        cfg_scale=args.cfg_scale,
        audio_temperature=args.audio_temperature,
        audio_topk=args.audio_topk,
        audio_topp=args.audio_topp,
        audio_repetition_penalty=args.audio_repetition_penalty,
    )

    total = sum(len(g) for g in groups)
    sentences = []
    n = 0
    for group in groups:
        parts = []
        for seg in group:
            n += 1
            if total > 1:
                print(f"    [{n}/{total}] {seg[:70]}{'…' if len(seg) > 70 else ''}")
            parts.append(tts.synthesize(seg, voice=voice or args.voice, **sampling))
        sentences.append(concat_with_silence(parts, args.gap_sec, tts.sample_rate))
    audio = concat_with_silence(sentences, args.sentence_gap_sec, tts.sample_rate)

    dest.parent.mkdir(parents=True, exist_ok=True)
    if args.format == "mp3":
        wav = TMP_DIR / (dest.stem + ".wav")
        wav.parent.mkdir(parents=True, exist_ok=True)
        tts.save_audio(audio, str(wav))
        encode_mp3(wav, dest, args.bitrate)
        wav.unlink(missing_ok=True)
        # A clip generated as wav before the default changed would still shadow
        # the mp3 in Assets.voice, so retire it.
        drop_other_encodings(dest)
    else:
        tts.save_audio(audio, str(dest))

    return float(audio.shape[-1]) / tts.sample_rate


# --- main ------------------------------------------------------------------

def parse_args(argv=None) -> argparse.Namespace:
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    src = p.add_argument_group("what to speak")
    src.add_argument("--key",
                     help="property key to synthesize, e.g. lobby.title; a shell "
                          "pattern such as 'campaign.conjurer.*' matches every "
                          "key under that prefix")
    src.add_argument("--keys", help="comma-separated property keys; patterns allowed")
    src.add_argument("--all", action="store_true", help="every key in vi.properties")
    src.add_argument("--campaign", action="store_true",
                     help="every spoken campaign.* line (messages, win/lose, narration); "
                          "skips .name labels and .choice buttons")
    src.add_argument("--site", action="store_true",
                     help="every site.* line the landing page speaks; written to "
                          f"{SITE_OUT_DIR.relative_to(ROOT)} unless -o is given")
    src.add_argument("--text", help="speak this literal text instead of a bundle entry")
    src.add_argument("--list-keys", nargs="?", const="", default=None, metavar="FILTER",
                     help="list keys in vi.properties, optionally matching FILTER")
    src.add_argument("--list-voices", action="store_true",
                     help="list available voices (bundled, installed and asset)")
    src.add_argument("--resolve-voices", action="store_true",
                     help="print the character → voice table the map resolves to")

    out = p.add_argument_group("output")
    out.add_argument("-o", "--out-dir", type=Path, default=None,
                     help=f"where the audio goes (default: {OUT_DIR.relative_to(ROOT)}, "
                          f"or {SITE_OUT_DIR.relative_to(ROOT)} with --site)")
    out.add_argument("--format", choices=("wav", "mp3"), default="mp3",
                     help="output encoding; mp3 (default) matches assets/sounds/")
    out.add_argument("--bitrate", default="128k",
                     help="MP3 bitrate for --format mp3 (default: 128k)")
    out.add_argument("--compress", action="store_true",
                     help="re-encode existing .wav/.ogg files in --out-dir to mp3 "
                          "without synthesizing; the sources are deleted")
    out.add_argument("--force", action="store_true", help="overwrite existing files")
    out.add_argument("--dry-run", action="store_true",
                     help="print what would be written, without synthesizing")

    voc = p.add_argument_group("voice")
    voc.add_argument("-v", "--voice", default=None,
                     help="force one voice pack for every key instead of the "
                          f"per-character map (default: {DEFAULT_VOICE}); "
                          "accepts the map fields 'narrator_voice' and "
                          "'default_voice' (or 'narrator'/'default')")
    voc.add_argument("--voice-map", type=Path, default=VOICE_MAP,
                     help="campaign character → voice map "
                          f"(default: {VOICE_MAP.relative_to(ROOT)})")
    voc.add_argument("--voices-dir", type=Path, default=VOICES_DIR,
                     help="directory of extra voice packs (.zip downloads or "
                          "unpacked pack folders) to load beside the bundled "
                          f"ones (default: {VOICES_DIR.relative_to(ROOT)})")
    voc.add_argument("--model", help="HF repo id or local model directory")
    voc.add_argument("--model-dir", type=Path, default=None,
                     help="where a repo id is materialised as real files "
                          "(default: $ZEROTTS_MODEL_DIR or ~/.cache/zerotts/<repo>)")
    voc.add_argument("--revision", help="HF revision to pin")
    voc.add_argument("--refresh", action="store_true",
                     help="check the Hub for model updates even when a complete "
                          "copy is already cached (default: use the cache as-is)")
    voc.add_argument("--threads", type=int, default=4, help="onnxruntime intra-op threads")
    voc.add_argument("--workers", type=int, default=1,
                     help="parallel file downloads; 1 = one at a time (default)")
    voc.add_argument("--seed", type=int, default=None, help="seed the sampler")
    voc.add_argument("--no-normalize", action="store_true",
                     help="skip Vietnamese date/time/number expansion")

    txt = p.add_argument_group("text handling")
    txt.add_argument("--keep-placeholders", action="store_true",
                     help="speak {0}-style placeholders instead of dropping them")
    txt.add_argument("--no-chunk", dest="chunk", action="store_false",
                     help="treat the whole string as one utterance")
    txt.add_argument("--max-chunk-sec", type=float, default=15.0)
    txt.add_argument("--gap-sec", type=float, default=0.15,
                     help="silence between chunks of one long sentence")
    txt.add_argument("--sentence-gap-sec", type=float, default=0.35,
                     help="silence between sentences")
    p.set_defaults(chunk=True)

    gen = p.add_argument_group("sampling (ZeroTTS defaults)")
    gen.add_argument("--cfg-scale", type=float, default=1.0)
    gen.add_argument("--audio-temperature", type=float, default=0.8)
    gen.add_argument("--audio-topk", type=int, default=25)
    gen.add_argument("--audio-topp", type=float, default=0.95)
    gen.add_argument("--audio-repetition-penalty", type=float, default=1.2)
    return p.parse_args(argv)


def compress_dir(out_dir: Path, bitrate: str, force: bool, dry_run: bool) -> int:
    """Transcode every .wav/.ogg clip in `out_dir` to mp3 and delete the source.

    This is the escape hatch for a batch that was generated while wav was still
    the default: re-synthesising it would cost minutes, re-encoding does not.
    Returns how many clips would be (or were) compressed.
    """
    sources = sorted(
        p for p in out_dir.glob("*")
        if p.suffix.lower() in (".wav", ".ogg")
    )
    if not sources:
        print(f"nothing to compress in {out_dir}")
        return 0
    done = 0
    for src in sources:
        dest = src.with_suffix(".mp3")
        if dest.exists() and not force:
            print(f"skip {dest.name}: exists (use --force)")
            continue
        action = "would compress" if dry_run else "compress"
        print(f"{action} {src.name} → {dest.name}")
        if not dry_run:
            encode_mp3(src, dest, bitrate)
            src.unlink()
        done += 1
    print(f"compressed {done} file(s) in {out_dir}")
    return done


def campaign_keys(props: dict[str, str]) -> list[str]:
    """The campaign lines the game can actually speak.

    That is every `campaign.*` entry except the `.name` labels and `.choice`
    buttons, which are drawn but never played.  Order follows the bundle.
    """
    suffix_includeds = ["message", "win", "lose", "wake",
                        "epilogue", "intro", "rest", "visit", "meal", "out"]
    return [
        key for key in props
        if key.startswith("campaign.")
        and not key.endswith(".name")
        and ".choice." not in key
        and any(suffix for suffix in suffix_includeds if key.endswith(suffix))
    ]


def site_keys(props: dict[str, str]) -> list[str]:
    """The `site.*` entries the website can speak (its About story, ...).

    These live in the bundle for the landing page rather than the game, and
    their audio belongs under `docs/`.  Order follows the bundle.
    """
    return [key for key in props if key.startswith("site.")]


def expand_keys(names: list[str], props: dict[str, str]) -> list[str]:
    """Resolve exact keys and shell-glob patterns against the bundle.

    A name containing `*`, `?` or `[` is an `fnmatch` pattern and expands to
    every matching key in bundle order (a plain `*` crosses dots, so
    `campaign.conjurer.*` selects every key under that prefix).  Exact names
    pass through untouched; anything that matches nothing is an error.
    """
    selected: list[str] = []
    seen: set[str] = set()
    for name in names:
        if any(ch in name for ch in "*?["):
            matched = [k for k in props if fnmatch.fnmatchcase(k, name) and not k.endswith("name")  and not k.endswith("title")]
            if not matched:
                sys.exit(f"no keys match: {name} (try --list-keys)")
        elif name in props:
            matched = [name]
        else:
            sys.exit(f"unknown key: {name} (try --list-keys)")
        for key in matched:
            if key not in seen:
                seen.add(key)
                selected.append(key)
    return selected


def select_keys(args, props: dict[str, str]) -> list[str]:
    if args.key:
        return expand_keys([args.key], props)
    if args.keys:
        names = [k.strip() for k in args.keys.split(",") if k.strip()]
        return expand_keys(names, props)
    if args.all:
        return list(props)
    if args.campaign:
        return campaign_keys(props)
    if args.site:
        return site_keys(props)
    sys.exit("nothing to do: pass --key, --keys, --all, --campaign, --site, "
             "--text, --list-keys or --list-voices")


def main(argv=None) -> int:
    args = parse_args(argv)
    # Site entries belong to the website, so --site writes into docs/ by
    # default; an explicit -o still wins.  Resolve before --compress uses it.
    if args.out_dir is None:
        args.out_dir = SITE_OUT_DIR if args.site else OUT_DIR
    if args.compress:
        compress_dir(args.out_dir, args.bitrate, args.force, args.dry_run)
        return 0
    props = load_properties(PROPERTIES)
    vmap = load_voice_map(args.voice_map)
    force_voice = resolve_voice_alias(args.voice, vmap)  # explicit -v/--voice, else None
    args.voice = force_voice or map_default_voice(vmap)

    if args.list_keys is not None:
        for key in props:
            if args.list_keys in key:
                print(key)
        return 0

    if args.list_voices:
        tts = build_tts(args)
        names = tts.list_voices()
        if not names:
            print(f"no voice packs in {args.model or 'the default model'}")
            return 1
        for name in names:
            v = tts.load_voice(name)
            desc = f"  {v.description}" if v.description else ""
            print(f"{name:20s} {v.language:4s} {v.n_voice_queries} queries{desc}")
        return 0

    if args.resolve_voices:
        if not vmap:
            sys.exit(f"no voice map at {args.voice_map}")
        characters = vmap.get("characters") or {}
        skipped = vmap.get("skipped") or {}
        width = max((len(c) for c in characters), default=0)
        for cid in sorted(characters):
            entry = characters[cid]
            tags = entry if isinstance(entry, dict) else {}
            voice = voice_for_key(f"campaign.{cid}.message", vmap)
            print(f"{cid:{width}s}  {voice or '-':11s} "
                  f"{tags.get('gender', ''):5s} {tags.get('age', ''):6s} "
                  f"{tags.get('tone', '')}")
        for cid, reason in sorted(skipped.items()):
            print(f"{cid:{width}s}  (skipped: {reason})")
        return 0

    if args.text:
        jobs = [(None, args.text)]
    else:
        jobs = [(key, props[key]) for key in select_keys(args, props)]

    def voice_of(key: str | None) -> str:
        # An explicit --voice wins; otherwise the map picks a voice per key.
        if force_voice is not None:
            return force_voice
        return voice_for_key(key, vmap) or args.voice

    def skip_reason(key: str | None) -> str | None:
        if force_voice is not None:
            return None
        return skipped_character(key, vmap)

    if args.dry_run:
        for key, value in jobs:
            name = output_name(key, args.format) if key else "text." + args.format
            reason = skip_reason(key)
            tag = f"skipped: {reason}" if reason else voice_of(key)
            print(f"{name} [{tag}]: "
                  f"{spoken_text(value, args.keep_placeholders)}")
        return 0

    todo = []
    for key, value in jobs:
        dest = args.out_dir / (output_name(key, args.format) if key
                               else "text." + args.format)
        text = spoken_text(value, args.keep_placeholders)
        reason = skip_reason(key)
        if not text:
            print(f"skip {key or '(text)'}: empty after cleaning")
        elif reason:
            print(f"skip {key}: {reason}")
        elif dest.exists() and not args.force:
            print(f"skip {dest.name}: exists (use --force)")
        else:
            todo.append((key, text, dest, voice_of(key)))

    if not todo:
        return 0

    tts = build_tts(args)
    for key, text, dest, voice in todo:
        print(f"{key or '(text)'} [{voice}] → {dest.relative_to(ROOT)}")
        secs = synthesize_one(tts, text, args, dest, voice=voice)
        print(f"  {secs:.2f}s audio")
    print(f"done: {len(todo)} file(s) in {args.out_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
