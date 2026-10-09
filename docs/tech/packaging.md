# Packaging

The GUI is a native OCaml + raylib binary. raylib is linked statically, so a
package is the executable, the `assets/` tree, and a launcher that points at
that tree. Unlike the Scala `desktop:packageApp` (which can download a JRE
for every OS from one machine), you build **once on each OS**.

```sh
scripts/package.sh
```

That writes `dist/mancer-<version>-<os>-<arch>/` and a matching zip. Override
the version with `VERSION=1.0 scripts/package.sh`. The default is
`git describe`.

Players run `./mancer` on Linux, `mancer.app` on macOS, or `mancer.bat` on
Windows. Settings stay in `~/.mancer/` (or `%USERPROFILE%\.mancer` on
Windows). The launcher sets `MANCER_ASSETS` so art is found even if the
working directory is elsewhere.

## CI (recommended)

`.github/workflows/package.yml` runs `scripts/package.sh` on `ubuntu-22.04`,
`macos-14` (arm64), `macos-15-intel` (x64) and `windows-latest`, and uploads
each zip as an artifact. Push a `v*` tag, or run the workflow by hand, to
build; a tag also attaches the zips to a GitHub Release.

The Linux job builds on glibc 2.35 and fails if the binary needs a newer
one. A binary only runs on systems at least as new as the glibc it was built
against, so do not ship a zip built on a rolling distro (Arch, Fedora
Rawhide, Ubuntu devel). The macOS jobs set `MACOSX_DEPLOYMENT_TARGET=11.0`.
The macOS app gets an ad-hoc signature so Apple Silicon will launch it.

## Linux

Needs a desktop with X11 (`libX11`). Wayland sessions work through Xwayland;
the GUI clears `WAYLAND_DISPLAY` so GLFW talks to X11.

```sh
eval $(opam env --switch=mancer)
opam install notty raylib
git submodule update --init
dune build --release bin/gui.exe
scripts/package.sh
```

`scripts/package.sh` copies `_build/default/bin/gui.exe` as `mancer.bin`,
strips it, copies the runtime asset folders, and writes a `./mancer`
wrapper. Ship `dist/mancer-*-linux-*.zip`.

To do the same by hand:

```sh
mkdir -p dist/mancer/assets
cp _build/default/bin/gui.exe dist/mancer/mancer.bin
strip dist/mancer/mancer.bin
cp -R assets/backgrounds assets/campaign assets/cards assets/combat \
      assets/fonts assets/i18n assets/particle assets/portraits \
      assets/shaders assets/sounds assets/ui dist/mancer/assets/
# launcher: cd to the folder, export MANCER_ASSETS=$PWD/assets, exec ./mancer.bin
```

Do not copy `assets/overrides/` or `assets/.git`. Overrides are already
baked in by `sync_assets`.

## macOS

Same script, run on a Mac (Intel → `mac-x64`, Apple Silicon → `mac-aarch64`).
A Linux machine cannot produce that zip: the binary is native OCaml 5.2 +
raylib, and there is no maintained Linux→macOS toolchain for that stack.
`opam-cross-macos` stops at OCaml 4.14; raylib-ocaml builds the C library
from source against Cocoa / CoreAudio, which need Apple's SDK. Use a Mac
or a `macos-*` CI runner.

```sh
eval $(opam env --switch=mancer)
opam install notty raylib
git submodule update --init
scripts/package.sh
```

That builds `mancer.app`:

```text
mancer.app/Contents/MacOS/mancer     launcher
mancer.app/Contents/MacOS/gui        the binary
mancer.app/Contents/Resources/assets
mancer.app/Contents/Info.plist
```

The first launch needs right-click → Open; the app is not signed. To sign
and notarize for Gatekeeper:

```sh
codesign --force --deep --sign "Developer ID Application: …" dist/mancer-*-mac-*/mancer.app
xcrun notarytool submit dist/mancer-*-mac-*.zip --keychain-profile notary --wait
xcrun stapler staple dist/mancer-*-mac-*/mancer.app
```

Universal binaries (x64 + arm64 in one app) are not set up. Ship two zips,
or `lipo` two `gui` builds into one after packaging.

## Windows

Build on Windows. [Diskuv OCaml](https://diskuv.com/ocaml) or an opam
switch with the mingw-w64 compiler both work; the raylib bindings need
mingw (not MSVC). Git Bash or MSYS2 can run `scripts/package.sh`.

```bat
opam env --switch=mancer > tmp.bat && tmp.bat
opam install notty raylib
git submodule update --init
dune build --release bin/gui.exe
sh scripts/package.sh
```

The zip contains `mancer.exe`, `mancer.bat`, and `assets/`. `mancer.bat`
sets `HOME` to `%USERPROFILE%` (so prefs land in `%USERPROFILE%\.mancer`)
and `MANCER_ASSETS` to the shipped `assets\` folder.

raylib is static. If `mancer.exe` still needs mingw runtime DLLs
(`libwinpthread-1.dll`, `libgcc_s_seh-1.dll`, `libstdc++-6.dll`), copy
them next to the exe from the mingw `bin` directory. Check with:

```bat
ldd _build/default/bin/gui.exe
```

(or Dependencies / dumpbin). System libraries (`KERNEL32.dll`,
`USER32.dll`, `opengl32.dll`, …) stay on the machine.

An installer (Inno Setup, NSIS) is optional: the zip already runs. If you
add one, install the folder as-is and point the shortcut at `mancer.bat`.

## What not to ship

- `bin/tui.exe` — needs a real terminal; the zip is the raylib window.
- `bin/main.exe`, `compare_engines`, `sync_assets` — tournament and
  developer tools.
- `assets/overrides/` and the assets git metadata.
- The Scala JRE / `files/vibe` atlas. This port reads loose JPEGs from
  `assets/`, not a libGDX pack.

## Check the zip

Unzip to a throwaway folder (not the repo) and launch without
`MANCER_ASSETS` set. The lobby portraits and a Fire card should have art.
On Linux:

```sh
tmpdir=$(mktemp -d)
unzip -q dist/mancer-*-linux-*.zip -d "$tmpdir"
(cd "$tmpdir"/mancer-* && ./mancer)
```
