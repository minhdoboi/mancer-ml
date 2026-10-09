#!/usr/bin/env sh
# Zip the raylib GUI and assets for this computer.
#
#   scripts/package.sh
#   VERSION=0.1 scripts/package.sh
#
# Writes dist/mancer-<version>-<os>-<arch>/ and a matching zip.
# Build once on Linux, once on macOS, and once on Windows — the binary is
# native. See docs/packaging.md.
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
cd "$root"

uname_s=$(uname -s | tr '[:upper:]' '[:lower:]')
uname_m=$(uname -m | tr '[:upper:]' '[:lower:]')
case $uname_m in
  x86_64|amd64) arch=x64 ;;
  aarch64|arm64) arch=aarch64 ;;
  *) arch=$uname_m ;;
esac

kind=unix
case $uname_s in
  linux*) os=linux ;;
  darwin*) os=mac; kind=mac ;;
  mingw*|msys*|cygwin*|windows*) os=windows; kind=windows ;;
  *) os=$uname_s ;;
esac
platform="$os-$arch"

version=${VERSION:-}
if [ -z "$version" ]; then
  version=$(git describe --tags --always --dirty 2>/dev/null || echo 0.1)
fi
folder="mancer-$version-$platform"
out="$root/dist/$folder"
zip="$root/dist/$folder.zip"

[ -d assets/cards ] || {
  echo "assets/ is empty. Run: git submodule update --init" >&2
  exit 1
}

eval "$(opam env --switch=mancer 2>/dev/null || true)"
dune build --release bin/gui.exe
bin=_build/default/bin/gui.exe
[ -f "$bin" ] || { echo "dune did not write $bin" >&2; exit 1; }

rm -rf "$out"
mkdir -p "$out"
cp LICENSE "$out/LICENSE"

copy_assets() {
  dest=$1
  mkdir -p "$dest"
  [ -f assets/LICENSE ] && cp assets/LICENSE "$dest/LICENSE"
  for d in backgrounds campaign cards combat fonts i18n particle portraits shaders sounds ui; do
    [ -d "assets/$d" ] || continue
    cp -R "assets/$d" "$dest/"
  done
  # The game asks this file which version it is, so the update check can
  # compare it with the latest release.
  printf '%s' "$version" > "$dest/version.txt"
}

# The mingw runtime is not on a player's machine: the exe links against
# libwinpthread-1.dll (OCaml 5 runs on threads), and Windows refuses to
# start the game without it. Copy the DLLs the binary needs next to it; the
# system ones (KERNEL32, USER32, ...) stay on the machine.
copy_runtime_dlls() {
  dest=$1
  exe=$2

  # The exe is x86_64; a 32-bit DLL next to it does not load.
  skip_arch="i686"

  copy_one() {
    [ -f "$1" ] || return 0
    base=$(basename "$1")
    [ -f "$dest/$base" ] && return 0
    cp "$1" "$dest/" 2>/dev/null || return 0
    echo "  + $base"
  }

  # The DLLs the compiler's own runtime ships: the exact ones the binary
  # was linked against, and the right architecture. The x86_64 tree only:
  # a 32-bit DLL does not load next to a 64-bit exe.
  for cc in x86_64-w64-mingw32-gcc ocamlopt ocamlc; do
    cc_path=$(command -v "$cc" 2>/dev/null) || continue
    dir=$(dirname "$cc_path")
    for name in libwinpthread-1.dll libgcc_s_seh-1.dll libstdc++-6.dll; do
      copy_one "$dir/$name"
    done
  done

  # The usual mingw trees, for a build that keeps the compiler elsewhere.
  for dir in /mingw64/bin /usr/x86_64-w64-mingw32/sys-root/mingw/bin; do
    for name in libwinpthread-1.dll libgcc_s_seh-1.dll libstdc++-6.dll; do
      copy_one "$dir/$name"
    done
  done

  # And whatever else the binary needs that Windows does not have. The
  # system DLLs resolve into the Windows folder and stay there.
  if command -v ldd >/dev/null 2>&1; then
    ldd "$exe" 2>/dev/null | awk -v skip="$skip_arch" '
      { i = index($0, "=> ")
        if (i == 0) next
        rest = substr($0, i + 3)
        j = index(rest, " (0x")
        if (j > 0) rest = substr(rest, 1, j - 1)
        if (tolower(rest) ~ /\/windows\//) next
        # A 32-bit DLL next to a 64-bit exe does not load.
        if (skip != "" && tolower(rest) ~ skip) next
        print rest }' | while IFS= read -r dll; do
      copy_one "$dll"
    done || true
  fi
}

# mingw's strip does not understand a cygwin path.
strip_exe() {
  command -v strip >/dev/null 2>&1 || return 0
  path=$1
  if command -v cygpath >/dev/null 2>&1; then path=$(cygpath -w "$path"); fi
  strip "$path" 2>/dev/null || true
}

write_readme() {
  dest=$1
  run=$2
  cat >"$dest" <<EOF
The mancer's echo
=================

$run

Settings go in:
  Linux/macOS: ~/.mancer/
  Windows:     %USERPROFILE%\\.mancer\\

Card art and sounds ship in the assets/ folder next to the launcher.
EOF
}

if [ "$kind" = mac ]; then
  app="$out/mancer.app"
  contents="$app/Contents"
  macos="$contents/MacOS"
  resources="$contents/Resources"
  mkdir -p "$macos" "$resources"
  cp "$bin" "$macos/gui"
  chmod 755 "$macos/gui"
  if command -v strip >/dev/null 2>&1; then strip "$macos/gui" || true; fi
  copy_assets "$resources/assets"
  cat >"$macos/mancer" <<'EOF'
#!/bin/sh
set -e
DIR=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
export MANCER_ASSETS="$DIR/Resources/assets"
cd "$DIR/Resources"
exec "$DIR/MacOS/gui" "$@"
EOF
  chmod 755 "$macos/mancer"
  cat >"$contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key>
  <string>The mancer's echo</string>
  <key>CFBundleDisplayName</key>
  <string>The mancer's echo</string>
  <key>CFBundleIdentifier</key>
  <string>com.mancer.echo</string>
  <key>CFBundleVersion</key>
  <string>$version</string>
  <key>CFBundleShortVersionString</key>
  <string>$version</string>
  <key>CFBundleExecutable</key>
  <string>mancer</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>LSMinimumSystemVersion</key>
  <string>11.0</string>
  <key>NSHighResolutionCapable</key>
  <true/>
</dict>
</plist>
EOF
  # Ad-hoc signature: Apple Silicon refuses to run an unsigned binary at all.
  # (A Developer ID signature replaces this; see docs/packaging.md.)
  if command -v codesign >/dev/null 2>&1; then
    codesign --force --deep --sign - "$app"
  fi
  write_readme "$out/README.txt" \
    "Open mancer.app. The first time, right-click it and choose Open (the download is not signed)."

elif [ "$kind" = windows ]; then
  cp "$bin" "$out/mancer.exe"
  strip_exe "$out/mancer.exe"
  copy_runtime_dlls "$out" "$out/mancer.exe"
  copy_assets "$out/assets"
  # Git Bash / MSYS writes LF; CRLF is what Explorer expects.
  printf '%s\r\n' \
    '@echo off' \
    'setlocal' \
    'cd /d "%~dp0"' \
    'if not defined HOME set HOME=%USERPROFILE%' \
    'set MANCER_ASSETS=%CD%\assets' \
    'start "" "%CD%\mancer.exe" %*' \
    >"$out/mancer.bat"
  write_readme "$out/README.txt" \
    "Double-click mancer.bat."

else
  cp "$bin" "$out/mancer.bin"
  chmod 755 "$out/mancer.bin"
  if command -v strip >/dev/null 2>&1; then strip "$out/mancer.bin" || true; fi
  copy_assets "$out/assets"
  cat >"$out/mancer" <<'EOF'
#!/bin/sh
set -e
DIR=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
export MANCER_ASSETS="$DIR/assets"
cd "$DIR"
exec "$DIR/mancer.bin" "$@"
EOF
  chmod 755 "$out/mancer"
  write_readme "$out/README.txt" \
    "Run ./mancer from this folder."
fi

rm -f "$zip"
# zip from dist/ so the archive contains the versioned folder.
if command -v zip >/dev/null 2>&1; then
  (cd "$root/dist" && zip -qr "$folder.zip" "$folder")
else
  (cd "$root/dist" && python3 -m zipfile -c "$folder.zip" "$folder")
fi
echo "Wrote $out"
echo "Wrote $zip"
