#!/usr/bin/env sh
# Check that a packaged build actually starts.
#
#   scripts/smoke.sh                       # the newest package for this OS
#   scripts/smoke.sh dist/mancer-1.0-mac-aarch64
#   scripts/smoke.sh dist/mancer-1.0.zip   # the archive the player gets
#   scripts/smoke.sh --no-window           # the assets only, no display needed
#   scripts/smoke.sh --allow-missing-window   # no OpenGL here is not a failed build
#
# The game opens a hidden window, loads its fonts and its art, and plays a
# few seconds of a game. It exits 0 when the build runs, 1 when the package
# is broken, and 3 when this machine has no display and no OpenGL 3.3 — the
# last one is the machine's fault, not the package's, and
# --allow-missing-window turns it into a warning. CI runs this on the
# packaged macOS app and on the Windows exe, so a build that cannot start
# fails the pipeline instead of the player's desktop.
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
case $uname_s in
  linux*) os=linux ;;
  darwin*) os=mac ;;
  mingw*|msys*|cygwin*|windows*) os=windows ;;
  *) os=$uname_s ;;
esac

# The package is the first argument that names one; --allow-missing-window
# is for this script, the rest go to the game (--no-window, --check-secs).
folder=""
allow_missing=0
game_args=""
for arg in "$@"; do
  case $arg in
    --allow-missing-window) allow_missing=1 ;;
    *.zip) folder=$arg ;;
    *)
      if [ -z "$folder" ] && [ -d "$arg" ]; then folder=$arg
      else game_args="$game_args $arg"
      fi
      ;;
  esac
done
if [ -z "$folder" ]; then
  folder=$(ls -dt "dist/mancer-"*-"$os-$arch" 2>/dev/null | head -1 || true)
  [ -n "$folder" ] || {
    echo "no dist/mancer-*-$os-$arch package here; run scripts/package.sh first" >&2
    exit 1
  }
fi

# An archive is unpacked next to itself, so the paths stay relative and a
# Windows build still understands them.
case $folder in
  *.zip)
    command -v unzip >/dev/null 2>&1 || {
      echo "no unzip here to open $folder" >&2
      exit 1
    }
    out=$(dirname "$folder")/.smoke
    rm -rf "$out"
    mkdir -p "$out"
    unzip -q "$folder" -d "$out"
    folder=$(ls -d "$out"/mancer-* | head -1)
    ;;
esac

# The launcher sets MANCER_ASSETS and starts the game; the Windows .bat
# cannot report an exit code (it starts the game in the background), so
# there the exe runs directly.
case $os in
  mac)
    bin="$folder/mancer.app/Contents/MacOS/mancer"
    assets="$folder/mancer.app/Contents/Resources/assets"
    ;;
  windows)
    bin="$folder/mancer.exe"
    assets="$folder/assets"
    ;;
  *)
    bin="$folder/mancer"
    assets="$folder/assets"
    ;;
esac
[ -f "$bin" ] || {
  echo "no game at $bin" >&2
  exit 1
}
[ -d "$assets" ] || {
  echo "no assets at $assets" >&2
  exit 1
}
# The paths are absolute from here: the game may look for its assets after
# the working directory moved.
bin=$(CDPATH= cd -- "$(dirname "$bin")" && pwd)/$(basename "$bin")
assets=$(CDPATH= cd -- "$(dirname "$assets")" && pwd)/$(basename "$assets")
# The game reads MANCER_ASSETS and HOME, and a native Windows program does
# not read the POSIX paths Git Bash hands out. The program itself is started
# by the shell, which needs the POSIX path.
if command -v cygpath >/dev/null 2>&1; then
  assets=$(cygpath -w "$assets")
  # The prefs go to the home folder; the .bat sets the same thing.
  if [ -n "${HOME:-}" ]; then
    HOME=$(cygpath -w "$HOME")
    export HOME
  fi
fi
MANCER_ASSETS="$assets"
export MANCER_ASSETS

# A headless Linux box has no display: Xvfb gives the game one.
runner=""
if [ "$os" = linux ] && [ -z "${DISPLAY:-}" ] && command -v xvfb-run >/dev/null 2>&1; then
  runner="xvfb-run -a"
fi

echo "checking $bin"
# shellcheck disable=SC2086
$runner "$bin" --check $game_args && status=0 || status=$?
case $status in
  0) ;;
  3)
    # The game could not open a window: this machine has no display and no
    # OpenGL 3.3. The package was checked all the same.
    if [ "$allow_missing" = 1 ]; then
      echo "warning: this machine cannot open a window (no display, or no OpenGL 3.3):"
      echo "warning: the package was checked, the window was not"
      exit 0
    fi
    echo "this machine cannot open a window (no display, or no OpenGL 3.3)" >&2
    exit 3
    ;;
  *) exit $status ;;
esac
