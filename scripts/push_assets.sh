#!/usr/bin/env sh
# Commit what changed in the assets submodule (hand edits, or a fresh
# sync_assets run), push it, and stage the new submodule pointer here.
#
#   scripts/push_assets.sh "message"
#
# The parent repo is left uncommitted; review with `git status`, then commit.
set -eu

msg=${1:?usage: scripts/push_assets.sh "commit message"}
root=$(git -C "$(dirname "$0")" rev-parse --show-toplevel)
cd "$root"

[ -e assets/.git ] || { echo "assets/ is not a checked-out submodule (git submodule update --init)" >&2; exit 1; }

branch=$(git config -f .gitmodules submodule.assets.branch || echo master)

git -C assets add -A
if git -C assets diff --cached --quiet; then
  echo "assets: nothing to commit"
else
  git -C assets commit -q -m "$msg"
  git -C assets --no-pager log --stat --oneline -1
fi

# a fresh clone leaves the submodule on a detached HEAD, so name the branch
git -C assets push origin "HEAD:refs/heads/$branch"
git add assets
echo "staged assets pointer: $(git -C assets rev-parse --short HEAD); commit it here when ready"
