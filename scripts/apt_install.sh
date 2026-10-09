#!/usr/bin/env sh
# Install packages with apt on a GitHub Actions Ubuntu runner.
#
# The runner's apt sources go through /etc/apt/apt-mirrors.txt, which points
# at azure.archive.ubuntu.com. Its DNS entry is flaky, and when it fails apt
# dies with "Temporary failure resolving 'azure.archive.ubuntu.com'" and
# never downloads a single .deb — a whole CI job lost to a hiccup.
#
# So: point apt at the public archive, retry the update/install a few times,
# and let apt itself retry each download. Run it with sudo:
#
#   sudo sh scripts/apt_install.sh libx11-dev libgl-dev ...
set -eu

# The mirror file comes first: sources.list reaches the mirror through it.
if [ -f /etc/apt/apt-mirrors.txt ]; then
  sed -i 's|//azure\.archive\.ubuntu\.com|//archive.ubuntu.com|g' \
    /etc/apt/apt-mirrors.txt
fi
sed -i 's|//azure\.archive\.ubuntu\.com|//archive.ubuntu.com|g' \
  /etc/apt/sources.list /etc/apt/sources.list.d/*.list \
  /etc/apt/sources.list.d/*.sources 2>/dev/null || true

apt_opts="-o Acquire::Retries=5 -o Acquire::http::Timeout=30 -o Acquire::https::Timeout=30"

# The resolver often comes back on its own, so retry the update with backoff
# instead of failing the job.
i=1
while [ "$i" -le 5 ]; do
  if apt-get $apt_opts update; then
    break
  fi
  echo "apt-get update failed (attempt $i/5), retrying..." >&2
  sleep $((i * 15))
  i=$((i + 1))
done

# Even if the update above gave up, install can still use the cached index;
# failure to reach the archive is then reported per-package.
apt-get $apt_opts install -y "$@"
