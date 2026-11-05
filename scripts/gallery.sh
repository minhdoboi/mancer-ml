#!/bin/sh
# Regenerate docs/gallery/photos.json from the pictures in docs/gallery/.
# The gallery page reads that list and shows ten pictures at a time, adding
# the rest as the reader scrolls. Run this after adding or removing a picture,
# and commit the manifest with the page.
set -eu
cd "$(dirname "$0")/.."
dir=docs/gallery
python3 - "$dir" <<'PY'
import json, os, sys
d = sys.argv[1]
exts = ('.jpg', '.jpeg', '.png', '.webp', '.gif')
names = sorted(
    (n for n in os.listdir(d) if n.lower().endswith(exts)),
    key=str.lower,
)
with open(os.path.join(d, 'photos.json'), 'w', encoding='utf-8') as f:
    json.dump(names, f, ensure_ascii=False, indent=2)
    f.write('\n')
print(f'wrote {d}/photos.json ({len(names)} photos)')
PY
