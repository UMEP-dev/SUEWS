#!/usr/bin/env bash
# Plant one known fault: the building surface fraction is raised from 0.38 to
# 0.58, so the seven land-cover fractions sum to 1.2 instead of 1.0.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../../../.." && pwd)"
python3 - "$repo/src/supy/sample_data/sample_config.yml" config.yml <<'PY'
import sys
src, dst = sys.argv[1], sys.argv[2]
text = open(src, encoding="utf-8").read()
old = "sfr:\n          value: 0.38"
assert text.count(old) == 1, "sample_config.yml changed: re-plant the fault"
open(dst, "w", encoding="utf-8").write(text.replace(old, "sfr:\n          value: 0.58"))
PY
