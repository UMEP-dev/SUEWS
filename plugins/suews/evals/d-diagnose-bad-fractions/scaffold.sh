#!/usr/bin/env bash
# Plant one known fault: the building surface fraction is raised from 0.38 to
# 0.58, so the seven land-cover fractions sum to 1.2 instead of 1.0.
#
# The sample configuration and its forcing file come from the SuPy that the
# plugin's MCP server runs on (the uvx spec in plugins/suews/.mcp.json), not
# from this checkout, so the fixture's schema version matches the validator
# the agent calls and the fraction sum is the only fault.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
mcp_json="$here/../../.mcp.json"
spec="$(python3 - "$mcp_json" <<'PY'
import json, sys
args = json.load(open(sys.argv[1]))["mcpServers"]["suews"]["args"]
print(args[args.index("--from") + 1])
PY
)"
uvx --quiet --from "$spec" python - <<'PY'
import re
import shutil
from importlib.resources import files

sample = files("supy") / "sample_data"
text = (sample / "sample_config.yml").read_text(encoding="utf-8")
planted, n = re.subn(r"(sfr:\n\s*value: )0\.38\b", r"\g<1>0.58", text)
assert n == 1, "sample_config.yml changed: re-plant the fault"
open("config.yml", "w", encoding="utf-8").write(planted)
with (sample / "Kc_2012_data_60.txt").open("rb") as src, open("Kc_2012_data_60.txt", "wb") as dst:
    shutil.copyfileobj(src, dst)
PY
