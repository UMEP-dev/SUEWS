#!/usr/bin/env bash
# Install, into ./.venv of the current eval workspace, the same SuPy release
# that the plugin's MCP server runs on (resolved through the uvx spec in
# plugins/suews/.mcp.json). Scaffolds call this so the with-plugin and
# no-plugin arms start from the same runtime, and the ablation delta measures
# the plugin rather than whether SuPy happens to be installed.
set -euo pipefail
evals="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
spec="$(python3 - "$evals/../.mcp.json" <<'PY'
import json, sys
args = json.load(open(sys.argv[1]))["mcpServers"]["suews"]["args"]
print(args[args.index("--from") + 1])
PY
)"
version="$(uvx --quiet --from "$spec" python -c 'import supy; print(supy.__version__)')"
uv venv --quiet .venv
uv pip install --quiet --python .venv/bin/python "supy==$version"
