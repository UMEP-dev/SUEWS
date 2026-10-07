#!/usr/bin/env bash
# Run the SUEWS agent eval suite on your own Claude Code account, one model at
# a time, with and without the plugin. Results land in plugins/suews/evals/results/.
#
# Usage (from anywhere inside the SUEWS checkout):
#   plugins/suews/evals/run_local.sh                 # haiku only (cheapest first pass)
#   plugins/suews/evals/run_local.sh haiku sonnet opus
#   MAX_COST_USD=10 RUNS=1 plugins/suews/evals/run_local.sh sonnet
set -euo pipefail

repo="$(git rev-parse --show-toplevel)"
cd "$repo"
models=("${@:-haiku}")
max_cost="${MAX_COST_USD:-20}"
runs="${RUNS:-3}"
stamp="$(date -u +%Y%m%dT%H%M%SZ)"
out="plugins/suews/evals/results/$stamp"
mkdir -p "$out"

# The eval loads the skill from plugins/suews/skills/, a gitignored build artefact.
python3 scripts/build_plugin.py >/dev/null

for model in "${models[@]}"; do
  echo "== $model (runs=$runs, cost ceiling \$$max_cost)"
  claude plugin eval plugins/suews \
    --model "$model" --judge-model opus --runs "$runs" \
    --mocks off --scaffold --trust-plugin --no-publish \
    --allow-tools Bash Write Edit 'mcp__plugin_suews_suews__*' \
    --max-cost-usd "$max_cost" \
    --threshold 0 \
    --json "$out/$model.json" \
    --report "$out/$model.html" || echo "   $model finished with exit $? (see $out/$model.json)"
done

echo "Results: $out"
