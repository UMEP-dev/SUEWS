#!/usr/bin/env bash
# Run the SUEWS agent eval suite on your own Claude Code account, one model at
# a time, with and without the plugin. Results land in plugins/suews/evals/results/.
#
# Usage (from anywhere inside the SUEWS checkout):
#   plugins/suews/evals/run_local.sh                 # haiku only (cheapest first pass)
#   plugins/suews/evals/run_local.sh haiku sonnet opus
#   MAX_COST_USD=10 RUNS=1 plugins/suews/evals/run_local.sh sonnet
#
# Each case runs as its own eval call so it gets only the operator grants it
# needs: d-diagnose-bad-fractions is a read-only task and is granted only the
# three read-only SUEWS MCP tools it lists, never Bash, Write, Edit or a tool
# that writes a case. The cost ceiling applies per case.
set -euo pipefail

repo="$(git rev-parse --show-toplevel)"
cd "$repo"
models=("${@:-haiku}")
max_cost="${MAX_COST_USD:-20}"
runs="${RUNS:-3}"
stamp="$(date -u +%Y%m%dT%H%M%SZ)"
out="plugins/suews/evals/results/$stamp"
mcp_grant='mcp__plugin_suews_suews__*'
read_only_grants=(
  mcp__plugin_suews_suews__validate_config
  mcp__plugin_suews_suews__inspect_config
  mcp__plugin_suews_suews__diagnose_run
)

# The eval loads the skill from plugins/suews/skills/, a gitignored build artefact.
python3 scripts/build_plugin.py >/dev/null

for model in "${models[@]}"; do
  mkdir -p "$out/$model"
  echo "== $model (runs=$runs, cost ceiling \$$max_cost per case)"
  for case_yaml in plugins/suews/evals/*/case.yaml; do
    case_name="$(basename "$(dirname "$case_yaml")")"
    if [[ "$case_name" == d-diagnose-bad-fractions ]]; then
      grants=("${read_only_grants[@]}")
    else
      grants=(Bash Write Edit "$mcp_grant")
    fi
    echo "   -- $case_name (grants: ${grants[*]})"
    claude plugin eval plugins/suews \
      --case "$case_name" \
      --model "$model" --judge-model opus --runs "$runs" \
      --mocks off --scaffold --trust-plugin --no-publish \
      --allow-tools "${grants[@]}" \
      --max-cost-usd "$max_cost" \
      --threshold 0 \
      --json "$out/$model/$case_name.json" \
      --report "$out/$model/$case_name.html" \
      || echo "   $case_name finished with exit $? (see $out/$model/$case_name.json)"
  done
done

echo "Results: $out"
