#!/usr/bin/env bash
# test-detect-changes.sh - Run CI path detection locally using act
#
# Runs the actual dorny/paths-filter action (same config as CI) in Docker
# via nektos/act, so results match what GitHub Actions would produce.
#
# Usage:
#   ./scripts/test-detect-changes.sh              # compare HEAD vs origin/master
#   ./scripts/test-detect-changes.sh main          # compare HEAD vs main
#   ./scripts/test-detect-changes.sh abc123        # compare HEAD vs specific commit
#
# Prerequisites:
#   - act (brew install act)
#   - Docker running

set -euo pipefail

BASE="${1:-origin/master}"

# Resolve to SHA so the container doesn't need to git-fetch from the remote
# (no SSH credentials available inside Docker).
BASE_SHA="$(git rev-parse "$BASE")"

echo "Comparing HEAD against: $BASE ($BASE_SHA)"
echo "---"

# Base act arguments
ACT_ARGS=(
  workflow_dispatch
  -W .github/workflows/test-detect-changes.yml
  --input "base=$BASE_SHA"
  --detect-event
  --container-architecture linux/amd64
  --pull=false
  # node:20 is sufficient: dorny/paths-filter is a JS action.
  # Shell steps (echo, etc.) work fine in node:20 which includes bash,
  # but the full ubuntu-latest toolset (apt packages, etc.) is not available.
  # The pyproject.toml content-classification step (Python) only runs in
  # the real CI workflow, not this local test (see workflow NOTE).
  --platform ubuntu-latest=node:20
)

# Handle git worktrees: the .git file references a path outside the working
# directory which won't be available inside Docker. Mount the common .git
# directory so git operations work correctly in the container.
GIT_COMMON_DIR_RAW="$(git rev-parse --git-common-dir)"
if [[ "$GIT_COMMON_DIR_RAW" != ".git" ]]; then
  # Resolve to absolute path — git rev-parse can return relative paths
  # when run from a subdirectory, which breaks Docker volume mounts.
  GIT_COMMON_DIR="$(cd "$GIT_COMMON_DIR_RAW" && pwd)"
  echo "Worktree detected, mounting: $GIT_COMMON_DIR"
  # --bind: use host bind-mount instead of copy so the external .git dir is reachable
  ACT_ARGS+=(--bind)
  ACT_ARGS+=(--container-options "-v ${GIT_COMMON_DIR}:${GIT_COMMON_DIR}")
fi

act "${ACT_ARGS[@]}"

# --- Host-side pyproject.toml content classification ---
# The Docker container (node:20) lacks Python 3.11+, so classify_pyproject.py
# cannot run inside act. Run it on the host where Python is available.
# This mirrors the real CI classify-pyproject step (build-publish_to_pypi.yml).

if git diff --name-only "$BASE_SHA" HEAD | grep -q '^pyproject\.toml$'; then
  echo ""
  echo "=== pyproject.toml Content Classification (host-side) ==="
  PYPROJECT_SCRIPT=".github/scripts/classify_pyproject.py"
  if python3 -c 'import tomllib' 2>/dev/null; then
    TMPOUT=$(mktemp)
    if BASE_SHA="$BASE_SHA" GITHUB_OUTPUT="$TMPOUT" python3 "$PYPROJECT_SCRIPT" 2>&1; then
      echo "Classification outputs:"
      while IFS='=' read -r key val; do
        echo "  $key: $val"
      done < "$TMPOUT"
    else
      echo "WARNING: classify_pyproject.py failed (exit $?)."
    fi
    rm -f "$TMPOUT"
  else
    PY_VER=$(python3 -c 'import sys; print(".".join(map(str, sys.version_info[:2])))' 2>/dev/null || echo "unknown")
    echo "WARNING: Python $PY_VER lacks tomllib (needs 3.11+). Cannot classify."
    echo "  All pyproject.toml changes treated as build-triggering (conservative)."
  fi
else
  echo ""
  echo "(pyproject.toml unchanged -- classification skipped)"
fi
