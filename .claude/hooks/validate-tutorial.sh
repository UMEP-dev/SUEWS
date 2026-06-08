#!/usr/bin/env bash
# Hook: Validate tutorial files after editing
# Triggered by PostToolUse for Edit/Write on tutorial files

set -euo pipefail

# Fallback to git root if CLAUDE_PROJECT_DIR is not set
CLAUDE_PROJECT_DIR="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"

# Resolve a Python interpreter. Hooks run without an activated venv, so do NOT
# rely on a bare `python` being on PATH (it usually is not). Prefer the repo
# venv, then fall back to python3/python from PATH.
if [[ -x "$CLAUDE_PROJECT_DIR/.venv/bin/python" ]]; then
    PYTHON="$CLAUDE_PROJECT_DIR/.venv/bin/python"
else
    PYTHON="$(command -v python3 || command -v python || true)"
fi
# No interpreter available: nothing we can validate, and we must not block the edit.
if [[ -z "$PYTHON" ]]; then
    exit 0
fi

# Get the edited file path from stdin (JSON format)
INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // .tool_input.filePath // empty')

# Exit if no path provided
if [[ -z "$FILE_PATH" ]]; then
    exit 0
fi

# Resolve to canonical path under project root
if [[ "$FILE_PATH" = /* ]]; then
    CANDIDATE_PATH="$FILE_PATH"
else
    CANDIDATE_PATH="$CLAUDE_PROJECT_DIR/$FILE_PATH"
fi

CANONICAL_PATH=$("$PYTHON" -c 'import os,sys; print(os.path.realpath(sys.argv[1]))' "$CANDIDATE_PATH" 2>/dev/null || true)
PROJECT_ROOT=$("$PYTHON" -c 'import os,sys; print(os.path.realpath(sys.argv[1]))' "$CLAUDE_PROJECT_DIR")

# Only validate tutorial Python files inside this repository
if [[ -z "$CANONICAL_PATH" ]] || [[ ! "$CANONICAL_PATH" =~ ^$PROJECT_ROOT/docs/source/tutorials/[^/]+\.py$ ]]; then
    exit 0
fi

# Check if file exists
if [[ ! -f "$CANONICAL_PATH" ]]; then
    exit 0
fi

# Validate syntax only without executing untrusted tutorial code
"$PYTHON" -m py_compile "$CANONICAL_PATH"

echo "Tutorial syntax validated: $(basename "$CANONICAL_PATH")"
