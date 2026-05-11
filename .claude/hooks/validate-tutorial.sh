#!/usr/bin/env bash
# Hook: Validate tutorial files after editing
# Triggered by PostToolUse for Edit/Write on tutorial files

set -euo pipefail

# Fallback to git root if CLAUDE_PROJECT_DIR is not set
CLAUDE_PROJECT_DIR="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"

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

CANONICAL_PATH=$(python -c 'import os,sys; print(os.path.realpath(sys.argv[1]))' "$CANDIDATE_PATH" 2>/dev/null || true)
PROJECT_ROOT=$(python -c 'import os,sys; print(os.path.realpath(sys.argv[1]))' "$CLAUDE_PROJECT_DIR")

# Only validate tutorial Python files inside this repository
if [[ -z "$CANONICAL_PATH" ]] || [[ ! "$CANONICAL_PATH" =~ ^$PROJECT_ROOT/docs/source/tutorials/[^/]+\.py$ ]]; then
    exit 0
fi

# Check if file exists
if [[ ! -f "$CANONICAL_PATH" ]]; then
    exit 0
fi

# Activate virtual environment if available
if [[ -f "$CLAUDE_PROJECT_DIR/.venv/bin/activate" ]]; then
    source "$CLAUDE_PROJECT_DIR/.venv/bin/activate"
fi

# Validate syntax only without executing untrusted tutorial code
python -m py_compile "$CANONICAL_PATH"

echo "Tutorial syntax validated: $(basename "$CANONICAL_PATH")"
