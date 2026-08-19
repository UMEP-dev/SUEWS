#!/usr/bin/env bash
# Hook: Check SUEWS bib files for topic-tag convention violations
# Triggered by PostToolUse for Edit/Write on docs/source/assets/refs/*.bib
# Complements the audit-docs skill (.claude/skills/audit-docs/)

set -euo pipefail

CLAUDE_PROJECT_DIR="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"

# Get the edited file path from stdin (JSON format)
INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // .tool_input.filePath // empty')

# Exit early if no file path
if [[ -z "$FILE_PATH" ]]; then
    exit 0
fi

# Only check the SUEWS reference bib files
if [[ ! "$FILE_PATH" =~ docs/source/assets/refs/.*\.bib$ ]]; then
    exit 0
fi

# Check if file exists
if [[ ! -f "$FILE_PATH" ]]; then
    exit 0
fi

# Resolve a Python interpreter. Hooks run without an activated venv, so prefer
# the repo venv, then fall back to python3/python from PATH.
if [[ -x "$CLAUDE_PROJECT_DIR/.venv/bin/python" ]]; then
    PYTHON="$CLAUDE_PROJECT_DIR/.venv/bin/python"
else
    PYTHON="$(command -v python3 || command -v python || true)"
fi
if [[ -z "$PYTHON" ]]; then
    exit 0
fi

AUDIT="$CLAUDE_PROJECT_DIR/.claude/skills/audit-docs/scripts/audit.py"
if [[ ! -f "$AUDIT" ]]; then
    exit 0
fi

# audit.py uses only the standard library and exits non-zero on a convention
# violation (missing/invalid topic slug, bad format, missing required field).
if ! OUTPUT=$("$PYTHON" "$AUDIT" "$FILE_PATH" 2>&1); then
    echo "BIB TOPIC-TAG VIOLATION in $(basename "$FILE_PATH")"
    echo "Run /audit-docs (refs mode) to fix. Details:"
    echo "$OUTPUT" | head -20
    # Exit 1 to block the edit and signal the issue
    exit 1
fi
