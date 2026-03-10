#!/usr/bin/env bash
# Hook: Check documentation source files for non-ASCII characters
# Triggered by PostToolUse for Edit/Write on docs/ files
# Complements the doc-styler skill (.claude/skills/doc-styler/)

set -euo pipefail

# Get the edited file path from stdin (JSON format)
INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // .tool_input.filePath // empty')

# Exit early if no file path
if [[ -z "$FILE_PATH" ]]; then
    exit 0
fi

# Only check files under docs/source/ with relevant extensions
if [[ ! "$FILE_PATH" =~ docs/source/.*\.(py|rst|md)$ ]]; then
    exit 0
fi

# Check if file exists
if [[ ! -f "$FILE_PATH" ]]; then
    exit 0
fi

# Scan for non-ASCII characters (perl for portability -- macOS grep lacks -P)
MATCHES=$(perl -ne 'print "$.:$_" if /[^\x00-\x7F]/' "$FILE_PATH" 2>/dev/null || true)

if [[ -n "$MATCHES" ]]; then
    COUNT=$(echo "$MATCHES" | wc -l | tr -d ' ')
    echo "NON-ASCII CHARACTERS FOUND in $(basename "$FILE_PATH") ($COUNT lines)"
    echo "Run /doc-styler to fix. Affected lines:"
    echo "$MATCHES" | head -10
    if [[ "$COUNT" -gt 10 ]]; then
        echo "... and $((COUNT - 10)) more"
    fi
    # Exit 1 to block the edit and signal the issue
    exit 1
fi
