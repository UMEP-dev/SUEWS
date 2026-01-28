#!/usr/bin/env bash
# Hook: Validate tutorial files after editing
# Triggered by PostToolUse for Edit/Write on tutorial files

set -euo pipefail

# Get the edited file path from stdin (JSON format)
INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // .tool_input.filePath // empty')

# Exit if no file path or not a tutorial file
if [[ -z "$FILE_PATH" ]] || [[ ! "$FILE_PATH" =~ docs/source/tutorials/.*\.py$ ]]; then
    exit 0
fi

# Check if file exists
if [[ ! -f "$FILE_PATH" ]]; then
    exit 0
fi

# Activate virtual environment if available
if [[ -f "$CLAUDE_PROJECT_DIR/.venv/bin/activate" ]]; then
    source "$CLAUDE_PROJECT_DIR/.venv/bin/activate"
fi

# Run the tutorial with matplotlib non-interactive backend
# Capture output but limit to key indicators
cd "$CLAUDE_PROJECT_DIR"
OUTPUT=$(MPLBACKEND=Agg timeout 120 python "$FILE_PATH" 2>&1) || EXIT_CODE=$?

if [[ ${EXIT_CODE:-0} -ne 0 ]]; then
    # Extract the last error for context
    ERROR_MSG=$(echo "$OUTPUT" | grep -E "(Error|Exception|Traceback)" | tail -5)
    echo "TUTORIAL VALIDATION FAILED: $FILE_PATH"
    echo "---"
    echo "$ERROR_MSG"
    exit 1
fi

echo "Tutorial validated: $(basename "$FILE_PATH")"
