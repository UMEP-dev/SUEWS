#!/usr/bin/env bash
# Hook: Pre-commit quality gate for Claude Code
# Triggered by PreToolUse on Bash when git commit is invoked.
# Runs file-type-specific checks on staged files and blocks commit if any fail.

set -euo pipefail

# Read tool input from stdin (JSON with tool_input.command)
INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

# Only intercept commands that contain git commit (handles chained commands
# like "git add . && git commit -m msg" which Claude Code commonly produces)
if [[ -z "$COMMAND" ]] || ! echo "$COMMAND" | grep -qE '\bgit\s+commit\b'; then
    exit 0
fi

# Skip if this is just a dry-run or status check
if echo "$COMMAND" | grep -qE '\-\-dry-run|--short|--porcelain'; then
    exit 0
fi

cd "${CLAUDE_PROJECT_DIR:-.}"

# Activate virtual environment if available
if [[ -f ".venv/bin/activate" ]]; then
    source .venv/bin/activate
fi

# Get staged files
STAGED=$(git diff --cached --name-only 2>/dev/null || true)
if [[ -z "$STAGED" ]]; then
    exit 0
fi

# Classify staged files
PY_FILES=$(echo "$STAGED" | grep -E '\.py$' || true)
F90_FILES=$(echo "$STAGED" | grep -E '\.(f95|f90)$' || true)
RST_FILES=$(echo "$STAGED" | grep -E '\.rst$' || true)
CHANGELOG=$(echo "$STAGED" | grep -E '^CHANGELOG\.md$' || true)
MESON=$(echo "$STAGED" | grep -E 'meson\.build$' || true)

ERRORS=""
PASS_COUNT=0
FAIL_COUNT=0

report_pass() {
    PASS_COUNT=$((PASS_COUNT + 1))
    echo "[OK] $1"
}

report_fail() {
    FAIL_COUNT=$((FAIL_COUNT + 1))
    ERRORS="${ERRORS}\n[FAIL] $1: $2"
    echo "[FAIL] $1: $2"
}

echo "=== PRE-COMMIT QUALITY GATE ==="
echo ""

# --- Python checks ---
if [[ -n "$PY_FILES" ]]; then
    echo "-- Python files detected --"

    # ruff check (xargs handles newline-separated filenames safely)
    if command -v ruff >/dev/null 2>&1; then
        RUFF_OUT=$(echo "$PY_FILES" | xargs ruff check 2>&1) && report_pass "ruff check" || report_fail "ruff check" "$RUFF_OUT"
    fi

    # ruff format check (non-destructive)
    if command -v ruff >/dev/null 2>&1; then
        FMT_OUT=$(echo "$PY_FILES" | xargs ruff format --check 2>&1) && report_pass "ruff format" || report_fail "ruff format" "Files need formatting. Run: make format"
    fi
fi

# --- Fortran checks ---
if [[ -n "$F90_FILES" ]]; then
    echo "-- Fortran files detected --"

    # fprettify diff check
    if command -v fprettify >/dev/null 2>&1 && [[ -f ".fprettify.rc" ]]; then
        while IFS= read -r f; do
            [[ -z "$f" ]] && continue
            if [[ -f "$f" ]]; then
                FPRET_OUT=$(fprettify --diff --config .fprettify.rc "$f" 2>&1)
                if [[ -n "$FPRET_OUT" ]]; then
                    report_fail "fprettify ($f)" "Needs formatting. Run: make format"
                else
                    report_pass "fprettify ($f)"
                fi
            fi
        done <<< "$F90_FILES"
    fi

    # Check new Fortran files are in meson.build
    while IFS= read -r f; do
        [[ -z "$f" ]] && continue
        BASENAME=$(basename "$f")
        if [[ "$f" == src/suews/src/* ]] && ! git show HEAD:"$f" >/dev/null 2>&1; then
            # New file -- check meson.build
            if ! grep -q "$BASENAME" src/suews/src/meson.build 2>/dev/null; then
                report_fail "meson.build" "New file $BASENAME not listed in src/suews/src/meson.build"
            else
                report_pass "meson.build ($BASENAME)"
            fi
        fi
    done <<< "$F90_FILES"
fi

# --- RST checks ---
if [[ -n "$RST_FILES" ]]; then
    echo "-- RST documentation detected --"

    for f in $RST_FILES; do
        if [[ -f "$f" ]]; then
            # Check heading nesting depth (max 3 levels: =, -, ~)
            HEADING_CHARS=$(grep -cE '^[=\-~^"]{3,}$' "$f" 2>/dev/null || echo "0")
            DEEP_HEADINGS=$(grep -cE '^[\^"]{3,}$' "$f" 2>/dev/null || echo "0")
            if [[ "$DEEP_HEADINGS" -gt 0 ]]; then
                report_fail "RST headings ($f)" "More than 3 heading levels detected (max: =, -, ~)"
            else
                report_pass "RST headings ($f)"
            fi
        fi
    done
fi

# --- CHANGELOG checks ---
if [[ -n "$CHANGELOG" ]]; then
    echo "-- CHANGELOG.md detected --"

    # Check date format: DD Mon YYYY (e.g., 28 Jan 2026)
    BAD_DATES=$(grep -nE '^## ' CHANGELOG.md | grep -vE '## [0-9]{1,2} [A-Z][a-z]{2} [0-9]{4}' | head -3 || true)
    if [[ -n "$BAD_DATES" ]]; then
        report_fail "CHANGELOG dates" "Non-standard date format found. Expected: DD Mon YYYY"
    else
        report_pass "CHANGELOG dates"
    fi

    # Check category tags
    BAD_CATS=$(grep -nE '^\- \[' CHANGELOG.md | grep -vE '\[(feature|bugfix|change|maintenance|doc)\]' | head -3 || true)
    if [[ -n "$BAD_CATS" ]]; then
        report_fail "CHANGELOG categories" "Unknown category tag. Use: [feature], [bugfix], [change], [maintenance], [doc]"
    else
        report_pass "CHANGELOG categories"
    fi
fi

# --- meson.build checks ---
if [[ -n "$MESON" ]]; then
    echo "-- meson.build detected --"

    # Verify all .f95 files in src/suews/src/ are listed
    if [[ -f "src/suews/src/meson.build" ]]; then
        MISSING=""
        for f in src/suews/src/*.f95; do
            BASENAME=$(basename "$f")
            if ! grep -q "$BASENAME" src/suews/src/meson.build 2>/dev/null; then
                MISSING="${MISSING} ${BASENAME}"
            fi
        done
        if [[ -n "$MISSING" ]]; then
            report_fail "meson.build sources" "Missing Fortran files:${MISSING}"
        else
            report_pass "meson.build sources"
        fi
    fi
fi

# --- Smoke tests (only for Python/Fortran changes) ---
if [[ -n "$PY_FILES" ]] || [[ -n "$F90_FILES" ]]; then
    echo ""
    echo "-- Running smoke tests --"
    SMOKE_LOG=$(mktemp)
    if make test-smoke >"$SMOKE_LOG" 2>&1; then
        report_pass "make test-smoke"
    else
        report_fail "make test-smoke" "Smoke tests failed (last 20 lines below). Run: make test-smoke"
        echo "--- smoke test output ---"
        tail -20 "$SMOKE_LOG"
        echo "---"
    fi
    rm -f "$SMOKE_LOG"
fi

# --- Summary ---
echo ""
echo "=== GATE SUMMARY ==="
echo "Passed: $PASS_COUNT  Failed: $FAIL_COUNT"

if [[ $FAIL_COUNT -gt 0 ]]; then
    echo ""
    echo "Failures:"
    echo -e "$ERRORS"
    echo ""
    echo "Commit blocked. Fix the issues above and try again."
    exit 1
fi

echo "All checks passed."
exit 0
