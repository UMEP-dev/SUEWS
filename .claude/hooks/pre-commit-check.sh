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

# During a merge, check only files whose staged content also differs from the
# merged-in branch. Files that arrive unchanged from that side (e.g. master
# merged into a PR branch) are not the contributor's changes, so a clean merge
# checks nothing while resolved conflicts are still checked.
if [[ -n "$STAGED" ]] && git rev-parse -q --verify MERGE_HEAD >/dev/null 2>&1; then
    FROM_MERGE=$(git diff --cached --name-only MERGE_HEAD 2>/dev/null || true)
    if [[ -n "$FROM_MERGE" ]]; then
        STAGED=$(comm -12 <(sort <<< "$STAGED") <(sort <<< "$FROM_MERGE"))
    else
        STAGED=""
    fi
fi

if [[ -z "$STAGED" ]]; then
    exit 0
fi

# Classify staged files
PY_FILES=$(echo "$STAGED" | grep -E '\.py$' || true)
PY_FMT_FILES=$(echo "$STAGED" | grep -E '^(src|test)/.*\.py$' || true)
F90_FILES=$(echo "$STAGED" | grep -E '\.(f95|f90|F90)$' || true)
F90_FMT_FILES=$(echo "$STAGED" | grep -E '^src/suews/src/.*\.f95$' || true)
RST_FILES=$(echo "$STAGED" | grep -E '\.rst$' || true)
CHANGELOG=$(echo "$STAGED" | grep -E '^CHANGELOG\.md$' || true)
AGENT_FILES=$(echo "$STAGED" | grep -E '^(AGENTS\.md|CLAUDE\.md|dev-ref/CODEBASE_AUDIT_PROTOCOL\.md|\.claude/(README\.md|rules|agents|hooks|scripts|settings\.json))' || true)

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

    if command -v ruff >/dev/null 2>&1; then
        RUFF_OUT=$(echo "$PY_FILES" | xargs ruff check 2>&1) && report_pass "ruff check" || report_fail "ruff check" "$RUFF_OUT"
    fi

    # ruff format check matches Makefile `make format` scope (src/ and test/)
    if [[ -n "$PY_FMT_FILES" ]] && command -v ruff >/dev/null 2>&1; then
        FMT_OUT=$(echo "$PY_FMT_FILES" | xargs ruff format --check 2>&1) && report_pass "ruff format" || report_fail "ruff format" "Files need formatting. Run: make format"
    fi
fi

# --- Fortran checks ---
if [[ -n "$F90_FILES" ]]; then
    echo "-- Fortran files detected --"

    # fprettify diff check matches Makefile `make format` scope (src/suews/src/*.f95)
    if [[ -n "$F90_FMT_FILES" ]] && command -v fprettify >/dev/null 2>&1 && [[ -f ".fprettify.rc" ]]; then
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
        done <<< "$F90_FMT_FILES"
    fi

    # Check newly added Fortran files are registered in the active build files
    while IFS= read -r f; do
        [[ -z "$f" ]] && continue
        BASENAME=$(basename "$f")
        STEM="${BASENAME%.*}"
        if [[ "$f" == src/suews/src/*.f95 ]] && [[ -f "$f" ]] && ! git show HEAD:"$f" >/dev/null 2>&1; then
            if ! grep -q "$BASENAME" src/supy/meson.build 2>/dev/null; then
                report_fail "src/supy/meson.build" "New file $BASENAME not listed in src/supy/meson.build"
            else
                report_pass "src/supy/meson.build ($BASENAME)"
            fi
            if ! grep -q "$STEM" src/suews/Makefile 2>/dev/null; then
                report_fail "src/suews/Makefile" "New module $STEM not listed in src/suews/Makefile"
            else
                report_pass "src/suews/Makefile ($BASENAME)"
            fi
        elif [[ "$f" == src/suews_bridge/c_api/*.f95 ]] && [[ -f "$f" ]] && ! git show HEAD:"$f" >/dev/null 2>&1; then
            if ! grep -q "$BASENAME" src/suews_bridge/build.rs 2>/dev/null; then
                report_fail "src/suews_bridge/build.rs" "New C-API file $BASENAME not listed in src/suews_bridge/build.rs"
            else
                report_pass "src/suews_bridge/build.rs ($BASENAME)"
            fi
        fi
    done <<< "$F90_FILES"
fi

# --- RST checks ---
if [[ -n "$RST_FILES" ]]; then
    echo "-- RST documentation detected --"

    while IFS= read -r f; do
        [[ -z "$f" ]] && continue
        if [[ -f "$f" ]]; then
            # Check heading nesting depth (max 3 levels: =, -, ~)
            DEEP_HEADINGS=$(grep -cE '^[\^"]{3,}$' "$f" 2>/dev/null || echo "0")
            if [[ "$DEEP_HEADINGS" -gt 0 ]]; then
                report_fail "RST headings ($f)" "More than 3 heading levels detected (max: =, -, ~)"
            else
                report_pass "RST headings ($f)"
            fi
        fi
    done <<< "$RST_FILES"
fi

# --- CHANGELOG checks (inspect staged additions so legacy historical lines do not block commits) ---
if [[ -n "$CHANGELOG" ]] && [[ -f "CHANGELOG.md" ]]; then
    echo "-- CHANGELOG.md detected --"
    CHANGELOG_ADDED=$(git diff --cached -U0 -- CHANGELOG.md | grep -E '^\+[^+]' | sed 's/^\+//' || true)

    # Release/date headings use level 3: ### DD Mon YYYY (level 2 is ## YYYY / ## Table of Contents)
    BAD_DATES=$(printf '%s\n' "$CHANGELOG_ADDED" | grep -E '^### ' | grep -vE '^### [0-9]{1,2} [A-Z][a-z]{2} [0-9]{4}$' | head -3 || true)
    if [[ -n "$BAD_DATES" ]]; then
        report_fail "CHANGELOG dates" "Non-standard date heading found (expected: ### DD Mon YYYY): $BAD_DATES"
    else
        report_pass "CHANGELOG dates"
    fi

    # Check category tags on top-level bullet entries while ignoring TOC links like `- [2026](#2026)` and indented sub-bullets
    BAD_CATS=$(printf '%s\n' "$CHANGELOG_ADDED" | grep -E '^- \[[^]]+\]([^(\n]|$)' | grep -vE '^-\s+\[(feature|bugfix|change|maintenance|doc)\]' | head -3 || true)
    if [[ -n "$BAD_CATS" ]]; then
        report_fail "CHANGELOG categories" "Unknown category tag (use [feature], [bugfix], [change], [maintenance], [doc]): $BAD_CATS"
    else
        report_pass "CHANGELOG categories"
    fi
fi

# --- Shared agent instruction checks ---
if [[ -n "$AGENT_FILES" ]]; then
    echo "-- Agent governance files detected --"
    PYTHON_BIN="$(command -v python3 || command -v python || true)"
    if [[ -n "$PYTHON_BIN" ]] && [[ -f ".claude/scripts/validate-claude-md.py" ]]; then
        VAL_OUT=$("$PYTHON_BIN" .claude/scripts/validate-claude-md.py 2>&1) && report_pass "validate-claude-md.py" || report_fail "validate-claude-md.py" "$VAL_OUT"
    fi
fi

# --- Smoke tests (only for Python/Fortran changes) ---
if [[ -n "$PY_FMT_FILES" ]] || [[ -n "$F90_FILES" ]]; then
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
