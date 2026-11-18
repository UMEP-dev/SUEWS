#!/usr/bin/env bash
# Git pre-commit hook to validate shared agent instructions (AGENTS.md + CLAUDE.md + .claude/rules/)
#
# Installation:
#   cp .claude/scripts/pre-commit-hook.sh .git/hooks/pre-commit
#   chmod +x .git/hooks/pre-commit

REPO_ROOT=$(git rev-parse --show-toplevel)

if git diff --cached --name-only | grep -qE '^(AGENTS\.md|CLAUDE\.md|MAINTAINERS\.md|dev-ref/.*\.md|\.claude/)'; then
    echo "Validating AGENTS.md and shared agent governance..."

    cd "$REPO_ROOT" || exit 1
    if [ -x "$REPO_ROOT/.venv/bin/python" ]; then
        PYTHON_BIN="$REPO_ROOT/.venv/bin/python"
    else
        PYTHON_BIN="$(command -v python3 || command -v python || true)"
    fi
    if [ -z "$PYTHON_BIN" ]; then
        echo "[WARN] No Python interpreter found; skipping shared agent instruction validation."
        exit 0
    fi
    "$PYTHON_BIN" .claude/scripts/validate-claude-md.py

    if [ $? -ne 0 ]; then
        echo ""
        echo "[FAIL] Shared agent instruction validation failed."
        echo "Review the warnings above and fix the issues before committing."
        echo ""
        echo "To restore from backup (if available):"
        echo "  cp AGENTS.md.backup AGENTS.md"
        echo ""
        exit 1
    fi

    echo "[OK] Shared agent instruction validation passed."
fi

exit 0
