#!/usr/bin/env bash
# Git pre-commit hook to validate shared agent instructions (AGENTS.md + CLAUDE.md + .claude/rules/)
#
# Installation:
#   cp .claude/scripts/pre-commit-hook.sh .git/hooks/pre-commit
#   chmod +x .git/hooks/pre-commit

REPO_ROOT=$(git rev-parse --show-toplevel)

if git diff --cached --name-only | grep -qE '^(AGENTS\.md|CLAUDE\.md|dev-ref/CODEBASE_AUDIT_PROTOCOL\.md|\.claude/(README\.md|rules|agents|hooks|scripts|settings\.json))'; then
    echo "Validating AGENTS.md and shared agent governance..."

    cd "$REPO_ROOT" || exit 1
    PYTHON_BIN="$(command -v python3 || command -v python)"
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
