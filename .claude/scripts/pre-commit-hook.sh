#!/bin/bash
# Git pre-commit hook to validate AGENTS.md (and its CLAUDE.md loader) integrity
# 
# Installation:
#   cp .claude/scripts/pre-commit-hook.sh .git/hooks/pre-commit
#   chmod +x .git/hooks/pre-commit

# Get the repository root (works for both regular repos and worktrees)
REPO_ROOT=$(git rev-parse --show-toplevel)

# Check if AGENTS.md or CLAUDE.md is being committed
if git diff --cached --name-only | grep -qE '^(AGENTS|CLAUDE)\.md$'; then
    echo "Validating AGENTS.md / CLAUDE.md integrity..."
    
    # Change to repo root and run validation script
    cd "$REPO_ROOT"
    python3 .claude/scripts/validate-claude-md.py
    
    if [ $? -ne 0 ]; then
        echo ""
        echo "[X] AGENTS.md / CLAUDE.md validation failed!"
        echo ""
        echo "The file appears to have placeholder text or missing content."
        echo "Please review the warnings above and fix the issues."
        echo ""
        echo "To bypass this check (NOT recommended):"
        echo "  git commit --no-verify"
        echo ""
        echo "To restore from backup:"
        echo "  cp AGENTS.md.backup AGENTS.md"
        echo ""
        exit 1
    fi
    
    echo "[OK] AGENTS.md / CLAUDE.md validation passed"
fi

# Continue with commit
exit 0