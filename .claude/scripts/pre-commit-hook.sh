#!/bin/bash
# Git pre-commit hook to validate CLAUDE.md integrity
# 
# Installation:
#   cp .claude/scripts/pre-commit-hook.sh .git/hooks/pre-commit
#   chmod +x .git/hooks/pre-commit

# Get the repository root (works for both regular repos and worktrees)
REPO_ROOT=$(git rev-parse --show-toplevel)

# Check if CLAUDE.md is being committed
if git diff --cached --name-only | grep -q "CLAUDE.md"; then
    echo "Validating CLAUDE.md integrity..."
    
    # Change to repo root and run validation script
    cd "$REPO_ROOT"
    python3 .claude/scripts/validate-claude-md.py
    
    if [ $? -ne 0 ]; then
        echo ""
        echo "❌ CLAUDE.md validation failed!"
        echo ""
        echo "The file appears to have placeholder text or missing content."
        echo "Please review the warnings above and fix the issues."
        echo ""
        echo "To bypass this check (NOT recommended):"
        echo "  git commit --no-verify"
        echo ""
        echo "To restore from backup:"
        echo "  cp CLAUDE.md.backup CLAUDE.md"
        echo ""
        exit 1
    fi
    
    echo "✅ CLAUDE.md validation passed"
fi

# Continue with commit
exit 0