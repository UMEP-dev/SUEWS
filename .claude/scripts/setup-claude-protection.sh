#!/usr/bin/env bash
# Setup script for AGENTS.md and shared agent-instruction protection

set -euo pipefail

echo "Setting up AGENTS.md protection system..."

GIT_COMMON_DIR=$(git rev-parse --git-common-dir 2>/dev/null || echo ".git")

# 1. Create initial backup
if [ ! -f "AGENTS.md.backup" ]; then
    cp AGENTS.md AGENTS.md.backup
    echo "[OK] Created initial backup: AGENTS.md.backup"
else
    echo "[INFO] Backup already exists: AGENTS.md.backup"
fi

# 2. Install Git pre-commit hook
HOOK_PATH="$GIT_COMMON_DIR/hooks/pre-commit"
HOOK_SOURCE=".claude/scripts/pre-commit-hook.sh"

if [ -f "$HOOK_SOURCE" ]; then
    mkdir -p "$(dirname "$HOOK_PATH")"

    if [ -f "$HOOK_PATH" ]; then
        if grep -q "validate-claude-md.py" "$HOOK_PATH" 2>/dev/null; then
            echo "[INFO] Pre-commit hook already installed"
        else
            cp "$HOOK_PATH" "$HOOK_PATH.backup"
            {
                echo ""
                echo "# Shared agent instruction validation"
                cat "$HOOK_SOURCE"
            } >> "$HOOK_PATH"
            echo "[OK] Appended agent-instruction validation to existing pre-commit hook"
        fi
    else
        cp "$HOOK_SOURCE" "$HOOK_PATH"
        chmod +x "$HOOK_PATH"
        echo "[OK] Installed Git pre-commit hook"
    fi
else
    echo "[WARN] Hook source not found: $HOOK_SOURCE"
fi

# 3. Create snapshots directory
mkdir -p .claude/snapshots
echo "[OK] Snapshots directory ready: .claude/snapshots/"

# 4. Run initial validation
echo ""
echo "Running initial validation..."
if command -v python3 >/dev/null 2>&1; then
    python3 .claude/scripts/validate-claude-md.py
else
    echo "[WARN] python3 not found - skipping validation"
fi

# 5. Add to .gitignore if needed
if [ -f ".gitignore" ] && ! grep -q "AGENTS.md.backup" .gitignore; then
    {
        echo ""
        echo "# AGENTS.md protection"
        echo "AGENTS.md.backup"
        echo ".claude/snapshots/"
    } >> .gitignore
    echo "[OK] Added protection files to .gitignore"
fi

echo ""
echo "[OK] AGENTS.md protection system setup complete."
echo "To manually validate: python3 .claude/scripts/validate-claude-md.py"
