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
        if grep -q "Shared agent instruction validation" "$HOOK_PATH" 2>/dev/null; then
            echo "[INFO] Pre-commit hook already installed"
        elif head -n 2 "$HOOK_PATH" | grep -q "validate CLAUDE.md integrity"; then
            # Standalone legacy CLAUDE.md-only hook: replace it with the current one
            cp "$HOOK_PATH" "$HOOK_PATH.backup"
            cp "$HOOK_SOURCE" "$HOOK_PATH"
            chmod +x "$HOOK_PATH"
            echo "[OK] Replaced legacy CLAUDE.md-only pre-commit hook (previous copy: $HOOK_PATH.backup)"
        elif grep -q "validate-claude-md.py" "$HOOK_PATH" 2>/dev/null; then
            echo "[WARN] $HOOK_PATH contains the legacy CLAUDE.md-only validation inside another hook."
            echo "       Replace that block with the contents of $HOOK_SOURCE by hand."
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
if [ -f ".gitignore" ]; then
    for entry in "AGENTS.md.backup" ".claude/snapshots/"; do
        if ! grep -qxF "$entry" .gitignore; then
            echo "$entry" >> .gitignore
            echo "[OK] Added $entry to .gitignore"
        fi
    done
fi

echo ""
echo "[OK] AGENTS.md protection system setup complete."
echo "To manually validate: python3 .claude/scripts/validate-claude-md.py"
