#!/bin/bash
# Automatic setup script for CLAUDE.md protection
# This script sets up all protections automatically

set -e

echo "🛡️ Setting up CLAUDE.md protection system..."

# Get the Git common directory (works for both regular repos and worktrees)
GIT_COMMON_DIR=$(git rev-parse --git-common-dir 2>/dev/null || echo ".git")

# 1. Create initial backup
if [ ! -f "CLAUDE.md.backup" ]; then
    cp CLAUDE.md CLAUDE.md.backup
    echo "✅ Created initial backup: CLAUDE.md.backup"
else
    echo "ℹ️  Backup already exists"
fi

# 2. Install Git pre-commit hook
HOOK_PATH="$GIT_COMMON_DIR/hooks/pre-commit"
HOOK_SOURCE=".claude/scripts/pre-commit-hook.sh"

if [ -f "$HOOK_SOURCE" ]; then
    mkdir -p "$(dirname "$HOOK_PATH")"
    
    # Check if hook already exists
    if [ -f "$HOOK_PATH" ]; then
        # Check if it's our hook
        if grep -q "CLAUDE.md validation" "$HOOK_PATH" 2>/dev/null; then
            echo "ℹ️  Pre-commit hook already installed"
        else
            # Backup existing hook and append our validation
            cp "$HOOK_PATH" "$HOOK_PATH.backup"
            echo "" >> "$HOOK_PATH"
            echo "# CLAUDE.md validation" >> "$HOOK_PATH"
            cat "$HOOK_SOURCE" >> "$HOOK_PATH"
            echo "✅ Appended CLAUDE.md validation to existing pre-commit hook"
        fi
    else
        cp "$HOOK_SOURCE" "$HOOK_PATH"
        chmod +x "$HOOK_PATH"
        echo "✅ Installed Git pre-commit hook"
    fi
else
    echo "⚠️  Hook source not found: $HOOK_SOURCE"
fi

# 3. Create snapshots directory
mkdir -p .claude/snapshots
echo "✅ Snapshots directory ready: .claude/snapshots/"

# 4. Run initial validation
echo ""
echo "🔍 Running initial validation..."
if command -v python3 &> /dev/null; then
    python3 .claude/scripts/validate-claude-md.py
else
    echo "⚠️  Python3 not found - skipping validation"
fi

# 5. Add to .gitignore if needed
if [ -f ".gitignore" ]; then
    if ! grep -q "CLAUDE.md.backup" .gitignore; then
        echo "" >> .gitignore
        echo "# CLAUDE.md protection" >> .gitignore
        echo "CLAUDE.md.backup" >> .gitignore
        echo ".claude/snapshots/" >> .gitignore
        echo "✅ Added protection files to .gitignore"
    fi
fi

echo ""
echo "🎉 CLAUDE.md protection system setup complete!"
echo ""
echo "Protection features now active:"
echo "  • Git pre-commit validation"
echo "  • Automatic backup (CLAUDE.md.backup)"
echo "  • Snapshot on validation failures"
echo "  • GitHub Actions validation (on push/PR)"
echo ""
echo "To manually validate: python3 .claude/scripts/validate-claude-md.py"