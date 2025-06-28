#!/bin/bash
# Automated worktree cleanup for Claude Code
# Usage: ./.claude/scripts/worktree-cleanup.sh feature-name

set -e  # Exit on error

FEATURE=$1
if [ -z "$FEATURE" ]; then
    echo "Usage: $0 feature-name"
    echo "Example: $0 yaml-validation"
    exit 1
fi

# Colours for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Colour

echo -e "${YELLOW}Cleaning up worktree: $FEATURE${NC}"

# Check if worktree exists
if [ ! -d "worktrees/$FEATURE" ]; then
    echo -e "${RED}Worktree not found at worktrees/$FEATURE${NC}"
    exit 1
fi

# Deactivate any active environment
if [ -n "$VIRTUAL_ENV" ]; then
    echo "Deactivating current virtual environment..."
    deactivate 2>/dev/null || true
fi

# Remove worktree
echo "Removing git worktree..."
git worktree remove worktrees/$FEATURE --force || {
    echo -e "${RED}Failed to remove worktree. It may have uncommitted changes.${NC}"
    echo "Try: git worktree remove worktrees/$FEATURE --force"
    exit 1
}

# Check for mamba environment (legacy cleanup)
if command -v mamba &> /dev/null; then
    if mamba env list | grep -q "suews-dev-$FEATURE"; then
        echo "Found legacy mamba environment, removing..."
        mamba env remove -n suews-dev-$FEATURE -y 2>/dev/null || true
    fi
fi

# Remove worktree plan if it exists
PLAN_FILE=".claude/worktree-plans/feature-$FEATURE.md"
if [ -f "$PLAN_FILE" ]; then
    echo "Removing worktree plan..."
    git rm "$PLAN_FILE" 2>/dev/null || rm -f "$PLAN_FILE"
    
    # Only commit if the file was tracked
    if git status --porcelain | grep -q "^D.*$PLAN_FILE"; then
        git commit -m "chore: remove worktree plan for $FEATURE" || {
            echo "Note: Could not commit plan removal. You may need to commit manually."
        }
    fi
fi

# List remaining worktrees
echo ""
echo "Remaining worktrees:"
git worktree list | grep -v "bare" || echo "  None"

echo ""
echo -e "${GREEN}✓ Cleanup complete for: $FEATURE${NC}"
echo ""
echo "If the branch is merged, you can delete it with:"
echo "  git branch -d feature/$FEATURE"
echo "  git push origin --delete feature/$FEATURE"