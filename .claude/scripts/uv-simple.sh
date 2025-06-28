#!/bin/bash
# Dead simple uv worktree setup
# Usage: ./.claude/scripts/uv-simple.sh feature-name

FEATURE=$1
if [ -z "$FEATURE" ]; then
    echo "Usage: $0 feature-name"
    exit 1
fi

# Create worktree
git worktree add worktrees/$FEATURE feature/$FEATURE || exit 1
cd worktrees/$FEATURE

# Setup with uv and use make dev (now uv-compatible!)
uv venv
source .venv/bin/activate  # Activate for make commands
uv pip install -r ../../.claude/scripts/requirements-core.txt
make dev  # This will use uv automatically!

echo "✓ Ready! Commands:"
echo "  cd worktrees/$FEATURE"
echo "  uv run python         # Run Python"
echo "  uv run pytest        # Run specific tests"
echo "  make test            # Run full test suite"
echo "  make dev             # Rebuild (uses uv)"