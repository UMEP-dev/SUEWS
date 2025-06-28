#!/bin/bash
# Automated worktree setup for Claude Code
# Usage: ./.claude/scripts/worktree-setup.sh feature-name

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
NC='\033[0m' # No Colour

echo -e "${YELLOW}Setting up worktree for feature: $FEATURE${NC}"

# Create worktree
if [ -d "worktrees/$FEATURE" ]; then
    echo "Worktree already exists at worktrees/$FEATURE"
    echo "Remove it first with: git worktree remove worktrees/$FEATURE"
    exit 1
fi

echo "Creating git worktree..."
git worktree add worktrees/$FEATURE feature/$FEATURE || {
    echo "Failed to create worktree. Ensure branch feature/$FEATURE exists."
    exit 1
}

# Navigate to worktree
cd worktrees/$FEATURE

# Setup Python environment
echo "Creating Python virtual environment..."
python -m venv .venv

# Activate and install
echo "Installing package in development mode..."
source .venv/bin/activate
pip install --upgrade pip setuptools wheel
pip install -e . || {
    echo "Failed to install package. Check dependencies."
    exit 1
}

# Create environment marker
echo "$FEATURE" > .worktree-name
echo "feature/$FEATURE" > .git-branch

# Run initial build
echo "Running initial build..."
make dev || echo "Note: 'make dev' failed - you may need to run it manually"

# Create activation script
cat > activate.sh << 'EOF'
#!/bin/bash
# Quick activation script for this worktree
source .venv/bin/activate
echo "Activated environment for: $(cat .worktree-name)"
echo "Branch: $(git branch --show-current)"
EOF
chmod +x activate.sh

# Success message
echo -e "${GREEN}✓ Worktree setup complete!${NC}"
echo ""
echo "Location: $PWD"
echo "Branch: feature/$FEATURE"
echo ""
echo "To start working:"
echo "  cd worktrees/$FEATURE"
echo "  source .venv/bin/activate  # or ./activate.sh"
echo "  make test"
echo ""
echo "To clean up later:"
echo "  ./.claude/scripts/worktree-cleanup.sh $FEATURE"