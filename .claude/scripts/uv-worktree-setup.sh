#!/bin/bash
# Ultra-fast worktree setup using uv
# Usage: ./.claude/scripts/uv-worktree-setup.sh feature-name

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

echo -e "${YELLOW}Setting up worktree for feature: $FEATURE${NC}"

# Check if uv is installed
if ! command -v uv &> /dev/null; then
    echo -e "${RED}Error: uv is not installed${NC}"
    echo "Install with: curl -LsSf https://astral.sh/uv/install.sh | sh"
    echo "Or with Homebrew: brew install uv"
    exit 1
fi

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

# Setup with uv (FAST!)
echo "Creating virtual environment with uv..."
uv venv

# Activate and install
echo "Installing package in development mode..."
source .venv/bin/activate

# Use uv for installation
uv pip install --upgrade pip setuptools wheel
uv pip install -e . || {
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
echo "Using uv for package management"
EOF
chmod +x activate.sh

# Success message with timing
END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))

echo -e "${GREEN}✓ Worktree setup complete in seconds!${NC}"
echo ""
echo "Location: $PWD"
echo "Branch: feature/$FEATURE"
echo "Package manager: uv (ultra-fast)"
echo ""
echo "To start working:"
echo "  cd worktrees/$FEATURE"
echo "  source .venv/bin/activate  # or ./activate.sh"
echo "  make test"
echo ""
echo "To install additional packages:"
echo "  uv pip install package-name"
echo ""
echo "To clean up later:"
echo "  ./.claude/scripts/worktree-cleanup.sh $FEATURE"