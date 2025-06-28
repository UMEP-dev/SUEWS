#!/bin/bash
# Simplified SUEWS worktree setup with uv
# Assumes gfortran is handled by macOS
# Usage: ./.claude/scripts/simple-uv-setup.sh feature-name

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

echo -e "${YELLOW}Setting up SUEWS worktree: $FEATURE${NC}"

# Check if uv is installed
if ! command -v uv &> /dev/null; then
    echo "Error: uv is not installed"
    echo "Install with: brew install uv"
    exit 1
fi

# Create worktree
if [ -d "worktrees/$FEATURE" ]; then
    echo "Worktree already exists at worktrees/$FEATURE"
    exit 1
fi

git worktree add worktrees/$FEATURE feature/$FEATURE || exit 1
cd worktrees/$FEATURE

# Create venv with uv (ultra-fast!)
echo "Creating virtual environment..."
uv venv
source .venv/bin/activate

# Install core scientific packages that SUEWS needs
echo "Installing scientific stack..."
uv pip install --upgrade pip setuptools wheel

# Install from requirements file (much faster with uv!)
echo "Installing from requirements..."
uv pip install -r .claude/scripts/requirements-core.txt

# Install SUEWS in development mode
echo "Installing SUEWS..."
uv pip install -e .

# Create markers
echo "$FEATURE" > .worktree-name

# Create simple activation script
cat > activate.sh << 'EOF'
#!/bin/bash
source .venv/bin/activate
echo "Activated: $(cat .worktree-name)"
echo "Branch: $(git branch --show-current)"
EOF
chmod +x activate.sh

# Quick test
python -c "import supy; print(f'✓ SuPy {supy.__version__} ready')" || true

echo -e "${GREEN}✓ Setup complete in seconds!${NC}"
echo ""
echo "To work:"
echo "  cd worktrees/$FEATURE"
echo "  ./activate.sh"
echo "  make test"