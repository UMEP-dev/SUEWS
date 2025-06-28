#!/bin/bash
# SUEWS-specific worktree setup with uv
# Handles compiled dependencies properly
# Usage: ./.claude/scripts/suews-uv-setup.sh feature-name

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
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Colour

echo -e "${YELLOW}Setting up SUEWS worktree for feature: $FEATURE${NC}"

# Check if uv is installed
if ! command -v uv &> /dev/null; then
    echo -e "${RED}Error: uv is not installed${NC}"
    echo "Install with: curl -LsSf https://astral.sh/uv/install.sh | sh"
    echo "Or with Homebrew: brew install uv"
    exit 1
fi

# Check for required system dependencies
echo -e "${BLUE}Checking system dependencies...${NC}"
MISSING_DEPS=""
which gfortran >/dev/null 2>&1 || MISSING_DEPS="$MISSING_DEPS gfortran"

if [ -n "$MISSING_DEPS" ]; then
    echo -e "${YELLOW}Warning: Missing system dependencies:$MISSING_DEPS${NC}"
    
    # Check if mamba is available
    if command -v mamba &> /dev/null && mamba env list | grep -q suews-dev; then
        echo -e "${GREEN}suews-dev mamba environment is available${NC}"
        echo "Recommended: Run 'mamba activate suews-dev' before this script"
    else
        echo "Options to provide Fortran compiler:"
        echo "1. Install system-wide: brew install gcc"
        echo "2. Use mamba: mamba activate suews-dev"
    fi
    
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
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

# Detect environment and create venv
if [ -n "$CONDA_DEFAULT_ENV" ]; then
    echo -e "${GREEN}Detected mamba environment: $CONDA_DEFAULT_ENV${NC}"
    echo "Using its Python and compilers..."
    
    # Create venv with mamba's Python
    uv venv --python $(which python)
    
    # Store compiler paths from mamba
    MAMBA_FC=$(which gfortran 2>/dev/null || echo "")
    MAMBA_CC=$(which gcc 2>/dev/null || echo "")
else
    echo "No mamba environment detected"
    echo "Using system Python and compilers..."
    
    # Create venv with system Python
    uv venv
    MAMBA_FC=""
    MAMBA_CC=""
fi

# Activate venv
source .venv/bin/activate

# Set compiler environment variables
export FC=${FC:-${MAMBA_FC:-gfortran}}
export CC=${CC:-${MAMBA_CC:-gcc}}
export F90=${F90:-$FC}
export F77=${F77:-$FC}

echo -e "${BLUE}Compiler configuration:${NC}"
echo "  FC=$FC ($(which $FC 2>/dev/null || echo 'not found'))"
echo "  CC=$CC ($(which $CC 2>/dev/null || echo 'not found'))"

# Install with uv
echo "Installing SUEWS dependencies..."
uv pip install --upgrade pip setuptools wheel

# Install numpy first (required for f2py)
uv pip install numpy

# Install SUEWS in development mode
echo "Installing SUEWS in development mode..."
uv pip install -e . || {
    echo -e "${RED}Build failed. Ensure Fortran compiler is available.${NC}"
    echo "Try: mamba activate suews-dev && $0 $FEATURE"
    exit 1
}

# Create environment markers
echo "$FEATURE" > .worktree-name
echo "feature/$FEATURE" > .git-branch

# Create environment info file
cat > .env-info << EOF
SUEWS Worktree Environment Info
===============================
Feature: $FEATURE
Created: $(date)
Python: $(which python)
Python Version: $(python --version)
Fortran: $FC ($(which $FC 2>/dev/null || echo "not found"))
C Compiler: $CC ($(which $CC 2>/dev/null || echo "not found"))
Mamba Environment: ${CONDA_DEFAULT_ENV:-None}
Package Manager: uv
EOF

# Create activation script with compiler setup
cat > activate.sh << 'EOF'
#!/bin/bash
# Quick activation script for SUEWS worktree
source .venv/bin/activate

# Set compilers if available
if [ -n "$CONDA_DEFAULT_ENV" ]; then
    # Use mamba's compilers
    export FC=$(which gfortran 2>/dev/null || echo ${FC:-gfortran})
    export CC=$(which gcc 2>/dev/null || echo ${CC:-gcc})
else
    # Use system compilers
    export FC=${FC:-gfortran}
    export CC=${CC:-gcc}
fi
export F90=${F90:-$FC}
export F77=${F77:-$FC}

echo "Activated SUEWS environment for: $(cat .worktree-name)"
echo "Branch: $(git branch --show-current)"
echo "Compilers: FC=$FC, CC=$CC"
echo "Package manager: uv"

# Test Fortran modules
python -c "import supy._suews_calib" 2>/dev/null && \
    echo "✓ Fortran modules loaded successfully" || \
    echo "⚠ Fortran modules not found - may need rebuild"
EOF
chmod +x activate.sh

# Run initial build
echo "Running initial build..."
make dev || echo "Note: 'make dev' failed - you may need to run it manually"

# Test installation
echo -e "${BLUE}Testing SUEWS installation...${NC}"
python -c "import supy; print(f'✓ SuPy version: {supy.__version__}')" || echo "⚠ Import failed"
python -c "import supy._suews_calib" 2>/dev/null && \
    echo "✓ Fortran modules working" || \
    echo "⚠ Fortran modules not loaded"

# Success message
echo ""
echo -e "${GREEN}✓ SUEWS worktree setup complete!${NC}"
echo ""
echo "Location: $PWD"
echo "Branch: feature/$FEATURE"
echo "Environment: .venv (managed by uv)"
echo ""
echo "To start working:"
echo "  cd worktrees/$FEATURE"
echo "  ./activate.sh  # or: source .venv/bin/activate"
echo "  make test"
echo ""
echo "For best results with SUEWS:"
echo "  mamba activate suews-dev  # Before running this script"
echo ""
echo "To clean up later:"
echo "  ./.claude/scripts/worktree-cleanup.sh $FEATURE"