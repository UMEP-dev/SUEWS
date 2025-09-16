#!/bin/bash
# setup-beta.sh - Quick setup for beta testing SUEWS/SuPy development versions

set -e

echo "SUEWS/SuPy Beta Testing Setup"
echo "============================"
echo ""

# Check if uv is installed
if ! command -v uv &> /dev/null; then
    echo "Installing uv package manager..."
    curl -LsSf https://astral.sh/uv/install.sh | sh
    
    # Source shell configuration to make uv available
    if [ -f "$HOME/.zshrc" ]; then
        source "$HOME/.zshrc"
    elif [ -f "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi
    
    # Check again
    if ! command -v uv &> /dev/null; then
        echo "Error: uv installation failed or not in PATH"
        echo "Please restart your terminal and run this script again"
        exit 1
    fi
else
    echo "uv is already installed"
fi

# Create virtual environment for beta testing
echo ""
echo "Creating virtual environment for beta testing..."
uv venv .venv-beta

# Activate environment
echo ""
echo "Activating environment..."
if [ -f ".venv-beta/bin/activate" ]; then
    source .venv-beta/bin/activate
elif [ -f ".venv-beta/Scripts/activate" ]; then
    source .venv-beta/Scripts/activate
else
    echo "Error: Could not find activation script"
    exit 1
fi

# Install latest development version
echo ""
echo "Installing latest SuPy development version from test.pypi.org..."
uv pip install --index-url https://test.pypi.org/simple/ \
              --extra-index-url https://pypi.org/simple/ \
              supy

# Verify installation
echo ""
echo "Verifying installation..."
python -c "import supy as sp; print(f'SuPy version installed: {sp.__version__}')"

echo ""
echo "Beta testing environment setup complete!"
echo ""
echo "To activate this environment in the future, run:"
echo "  source .venv-beta/bin/activate    # Linux/macOS"
echo "  .venv-beta\\Scripts\\activate       # Windows"
echo ""
echo "To install a specific version, run:"
echo "  uv pip install --index-url https://test.pypi.org/simple/ \\"
echo "                --extra-index-url https://pypi.org/simple/ \\"
echo "                supy==VERSION"
echo ""
echo "For more information, see:"
echo "  - .claude/howto/beta-testing.md"
echo "  - https://test.pypi.org/project/supy/"