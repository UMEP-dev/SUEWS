#!/bin/bash
set -euo pipefail

# Only run in Claude Code web environment
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Run asynchronously - session starts immediately while this runs in background
echo '{"async": true, "asyncTimeout": 300000}'

# Install system dependencies for Fortran compilation
echo "Installing system dependencies..."
apt-get update -qq
apt-get install -y -qq gfortran meson ninja-build

# Create virtual environment if it doesn't exist
if [ ! -d "$CLAUDE_PROJECT_DIR/.venv" ]; then
  echo "Creating virtual environment..."
  cd "$CLAUDE_PROJECT_DIR"
  uv venv
fi

# Persist VIRTUAL_ENV for the session
echo "export VIRTUAL_ENV=\"$CLAUDE_PROJECT_DIR/.venv\"" >> "$CLAUDE_ENV_FILE"
echo "export PATH=\"$CLAUDE_PROJECT_DIR/.venv/bin:\$PATH\"" >> "$CLAUDE_ENV_FILE"

# Activate venv for this script
export VIRTUAL_ENV="$CLAUDE_PROJECT_DIR/.venv"
export PATH="$CLAUDE_PROJECT_DIR/.venv/bin:$PATH"

# Install build dependencies and project in editable mode
cd "$CLAUDE_PROJECT_DIR"

# Fetch git tags (required for version detection)
echo "Fetching git tags..."
git fetch --depth=1 origin 'refs/tags/*:refs/tags/*'

# Initialise submodules and install in editable mode
echo "Installing SUEWS in editable mode..."
git submodule update --init --recursive
uv pip install --no-build-isolation -e ".[dev]"

echo "Session setup complete."
