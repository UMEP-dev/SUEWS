#!/bin/bash
set -euo pipefail

# Only run in Claude Code web environment
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Install system dependencies for Fortran compilation
echo "Installing system dependencies..."
apt-get update -qq
apt-get install -y -qq gfortran meson ninja-build

# Create and activate virtual environment if it doesn't exist
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

echo "Installing build dependencies..."
uv pip install wheel pytest "f90wrap==0.2.16" "numpy>=2.0" "meson-python>=0.12.0"

echo "Installing SUEWS in editable mode..."
git submodule update --init --recursive
uv pip install --no-build-isolation -e ".[dev]"

echo "Session setup complete."
