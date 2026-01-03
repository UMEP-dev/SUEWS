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

echo "Session setup complete."
