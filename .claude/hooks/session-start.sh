#!/usr/bin/env bash
set -euo pipefail

# Only run in Claude Code remote container environment
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Run asynchronously - session starts immediately while this runs in background
echo '{"async": true, "asyncTimeout": 300000}'

# Install system dependencies for Fortran compilation when missing
if ! command -v gfortran >/dev/null 2>&1 || ! command -v meson >/dev/null 2>&1 || ! command -v ninja >/dev/null 2>&1; then
  echo "Installing system dependencies..."
  if [ "$(id -u)" -eq 0 ]; then
    apt-get update -qq
    apt-get install -y -qq gfortran meson ninja-build
  elif command -v sudo >/dev/null 2>&1; then
    sudo apt-get update -qq
    sudo apt-get install -y -qq gfortran meson ninja-build
  else
    echo "WARNING: Missing gfortran/meson/ninja and root/sudo is unavailable."
  fi
fi

# Ensure Cargo is on PATH when installed in $HOME/.cargo/bin
if [ -d "$HOME/.cargo/bin" ]; then
  export PATH="$HOME/.cargo/bin:$PATH"
  echo "export PATH=\"\$HOME/.cargo/bin:\$PATH\"" >> "$CLAUDE_ENV_FILE"
fi

if ! command -v cargo >/dev/null 2>&1; then
  echo "WARNING: Rust cargo toolchain not found; editable build requires rustc and cargo."
fi

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

cd "$CLAUDE_PROJECT_DIR"

# Fetch git tags (required for version detection)
echo "Fetching git tags..."
git fetch --depth=1 origin 'refs/tags/*:refs/tags/*' || true

# Initialise submodules, install build prerequisites, and install in editable mode
echo "Installing SUEWS in editable mode..."
git submodule update --init --recursive
uv pip install wheel setuptools pytest "numpy>=2.0" "meson-python>=0.12.0"
uv pip install --no-build-isolation -e ".[dev]"

echo "Session setup complete."
