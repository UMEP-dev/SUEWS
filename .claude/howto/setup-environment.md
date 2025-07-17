# Environment Setup Guide

This guide covers Python environment setup for SUEWS development using uv, the ultra-fast Python package manager.

## Quick Start with uv (Recommended)

### Installation

```bash
# macOS/Linux
brew install uv
# or
curl -LsSf https://astral.sh/uv/install.sh | sh

# Verify installation
uv --version
```

### Setup and Usage

```bash
# Create virtual environment
uv venv

# Install packages (10-100x faster than pip!)
uv pip install pandas scipy matplotlib

# Run without activation - the modern way!
uv run python script.py
uv run pytest
uv run make test

# Or activate traditionally if preferred
source .venv/bin/activate
python script.py  # Works as normal
```

### Installing SUEWS Dependencies

```bash
# Core packages for SUEWS development
# Note: Some package names differ between conda and pip
uv pip install pandas scipy matplotlib scikit-learn scikit-image \
    geopandas rtree openpyxl tables psutil salem==0.3.8 floweaver==2.0.0 \
    f90nml click pydantic ipykernel jupyter_client jupyter_core \
    pytest pytest-cov ruff f90wrap==0.2.16 atmosp meson-python>=0.17.0 \
    pip>=22.0 setuptools>=65.0 wheel

# Then build SUEWS
make dev
```

## Compiler Requirements

SUEWS requires a Fortran compiler:

```bash
# macOS
brew install gcc  # Provides gfortran

# Linux
sudo apt-get install gfortran  # Ubuntu/Debian
sudo dnf install gcc-gfortran  # Fedora/RHEL

# Verify
which gfortran
```

### macOS ARM64 (Apple Silicon) Special Instructions

When building on macOS ARM64, you may encounter issues with the build system looking for architecture-specific tools. Here's how to resolve them:

```bash
# Set environment variables for the build tools
export AR=/usr/bin/ar
export RANLIB=/usr/bin/ranlib
export FC=/opt/homebrew/bin/gfortran
export CC=clang

# Then install with pip (not uv) for ARM64 builds
pip install --no-build-isolation --editable .

# Or if using make dev, the Makefile will handle FC for you:
make dev
```

**Note**: On ARM64 Macs, the build may fail if it tries to use conda/mamba's cross-compilation tools. Always use Homebrew's native gfortran and system tools for best compatibility.

## Performance Comparison

| Tool | Create Env | Install 50 packages |
|------|------------|-------------------|
| uv | 1s | 5-10s |
| pip | 3s | 60-120s |

## Best Practices

1. **One environment per worktree** - Avoid conflicts
2. **Always use uv when available** - It's dramatically faster
3. **Run tests after setup** - `make test` to verify

## Quick Reference

```bash
# uv commands
uv venv                    # Create environment
uv pip install package     # Install package
uv run python script.py    # Run without activation
uv run pytest             # Run tests without activation
uv run make test          # Run make commands
```

For legacy setup methods (venv, mamba/conda), see the SUEWS development guide.