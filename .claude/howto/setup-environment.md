# Environment Setup Guide

This guide covers Python environment setup for SUEWS development, with uv as the recommended approach.

## Quick Start (Recommended)

### With uv (Ultra-fast)

```bash
# Install uv (if not already installed)
brew install uv  # macOS
# or
curl -LsSf https://astral.sh/uv/install.sh | sh

# Create environment and install SUEWS
make setup  # Creates .venv with uv
source .venv/bin/activate  # Required due to Python 3.13 compatibility
make dev    # Install in editable mode

# Verify installation
python -c "import supy; print(f'✓ SuPy {supy.__version__} ready')"
```

### With mamba

```bash
mamba activate suews-dev  # or create new environment
make dev  # Detects mamba and installs accordingly
```

## Why uv?

- **10-100x faster** than pip/mamba for package installation
- **Single tool** for all Python needs
- **Built-in lock file support** for reproducible environments
- **Automatic dependency resolution** without mamba complexity

| Tool | Create Env | Install 50 packages |
|------|------------|-------------------|
| uv | 1s | 5-10s |
| pip | 3s | 60-120s |
| mamba | 5s | 30-60s |

## Detailed Setup Options

### Option 1: Using Makefile (Simplest)

The Makefile automatically detects your environment and uses the appropriate tools:

```bash
make setup  # Creates .venv with uv (if uv available)
source .venv/bin/activate
make dev    # Installs SUEWS with all dependencies
make test   # Run tests to verify
```

### Option 2: Manual uv Setup

For more control over the installation process:

```bash
# Create virtual environment
uv venv
source .venv/bin/activate

# Install SUEWS with development dependencies
uv pip install -e ".[dev,docs]"

# Or install specific packages manually
uv pip install pandas scipy matplotlib matplotlib-inline scikit-learn scikit-image \
    geopandas rtree openpyxl tables psutil salem==0.3.8 floweaver==2.0.0 \
    f90nml click pydantic ipykernel jupyter_client jupyter_core \
    pytest pytest-cov ruff f90wrap==0.2.16 atmosp "meson-python>=0.17.0"
```

### Option 3: Standard Python venv

If uv is not available:

```bash
# Create and activate venv
python -m venv .venv
source .venv/bin/activate

# Install SUEWS (automatically installs dependencies)
make dev
```

### Option 4: Mamba Environment

For complex dependencies or when system packages are needed:

```bash
# Create environment from env.yml
mamba env create -f env.yml -n suews-dev
mamba activate suews-dev

# Install SUEWS in development mode
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

# Verify installation
which gfortran
gfortran --version
```

**macOS Note**: The Makefile automatically uses Homebrew's gfortran if available:
```bash
FC=/opt/homebrew/bin/gfortran make dev
```

## Package Name Differences

When migrating from mamba, note these package name differences:
- mamba: `matplotlib-base` → pip/uv: `matplotlib`
- mamba: `pytables` → pip/uv: `tables`

## Python 3.13 Compatibility

Currently, you need to activate the environment (`source .venv/bin/activate`) rather than using `uv run` due to Python 3.13 compatibility. This is temporary and will be resolved as packages add Python 3.13 support.

For specific Python version:
```bash
uv venv --python 3.12  # Use Python 3.12 if 3.13 causes issues
```

## Working with Git Worktrees

Each worktree MUST have its own environment to avoid conflicts:

```bash
# In each worktree
cd worktrees/my-feature
make setup && source .venv/bin/activate && make dev
```

See `worktree-workflow.md` for complete worktree setup instructions.

## Troubleshooting

### Import errors
- Ensure environment is activated: `source .venv/bin/activate`
- Rebuild: `make clean && make dev`

### Package conflicts
- Clear uv cache: `uv cache clean`
- Remove and recreate environment: `rm -rf .venv && make setup`

### Compiler issues
- macOS: Ensure Xcode Command Line Tools installed: `xcode-select --install`
- Check gfortran path: `which gfortran`

### Python version issues
- Check version: `python --version`
- Use specific version: `uv venv --python 3.12`

## Makefile Commands

- `make setup` - Create virtual environment with uv
- `make dev` - Install SUEWS in editable mode
- `make test` - Run test suite
- `make clean` - Smart clean (keeps .venv if active)
- `make format` - Format Python and Fortran code

## Best Practices

1. **One environment per worktree** - Avoid conflicts
2. **Always use uv when available** - Dramatically faster
3. **Use Makefile recipes** - Handles environment detection
4. **Test after setup** - `make test` to verify installation
5. **Document deviations** - If you use a different setup

## Quick Reference

```bash
# Essential commands
make setup                  # Create environment
source .venv/bin/activate   # Activate
make dev                    # Install SUEWS
make test                   # Run tests
make clean                  # Clean build artifacts

# uv-specific commands
uv venv                     # Create environment
uv pip install package      # Install package
uv cache clean              # Clear cache
```

For additional details on uv capabilities and advanced usage, see the [uv documentation](https://github.com/astral-sh/uv).