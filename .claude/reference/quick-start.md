# Quick Start Commands

**Single source of truth for SUEWS development environment setup.**

## Recommended: uv (Ultra-fast)

```bash
# Create environment (if .venv doesn't exist)
uv venv

# Activate environment
source .venv/bin/activate

# Install SUEWS in editable mode
make dev

# Verify installation
python -c "import supy; print(f'✓ SuPy {supy.__version__} ready')"

# Run tests
make test
```

## Alternative: Standard venv

```bash
# Create environment (if .venv doesn't exist)
python -m venv .venv

# Activate environment
source .venv/bin/activate

# Install SUEWS in editable mode
make dev

# Run tests
make test
```

## Alternative: mamba

```bash
# Activate mamba environment
mamba activate suews-dev

# Install SUEWS in editable mode
make dev

# Run tests
make test
```

## Makefile Commands

- `make setup` - Create virtual environment with uv (if available)
- `make dev` - Install SUEWS in editable mode
- `make test` - Run test suite
- `make clean` - Smart clean (keeps .venv if active)
- `make format` - Format Python and Fortran code

## Package Name Differences

When migrating from mamba:
- mamba: `matplotlib-base` → pip/uv: `matplotlib`
- mamba: `pytables` → pip/uv: `tables`

## Compiler Requirements

SUEWS requires gfortran:
```bash
# macOS
brew install gcc

# Linux
sudo apt-get install gfortran  # Ubuntu/Debian
```

For detailed setup options and troubleshooting, see `.claude/howto/setup-environment.md`
