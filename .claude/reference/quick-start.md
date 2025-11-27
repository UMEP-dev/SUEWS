# Quick Start Commands

**Run `make` in the repository root to see quick start workflows and available commands.**

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

Run `make` to see all commands. Key ones:

- `make setup` - Create virtual environment with uv
- `make dev` - Install SUEWS in editable mode
- `make test` - Run test suite
- `make clean` - Smart clean (keeps .venv if active)

## Common Workflows

```bash
# Fresh start (most common for troubleshooting)
make clean && make dev

# Update code and rebuild
git pull && make dev

# Build and test changes
make dev && make test
```

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
