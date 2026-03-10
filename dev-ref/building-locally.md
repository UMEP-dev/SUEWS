# Building SUEWS Locally

If you want to build SUEWS from source for local use, this guide covers prerequisites, setup, testing, and the project structure.

## Prerequisites

- Python 3.9+
- `gfortran` compiler (≥ 9.3.0) — see [README prerequisites](../README.md#prerequisites) for platform-specific instructions
- [`uv`](https://docs.astral.sh/uv/) (recommended) or `pip`

## Quick Start

The Makefile provides convenient recipes for common tasks. Run `make help` to see all available commands.

### Recommended: Using uv

```bash
# Clone repository
git clone https://github.com/UMEP-dev/SUEWS.git
cd SUEWS

# Create virtual environment and install
make setup                      # Create .venv
source .venv/bin/activate       # Activate environment
make dev                        # Install SUEWS in editable mode

# Verify installation
make test
```

### Alternative: Using pip directly

```bash
pip install -e .
```

## Common Makefile Recipes

### Development
- `make setup` - Create virtual environment (requires `uv`)
- `make dev` - Install SUEWS in editable mode (self-healing, works after clean)

### Testing
- `make test` - Run test suite (excludes slow tests ~3-4 min)
- `pytest test -m slow -v` - Run slow tests manually

### Documentation
- `make docs` - Build HTML documentation
- `cd docs && make livehtml` - Live-reload development server

### Maintenance
- `make clean` - Smart clean (preserves active `.venv`)
- `make format` - Format Python (ruff) and Fortran (fprettify)
- `make help` - Show all available commands

### Common Workflows

```bash
# Fresh start (most common for troubleshooting)
make clean && make dev

# Update code and rebuild
git pull && make dev

# Build and test changes
make dev && make test
```

## Additional Notes

- The Makefile automatically detects and uses `uv` if available (faster installation)
- On macOS, it automatically uses Homebrew's `gfortran` for compatibility
- `make clean` intelligently preserves `.venv` if you're currently using it
- Slow tests are skipped in `make test` but run automatically in CI

## Project Structure

```text
SUEWS/
├── src/
│   ├── suews/          # Fortran physics engine
│   ├── supy/           # Python interface
│   └── supy_driver/    # Python-Fortran bridge
├── test/               # Test suite
├── docs/               # User documentation (Sphinx)
└── dev-ref/            # Developer documentation (Markdown)
```

For a more comprehensive developer overview (workflow, testing strategy, best practices), see the [Onboarding Guide](onboarding-guide.md).

