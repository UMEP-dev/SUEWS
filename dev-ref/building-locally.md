# Building SUEWS Locally

If you want to build SUEWS from source for local use, this guide covers prerequisites, setup, testing, and the project structure.

## Prerequisites

- Python 3.12 or newer
- `gfortran` compiler (≥ 9.3.0)
  - macOS: `brew install gcc`
  - Ubuntu/Debian: `sudo apt-get install gfortran`
  - Windows (MSYS2 UCRT64): `pacman -S mingw-w64-ucrt-x86_64-gcc-fortran`
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
- `make docs-setup` - Install docs tooling and strip legacy namespace `.pth` hooks
- `make docs` - Build HTML documentation
- `cd docs && make livehtml` - Live-reload development server

### Maintenance
- `make clean` - Smart clean (preserves active `.venv`)
- `make format` - Format Python (ruff) and Fortran (fprettify)
- `make audit-deps` - Audit dependency advisories and startup hooks
- `make help` - Show all available commands

### Common Workflows

```bash
# Fresh start (most common for troubleshooting)
make clean && make dev

# Update code and rebuild
git pull && make dev

# Build and test changes
make dev && make test

# Build docs (first time needs docs-setup)
make dev && make docs-setup && make docs
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


## Build profiles

The Fortran physics library and the Rust bridge's own Fortran sources are
compiled under one of two profiles, selected by the `SUEWS_BUILD_PROFILE`
environment variable at build time (`src/supy/run_make.py` passes it to the
SUEWS Makefile as `DEBUG=1` or `DEBUG=`; `src/suews_bridge/build.rs` reads the
same variable):

- `checked` (the current default): `-O0 -fcheck=all -finit-real=zero`.
  Out-of-bounds and uninitialised reads surface as Fortran runtime errors
  rather than silent numbers. Measured against `release` on the same commit:
  about 1.7x slower on the sample configuration and 5x on SPARTACUS-heavy
  configurations.
- `release`: `-O3 -finit-real=zero`, no runtime checks. Neither profile arms
  floating-point exception traps; `src/suews/Makefile.gfortran` explains why.

```bash
SUEWS_BUILD_PROFILE=release make dev     # optimised local build
SUEWS_BUILD_PROFILE=checked make dev     # the default, spelled out
```

The variable is read when the Fortran objects are compiled, so change it with
a clean build (`make clean && make dev`). In CI the profile is an input of the
`build-suews` action and a `workflow_dispatch` choice on the wheel workflow;
the default stays `checked` until the release profile has been validated on
every platform.
