# Building SUEWS Locally

> **Canonical Rules Notice**
>
> For active project conventions, build gates, and agent workflows, read [`AGENTS.md`](../AGENTS.md), [`.claude/README.md`](../.claude/README.md), [`.claude/rules/00-project-essentials.md`](../.claude/rules/00-project-essentials.md), and [`.claude/rules/rust/conventions.md`](../.claude/rules/rust/conventions.md).

If you want to build SUEWS from source for local development, this guide covers prerequisites, setup, build profiles, testing, and the project layout.

## Prerequisites

- **Python `>= 3.12`**
- **`gfortran` compiler (`>= 9.3.0`)**:
  - macOS: `brew install gcc`
  - Ubuntu/Debian: `sudo apt-get install gfortran`
  - Windows (MSYS2 UCRT64): `pacman -S mingw-w64-ucrt-x86_64-gcc-fortran`
- **Rust toolchain (`rustup` / `cargo`)**:
  - Install from [https://rustup.rs](https://rustup.rs) (required to compile `src/suews_bridge/`, the C-ABI + Rust PyO3 extension that replaced `f90wrap` / `f2py`)
- **Build tools**: `meson` and `ninja` (`make dev` installs `meson-python` into `.venv`; `brew install meson ninja` or system equivalents also work for incremental `ninja` builds)
- **[`uv`](https://docs.astral.sh/uv/)** (recommended) or `pip`

## Quick Start

The `Makefile` provides recipes for common tasks. Run `make help` to see all available commands.

### Recommended: Using `uv`

```bash
# Clone repository
git clone git@github.com:UMEP-dev/SUEWS.git
cd SUEWS

# Create virtual environment and install in editable mode
make setup                      # Creates .venv via uv
source .venv/bin/activate       # Activate environment
make dev                        # Builds Fortran + suews_bridge and installs supy -e ".[dev]"

# Verify installation
make test-smoke                 # Fast smoke check (~30-60 s)
make test                       # Everyday dev suite (core, data_model, physics, io_tests)
```

### Building the Standalone Rust Engine CLI

After building the Fortran physics library (`make dev`), you can also build the developer/test Rust engine binary (`src/suews_bridge/target/release/suews-engine`):

```bash
make bridge
```

## Common Makefile Recipes

### Development
- `make setup` - Create `.venv` virtual environment (requires `uv`).
- `make dev` - Initialise submodules, compile Fortran + `suews_bridge` (C-ABI + PyO3 `abi3`), and install `supy` in editable mode.
- `make bridge` - Build the standalone `suews-engine` CLI (`cargo build --release --features physics,arrow-output`).

### Testing
- `make test-smoke` - Fast critical-path smoke tests (~30-60 s; run before every commit).
- `make test` - Everyday development tests (`core`, `data_model`, `physics`, `io_tests`; excludes `slow` and peripheral surfaces, parallelised across `TEST_JOBS=4` workers).
- `make test-all` - Full test suite including `slow` regressions and peripheral surfaces (`cmd`, `mcp`, `docs`, `knowledge`, `umep`).
- `make test-qgis` - UMEP/QGIS compatibility tests (targets Windows + Python 3.12; skips elsewhere).

### Documentation
- `make docs-setup` - Install documentation dependencies and strip legacy namespace `.pth` hooks.
- `make docs` - Build HTML documentation (`make -C docs html`).
- `make livehtml` - Live-reload documentation server (`make -C docs livehtml`).

### Maintenance
- `make clean` - Smart clean (preserves active `.venv`; for a full cache purge run `make clean && pip cache purge && make dev`).
- `make format` - Format Python (`ruff format src test`) and Fortran (`fprettify`). Use `/lint-code` for full convention checks.
- `make audit-deps` - Audit Python startup hooks and declared dependency advisories.
- `make help` - Show all available commands.

### Adding Source Files or Fortran Dependencies

- Register new Python (`.py`) and Fortran (`.f95`) files in `meson.build` (and `src/suews/Makefile` / `src/suews_bridge/build.rs` when touching the Fortran library or `src/suews_bridge/c_api/`).
- Whenever you add a Fortran file or change `USE` statements, regenerate the module dependency edges:
  ```bash
  python scripts/suews/gen_fortran_deps.py
  ```
  This updates `src/suews/Makefile.deps` (verified in CI by `test/core/test_fortran_deps.py`).
- Convert legacy table-based inputs to YAML with `suews convert` (the older `suews-convert` command is deprecated).
- Never push directly to `master`: open a pull request and merge via the GitHub merge queue (`gh pr merge --auto` or `/queue-pr`).

## Project Structure

```text
SUEWS/
|-- src/
|   |-- suews/          # Fortran physics engine (src/suews/src/*.f95)
|   |-- suews_bridge/   # C-ABI facades (c_api/) + Rust PyO3 extension (suews_bridge)
|   `-- supy/           # Python interface (SUEWSSimulation, Pydantic data_model, CLI)
|-- test/               # Pytest suite (core, data_model, physics, io_tests, cmd, mcp, docs, umep)
|-- docs/               # User documentation (Sphinx)
|-- dev-ref/            # Developer documentation, rationale, and audit ledgers
`-- .claude/            # Active coding rules, skills, and review subagents
```

For a broader developer walkthrough, see the [Onboarding Guide](onboarding-guide.md).

## Build Profiles

The Fortran physics library and the Rust bridge's Fortran C-API facades are compiled under one of two profiles selected by `SUEWS_BUILD_PROFILE` at build time (`src/supy/run_make.py` passes `DEBUG=1` or `DEBUG=` to `src/suews/Makefile`, and `src/suews_bridge/build.rs` reads the same variable):

- `release` (default): `-O3 -finit-real=zero`, no runtime bounds checks. Used for published wheels.
- `checked`: `-O0 -fcheck=all -finit-real=zero`. Out-of-bounds and uninitialised reads surface as Fortran runtime errors rather than silent numbers (~1.7x slower on the sample config and ~5x on SPARTACUS-heavy runs). The nightly CI workflow builds and tests this profile across all platforms.

Neither profile arms floating-point exception traps; `src/suews/Makefile.gfortran` explains why. Note that `FCFLAGS` is not forwarded through `meson-python`; use `SUEWS_BUILD_PROFILE` instead:

```bash
make dev                                 # release profile (default)
make clean && SUEWS_BUILD_PROFILE=checked make dev  # runtime checks for debugging physics
```
