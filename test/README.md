# SUEWS Test Suite

This directory contains the test suite for SUEWS/SuPy, organised by functionality to improve maintainability and clarity.

## Test Organisation

### Core Tests (`core/`)
Essential core functionality tests including:
- **test_sample_output.py** - Tolerance-based validation (runs first in CI fast-fail)
- **test_fortran_state_persistence.py** - Ensures Fortran state isolation between runs
- **test_floating_point_stability.py** - Numerical stability and reproducibility tests
- **test_suews_simulation.py** - High-level API interface tests
- **test_supy.py** - Comprehensive test suite (runs during wheel building)
- **test_spurious_warnings.py** - Ensures clean imports without warnings

### Data Model Tests (`data_model/`)
Configuration and data model validation tests:
- **test_data_model.py** - Data model structure and conversion tests
- **test_precheck.py** - Pre-run validation and checks
- **test_conditional_validation.py** - Physics option compatibility validation
- **test_validation_topdown.py** - Top-down configuration validation
- **test_validation_utils.py** - Validation utility functions
- **test_flexible_refvalue_clean.py** - RefValue wrapper functionality

### Physics Tests (`physics/`)
Scientific and physics validation tests:
- **test_core_physics.py** - Physical consistency checks (runs during wheel building)

### I/O Tests (`io_tests/`)
Input/output and data handling tests:
- **test_output_config.py** - Output configuration options
- **test_save_supy.py** - Output saving functionality
- **test_resample_output.py** - Output resampling capabilities
- **test_dailystate_output.py** - Daily state output handling
- **test_forcing_file_list.py** - Forcing file list handling
- **test_yaml_annotation.py** - YAML annotation features

### UMEP/QGIS Tests (`umep/`)
UMEP plugin compatibility tests (Windows + Python 3.12 target, GH-901):
- **test_preprocessor.py** - Database Manager, Database Prepare, ERA5 Download APIs
- **test_processor.py** - SUEWS model runs (init, run, save)
- **test_postprocessor.py** - Output path handling
- **test_environment.py** - QGIS-specific environment (None stdout/stderr)
- **test_imports.py** - Import path verification

These tests are still needed with the Rust backend. They do not duplicate the
physics guardrails; they protect the UMEP/QGIS integration surface: import
paths, YAML-backed runtime construction, output path handling, `run_supy`
calling patterns, and QGIS stdout/stderr behaviour. Current Windows QGIS 3 LTR
and QGIS 4 runtimes both use Python 3.12, so a single Windows + Python 3.12
lane is enough for this repository's plugin-facing compatibility checks.

### Test Fixtures (`fixtures/`)
Test data and resources:
- **benchmark1/** - Benchmark test configuration and data
- **data_test/** - Sample data for various tests
- **precheck_testcase/** - Test cases for precheck functionality

## Running Tests

```bash
# Run all tests
pytest test/ -v

# Run tests by category
pytest test/core/ -v              # Core functionality
pytest test/data_model/ -v        # Data model tests
pytest test/physics/ -v           # Physics validation
pytest test/io_tests/ -v          # I/O tests
pytest test/umep/ -v -m qgis      # UMEP/QGIS tests (Windows + Python 3.12 target)
make test-qgis                    # Same QGIS/UMEP lane via Makefile

# Run specific key tests
pytest test/core/test_sample_output.py -v    # Fast validation
pytest test/physics/test_core_physics.py -v  # Physics checks
```

## Markers

Markers sit on two orthogonal axes (gh#1300). Every test file must carry at
least one marker from the **nature** axis; markers from the **tier** axis
compose on top.

### Nature axis — what is the test actually exercising?

- `physics` — numerical / binary correctness. Outputs are determined by the
  compiled artefact and CPU floating-point, so running once per
  `(OS, arch)` on the canonical Python is sufficient. Examples: mass /
  energy balance, DailyState accumulation, surface-temperature regression.
- `api` — Python wrapper correctness. Exercises the pandas / numpy /
  pydantic surface, config validation, CLI UX, or `SUEWSSimulation`
  methods. Runs across `(platform × Python)` because the dependency
  surface varies per interpreter.

Rare files belong on **both** axes (e.g. the main integration test that
runs the model *and* heavily exercises the wrapper). Use list form:

```python
pytestmark = [pytest.mark.physics, pytest.mark.api]
```

UMEP tests pick up `api` automatically via `test/umep/conftest.py`; they
are gated to Windows + Python 3.12 by the existing `qgis` marker. This matches
the current Windows runtime for both QGIS 3 LTR and QGIS 4; the Qt/PyQt
difference is outside this repository's direct test surface.

### Tier axis — how fast or expensive is the test?

- `smoke` — minimal wheel validation (~6 tests, ~60s).
- `smoke_bridge` — legacy marker for the bridge-loading subset; still
  registered, but CI no longer selects on it directly. Post-gh#1300,
  cross-CPython coverage is driven by `-m "api and <tier>"` in the
  `test_api_cross_python` job.
- `core` — core physics and logic tests (Fortran, driver).
- `rust` — Rust bridge backend tests (requires `suews_bridge` with the
  `physics` feature).
- `util` — utility function tests (non-critical).
- `cfg` — config / schema validation tests.
- `slow` — tests taking more than 30s individually.
- `qgis` — UMEP plugin tests in `test/umep/` (Windows + Python 3.12 target).

### Selecting a subset

```bash
pytest -m physics                  # numerical / binary correctness only
pytest -m api                      # wrapper surface only
pytest -m "physics and smoke"      # physics tests in the smoke tier
pytest -m "api and not slow"       # wrapper surface, skip slow tests
pytest -m "physics and api"        # files that straddle both axes
```

### PR/CR placement rules

- Put numerical guardrails in `test/physics/` or a clearly named physics file
  under `test/core/`, mark them `physics`, and add `core` only when they are
  fast enough for draft PRs and merge-queue checks.
- Put pandas / numpy / pydantic / CLI / wrapper behaviour in `api` tests. These
  run across the CPython bookends because the dependency surface varies by
  interpreter.
- Mark long regression or reproduction tests `slow`. Slow tests run in
  `test-all`, scheduled builds, release builds, or explicit manual validation;
  they are excluded from smoke, core, cfg, standard, and local `make test`.
- Keep UMEP/QGIS tests under `test/umep/` with the auto-applied `api` + `qgis`
  markers. They run in `all` validation on the Windows + Python 3.12 cell or
  through `make test-qgis`; keep them out of normal PR/CR tiers unless a change
  directly touches the UMEP/QGIS integration contract.
- Keep `smoke` tiny: imports, one short model run, and the minimum output
  validation needed to fail fast.

### Adding a new test file

Decide which axis the file belongs on, then add one of:

```python
pytestmark = pytest.mark.api        # or pytest.mark.physics
# or, for a file that straddles both axes:
pytestmark = [pytest.mark.physics, pytest.mark.api]
```

A collection-time lint in `test/conftest.py` fails any full-tree
invocation that encounters a test file lacking both `physics` and `api`.
Subset runs (`pytest test/core/test_x.py`) are unaffected.

## Test Order

The test suite uses `conftest.py` to ensure `test_sample_output.py` runs first. This is necessary because the Fortran model maintains internal state between test runs, and running this benchmark test first ensures consistent results.

## Adding New Tests

When adding new tests:
1. Place them in the appropriate category directory
2. Follow the existing naming convention: `test_<functionality>.py`
3. Use descriptive test names that explain what is being tested
4. Add docstrings to explain complex test logic
5. Update this README if adding a new test category

For detailed testing approach, see docstrings in test files or `docs/source/contributing/testing_guide.rst`.
