# SUEWS Test Suite

This directory contains the test suite for SUEWS/SuPy, organised by functionality to improve maintainability and clarity.

## Test Organisation

### Core Tests (`core/`)
API, CLI, and utility tests plus the numerical guardrails:
- **test_sample_output.py** - Tolerance-based reference-output validation
- **test_fortran_state_persistence.py** - Exercises native-state isolation across an in-process order matrix
- **test_suews_simulation.py** - High-level `SUEWSSimulation` API interface tests
- **test_supy.py** - Comprehensive test suite (runs during wheel building)
- **test_load.py / test_post.py** - Forcing loading and post-processing
- **test_util_*.py** - Utility modules (atmospheric, OHM, ERA5, surface conductance)
- **test_cli_*.py** - CLI entry-point behaviour (run, validation, conversion)
- CI/tooling self-tests (**test_pytest_ci_metrics.py**, **test_ci_run_metrics.py**, **test_ci_phase_metrics.py**, **test_scheduler_benchmark.py**, **test_check_schema_version_bump.py**, **test_audit_python_startup.py**) - guard the repo's instrumentation and lint gates rather than the model

### Data Model Tests (`data_model/`)
Configuration and data model validation tests:
- **test_validation.py** - Validator behaviour: physics-option dependencies, phase-B science checks, forcing validation, DLS/irrigation rules
- **test_yaml_processing.py** - Three-phase YAML processor pipeline (Phase A uptodate, Phase B science check, Phase C Pydantic validation) plus precheck helpers
- **test_data_model.py** - Data model structure and conversion tests
- **test_physics_options.py** - Physics-option input forms: flat codes, nested family tags, orthogonal form, readable names
- **test_renames.py** - Rename registries: Pydantic field renames and DataFrame column renames, dual-read helpers, Rust bridge alignment
- **test_yaml_roundtrip.py** - `SUEWSConfig` YAML I/O: from_yaml error handling and drift hints, to_yaml round-trips, serialisation warnings
- **test_yaml_upgrade.py** - Schema migration handlers (includes `TestNoSilentFieldDrops`)
- **test_schema_versioning.py** - Schema version constant, lineage, and sample-config sync
- **test_release_compat.py** - Vendored release fixtures round-trip through the current validator

### Physics Tests (`physics/`)
Scientific and physics validation tests:
- **test_core_physics.py** - Physical consistency checks (runs during wheel building)
- Scheme-specific guardrails: anthropogenic heat, attribution, DyOHM building, irrigation wiring, Macdonald roughness, OHM coefficient blending, RSL profiles, SPARTACUS

### I/O Tests (`io_tests/`)
Input/output and data handling tests:
- **test_output_config.py** - Output configuration options
- **test_save_supy.py** - Output saving functionality
- **test_resample_output.py** - Output resampling capabilities
- **test_dailystate_output.py** - Daily state output handling
- **test_forcing_interpolation.py / test_named_column_forcing.py** - Forcing handling
- **test_output_layout_parity.py** - Output layout parity across backends
- **test_yaml_annotation.py** - YAML annotation features

### CLI Command Tests (`cmd/`)
Entry points under `suews` (init-case, validate-config, inspect-config, diagnose-run, compare-runs, summarise-output, metrics, JSON envelope contract, knowledge CLI).

### MCP Server Tests (`mcp/`)
One file per MCP tool/resource plus protocol handshake, packaging manifests, and CLI smoke tests. The one-file-per-tool layout is deliberate.

### Documentation Tests (`docs/`)
Docs-drift guards (site and docs sources), bibliography audit, and the data-model RST generator.

### Knowledge Pack Tests (`knowledge/`)
Knowledge pack integrity (`test_pack.py`).

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
- **release_configs/** - Vendored per-release YAML shapes for migration regression
- **legacy_tables/** - Vendored legacy table-based inputs

## Running Tests

```bash
# Everyday development default: core, data_model, physics, io_tests
# (excludes slow tests and the peripheral surfaces cmd/mcp/docs/knowledge/umep)
make test

# Everything, including slow tests and peripheral surfaces
make test-all

# Run tests by category
pytest test/core/ -v              # Core functionality
pytest test/data_model/ -v        # Data model tests
pytest test/physics/ -v           # Physics validation
pytest test/io_tests/ -v          # I/O tests
pytest test/cmd/ -v               # CLI entry points (run when touched)
pytest test/mcp/ -v               # MCP server (run when touched)
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

The test suite only prioritises `test_api_surface.py` so broken imports fail fast.
All other tests retain their declared collection order.
Fresh native simulation calls own fresh state; continuation is explicit through the state returned by the previous call.
The removed workaround had protected every `test_sample_output.py` item plus `TestPublicAPIEquivalence` and `test_functional_matches_oop`; `test_api_surface.py` now checks the real bounded node set retains requested order.
`test_fortran_state_persistence.py` gives A and B independently clean process references, then exercises A->B, B->A, A->A and B->B as separate test items with both calls in each transition sharing one process.

## Adding New Tests

When adding new tests:
1. Place them in the appropriate category directory
2. Follow the existing naming convention: `test_<functionality>.py`
3. Use descriptive test names that explain what is being tested
4. Prefer `pytest.mark.parametrize` over copy-pasting a test body with
   different inputs — symmetric cases (e.g. wall/roof, albedo/emissivity)
   belong in one parametrised test
5. Add docstrings to explain complex test logic
6. Update this README if adding a new test category

For detailed testing approach, see docstrings in test files or `docs/source/contributing/testing_guide.rst`.
