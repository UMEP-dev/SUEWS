# SUEWS Test Suite

The test suite is organised around two ideas:

- Functional areas, which keep related assertions close to the code they protect.
- Cross-cutting markers, which decide where each test runs in the CI matrix.

Every collected test file must carry at least one nature marker: `api` or `physics`.

## Test Organisation

- `test_api_surface.py` - import-surface guard that runs first.
- `cmd/` - command helper and conversion internals.
- `core/` - public APIs, CLI wiring, simulation workflows, runtime state, utilities, and key regression tests.
- `data_model/` - schema, YAML processing, validation, release compatibility, and migration checks.
- `e2e/` - scenario-level tests written as user or maintainer stories.
- `io_tests/` - output configuration, saving, resampling, annotations, and file-format behaviour.
- `physics/` - scientific and numerical validation.
- `umep/` - QGIS/UMEP integration surface, gated to Windows + Python 3.12 with the `qgis` marker.
- `fixtures/` - vendored inputs and expected outputs used by the tests.

UMEP tests protect the plugin-facing integration surface: import paths, YAML-backed runtime construction, output path handling, `run_supy` calling patterns, and QGIS stdout/stderr behaviour. Current Windows QGIS 3 LTR and QGIS 4 runtimes both use Python 3.12, so a single Windows + Python 3.12 lane is enough for this repository's plugin-facing compatibility checks.

## Running Tests

Use the Makefile targets after `make dev` has installed the editable package:

```bash
make test-smoke
make test
make test-e2e
make test-e2e-all
make test-all
make test-qgis
```

Direct pytest examples:

```bash
pytest test/core -v
pytest test/data_model -v
pytest test/e2e -m "not slow" -v
pytest test/physics -v
pytest test/umep -m qgis -v
```

The Makefile autodetects the active virtual environment, `.venv/bin/python`, or `python3`.
If `pytest` or the editable `supy` package is missing, the test targets stop with setup guidance.

## Markers

Markers sit on three axes.

### Nature Axis

- `physics` - numerical or binary correctness. Outputs are determined mostly by the compiled artefact and CPU floating-point behaviour.
- `api` - Python wrapper, CLI, pandas/numpy/pydantic, schema, validation, or user-facing API behaviour.

Rare files belong on both axes:

```python
pytestmark = [pytest.mark.physics, pytest.mark.api]
```

UMEP tests pick up `api` automatically via `test/umep/conftest.py`; they are gated to Windows + Python 3.12 by the `qgis` marker. This matches the current Windows runtime for both QGIS 3 LTR and QGIS 4; the Qt/PyQt difference is outside this repository's direct test surface.

### Scenario Axis

- `e2e` - scenario-level workflow tests. Each E2E test states the persona, starting condition, user action, expected warning/error, and final result.

E2E tests still need `api` or `physics`. Most current scenarios are `api` tests because they exercise user-facing Python and CLI workflows.

### Tier Axis

- `smoke` - minimal wheel validation, about 6 tests and about 60 seconds.
- `smoke_bridge` - legacy bridge-loading subset marker.
- `core` - core physics and driver logic.
- `rust` - Rust bridge backend tests requiring the physics-enabled bridge.
- `util` - utility function tests.
- `cfg` - configuration and schema validation tests.
- `slow` - tests taking more than 30 seconds individually.
- `qgis` - UMEP plugin tests in `test/umep/`.

### Selecting a Subset

```bash
pytest -m physics
pytest -m api
pytest -m "physics and smoke"
pytest -m "api and not slow"
pytest -m "physics and api"
```

### PR/CR Placement Rules

- Put numerical guardrails in `test/physics/` or a clearly named physics file under `test/core/`, mark them `physics`, and add `core` only when they are fast enough for draft PRs and merge-queue checks.
- Put pandas, numpy, pydantic, CLI, and wrapper behaviour in `api` tests. These run across the CPython bookends because the dependency surface varies by interpreter.
- Mark long regression or reproduction tests `slow`. Slow tests run in `test-all`, scheduled builds, release builds, or explicit manual validation; they are excluded from smoke, core, cfg, standard, and local `make test`.
- Keep UMEP/QGIS tests under `test/umep/` with the auto-applied `api` and `qgis` markers. They run in `all` validation on the Windows + Python 3.12 cell or through `make test-qgis`; keep them out of normal PR/CR tiers unless a change directly touches the UMEP/QGIS integration contract.
- Keep `smoke` tiny: imports, one short model run, and the minimum output validation needed to fail fast.

## Scenario Tests

Scenario-level tests live in `test/e2e/` and complement, rather than replace, focused functional tests. They are for complete workflows where the useful question is:

- Can the user or maintainer complete the job?
- Did the intended warning or error appear?
- Did the expected output, report, or migrated artefact exist and contain meaningful content?

Keep default E2E scenarios short. Mark heavier full-run scenarios as `slow` so `make test-e2e` remains suitable for normal development.

## Adding New Tests

1. Put focused component tests in the relevant functional directory.
2. Put whole workflow tests in `test/e2e/`.
3. Add `pytestmark = pytest.mark.api` or `pytestmark = pytest.mark.physics` at module level.
4. Add `pytest.mark.e2e` to scenario files or classes.
5. Use `slow` only when the individual test is expensive enough to keep out of normal development loops.
6. Update `test/e2e/README.md` when adding or changing a scenario.
