---
paths:
  - test/**/*.py
  - tests/**/*.py
---

# Test Patterns

Design tests that are Fast, Independent, Repeatable, Self-validating, and Timely (FIRST).

---

## AAA Pattern

```python
def test_example():
    """Clear description of what is being tested."""
    # ARRANGE: Set up test data
    input_data = prepare_inputs()

    # ACT: Execute the functionality
    result = function_under_test(input_data)

    # ASSERT: Verify the results
    assert result == expected
```

---

## Tolerance Guidelines (Scientific Basis)

| Variable | Relative | Absolute | Justification |
|----------|----------|----------|---------------|
| Energy fluxes (QN, QH, QE, QS) | 0.8% | 1 W/m^2 | Eddy covariance uncertainty 5-10% |
| Temperature | 0.2% | 0.01 C | Sensor accuracy +/-0.1 C |
| Humidity | 1% | 0.5% | Sensor accuracy +/-2-3% |
| Wind speed | 0.5% | 0.01 m/s | Anemometer +/-0.1 m/s |

**Why 0.8% tolerance?** Conservative, well within measurement uncertainty. Eddy covariance has 5-10% uncertainty; energy balance closure rarely better than 70-90%.

---

## Assertions

**Scalars:**
```python
assert result == pytest.approx(expected, rel=0.01)  # 1% tolerance
```

**Arrays:**
```python
np.testing.assert_allclose(actual, expected, rtol=0.008, atol=1.0)
```

**Exceptions:**
```python
with pytest.raises(ValueError, match="must be positive"):
    function(-1)
```

---

## Pytest Markers

Markers sit on two orthogonal axes (gh#1300). Every new test file **must**
declare the nature axis at module level; tier markers compose on top as
per-test decorators.

### Nature axis — REQUIRED on every file

Pick exactly one (or both, rarely) and declare at module level:

```python
import pytest

pytestmark = pytest.mark.api        # Python wrapper surface
# or:
pytestmark = pytest.mark.physics    # numerical / binary correctness
# or (file straddles both):
pytestmark = [pytest.mark.physics, pytest.mark.api]
```

- `physics` — outputs determined by the compiled artefact + CPU
  floating-point. CI runs these once per `(OS, arch)` on the build
  Python. Typical: mass/energy balance, DailyState accumulation, Fortran
  state persistence.
- `api` — exercises the pandas / numpy / pydantic / click surface. CI
  runs these across `(platform x Python)` because the dependency
  surface varies per interpreter. Typical: config validation, CLI, YAML
  round-trip, `SUEWSSimulation` methods.

When unsure, pick `api` — it's the broader coverage axis and safer if
the test is genuinely mixed.

UMEP tests (`test/umep/*.py`) pick up `api` automatically via
`test/umep/conftest.py`; no file-level declaration needed there. These remain
needed with the Rust backend because they guard the UMEP/QGIS plugin-facing API
contracts rather than model physics.

**A static CI lint (`scripts/lint/check_test_markers.py`) and a
`pytest_collection_finish` hook in `test/conftest.py` both fail any PR
that introduces a test file without a nature marker.** If you see the
lint fire, add a `pytestmark = pytest.mark.api` (or physics) line — do
not bypass.

### Tier axis — per-test decorators

```python
@pytest.mark.smoke   # Critical, fast tests (~60s total)
@pytest.mark.core    # Core physics/logic tests
@pytest.mark.slow    # Tests taking >30s individually
@pytest.mark.util    # Utility function tests (non-critical)
@pytest.mark.cfg     # Config/schema validation tests
```

The tier axis composes with the nature axis. CI expressions like
`-m "api and smoke"` and `-m "physics and not slow"` select the right
subset per matrix cell.

### PR/CR placement

- `smoke`: minimal fail-fast checks only. Keep this tier small enough for quick
  wheel validation.
- `core`: essential guardrails that are fast enough for draft PRs and merge
  queue. Do not mark a test `core` merely because the feature matters; use
  `slow` if the regression is important but expensive.
- `standard`: all non-slow tests for the relevant nature axis.
- `slow`: long regressions and reproductions. These belong in `make test-all`,
  scheduled/release builds, or explicit manual validation, not normal PR/CR.
- `qgis`: UMEP/QGIS tests only. These target Windows + Python 3.12, which
  matches the current Windows runtime line for both QGIS 3 LTR and QGIS 4.
  They should stay out of local `make test` and normal PR/CR tiers unless
  selected explicitly.

---

## Test File Locations

**All tests live under `test/`. Do not place `test_*.py` anywhere else in the
repository** — not in `benchmark/tests/`, not beside the source they exercise,
not in any tool subtree. Pick the subdirectory by what the test exercises:

```
test/
├── core/           # API, CLI, utilities
├── data_model/     # Pydantic config + converters (yaml_upgrade, table converter)
├── physics/        # Scientific validation
├── io_tests/       # Input/output handling
├── cmd/            # CLI entry points
├── docs/           # Documentation checks
└── fixtures/       # Test data
```

### Why this is a hard rule, not a preference

Only `test/` is wired into the project's test machinery:

- **CI runs `pytest test`** (the cross-CPython matrix in
  `test-api-cross-python-reusable.yml`). A `test_*.py` outside `test/` runs in
  **no CI job** — it silently provides zero coverage, however green it looks
  locally.
- **The nature-marker gate** (`scripts/lint/check_test_markers.py` +
  `test/conftest.py`) only walks `test/`. Tests placed elsewhere escape the
  `api` / `physics` marker discipline entirely.
- Shared fixtures, conftest helpers, and tier markers (`smoke` / `slow` / …)
  are all rooted at `test/`.

So a test's *location* determines whether it is actually run and disciplined.
A test for shipped `src/supy/**` code that lives under `benchmark/` is the
canonical mistake: it tests core code but is invisible to CI. Move it to the
matching `test/` subdirectory and give it a nature marker.

If a fixture lives in a git tag or external source, **vendor a minimal,
self-contained copy under `test/fixtures/`** rather than fetching at test time
— the standard CI checkout is shallow and tag-less, so a tag-fetching test
skips (no coverage). See `test/fixtures/legacy_tables/` for the pattern.

---

## Coverage Targets

- **Overall**: 80%
- **Critical paths**: 95-100%
- **Core functions**: 85-90%
- **Utilities**: 70-80%

---

## Anti-Patterns to Avoid

- Exact floating-point equality (`==`)
- Magic number tolerances without justification
- Testing implementation details rather than behaviour
- Relative paths from repository root
- `Path(supy.__file__).parent` to reach packaged data (see "Locating packaged data" below)
- Tests depending on execution order
- Duplicating setup logic across multiple test files (use conftest.py)
- Warning suppression in setUp methods (use autouse fixtures)
- Relative imports for shared test utilities from subdirectories
- Magic numbers without named constants (e.g., `288` for timesteps/day)
- Skipping without a documented reason (see "Skipping Tests" below)
- `try/except Exception: pytest.skip(...)` to hide real failures
- `try/except ImportError: pytest.skip(...)` — use `pytest.importorskip` instead

---

## Locating packaged data

Never reach for packaged data through the module's file location:

```python
# WRONG - assumes the package is an unpacked directory on disk
sample_dir = Path(supy.__file__).parent / "sample_data"
sample_dir = Path(sp.__file__).parent / "sample_data"
```

Use the package's own resource handle instead:

```python
# RIGHT - what supy itself uses, in src/supy/_env.py
from supy._env import trv_supy_module

sample_dir = trv_supy_module / "sample_data"
```

`trv_supy_module` is `importlib.resources.files("supy")`. It returns a
`Traversable`, which is **not** a `Path` — and the difference is the point, so do
not treat it as a drop-in.

The protocol guarantees exactly: `joinpath` (and so `/`), `name`, `is_dir`,
`is_file`, `iterdir`, `open`, `read_bytes`, `read_text`. Use those:

```python
sample_config = trv_supy_module / "sample_data" / "sample_config.yml"

assert sample_config.is_file()                  # NOT .exists(), not in the protocol
with sample_config.open(encoding="utf-8") as f: # NOT builtin open(...)
    cfg = yaml.safe_load(f)
text = sample_config.read_text(encoding="utf-8")
```

Anything that requires `os.PathLike` — builtin `open()`, `shutil.copy()`,
subprocess arguments — needs a real filesystem path, which means
`importlib.resources.as_file()`:

```python
from importlib.resources import as_file

with as_file(sample_config) as real_path:
    shutil.copy(real_path, destination)
```

Under an editable install these all appear to work regardless, because the
Traversable happens to wrap a real path. That is what makes the mistake easy: it
passes locally and only fails where the abstraction was supposed to help.

### Why

- `__file__` assumes the package is an unpacked directory on disk. The import
  system does not guarantee that; zip imports and some packaging layouts break it.
  `importlib.resources` exists precisely to abstract over this.
- It couples the test to supy's internal directory layout, so a package
  reorganisation breaks tests that are not testing packaging.
- It reimplements, less safely, something the package already does. `_env.py` has
  resolved its own data with `files("supy")` since May 2024.

### Why this keeps happening

There is no public accessor for the sample-data path. `dir(supy)` exposes nothing
for it and `trv_supy_module` is private, so a test author who needs the directory
has no supported route and reaches for the obvious one. As of August 2026 there
are around two dozen occurrences across roughly ten test modules, each invented
independently.

Until a public accessor exists, importing `trv_supy_module` from `supy._env` is
the correct thing for a test to do: a test importing a private helper is normal,
and matching the package's own convention is what stops the two drifting apart.
If you are adding a new test that needs packaged data, use it rather than adding
a twenty-fifth variant.

## Skipping Tests

Every `pytest.skip`, `@pytest.mark.skip`, `@pytest.mark.skipif`, and `pytest.importorskip` **must** carry a concrete `reason=` string that tells a future reader *why* the skip exists and *under what condition* it fires. Skips without rationale become permanent dead code (#912).

### Required rationale

- **Conditional skip** (`skipif`) — name the missing dependency, credential, platform, or fixture:
  ```python
  @pytest.mark.skipif(
      not has_cds_credentials(),
      reason="Requires CDS API credentials (~/.cdsapirc)",
  )
  ```
- **In-body skip** (`pytest.skip(...)`) — name the missing resource:
  ```python
  if not fixture_path.exists():
      pytest.skip(f"Fixture data not available at {fixture_path}")
  ```
- **Unconditional skip** (`@pytest.mark.skip`) — only acceptable with a tracking issue reference and a real failure-mode description. "Pre-existing issue" is not a reason.
  ```python
  @pytest.mark.skip(
      reason="from_state(parquet) round-trip drops metadata; see #NNNN",
  )
  ```

### Choose the right primitive

- **Optional import** → `pytest.importorskip("pvlib")` at module scope (or per-test). Never `try/except ImportError: pytest.skip(...)`.
- **Required-but-optional resource** (data file, binary, credentials) → a fixture in `conftest.py` that probes the resource once and calls `pytest.skip` with a concrete reason. See `cru_data_available` in `test/conftest.py`.
- **Never** wrap the test body in `try/except Exception: pytest.skip(...)`. This swallows real regressions silently. Let failures surface; use a targeted `skipif` guard if the test genuinely cannot run in some environments.

### When to delete rather than skip

- If a test has been permanently skipped for more than one release cycle with no tracking issue, delete it. Zombie tests are worse than missing tests — they imply coverage that doesn't exist.
- If a test's only job is to `print(...)` and `assert True`, it is documentation, not a test. Delete it; put the concern in lint config or a dedicated check.

---

## Validation Edge Cases

When a change tightens an input contract, add explicit regressions for all relevant invalid forms, not just the obvious one:

- Negative values
- Canonical sentinel values (for example `-999`)
- `NaN` / missing values

If the feature is available through more than one public API, include at least one regression per API path. Covering only the modern interface is not enough if deprecated or legacy entry points still exist.

---

## Centralisation Patterns

### Shared Utilities in conftest.py

Place shared decorators, fixtures, and utilities in `test/conftest.py`:

```python
# test/conftest.py

# Centralised imports with fallbacks
try:
    from debug_utils import debug_on_ci, capture_test_artifacts
except ImportError:
    def debug_on_ci(func):
        return func
    def capture_test_artifacts(name):
        return lambda func: func

# Global warning suppression (replaces setUp boilerplate)
@pytest.fixture(autouse=True)
def suppress_import_warnings():
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=ImportWarning)
        yield

# Named constants
TIMESTEPS_PER_DAY = 288  # 24*60/5 = 288 five-minute intervals
```

### Importing in Test Files

```python
# Good: Import from conftest
from conftest import debug_on_ci, capture_test_artifacts, TIMESTEPS_PER_DAY

# Bad: Relative imports with fallback logic in each file
try:
    from .debug_utils import debug_on_ci  # Don't do this
except ImportError:
    def debug_on_ci(func): return func  # Duplicated everywhere
```

---

## Physics Test Example

```python
@pytest.mark.core
def test_energy_balance_closure():
    """Verify energy balance: Rn = QH + QE + QS + QF."""
    # ARRANGE
    df_output = run_simulation(config)

    # ACT
    residual = df_output["QN"] - (
        df_output["QH"] + df_output["QE"] +
        df_output["QS"] + df_output["QF"]
    )

    # ASSERT - tolerance based on measurement uncertainty
    np.testing.assert_allclose(
        residual, 0,
        atol=5.0,  # W/m^2 - within measurement uncertainty
        err_msg="Energy balance not closed"
    )
```
