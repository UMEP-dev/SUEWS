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
`test/umep/conftest.py`; no file-level declaration needed there.

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

---

## Test File Locations

```
test/
├── core/           # API, CLI, utilities
├── data_model/     # Pydantic configuration
├── physics/        # Scientific validation
├── io_tests/       # Input/output handling
└── fixtures/       # Test data and benchmarks
```

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
- Tests depending on execution order
- Duplicating setup logic across multiple test files (use conftest.py)
- Warning suppression in setUp methods (use autouse fixtures)
- Relative imports for shared test utilities from subdirectories
- Magic numbers without named constants (e.g., `288` for timesteps/day)
- Skipping without a documented reason (see "Skipping Tests" below)
- `try/except Exception: pytest.skip(...)` to hide real failures
- `try/except ImportError: pytest.skip(...)` — use `pytest.importorskip` instead

---

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
