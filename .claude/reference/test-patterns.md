# SUEWS Test Patterns Reference

Design tests that are Fast, Independent, Repeatable, Self-validating, and Timely.

> **Note**: This is a reference document, not an actionable skill. Use when writing tests, reviewing test coverage, or setting tolerances for numerical comparisons.

---

## Test Design Workflow

### Step 1: Identify Test Type

Ask: "What am I testing?"

| Code Type | Test Category | Reference |
|-----------|---------------|-----------|
| Fortran physics function | Physics validation | `dev-ref/testing/FORTRAN_TEST_PATTERNS.md` |
| Python utility function | Unit test | Standard pytest |
| Pydantic config model | Data model test | Schema validation |
| Energy/water balance | Physics validation | Conservation laws |
| Benchmark comparison | Tolerance test | See tolerances below |
| SUEWS benchmark | Regression test | `test/fixtures/` |

### Step 2: Apply AAA Pattern

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

### Step 3: Choose Assertions

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

### Step 4: Add pytest Markers

```python
@pytest.mark.smoke   # Critical, fast tests (~60s total)
@pytest.mark.core    # Core physics/logic tests
@pytest.mark.slow    # Tests taking >30s individually
@pytest.mark.util    # Utility function tests (non-critical)
@pytest.mark.cfg     # Config/schema validation tests
```

---

## Tolerance Guidelines (Scientific Basis)

| Variable | Relative | Absolute | Justification |
|----------|----------|----------|---------------|
| Energy fluxes (QN, QH, QE, QS) | 0.8% | 1 W/m² | Eddy covariance uncertainty 5-10% |
| Temperature | 0.2% | 0.01°C | Sensor accuracy ±0.1°C |
| Humidity | 1% | 0.5% | Sensor accuracy ±2-3% |
| Wind speed | 0.5% | 0.01 m/s | Anemometer ±0.1 m/s |

### Why These Tolerances?

- **Eddy covariance measurements** typically have 5-10% uncertainty
- **Energy balance closure** in field measurements rarely better than 70-90%
- **Model structural uncertainty** is comparable to measurement uncertainty
- **0.8% tolerance** is conservative, well within measurement uncertainty

---

## Key Questions

Before writing a test, answer:

1. **What am I testing?** One clear objective
2. **What's the expected behaviour?** Define success
3. **What could go wrong?** Failure modes
4. **How do I know it worked?** Assertions
5. **Is this test reproducible?** Deterministic
6. **Is this test fast enough?** Performance

---

## Package Data Access

```python
from importlib.resources import files, as_file

# Read package data
config = files("supy") / "sample_data" / "sample_config.yml"
with as_file(config) as path:
    df_state = sp.init_supy(path)
```

---

## Fortran-Specific Pitfalls

1. **State persistence**: Fortran module variables persist between tests
2. **Array indexing**: Fortran is 1-based, Python is 0-based
3. **Floating-point**: Use `np.allclose()`, never exact equality

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
- Tests that depend on execution order (unless documented for Fortran state)

---

## Example: Physics Test

```python
@pytest.mark.core
def test_energy_balance_closure():
    """Verify energy balance: Rn = QH + QE + QS + QF."""
    # ARRANGE
    df_output = run_simulation(config)

    # ACT
    rn = df_output["QN"]
    qh = df_output["QH"]
    qe = df_output["QE"]
    qs = df_output["QS"]
    qf = df_output["QF"]
    residual = rn - (qh + qe + qs + qf)

    # ASSERT - tolerance based on measurement uncertainty
    np.testing.assert_allclose(
        residual, 0,
        atol=5.0,  # W/m² - within measurement uncertainty
        err_msg="Energy balance not closed"
    )
```
