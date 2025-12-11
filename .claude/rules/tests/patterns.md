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

```python
@pytest.mark.smoke   # Critical, fast tests (~60s total)
@pytest.mark.core    # Core physics/logic tests
@pytest.mark.slow    # Tests taking >30s individually
@pytest.mark.util    # Utility function tests (non-critical)
@pytest.mark.cfg     # Config/schema validation tests
```

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
