---
name: design-tests
description: Design simple, effective tests following FIRST principles and scientific computing best practices. Use this skill when (1) writing new tests for Python or Fortran code, (2) reviewing existing test coverage, (3) designing physics validation tests, (4) creating data model tests, or (5) setting appropriate tolerances for numerical comparisons. Especially useful for mixed Python-Fortran codebases and scientific computing with tolerance-based assertions.
---

# Design Tests

Design tests that are Fast, Independent, Repeatable, Self-validating, and Timely.

## Test Design Workflow

### Step 1: Identify Test Type

Ask: "What am I testing?"

| Code Type | Test Category | Reference |
|-----------|---------------|-----------|
| Fortran physics function | Physics validation | [fortran-tests.md](references/fortran-tests.md) |
| Python utility function | Unit test | [python-tests.md](references/python-tests.md) |
| Pydantic config model | Data model test | [data-model-tests.md](references/data-model-tests.md) |
| Energy/water balance | Physics validation | [physics-tests.md](references/physics-tests.md) |
| Benchmark comparison | Tolerance test | [tolerance-patterns.md](references/tolerance-patterns.md) |
| SUEWS benchmark | Regression test | [benchmark-tests.md](references/benchmark-tests.md) |

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

## Quick Reference

### Key Questions

1. **What am I testing?** One clear objective
2. **What's the expected behaviour?** Define success
3. **What could go wrong?** Failure modes
4. **How do I know it worked?** Assertions
5. **Is this test reproducible?** Deterministic
6. **Is this test fast enough?** Performance

### Tolerance Guidelines (Scientific Basis)

| Variable | Relative | Absolute | Justification |
|----------|----------|----------|---------------|
| Energy fluxes | 0.8% | 1 W/m2 | Eddy covariance uncertainty 5-10% |
| Temperature | 0.2% | 0.01 C | Sensor accuracy +/-0.1C |
| Humidity | 1% | 0.5% | Sensor accuracy +/-2-3% |
| Wind speed | 0.5% | 0.01 m/s | Anemometer +/-0.1 m/s |

### Package Data Access

```python
from importlib.resources import files, as_file

# Read package data
config = files("supy") / "sample_data" / "sample_config.yml"
with as_file(config) as path:
    df_state = sp.init_supy(path)
```

### Fortran-Specific Pitfalls

1. **State persistence**: Fortran module variables persist between tests
2. **Array indexing**: Fortran is 1-based, Python is 0-based
3. **Floating-point**: Use `np.allclose()`, never exact equality

## Test File Locations

```
test/
├── core/           # API, CLI, utilities
├── data_model/     # Pydantic configuration
├── physics/        # Scientific validation
├── io_tests/       # Input/output handling
└── fixtures/       # Test data and benchmarks
```

## Coverage Targets

- **Overall**: 80%
- **Critical paths**: 95-100%
- **Core functions**: 85-90%
- **Utilities**: 70-80%

## Anti-Patterns

Avoid:
- Exact floating-point equality (`==`)
- Magic number tolerances without justification
- Testing implementation details rather than behaviour
- Relative paths from repository root
- Tests that depend on execution order
