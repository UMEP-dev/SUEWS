# Tolerance-Based Testing Patterns

Patterns for scientific computing where exact equality is inappropriate.

## Contents

- [Why Tolerance Testing?](#why-tolerance-testing)
- [Scientific Tolerance Justification](#scientific-tolerance-justification)
- [Pattern 1: Benchmark Tolerance Testing](#pattern-1-benchmark-tolerance-testing)
- [Pattern 2: pytest.approx for Scalars](#pattern-2-pytestapprox-for-scalars)
- [Pattern 3: Array Comparison with numpy](#pattern-3-array-comparison-with-numpy)
- [Pattern 4: Statistical Tolerance](#pattern-4-statistical-tolerance)
- [Pattern 5: Tolerance with Context](#pattern-5-tolerance-with-context)
- [Pattern 6: Platform-Specific Tolerance](#pattern-6-platform-specific-tolerance)
- [Anti-Patterns](#anti-patterns)
- [Documenting Tolerance Choices](#documenting-tolerance-choices)

## Why Tolerance Testing?

1. **Floating-point arithmetic**: Results vary across platforms/compilers
2. **Measurement uncertainty**: Model outputs shouldn't be more precise than observations
3. **Numerical methods**: Iterative algorithms have inherent tolerance
4. **Cross-platform consistency**: Different optimisation levels produce different results

## Scientific Tolerance Justification

Base tolerances on measurement uncertainty, not numerical precision:

| Variable | Relative Tol | Absolute Tol | Scientific Basis |
|----------|--------------|--------------|------------------|
| Energy fluxes (QH, QE, QS) | 0.8% | 1 W/m2 | Eddy covariance: 5-10% uncertainty |
| Net radiation (QN) | 0.5% | 2 W/m2 | Radiometer accuracy: 1-2% |
| Temperature (T2) | 0.2% | 0.01 C | Sensor accuracy: +/-0.1-0.2C |
| Humidity (RH2) | 1% | 0.5% | Sensor accuracy: +/-2-3% |
| Wind speed (U10) | 0.5% | 0.01 m/s | Anemometer: +/-0.1-0.2 m/s |

## Pattern 1: Benchmark Tolerance Testing

```python
import numpy as np
import pytest

def test_benchmark_output():
    """Test model outputs against known good results."""

    # Load expected and actual
    expected = load_benchmark_results()
    actual = run_simulation()

    # Tolerance configuration
    tolerances = {
        'QN': {'rtol': 0.008, 'atol': 2.0},    # 0.8% or 2 W/m2
        'QH': {'rtol': 0.008, 'atol': 1.0},
        'QE': {'rtol': 0.008, 'atol': 1.0},
        'T2': {'rtol': 0.002, 'atol': 0.01},   # 0.2% or 0.01C
    }

    for var, tol in tolerances.items():
        np.testing.assert_allclose(
            actual[var],
            expected[var],
            rtol=tol['rtol'],
            atol=tol['atol'],
            err_msg=f"{var} exceeds tolerance"
        )
```

## Pattern 2: pytest.approx for Scalars

```python
import pytest

def test_single_value():
    """Test scalar calculations with tolerance."""
    result = calculate_ustar(h=100, z=10, u=5)

    # Relative tolerance
    assert result == pytest.approx(0.35, rel=0.01)  # 1% tolerance

    # Absolute tolerance
    assert result == pytest.approx(0.35, abs=0.01)  # +/- 0.01

    # Both
    assert result == pytest.approx(0.35, rel=0.01, abs=0.001)
```

## Pattern 3: Array Comparison with numpy

```python
import numpy as np

def test_array_output():
    """Test array outputs with tolerance."""
    expected = np.array([100.0, 150.0, 200.0])
    actual = calculate_fluxes()

    # Element-wise comparison
    np.testing.assert_allclose(actual, expected, rtol=0.01, atol=1.0)

    # Custom comparison with diagnostics
    diff = np.abs(actual - expected)
    rel_diff = diff / np.abs(expected)

    assert np.all(rel_diff < 0.01), \
        f"Max relative difference: {rel_diff.max():.2%} at index {rel_diff.argmax()}"
```

## Pattern 4: Statistical Tolerance

```python
import numpy as np

def test_statistical_agreement():
    """Test using statistical metrics rather than point-by-point."""
    expected = load_reference()
    actual = run_simulation()

    # Mean Absolute Error
    mae = np.mean(np.abs(actual - expected))
    assert mae < 5.0, f"MAE too high: {mae:.2f}"

    # Root Mean Square Error
    rmse = np.sqrt(np.mean((actual - expected) ** 2))
    assert rmse < 10.0, f"RMSE too high: {rmse:.2f}"

    # Correlation coefficient
    correlation = np.corrcoef(actual, expected)[0, 1]
    assert correlation > 0.99, f"Low correlation: {correlation:.4f}"

    # Mean Bias Error
    mbe = np.mean(actual - expected)
    assert abs(mbe) < 2.0, f"Significant bias: {mbe:.2f}"
```

## Pattern 5: Tolerance with Context

```python
def test_with_diagnostic_context():
    """Provide useful diagnostics on failure."""
    expected = load_expected()
    actual = run_calculation()

    rtol = 0.01
    atol = 1.0

    # Check with custom error message
    if not np.allclose(actual, expected, rtol=rtol, atol=atol):
        diff = np.abs(actual - expected)
        rel_diff = diff / (np.abs(expected) + atol)

        # Find worst cases
        worst_idx = np.argsort(rel_diff)[-5:][::-1]

        msg = f"Values differ beyond tolerance:\n"
        msg += f"Max absolute diff: {diff.max():.4f}\n"
        msg += f"Max relative diff: {rel_diff.max():.4%}\n"
        msg += f"Worst indices: {worst_idx}\n"
        for idx in worst_idx:
            msg += f"  [{idx}]: expected={expected[idx]:.4f}, actual={actual[idx]:.4f}\n"

        pytest.fail(msg)
```

## Pattern 6: Platform-Specific Tolerance

```python
import sys
import numpy as np

def test_cross_platform_consistency():
    """Adjust tolerance for known platform differences."""

    # Base tolerance
    rtol = 0.008

    # Platform adjustments
    if sys.platform == 'win32':
        rtol *= 1.5  # Windows compiler differences
    elif 'arm' in platform.machine().lower():
        rtol *= 1.2  # ARM floating-point differences

    np.testing.assert_allclose(actual, expected, rtol=rtol)
```

## Anti-Patterns

**AVOID:**
```python
# Too strict - fails due to floating-point
assert actual == expected

# Too loose - hides real problems
assert actual == pytest.approx(expected, rel=1.0)

# No scientific basis
assert abs(actual - expected) < 0.12345  # Magic number
```

**PREFER:**
```python
# Scientifically justified tolerance
# Eddy covariance uncertainty is 5-10%, use 0.8% for regression
np.testing.assert_allclose(actual, expected, rtol=0.008, atol=1.0)
```

## Documenting Tolerance Choices

```python
def test_energy_flux_tolerance():
    """
    Test energy fluxes with scientifically justified tolerance.

    Tolerance: 0.8% relative, 1 W/m2 absolute
    Basis: Eddy covariance measurements have 5-10% uncertainty
           (Aubinet et al. 2012, Eddy Covariance Handbook).
           We use 0.8% to detect regressions while allowing
           for legitimate platform differences.
    """
    np.testing.assert_allclose(qh_actual, qh_expected, rtol=0.008, atol=1.0)
```
