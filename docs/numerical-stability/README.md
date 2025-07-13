# Numerical Stability in SUEWS

This directory documents the numerical stability improvements made to handle platform-specific floating-point precision differences in SUEWS.

## The Issue

Different platforms (Windows/Linux/macOS) with different Python versions and NumPy versions can produce slightly different floating-point values. When these values are near decision thresholds (e.g., 10°C for OHM coefficient selection), tiny differences can cause different code paths and significantly different results.

### Affected Configurations
- Python 3.9 with NumPy 1.x (Linux)
- Python 3.13 with NumPy 2.1 (Windows)
- Various compiler and BLAS/LAPACK implementations

## The Solution

Implemented tolerance-based comparisons in critical threshold checks within the OHM (Objective Hysteresis Model) module:

```fortran
! Tolerance parameters for numerical comparisons
REAL(KIND(1D0)), PARAMETER :: eps_temp = 1.0D-5      ! Temperature tolerance (0.00001°C)
REAL(KIND(1D0)), PARAMETER :: eps_moisture = 1.0D-8  ! Moisture ratio tolerance
REAL(KIND(1D0)), PARAMETER :: eps_state = 1.0D-11    ! State comparison tolerance
```

These tolerances are:
- Well below measurement precision (sensors typically ±0.1°C)
- Physically insignificant (0.00001°C has no meaningful impact)
- Standard practice in numerical computing

## Documentation

- **[WIN313_OHM_DIVERGENCE_AUTOPSY_REPORT.md](WIN313_OHM_DIVERGENCE_AUTOPSY_REPORT.md)** - Detailed forensic analysis of the Windows Python 3.13 divergence issue
- **[coefficient_mismatch_summary.png](coefficient_mismatch_summary.png)** - Visual explanation of how tiny floating-point differences cause coefficient selection divergence
- **[win313_ohm_threshold_visualization.png](win313_ohm_threshold_visualization.png)** - Temperature threshold analysis showing concentration of issues near 10°C

## Impact

This fix ensures:
- ✅ Consistent results across all supported platforms
- ✅ Maintained physical accuracy of the model
- ✅ No changes to the underlying physics
- ✅ Robust handling of floating-point arithmetic

## Technical Details

The OHM model selects coefficients based on environmental conditions:
- **Summer/Dry**: When 5-day average temperature ≥ 10°C
- **Winter/Wet**: When 5-day average temperature < 10°C

Without tolerances, a temperature of 9.99999°C vs 10.00001°C (0.00002°C difference) would select different coefficients, leading to storage heat flux differences up to 55.6 W/m².

With tolerances, both values are treated consistently, eliminating platform-specific divergence while maintaining the model's physical behavior.

## References

- PR #471: Implementation of tolerance-based comparisons
- Issue #468: Python 3.9 Linux test failures
- Issue #469: Windows build issues