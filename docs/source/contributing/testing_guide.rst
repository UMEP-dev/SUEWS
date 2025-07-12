.. _testing_guide:

Testing Guide
=============

This document describes the pragmatic testing approach adopted for SUEWS (Surface Urban Energy and Water Balance Scheme) to handle numerical differences across platforms while ensuring scientific validity.

Background
----------

Scientific models like SUEWS face inherent challenges with exact reproducibility across different platforms and Python versions due to:

- Floating-point arithmetic differences between CPU architectures
- Compiler optimisation variations
- Library implementation differences (BLAS, LAPACK, etc.)
- Python/NumPy version changes

Rather than pursuing bit-for-bit reproducibility (which is often impractical), we adopt a **tolerance-based validation approach** that ensures results are scientifically valid across all supported platforms.

Testing Philosophy
------------------

Core Principles
***************

1. **Scientific validity over numerical exactness** - Results should be physically meaningful and consistent
2. **Tolerance-based comparison** - Accept small numerical differences within scientifically justified bounds
3. **Physical consistency checks** - Validate conservation laws and physical constraints
4. **Transparent reporting** - Clear documentation of why tolerances are acceptable
5. **Fast feedback** - Fail quickly before expensive operations

Implementation
--------------

Sample Output Validation
************************

The primary validation test compares model outputs against reference results with appropriate tolerances (``test/test_sample_output.py``).

**Tolerance Configuration:**

.. code-block:: python

   TOLERANCE_CONFIG = {
       # Energy fluxes - 0.8% relative tolerance
       "QN": {"rtol": 0.008, "atol": 0.1},   # Net all-wave radiation [W/m²]
       "QF": {"rtol": 0.008, "atol": 0.1},   # Anthropogenic heat flux [W/m²]
       "QS": {"rtol": 0.008, "atol": 0.1},   # Storage heat flux [W/m²]
       "QE": {"rtol": 0.008, "atol": 0.1},   # Latent heat flux [W/m²]
       "QH": {"rtol": 0.008, "atol": 0.1},   # Sensible heat flux [W/m²]
       
       # Meteorological variables
       "T2": {"rtol": 0.002, "atol": 0.01},  # 2m air temperature [°C]
       "RH2": {"rtol": 0.010, "atol": 0.5},  # 2m relative humidity [%]
       "U10": {"rtol": 0.005, "atol": 0.01}, # 10m wind speed [m/s]
   }

Scientific Justification
************************

**Energy Fluxes (0.8% tolerance):**

- Eddy covariance measurements typically have 5-10% uncertainty
- Model structural uncertainty is comparable
- 0.8% tolerance is well within measurement uncertainty
- Ensures energy balance closure within acceptable bounds

**Temperature (0.2% tolerance):**

- Modern temperature sensors: ±0.1-0.2°C accuracy
- 0.2% relative tolerance for typical urban temperatures
- More stringent due to high sensor accuracy

**Relative Humidity (1% tolerance):**

- RH sensors typically: ±2-3% accuracy
- 1% tolerance is conservative
- Accounts for nonlinear RH calculations

**Wind Speed (0.5% tolerance):**

- Anemometer accuracy: ±0.1-0.2 m/s
- 0.5% tolerance for typical urban wind speeds
- Important for turbulent exchange calculations

CI/CD Integration
*****************

**Fast-Fail Validation**

The GitHub Actions workflow runs sample validation **before** expensive wheel building:

.. code-block:: yaml

   jobs:
     validate_sample_output:
       name: Validate Sample Output
       strategy:
         matrix:
           os: [ubuntu-latest, macos-latest, windows-latest]
           python-version: ["3.9", "3.10", "3.11", "3.12", "3.13"]
         fail-fast: true
       steps:
         # ... build and test ...
         
     build_wheels:
       needs: validate_sample_output  # Only runs if validation passes
       # ... expensive wheel building ...

Benefits:

- Saves ~30 minutes per failed build
- Immediate feedback on breaking changes
- Tests all supported Python versions

**Debug Artifacts**

When tests fail in CI, comprehensive artifacts are saved:

- Input data (state, forcing)
- Model output
- Reference data
- Detailed comparison report
- Platform information

This enables offline debugging without re-running simulations.

Custom Comparison Framework
***************************

Instead of relying on pandas testing utilities (which vary between versions), we implement custom NumPy-based comparison:

.. code-block:: python

   def compare_arrays_with_tolerance(actual, expected, rtol, atol, var_name):
       """Compare arrays using same logic as numpy.allclose"""
       abs_diff = np.abs(actual - expected)
       rel_diff = abs_diff / (np.abs(expected) + np.finfo(float).eps)
       within_tol = (abs_diff <= atol) | (rel_diff <= rtol)
       # ... detailed reporting ...

This ensures consistent behaviour across all platforms and library versions.

Physical Validation Tests
-------------------------

Beyond numerical comparison, additional tests validate physical consistency:

Energy Balance Closure
**********************

.. code-block:: python

   # Energy balance should close within measurement uncertainty
   Rn = QN  # Net radiation
   EB = QH + QE + QS + QF  # Energy balance components
   closure_error = abs(Rn - EB) / abs(Rn)
   assert closure_error < 0.05  # 5% closure typical for measurements

Water Balance Closure
*********************

.. code-block:: python

   # Water should be conserved
   P + I = E + R + ΔS  # Precipitation + Irrigation = Evap + Runoff + Storage change

Physical Bounds
***************

- 0 ≤ RH ≤ 100%
- Albedo: 0 ≤ α ≤ 1
- Temperature: within reasonable bounds for location/season
- Energy fluxes: sign conventions respected

Platform-Specific Considerations
--------------------------------

While the base tolerances work across platforms, the framework supports platform-specific adjustments if needed:

.. code-block:: python

   PLATFORM_ADJUSTMENTS = {
       "darwin-arm64": {
           "QN": {"rtol": 0.010}  # Slightly relaxed for ARM64 if needed
       }
   }

Best Practices for Developers
-----------------------------

1. **Run tests locally** before pushing:

   .. code-block:: bash

      pytest test/test_sample_output.py -v

2. **Check tolerance reports** - Even passing tests show maximum differences

3. **Update reference data carefully** - Document why changes are needed

4. **Add physical validation** for new features

5. **Document assumptions** in code comments

Troubleshooting Test Failures
-----------------------------

Debugging Failed Tests
**********************

1. **Download CI artifacts** from failed runs
2. **Load in Python**:

   .. code-block:: python

      import pandas as pd
      actual = pd.read_pickle('output_*.pkl')
      expected = pd.read_pickle('sample_reference_*.pkl')

3. **Analyse differences** - Are they physically meaningful?
4. **Check platform info** - Different architecture/library versions?

Common Issues
*************

- **Length mismatches**: Usually timestamp/calendar issues
- **Systematic bias**: May indicate algorithm changes
- **Random failures**: Possible memory/initialisation issues
- **Platform-specific**: Often related to math library differences

Future Enhancements
-------------------

1. **Statistical validation** - Compare distributions rather than point values
2. **Ensemble testing** - Run multiple configurations
3. **Performance benchmarks** - Track computational efficiency
4. **Automated tolerance tuning** - Based on sensitivity analysis

References
----------

1. `Numerical Reproducibility in Geoscientific Computing <https://doi.org/10.5194/gmd-2021-203>`_
2. `Best Practices for Scientific Computing <https://doi.org/10.1371/journal.pbio.1001745>`_
3. `Uncertainty in Eddy Covariance Measurements <https://doi.org/10.1016/j.agrformet.2012.09.004>`_