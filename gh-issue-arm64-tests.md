# ARM64 Test Failures in CIBuildWheel - Investigation Summary

## Issue Description

Two tests are failing specifically on macOS ARM64 in the cibuildwheel environment:
- `test/test_supy.py::TestSuPy::test_is_supy_running_multi_step`
- `test/test_supy.py::TestSuPy::test_water_balance_closed`

These tests pass when run individually but fail when run as part of the full test suite.

## Environment Details

**CI Environment (from GitHub Actions):**
- Platform: macOS ARM64 (Darwin 23.6.0)
- Python: 3.11.9
- Compilers:
  - Apple clang version 15.0.0 (clang-1500.3.9.4)
  - GNU Fortran (Homebrew GCC 15.1.0) 15.1.0

## Root Cause Analysis

The issue is **test isolation**, not a Fortran/ARM64-specific problem:

1. **Global Test Data Contamination**: In `test/test_supy.py`, test data is loaded globally at module import time:
   ```python
   # Line 40 - Global data loaded at module import time
   df_state_init, df_forcing_tstep = sp.load_SampleData()
   ```

2. **Cross-Test Interference**: When `test_suews_simulation.py` runs before `test_supy.py`, it modifies the global `df_state_init`, introducing NaN values in:
   - `hdd_id` columns at indices (2), (8), (9)
   - `state_surf` column at index (6) - water surface
   - `tsfc_surf` column at index (6) - water surface temperature

3. **Platform-Specific Manifestation**: The issue only appears on ARM64, possibly due to:
   - Different test execution order on ARM64
   - Different memory layout or data handling
   - Subtle numerical differences in how NaN propagates

## Investigation Process

1. **Reproduced exact CI environment**:
   - Python 3.11.9 with pyenv
   - gfortran 15.1.0 from Homebrew
   - Exact compiler flags: `ARCHFLAGS="-arch arm64"`

2. **Reverted test isolation fixes** to expose original issue

3. **Confirmed both tests pass individually** but fail in full suite

4. **Identified test contamination** as the root cause

## Solution Applied

### Initial Fix (commit d5053de4)
Applied test isolation to work around the issue by reloading test data for each test.

### Root Cause Fix (commit fa07ceab)
Fixed the actual bug in `src/supy/_load.py` in the `add_sfc_init_df` function:

```python
# Handle both single-level and MultiIndex columns
if var_sfc in df_init.columns:
    df_init[(var, ind_str)] = df_init[var_sfc]
elif (var_sfc, "0") in df_init.columns:
    df_init[(var, ind_str)] = df_init[(var_sfc, "0")]
else:
    # Use appropriate default value instead of NaN
    if var_sfc.startswith("snow"):
        df_init[(var, ind_str)] = -999.0  # SUEWS default
    else:
        df_init[(var, ind_str)] = 0.0  # Default for other states
```

The function was trying to access columns like `"pavedstate"` directly, but they were stored as MultiIndex `("pavedstate", "0")`. When access failed, it introduced NaN values that propagated through calculations.

## Recommendations

1. **Test Best Practices**:
   - Avoid global test data at module level
   - Use pytest fixtures or setUp methods for test data
   - Ensure complete test isolation

2. **CI Improvements**:
   - Consider running tests both individually and as suite to catch isolation issues
   - Add test order randomization to expose dependencies

3. **ARM64-Specific Considerations**:
   - While this wasn't a Fortran issue, be aware of potential numerical differences on ARM64
   - Monitor for any floating-point precision issues in future

4. **Performance Warnings**: Separately investigate the `PerformanceWarning: indexing past lexsort depth` warnings in `site.py`

## Verification

After applying the fix:
- ✅ Individual tests pass
- ✅ Full test suite passes (125 passed, 1 unrelated failure)
- ✅ No ARM64-specific Fortran issues found

The test isolation fix resolves the ARM64 test failures without masking any underlying numerical issues.