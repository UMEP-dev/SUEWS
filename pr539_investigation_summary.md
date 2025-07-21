# PR539 Investigation Summary: RSL Interpolation and Height Array Issues

## Overview

PR #539 was created to fix RSL interpolation crashes for short buildings (Issue #419). During testing and investigation, a deeper issue was discovered: non-monotonic height arrays in the MOST diagnostic method output.

## Original Issue (#419)

**Problem**: SUEWS crashed with "Index '0' of dimension 1 of array 'z' below lower bound of 1" when using RSL with short buildings.

**Root Cause**: The `interp_z` function in `suews_phys_rslprof.f95` used `MAXLOC` which returns 0 when the interpolation point is below all array values, causing invalid array access in Fortran.

**PR #539 Fix**: Added extrapolation logic to handle cases where `idx_low == 0` or `idx_high == 0`.

**Critical Insight**: This fix is actually a workaround that masks the real problem. The interpolation function should NEVER need to extrapolate. If interpolation is requested outside the height array bounds, it indicates a fundamental issue with either:
- The height array not covering the required diagnostic heights (2m, 10m)
- The height array generation being incorrect (e.g., non-monotonic or insufficient coverage)

**Proper Fix**: Instead of allowing extrapolation, the code should ensure the height array always covers all required diagnostic heights properly.

## Newly Discovered Issue: Non-Monotonic Height Arrays

### Symptoms
When using MOST diagnostic method (`DiagMethod=0`), the output height array shows:
- Levels 1-20: Constant value (e.g., 16.261m for 10m building)
- Level 21: Drops to a lower value (e.g., 11.272m)
- Levels 22-30: Increase monotonically

### Root Cause Analysis

1. **Problematic Code Line** (line ~411 in original):
   ```fortran
   IF (zarray(z) <= zd_RSL) zarray(z) = 1.01*(zd_RSL + z0_RSL)
   ```

2. **The Issue**:
   - RSL scheme calculates `zd_RSL` that can exceed building height (physically realistic for RSL)
   - MOST section tries to "correct" heights below `zd_RSL` by setting them to `1.01*(zd_RSL + z0_RSL)`
   - This creates constant values for the first 20 levels
   - When the logarithmic MOST heights kick in at level 21, they start below this constant, creating non-monotonicity

3. **Underlying Problem**: 
   - RSL and MOST logic are intermingled in the same subroutine
   - Height array initialization is shared between fundamentally different schemes
   - RSL's displacement height concept is incompatible with MOST's height requirements

### Test Results

#### Building Height Tests (DiagMethod=0 - MOST):
- 10m building: Constant 16.261m for levels 1-20
- 5m building: Constant 8.130m for levels 1-20
- 2m building: Constant 3.252m for levels 1-20

All show the same pattern: constant values followed by a drop at level 21.

#### Key Finding:
The constant value formula `1.01*(zd_RSL + z0_RSL)` where:
- For 10m building: zd_RSL ≈ 15.268m, z0_RSL ≈ 0.854m
- Result: 1.01 × (15.268 + 0.854) = 16.283m ≈ 16.261m (observed)

## Proposed Solution

### Separate RSL and MOST Height Array Logic

1. **Clear Separation**: Create distinct height initialization for RSL vs MOST
2. **Remove Problematic Line**: Delete the `IF (zarray(z) <= zd_RSL)` correction
3. **MOST-Specific Heights**: Use proper logarithmic distribution starting above surface

### Conceptual Structure:
```fortran
IF (flag_RSL) THEN
    ! RSL height initialization (existing logic)
    ! Heights within and above canopy
ELSE
    ! MOST height initialization
    ! Pure logarithmic distribution above zdm + z0m
END IF
```

## Test Scripts Created

1. **test_rsl_simple.py**: Discovered constant values in height array
2. **test_height_monotonic.py**: Confirms non-monotonic behaviour
3. **analyze_16261m.py**: Traced constant value to specific code line
4. **analyze_most_heights.py**: Analysed diagnostic height relationships
5. **test_fortran_debug.f90**: Verified logarithmic height formula
6. **PR539_RSL_Analysis.ipynb**: Comprehensive analysis notebook

## Key Insights

1. **PR #539 successfully fixes the interpolation crash** but doesn't address the height array issue
2. **The height array issue is a separate, deeper problem** in how RSL and MOST schemes are mixed
3. **Physical inconsistency**: RSL's displacement height (can exceed building height) is incompatible with MOST's assumptions
4. **Code structure issue**: Single subroutine trying to handle two fundamentally different approaches

## Proper Solution Approach

### Remove Extrapolation Workaround
1. **Replace extrapolation with error checking**: The `interp_z` function should stop with a clear error message if interpolation is requested outside bounds
2. **Fix root causes**: Ensure height arrays properly cover all diagnostic heights

### Height Array Requirements
For MOST approach:
- Must start at or below 2m diagnostic height (typically at z0m + small offset)
- Must extend above 10m diagnostic height
- Must be monotonically increasing
- Should use appropriate logarithmic spacing

For RSL approach:
- Must cover both within-canopy and above-canopy regions
- Must include diagnostic heights (2m, 10m)
- Must be monotonically increasing throughout

### Code Structure Fix
- Completely separate RSL and MOST height array generation
- Remove any "correction" logic that creates non-monotonic arrays
- Ensure each approach generates physically consistent heights

## Height Array Generation Analysis (from suews_phys_rslprof.f95)

### How zarray is Determined (Lines 274-310)

The height array is generated in three distinct parts:

1. **Minimum Canyon Height (Line 278)**:
   ```fortran
   zH_RSL = MAX(Zh, 2.)  ! Minimum 2m to avoid insane values
   ```

2. **Within Canopy Heights (Lines 281-298)**:
   - 20 levels total (`nz_can = 20`)
   - Split into two sub-sections:
     
   **Lower Half (Levels 1-10)**: Surface to half canyon height
   ```fortran
   zarray(1) = MIN(zH_RSL*.01, 1.999)  ! Guarantee 2m is within array
   zarray(10) = zH_RSL*.5
   ```
   - Densifies near the surface using backward calculation
   
   **Upper Half (Levels 11-20)**: Half canyon to canyon top
   ```fortran
   DO i = 11, nz_can
       zarray(i) = zarray(i - 1) + dz_can*.5
       dz_can = zH_RSL - zarray(i)
   END DO
   ```
   - Densifies near the canyon top

3. **Above Canopy Heights (Lines 300-310)**:
   - 10 levels from canyon top to measurement height
   ```fortran
   zarray(nz) = zMeas  ! Top level at measurement height
   DO i = nz - 1, nz_can + 1, -1
       zarray(i) = zarray(i + 1) - dz_above*.3
       dz_above = zarray(i) - Zh_RSL
   END DO
   ```
   - Densifies near the canyon top

### The Problematic Correction (Lines 407-418)

When `flag_RSL = .FALSE.` (MOST approach), the code applies a correction:

```fortran
DO z = 1, nz_can
    IF (zarray(z) <= zd_RSL) zarray(z) = 1.01*(zd_RSL + z0_RSL)
    ! ... MOST calculations ...
END DO
```

**This is the root cause of the non-monotonic array!**

### Why This Causes Problems

1. **zd_RSL Calculation (Line 985)**:
   ```fortran
   zd_RSL = Zh_RSL/(1.-EXP(-Zh_RSL/((beta**2.)*Lc))) - (beta**2.)*Lc
   ```
   - For urban canopies, `zd_RSL` can exceed building height
   - This is physically valid for RSL but incompatible with MOST

2. **The Correction Creates Constants**:
   - When `zarray(z) <= zd_RSL`, it's replaced with `1.01*(zd_RSL + z0_RSL)`
   - For a 10m building: `zd_RSL ≈ 15.3m`, so levels 1-20 all become 16.26m
   - Level 21 (first above-canopy level) may be lower, creating non-monotonicity

3. **Diagnostic Height Coverage Failure**:
   - The 2m diagnostic height is never properly covered
   - The interpolation at lines 448-450 tries to compensate:
   ```fortran
   T2_C = interp_z(2D0 + zd_rsl + z0_rsl, zarray, dataoutLineTRSL)
   ```
   - This requests interpolation at ~18.3m for a 2m diagnostic!

### Key Finding: Mixed Logic Problem

The code tries to use RSL-derived parameters (`zd_RSL`, `z0_RSL`) even when using MOST diagnostics. This creates:
- Non-monotonic height arrays
- Diagnostic heights outside the array bounds
- Need for extrapolation (which PR #539 added)

## Solution Implemented

The issue has been successfully resolved by implementing a complete separation of RSL and MOST approaches:

### Implementation Details

1. **Refactored RSLProfile** as a gateway/router function that:
   - Determines which method to use (RSL vs MOST)
   - Calls appropriate calculation subroutine
   - No longer mixes parameters between approaches

2. **Created cal_profile_MOST** subroutine:
   - Pure MOST calculations without RSL parameters
   - Uses standard z0m/zdm (not RSL-derived values)
   - Generates monotonic height arrays via setup_MOST_heights

3. **Created cal_profile_RSL** subroutine:
   - Dedicated RSL calculations with Harman & Finnigan corrections
   - Uses RSL-specific parameters (zd_RSL, z0_RSL, etc.)
   - Maintains proper within-canopy and above-canopy profiles

4. **Created height array generation helpers**:
   - `setup_MOST_heights`: Generates logarithmic distribution for MOST
   - `setup_RSL_heights`: Generates within/above canopy levels for RSL
   - Both ensure coverage of diagnostic heights (2m, 10m)
   - No artificial corrections or parameter mixing

5. **Removed problematic code**:
   - Deleted line 411: `IF (zarray(z) <= zd_RSL) zarray(z) = 1.01*(zd_RSL + z0_RSL)`
   - This line was causing constant values and non-monotonic arrays

6. **Cleaned up function signatures**:
   - Removed unused arguments from both cal_profile_MOST and cal_profile_RSL
   - Improved code clarity and maintainability

7. **Achieved complete separation in RSLProfile**:
   - Single flag check determines the approach (no scattered conditionals)
   - All RSL-specific logic contained in RSL branch
   - All MOST-specific logic contained in MOST branch
   - Each approach is fully self-contained with its own:
     - Height array generation
     - Parameter calculations
     - Stability functions
     - Profile calculations

### Test Results

All tests now pass successfully:
- ✅ Height arrays are strictly monotonic for all building heights
- ✅ No constant value regions in height arrays
- ✅ Proper coverage of 2m and 10m diagnostic heights
- ✅ No parameter mixing between RSL and MOST approaches
- ✅ Works correctly for short buildings (2m, 5m, 10m) that previously failed
- ✅ Stable across multiple timesteps and diurnal cycles

### Impact

This fix properly addresses the root cause rather than working around it:
- No longer needs extrapolation in interp_z function
- Physically consistent height arrays for both approaches
- Clear separation of concerns between RSL and MOST
- More maintainable and understandable code structure

The original PR #539 extrapolation workaround can now be reconsidered, as the underlying issue that necessitated it has been resolved.