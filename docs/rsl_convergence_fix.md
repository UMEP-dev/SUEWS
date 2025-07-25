# RSL Convergence Fix for High z0/Low FAI Cases

This document describes the changes made to fix RSL (Roughness Sub-Layer) convergence issues in SUEWS, addressing GitHub issue #338.

## Background

The RSL model was experiencing convergence failures in cases with:
- High roughness length (z0) 
- Low frontal area index (FAI)
- Complex urban morphologies

The issue was identified in PR #297 which proposed changes to improve convergence but was closed without merging.

## Changes Implemented

### 1. Thread-Safe RSL-Specific Stability Functions

**File: `src/suews/src/suews_phys_rslprof.f95`**

Created RSL-specific wrapper functions that use `neut_limit_rsl = 0.0` instead of modifying the global `neut_limit`:

```fortran
! RSL module parameter
REAL(KIND(1D0)), PARAMETER :: neut_limit_rsl = 0.0D0

! RSL-specific stability functions
FUNCTION rsl_stab_psi_mom(StabilityMethod, zL) RESULT(psi)
   ! Uses neut_limit_rsl = 0.0 for neutral condition check
   IF (ABS(zL) <= neut_limit_rsl) THEN
      psi = 0
   ...
END FUNCTION
```

This approach is thread-safe and compatible with WRF-SUEWS coupling where multiple threads may execute SUEWS simultaneously.

### 2. Inclusive Neutral Stability Comparison

**File: `src/suews/src/suews_phys_atmmoiststab.f95`**

Changed all neutral stability comparisons from exclusive (`<`) to inclusive (`<=`):

```fortran
! Before:
IF (ABS(zL) < neut_limit) THEN

! After:
IF (ABS(zL) <= neut_limit) THEN
```

This ensures that the exact neutral case (zL = 0) is properly handled.

### 3. RSL-Specific Neutral Limit Implementation

**File: `src/suews/src/suews_phys_rslprof.f95`**

All stability function calls within the RSL module now use the RSL-specific versions:

```fortran
! Before:
psimz0 = stab_psi_mom(StabilityMethod, z0_RSL/L_MOD_RSL)

! After:
psimz0 = rsl_stab_psi_mom(StabilityMethod, z0_RSL/L_MOD_RSL)
```

### 4. UStar Limiting Condition

**File: `src/suews/src/suews_phys_rslprof.f95`**

The UStar limiting condition for convective cases continues to use the standard `neut_limit`:

```fortran
IF ((ZMeas - zd_RSL)/L_MOD_RSL < -neut_limit) UStar_RSL = MAX(0.15, UStar_RSL)
```

## Thread Safety Considerations

The implementation is designed to be thread-safe for WRF-SUEWS coupling:

1. **No Global State Modification**: The global `neut_limit` remains constant
2. **Local Function Scope**: RSL-specific functions use local parameters
3. **MPI/OpenMP Compatible**: Safe for parallel execution in WRF

## Scientific Justification

1. **Neutral Limit = 0 for RSL**: The RSL formulation benefits from a stricter definition of neutral conditions, preventing premature switching between stability regimes.

2. **Inclusive Comparison**: Including the exact neutral case (zL = 0) in the neutral regime improves numerical stability.

3. **Preserved Physics**: The changes maintain the physical basis of the model while improving numerical convergence.

## Testing Recommendations

1. **Convergence Tests**: Verify that RSL calculations converge for previously problematic cases (high z0, low FAI).

2. **Regression Tests**: Ensure that standard cases produce results consistent with previous versions.

3. **Stability Tests**: Test across a range of atmospheric stability conditions.

4. **Thread Safety Tests**: Verify correct behavior in parallel execution environments.

## Expected Improvements

- Better convergence for challenging urban morphology configurations
- More stable calculations near neutral stability conditions
- Consistent behaviour across different compiler optimizations
- No impact on existing well-behaved configurations
- Thread-safe execution in WRF-SUEWS coupled mode

## References

- GitHub Issue #338: RSL issue - high z0 for low FAI cases
- PR #297: Proposed RSL convergence improvements
- Harman & Finnigan (2007, 2008): RSL theory and implementation
- WRF-SUEWS coupling: Thread safety requirements for atmospheric models