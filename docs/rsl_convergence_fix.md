# RSL Convergence Fix for Issue #338

## Summary

This fix addresses convergence issues in the Roughness Sublayer (RSL) calculations when dealing with high roughness length (z0) and low frontal area index (FAI) scenarios, as reported in GitHub issue #338.

## Changes Made

### 1. `src/suews/src/suews_phys_atmmoiststab.f95`

- Changed `neut_limit` from a `PARAMETER` to a `SAVE` variable to allow flexibility in RSL calculations
- Changed all neutral stability comparisons from `<` to `<=` for better numerical stability
- This allows the RSL module to temporarily set different neutral limits during calculations

### 2. `src/suews/src/suews_phys_rslprof.f95`

- Added mechanism to temporarily set `neut_limit = 0` during RSL calculations
- This prevents premature neutral stability assumptions within the RSL profile
- Original `neut_limit` value is preserved and used at measurement height for UStar calculations
- Restored after RSL calculations complete

## Technical Details

### Problem Description

The original issue occurs when:
- High roughness length (z0) relative to low frontal area index (FAI)
- The RSL calculations would converge slowly or fail to converge
- This was particularly problematic in urban areas with tall, sparse buildings

### Solution Rationale

1. **Flexible neutral limit**: By making `neut_limit` a variable instead of a constant, the RSL calculations can use a different threshold internally while preserving the original behaviour at measurement height.

2. **Inclusive comparisons**: Changing from `<` to `<=` ensures that exact equality cases are handled consistently, preventing numerical instabilities at the neutral stability boundary.

3. **Temporary zero neutral limit in RSL**: Setting `neut_limit = 0` within RSL calculations prevents the code from assuming neutral conditions prematurely, allowing the iterative solver to converge properly.

## Testing

A comprehensive test suite has been created in `test/test_rsl_convergence.py` that:
- Tests high z0/low FAI scenarios that previously caused convergence issues
- Verifies neutral stability handling
- Compares RSL and MOST approaches
- Sweeps through various problematic parameter combinations

## Expected Improvements

- Better convergence for challenging urban morphology configurations
- More stable calculations near neutral stability conditions
- Consistent behaviour across different compiler optimizations
- No impact on existing well-behaved configurations

## References

- GitHub Issue #338: RSL problems with high z0 for low FAI cases
- PR #297: Original attempted fix (not merged)
- Harman & Finnigan (2007): Theoretical basis for RSL calculations