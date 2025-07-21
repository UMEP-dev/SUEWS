# RSL Interpolation Fix for Short Buildings

## Issue #419
When running SUEWS with `stabilitymethod=2` (RSL) and `diagmethod=0` (MOST) with short buildings (e.g., 10m), the model crashes with a Fortran runtime error:
```
Index '0' of dimension 1 of array 'z' below lower bound of 1
```

## Root Cause
The `interp_z` function in `suews_phys_rslprof.f95` uses Fortran's `MAXLOC` and `MINLOC` functions with masks to find interpolation indices. When all values in the height array are above the interpolation point (e.g., trying to interpolate at 2m when all heights are ≥10m), these functions return 0, which is then used as an array index, causing the error.

## Solution
1. **Enhanced `interp_z` function** (lines 1244-1275): Added proper handling for edge cases:
   - When `idx_low == 0`: Extrapolate using first two points
   - When `idx_high == 0`: Extrapolate using last two points
   - Added checks for identical z values to avoid division by zero

2. **Consistent array initialization**: Applied fix to RSLProfile_DTS:
   ```fortran
   zarray(1) = MIN(zH_RSL*.01, 1.999) ! guaranttee 2 m is within the zarray
   ```

3. **Updated documentation**: Clarified that minimum canyon height is 2.0m to ensure proper interpolation at diagnostic heights.

4. **Code cleanup**: Removed duplicate RSLProfile subroutine as only RSLProfile_DTS (derived type version) is used.

## Testing
Created test cases for:
- 10m buildings (original reported issue)
- 2m buildings (extreme case)
- 50m buildings (control case)

All tests pass with the fix applied.

## Files Modified
- `src/suews/src/suews_phys_rslprof.f95`: 
  - Fixed interpolation function to handle edge cases
  - Applied array initialization fix in RSLProfile_DTS
  - Removed duplicate RSLProfile subroutine (only RSLProfile_DTS is used)
- `src/suews/src/suews_ctrl_driver.f95`:
  - Removed unused RSLProfile from module imports
  - Updated debug message to reference RSLProfile_DTS