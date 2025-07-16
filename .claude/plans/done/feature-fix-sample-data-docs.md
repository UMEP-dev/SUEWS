# Feature: Fix Sample Data and Documentation Issues

## Context
This feature addresses two related user experience issues:
1. Documentation errors in the quickstart example (Issue #486)
2. Inconsistency between check_state and load_sample_data (Issue #482)

Both issues affect new users trying to get started with SUEWS/SuPy.

## GitHub Issues
- #486 - Two errors in Quickstart example (PRIMARY)
- #482 - Inconsistency between check_state and load_sample_data

## Lead Developer
tingsun

## Created
2025-07-16

## Completed
2025-07-16

## Progress Tracking

### Issue #486: Documentation Fixes
- [x] Fix df_output reformatting in quickstart example
- [x] Change 'Runoff' to 'RO' in key variables list
- [x] Test the corrected example to ensure it works
- [x] Update any other related documentation if needed

### Issue #482: Sample Data Validation
- [x] Investigate why check_state fails on load_sample_data
- [x] Identify missing parameters in validation rules
- [x] Fix daywat validation issue (tuple vs expected values)
- [x] Update either sample data or validation rules for consistency
- [x] Test that fresh install runs without warnings

### Testing
- [x] Run the quickstart example end-to-end
- [x] Verify load_sample_data passes check_state without warnings
- [x] Run full test suite to ensure no regressions

## Key Decisions
- Fix both issues together as they impact the same user journey (getting started)
- Prioritise minimal changes that maintain backward compatibility
- Silently filter STEBBS parameters (not ready for use) and internal metadata

## Implementation Notes
- The quickstart documentation is likely in docs/source/workflow.rst or similar
- The sample data validation involves src/supy/_load.py and validation rules
- Need to check if STEBBS parameters are supposed to be in sample data

## Files Modified
- `docs/source/workflow.rst` - Fixed the quickstart example code
- `src/supy/_check.py` - Improved validation to silently ignore STEBBS and metadata

## Outcome
**COMPLETED via PR #499** (MERGED)

### Summary of Changes:
1. **Documentation fixes** - Added proper MultiIndex handling with `droplevel('grid')`
2. **Variable name fix** - Changed 'Runoff' to 'RO'
3. **Validation improvements** - Silently filter STEBBS parameters and metadata columns
4. **User experience** - Significantly reduced validation noise

Both issues #486 and #482 have been successfully resolved.