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

## Progress Tracking

### Issue #486: Documentation Fixes
- [ ] Fix df_output reformatting in quickstart example
- [ ] Change 'Runoff' to 'RO' in key variables list
- [ ] Test the corrected example to ensure it works
- [ ] Update any other related documentation if needed

### Issue #482: Sample Data Validation
- [ ] Investigate why check_state fails on load_sample_data
- [ ] Identify missing parameters in validation rules
- [ ] Fix daywat validation issue (tuple vs expected values)
- [ ] Update either sample data or validation rules for consistency
- [ ] Test that fresh install runs without warnings

### Testing
- [ ] Run the quickstart example end-to-end
- [ ] Verify load_sample_data passes check_state without warnings
- [ ] Run full test suite to ensure no regressions

## Key Decisions
- Fix both issues together as they impact the same user journey (getting started)
- Prioritise minimal changes that maintain backward compatibility

## Implementation Notes
- The quickstart documentation is likely in docs/source/workflow.rst or similar
- The sample data validation involves src/supy/_load.py and validation rules
- Need to check if STEBBS parameters are supposed to be in sample data

## Files to Modify
- Documentation files for quickstart example
- `src/supy/_load.py` - load_sample_data function
- `src/supy/_check.py` - check_state function and validation rules
- Sample data files if needed
- Test files to verify fixes