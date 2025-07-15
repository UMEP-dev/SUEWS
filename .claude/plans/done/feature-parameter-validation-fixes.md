# Feature: Parameter Validation Fixes

## Lead Developer
- **GitHub**: @sunt05
- **Started**: 2025-07-01

## Context
Fix parameter validation issues including false positive warnings for supplied parameters and site/sites configuration problems that allow the model to run without proper parameters. These are standalone issues in the YAML configuration system.

## GitHub Issues
- #444 - Fix parameter validation false positives and site/sites configuration issues (PRIMARY)
- #448 - PR addressing issue #444
- #453 - PR for annotated YAML documentation
- #400 - Conditional validation migration guide
- #454 - Config builder improvements (follow-up)

## Status
- **Current**: done
- **Outcome**: completed
- **Completed**: 2025-07-03
- **PR**: #448, #453

## Progress Tracking

### 1. Parameter Validation False Positives
- [x] Investigate current validation logic flow
- [x] Identify why supplied parameters trigger missing warnings (RefValue with None not detected)
- [x] Fix validation to properly detect supplied values
- [x] Add unit tests for parameter validation
- [x] Test with various parameter configurations

### 2. Site vs Sites Configuration Issue
- [x] Investigate how model can run with no parameters (validation was bypassed)
- [x] Trace the site/sites parameter handling (found bug: looking for 'site' instead of 'sites')
- [x] Add validation to ensure required parameters are present (fixed validation to use correct key)
- [x] Implement clear error messages for missing parameters (existing warnings now work correctly)
- [x] Handle both `site` and `sites` formats correctly (only 'sites' is valid per YAML spec)
- [x] Add integration tests for configuration validation (verified fix works)

### 3. False Positives for Actually Supplied Parameters (New Issue)
- [x] Investigate bldgh parameter validation flow
- [x] Trace when validation occurs vs when YAML parameters are loaded
- [x] Check if validation happens on default values before YAML merge
- [x] Verify parameter name mapping between YAML and internal model
- [x] Create test demonstrating correct vs incorrect parameter placement
- [x] Confirm validation is working correctly

**Root Cause Identified**: 
- This is NOT a false positive - the validation is working correctly
- The issue is user error: placing parameters at the wrong YAML level
- Users are placing `bldgh: 15.0` at the site level
- It should be nested under `properties.land_cover.bldgs.bldgh.value: 15.0`
- The validation correctly reports the parameter as missing from its expected location
- Test confirms: correct placement eliminates the warning

### 4. Documentation ⚠️ PARTIALLY COMPLETED
- [x] Document proper parameter configuration (via annotated YAML)
- [ ] Add troubleshooting guide for validation warnings
- [ ] Update error message documentation

### 5. Test Simplification (COMPLETED)
- [x] Removed obsolete test_validation_warnings.py
- [x] Updated test_spurious_warnings.py for new approach
- [x] Created test_parameter_validation_simplified.py (renamed to test_validation_topdown.py)
- [x] Simplified YAML annotator output format
- [x] Created migration guide for Silvia (issue #400)

### 6. Message Clarity Improvements (COMPLETED)
- [x] Updated validation messages to indicate file "will shortly be generated"
- [x] Fixed message flow to be a natural sentence
- [x] Updated tests to match new message format

### 7. GitHub Updates (COMPLETED)
- [x] Posted detailed comment on issue #444 with fix explanation
- [x] Posted migration guide comment on issue #400 for Silvia
- [x] Posted friendly note on PR #446 referring to migration guide
- [x] Created PR #448 to address issue #444

## Key Decisions
- Validation strategy (fail-fast vs collect all errors)
- Warning vs error thresholds for missing parameters
- Backward compatibility considerations

## Implementation Notes

### RefValue Validation Fix
- Fixed RefValue validation: check_missing_params now properly detects RefValue objects with None values
- The issue was that RefValue(value=None) wasn't being detected as missing
- Solution: Check both if value is None and if it's a RefValue with value=None
- Added comprehensive test coverage for all validation scenarios

### Site/Sites Configuration Fix
- Fixed validation code that was looking for 'site' (singular) instead of 'sites' (plural)
- All YAML configurations use 'sites' key, but validation was looking for wrong key
- This caused site-specific parameter validation to be completely bypassed
- Fixed in both validation.py and validation_controller.py

### New Feedback: False Positives for Supplied Parameters
- **Issue reported**: Model reports parameters as missing (e.g., bldgh) even when they are correctly supplied in YAML
- **Symptom**: Model runs with correct values but still generates missing parameter warnings
- **This suggests**: The validation check happens at a different stage than parameter loading
- **Possible causes**:
  - Validation runs before YAML parameters are merged into the data model
  - Parameter aliasing or naming mismatches between YAML and internal model
  - Timing issue where validation happens on initial/default values before YAML override
  - Different parameter paths for validation vs actual usage

## Files to Investigate
- `src/supy/data_model/` - Data model validation logic
- `src/supy/_precheck.py` - Pre-run validation checks
- `src/supy/_check.py` - Runtime validation
- Parameter warning generation code

## Testing Strategy
- Unit tests for individual validation functions
- Integration tests with various configurations
- Test both valid and invalid parameter combinations
- Verify error messages are helpful and accurate

## Current Status
Completed:
- ✅ RefValue validation now properly detects None values
- ✅ Site/sites configuration key bug has been corrected
- ✅ Investigated "false positive" for bldgh - found to be user error (incorrect YAML structure)
- ✅ Created test demonstrating correct parameter placement
- ✅ Implemented top-down validation approach (all validation at SUEWSConfig level)
- ✅ Simplified YAML annotations for better readability
- ✅ Improved validation message clarity
- ✅ Posted migration guide for conditional validation work
- ✅ Created PR #448 addressing issue #444

## Summary of Findings
1. The parameter validation is working correctly
2. The reported "false positives" are actually correct warnings - parameters are truly missing from their expected locations
3. Users need to place parameters at the correct YAML nesting level
4. Documentation improvements needed to guide users on proper YAML structure
5. Top-down validation approach successfully eliminates spurious warnings during component creation

## Next Steps
- ✅ PR #448 merged successfully
- ✅ Created PR #453 for annotated YAML documentation
- ✅ Config builder improvements moved to separate issue #454
- Documentation tasks for parameter configuration guide still pending

## Final Status
This feature branch accomplished its primary goals:
1. Fixed parameter validation false positives (RefValue with None)
2. Fixed site/sites configuration key bug
3. Clarified that reported "false positives" were actually user errors
4. Implemented top-down validation approach
5. Created helpful YAML annotation system

Additional work on config builder improvements has been moved to a new feature branch.