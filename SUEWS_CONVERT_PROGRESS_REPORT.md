# SUEWS Convert Progress Report - 2016a to YAML Conversion Issues

## Date: 2025-08-07

## Initial Problem
The `suews-convert` command was failing when converting from 2016a format to 2025a (YAML) format with error:
```
Failed to load SUEWS tables: -999
```

## Investigation Summary

### Root Causes Identified

1. **Footer Lines Issue**: Legacy footer lines (`-9 -999 -999...`) were being added during conversion and then incorrectly parsed as data rows
2. **Debug Mode Not Working**: The `-d` debug directory option wasn't functioning properly
3. **Non-existent Profile References**: Conversion rules were adding references to profile codes that don't exist

## Fixes Implemented

### 1. Debug Directory Functionality (✅ FIXED)
**Location**: `src/supy/util/_converter.py`

**Problems Fixed**:
- Debug directory wasn't being created
- Temporary directories were deleted even in debug mode  
- Debug option wasn't properly exposed in CLI

**Changes Made**:
- Added debug directory creation with `parents=True`
- Preserved intermediate files when debug_dir is specified
- Added snapshot functionality to save each conversion step
- Exposed `-d` option in both `table_converter.py` and `to_yaml.py`

### 2. Legacy Footer Lines (✅ FIXED)
**Location**: `src/supy/util/_converter.py` (lines 428-430)

**Problem**: The `add_var` function was adding footer lines:
```python
# OLD CODE (REMOVED):
f.write("-9 " + " ".join(["-999"] * (len(headers) - 1)) + "\n")
f.write("-9 " + " ".join(["-999"] * (len(headers) - 1)) + "\n")
```

**Solution**: Removed footer line generation completely. These are legacy format requirements not needed in modern versions.

### 3. Non-existent Profile References (⚠️ PARTIALLY FIXED)
**Location**: `src/supy/util/rules.csv`

**Problem**: Conversion rules were adding profile code references that don't exist:
- Codes 801-805 (for ESTM building classes)
- Codes 44, 45 (for EnergyUseProfWD/WE)
- Codes 55663, 55664 (for ActivityProfWD/WE)
- Codes 701, 702 (for TraffProfWD/WE)

**Partial Fix Applied**:
Changed the following to use profile 999 (placeholder):
- PopProfWD/WE: 801,802 → 999
- Code_ESTMClass_Bldgs1-5: 801-805 → 999
- EnergyUseProfWD/WE: 44,45 → 999
- ActivityProfWD/WE: 55663,55664 → 999
- TraffProfWD/WE: 701,702 → 999

## Remaining Issues

### 1. More Profile References Need Fixing
The conversion still fails with error:
```
KeyError: "None of [Index([31, 31, 31], dtype='int64', name='Code')] are in the [index]"
```

This indicates there are more profile codes being referenced that don't exist. A systematic review of all profile references in `rules.csv` is needed.

### 2. Design Issue in Conversion Rules
**Fundamental Problem**: The conversion rules in `src/supy/util/rules.csv` are adding profile field references without ensuring the referenced profiles exist.

**Potential Solutions**:
1. **Option A**: Create placeholder profiles for all referenced codes
2. **Option B**: Update ALL conversion rules to only reference existing codes
3. **Option C**: Make the loading process more tolerant of missing profiles during conversion
4. **Option D**: Add a validation step that creates missing profiles automatically

## Test Case Used
```bash
suews-convert -i test/fixtures/legacy_format/2016a -f 2016a -o test_conv-x.yml -t 2025a -d debug_conversion
```

## Files Modified
1. `src/supy/util/_converter.py` - Fixed debug mode and removed footer lines
2. `src/supy/cmd/table_converter.py` - Added debug option to CLI
3. `src/supy/cmd/to_yaml.py` - Added debug option support
4. `src/supy/util/rules.csv` - Partially updated profile references
5. `.gitignore` - Added debug files to ignore list
6. `README.md` - Added minimal README to fix build issue
7. `CHANGELOG.md` - Updated with maintenance entries

## Debug Output Structure
When using `-d debug_conversion`, the following directory structure is created:
```
debug_conversion/
├── step_2016a_to_2017a/     # First conversion step
├── step_2017a_to_2018a/     # Second conversion step
├── ...                       # More conversion steps
├── step_2024a_to_2025a_final/ # Final conversion step
├── temp1/                    # Working directory 1
└── temp2/                    # Working directory 2
```

## Next Steps

### Immediate Actions Needed
1. **Comprehensive Profile Audit**: List all profile codes referenced in rules.csv
2. **Profile Existence Check**: Verify which profiles exist vs. which are missing
3. **Systematic Fix**: Either create missing profiles or update all references

### Long-term Improvements
1. **Validation Layer**: Add pre-conversion validation to check profile references
2. **Profile Management**: Create a profile registry system
3. **Better Error Messages**: Provide clearer errors about missing profiles
4. **Test Coverage**: Add tests for conversion from all supported versions

## Key Learning Points

1. **Legacy Format Complexity**: The SUEWS table format has evolved significantly, with many fields added/removed across versions
2. **Profile Reference System**: The profile system relies on codes that must exist in SUEWS_Profiles.txt
3. **Conversion Chain**: Converting from 2016a → 2025a involves 11 intermediate steps, each potentially introducing issues
4. **Footer Lines**: Legacy `-9` footer lines should not be preserved in modern conversions

## Commit Information
- Branch: `fix/566-to-yaml-module`
- Commit: `f7ddc443` 
- Message: "fix: Partial fix for suews-convert 2016a to YAML conversion issues"

## Related GitHub Issue
This work relates to issue #566 regarding the YAML module conversion functionality.

---

*Report generated by Claude Code session on 2025-08-07*