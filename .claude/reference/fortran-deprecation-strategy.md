# Fortran Output Variable Deprecation Strategy

## Problem

Previously, output variable definitions were duplicated in two places:
1. **Fortran** (`src/suews/src/suews_ctrl_output.f95`): ~1200 lines of DATA statements
2. **Python** (`src/supy/data_model/output/`): Pydantic models with 528 variables

This duplication created maintenance burden and risk of drift between Fortran and Python.

## Solution: Python as Single Source of Truth

We've implemented automatic Fortran code generation from the Python OUTPUT_REGISTRY, making Python the single source of truth.

### Architecture

```
Python Pydantic Models (OUTPUT_REGISTRY)
    ↓
generate_varlist.py (generator script)
    ↓
varlist_generated.f95 (auto-generated Fortran)
    ↓
suews_ctrl_output.f95 (includes generated file)
    ↓
Fortran compilation
```

### Implementation

**1. Generator Script** (`src/suews/src/generate_varlist.py`)
- Reads `OUTPUT_REGISTRY` from Python
- Generates Fortran DATA statements matching original structure
- Creates `varlist_generated.f95` with all 528 variables
- Organized by group (datetime, SUEWS, snow, ESTM, RSL, DailyState, BL, BEERS, debug)

**2. Fortran File Modification** (`src/suews/src/suews_ctrl_output.f95`)
- Replaced ~1200 lines of manual DATA statements with single INCLUDE directive
- Reduced file from 2566 lines to 1368 lines (-46%)
- Preserves exact same structure and behaviour
- Added comments about experimental groups not yet migrated

**3. Build System Integration** (`src/suews/Makefile`)
- Added rule to generate `varlist_generated.f95` before compiling `suews_ctrl_output.o`
- Automatic regeneration when `generate_varlist.py` changes
- Fallback to existing file if generation fails
- Clean target removes generated files

```makefile
# Generate varlist from Python before compilation
src/varlist_generated.f95: src/generate_varlist.py
	python3 src/generate_varlist.py

src/suews_ctrl_output.o: src/suews_ctrl_output.f95 src/varlist_generated.f95
	$(FC) $(FCFLAGS) -c $< -o $@
```

## Current Status

### Migrated to Python (528 variables)
- ✅ datetime (5 vars)
- ✅ SUEWS (85 vars)
- ✅ snow (98 vars)
- ✅ ESTM (27 vars)
- ✅ RSL (135 vars)
- ✅ DailyState (47 vars)
- ✅ BL (17 vars)
- ✅ BEERS (29 vars)
- ✅ debug (85 vars)

### Not Yet Migrated (Fortran-only)
- ⏳ SPARTACUS (experimental radiation model)
- ⏳ EHC (experimental heat capacity model)
- ⏳ STEBBS (experimental energy balance model)
- ⏳ NHood (neighbourhood iteration diagnostics)

These experimental groups are conditionally compiled and rarely used. They can be migrated following the same pattern when needed.

## Benefits

### Eliminated Duplication
- No more manual maintenance of Fortran DATA statements
- Python is the single source of truth
- Automatic synchronisation between Python and Fortran

### Improved Maintainability
- Add new variables in Python only
- Type-safe with Pydantic validation
- Self-documenting with rich metadata
- Easier to extend and modify

### Build Integration
- Automatic generation at build time
- No manual intervention required
- Fallback mechanism for robustness

### Code Reduction
- Removed ~1200 lines of Fortran DATA statements
- Net reduction: 46% in `suews_ctrl_output.f95`
- Simpler codebase to maintain

## Usage

### Adding New Variables

1. Add variable definition in Python:
```python
# In src/supy/data_model/output/suews_vars.py (or appropriate module)
OutputVariable(
    name="NewVar",
    unit="W m-2",
    description="Description of new variable",
    aggregation=AggregationMethod.AVERAGE,
    group=OutputGroup.SUEWS,
    level=OutputLevel.DEFAULT,
    format="f104",
)
```

2. Build system automatically:
   - Regenerates `varlist_generated.f95`
   - Includes new variable in Fortran code
   - No manual Fortran changes needed

### Manual Regeneration

If needed, regenerate manually:
```bash
cd src/suews/src
python3 generate_varlist.py
```

### Verification

Check generated file:
```bash
cat src/suews/src/varlist_generated.f95 | head -50
```

## Testing

The implementation includes:
- Standalone test script (`test_output_models.py`)
- Build-time generation verification
- Backward compatibility with existing code

## Migration Path for Experimental Groups

For SPARTACUS, EHC, STEBBS, or NHood:

1. Create Python module (e.g., `spartacus_vars.py`)
2. Define variables using `OutputVariable`
3. Add to `OUTPUT_REGISTRY` in `__init__.py`
4. Regenerate with `generate_varlist.py`
5. Remove Fortran DATA statements
6. Done!

## Deprecation Timeline

- **Phase 1 (Complete)**: Dual implementation with generated code
- **Phase 2 (Current)**: Monitoring and validation
- **Phase 3 (Future)**: Migrate experimental groups
- **Phase 4 (Future)**: Complete removal of manual Fortran definitions

## Files Changed

### Created
- `src/suews/src/generate_varlist.py` - Generator script
- `src/suews/src/varlist_generated.f95` - Auto-generated (not version-controlled)
- `.claude/reference/fortran-deprecation-strategy.md` - This document

### Modified
- `src/suews/src/suews_ctrl_output.f95` - Replaced DATA with INCLUDE
- `src/suews/Makefile` - Added generation rules

### Backup
- `src/suews/src/suews_ctrl_output.f95.backup` - Original file preserved

## Notes

- The `varlist_generated.f95` file should NOT be version-controlled (add to .gitignore)
- It is generated automatically at build time
- The generator script (`generate_varlist.py`) IS version-controlled
- Backup file preserved for reference during transition
