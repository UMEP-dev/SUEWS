# SUEWS Output Variable Definitions - Python/Pydantic Implementation

## Overview

This module provides **Python-first** definitions of SUEWS output variables using Pydantic models, replacing the previous Fortran-first runtime extraction approach.

## Status: Complete ✅

**Implementation:**
- ✅ Core Pydantic models with full metadata
- ✅ **All 528 variables implemented** across 9 groups
- ✅ Type-safe variable registry
- ✅ Backward-compatible DataFrame conversion
- ✅ Aggregation rules generation
- ✅ Integration with `_post.py` (dual implementation with Fortran fallback)
- ✅ Comprehensive test suite (all tests passing)

**Variable Coverage:**
- datetime: 5 variables
- SUEWS: 85 variables (core energy, water, met, carbon)
- snow: 98 variables (snow properties by surface type)
- ESTM: 27 variables (element surface temperatures)
- RSL: 135 variables (roughness sublayer profiles)
- DailyState: 47 variables (daily accumulated states)
- BL: 17 variables (boundary layer profiles)
- BEERS: 29 variables (detailed radiation)
- debug: 85 variables (diagnostic outputs by surface)

**Fortran Deprecation:**
- ✅ Automatic Fortran code generation from Python OUTPUT_REGISTRY
- ✅ Python is now the single source of truth
- ✅ Build system integration (Makefile)
- ✅ Eliminated ~1200 lines of duplicate Fortran DATA statements
- See `.claude/reference/fortran-deprecation-strategy.md` for details

**Next Steps:**
- Update documentation generation to use Pydantic models
- Full integration testing with compiled Fortran
- Performance benchmarking
- Migrate experimental groups (SPARTACUS, EHC, STEBBS, NHood) when needed

## Architecture

```
Python Pydantic Models (source of truth)
    ↓
OUTPUT_REGISTRY (type-safe registry)
    ↓
Runtime metadata (DataFrame compatible)
    ↓
Post-processing & output
```

## Key Components

### 1. Core Models (`variables.py`)

- **`OutputVariable`**: Complete variable metadata
- **`OutputVariableRegistry`**: Central registry with querying methods
- **`AggregationMethod`**: Enum for resampling methods (T/A/S/L)
- **`OutputGroup`**: Enum for logical grouping
- **`OutputLevel`**: Enum for output selection (0/1/2)

### 2. Variable Definitions

Variables organized by group in separate modules:

- `datetime_vars.py`: Timestamp variables (Year, DOY, Hour, Min, Dectime)
- `suews_vars.py`: Core SUEWS variables (QH, QE, T2, Rain, etc.)
- *(Future)* `snow_vars.py`, `estm_vars.py`, `rsl_vars.py`, etc.

### 3. Integration

The registry integrates with existing SUEWS code via `_post.py`:

```python
# _post.py attempts Pydantic first, falls back to Fortran
df_var = get_output_info_df()  # Returns same structure as before
```

## Usage Examples

### Basic Registry Access

```python
from supy.data_model.output import OUTPUT_REGISTRY, OutputGroup, OutputLevel

# Get all SUEWS variables
suews_vars = OUTPUT_REGISTRY.by_group(OutputGroup.SUEWS)

# Get default output level variables
default_vars = OUTPUT_REGISTRY.by_level(OutputLevel.DEFAULT)

# Get specific variable
qh = OUTPUT_REGISTRY.by_name("QH")
print(f"{qh.name}: {qh.description} [{qh.unit}]")
# Output: QH: Sensible heat flux [W m-2]
```

### Aggregation Rules

```python
# Generate aggregation rules for pandas resample()
agg_rules = OUTPUT_REGISTRY.get_aggregation_rules()

# Use in resampling
df_resampled = df.resample('1H').agg(agg_rules['SUEWS'])
```

### DataFrame Conversion

```python
# Convert to backward-compatible DataFrame
df_var = OUTPUT_REGISTRY.to_dataframe()

# Same structure as before: MultiIndex (group, var) with columns (aggm, outlevel, func)
print(df_var.loc[('SUEWS', 'QH')])
```

## Variable Metadata

Each variable has complete metadata:

| Attribute | Description | Example |
|-----------|-------------|---------|
| `name` | Variable name | `"QH"` |
| `unit` | Physical units | `"W m-2"` |
| `description` | Long description | `"Sensible heat flux"` |
| `aggregation` | Resampling method | `AVERAGE` |
| `group` | Logical group | `SUEWS` |
| `level` | Output priority | `DEFAULT` (0) |
| `format` | Fortran format | `"f104"` |

## Output Groups

| Group | Purpose | Variables |
|-------|---------|-----------|
| `datetime` | Timestamps | Year, DOY, Hour, Min, Dectime |
| `SUEWS` | Core outputs | QH, QE, QN, T2, Rain, etc. |
| `snow` | Snow-specific | SWE_*, Sd_*, DensSnow_*, etc. |
| `ESTM` | Surface temperatures | QS, TWALL*, TROOF*, etc. |
| `RSL` | Roughness sublayer | T_1..T_30, U_1..U_30, etc. |
| `DailyState` | Daily states | LAI_*, GDD_*, SDD_*, etc. |
| `BL` | Boundary layer | z, theta, q, etc. |
| `BEERS` | Radiation | Kdown2d, Ldown2d, Tmrt, etc. |
| `debug` | Diagnostics | RA, RS, Ts_*, QN_*, etc. |

## Output Levels

Variables are prioritised for selective output:

- **Level 0 (DEFAULT)**: Core variables always included (39 vars currently)
  - Example: QH, QE, T2, RH2, Rain, Evap
- **Level 1 (EXTENDED)**: Extended variable set (78 vars currently)
  - Example: RA, RS, QHlumps, SMD by surface type
- **Level 2 (SNOW_DETAILED)**: Snow-specific detailed output (90 vars currently)
  - Example: QNSnow, AlbSnow, SWE, MeltWater

## Aggregation Methods

| Method | Code | Pandas Function | Use Case |
|--------|------|-----------------|----------|
| `TIME` | `T` | Last value | Timestamps |
| `AVERAGE` | `A` | `mean` | Fluxes (QH, QE) |
| `SUM` | `S` | `sum` | Accumulations (Rain, Evap) |
| `LAST` | `L` | Last value | State variables (SMD, State) |

## Testing

Run the standalone test to verify the implementation:

```bash
python test_output_models.py
```

Expected output:
```
✅ ALL TESTS PASSED!

Summary:
- Total variables in registry: 90
- Datetime variables: 5
- SUEWS variables: 85
```

## Migration Plan

See `.claude/reference/output-variables-migration.md` for the complete migration strategy.

### Next Steps

1. **Extend variable coverage**: Add remaining variable groups
2. **Documentation integration**: Update `docs/generate_datamodel_rst.py`
3. **Validation**: Compare Pydantic vs Fortran metadata
4. **Testing**: Full integration tests with compiled SUEWS
5. **Deprecation**: Eventually remove Fortran metadata dependency

## Benefits

### Compared to Fortran-First Approach

✅ **Type Safety**: Pydantic validation ensures correctness
✅ **IDE Support**: Autocomplete and type hints
✅ **Self-Documenting**: Rich metadata in Python
✅ **Extensibility**: Easy to add variables in Python
✅ **Testing**: Unit tests for variable definitions
✅ **Integration**: Better integration with Python ecosystem
✅ **Maintainability**: Centralised, version-controlled definitions

### Backward Compatibility

The implementation maintains full backward compatibility:

- Same DataFrame structure from `get_output_info_df()`
- Same aggregation rules for `resample_output()`
- Automatic fallback to Fortran if Pydantic fails
- No changes required to existing code

## File Structure

```
src/supy/data_model/output/
├── __init__.py              # Registry assembly & exports
├── README.md                # This file
├── variables.py             # Core Pydantic models & enums
├── datetime_vars.py         # Datetime variables (5 vars)
└── suews_vars.py            # Core SUEWS variables (85 vars)

Future additions:
├── snow_vars.py             # Snow variables
├── estm_vars.py             # ESTM variables
├── rsl_vars.py              # RSL profile variables
├── dailystate_vars.py       # Daily state variables
├── bl_vars.py               # Boundary layer variables
├── beers_vars.py            # BEERS radiation variables
└── debug_vars.py            # Debug variables
```

## References

- **Migration plan**: `.claude/reference/output-variables-migration.md`
- **Fortran definitions**: `src/suews/src/suews_ctrl_output.f95`
- **Integration point**: `src/supy/_post.py`
- **Test script**: `test_output_models.py`
