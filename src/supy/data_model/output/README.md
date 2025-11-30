# SUEWS Output Variable Definitions

## Overview

This module defines all SUEWS output variables using Python Pydantic models. The `OUTPUT_REGISTRY` serves as the **single source of truth** for output variable metadata, providing type-safe definitions with validation, aggregation rules for temporal resampling, and automatic documentation generation.

## Variable Groups

Output variables are organised by group:

| Group | Description |
|-------|-------------|
| datetime | Year, DOY, Hour, Min, Dectime |
| SUEWS | Core energy, water, met, carbon, surface temperatures |
| snow | Snow properties by surface type |
| ESTM | Element surface temperatures |
| EHC | Element heat capacity (surface, roof layers, wall layers) |
| RSL | Roughness sublayer profiles |
| DailyState | Daily accumulated states |
| BL | Boundary layer profiles |
| BEERS | Detailed radiation |
| debug | Diagnostic outputs with soil store and atmospheric vars |
| SPARTACUS | SPARTACUS radiation model (scalars and layer profiles) |
| STEBBS | Building energy model |
| NHood | Neighbourhood iteration count |

To get the current count: `len(OUTPUT_REGISTRY)`

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

Key design principles:
- **Python as source of truth**: All output metadata defined in Python, Fortran produces raw arrays
- **No code generation**: Definitions are used directly at runtime
- **Auto-generated documentation**: Sphinx docs built from Pydantic models via `docs/generate_output_variable_rst.py`

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
- `snow_vars.py`, `estm_vars.py`, `rsl_vars.py`, `dailystate_vars.py`, etc.

### 3. Integration

The registry integrates with existing SUEWS code via `_post.py`:

```python
# Python OUTPUT_REGISTRY is the single source of truth
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
df_resampled = df.resample('1h').agg(agg_rules['SUEWS'])
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

- **Level 0 (DEFAULT)**: Core variables always included
  - Example: QH, QE, T2, RH2, Rain, Evap
- **Level 1 (EXTENDED)**: Extended variable set
  - Example: RA, RS, QHlumps, SMD by surface type
- **Level 2 (SNOW_DETAILED)**: Snow-specific detailed output
  - Example: QNSnow, AlbSnow, SWE, MeltWater

## Aggregation Methods

| Method | Code | Pandas Function | Use Case |
|--------|------|-----------------|----------|
| `TIME` | `T` | Last value | Timestamps |
| `AVERAGE` | `A` | `mean` | Fluxes (QH, QE) |
| `SUM` | `S` | `sum` | Accumulations (Rain, Evap) |
| `LAST` | `L` | Last value | State variables (SMD, State) |

## Testing

Run the test suite:

```bash
make test  # or: pytest test/data_model/test_output_models.py
```

## Design Benefits

- **Type Safety**: Pydantic validation ensures correctness at definition time
- **IDE Support**: Full autocomplete and type hints for variable metadata
- **Self-Documenting**: Rich metadata enables automatic documentation generation
- **Extensibility**: Adding new variables requires only Python changes
- **Testing**: Variable definitions have unit test coverage
- **Ecosystem Integration**: Works naturally with pandas, numpy, and scientific Python stack

## Backward Compatibility

The registry provides backward-compatible interfaces:

- `get_output_info_df()` returns the same DataFrame structure used historically
- `resample_output()` uses aggregation rules from the registry
- Existing code continues to work without modification

## File Structure

```
src/supy/data_model/output/
├── __init__.py              # Registry assembly & exports
├── README.md                # This file
├── variables.py             # Core Pydantic models & enums
├── datetime_vars.py         # Datetime variables
├── suews_vars.py            # Core SUEWS variables
├── snow_vars.py             # Snow variables
├── estm_vars.py             # ESTM variables
├── rsl_vars.py              # RSL profile variables
├── dailystate_vars.py       # Daily state variables
├── bl_vars.py               # Boundary layer variables
├── beers_vars.py            # BEERS radiation variables
├── debug_vars.py            # Debug variables
├── ehc_vars.py              # EHC variables
├── spartacus_vars.py        # SPARTACUS variables
├── stebbs_vars.py           # STEBBS variables
└── nhood_vars.py            # NHood variables
```

## References

- **Integration point**: `src/supy/_post.py`
- **Documentation generator**: `docs/generate_output_variable_rst.py`
- **Test module**: `test/data_model/test_output_models.py`
