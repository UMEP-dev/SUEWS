# Output Variables Migration Plan: Fortran → Python/Pydantic

## Overview

This document outlines the strategy for migrating output variable definitions from Fortran-first to Python/Pydantic-first architecture.

## Current Architecture (Fortran-First)

```
Fortran varListAll (1400 variables)
    ↓
f90wrap bindings (sd.output_name_n, sd.output_size)
    ↓
Runtime extraction (get_output_info_df)
    ↓
DataFrame df_var (metadata catalog)
    ↓
Post-processing & output
```

**Issues:**
- Python has no compile-time knowledge of available variables
- No type checking or validation in Python
- Documentation must be manually synced
- Adding variables requires Fortran rebuild

## Target Architecture (Python-First)

```
Python Pydantic Models (source of truth)
    ↓
Type-checked configuration & validation
    ↓
Runtime metadata (from Pydantic)
    ↓
Post-processing & output
    ↓
Optional: Validation against Fortran (transitional)
```

**Benefits:**
- Type safety and IDE autocomplete
- Self-documenting code
- Easier to extend and maintain
- Potential for auto-generating Fortran (future)
- Better integration with Python ecosystem

## Pydantic Model Design

### Core Data Structures

```python
# src/supy/data_model/output/variables.py

from pydantic import BaseModel, Field
from enum import Enum
from typing import List, Dict, Optional

class AggregationMethod(str, Enum):
    """How variables are aggregated during resampling"""
    TIME = "T"        # Time columns (no aggregation)
    AVERAGE = "A"     # Time-averaged (mean)
    SUM = "S"         # Cumulative sum
    LAST = "L"        # Last value in period

class OutputLevel(int, Enum):
    """Output priority levels for selective output"""
    DEFAULT = 0       # Always included (core variables)
    EXTENDED = 1      # Extended output set
    SNOW_DETAILED = 2 # Snow-specific detailed output

class OutputGroup(str, Enum):
    """Logical grouping of output variables"""
    DATETIME = "datetime"
    SUEWS = "SUEWS"
    SNOW = "snow"
    ESTM = "ESTM"
    RSL = "RSL"
    BL = "BL"
    DEBUG = "debug"
    BEERS = "BEERS"
    DAILYSTATE = "DailyState"

class OutputVariable(BaseModel):
    """Definition of a single output variable"""
    name: str = Field(description="Variable name (column header)")
    unit: str = Field(description="Physical units")
    description: str = Field(description="Long-form description")
    aggregation: AggregationMethod = Field(description="Resampling aggregation method")
    group: OutputGroup = Field(description="Output group membership")
    level: OutputLevel = Field(description="Output priority level")
    format: str = Field(default="f104", description="Fortran format specifier")

    class Config:
        use_enum_values = True

class OutputVariableRegistry(BaseModel):
    """Complete registry of all output variables"""
    variables: List[OutputVariable] = Field(description="All registered output variables")

    def by_group(self, group: OutputGroup) -> List[OutputVariable]:
        """Get all variables in a specific group"""
        return [v for v in self.variables if v.group == group]

    def by_level(self, max_level: OutputLevel) -> List[OutputVariable]:
        """Get variables up to specified output level"""
        return [v for v in self.variables if v.level.value <= max_level.value]

    def by_name(self, name: str) -> Optional[OutputVariable]:
        """Get variable by name"""
        return next((v for v in self.variables if v.name == name), None)

    def get_aggregation_rules(self) -> Dict[str, Dict[str, str]]:
        """Generate aggregation rules dict for resample_output()"""
        dict_var_aggm = {}
        for group in OutputGroup:
            group_vars = self.by_group(group)
            if group_vars:
                dict_var_aggm[group.value] = {
                    v.name: self._get_agg_func(v.aggregation)
                    for v in group_vars
                }
        return dict_var_aggm

    @staticmethod
    def _get_agg_func(method: AggregationMethod) -> str:
        """Map aggregation method to pandas function"""
        mapping = {
            AggregationMethod.AVERAGE: "mean",
            AggregationMethod.SUM: "sum",
            AggregationMethod.LAST: lambda x: x.iloc[-1] if len(x) > 0 else np.nan,
            AggregationMethod.TIME: lambda x: x.iloc[-1] if len(x) > 0 else np.nan,
        }
        return mapping[method]
```

### Variable Definitions

Variables will be defined in separate modules by group:

```python
# src/supy/data_model/output/datetime_vars.py
from .variables import OutputVariable, AggregationMethod, OutputGroup, OutputLevel

DATETIME_VARIABLES = [
    OutputVariable(
        name="Year",
        unit="YYYY",
        description="Year",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
        format="i0004"
    ),
    OutputVariable(
        name="DOY",
        unit="DOY",
        description="Day of Year",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
        format="i0004"
    ),
    # ... etc
]
```

```python
# src/supy/data_model/output/suews_vars.py
SUEWS_VARIABLES = [
    OutputVariable(
        name="QH",
        unit="W m-2",
        description="Sensible heat flux",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.SUEWS,
        level=OutputLevel.DEFAULT,
        format="f104"
    ),
    OutputVariable(
        name="QE",
        unit="W m-2",
        description="Latent heat flux",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.SUEWS,
        level=OutputLevel.DEFAULT,
        format="f104"
    ),
    # ... etc
]
```

### Registry Assembly

```python
# src/supy/data_model/output/__init__.py
from .variables import OutputVariableRegistry
from .datetime_vars import DATETIME_VARIABLES
from .suews_vars import SUEWS_VARIABLES
from .snow_vars import SNOW_VARIABLES
from .estm_vars import ESTM_VARIABLES
from .rsl_vars import RSL_VARIABLES
from .dailystate_vars import DAILYSTATE_VARIABLES
from .debug_vars import DEBUG_VARIABLES

# Create global registry
OUTPUT_REGISTRY = OutputVariableRegistry(
    variables=(
        DATETIME_VARIABLES +
        SUEWS_VARIABLES +
        SNOW_VARIABLES +
        ESTM_VARIABLES +
        RSL_VARIABLES +
        DAILYSTATE_VARIABLES +
        DEBUG_VARIABLES
    )
)

# Export key items
__all__ = [
    "OUTPUT_REGISTRY",
    "OutputVariable",
    "OutputVariableRegistry",
    "AggregationMethod",
    "OutputGroup",
    "OutputLevel",
]
```

## Migration Steps

### Phase 1: Create Python Models (Non-breaking)
1. ✅ Design Pydantic models
2. Create new directory: `src/supy/data_model/output/`
3. Extract all ~1400 variables from Fortran
4. Define variables in Python modules by group
5. Create registry and accessor functions
6. Add validation against Fortran at import time

### Phase 2: Update Runtime Code
1. Modify `_post.py::get_output_info_df()` to use Python registry
2. Update `_post.py::dict_var_aggm` generation
3. Ensure backward compatibility with existing code
4. Add deprecation warnings for Fortran-based access

### Phase 3: Update Documentation
1. Modify `docs/generate_datamodel_rst.py` to use Pydantic models
2. Auto-generate RST from `OUTPUT_REGISTRY`
3. Ensure documentation matches Python definitions

### Phase 4: Testing & Validation
1. Run existing benchmark tests
2. Verify output compatibility
3. Add tests for new Pydantic models
4. Performance testing

### Phase 5: Cleanup (Future)
1. Remove Fortran output metadata (keep only in Python)
2. Consider auto-generating Fortran from Python (long-term)

## Implementation Priority

**Immediate (this session):**
- Create core Pydantic models
- Extract SUEWS core variables (QH, QE, QN, T2, etc.)
- Create registry structure
- Update `get_output_info_df()` to use registry

**Follow-up:**
- Extract all remaining variables
- Documentation generation updates
- Full test suite validation
- Remove Fortran dependency

## Compatibility Strategy

During transition, support both systems:

```python
def get_output_info_df():
    """Get output variable metadata (Pydantic-first, Fortran fallback)"""
    try:
        # Try Python registry first
        return _get_output_info_from_registry()
    except Exception as e:
        logger.warning(f"Falling back to Fortran metadata: {e}")
        # Fallback to Fortran extraction
        return _get_output_info_from_fortran()

def _get_output_info_from_registry():
    """Extract metadata from Python OUTPUT_REGISTRY"""
    from .data_model.output import OUTPUT_REGISTRY

    data = []
    for var in OUTPUT_REGISTRY.variables:
        data.append({
            "var": var.name,
            "group": var.group.value,
            "aggm": var.aggregation.value,
            "outlevel": str(var.level.value)
        })

    df = pd.DataFrame(data)
    df_indexed = df.set_index(["group", "var"])
    return df_indexed

def _get_output_info_from_fortran():
    """Legacy: Extract metadata from Fortran (original implementation)"""
    size_var_list = sd.output_size()
    list_var_x = [np.array(sd.output_name_n(i)) for i in np.arange(size_var_list) + 1]
    df_var_list = pd.DataFrame(list_var_x, columns=["var", "group", "aggm", "outlevel"])
    # ... rest of original implementation
    return df_var_dfm
```

## File Structure

```
src/supy/data_model/output/
├── __init__.py              # Registry assembly & exports
├── variables.py             # Core Pydantic models & enums
├── datetime_vars.py         # Datetime variables (5 vars)
├── suews_vars.py            # Core SUEWS variables (~163 vars)
├── snow_vars.py             # Snow variables (~130 vars)
├── estm_vars.py             # ESTM variables (~26 vars)
├── rsl_vars.py              # RSL profile variables (~140 vars)
├── dailystate_vars.py       # Daily state variables (~47 vars)
├── bl_vars.py               # Boundary layer variables (~17 vars)
├── beers_vars.py            # BEERS radiation variables (~28 vars)
└── debug_vars.py            # Debug variables (~50 vars)
```

## Benefits Summary

1. **Type Safety**: Pydantic validation ensures correctness
2. **IDE Support**: Autocomplete for all variables
3. **Documentation**: Self-documenting with descriptions
4. **Extensibility**: Easy to add new variables in Python
5. **Testing**: Unit tests for variable definitions
6. **Integration**: Better integration with Python data model
7. **Future-proof**: Foundation for potential Fortran code generation

## Risks & Mitigation

| Risk | Mitigation |
|------|------------|
| Breaking existing code | Dual implementation with fallback |
| Performance degradation | Benchmark tests, cache registry |
| Desync with Fortran | Validation function, CI checks |
| Migration complexity | Phased approach, extensive testing |

## Success Criteria

- ✅ All output variables defined in Python
- ✅ Existing tests pass without modification
- ✅ Documentation auto-generated from Pydantic
- ✅ No performance regression
- ✅ Backward compatible with existing code
