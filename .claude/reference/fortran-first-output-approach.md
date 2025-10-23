# Fortran-First Output Variable Approach (Option A)

## Problem Statement

Current inconsistency in SUEWS codebase:

- **Input parameters**: Fortran TYPEs → Python Pydantic (Fortran as source)
- **Output variables**: Python Pydantic → Generated Fortran DATA statements (Python as source)

This creates:
1. Two different patterns for essentially the same metadata
2. Python codegen scripts mixed in Fortran source directory (`src/suews/src/generate_varlist.py`)
3. Maintenance burden keeping two systems in sync
4. Unclear "source of truth" for variable definitions

## Proposed Solution: Fortran as Source of Truth

Align output variables with the same pattern used for input parameters.

### Current Input Parameter Pattern

```fortran
! In src/suews/src/suews_ctrl_type.f95
TYPE, PUBLIC :: SUEWS_CONFIG
   INTEGER :: DiagMethod = 0 ! Defines how near surface diagnostics are calculated
   INTEGER :: EmissionsMethod = 0 ! method to calculate anthropogenic heat [-]
   ...
END TYPE SUEWS_CONFIG
```

```python
# In src/supy/data_model/core/model.py
class ModelPhysics(BaseModel):
    """Model physics configuration options."""

    netradiationmethod: FlexibleRefValue(NetRadiationMethod) = Field(...)
    emissionsmethod: FlexibleRefValue(EmissionsMethod) = Field(...)
    ...

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert to DataFrame for f90wrap bridge"""

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "ModelPhysics":
        """Reconstruct from DataFrame"""
```

**Bridge**: DataFrame via `to_df_state()` / `from_df_state()` methods
**Sync**: Manual maintenance, kept in sync by developers

### Proposed Output Variable Pattern

Define output variables as Fortran TYPEs similar to SUEWS_CONFIG:

```fortran
! In src/suews/src/suews_ctrl_type.f95 (or new suews_ctrl_output_type.f95)

TYPE, PUBLIC :: OUTPUT_VAR_DEF
   CHARACTER(len=20) :: name        ! Variable name (e.g., 'Kdown')
   CHARACTER(len=20) :: unit        ! Physical unit (e.g., 'W m-2')
   CHARACTER(len=100) :: description ! Human-readable description
   CHARACTER(len=10) :: format      ! Output format (e.g., 'f104')
   CHARACTER(len=1) :: aggregation  ! Aggregation method ('A', 'S', 'L', 'T')
   CHARACTER(len=20) :: group       ! Output group (e.g., 'SUEWS', 'EHC')
   INTEGER :: level                 ! Output level (0=default, 1=advanced, 2=debug)
END TYPE OUTPUT_VAR_DEF

! Define output variables in structured groups
TYPE, PUBLIC :: OUTPUT_SUEWS_DEFS
   INTEGER :: n_vars = 85
   TYPE(OUTPUT_VAR_DEF), DIMENSION(85) :: vars
END TYPE OUTPUT_SUEWS_DEFS

TYPE, PUBLIC :: OUTPUT_EHC_DEFS
   INTEGER :: n_vars = 224
   TYPE(OUTPUT_VAR_DEF), DIMENSION(224) :: vars
END TYPE OUTPUT_EHC_DEFS

! Global registry
TYPE, PUBLIC :: OUTPUT_REGISTRY_DEFS
   TYPE(OUTPUT_SUEWS_DEFS) :: suews
   TYPE(OUTPUT_EHC_DEFS) :: ehc
   TYPE(OUTPUT_SPARTACUS_DEFS) :: spartacus
   ! ... other groups
END TYPE OUTPUT_REGISTRY_DEFS
```

Initialize with DATA statements in Fortran:

```fortran
! Initialize SUEWS output variables
DATA output_registry%suews%vars(1:5) / &
   OUTPUT_VAR_DEF('Kdown', 'W m-2', 'f104', &
                  'Incoming shortwave radiation', 'A', 'SUEWS', 0), &
   OUTPUT_VAR_DEF('Kup', 'W m-2', 'f104', &
                  'Outgoing shortwave radiation', 'A', 'SUEWS', 0), &
   ! ... rest of variables
   /
```

Python side becomes a **parser** instead of generator:

```python
# In src/supy/data_model/output/
# Parse Fortran TYPE definitions to build Pydantic models

from supy._output_defs import output_registry  # f90wrap binding

class OutputVariable(BaseModel):
    """Python representation of Fortran OUTPUT_VAR_DEF"""
    name: str
    unit: str
    description: str
    format: str
    aggregation: AggregationMethod
    group: OutputGroup
    level: OutputLevel

    @classmethod
    def from_fortran(cls, fvar) -> "OutputVariable":
        """Parse f90wrap binding"""
        return cls(
            name=fvar.name.strip(),
            unit=fvar.unit.strip(),
            description=fvar.description.strip(),
            format=fvar.format.strip(),
            aggregation=AggregationMethod(fvar.aggregation.strip()),
            group=OutputGroup(fvar.group.strip()),
            level=OutputLevel(fvar.level)
        )

# Build registry from Fortran definitions
OUTPUT_REGISTRY = OutputVariableRegistry(
    variables=tuple(
        OutputVariable.from_fortran(fvar)
        for group in output_registry._fortran_groups
        for fvar in group.vars
    )
)
```

## Benefits

1. **Consistency**: Both inputs and outputs follow same pattern (Fortran → Python)
2. **Single source of truth**: Fortran TYPEs are canonical, Python mirrors them
3. **Metadata colocation**: Variable definitions live with Fortran computation code
4. **No codegen pollution**: No Python scripts in `src/suews/src/`
5. **Type safety**: Fortran compiler checks TYPE structure
6. **Easier maintenance**: Change Fortran, rebuild, Python automatically updates

## Migration Path

### Phase 1: Create Fortran TYPE Definitions
1. Create `src/suews/src/suews_ctrl_output_type.f95`
2. Define `OUTPUT_VAR_DEF` TYPE
3. Create group-specific TYPEs (OUTPUT_SUEWS_DEFS, OUTPUT_EHC_DEFS, etc.)
4. Initialize with DATA statements from current Python definitions

### Phase 2: Expose to Python via f90wrap
1. Update `src/suews/src/meson.build` to include new file
2. Add f90wrap bindings for output TYPE definitions
3. Test that Python can access Fortran output definitions

### Phase 3: Update Python to Parse Fortran
1. Create parser utilities in `src/supy/data_model/output/fortran_parser.py`
2. Implement `OutputVariable.from_fortran()` classmethod
3. Build OUTPUT_REGISTRY from parsed Fortran definitions
4. Verify Python registry matches current structure

### Phase 4: Remove Codegen
1. Delete `src/suews/src/generate_varlist.py`
2. Delete `src/suews/src/replace_data_statements.py`
3. Update `meson.build` to remove codegen steps
4. Remove generated file `src/suews/src/varlist_generated.f95`
5. Update build documentation

### Phase 5: Consolidate Variable Files
1. Move variable module files from `src/supy/data_model/output/*_vars.py` to `src/supy/data_model/output/fortran_parser.py`
2. Keep only parsing logic in Python
3. All metadata lives in Fortran TYPEs

## Trade-offs

### Advantages
- Consistent architecture across codebase
- Fortran compiler validates structure
- Simpler build process (no codegen step)
- Clear ownership (Fortran team owns definitions)
- Python becomes thin wrapper/mirror

### Disadvantages
- Fortran DATA statements more verbose than Python
- Requires f90wrap for Python access
- Changes require Fortran rebuild (but already true for code changes)
- Python-first developers must edit Fortran (cultural shift)

## Alternative Considered: Python-First Everywhere

Could instead apply codegen to input parameters (Python → Fortran TYPEs), making Python canonical for everything. Rejected because:

1. Input parameters already exist as Fortran TYPEs - would be wasted work to generate them
2. Fortran is the computation engine - natural to define data structures there
3. Python codegen scripts shouldn't live in Fortran source directories
4. Scientific computing convention: computation language owns data structures

## References

- Current input parameter implementation: `src/suews/src/suews_ctrl_type.f95`
- Current output variable implementation: `src/supy/data_model/output/__init__.py`
- Bridge pattern: `src/supy/data_model/core/model.py` (`to_df_state`/`from_df_state`)
- Current codegen: `src/suews/src/generate_varlist.py`

## Related Issues

- Code organisation: Python scripts mixed with Fortran source
- Build complexity: Multi-stage codegen during compilation
- Documentation generation: Currently reads Python, would read Fortran TYPEs
- Schema generation: Would parse Fortran instead of Python Pydantic models
