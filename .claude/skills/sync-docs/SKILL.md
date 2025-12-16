---
name: sync-docs
description: Check consistency between SUEWS code and documentation CONTENT. Use when modifying physics parameters, adding new features, changing APIs, updating Makefiles, or before releases. Validates scientific documentation against Fortran code, API docs against Python signatures, configuration docs against Pydantic models, parameter units/ranges/defaults, and build logic consistency. Note: RST/Markdown FORMATTING is handled by lint-code skill.
---

# SUEWS Documentation-Code Consistency Checker

Check that documentation **content** accurately reflects the codebase.

> **Scope**: This skill checks doc-code **content consistency** (values, signatures, parameters).
> For **formatting** checks (RST heading levels, image attributes, markdown style), use the `lint-code` skill.

## Workflow

### 1. Analyse Recent Code Changes

Review the most recent code modifications, focusing on:
- New functions, methods, or classes
- Modified function signatures or parameters
- Changed behaviour or logic
- New configuration options or settings
- Deprecated or removed features

### 2. Identify Documentation Locations

Determine where documentation should exist:
- API documentation in docstrings
- User guides in the `docs/` directory
- README files for module-level changes
- Configuration documentation for new settings
- Migration guides for breaking changes

### 3. Detect Synchronisation Issues

Compare code against documentation to find:
- Missing documentation for new features
- Outdated parameter descriptions
- Incorrect usage examples
- Obsolete references to removed functionality
- Inconsistent terminology or naming

### 4. Provide Recommendations

For each issue found, classify by importance:
- **Critical**: User-facing API changes without documentation
- **High**: New features lacking usage examples
- **Medium**: Internal changes affecting developer documentation
- **Low**: Minor clarifications or formatting improvements

---

## Areas to Check

### 1. Scientific Documentation (Critical)

**Location**: `docs/source/parameterisations-and-sub-models.rst`

Check against Fortran physics modules in `src/suews/src/`:

| Doc Section | Fortran Module |
|-------------|----------------|
| Net all-wave radiation (NARP) | `suews_phys_narp.f95` |
| BEERS radiation | `suews_phys_beers.f95` |
| Anthropogenic heat (QF) | `suews_phys_anthrop*.f95` |
| Storage heat (OHM) | `suews_phys_ohm.f95` |
| Storage heat (AnOHM) | `suews_phys_anohm.f95` |
| Storage heat (ESTM) | `suews_phys_estm.f95` |
| Evaporation | `suews_phys_evap.f95` |
| Snow | `suews_phys_snow.f95` |
| SPARTACUS | `suews_phys_spartacus.f95` |
| STEBBS | `suews_phys_stebbs.f95` |
| RSL profiles | `suews_phys_rslprof.f95` |
| Water distribution | `suews_phys_waterdist.f95` |

**Verify**:
- Scheme options documented match code constants
- Physics equations described match implementation
- Default parameter values match code defaults
- Units documented match code comments
- References (citations) are current

### 2. API Documentation

**Location**: `docs/source/api/`

Check against Python code in `src/supy/`:

```
ISSUE: Function signature changed but docstring not updated
  Code: def run_supy(df_forcing, df_state_init, save_state=False, n_yr=1)
  Docs: Parameters section missing n_yr

ISSUE: Return type changed
  Code: -> Tuple[pd.DataFrame, pd.DataFrame]
  Docs: Still describes single DataFrame return
```

**Verify**:
- All public functions documented
- Parameter descriptions match type hints
- Return values accurately described
- Examples are runnable

### 3. Configuration Documentation

**Location**: `docs/source/inputs/yaml/`

Check against Pydantic models in `src/supy/data_model/`:

```
ISSUE: New config field not documented
  Model: class PhysicsConfig has field 'spartacus_enabled'
  Docs: Field missing from YAML reference

ISSUE: Default value mismatch
  Model: scheme: str = "W16"
  Docs: States default is "K75"
```

**Verify**:
- All Pydantic fields documented
- Default values match
- Valid options/enums listed correctly
- Required vs optional status matches

### 4. Parameter Units and Ranges

**Scientific parameters must have consistent**:

| Check | Code Location | Doc Location |
|-------|---------------|--------------|
| Units | Fortran `! [unit]` comments | RST descriptions |
| Valid ranges | Pydantic validators | Input parameter docs |
| Defaults | Fortran PARAMETER or Pydantic | Parameter tables |

```
ISSUE: Unit mismatch
  Code: REAL :: temperature  ! [K]
  Docs: "Temperature in degrees Celsius"

ISSUE: Range mismatch
  Code: @validator albedo must be 0.0-1.0
  Docs: "Albedo values typically 0.1-0.5"
```

### 5. Build Logic Documentation

**Makefile targets vs documentation**:

Check `Makefile` against:
- `README.md` quick start section
- `.claude/skills/setup-dev/SKILL.md`
- `docs/source/installation.rst`

```
ISSUE: Makefile target renamed but docs not updated
  Makefile: make dev (was: make install-dev)
  Docs: Still references 'make install-dev'

ISSUE: New target not documented
  Makefile: make test-smoke
  Docs: Only mentions 'make test'
```

### 6. Output Variables

**Location**: `docs/source/output_files/`

Check against:
- Fortran output arrays
- Python DataFrame columns
- Data model output definitions

```
ISSUE: New output variable not documented
  Code: df_output contains 'QF_traff'
  Docs: Output reference missing QF_traff description
```

---

## Comprehensive Data Model Alignment Checks

### 7. Pydantic Field Registry (Exhaustive)

**Systematically check ALL Pydantic fields** in `src/supy/data_model/core/`:

#### Core Model Files to Check

| File | Models | Fields to Verify |
|------|--------|------------------|
| `config.py` | `SUEWSConfig` | All top-level config fields |
| `site.py` | `Site`, `SiteProperties`, `*Properties` | All site/vegetation fields |
| `surface.py` | `PavedProperties`, `BldgsProperties`, `EvetrProperties`, `DectrProperties`, `GrassProperties`, `BsoilProperties`, `WaterProperties`, `ThermalLayers`, `VerticalLayers` | All surface parameters |
| `model.py` | `Model`, `ModelPhysics`, `ModelControl` | All physics method fields |
| `state.py` | `InitialStates` | All state variables |
| `human_activity.py` | `AnthropogenicEmissions`, `AnthropogenicHeat`, `CO2Params`, `IrrigationParams` | All activity fields |
| `hydro.py` | `WaterDistribution`, `StorageDrainParams` | All hydrology fields |
| `ohm.py` | `OHM_Coefficient_season_wetness` | All OHM coefficient fields |
| `profile.py` | `DayProfile`, `WeeklyProfile`, `HourlyProfile` | All profile fields |

#### For Each Pydantic Field, Verify

1. **Field Name**: Matches Fortran variable name convention
2. **Type**: Correct Python type annotation
3. **Default Value**: Matches Fortran PARAMETER or initialisation
4. **Unit** (`json_schema_extra["unit"]`): Matches Fortran comment `! [unit]`
5. **Description** (`description=`): Accurate and complete
6. **Validators**: Range constraints match code logic
7. **Required vs Optional**: Correctly marked

```
ISSUE: Missing unit in Pydantic field
  File: surface.py
  Field: BldgsProperties.building_height
  Expected: unit="m" in json_schema_extra
  Found: No unit specified

ISSUE: Default mismatch
  File: model.py
  Field: ModelPhysics.ohmincqf
  Pydantic default: 1
  Fortran default: OHMIncQF = 0  ! in suews_ctrl_const.f95
```

### 8. Output Variable Registry (Exhaustive)

**Check ALL variables** in `src/supy/data_model/output/`:

#### Output Variable Files

| File | Variable Group | Fortran Source |
|------|---------------|----------------|
| `datetime_vars.py` | DATETIME | N/A (Python-only) |
| `suews_vars.py` | SUEWS | `suews_phys_*.f95` outputs |
| `snow_vars.py` | SNOW | `suews_phys_snow.f95` |
| `estm_vars.py` | ESTM | `suews_phys_estm.f95` |
| `rsl_vars.py` | RSL | `suews_phys_rslprof.f95` |
| `dailystate_vars.py` | DAILYSTATE | `suews_phys_dailystate.f95` |
| `bl_vars.py` | BL | `suews_phys_bluews.f95` |
| `beers_vars.py` | BEERS | `suews_phys_beers.f95` |
| `debug_vars.py` | DEBUG | Various modules |
| `ehc_vars.py` | EHC | `suews_phys_ehc.f95` |
| `spartacus_vars.py` | SPARTACUS | `suews_phys_spartacus.f95` |
| `stebbs_vars.py` | STEBBS | `suews_phys_stebbs.f95` |
| `nhood_vars.py` | NHOOD | Neighbourhood calculations |

#### For Each Output Variable, Verify

1. **name**: Matches Fortran output array column
2. **unit**: Matches Fortran `! [unit]` comment
3. **description**: Accurate physics description
4. **aggregation**: Correct method (AVERAGE/SUM/LAST/TIME)
5. **group**: Correct OutputGroup assignment
6. **level**: Appropriate OutputLevel (DEFAULT/EXTENDED/SNOW_DETAILED)

```
ISSUE: Unit mismatch in output variable
  File: suews_vars.py
  Variable: QH
  Python unit: "W m-2"
  Fortran comment: ! [W/m2]  (equivalent, OK)

ISSUE: Missing output variable
  Fortran: REAL :: QF_traff  ! Traffic anthropogenic heat [W m-2]
  Python: Not in SUEWS_VARIABLES list
  Docs: Not documented

ISSUE: Description mismatch
  Variable: QS
  Python: "Net storage heat flux"
  Docs: "Storage heat flux into the substrate"
  Fortran: ! Storage heat flux [W m-2]
```

### 9. Fortran-Python Parameter Cross-Reference

**For each physics module**, verify alignment:

#### Parameter Declaration Alignment

| Python Location | Fortran Location | Check |
|----------------|------------------|-------|
| `ModelPhysics.storageheatmethod` | `suews_ctrl_const.f95::StorageHeatMethod` | Values match |
| `SurfaceProperties.alb` | `suews_data.f95::alb` | Default matches |
| `OHM_Coefficient_season_wetness.a1` | `suews_phys_ohm.f95::a1` | Type matches |

#### Method/Scheme Option Alignment

For each physics scheme option:
1. Python Enum values match Fortran INTEGER constants
2. Documentation describes all valid options
3. Default option consistent across all three

```
ISSUE: Scheme option mismatch
  Physics: StorageHeatMethod
  Python enum: [1, 2, 3, 4]
  Fortran constants: [1, 2, 3, 4, 5]  ! New option 5 added
  Docs: Only mentions options 1-3
```

### 10. Enum Alignment (All Enums)

**Check ALL enums** in data model against code:

| Python Enum | Fortran Location | Doc Location |
|-------------|------------------|--------------|
| `OutputGroup` | N/A (Python-only) | Output files docs |
| `OutputLevel` | N/A (Python-only) | Output files docs |
| `AggregationMethod` | N/A (Python-only) | Output files docs |
| `SurfaceType` | `suews_ctrl_const.f95` | Input parameter docs |
| `TimezoneOffset` | N/A (Python-only) | Configuration docs |
| Various `*Method` enums | `suews_ctrl_const.f95` | Parameterisation docs |

```
ISSUE: Enum value missing in documentation
  Enum: StorageHeatMethod
  Value: 4 (EHC)
  Docs: Only describes methods 1-3
```

### 11. Unit Consistency Across Sources

**Unit notation conventions**:

| Style | Example | Use In |
|-------|---------|--------|
| ASCII superscript | `W m^-2` | Pydantic json_schema_extra |
| Minus sign | `W m-2` | Output variables |
| Slash | `W/m2` | Fortran comments |

**All three are equivalent but should be normalised for comparison.**

Cross-check units between:
1. Fortran `! [unit]` comments
2. Pydantic `json_schema_extra["unit"]`
3. OutputVariable `unit` field
4. Documentation descriptions

```
ISSUE: Unit notation inconsistency
  Variable: QN
  Fortran: ! [W/m2]
  Python output: "W m-2"
  Pydantic field: "W m^-2"
  Docs: "W/m²"
  Status: Equivalent (OK) but consider normalising
```

---

## Output Format

```
[sync-docs] Documentation-Code Consistency Report

=== Scientific Documentation ===
  parameterisations-and-sub-models.rst:
    L45: BEERS default albedo=0.2 but code uses 0.15
    L112: OHM equation missing recent update (cite:t:`S24`)

=== API Documentation ===
  api/run_supy.rst:
    Missing parameter: n_yr (added in v2025.10)
    Return type outdated

=== Configuration Documentation ===
  inputs/yaml/physics.rst:
    Missing field: spartacus_enabled
    Default mismatch: scheme (docs: K75, code: W16)

=== Pydantic Data Model ===
  core/surface.py:
    BldgsProperties.building_height: Missing unit in json_schema_extra
    ThermalLayers.k: Unit should be "W m^-1 K^-1"
  core/model.py:
    ModelPhysics.ohmincqf: Default mismatch (Python: 1, Fortran: 0)

=== Output Variable Registry ===
  output/suews_vars.py:
    QF_traff: Missing from registry (exists in Fortran)
    T2: Description differs from Fortran comment
  output/spartacus_vars.py:
    SWup_sky: Unit "W m-2" should match docs "W/m²"

=== Fortran-Python Alignment ===
  StorageHeatMethod: Python missing option 5
  DiagMethod: Fortran default 0, Pydantic default 1

=== Build Documentation ===
  README.md:
    L34: 'make install-dev' should be 'make dev'

Summary: 14 issues (4 critical, 6 high, 4 medium)
```

---

## Verification Commands

### List All Pydantic Fields

```bash
# Count fields in each core model file
for f in src/supy/data_model/core/*.py; do
  echo "=== $f ==="
  grep -c "Field(" "$f" 2>/dev/null || echo "0"
done

# Find fields missing units
grep -n "Field(" src/supy/data_model/core/*.py | grep -v "json_schema_extra"
```

### List All Output Variables

```bash
# Count variables in each output file
for f in src/supy/data_model/output/*_vars.py; do
  echo "=== $f ==="
  grep -c "OutputVariable(" "$f" 2>/dev/null || echo "0"
done

# List all variable names
grep "name=" src/supy/data_model/output/*_vars.py | sed 's/.*name="\([^"]*\)".*/\1/'
```

### Compare Fortran and Python Defaults

```bash
# Find Fortran PARAMETER declarations
grep -r "PARAMETER" src/suews/src/*.f95 | grep -v "^!" | head -20

# Find Fortran default assignments
grep -r "= [0-9]" src/suews/src/suews_ctrl_const.f95 | grep -v "^!"

# Find Pydantic defaults
grep -n "default=" src/supy/data_model/core/*.py
```

### Check Unit Consistency

```bash
# Fortran unit comments
grep -r "! \[" src/suews/src/*.f95 | head -30

# Python json_schema_extra units
grep -n '"unit":' src/supy/data_model/core/*.py

# Output variable units
grep 'unit="' src/supy/data_model/output/*_vars.py
```

### Check Enum Alignment

```bash
# Python enums
grep -r "class.*Enum" src/supy/data_model/

# Fortran integer constants (potential enum values)
grep -r "INTEGER.*PARAMETER" src/suews/src/suews_ctrl_const.f95
```

---

## Priority

1. **Critical**: Scientific equation/parameter mismatches
2. **Critical**: API signature changes not reflected in docs
3. **Critical**: Output variable missing from registry
4. **High**: Configuration default mismatches
5. **High**: Missing physics method in enum
6. **Medium**: Missing documentation for new features
7. **Medium**: Unit notation inconsistency
8. **Low**: Description wording differences
9. **Low**: Formatting/style inconsistencies

---

## Programmatic Validation (Python)

For comprehensive validation, run this check in the activated environment:

```python
# Quick validation script
from supy.data_model.output import OUTPUT_REGISTRY
from supy.data_model.core import SUEWSConfig

# Check output registry
print(f"Total output variables: {len(OUTPUT_REGISTRY.variables)}")
for group in ["SUEWS", "SNOW", "ESTM", "RSL", "BEERS", "SPARTACUS", "STEBBS"]:
    vars_in_group = [v for v in OUTPUT_REGISTRY.variables if v.group == group]
    print(f"  {group}: {len(vars_in_group)} variables")

# Check for missing units in output variables
missing_units = [v.name for v in OUTPUT_REGISTRY.variables if not v.unit]
if missing_units:
    print(f"Variables missing units: {missing_units}")

# Validate SUEWSConfig schema
schema = SUEWSConfig.model_json_schema()
print(f"Config schema has {len(schema.get('properties', {}))} top-level fields")
```

---

## Notes

- Run after any physics module changes
- Run before releases
- Auto-generated docs (`docs/source/inputs/yaml/config-reference/`) sync automatically via `generate_datamodel_rst.py`
- Focus on manually-written documentation
- When adding new Pydantic fields, always include `json_schema_extra={"unit": "..."}` for physical quantities
- Output variables are Python-only (not extracted from Fortran at runtime)
