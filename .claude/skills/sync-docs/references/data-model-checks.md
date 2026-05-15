# Data Model Alignment Checks

## Pydantic Field Registry

**Location**: `src/supy/data_model/core/`

| File | Models |
|------|--------|
| `config.py` | `SUEWSConfig` |
| `site.py` | `Site`, `SiteProperties`, `*Properties` |
| `surface.py` | `PavedProperties`, `BldgsProperties`, `EvetrProperties`, `DectrProperties`, `GrassProperties`, `BsoilProperties`, `WaterProperties`, `ThermalLayers`, `VerticalLayers` |
| `model.py` | `Model`, `ModelPhysics`, `ModelControl` |
| `state.py` | `InitialStates` |
| `human_activity.py` | `AnthropogenicEmissions`, `AnthropogenicHeat`, `CO2Params`, `IrrigationParams` |
| `hydro.py` | `WaterDistribution`, `StorageDrainParams` |
| `ohm.py` | `OHM_Coefficient_season_wetness` |
| `profile.py` | `DayProfile`, `WeeklyProfile`, `HourlyProfile` |

### For Each Field, Verify

1. **Field Name**: Matches Fortran variable convention
2. **Type**: Correct Python type annotation
3. **Default**: Matches Fortran PARAMETER or init
4. **Unit**: `json_schema_extra["unit"]` matches Fortran `! [unit]`
5. **Description**: Accurate and complete
6. **Validators**: Range constraints match code
7. **Required/Optional**: Correctly marked

---

## Output Variable Registry

**Location**: `src/supy/data_model/output/`

| File | Group | Fortran Source |
|------|-------|----------------|
| `datetime_vars.py` | DATETIME | N/A (Python-only) |
| `suews_vars.py` | SUEWS | `suews_phys_*.f95` |
| `snow_vars.py` | SNOW | `suews_phys_snow.f95` |
| `estm_vars.py` | ESTM | `suews_phys_estm.f95` |
| `rsl_vars.py` | RSL | `suews_phys_rslprof.f95` |
| `dailystate_vars.py` | DAILYSTATE | `suews_phys_dailystate.f95` |
| `bl_vars.py` | BL | `suews_phys_bluews.f95` |
| `beers_vars.py` | BEERS | `suews_phys_beers.f95` |
| `debug_vars.py` | DEBUG | Various |
| `ehc_vars.py` | EHC | `suews_phys_ehc.f95` |
| `spartacus_vars.py` | SPARTACUS | `suews_phys_spartacus.f95` |
| `stebbs_vars.py` | STEBBS | `suews_phys_stebbs.f95` |
| `nhood_vars.py` | NHOOD | Neighbourhood |

### For Each Variable, Verify

1. **name**: Matches Fortran output column
2. **unit**: Matches Fortran `! [unit]`
3. **description**: Accurate physics description
4. **aggregation**: Correct method (AVERAGE/SUM/LAST/TIME)
5. **group**: Correct OutputGroup
6. **level**: Appropriate OutputLevel

---

## Fortran-Python Alignment

| Python Location | Fortran Location | Check |
|----------------|------------------|-------|
| `ModelPhysics.storageheatmethod` | `suews_ctrl_const.f95::StorageHeatMethod` | Values match |
| `SurfaceProperties.alb` | `suews_data.f95::alb` | Default matches |
| `OHM_Coefficient_season_wetness.a1` | `suews_phys_ohm.f95::a1` | Type matches |

### Scheme Option Alignment

For each physics scheme:
1. Python Enum values match Fortran INTEGER constants
2. Documentation describes all valid options
3. Default consistent across all three

---

## Enum Alignment

| Python Enum | Fortran Location | Doc Location |
|-------------|------------------|--------------|
| `OutputGroup` | N/A (Python-only) | Output files docs |
| `OutputLevel` | N/A (Python-only) | Output files docs |
| `AggregationMethod` | N/A (Python-only) | Output files docs |
| `SurfaceType` | `suews_ctrl_const.f95` | Input parameter docs |
| `TimezoneOffset` | N/A (Python-only) | Configuration docs |
| `*Method` enums | `suews_ctrl_const.f95` | Parameterisation docs |

---

## Unit Notation Conventions

| Style | Example | Used In |
|-------|---------|---------|
| ASCII superscript | `W m^-2` | Pydantic json_schema_extra |
| Minus sign | `W m-2` | Output variables |
| Slash | `W/m2` | Fortran comments |

All equivalent - normalise for comparison.
