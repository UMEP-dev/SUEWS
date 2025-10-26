# SUEWS Source Code Naming Convention Analysis

## Summary

**File naming**: ✅ Excellent consistency
**Module naming**: ❌ Highly inconsistent (7+ different conventions)
**Subroutine/Function naming**: ⚠️ Moderately inconsistent (3-4 major patterns)

---

## 1. File Naming Convention

**Pattern**: `suews_<category>_<name>.f95`

**Status**: ✅ **EXCELLENT** - Fully consistent across all 33 Fortran files

### Categories
- `ctrl` (7 files): Control, driver, input/output, types
- `phys` (20 files): Physics modules (models and schemes)
- `util` (6 files): Utility functions (datetime, meteo, strings, etc.)

### Examples
```
suews_ctrl_const.f95
suews_ctrl_driver.f95
suews_phys_snow.f95
suews_phys_lumps.f95
suews_util_datetime.f95
suews_util_meteo.f95
```

**Strengths**:
- All lowercase
- Underscore-separated (snake_case)
- Clear categorical organisation
- Predictable and searchable
- Consistent `.f95` extension

---

## 2. Module Naming Convention

**Status**: ❌ **HIGHLY INCONSISTENT** - At least 7 different conventions observed

### Convention Breakdown

#### A. `_module` suffix (most common in newer physics code)
```fortran
MODULE Snow_module
MODULE AnOHM_module
MODULE AtmMoistStab_module
MODULE BLUEWS_module
MODULE CO2_module
MODULE beers_module
MODULE datetime_module
MODULE evap_module
MODULE lumps_module
MODULE resist_module
MODULE solweig_module
MODULE stebbs_module
```
**Note**: Inconsistent capitalisation (some PascalCase, some lowercase)

#### B. `mod_` prefix (utilities pattern)
```fortran
MODULE mod_clock
MODULE mod_constants
MODULE mod_datetime
MODULE mod_grav
MODULE mod_interp
MODULE mod_solver
MODULE mod_strftime
MODULE mod_timedelta
```
**Note**: Consistent lowercase with prefix

#### C. ALL UPPERCASE (legacy code pattern)
```fortran
MODULE PRECISION
MODULE METEO
MODULE SUEWS_DEF_DTS
MODULE SUEWS_Driver
MODULE NARP_MODULE
MODULE SPARTACUS_MODULE
MODULE cbl_MODULE
```
**Note**: Mix of fully uppercase and mixed case with `_MODULE`

#### D. PascalCase (no suffix/prefix)
```fortran
MODULE Initial
MODULE Thresh
MODULE VegPhenogy
MODULE WhereWhen
MODULE MathConstants
MODULE PhysConstants
MODULE MetDisagg
```

#### E. lowercase (no suffix/prefix)
```fortran
MODULE allocateArray
MODULE data_in
MODULE snowMod
MODULE time
MODULE gas
MODULE moist
MODULE resist
MODULE filename
MODULE strings
MODULE heatflux
```

#### F. `module` prefix (STEBBS subsystem)
```fortran
MODULE modulestebbs
MODULE modulestebbsfunc
MODULE modulestebbsprecision
MODULE modulesuewsstebbscouple
```

#### G. Mixed special cases
```fortran
MODULE ESTM_data
MODULE ctrl_output
MODULE run_info
MODULE gis_data
MODULE sues_data
MODULE qsort_c_module
```

### File-to-Module Mapping Issues

**No correlation** between file name and contained module names:

| File | Modules |
|------|---------|
| `suews_ctrl_const.f95` | `allocateArray`, `Initial`, `data_in`, `snowMod` |
| `suews_phys_snow.f95` | `Snow_module` |
| `suews_util_datetime.f95` | `mod_strftime`, `mod_constants`, `mod_timedelta`, `mod_datetime` |

---

## 3. Subroutine Naming Convention

**Status**: ⚠️ **MODERATELY INCONSISTENT** - 3 main patterns

### Pattern A: PascalCase (most common)
```fortran
SUBROUTINE AerodynamicResistance
SUBROUTINE AnthropogenicEmissions
SUBROUTINE BoundaryLayerResistance
SUBROUTINE DisaggregateDateTime
SUBROUTINE DisaggregateMet
```

### Pattern B: ALL_CAPS with underscores
```fortran
SUBROUTINE BEERS_cal_main
SUBROUTINE BEERS_cal_main_DTS
SUBROUTINE ESTM_ehc_initialise
SUBROUTINE ESTM_ehc_finalise
SUBROUTINE EHC_update_outputLine
```

### Pattern C: ALL_CAPS (acronyms/legacy)
```fortran
SUBROUTINE DAYLEN
SUBROUTINE CBL
SUBROUTINE ESTM
SUBROUTINE EHC
SUBROUTINE AnOHM
```

**Note**: Many subroutines use module/scheme name as prefix (e.g., `AnOHM_*`, `ESTM_*`, `CBL_*`)

---

## 4. Function Naming Convention

**Status**: ⚠️ **MODERATELY INCONSISTENT** - Similar to subroutines

### Pattern A: PascalCase
```fortran
FUNCTION NewtonPolynomial
FUNCTION SmithLambda
FUNCTION SnowDepletionCurve
FUNCTION RandomSamples
```

### Pattern B: Mixed case with underscores
```fortran
FUNCTION DisaggP_amongN
FUNCTION DisaggP_amongNMult
FUNCTION Disagg_Lin
FUNCTION WC_fraction
```

### Pattern C: ALL_CAPS
```fortran
FUNCTION EMIS_CLOUD
FUNCTION EMIS_CLOUD_SQ
FUNCTION PRATA_EMIS
FUNCTION ISURFACE
```

### Pattern D: `cal_` prefix (calculation functions)
```fortran
FUNCTION cal_beta_RSL
FUNCTION cal_beta_lc
FUNCTION cal_elm_RSL
FUNCTION cal_phim_hat
FUNCTION cal_smd_veg
FUNCTION cal_tsfc
```

### Pattern E: lowercase snake_case
```fortran
FUNCTION additionalSystemCoolingEnergy
FUNCTION additionalSystemHeatingEnergy
```

---

## 5. Recommendations

### Priority 1: Module Naming (High Impact)

**Recommended standard**: Choose ONE of these patterns:

#### Option A: `suews_<category>_<name>` (match file naming)
```fortran
! File: suews_phys_snow.f95
MODULE suews_phys_snow

! File: suews_util_datetime.f95
MODULE suews_util_datetime
MODULE suews_util_datetime_ops  ! If multiple modules needed
```

**Advantages**:
- Perfect alignment with file names
- Immediately obvious which file contains which module
- Consistent with Python package naming conventions
- Easy to search and navigate
- Clear categorical organisation

**Migration path**:
- One module per file preferred
- If multiple modules in one file, use suffix pattern (e.g., `_ops`, `_types`, `_constants`)

#### Option B: `<name>_module` suffix (continue current trend)
```fortran
MODULE snow_module
MODULE datetime_module
MODULE meteo_module
```

**Advantages**:
- Already partially adopted
- Clear MODULE identifier
- Common in Fortran codebases

**Disadvantages**:
- Loses categorical information
- Still need to enforce capitalisation consistency

---

### Priority 2: Subroutine/Function Naming (Medium Impact)

**Recommended standard**: `snake_case` for all subroutines and functions

**Rationale**: Consistency - everything uses snake_case (files, modules, variables, functions). Simpler - one rule, not multiple based on scope. Matches scientific Python conventions.

```fortran
! All routines use snake_case - public and private
SUBROUTINE calculate_snow_density(...)
SUBROUTINE init_snow_arrays(...)
FUNCTION get_atmospheric_pressure(...) RESULT(pressure)
FUNCTION calculate_thermal_conductivity(...) RESULT(k)
```

**Special cases**:
- Well-known acronyms: Can be lowercase or UPPERCASE (e.g., `ehc_update` or `EHC_update`)
- Calculation utilities: Use `calc_` prefix consistently
- Module-specific routines: Use module topic as prefix (e.g., `snow_init`, `snow_update`)
- Types: Use `_t` suffix (e.g., `snow_state_t`)

---

### Priority 3: Documentation and Transition

1. **Document current standard** in developer guide
2. **Adopt standard for all NEW code** immediately
3. **Plan gradual migration** for existing code:
   - Phase 1: New modules only
   - Phase 2: High-churn modules
   - Phase 3: Legacy modules (as touched)
4. **Create linter/checker** to enforce standards in CI
5. **Module aliases** for backwards compatibility during transition

---

## 6. Proposed Naming Style Guide

### Files
```
suews_<category>_<name>.f95
```
✅ Already implemented

### Modules
```fortran
MODULE suews_<category>_<name>
! or if multiple modules per file:
MODULE suews_<category>_<name>_<suffix>
```

### Subroutines (All)
```fortran
! All subroutines use snake_case - simple and consistent
SUBROUTINE calculate_snow_density(...)
SUBROUTINE update_atmospheric_state(...)
SUBROUTINE init_arrays(...)  ! Internal routines too

! Well-known acronyms can be lowercase or UPPERCASE
SUBROUTINE estm_update(...)  or  SUBROUTINE ESTM_update(...)
```

### Functions (All)
```fortran
! All functions use snake_case
FUNCTION calc_aerodynamic_resistance(...) RESULT(ra)
FUNCTION get_surface_albedo(...) RESULT(alpha)
FUNCTION interpolate_linear(...) RESULT(y)
```

### Types
```fortran
! snake_case with _t suffix
TYPE :: snow_state_t
TYPE :: atmospheric_forcing_t
```

### Variables
```fortran
! Parameters: ALL_CAPS
REAL(KIND=8), PARAMETER :: PI = 3.14159265358979323846

! Module variables: lowercase with underscores
REAL(KIND=8) :: air_temperature
INTEGER :: time_step_count

! Local variables: lowercase with underscores
REAL(KIND=8) :: local_var, temp_value
```

### Constants/Parameters (in modules)
```fortran
MODULE suews_phys_constants
   REAL(KIND=8), PARAMETER :: STEFAN_BOLTZMANN = 5.67e-8
   REAL(KIND=8), PARAMETER :: GRAVITY_ACCEL = 9.81
   REAL(KIND=8), PARAMETER :: GAS_CONSTANT_DRY_AIR = 287.04
END MODULE
```

---

## 7. Implementation Checklist

- [ ] Discuss and agree on standard with development team
- [ ] Document chosen standard in `.claude/reference/` or `dev-ref/`
- [ ] Create pre-commit hook or CI check for new code
- [ ] Add linter configuration (if using fprettify or similar)
- [ ] Update CONTRIBUTING.md with naming guidelines
- [ ] Create GitHub issue for gradual migration plan
- [ ] Add naming convention section to developer documentation
- [ ] Create module name mapping document for deprecation period

---

## Appendix: Current Module Inventory

Total modules found: ~83
Unique naming patterns: 7+
Files with multiple modules: ~15 (e.g., `suews_ctrl_const.f95` has 4 modules)

**Suggestion**: Prefer one-module-per-file design for better organisation.
