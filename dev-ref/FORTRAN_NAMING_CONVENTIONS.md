# Fortran Naming Conventions for SUEWS

This document establishes the official naming conventions for all Fortran code in SUEWS. These conventions aim to provide consistency, maintainability, and clear organisation as the codebase evolves.

## Table of Contents
1. [Current State and Rationale](#current-state-and-rationale)
2. [File Naming (Established)](#file-naming-established)
3. [Module Naming (New Standard)](#module-naming-new-standard)
4. [Subroutine and Function Naming](#subroutine-and-function-naming)
5. [Variable and Constant Naming](#variable-and-constant-naming)
6. [Type Naming](#type-naming)
7. [Migration Strategy](#migration-strategy)
8. [Validation Tools](#validation-tools)

---

## Current State and Rationale

### What We Have
As of October 2025, the SUEWS Fortran source (`src/suews/src/`) contains:
- ✅ **Files**: Perfectly consistent (33 files)
- ❌ **Modules**: 7+ different naming patterns across ~83 modules
- ⚠️ **Subroutines/Functions**: 3-4 inconsistent patterns

### Why This Matters
- **Discoverability**: Developers should instantly know which file contains which module
- **Maintainability**: Consistent patterns reduce cognitive load
- **Tooling**: Automated checks and refactoring tools work better with conventions
- **Onboarding**: New contributors learn the codebase faster

### Design Principles
1. **Align module names with file names** (one source of truth)
2. **Maintain categorical organisation** (ctrl, phys, util)
3. **Use lowercase with underscores** (match scientific Python conventions)
4. **Prefer one module per file** (simpler mental model)

---

## File Naming (Established)

**Status**: ✅ **Current and enforced**

### Pattern
```
suews_<category>_<name>.f95
```

### Categories
- **`ctrl`**: Control flow, driver, input/output, type definitions, error handling
- **`phys`**: Physics schemes and models (OHM, LUMPS, ESTM, snow, etc.)
- **`util`**: Utilities (datetime, meteo calculations, string operations, etc.)

### Examples
```
suews_ctrl_const.f95      - Constants and parameters
suews_ctrl_driver.f95     - Main driver and orchestration
suews_ctrl_type.f95       - Type definitions
suews_phys_snow.f95       - Snow physics
suews_phys_lumps.f95      - LUMPS urban scheme
suews_util_datetime.f95   - Date/time utilities
suews_util_meteo.f95      - Meteorological calculations
```

### Rules
- All lowercase
- Underscore-separated (snake_case)
- `.f95` extension (not `.f90`)
- No abbreviations in category names
- Keep `<name>` concise but descriptive

---

## Module Naming (New Standard)

**Status**: 🆕 **Adopted for all new code from October 2025**

### Pattern
```fortran
MODULE suews_<category>_<name>
```

### Rationale
Aligning module names with file names provides:
- **Perfect correlation**: `suews_phys_snow.f95` contains `MODULE suews_phys_snow`
- **Easy grepping**: Search for module by file name pattern
- **Clear ownership**: One module per file as default
- **Namespace clarity**: All SUEWS modules share `suews_` prefix

### Examples

#### Single Module per File (Preferred)
```fortran
! File: suews_phys_snow.f95
MODULE suews_phys_snow
    IMPLICIT NONE
    ! Snow physics implementation
CONTAINS
    SUBROUTINE update_snow_state(...)
    END SUBROUTINE update_snow_state
END MODULE suews_phys_snow
```

#### Multiple Modules per File (When Necessary)
```fortran
! File: suews_util_datetime.f95

! Main datetime functionality
MODULE suews_util_datetime
    IMPLICIT NONE
    ! Primary datetime operations
END MODULE suews_util_datetime

! Supporting constants (if can't be inside main module)
MODULE suews_util_datetime_const
    IMPLICIT NONE
    ! Constants used by datetime module
END MODULE suews_util_datetime_const

! Optional helper types
MODULE suews_util_datetime_types
    IMPLICIT NONE
    ! Type definitions for datetime
END MODULE suews_util_datetime_types
```

**Suffix convention** when multiple modules required:
- `_const` - Constants and parameters
- `_types` - Type definitions
- `_ops` - Operations/functions
- `_io` - Input/output routines
- `_util` - Helper utilities

### Comparison with Current Codebase

| File | Current Module(s) | Proposed Module |
|------|------------------|-----------------|
| `suews_phys_snow.f95` | `Snow_module` | `suews_phys_snow` |
| `suews_util_datetime.f95` | `mod_datetime`, `mod_strftime`, `mod_timedelta`, `mod_constants` | `suews_util_datetime` (consolidate or use `_const`, `_ops` suffixes) |
| `suews_ctrl_const.f95` | `allocateArray`, `Initial`, `data_in`, `snowMod` | Split into separate files OR use `suews_ctrl_const`, `suews_ctrl_const_arrays`, etc. |

---

## Subroutine and Function Naming

**Pattern**: `snake_case` for all subroutines and functions

**Rationale**: Consistent with file names, module names, and variable names. Matches scientific Python conventions (NumPy, SciPy). Simpler - no need to remember scope-based casing rules.

```fortran
! Subroutines
SUBROUTINE calculate_snow_density(temperature, age, density)
SUBROUTINE update_atmospheric_state(pressure, temperature, humidity)
SUBROUTINE initialise_physics_scheme(config, state)

! Functions
FUNCTION calculate_saturation_vapour_pressure(temp) RESULT(es)
FUNCTION get_surface_resistance(lai, radiation) RESULT(rs)
FUNCTION interpolate_linear(x1, x2, y1, y2, x) RESULT(y)
```

### Common Prefixes

Use clear, descriptive prefixes for clarity:

```fortran
! Calculation functions
FUNCTION calc_aerodynamic_resistance(...) RESULT(ra)
FUNCTION calc_roughness_length(...) RESULT(z0)
FUNCTION calc_beta_rsl(...) RESULT(beta)

! Initialisation/finalisation
SUBROUTINE init_snow_arrays(nsteps, nlayers)
SUBROUTINE finalise_physics_state(state)

! Update routines
SUBROUTINE update_snow_state(...)
SUBROUTINE update_thermal_conductivity(density, temp, k_eff)

! Get/set accessors (if needed)
FUNCTION get_snow_albedo(...) RESULT(alpha)
SUBROUTINE set_surface_properties(...)
```

### Well-Known Acronyms

For well-established acronyms (≤4 letters), keep uppercase with underscores:
```fortran
SUBROUTINE ohm_calculate(...)      ! Objective Hysteresis Model
SUBROUTINE estm_update(...)        ! Element Surface Temperature Method
SUBROUTINE ehc_finalise(...)       ! Element Heat Capacity
FUNCTION cbl_height(...) RESULT(h) ! Convective Boundary Layer
```

**Note**: These can also be lowercase (e.g., `ohm_calculate`) - choose one style and be consistent within a module.

### Module-Specific Routines

Prefix with module topic for clarity:
```fortran
MODULE suews_phys_snow
CONTAINS
    SUBROUTINE snow_initialise(...)   ! Initialisation
    SUBROUTINE snow_update(...)       ! Main update step
    SUBROUTINE snow_finalise(...)     ! Cleanup
    FUNCTION snow_albedo(...) RESULT(alpha)
END MODULE suews_phys_snow
```

### Summary

| Component | Convention | Example |
|-----------|------------|---------|
| All subroutines | snake_case | `update_snow_state` |
| All functions | snake_case | `calc_density` |
| Well-known acronyms | lowercase or UPPERCASE | `estm_update` or `ESTM_update` |
| Module-specific | `<topic>_<action>` | `snow_initialise` |

**Key point**: One simple rule - everything uses snake_case (except UPPERCASE for constants)

---

## Variable and Constant Naming

### Local Variables
**Pattern**: `lowercase_with_underscores`

```fortran
REAL(KIND(1D0)) :: air_temperature      ! [K]
REAL(KIND(1D0)) :: surface_resistance   ! [s m-1]
REAL(KIND(1D0)) :: latent_heat_flux     ! [W m-2]
INTEGER :: time_step_count
INTEGER :: surface_type_id
LOGICAL :: is_snow_covered
```

### Module-Level Variables
Same as local variables, but document scope:
```fortran
MODULE suews_phys_snow
    IMPLICIT NONE

    ! Module state (should be minimal, prefer passing as arguments)
    REAL(KIND(1D0)), SAVE :: prev_snow_depth = 0.0D0  ! [m]
    LOGICAL, SAVE :: is_initialised = .FALSE.
```

### Constants and Parameters
**Pattern**: `UPPERCASE_WITH_UNDERSCORES`

```fortran
! Physical constants
REAL(KIND(1D0)), PARAMETER :: STEFAN_BOLTZMANN = 5.67D-8     ! [W m-2 K-4]
REAL(KIND(1D0)), PARAMETER :: GRAVITY_ACCEL = 9.81D0         ! [m s-2]
REAL(KIND(1D0)), PARAMETER :: GAS_CONSTANT_DRY_AIR = 287.04D0 ! [J kg-1 K-1]

! Model constants
REAL(KIND(1D0)), PARAMETER :: SNOW_DENSITY_FRESH = 100.0D0   ! [kg m-3]
INTEGER, PARAMETER :: MAX_ITERATIONS = 100
REAL(KIND(1D0)), PARAMETER :: CONVERGENCE_THRESHOLD = 1.0D-6
```

### Arrays
Include dimensionality information:
```fortran
! Allocatable arrays
REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: surface_temperature  ! [K] (nsurf)
REAL(KIND(1D0)), DIMENSION(:,:), ALLOCATABLE :: soil_moisture      ! [m3 m-3] (nsurf, nlayers)

! Fixed-size arrays
REAL(KIND(1D0), DIMENSION(7) :: surface_fraction  ! [-] (nsurf)
```

### Physical Units Documentation
**Always document units in comments** using square brackets:
```fortran
REAL(KIND(1D0)) :: temperature = 0.0D0   ! [K]
REAL(KIND(1D0)) :: rainfall = 0.0D0      ! [mm h-1]
REAL(KIND(1D0)) :: pressure = 0.0D0      ! [Pa]
REAL(KIND(1D0)) :: wind_speed = 0.0D0    ! [m s-1]
REAL(KIND(1D0)) :: fraction = 0.0D0      ! [-] dimensionless
```

---

## Type Naming

### User-Defined Types
**Pattern**: `snake_case` with `_t` suffix to indicate it's a type

**Rationale**: Consistent with all other naming. The `_t` suffix makes it clear it's a type definition.

```fortran
TYPE :: snow_state_t
    REAL(KIND(1D0)) :: depth = 0.0D0              ! [m]
    REAL(KIND(1D0)) :: density = 100.0D0          ! [kg m-3]
    REAL(KIND(1D0)) :: albedo = 0.8D0             ! [-]
    REAL(KIND(1D0)) :: temperature = 273.15D0     ! [K]
    INTEGER :: age_days = 0                        ! [days]
END TYPE snow_state_t

TYPE :: atmospheric_forcing_t
    REAL(KIND(1D0)) :: temperature   ! [K]
    REAL(KIND(1D0)) :: pressure      ! [Pa]
    REAL(KIND(1D0)) :: humidity      ! [kg kg-1]
    REAL(KIND(1D0)) :: wind_speed    ! [m s-1]
END TYPE atmospheric_forcing_t

TYPE :: surface_fluxes_t
    REAL(KIND(1D0)) :: sensible_heat  ! [W m-2]
    REAL(KIND(1D0)) :: latent_heat    ! [W m-2]
    REAL(KIND(1D0)) :: ground_heat    ! [W m-2]
    REAL(KIND(1D0)) :: net_radiation  ! [W m-2]
END TYPE surface_fluxes_t
```

### Type Components
Use `lowercase_with_underscores` (same as variables):
```fortran
TYPE :: ohm_state_t
    REAL(KIND(1D0)) :: storage_heat_flux        ! [W m-2]
    REAL(KIND(1D0)) :: hysteresis_coef          ! [-]
    REAL(KIND(1D0)) :: thermal_time_constant    ! [s]
END TYPE ohm_state_t
```

### Alternative: No Suffix
If the `_t` suffix feels redundant in context, you can omit it:
```fortran
TYPE :: snow_state
TYPE :: atmospheric_forcing
TYPE :: surface_fluxes
```

Choose one convention (with or without `_t`) and be consistent throughout the codebase.

---

## Migration Strategy

### For New Code (Effective Immediately)
All new Fortran code must follow these conventions:
- New modules: `MODULE suews_<category>_<name>`
- New files: One module per file preferred
- New subroutines/functions: Follow public/private patterns
- New variables: Follow case conventions

### For Existing Code (Gradual Migration)

#### Phase 1: High-Priority Files (2025-2026)
Files actively being modified or with poor current naming:
1. Create module aliases for backward compatibility
2. Rename module to new convention
3. Update all `USE` statements in codebase
4. Remove aliases after one minor version

#### Phase 2: Medium-Priority Files (2026-2027)
Files with moderate activity or partial consistency

#### Phase 3: Low-Priority Files (2027+)
Legacy files rarely touched, migrate as opportunities arise

### Backward Compatibility Pattern
```fortran
! New standard module name
MODULE suews_phys_snow
    IMPLICIT NONE
    ! Implementation
END MODULE suews_phys_snow

! Temporary alias for backward compatibility
! TODO: Remove in version X.Y.0 (deprecated since X.(Y-1).0)
MODULE Snow_module
    USE suews_phys_snow
    ! All functionality is now in suews_phys_snow
END MODULE Snow_module
```

### Documentation Requirements
When renaming modules:
1. Add deprecation notice in module header comment
2. Update CHANGELOG with `[change]` entry
3. Update user documentation if module is referenced
4. Add migration note to release notes

---

## Validation Tools

### Automated Naming Checker

Location: `scripts/check_naming_conventions.py`

**Usage**:
```bash
# Check all Fortran files
python scripts/check_naming_conventions.py

# Check specific files
python scripts/check_naming_conventions.py src/suews/src/suews_phys_snow.f95

# Generate report
python scripts/check_naming_conventions.py --report naming_report.txt
```

**What it checks**:
- ✅ File naming matches pattern
- ✅ Module naming matches file
- ✅ Multiple modules use appropriate suffixes
- ⚠️ Public subroutines use PascalCase (warning only)
- ⚠️ Constants use UPPERCASE (warning only)

### Pre-commit Hook Integration

Add to `.pre-commit-config.yaml`:
```yaml
- repo: local
  hooks:
    - id: fortran-naming-check
      name: Fortran Naming Convention Check
      entry: python scripts/check_naming_conventions.py
      language: python
      files: \.f95$
      pass_filenames: true
```

### CI/CD Integration

Add to GitHub Actions (`.github/workflows/naming-check.yml`):
```yaml
name: Naming Convention Check
on: [pull_request]
jobs:
  check-naming:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Check Fortran Naming
        run: python scripts/check_naming_conventions.py
```

---

## Examples and Anti-Patterns

### ✅ Good Example

```fortran
! File: suews_phys_resistance.f95
MODULE suews_phys_resistance
    IMPLICIT NONE

    ! Constants
    REAL(KIND(1D0)), PARAMETER :: VON_KARMAN = 0.4D0  ! [-]

    ! Types
    TYPE :: resistance_state_t
        REAL(KIND(1D0)) :: aerodynamic    ! [s m-1]
        REAL(KIND(1D0)) :: boundary_layer ! [s m-1]
        REAL(KIND(1D0)) :: surface        ! [s m-1]
    END TYPE resistance_state_t

    PRIVATE
    PUBLIC :: resistance_state_t
    PUBLIC :: calc_aerodynamic_resistance

CONTAINS

    ! Public API - snake_case for all functions
    FUNCTION calc_aerodynamic_resistance(wind_speed, height, roughness) &
             RESULT(ra)
        REAL(KIND(1D0)), INTENT(IN) :: wind_speed  ! [m s-1]
        REAL(KIND(1D0)), INTENT(IN) :: height      ! [m]
        REAL(KIND(1D0)), INTENT(IN) :: roughness   ! [m]
        REAL(KIND(1D0)) :: ra                      ! [s m-1]

        REAL(KIND(1D0)) :: log_ratio

        log_ratio = calc_log_wind_profile(height, roughness)
        ra = log_ratio / (VON_KARMAN * wind_speed)
    END FUNCTION calc_aerodynamic_resistance

    ! Private helper - also snake_case (consistent!)
    FUNCTION calc_log_wind_profile(z, z0) RESULT(psi)
        REAL(KIND(1D0)), INTENT(IN) :: z, z0
        REAL(KIND(1D0)) :: psi
        psi = LOG(z / z0)
    END FUNCTION calc_log_wind_profile

END MODULE suews_phys_resistance
```

### ❌ Anti-Patterns to Avoid

```fortran
! File: snow.f95  ❌ Missing category and suews prefix
MODULE Snow_Module  ❌ Doesn't match file, inconsistent case

    REAL(KIND(1D0)) :: globalSnowDepth  ❌ Module state without SAVE, camelCase

    FUNCTION getDensity(T) RESULT(rho)  ❌ PascalCase inconsistent with snake_case standard
        REAL(KIND(1D0)) :: T  ❌ Single-letter non-standard variable
        REAL(KIND(1D0)) :: rho
        rho = 100 + 0.5*T  ❌ Magic numbers without explanation
    END FUNCTION

    type snowstate  ❌ Missing _t suffix, no consistency
        real :: d  ❌ No precision, single letter, no units
        integer :: t
    end type

END MODULE
```

---

## References

- [Fortran Naming Best Practices (Fortran-Lang)](https://fortran-lang.org/learn/best_practices/)
- [Modern Fortran Style Guide (Goddard Space Flight Center)](https://www.fortran90.org/src/best-practices.html)
- [Scientific Software Naming Conventions (NumPy, SciPy)](https://numpy.org/doc/stable/dev/gitwash/development_workflow.html)

---

## Revision History

- 26 Oct 2025, [TS](@sunt05): Initial version establishing module naming standard
- 26 Oct 2025, [TS](@sunt05): Added migration strategy and validation tools
