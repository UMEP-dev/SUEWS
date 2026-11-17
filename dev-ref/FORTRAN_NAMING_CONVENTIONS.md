# Fortran Naming Conventions for SUEWS

> **Canonical Rules Notice**
>
> Active, path-scoped rules in `.claude/` and executable CI lints supersede this reference whenever an example drifts. Always read the governing rule files first:
> - [`AGENTS.md`](../AGENTS.md) and [`.claude/rules/00-project-essentials.md`](../.claude/rules/00-project-essentials.md)
> - Fortran: [`.claude/rules/fortran/conventions.md`](../.claude/rules/fortran/conventions.md) and [`.claude/rules/fortran/error-reporting.md`](../.claude/rules/fortran/error-reporting.md)
> - Python: [`.claude/rules/python/conventions.md`](../.claude/rules/python/conventions.md), [`.claude/rules/python/api-approach.md`](../.claude/rules/python/api-approach.md), and [`.claude/rules/python/config-patterns.md`](../.claude/rules/python/config-patterns.md)
> - Rust bridge: [`.claude/rules/rust/conventions.md`](../.claude/rules/rust/conventions.md) and [`.claude/rules/rust/bridge-contract.md`](../.claude/rules/rust/bridge-contract.md)
> - Cross-layer identifier naming: [`.claude/rules/naming-convention.md`](../.claude/rules/naming-convention.md)

This document details the naming and structural conventions for Fortran code in SUEWS (`src/suews/src/`) and its migration strategy from legacy modules.

## Table of Contents
1. [Current State and Rationale](#current-state-and-rationale)
2. [File Naming](#file-naming)
3. [Module Naming](#module-naming)
4. [Subroutine and Function Naming](#subroutine-and-function-naming)
5. [Variable, Constant, and Thread-Safety Rules](#variable-constant-and-thread-safety-rules)
6. [Derived-Type Naming](#derived-type-naming)
7. [Formatting and Validation Tools](#formatting-and-validation-tools)
8. [Migration Strategy](#migration-strategy)
9. [Examples and Anti-Patterns](#examples-and-anti-patterns)

---

## Current State and Rationale

### What We Have
The SUEWS Fortran source (`src/suews/src/`) contains:
- `[OK]` **Files**: Consistent `suews_<category>_<name>.f95` naming across `ctrl`, `phys`, `type`, and `util`.
- `[WIP]` **Modules**: New and migrated files use `module_<category>_<name>`, while older files still carry legacy names (`*_Module`, `METEO`, etc.) tracked for gradual migration.
- `[WIP]` **Subroutines/Functions and Types**: New code uses `snake_case` procedures and `dts_<name>` derived types; cross-layer TYPE field migrations are tracked in gh#1324 / gh#1325 / gh#1326.

### Design Principles
1. **Align module names with file categories** (`suews_<category>_<name>.f95` -> `module_<category>_<name>`).
2. **Maintain categorical organisation** (`ctrl`, `phys`, `type`, `util`).
3. **Use `snake_case`** for procedures, variables, and type components, and `UPPERCASE` for `PARAMETER` constants.
4. **Prefer one primary module per file** (avoid splintering one physics scheme across five tiny modules).
5. **Keep procedures re-entrant and thread-safe** for parallel multi-grid execution through `src/suews_bridge/`.

---

## File Naming

### Pattern
```text
suews_<category>_<name>.f95
```

### Categories
- **`ctrl`**: Control flow, main driver, constants, top-level state composition, and thread-local error state (`suews_ctrl_const.f95`, `suews_ctrl_driver.f95`, `suews_ctrl_type.f95`, `suews_ctrl_error.f95`).
- **`phys`**: Physics schemes and parameterisations (`suews_phys_snow.f95`, `suews_phys_lumps.f95`, `suews_phys_spartacus.f95`, `suews_phys_stebbs.f95`, etc.).
- **`type`**: Per-domain parameter and state derived-type definitions (`suews_type_snow.f95`, `suews_type_heat.f95`, `suews_type_hydro.f95`, `suews_type_stebbs.f95`, etc.).
- **`util`**: Shared utilities (`suews_util_datetime.f95`, `suews_util_meteo.f95`, `suews_util_profile.f95`, `suews_util_time.f95`).

### Rules
- Lowercase `snake_case`, `.f95` extension.
- Whenever you add a Fortran file or modify `USE` statements, update `meson.build` (and `src/suews/Makefile` if applicable) and regenerate `src/suews/Makefile.deps`:
  ```bash
  python scripts/suews/gen_fortran_deps.py
  ```
  (`test/core/test_fortran_deps.py` fails in CI when `Makefile.deps` is stale.)

---

## Module Naming

### Pattern
```fortran
MODULE module_<category>_<name>
```

### File vs Module Naming
- **Files** keep the `suews_` prefix (`suews_phys_snow.f95`, `suews_type_snow.f95`) to mark SUEWS source files clearly.
- **Modules** use the `module_` prefix (`MODULE module_phys_snow`, `MODULE module_type_snow`) for clean `USE` statements.

### Examples

#### Single Primary Module per File (Preferred)
```fortran
! File: suews_phys_snow.f95
MODULE module_phys_snow
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: update_snow_state
CONTAINS
   SUBROUTINE update_snow_state(state)
      ! ...
   END SUBROUTINE update_snow_state
END MODULE module_phys_snow
```

#### Avoid Splitting One Scheme into Many Sub-Modules

If procedures and constants logically belong to one physics scheme, keep them in a single `module_phys_<scheme>` module (with state/parameter types in `suews_type_<scheme>.f95` where shared with the driver and C-API bridge) rather than creating `_precision`, `_core`, `_func`, and `_couple` sub-modules inside one file.

| File | Legacy Module(s) | Target Standard Module |
|------|------------------|------------------------|
| `suews_phys_snow.f95` | `Snow_module` | `module_phys_snow` |
| `suews_phys_stebbs.f95` | `modulestebbsprecision`, `modulestebbs`, `modulestebbsfunc`, etc. | `module_phys_stebbs` |
| `suews_type_snow.f95` | (extracted from `suews_ctrl_type.f95`) | `module_type_snow` |
| `suews_util_meteo.f95` | `METEO` | `module_util_meteo` |
| `suews_util_time.f95` | `time_module` | `module_util_time` |

---

## Subroutine and Function Naming

**Pattern**: `snake_case` for all subroutines and functions, using variant-neutral verbs (`init`, `setup`, `prepare`, `complete`, `compute`, `calc`, `update`) rather than `-ise`/`-ize` spellings (see `.claude/rules/00-project-essentials.md`).

```fortran
! Subroutines
SUBROUTINE calc_snow_density(temperature, age_days, density)
SUBROUTINE update_atmospheric_state(pressure, temperature, humidity)
SUBROUTINE init_snow_arrays(nsteps, nlayers)

! Functions
FUNCTION calc_saturation_vapour_pressure(temp_k) RESULT(es_pa)
FUNCTION get_surface_resistance(lai, kdown) RESULT(rs)
FUNCTION interp_linear(x1, x2, y1, y2, x) RESULT(y)
```

For domain acronyms (`ohm`, `ehc`, `rsl`, `cbl`), prefer lowercase `snake_case` tokens (`ohm_calc_qs`, `ehc_step`, `cbl_height`) for consistency across Fortran, Rust, and Python.

---

## Variable, Constant, and Thread-Safety Rules

### Local Variables (Never Initialise in Declarations!)
**Pattern**: `lowercase_with_underscores`, explicit `KIND(1D0)` or `REAL64` precision, bare declaration followed by executable assignment.

In Fortran, initialising a local variable in its declaration (`REAL(KIND(1D0)) :: temp_val = 0.0D0`) **implicitly grants the `SAVE` attribute**: the variable becomes static, retains its value across calls, and is shared across Rayon worker threads in multi-grid runs. `scripts/lint/check_saved_fortran_locals.py` blocks this in CI (gh#1741).

```fortran
! WRONG -- implicit SAVE! Corrupts multi-grid runs and fails CI lint.
REAL(KIND(1D0)) :: temp_val = 0.0D0   ! [K]
INTEGER :: iter_count = 0

! CORRECT -- declare bare, then assign in the executable section
REAL(KIND(1D0)) :: air_temperature    ! [K]
REAL(KIND(1D0)) :: surface_resistance ! [s m-1]
REAL(KIND(1D0)) :: temp_val           ! [K]
INTEGER :: iter_count
LOGICAL :: is_snow_covered

temp_val = 0.0D0
iter_count = 0
is_snow_covered = .FALSE.
```

### No Mutable Module State (`SAVE` Forbidden)
Do **not** declare mutable module-level variables or `SAVE` state in physics, utility, or control modules.
- **Compile-time constants** -> `PARAMETER` (`UPPERCASE_WITH_UNDERSCORES`). Before converting a legacy local to `PARAMETER`, confirm no callee writes to the dummy argument it is passed to.
- **Cross-timestep or per-grid state** -> store on `SUEWS_STATE` (`dts_*` types in `suews_type_*.f95` / `suews_ctrl_type.f95`) and pass explicitly.

### Constants and Parameters
**Pattern**: `UPPERCASE_WITH_UNDERSCORES` with `PARAMETER` and unit comments:

```fortran
REAL(KIND(1D0)), PARAMETER :: STEFAN_BOLTZMANN = 5.67D-8      ! [W m-2 K-4]
REAL(KIND(1D0)), PARAMETER :: GRAVITY_ACCEL = 9.81D0          ! [m s-2]
REAL(KIND(1D0)), PARAMETER :: GAS_CONSTANT_DRY_AIR = 287.04D0 ! [J kg-1 K-1]
REAL(KIND(1D0)), PARAMETER :: SNOW_DENSITY_FRESH = 100.0D0    ! [kg m-3]
INTEGER, PARAMETER :: MAX_ITERATIONS = 100
```

### Arrays and Physical Units
Always document physical units in square brackets using plain ASCII (`[K]`, `[mm h-1]`, `[Pa]`, `[m s-1]`, `[W m-2]`, `[-]`):
```fortran
REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: surface_temperature ! [K] (nsurf)
REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: soil_moisture    ! [m3 m-3] (nsurf, nlayers)
REAL(KIND(1D0)), DIMENSION(7) :: surface_fraction                 ! [-] (nsurf)
```

---

## Derived-Type Naming

**Pattern**: `dts_<name>` (`dts` = derived type; never `_t` suffix or bare CamelCase in new types).

Default component values inside a `TYPE :: dts_<name>` block are per-instance initialisers (unlike procedure-local initialisers, they do **not** imply `SAVE`). Note that for bridged parameter types (`*_PRM`), representation initialisers must not stand in for missing user configuration -- defaults belong in the Python Pydantic data model (see [RUST_BRIDGE_ADD_PARAMETER.md](RUST_BRIDGE_ADD_PARAMETER.md) and `.claude/rules/rust/bridge-contract.md`).

```fortran
TYPE :: dts_snow_state
   REAL(KIND(1D0)) :: depth = 0.0D0          ! [m]
   REAL(KIND(1D0)) :: density = 100.0D0      ! [kg m-3]
   REAL(KIND(1D0)) :: albedo = 0.8D0         ! [-]
   REAL(KIND(1D0)) :: temperature = 273.15D0 ! [K]
   INTEGER :: age_days = 0                   ! [days]
END TYPE dts_snow_state

TYPE :: dts_fluxes
   REAL(KIND(1D0)) :: sensible_heat = 0.0D0  ! [W m-2]
   REAL(KIND(1D0)) :: latent_heat = 0.0D0    ! [W m-2]
   REAL(KIND(1D0)) :: storage_heat = 0.0D0   ! [W m-2]
   REAL(KIND(1D0)) :: net_radiation = 0.0D0  ! [W m-2]
END TYPE dts_fluxes
```

---

## Formatting and Validation Tools

### `fprettify` and `.fprettify.rc`
Format Fortran files with `make format` (which runs `fprettify` using the repo-root `.fprettify.rc`) or check diffs via `/lint-code` (`fprettify --diff`).
- Indentation is 3 spaces (`indent = 3`), `line-length = 132`.
- `.fprettify.rc` sets `case = [0, 0, 0, 0]`, which **preserves existing case** for keywords, procedures/modules, derived types, and intrinsics rather than forcing lowercase or uppercase rewrites across legacy files.

### CI Gates and Audit Helpers
- **Saved-locals gate (blocking in CI)**: `python scripts/lint/check_saved_fortran_locals.py` rejects any procedure-local declaration initialiser or `SAVE` local in `src/suews/`.
- **Module dependency graph (blocking in CI)**: `python scripts/suews/gen_fortran_deps.py` regenerates `src/suews/Makefile.deps`; verified by `pytest test/core/test_fortran_deps.py`.
- **Standalone naming audit script**: `python scripts/suews/check_naming_conventions.py [files...]` inspects file/module/procedure naming patterns during refactors.

---

## Migration Strategy

### For New Code
All new Fortran files, modules, derived types, and procedures must follow:
- File: `suews_<category>_<name>.f95` (`ctrl`, `phys`, `type`, `util`)
- Module: `MODULE module_<category>_<name>`
- Derived type: `TYPE :: dts_<name>`
- Procedures and variables: `snake_case`, explicit `KIND(1D0)`, no local declaration initialisers, no `SAVE` state
- Error handling: `set_supy_error` / `supy_error_flag()` / `modState%errorstate%report` (never `STOP` or `WRITE(*,...)`)

### For Existing Code
Migrate legacy names (`Snow_module`, `METEO`, fused TYPE field names) in dedicated refactoring PRs rather than mixing cosmetic renames into physics bug fixes (see `.claude/rules/work-sizing.md` and gh#1324 / gh#1325). When renaming an internal Fortran module across the tree:
1. Update `USE` statements across `src/suews/src/` and `src/suews_bridge/c_api/`.
2. Run `python scripts/suews/gen_fortran_deps.py` to refresh `src/suews/Makefile.deps`.
3. Rebuild and verify with `make dev && make test-smoke`.
4. Add a `[maintenance]` entry to `CHANGELOG.md` following `.claude/rules/changelog/format.md`.

---

## Examples and Anti-Patterns

### `[OK]` Good Example

```fortran
! File: src/suews/src/suews_phys_resistance.f95
MODULE module_phys_resistance
   USE module_ctrl_error_state, ONLY: set_supy_error
   IMPLICIT NONE
   PRIVATE

   REAL(KIND(1D0)), PARAMETER :: VON_KARMAN = 0.4D0  ! [-]

   TYPE :: dts_resistance_state
      REAL(KIND(1D0)) :: aerodynamic = 0.0D0    ! [s m-1]
      REAL(KIND(1D0)) :: boundary_layer = 0.0D0 ! [s m-1]
      REAL(KIND(1D0)) :: surface = 0.0D0        ! [s m-1]
   END TYPE dts_resistance_state

   PUBLIC :: dts_resistance_state
   PUBLIC :: calc_aerodynamic_resistance

CONTAINS

   SUBROUTINE calc_aerodynamic_resistance(wind_speed, height, roughness, ra)
      REAL(KIND(1D0)), INTENT(IN) :: wind_speed ! [m s-1]
      REAL(KIND(1D0)), INTENT(IN) :: height     ! [m]
      REAL(KIND(1D0)), INTENT(IN) :: roughness  ! [m]
      REAL(KIND(1D0)), INTENT(OUT) :: ra        ! [s m-1]

      REAL(KIND(1D0)) :: log_ratio

      log_ratio = 0.0D0
      IF (wind_speed <= 0.0D0 .OR. roughness <= 0.0D0 .OR. height <= roughness) THEN
         CALL set_supy_error(103, 'calc_aerodynamic_resistance: invalid wind_speed, height, or roughness')
         ra = -999.0D0
         RETURN
      END IF

      log_ratio = LOG(height / roughness)
      ra = log_ratio / (VON_KARMAN * wind_speed)
   END SUBROUTINE calc_aerodynamic_resistance

END MODULE module_phys_resistance
```

### `[X]` Anti-Patterns to Avoid

```fortran
! File: snow.f95                      ! [X] Missing suews_<category>_ prefix
MODULE Snow_Module                    ! [X] Should be MODULE module_phys_snow

   REAL(KIND(1D0)), SAVE :: prev_d    ! [X] Mutable SAVE module state breaks multi-grid threads

   TYPE :: snowstate                  ! [X] Should be TYPE :: dts_snow_state
      REAL :: d                       ! [X] Bare REAL without KIND(1D0), cryptic name, no units
   END TYPE snowstate

CONTAINS

   SUBROUTINE GetDensity(T, rho)      ! [X] CamelCase instead of snake_case
      REAL(KIND(1D0)), INTENT(IN) :: T
      REAL(KIND(1D0)), INTENT(OUT) :: rho
      REAL(KIND(1D0)) :: tmp = 0.0D0  ! [X] Declaration initialiser implies SAVE!
      IF (T < 0.0D0) STOP 'bad T'     ! [X] STOP kills Python; use set_supy_error + RETURN
      tmp = 100.0D0 + 0.5D0*T
      rho = tmp
   END SUBROUTINE GetDensity

END MODULE Snow_Module
```

---

## References

- [`.claude/rules/fortran/conventions.md`](../.claude/rules/fortran/conventions.md)
- [`.claude/rules/fortran/error-reporting.md`](../.claude/rules/fortran/error-reporting.md)
- [Fortran-Lang Best Practices](https://fortran-lang.org/learn/best_practices/)
