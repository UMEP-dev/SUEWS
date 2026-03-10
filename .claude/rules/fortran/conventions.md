---
paths:
  - src/suews/**/*.f9*
  - src/suews/**/*.f90
  - src/suews/**/*.f95
---

# Fortran Conventions

Conventions for SUEWS Fortran source files.

---

## Naming Patterns

| Element | Pattern | Example |
|---------|---------|---------|
| File | `suews_<category>_<name>.f95` | `suews_phys_snow.f95` |
| Module | `module_<category>_<name>` | `module_phys_snow` |
| Type | `dts_<name>` | `dts_snow_state` |
| Subroutine/Function | snake_case | `calculate_snow_density` |
| Constant | UPPERCASE | `VON_KARMAN` |
| Variable | lowercase_underscores | `air_temperature` |

**Categories**: `ctrl`, `phys`, `util`

---

## Critical Rules

1. **File naming**: `suews_<category>_<name>.f95`
2. **Module naming**: `module_<category>_<name>`
3. **IMPLICIT NONE**: Required in all modules
4. **Precision**: Always use `KIND(1D0)` or `REAL64`, never bare `REAL`

---

## Style Guidelines

- **Type naming**: `TYPE :: dts_<name>` (dts = derived type)
- **Constants**: `REAL(KIND(1D0)), PARAMETER :: UPPERCASE`
- **Variables**: `lowercase_with_underscores`
- **Unit documentation**: `! [K]`, `! [W m-2]`, `! [-]`

---

## Examples

```fortran
! BAD
MODULE Snow_Module           ! -> module_phys_snow
TYPE :: snowstate            ! -> dts_snow_state
SUBROUTINE CalculateSnow     ! -> calculate_snow
REAL :: temperature          ! -> REAL(KIND(1D0)) :: temperature  ! [K]

! GOOD
MODULE module_phys_snow
   IMPLICIT NONE

   TYPE :: dts_snow_state
      REAL(KIND(1D0)) :: temperature  ! [K]
      REAL(KIND(1D0)) :: density      ! [kg m-3]
   END TYPE

CONTAINS

   SUBROUTINE calculate_snow_density(state)
      TYPE(dts_snow_state), INTENT(INOUT) :: state
      ! Implementation
   END SUBROUTINE

END MODULE
```

---

## Debugging: Buffer Overrun / Stack Smashing

When investigating "stack smashing detected" or similar buffer overruns:

1. **Audit ALL output arrays, not just the changed ones**
   - For each `ncolumnsDataOut*` constant in `suews_ctrl_const.f95`, count the actual elements in the corresponding array constructor in `suews_ctrl_driver.f95`
   - The SUEWS output packing is around line 3500+ in `suews_ctrl_driver.f95`
   - The STEBBS output packing is in `suews_phys_stebbs.f95` (around line 892)
   - `output_line` type in `suews_ctrl_type.f95` bundles all output arrays

2. **`git log` shows triggers, not root causes**
   - A pre-existing bug can be exposed by an unrelated change (e.g. stack layout shift from resizing an adjacent array)
   - Don't assume the regression is solely in the most recent commit
   - Check code that PREDATES the regression window

3. **GCC version matters**
   - CI builds use GCC 14 (manylinux2014 image)
   - Users may have GCC 15 (Arch, Fedora 42) which has stricter stack protector defaults
   - A "works in CI but crashes locally" report often means a latent overrun that GCC 15 catches

4. **`FCFLAGS` is NOT forwarded through `make dev`**
   - Meson-python ignores the `FCFLAGS` env var; flags are hard-coded in `meson.build`
   - To add `-fcheck=bounds`, modify `meson.build` directly (the `fast_build` flag set)

---

## Legacy Patterns (Track for Migration)

When reviewing existing code, note these patterns for future migration:

- `*_Module` suffixes, CamelCase modules
- Multiple sub-modules per physics scheme
- Bare `REAL` without precision specification

Document in issue tracker when encountered.
