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

## Legacy Patterns (Track for Migration)

When reviewing existing code, note these patterns for future migration:

- `*_Module` suffixes, CamelCase modules
- Multiple sub-modules per physics scheme
- Bare `REAL` without precision specification

Document in issue tracker when encountered.
