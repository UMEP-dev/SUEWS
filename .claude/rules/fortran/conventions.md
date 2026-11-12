---
paths:
  - src/suews/**/*.f9*
  - src/suews/**/*.F90
  - src/suews_bridge/c_api/**/*.f9*
---

# Fortran Conventions

Conventions for SUEWS Fortran source files and C-API facades.

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

**Categories**: `ctrl`, `phys`, `type`, `util`

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

## Code Size (heuristics, not gates)

- **One module per file.** The `suews_<category>_<name>.f95` naming implies a
  single primary module per file; do not accumulate unrelated modules in one file.
- **Keep procedures focused.** Aim to keep a new subroutine/function to a single
  calculation, roughly a few hundred lines at most; extract helpers rather than
  growing one procedure. The legacy giants (e.g. the driver output packing) are
  migration debt, not a template.
- **Prefer derived types over long argument lists.** Bundle related state into a
  `dts_*` type (or `suews_type_*.f95` state/parameter type) and pass that,
  instead of adding more positional arguments; flag procedures with very many
  arguments for a derived-type refactor.
- A change that only stays small by editing an oversized procedure is a signal to
  split off a preparatory refactor first (see `.claude/rules/work-sizing.md`).

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

## Debugging: Buffer Overrun, Output Packing, and Build Flags

When investigating "stack smashing detected", ABI mismatches, or bounds errors:

1. **Audit output packing and `ncolumnsDataOut*` widths**
   - `src/suews/src/suews_ctrl_type.f95::output_line` bundles all output group arrays sized by `ncolumnsDataOut*` in `src/suews/src/suews_ctrl_const.f95`, and `src/suews/src/suews_ctrl_driver.f95::validate_outputline_layout` checks every group's runtime size against those constants before stepping (raising fatal code `104` on mismatch).
   - Main SUEWS output packing in `src/suews/src/suews_ctrl_driver.f95::SUEWS_update_outputLine` packs sequentially via `push_scalar` / `push_vec` and verifies the final offset in `check_packed_size` / `raise_pack_overflow` (raising fatal code `104` if packed count disagrees with `ncolumnsDataOutSUEWS - 5`).
   - Other group packers such as `src/suews/src/suews_ctrl_driver.f95::EHC_update_outputLine` and `src/suews/src/suews_phys_stebbs.f95::stebbsonlinecouple` must match their `ncolumnsDataOut*` constants in `src/suews/src/suews_ctrl_const.f95` and `OUTPUT_REGISTRY` (`test/io_tests/test_output_layout_parity.py`, `test/data_model/test_output_contract.py`, `test/data_model/test_output_contract_artefacts.py`).

2. **`git log` shows triggers, not root causes**
   - A pre-existing bug can be exposed by an unrelated change (e.g. stack layout shift from resizing an adjacent array).
   - Do not assume the regression is solely in the most recent commit; check code that predates the regression window.

3. **GCC version matters**
   - CI Linux builds use GCC 14 (`manylinux2014` image).
   - Users may have GCC 15 (Arch, Fedora 42) with stricter stack-protector defaults; a "works in CI but crashes locally" report often means a latent overrun that GCC 15 catches.

4. **Compiler flags and `SUEWS_BUILD_PROFILE` (`FCFLAGS` is NOT forwarded through `make dev`)**
   - Kernel flags live in `src/suews/Makefile.gfortran` (invoked via `src/supy/run_make.py`) and C-API facade flags live in `src/suews_bridge/build.rs`, both controlled by `SUEWS_BUILD_PROFILE=release|checked`.
   - Default `release` compiles the kernel with `-O3 -g -finit-real=zero` and C-API facades with `-O2 -finit-real=zero -finit-integer=0 -finit-logical=false` (preventing uninitialised scalar/derived-type descriptor crashes under gfortran 14+, while `ALLOCATABLE` arrays and pointers still require explicit initialisation).
   - To enable `-O0 -fcheck=all` bounds/pointer/array-temp checks locally, run `SUEWS_BUILD_PROFILE=checked make dev` (or `make -C src/suews DEBUG=1` for the standalone kernel).

---

## No Implicitly Saved Locals

The Rust bridge runs one grid per worker thread through the same compiled
procedures, so any static local is shared between grids.

- **Never initialise a local in its declaration** (`LOGICAL :: flag = .FALSE.`).
  The initialiser implies `SAVE`: the value persists between calls and every
  thread shares one copy. `-frecursive` does not change this.
- Constant -> `PARAMETER`. Per-call value -> declare bare, assign at the top of
  the executable part. Value that must persist -> model state (`SUEWS_STATE`).
- Before making a value `PARAMETER`, check the callee does not assign to the
  dummy it is passed to (dummies without `INTENT` can be written; a write to a
  `PARAMETER` actual is a bus error).
- `scripts/lint/check_saved_fortran_locals.py` enforces this in CI (gh#1741).

---

## Numerical Guarding

- Comparisons like `< 0.0D0` do not catch `NaN`.
- When validating floating-point inputs that must be finite or non-missing, check `IEEE_IS_NAN(...)` explicitly (import from intrinsic `ieee_arithmetic` as needed).
- Add the `NaN` branch to the same fatal-error path as the ordinary invalid values unless the algorithm has a clearly documented recovery path.

---

## Legacy Patterns (Track for Migration)

When reviewing existing code, note these patterns for future migration:

- `*_Module` suffixes, CamelCase modules
- Multiple sub-modules per physics scheme
- Bare `REAL` without precision specification

Document in issue tracker when encountered.
