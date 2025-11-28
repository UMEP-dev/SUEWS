---
name: lint-code
description: Check code style against SUEWS conventions for all languages (Fortran and Python). Use when reviewing code, before committing changes, or when asked to check style/conventions. For Fortran validates file/module/type/subroutine naming, units, precision. For Python validates variable prefixes (df_, dict_), config separation, logging, pathlib, type hints, NumPy docstrings. Complements ruff and fprettify with SUEWS-specific patterns.
---

# SUEWS Code Style Checker

Check code against `dev-ref/CODING_GUIDELINES.md` and `dev-ref/FORTRAN_NAMING_CONVENTIONS.md`.

---

## Fortran Style

### Quick Reference

| Element | Pattern | Example |
|---------|---------|---------|
| File | `suews_<category>_<name>.f95` | `suews_phys_snow.f95` |
| Module | `module_<category>_<name>` | `module_phys_snow` |
| Type | `dts_<name>` | `dts_snow_state` |
| Subroutine/Function | snake_case | `calculate_snow_density` |
| Constant | UPPERCASE | `VON_KARMAN` |
| Variable | lowercase_underscores | `air_temperature` |

Categories: `ctrl`, `phys`, `util`

### Fortran Checks

**Critical**:
1. File naming: `suews_<category>_<name>.f95`
2. Module naming: `module_<category>_<name>`
3. `IMPLICIT NONE` in all modules

**Style**:
4. Type naming: `TYPE :: dts_<name>`
5. Subroutine/function: snake_case
6. Constants: `REAL(KIND(1D0)), PARAMETER :: UPPERCASE`
7. Variables: `lowercase_with_underscores`
8. Unit docs: `! [K]`, `! [W m-2]`, `! [-]`
9. Precision: `KIND(1D0)` or `REAL64`, never bare `REAL`

**Legacy patterns** (track for migration):
- `*_Module` suffixes, CamelCase modules
- Multiple sub-modules per physics scheme

### Fortran Examples

```fortran
! BAD
MODULE Snow_Module           ! -> module_phys_snow
TYPE :: snowstate            ! -> dts_snow_state
SUBROUTINE CalculateSnow     ! -> calculate_snow
REAL :: temperature          ! -> REAL(KIND(1D0)) :: temperature  ! [K]

! GOOD
MODULE module_phys_snow
TYPE :: dts_snow_state
SUBROUTINE calculate_snow_density
REAL(KIND(1D0)) :: temperature  ! [K]
```

---

## Python Style

Complements ruff for SUEWS-specific patterns.

### Quick Reference

| Data Type | Prefix | Example |
|-----------|--------|---------|
| DataFrame | `df_` | `df_forcing`, `df_state` |
| Dictionary | `dict_` | `dict_state`, `dict_grid` |
| List | `list_` | `list_grids`, `list_years` |
| Series | `ser_` | `ser_var`, `ser_output` |
| Path | `path_` | `path_runcontrol` |

### Python Checks

**Critical**:
1. **Config separation**: No config objects in low-level functions
   ```python
   # BAD: def save_supy(df_output, config): ...
   # GOOD: def save_supy(df_output, freq_s: int = 3600): ...
   ```

2. **Deep copy**: Use `copy.deepcopy()` for mutable state

**Style**:
3. Variable prefixes: `df_`, `dict_`, `list_`, `ser_`, `path_`
4. Logging: `logger_supy` not `print()`
5. pathlib: `Path` not `os.path`
6. Type hints: Complete for public functions
7. NumPy docstrings (not Google style)
8. British English: organise, analyse, colour, initialise

### Python Exceptions

- CLI files (`cmd/`) may use `print()`
- Test files have relaxed requirements
- Generated files excluded (`_suews_driver.py`, `_version.py`)

---

## Output Format

```
[code-style] Analysis

=== Fortran ===
  suews_phys_snow.f95:
    L12: MODULE Snow_Module -> module_phys_snow
    L45: REAL :: temp -> REAL(KIND(1D0)) :: temperature  ! [K]

=== Python ===
  _load.py:
    L23: forcing = ... -> df_forcing = ...
    L78: os.path.join -> use pathlib

Summary: N files, M issues (X critical, Y style)
```

## Notes

- Run `ruff check` for standard Python linting
- Run `fprettify` for Fortran formatting
- October 2025 standard applies to new code
- Legacy code migrates gradually
