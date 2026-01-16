# Quick Checks Reference

## Fortran Critical

- File naming: `suews_<category>_<name>.f95`
- Module naming: `module_<category>_<name>`
- `IMPLICIT NONE` in all modules
- Precision: `KIND(1D0)` not bare `REAL`

## Python Critical

- Config separation: No config objects in low-level functions
- Variable prefixes: `df_`, `dict_`, `list_`, `ser_`, `path_`
- Logging: `logger_supy` not `print()`
- pathlib: `Path` not `os.path`

## RST Critical

- Max 3 heading levels (no `~`, `^`)
- No nested markup (`**:doc:...**` is invalid)
- Images require `:alt:` and `/assets/img/` path

## CHANGELOG Critical

- Date format: `### DD Mon YYYY`
- Categories: `[feature]`, `[bugfix]`, `[change]`, `[maintenance]`, `[doc]`

## Standard Tools

```bash
# Python
ruff check src/supy/

# Fortran
fprettify --diff src/suews/src/*.f95
```

## Output Format Example

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

=== RST ===
  docs/source/physics.rst:
    L34: Level 4 heading detected (max 3 levels allowed)

=== Markdown ===
  CHANGELOG.md:
    L12: Date format "26-Nov-2025" -> "26 Nov 2025"

Summary: N files, M issues (X critical, Y style)
```

## Notes

- Run `ruff check` for standard Python linting
- Run `fprettify` for Fortran formatting
- This skill applies SUEWS-specific patterns on top of standard tools
- October 2025 standard applies to new code; legacy code migrates gradually
