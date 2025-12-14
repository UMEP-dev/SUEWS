---
name: lint-code
description: Check code style against SUEWS conventions for all languages (Fortran, Python, RST, Markdown). Use when reviewing code, before committing changes, or when asked to check style/conventions. Complements ruff and fprettify with SUEWS-specific patterns. Conventions are defined in .claude/rules/.
---

# SUEWS Code Style Checker

Check code and documentation against project conventions.

**Conventions are defined in `.claude/rules/`** - these auto-load when editing relevant files.

---

## Workflow

### 1. Identify File Types

| File Type | Rules Location |
|-----------|----------------|
| Fortran (.f95, .f90) | `.claude/rules/fortran/conventions.md` |
| Python (.py) | `.claude/rules/python/conventions.md` |
| RST (.rst) | `.claude/rules/docs/conventions.md` |
| CHANGELOG.md | `.claude/rules/changelog/format.md` |

### 2. Run Standard Tools First

```bash
# Python
ruff check src/supy/

# Fortran
fprettify --diff src/suews/src/*.f95
```

### 3. Apply SUEWS-Specific Checks

Check files against the relevant rules in `.claude/rules/`:

**Fortran Critical**:
- File naming: `suews_<category>_<name>.f95`
- Module naming: `module_<category>_<name>`
- `IMPLICIT NONE` in all modules
- Precision: `KIND(1D0)` not bare `REAL`

**Python Critical**:
- Config separation: No config objects in low-level functions
- Variable prefixes: `df_`, `dict_`, `list_`, `ser_`, `path_`
- Logging: `logger_supy` not `print()`
- pathlib: `Path` not `os.path`

**RST Critical**:
- Max 3 heading levels (no `~`, `^`)
- No nested markup (`**:doc:...**` is invalid)
- Images require `:alt:` and `/assets/img/` path

**CHANGELOG Critical**:
- Date format: `### DD Mon YYYY`
- Categories: `[feature]`, `[bugfix]`, `[change]`, `[maintenance]`, `[doc]`

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

=== RST ===
  docs/source/physics.rst:
    L34: Level 4 heading detected (max 3 levels allowed)

=== Markdown ===
  CHANGELOG.md:
    L12: Date format "26-Nov-2025" -> "26 Nov 2025"

Summary: N files, M issues (X critical, Y style)
```

---

## Notes

- Run `ruff check` for standard Python linting
- Run `fprettify` for Fortran formatting
- This skill applies SUEWS-specific patterns on top of standard tools
- October 2025 standard applies to new code; legacy code migrates gradually
