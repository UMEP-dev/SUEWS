---
name: lint-code
description: Check code style against SUEWS conventions for all languages (Fortran, Python, RST, Markdown). Use when reviewing code, before committing changes, or when asked to check style/conventions. For Fortran validates file/module/type/subroutine naming, units, precision. For Python validates variable prefixes (df_, dict_), config separation, logging, pathlib, type hints, NumPy docstrings, and variant-neutral naming. For RST/Markdown validates structure, spelling, and formatting. Complements ruff and fprettify with SUEWS-specific patterns.
---

# SUEWS Code Style Checker

Check code and documentation against project conventions. Human language is code too.

References:
- `dev-ref/CODING_GUIDELINES.md`
- `dev-ref/FORTRAN_NAMING_CONVENTIONS.md`

---

## Variant-Neutral Naming

Avoid British/American spelling variants (-ise/-ize) in identifiers. Use neutral alternatives.

### Common Replacements

| Avoid | Use Instead |
|-------|-------------|
| initialise/initialize | `init`, `setup`, `prepare` |
| finalise/finalize | `complete`, `finish`, `cleanup` |
| normalise/normalize | `scale`, `adjust`, `rescale` |
| analyse/analyze | `examine`, `process`, `compute` |
| optimise/optimize | `improve`, `tune`, `refine` |
| serialise/serialize | `save`, `write`, `export` |
| visualise/visualize | `plot`, `display`, `show` |
| synchronise/synchronize | `sync`, `align`, `match` |
| sanitise/sanitize | `clean`, `validate`, `check` |
| summarise/summarize | `aggregate`, `total`, `reduce` |

### Examples

```python
# BAD - variant-specific
def initialize_model(): ...
def analyse_results(): ...
def serialize_output(): ...

# GOOD - neutral alternatives
def init_model(): ...
def process_results(): ...
def save_output(): ...
```

### Exceptions

- External library methods (e.g., pandas `.normalize()`)
- Established scientific terms (e.g., vector normalization)
- Legacy code (document why spelling is retained)

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

## RST Style

reStructuredText files in `docs/source/`.

### Heading Hierarchy (MAX 3 LEVELS)

| Level | Marker | Usage |
|-------|--------|-------|
| 1 | `=` overline+underline | Document title only |
| 2 | `=` underline only | Major sections |
| 3 | `-` underline | Subsections |

- **NO Level 4+**: Do not use `~`, `^`, or deeper nesting
- OK: 3.2.2 | NOT OK: 2.3.4.5
- Underline length must match title length

### RST Checks

**Critical**:
1. **Max 3 heading levels**: No `~`, `^`, or deeper
2. **No nested markup**: Cannot combine `**` with `:doc:`, `:ref:`
3. **Images require `:alt:`**: All figures must have alt text
4. **Image path**: Must be `/assets/img/` (absolute from docs/source)
5. **Auto-generated files**: NEVER manually edit CSV tables

**Style**:
6. Admonitions: Only `note`, `warning`, `tip`, `important`
7. Code blocks: Always specify language (`python`, `bash`, `fortran`, `yaml`)
8. Cross-references: `:ref:` for sections, `:doc:` for documents, `:option:` for parameters
9. Scientific notation: `Q\ :sub:`F`` (backslash-space before `:sub:`)
10. British English: organise, analyse, colour, behaviour

### RST Terminology

| Term | Correct | Wrong |
|------|---------|-------|
| Project name | SUEWS | Suews, suews |
| Python wrapper | SuPy | supy, SUPY |
| First mention | "SUEWS (Surface Urban Energy and Water Balance Scheme)" | Just "SUEWS" |

### RST Examples

```rst
.. BAD
~~~~~~~~~~~~~~~
Deep Subsection    (Level 4 - too deep!)
~~~~~~~~~~~~~~~

.. image:: ../../random/fig.png

.. GOOD
-----------
Subsection    (Level 3 - max allowed)
-----------

.. figure:: /assets/img/SUEWS_Overview_s.png
   :alt: Overview of SUEWS

   Caption text here.
```

---

## Markdown Style

Markdown files: `README.md`, `CHANGELOG.md`, `.claude/` docs, `dev-ref/`.

### CHANGELOG Format

**Date format**: `### DD Mon YYYY` (e.g., `### 26 Nov 2025`)
- No leading zero on single-digit days
- Three-letter month abbreviation

**References**:
- Issue/PR: `(#123)` with `#`
- Commit SHA: `(abc1234)` without `#`

**Multi-line entries**:
```markdown
- [category] Main description (#123)
  - Sub-bullet with 2-space indent
  - Additional details (abc1234)
```

**CRITICAL**: NEVER modify the Annual Statistics table

### CHANGELOG Categories

| Category | Use for |
|----------|---------|
| `[feature]` | New functionality |
| `[bugfix]` | Bug fixes (link issue) |
| `[change]` | User-facing changes |
| `[maintenance]` | Internal/dev tooling, CLAUDE.md |
| `[doc]` | User documentation (not CLAUDE.md) |

### Markdown Checks

**Critical**:
1. **CHANGELOG date**: `### DD Mon YYYY` format
2. **CHANGELOG categories**: Only `[feature]`, `[bugfix]`, `[change]`, `[maintenance]`, `[doc]`
3. **SKILL.md frontmatter**: Must have `name` and `description`
4. **Blank line before lists**: Required for rendering

**Style**:
5. README headings: h2-h5 only (no h1)
6. dev-ref file naming: `UPPERCASE_UNDERSCORES.md` for guidelines, `lowercase-hyphens.md` for guides
7. dev-ref sections: `### X.Y` numbering pattern
8. Code blocks: Specify language
9. British English: organise, analyse, colour, behaviour

### Markdown Terminology

| Term | Correct | Wrong |
|------|---------|-------|
| NumPy | NumPy | numpy, Numpy |
| Pandas | Pandas | pandas, PANDAS |
| GitHub | GitHub | Github, github |
| PyPI | PyPI | Pypi, pypi |
| QGIS | QGIS | Qgis |
| UMEP | UMEP | Umep |

### Markdown Examples

```markdown
.. BAD
### 26-Nov-2025              (Wrong date format)
- [new-feature] Added X      (Wrong category)

.. GOOD
### 26 Nov 2025
- [feature] Added new validation system (#123)
  - Implemented Phase A checks (abc1234)
```

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
    L56: Image missing :alt: attribute
    L78: Image path should be /assets/img/ not ../../images/

=== Markdown ===
  CHANGELOG.md:
    L12: Date format "26-Nov-2025" -> "26 Nov 2025"
    L15: Category "[new-feature]" -> "[feature]"
  README.md:
    L45: "organized" -> "organised" (British English)
    L67: "Github" -> "GitHub" (terminology)

Summary: N files, M issues (X critical, Y style)
```

## Notes

- Run `ruff check` for standard Python linting
- Run `fprettify` for Fortran formatting
- October 2025 standard applies to new code
- Legacy code migrates gradually
