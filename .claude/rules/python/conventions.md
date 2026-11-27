---
paths:
  - src/supy/**/*.py
  - mcp/**/*.py
  - scripts/**/*.py
  - .github/scripts/**/*.py
  - benchmark/**/*.py
  - docs/**/*.py
---

# Python Conventions

SUEWS-specific Python conventions. Complements ruff (`.ruff.toml`) for standard linting. [fact: git ls-files --error-unmatch .ruff.toml]

---

## Critical Rules

1. **Modern Python 3.12+ typing**: Prefer PEP 604 unions (`X | None`, `X | Y`) and built-in generics (`list[str]`, `dict[str, float]`) over legacy `typing.Optional`, `typing.Union`, `typing.List`, and `typing.Dict` [decision: software maintainer, 2026-11-17]
   ```python
   # BAD: Legacy Python 3.9 typing imports
   from typing import Dict, List, Optional


   def foo(x: Optional[str] = None) -> Dict[str, List[int]]: ...


   # GOOD: Python 3.12+ native syntax
   def foo(x: str | None = None) -> dict[str, list[int]]: ...
   ```
   **Why**: SUEWS requires `Python >= 3.12` (`pyproject.toml` `requires-python = ">=3.12"`, `.ruff.toml` `target-version = "py312"` with `"UP"` pyupgrade rules). Avoid adding `from __future__ import annotations` on Pydantic model modules unless verified safe, because Pydantic evaluates annotations at runtime. [fact: git grep -n "requires-python = \">=3.12\"" pyproject.toml && git grep -n "target-version = \"py312\"" pyproject.toml .ruff.toml]

2. **Config separation**: No config objects in low-level functions (see `.claude/rules/python/config-patterns.md`) [fact: git ls-files --error-unmatch .claude/rules/python/config-patterns.md]
   ```python
   # BAD: def save_backend(df_output, config): ...
   # GOOD: def save_backend(df_output, freq_s: int = 3600): ...
   ```

3. **Deep copy**: Use `copy.deepcopy()` for mutable state

4. **Logging**: Use `logger_supy` not `print()`

5. **pathlib**: Use `Path` not `os.path`

6. **UTF-8 encoding**: Always specify `encoding="utf-8"` for text file operations [decision: software maintainer, 2026-11-17]
   ```python
   # BAD: Windows default encoding (cp1252/cp1253) breaks non-ASCII text
   with open(path, "w") as f:
       f.write(content)
   Path(output).write_text(content)

   # GOOD: Explicit UTF-8 encoding for cross-platform compatibility
   with open(path, "w", encoding="utf-8") as f:
       f.write(content)
   Path(output).write_text(content, encoding="utf-8")
   ```
   **Why**: Windows uses locale-specific encodings by default (e.g. cp1252, cp1253).
   Non-ASCII bytes (such as Unicode arrows `U+2192`) cause `UnicodeEncodeError` and
   produce truncated or empty files (see issue #1097). This covers `open()`,
   `Path.read_text` / `Path.write_text`, and text-mode `tempfile.NamedTemporaryFile`. [decision: software maintainer, 2026-11-17]

   **Enforced in CI**: `.github/workflows/encoding-audit.yml` runs two
   complementary checks over the repo on every PR touching a `.py` file:
   - ruff `PLW1514` (`unspecified-encoding`) -- covers builtin `open()`,
     text-mode `tempfile.NamedTemporaryFile`, and `read_text`/`write_text`
     whose receiver type ruff can resolve. The rule is preview-only, so
     the workflow passes `--preview` explicitly. Most violations autofix
     with `ruff check --preview --select PLW1514 --fix --unsafe-fixes .`.
   - `scripts/lint/check_pathlib_encoding.py` -- an AST check that flags
     `read_text`/`write_text`, and text-mode `Path.open()`, on *any*
     receiver (e.g. `p.write_text(...)` or `with p.open() as f:` where `p`
     is a plain variable), which PLW1514 silently skips because it cannot
     infer the type. The `.open()` check is mode-aware: it skips binary
     modes and `zipfile`/`tarfile`-style `.open(name)` calls (non-mode
     first arg) to avoid false positives. `read_text`/`write_text` are
     pathlib-specific names, so those are flagged unconditionally.

   A maintainer can label a PR `0-ci:encoding-audit-ok` to bypass when a
   flagged call is genuinely correct without UTF-8.

---

## Naming and Container Prefixes

- **Canonical naming rules**: Follow `.claude/rules/naming-convention.md` (single source of truth for `snake_case` word separation, physical-quantity-first field ordering, domain abbreviations, and schema-backed renames) and `.claude/rules/00-project-essentials.md` (variant-neutral SUEWS verbs and legacy identifier policy). [fact: git grep -n "Variant-neutral SUEWS identifiers" .claude/rules/00-project-essentials.md && git grep -n "Protected legacy identifiers" .claude/rules/00-project-essentials.md]
- **Local container prefixes**: Use `df_` for `DataFrame` (`df_forcing`, `df_state`), `ser_` for `Series` (`ser_output`), `dict_` for `dict` (`dict_state`), `list_` for `list` (`list_grids`), and `path_` for `Path` (`path_config`) where type clarity aids readability. [decision: software maintainer, 2026-11-17]

## Style Guidelines

- **Type hints**: Complete for public functions (`X | None`, `list[...]`, `dict[...]`) [decision: software maintainer, 2026-11-17]
- **Docstrings**: NumPy style (not Google style)
- **British English**: `organise`, `colour`, `behaviour`, `artefact` in prose, comments, and docstrings (preserving upstream numpy/scipy/pandas API spellings where required) [decision: software maintainer, 2026-11-17]
- **Variant-neutral identifiers**: Avoid `-ise`/`-ize` spellings in SUEWS-owned identifiers (see `.claude/rules/00-project-essentials.md`) [fact: git grep -n "Variant-neutral SUEWS identifiers" .claude/rules/00-project-essentials.md]

---

## Code Size (heuristics, not gates)

- **Single-responsibility functions.** Keep functions focused and small (aim well
  under ~75 lines); extract helpers rather than nesting deeply. ruff complexity
  lints apply where configured.
- **Cohesive modules.** Split a module that has grown to cover several unrelated
  responsibilities; prefer several small modules over one catch-all.
- **Modest argument counts.** Combined with the config-separation rule above,
  prefer small, explicit signatures; pass plain values, not config objects.
- **Exemptions.** Pydantic data-model classes with long field enumerations and
  version/SCM helper modules (`src/supy/_version.py`, `src/supy/_version_scm.py`)
  are exempt from the function/module-length heuristics. [fact: git grep -n _version_scm.py get_ver_git.py]
- A fix that only stays small by editing an oversized function is a signal to
  split off a preparatory refactor first (see `.claude/rules/work-sizing.md`).

---

## Examples

```python
# BAD
forcing = load_data(file)
os.path.join(base, "output")
print(f"Processing {file}")

# GOOD
df_forcing = load_data(path_file)
path_base / "output"
logger_supy.info("Processing %s", path_file)
```

---

## Exceptions

- CLI files (`src/supy/cmd/`, `scripts/`) may use `print()` or `click.echo()` [decision: software maintainer, 2026-11-17]
- Test files have relaxed container-prefix requirements [decision: software maintainer, 2026-11-17]
- Version/SCM helper files (`src/supy/_version.py`, `src/supy/_version_scm.py`) follow build-system conventions [fact: git ls-files --error-unmatch src/supy/_version.py]

---

## Docstring Format

NumPy-style is **machine-enforced** via `ruff check --select D` with
`convention = "numpy"` in `.ruff.toml`. New offenders fail ruff; any finding
surfaced under `/lint-code`'s `=== Docstrings ===` section is new debt.

Legacy debt is parked per-file in `[lint.per-file-ignores]` in `.ruff.toml`
(see the `# --- BEGIN legacy docstring debt (gh1294) ---` block). To clean a
file: fix its docstrings, delete its line from that block, then confirm
`ruff check --select D <file>` exits 0. Regenerate the block end-to-end via
`uv run python scripts/lint/audit_docstrings.py` if many files change at once.

```python
def calculate_flux(
    df_input: pd.DataFrame,
    site: str,
    freq_s: int = 3600,
) -> pd.DataFrame:
    """Calculate energy flux from input data.

    Parameters
    ----------
    df_input : pd.DataFrame
        Input forcing data with meteorological variables.
    site : str
        Site identifier.
    freq_s : int, optional
        Output frequency in seconds, by default 3600.

    Returns
    -------
    pd.DataFrame
        Calculated flux values.

    Examples
    --------
    >>> df_flux = calculate_flux(df_forcing, "London")
    """
```

---

## No New Physical-Constant Literals

Physical, mathematical, and unit-conversion constants in `src/supy/` belong in
`src/supy/constants.py` (`supy.constants`), aligned to double precision with
`src/suews/src/suews_ctrl_const.f95` (`module_ctrl_const_mathconst` and
`module_ctrl_const_physconst`), not in inline literals (`#1971`). [decision: software maintainer, 2026-11-27]
Cross-layer agreement between `supy.constants`,
`src/suews/src/suews_ctrl_const.f95`, and `supy.units` is checked by
`test/core/test_supy_constants.py`. [fact: git ls-files --error-unmatch src/supy/constants.py test/core/test_supy_constants.py]

`scripts/lint/check_python_literals.py` parses every `*.py` file under
`src/supy/` -- excluding `src/supy/constants.py` (whose literals are the
canonical definitions) and generated files `src/supy/_version.py` and
`src/supy/_suews_driver.py` -- and counts gated numeric literal and
multiplicative/divisive expression kinds (`273.15`/`273.16`, Stefan-Boltzmann,
`pi`, degree-radian factors and `pi / 180` / `180 / pi`, `3600`, `86400` and
`24 * 3600` / `24 * 60 * 60`, `1440`, reference pressure, `cp_air` values, and
`Lv` values) per file. Because it scans the Python AST, comments, docstrings,
string literals, booleans, identifiers, and bare integer `Enum` member codes are
ignored, while float `Enum` members, `Enum` expressions, and `Enum` methods are
still checked. [fact: git grep -n "GATED_KINDS\|CONSTANT_MODULES\|EXCLUDED_FILES\|_is_bare_int_member" scripts/lint/check_python_literals.py]

Run `make lint-python-constants` (also a prerequisite of `make test`) or
`python scripts/lint/check_python_literals.py`; the check exits non-zero
whenever any per-file count differs from
`scripts/lint/python_literals_baseline.json` (also enforced against the live
tree by `test/core/test_check_python_literals.py`). [fact: git grep -n "lint-python-constants" Makefile && git ls-files --error-unmatch scripts/lint/python_literals_baseline.json test/core/test_check_python_literals.py]
`scripts/lint/python_literals_baseline.json` holds the per-file ratchet
baseline (being zeroed in PR #2418 and wired into CI alongside
`scripts/lint/check_fortran_literals.py` in `#1971` Part 3b). [decision: software maintainer, 2026-11-27]

- **Count went up**: import the named constant from `supy.constants` (for
  example `C2K` for `273.15`, `SEC_PER_HOUR` for `3600`, `SEC_PER_DAY` for
  `86400`, `DEG2RAD` for `pi / 180`); if none exists yet, add one to
  `src/supy/constants.py` aligned with `src/suews/src/suews_ctrl_const.f95` and
  update `FORTRAN_TO_PYTHON` in `test/core/test_supy_constants.py`. [decision: software maintainer, 2026-11-27]
- **Count went down**: lower the baseline in the same diff with
  `python scripts/lint/check_python_literals.py --write-baseline`. [fact: git grep -n -- "--write-baseline" scripts/lint/check_python_literals.py]
- **Replacing a literal with a name** must preserve the existing value so
  outputs do not move; changing a constant's value or precision goes in its own
  PR classified under `.claude/rules/physics-change-evidence.md`
  (`0-numerics:change` when only constant precision or representation changes
  within the numerical threshold, or `0-physics:change` otherwise). [decision: software maintainer, 2026-11-27]
- **`--report`** (`python scripts/lint/check_python_literals.py --report`)
  prints both gated counts and report-only `factor_1000` (`1000.0`, `1e-3`) and
  `factor_1e6` (`1e6`, `1e-6`) counts and exits `0`; those scale factors are not
  gated because most occurrences are not unit conversions. [fact: git grep -n "REPORTED_KINDS\|--report" scripts/lint/check_python_literals.py]
