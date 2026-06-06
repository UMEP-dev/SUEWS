---
paths:
  - src/supy/**/*.py
---

# Python Conventions

SUEWS-specific Python conventions. Complements ruff for standard linting.

---

## Variable Prefixes

| Data Type | Prefix | Example |
|-----------|--------|---------|
| DataFrame | `df_` | `df_forcing`, `df_state` |
| Dictionary | `dict_` | `dict_state`, `dict_grid` |
| List | `list_` | `list_grids`, `list_years` |
| Series | `ser_` | `ser_var`, `ser_output` |
| Path | `path_` | `path_runcontrol` |

---

## Critical Rules

1. **Python 3.9 compatibility**: Use `Optional[X]` not `X | None` for union types
   ```python
   from typing import Optional, Union

   # BAD: PEP 604 syntax requires Python 3.10+
   def foo(x: str | None = None): ...
   def bar(items: list[int] | None): ...

   # GOOD: Works on Python 3.9+
   def foo(x: Optional[str] = None): ...
   def bar(items: Optional[list[int]]): ...
   ```
   **Why**: PEP 604 union syntax (`X | Y`) requires Python 3.10+. Pydantic evaluates
   type hints at runtime, so `from __future__ import annotations` is not a safe
   workaround. Enforced by ruff rule `FA102` (configured in `.ruff.toml`), which is
   marked `unfixable` there because its autofix would add the unsafe future import.

2. **Config separation**: No config objects in low-level functions
   ```python
   # BAD: def save_supy(df_output, config): ...
   # GOOD: def save_supy(df_output, freq_s: int = 3600): ...
   ```

3. **Deep copy**: Use `copy.deepcopy()` for mutable state

4. **Logging**: Use `logger_supy` not `print()`

5. **pathlib**: Use `Path` not `os.path`

6. **UTF-8 encoding**: Always specify `encoding="utf-8"` for file operations
   ```python
   # BAD: Windows default encoding (cp1252/cp1253) breaks Unicode
   with open(path, "w") as f:
       f.write(content)
   Path(output).write_text(content)

   # GOOD: Explicit UTF-8 encoding for cross-platform compatibility
   with open(path, "w", encoding="utf-8") as f:
       f.write(content)
   Path(output).write_text(content, encoding="utf-8")
   ```
   **Why**: Windows uses locale-specific encodings by default (e.g., cp1252, cp1253).
   Unicode characters like `→` cause `UnicodeEncodeError` and produce empty files.
   See issue #1097 for details. This also covers `read_text`/`write_text` and
   text-mode `tempfile.NamedTemporaryFile`.

   **Enforced in CI**: `.github/workflows/encoding-audit.yml` runs two
   complementary checks over the repo on every PR touching a `.py` file:
   - ruff `PLW1514` (`unspecified-encoding`) — covers builtin `open()`,
     text-mode `tempfile.NamedTemporaryFile`, and `read_text`/`write_text`
     whose receiver type ruff can resolve. The rule is preview-only, so
     the workflow passes `--preview` explicitly. Most violations autofix
     with `ruff check --preview --select PLW1514 --fix --unsafe-fixes .`.
   - `scripts/lint/check_pathlib_encoding.py` — an AST check that flags
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

## Naming

- **snake_case** for all Python identifiers (fields, variables, functions, methods)
- **Underscore between every word**: `soil_depth` not `soildepth`, `lai_max` not `laimax`
- **Domain abbreviations** are single tokens separated by underscores: `ohm_inc_qf`, `rsl_method`
- **No redundant suffixes**: avoid appending `method`/`model` when the enum type already conveys this
- **YAML surface is snake_case throughout** — the pre-gh#1334 STEBBS PascalCase exception is retired; cross-layer naming (Fortran TYPE members, Rust struct fields) is tracked in #1324/#1325/#1326
- See `00-project-essentials.md` for full naming rules and legacy migration policy

## Style Guidelines

- **Type hints**: Complete for public functions
- **Docstrings**: NumPy style (not Google style)
- **British English**: organise, analyse, colour, initialise
- **Variant-neutral**: Avoid -ise/-ize spellings in identifiers (see project essentials)

---

## Examples

```python
# BAD
forcing = load_data(file)
os.path.join(base, 'output')
print(f"Processing {file}")

# GOOD
df_forcing = load_data(path_file)
path_base / 'output'
logger_supy.info("Processing %s", path_file)
```

---

## Exceptions

- CLI files (`cmd/`) may use `print()`
- Test files have relaxed requirements
- Generated files excluded: `_suews_driver.py`, `_version.py`

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
    freq_s: int = 3600
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
