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

SUEWS-specific Python conventions. Complements ruff (`.ruff.toml`) for standard linting.

---

## Critical Rules

1. **Modern Python 3.12+ typing**: Prefer PEP 604 unions (`X | None`, `X | Y`) and built-in generics (`list[str]`, `dict[str, float]`) over legacy `typing.Optional`, `typing.Union`, `typing.List`, and `typing.Dict`
   ```python
   # BAD: Legacy Python 3.9 typing imports
   from typing import Dict, List, Optional


   def foo(x: Optional[str] = None) -> Dict[str, List[int]]: ...


   # GOOD: Python 3.12+ native syntax
   def foo(x: str | None = None) -> dict[str, list[int]]: ...
   ```
   **Why**: SUEWS requires `Python >= 3.12` (`pyproject.toml` `requires-python = ">=3.12"`, `.ruff.toml` `target-version = "py312"` with `"UP"` pyupgrade rules). Avoid adding `from __future__ import annotations` on Pydantic model modules unless verified safe, because Pydantic evaluates annotations at runtime.

2. **Config separation**: No config objects in low-level functions (see `.claude/rules/python/config-patterns.md`)
   ```python
   # BAD: def save_backend(df_output, config): ...
   # GOOD: def save_backend(df_output, freq_s: int = 3600): ...
   ```

3. **Deep copy**: Use `copy.deepcopy()` for mutable state

4. **Logging**: Use `logger_supy` not `print()`

5. **pathlib**: Use `Path` not `os.path`

6. **UTF-8 encoding**: Always specify `encoding="utf-8"` for text file operations
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
   `Path.read_text` / `Path.write_text`, and text-mode `tempfile.NamedTemporaryFile`.

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

- **Canonical naming rules**: Follow `.claude/rules/naming-convention.md` (single source of truth for `snake_case` word separation, physical-quantity-first field ordering, domain abbreviations, and schema-backed renames) and `.claude/rules/00-project-essentials.md` (variant-neutral SUEWS verbs and legacy identifier policy).
- **Local container prefixes**: Use `df_` for `DataFrame` (`df_forcing`, `df_state`), `ser_` for `Series` (`ser_output`), `dict_` for `dict` (`dict_state`), `list_` for `list` (`list_grids`), and `path_` for `Path` (`path_config`) where type clarity aids readability.

## Style Guidelines

- **Type hints**: Complete for public functions (`X | None`, `list[...]`, `dict[...]`)
- **Docstrings**: NumPy style (not Google style)
- **British English**: `organise`, `colour`, `behaviour`, `artefact` in prose, comments, and docstrings (preserving upstream numpy/scipy/pandas API spellings where required)
- **Variant-neutral identifiers**: Avoid `-ise`/`-ize` spellings in SUEWS-owned identifiers (see `.claude/rules/00-project-essentials.md`)

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
  are exempt from the function/module-length heuristics.
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

- CLI files (`src/supy/cmd/`, `scripts/`) may use `print()` or `click.echo()`
- Test files have relaxed container-prefix requirements
- Version/SCM helper files (`src/supy/_version.py`, `src/supy/_version_scm.py`) follow build-system conventions

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
