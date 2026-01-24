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

1. **Config separation**: No config objects in low-level functions
   ```python
   # BAD: def save_supy(df_output, config): ...
   # GOOD: def save_supy(df_output, freq_s: int = 3600): ...
   ```

2. **Deep copy**: Use `copy.deepcopy()` for mutable state

3. **Logging**: Use `logger_supy` not `print()`

4. **pathlib**: Use `Path` not `os.path`

5. **UTF-8 encoding**: Always specify `encoding="utf-8"` for file operations
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
   See issue #1097 for details.

---

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
