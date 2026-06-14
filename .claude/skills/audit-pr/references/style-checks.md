# Style Checks Reference

## Fortran

| Check | Pattern | Example Issue |
|-------|---------|---------------|
| File naming | `suews_<cat>_<name>.f95` | `snow.f95` → `suews_phys_snow.f95` |
| Module naming | `module_<cat>_<name>` | `Snow_Module` → `module_phys_snow` |
| IMPLICIT NONE | Required in all modules | Missing `IMPLICIT NONE` |
| Type naming | `TYPE :: dts_<name>` | `snowstate` → `dts_snow_state` |
| Precision | `KIND(1D0)` or `REAL64` | Bare `REAL` usage |
| Units | `! [K]`, `! [W m-2]` | Missing unit annotation |

## Python

| Check | Pattern | Example Issue |
|-------|---------|---------------|
| DataFrame prefix | `df_` | `forcing` → `df_forcing` |
| Dict prefix | `dict_` | `state` → `dict_state` |
| Path prefix | `path_` | `file` → `path_file` |
| Logging | `logger_supy` | Using `print()` |
| Paths | `pathlib.Path` | Using `os.path` |
| Type hints | Required for public functions | Missing hints |
| Docstrings | NumPy style | Google style |
