# Quick Checks Reference

## Fortran Critical (`.claude/rules/fortran/conventions.md`, `error-reporting.md`)

- File naming: `suews_<category>_<name>.f95`
- Module naming: `module_<category>_<name>`
- `IMPLICIT NONE` in all modules
- Precision: `KIND(1D0)` not bare `REAL`
- No implicit `SAVE` on procedure locals (no declaration initialisers like `INTEGER :: n = 0` or `=> NULL()` inside `SUBROUTINE`/`FUNCTION`; enforced by `check_saved_fortran_locals.py`)
- Thread-safe error state via `errorHint` / `suews_error_ring`, never `STOP` in library code

## Python Critical (`.claude/rules/python/conventions.md`)

- Config separation: no config objects in low-level physics/utility functions
- Variable prefixes: `df_`, `dict_`, `list_`, `ser_`, `path_`
- Logging: `logger_supy` not `print()`, plain ASCII only
- pathlib: `Path` not `os.path`, and always pass explicit `encoding="utf-8"` to `.read_text()` / `.write_text()` (enforced by `check_pathlib_encoding.py`)

## Rust & Bridge Critical (`.claude/rules/rust/conventions.md`, `bridge-contract.md`)

- `FIELD_RENAMES` in `src/suews_bridge/src/field_renames.rs` must mirror `ALL_FIELD_RENAMES` in `src/supy/data_model/core/field_renames.py`, and Rust `pub` parameter struct fields must use canonical `snake_case` (`check_rust_yaml_aliases.py`)
- Public Fortran derived types under `src/suews/src/` must be accounted for in the bridge manifest (`check_bridge_manifest.py`)
- Lint without needing a compiled Fortran library by setting `SUEWS_BRIDGE_LINT_ONLY=1`

## Tests, CI, and CHANGELOG Critical

- Every `test/**/test_*.py` (outside auto-marked `test/umep/`) declares `physics` or `api` (`check_test_markers.py`), and never inspects imported modules via `.__file__` (`check_packaged_data_paths.py`)
- `physics and slow and not core` tests match the marker block in `.claude/rules/physics-change-evidence.md` (`check_physics_tier_delta.py`)
- `CHANGELOG.md`: date format `### DD Mon YYYY`, category tags `[feature]`, `[bugfix]`, `[change]`, `[maintenance]`, `[doc]` (optional `[experimental]`, `[stable]`, `[internal]` governance tags, no trailing colon)

## Active Static and Contract Linter Commands

### Standard Formatters / Linters

```bash
# Python
ruff check
ruff format --check src test

# Fortran
fprettify --diff src/suews/src/*.f95

# Rust Clippy (lint-only mode skips linking libphys)
SUEWS_BRIDGE_LINT_ONLY=1 cargo clippy --manifest-path src/suews_bridge/Cargo.toml --all-features --all-targets -- -D warnings
```

### Argument-Free Repository Checks (Pass NO CLI Flags)

```bash
# Zero-build AST/text checks (safe in any checkout):
python scripts/lint/check_saved_fortran_locals.py
python scripts/lint/check_test_markers.py
python scripts/lint/check_packaged_data_paths.py

# Built-venv check (invokes pytest --collect-only, importing supy via test/conftest.py):
python scripts/lint/check_physics_tier_delta.py
```

### Path, Contract, and Diff Checks

```bash
python scripts/lint/check_pathlib_encoding.py src test scripts mcp docs .claude plugins
python scripts/lint/check_rust_yaml_aliases.py
python scripts/lint/check_schema_version_bump.py --base origin/master
python scripts/lint/check_data_interface_version_history.py --base origin/master
python scripts/lint/check_forcing_contract_artefacts.py
python scripts/lint/check_output_contract_artefacts.py
python scripts/lint/check_knowledge_pack_freshness.py --base origin/master

# Optional: audit medium/slow pytest markers against downloaded nightly CI metrics
python scripts/lint/check_cost_markers.py <ci-metrics-dir>
```

### Fortran, Bridge, and Shared Agent Governance Checks

```bash
# Scope check_naming_conventions.py to new or modified .f95 files (full tree has legacy pre-2025 debt):
python scripts/suews/check_naming_conventions.py <new_or_changed.f95>
python scripts/suews/check_bridge_manifest.py
python .claude/scripts/validate-claude-md.py
```

## Output Format Example

```text
[code-style] Analysis

=== Fortran ===
  suews_phys_snow.f95 (module_phys_snow):
    MODULE Snow_Module -> module_phys_snow
    REAL :: temp -> REAL(KIND(1D0)) :: temperature  ! [K]

=== Python ===
  _load.py (load_SampleData):
    forcing = ... -> df_forcing = ...
    os.path.join -> use pathlib.Path

=== Static & Contract Lints ===
  check_saved_fortran_locals: [OK]
  check_pathlib_encoding: [X] 1 call missing encoding=

=== Markdown ===
  CHANGELOG.md:
    Date format "26-Nov-2025" -> "26 Nov 2025"

Summary: N files, M issues (X critical, Y style)
```

## Notes

- Pass NO CLI flags to `check_saved_fortran_locals.py`, `check_test_markers.py`, `check_packaged_data_paths.py`, and `check_physics_tier_delta.py`.
- `check_physics_tier_delta.py` collects pytest markers using the active interpreter, so run it inside `.venv` where `supy` and `pytest` are installed.
- October 2025 naming standard applies to new code; legacy code migrates gradually.
