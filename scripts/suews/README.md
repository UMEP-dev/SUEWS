# SUEWS Scripts

This directory contains utility scripts for development and maintenance.

## Pytest CI metrics

**Plugin**: `pytest_ci_metrics.py`

The API test workflow loads this standalone pytest plugin to publish a compact
machine-readable artefact and append the same run's headline measurements to
the GitHub Actions step summary. The plugin observes pytest hooks only: it does
not perform a second collection or test pass.

Set `SUEWS_CI_METRICS` to the JSON output path and load the plugin explicitly:

```bash
SUEWS_CI_METRICS=ci-metrics.json \
  python -m pytest -p scripts.suews.pytest_ci_metrics test
```

When `GITHUB_STEP_SUMMARY` is set, the plugin also appends a Markdown summary.

### Schema version 1

The top-level JSON fields are:

| Field | Meaning |
|---|---|
| `schema_version` | Integer contract version, currently `1` |
| `generated_at` | UTC timestamp at artefact creation |
| `environment` | Python and GitHub runner/job identity when available |
| `result` | Pytest exit code |
| `phases` | Collection, test-loop and whole-session durations in seconds |
| `inventory` | Collected node count and SHA-256 of sorted node IDs |
| `execution` | Effective worker count and whether xdist was active |
| `warnings` | Counts grouped by warning category and message, with stable SHA-256 fingerprints |

The node fingerprint makes two CI cells directly comparable without storing
the full collection in every artefact. Warning fingerprints deliberately omit
source paths and line numbers so unchanged warnings remain grouped after code
moves. Per-worker assignments, resource-tree peaks and workflow dependency-path
analysis remain follow-up layers under issue #1623.

## Naming Convention Checker

**Script**: `check_naming_conventions.py`

Validates Fortran source files against the naming conventions defined in `dev-ref/FORTRAN_NAMING_CONVENTIONS.md`.

### Usage

```bash
# Check all Fortran files in src/suews/src/
python3 scripts/suews/check_naming_conventions.py

# Check specific files
python3 scripts/suews/check_naming_conventions.py src/suews/src/suews_phys_snow.f95

# Check multiple files
python3 scripts/suews/check_naming_conventions.py src/suews/src/suews_ctrl_*.f95

# Show informational messages (including passing checks)
python3 scripts/suews/check_naming_conventions.py --show-info

# Strict mode (treat warnings as errors)
python3 scripts/suews/check_naming_conventions.py --strict

# Generate report file
python3 scripts/suews/check_naming_conventions.py --report naming_report.txt
```

### What It Checks

- ✅ **File naming**: `suews_<category>_<name>.f95` pattern
- ✅ **Module naming**: Modules should match file names
- ✅ **Multiple modules**: Checks for appropriate suffixes (`_const`, `_types`, `_ops`, etc.)
- ⚠️ **Subroutine naming**: Public routines should use PascalCase (warning only)
- ⚠️ **Function naming**: Public functions should use PascalCase (warning only)

### Exit Codes

- `0`: All checks passed (or only warnings without `--strict`)
- `1`: Errors found (or warnings with `--strict`)

### Integration with Pre-commit

To add this checker to your pre-commit hooks, add to `.pre-commit-config.yaml`:

```yaml
repos:
  # ... other repos ...

  - repo: local
    hooks:
      - id: fortran-naming-check
        name: Fortran Naming Convention Check
        entry: python3 scripts/suews/check_naming_conventions.py
        language: python
        files: \\.f95$
        pass_filenames: true
        # Note: Only checks new/modified files
```

Then install the hook:
```bash
pip install pre-commit
pre-commit install
```

### Integration with CI/CD

Example GitHub Actions workflow (`.github/workflows/naming-check.yml`):

```yaml
name: Naming Convention Check

on:
  pull_request:
    paths:
      - 'src/suews/src/*.f95'
      - 'src/suews/src/*.f90'

jobs:
  check-naming:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Set up Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.11'

      - name: Check Fortran naming conventions
        run: |
          python3 scripts/suews/check_naming_conventions.py
```

### Current Status

As of October 2025, the codebase has:
- ✅ **File naming**: 100% compliant (all 33 files follow pattern)
- ❌ **Module naming**: ~10-20% compliant (most modules use legacy patterns)
- ⚠️ **Subroutine/function naming**: Variable compliance

See `dev-ref/FORTRAN_NAMING_CONVENTIONS.md` for the migration strategy to gradually bring legacy code into compliance.

### Development

The checker is designed to be:
- **Non-blocking** for legacy code (warnings, not errors for subroutine naming)
- **Strict** for new code (errors for module naming mismatches)
- **Helpful** (provides suggestions for fixes)
- **Informative** (shows what's correct with `--show-info`)

To modify checking behaviour, edit `scripts/suews/check_naming_conventions.py`.

## Deprecation Warning Demo

**Script**: `demo_deprecation_warning.py`

Demonstrates the deprecation warnings emitted by legacy functional helpers (`load_sample_data`, `run_supy`, `save_supy`). This script verifies that warnings correctly point to the caller's location rather than internal implementation.

### Purpose

- Manual verification tool for deprecation warning behaviour
- Works without importing the compiled `_supy_driver` extension
- Useful for developers verifying `stacklevel` parameter correctness
- Complements automated tests in `test/core/test_functional_deprecations.py`

### Usage

```bash
# Run the demo (expect a deprecation warning)
python3 scripts/suews/demo_deprecation_warning.py
```

### Expected Output

```
Calling legacy helper (expect warning referencing caller frame)...
DeprecationWarning: load_sample_data is deprecated and will be removed in a future version.
Please use SUEWSSimulation.from_config() instead.
```

The warning location should point to the `user_code()` function in the demo script, confirming that `stacklevel=3` correctly skips internal frames.

### Technical Details

- Extracts `_warn_functional_deprecation` directly from `src/supy/_supy_module.py` using AST parsing
- Avoids importing compiled extensions that may be unavailable on some machines
- Enables `DeprecationWarning` visibility (normally hidden by default)
