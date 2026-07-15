# SUEWS Scripts

This directory contains utility scripts for development and maintenance.

## Pytest CI metrics

**Plugin**: `pytest_ci_metrics.py`

The API and physics test workflows load this standalone pytest plugin to
publish a compact machine-readable artefact and append the same run's headline
measurements to the GitHub Actions step summary. The plugin observes pytest
hooks only: it does not perform a second collection or test pass.

Set `SUEWS_CI_METRICS` to the JSON output path and load the plugin explicitly:

```bash
SUEWS_CI_METRICS=ci-metrics.json \
  python -m pytest -p scripts.suews.pytest_ci_metrics test
```

When `GITHUB_STEP_SUMMARY` is set, the plugin also appends a Markdown summary.

### Pytest schema version 2

Schema version 2 retains the version 1 top-level fields and adds execution,
resource and warning details. It intentionally changes warning fingerprints
to hash normalised rather than raw text, so consumers must check
`schema_version` before interpreting fingerprints. The deterministic consumer
fixture is `test/fixtures/ci_metrics/schema-v2-xdist.json`.

| Field | Meaning |
|---|---|
| `schema_version` | Integer contract version, currently `2` |
| `generated_at` | UTC timestamp at artefact creation |
| `environment` | Python and GitHub runner/job identity when available |
| `result` | Pytest exit code and stable passed/failed/skipped/xfailed/xpassed counts |
| `phases` | Collection, test-loop and whole-session wall durations in seconds |
| `inventory` | Collected node count and SHA-256 of sorted node IDs |
| `execution` | Effective worker count, xdist flag and worker timeline |
| `resources` | Process-tree CPU seconds and peak resident bytes with availability metadata |
| `warnings` | Counts grouped by normalised warning fingerprint, retaining one raw sample message |

For xdist, each `execution.workers` record contains the assigned node IDs,
their count/hash, `busy_duration_seconds` and `finished_at_seconds`.
Assignment means a node ID observed in that worker's pytest reports. Busy time
is the sum of setup, call and teardown report durations. Finish time is the
arrival of the worker's last test report, relative to the controller's
test-loop start; it is not worker shutdown time. The execution object reports
latest-minus-earliest finish skew and latest-minus-median tail. A serial run
deliberately has `workers: []`, `xdist: false` and effective worker count 1.

Linux resource measurement samples the controller process and its descendants
through procfs every 0.25 seconds by default. CPU values retain the last
observed cumulative total for an exited child. Peak RSS is the largest sampled
sum of resident bytes across the live tree. `sample_count`, interval, status,
method and reason are always explicit. Short-lived processes between samples
can be missed, and procfs access/exit races are ignored safely. macOS and
Windows records are explicitly unavailable rather than reported as zero.

Warning grouping replaces workspace/temp roots, memory addresses and UUIDs in
the fingerprint input. The first unmodified message remains in `message` for
diagnosis, and the grouped representation is in `normalised_message`. Other
numbers and paths are preserved so scientifically different warnings do not
collapse together.

### Wheel-job phase evidence

Physics wheel jobs publish three files under one
`ci-metrics-physics-<python>-<platform>-<arch>` artefact:

- the schema version 2 pytest JSON;
- raw cross-process phase boundaries;
- a `wheel-job-ci-metrics` JSON combining checkout, toolchain setup, build,
  repair, install, collection, tests and session durations.

Checkout is timed around `actions/checkout`. The other build phases use
cibuildwheel's before-all, before-build, repair and before-test/test-command
boundaries. Linux writes through an explicit host-mounted metrics directory;
macOS and Windows write to the host directory directly. Missing boundaries
remain `unavailable` with a reason and can never look like measured zeroes.

### Workflow critical-path view

`analyse_ci_run.py` consumes the GitHub Actions run and Jobs REST payloads plus
`.github/ci-metrics-needs.json`. It resolves the explicit `PR build validation`
target, expands declared matrix dependencies and follows the latest-finishing
predecessor through each fan-in. The output separates:

- orchestration hand-off: dependency barrier to job creation;
- runner queue: Jobs REST `created_at` to `started_at`;
- execution: `started_at` to `completed_at`;
- dependency fan-in spread: earliest to latest predecessor completion.

The later `CI observability summary` job is excluded from the gate path. Step
durations remain available in the workflow JSON, but they do not substitute
for the explicit wheel-job phase artefact.

Checkout, setup and install times vary with GitHub cache/network/service load
and are execution-phase load, not proof of a pytest scheduling improvement.
Compare scheduler candidates in the same job with the same wheel, inventory,
worker cap and alternating order.

### Controlled overhead evidence

The manual `CI metrics overhead check` workflow runs only from the default
branch. It verifies and installs one Linux wheel once, records the source SHA
and wheel SHA-256, and runs the standard non-slow physics selection in
the fixed order metrics-off/on/on/off. Every run uses four workers,
`--maxprocesses=4`, work stealing, a unique base temporary directory and no
pytest cache. It uploads four raw JSON files, captured logs and a comparison
manifest. The median measured test-phase overhead must be no more than 2%.

This manual same-job result is the acceptance evidence. Ordinary PR runs and
unrelated historical runs cannot establish the overhead bound because GitHub
host load is uncontrolled.

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
