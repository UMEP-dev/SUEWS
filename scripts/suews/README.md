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
Since the api-workers lane, every worker record carries `peak_rss_bytes` (the
worker process's own peak RSS on Linux, macOS and Windows) and `resources`
carries `controller_peak_rss_bytes`; both are additive to schema v2.

| Field | Meaning |
|---|---|
| `schema_version` | Integer contract version, currently `2` |
| `generated_at` | UTC timestamp at artefact creation |
| `environment` | Python and GitHub runner/job identity when available |
| `result` | Pytest exit code and stable passed/failed/skipped/xfailed/xpassed counts |
| `phases` | Collection, test-loop and whole-session wall durations in seconds |
| `inventory` | Collected node count and SHA-256 of sorted node IDs |
| `execution` | Effective worker count, xdist flag and worker timeline |
| `resources` | Process-tree CPU seconds and peak resident bytes (Linux), plus the controller's own peak RSS, each with availability metadata |
| `warnings` | Counts grouped by normalised warning fingerprint, retaining one raw sample message |
| `tests` | One record per collected test: node id, marker names and their `reason=` keywords, outcome, and wall and CPU seconds for the setup, call and teardown phases (plus their total) |

Per-test CPU is the `os.times()` delta (process plus reaped children) taken
around each phase in the executing process; under xdist the worker carries it
to the controller as `TestReport` attributes. `cpu_seconds` and `wall_seconds`
are keyed by phase. The `medium` and `slow` cost markers are checked against
these records by `scripts/lint/check_cost_markers.py`.

For xdist, each `execution.workers` record contains the assigned node IDs,
their count/hash, `busy_duration_seconds`, `finished_at_seconds` and
`peak_rss_bytes`, the worker process's own peak resident size reported through
xdist's worker output when the worker finishes.
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
Windows process-tree records are explicitly unavailable rather than reported as
zero. The per-process peaks (`controller_peak_rss_bytes` and each worker's
`peak_rss_bytes`) are available on all three platforms: `getrusage(RUSAGE_SELF)`
on Linux and macOS, `GetProcessMemoryInfo` on Windows. They cover one process
each and exclude its children.

Warning grouping replaces workspace/temp roots, memory addresses and UUIDs in
the fingerprint input. The first unmodified message remains in `message` for
diagnosis, and the grouped representation is in `normalised_message`. Other
numbers and paths are preserved so scientifically different warnings do not
collapse together.

The total across `warnings[].count` is a tracked number. On 9 September 2026 it
fell from 9261 to 690 in the cp312 API lane, of which the `DeprecationWarning`
share fell from 8754 to 684, when the `from_df_state` reconstructors stopped
passing deprecated field names to their own constructors and the test fixtures
moved to current names; a supy `DeprecationWarning` is now an error under
`filterwarnings` in `pyproject.toml`. The deprecation share is now about 684 on the
cp312 API lane, almost all of it from the tests that opt out because their
subject is the deprecated surface itself; expect it stable at that baseline. When the
total rises above it, read the new fingerprints rather than the total: a fixture or an
internal caller has regressed to a deprecated spelling (fix the caller, do not
add a filter), or a dependency has started warning (pin or adapt). Widening the
`filterwarnings` opt-out list is the last resort, and only for a test whose
subject is the deprecated surface itself.

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
and wheel SHA-256, and runs the standard physics selection in
the fixed order metrics-off/on/on/off. Every run uses four workers,
`--maxprocesses=4`, work stealing, a unique base temporary directory and no
pytest cache. It uploads four raw JSON files, captured logs and a comparison
manifest. The median measured test-phase overhead must be no more than 2%.

This manual same-job result is the acceptance evidence. Ordinary PR runs and
unrelated historical runs cannot establish the overhead bound because GitHub
host load is uncontrolled.

## Tolerance spread of the sample comparison

**Script**: `tolerance_spread.py`

`test/core/test_sample_output.py` accepts each output variable of the
full-year sample run within a tolerance, and every tolerance there is a bare
number: nothing records the cross-platform and cross-CPython spread the numbers
are meant to absorb. This script records that spread so the tolerances can be
derived from it.

```bash
# Run the full-year sample comparison with every tolerance set to zero
python scripts/suews/tolerance_spread.py measure \
  --output tolerance-spread/tolerance-spread-manylinux-x86_64-cp312.json

# Print the spread across a set of artefacts beside the current tolerance
python scripts/suews/tolerance_spread.py summarise artefacts/ [--markdown]
```

`measure` reuses the test module's loader, variable list, run helpers and
deviation arithmetic (`deviation_arrays`), so the numbers are exactly what the
comparator sees. Per variable it records the maximum absolute and maximum
relative deviation, the timestamp and grid where each occurs, the actual and
expected values there, the count of exactly equal points, and the tolerance the
test currently resolves on that platform and CPython. The header carries the
platform key, CPython version, supy version, git SHA (`--git-sha`, else
`GITHUB_SHA`, else `git rev-parse HEAD`) and Fortran build profile
(`--build-profile`, else `SUEWS_BUILD_PROFILE`, else `unknown`; the wheel does
not expose it). The exit code is 0 however large the spread is. When
`GITHUB_STEP_SUMMARY` is set, the same table is appended as Markdown.

The `tolerance_spread` job in `build-publish_to_pypi.yml` runs `measure` on
every scheduled run, on each built platform for the two CPython bookends, and
uploads one artefact per cell named
`tolerance-spread-<platform>-<arch>-<cpXY>` (retained 30 days). The same job
runs on `workflow_dispatch` when the `tolerance_spread` input is set. It never
fails the nightly: it feeds neither `report_scheduled_run` nor the PR gate.

`summarise` accepts artefact files or directories (the layout `gh run download
<id> --pattern 'tolerance-spread-*'` produces), lists the artefacts with their
SHA, supy version and build profile, warns when they span more than one SHA,
and prints per variable the largest absolute and relative deviation across all
of them, which artefact and timestamp produced each, and the range of current
tolerances. Deriving a tolerance from the spread is deliberately left to the
reader of that table.

## Hosted pytest scheduler comparison

**Workflow**: `Hosted pytest scheduler ABBA`

The manual workflow compares `loadscope` and `worksteal` without changing the
fixed GitHub-hosted worker budget. It downloads one successful
`cp312-manylinux-x86_64` wheel, checks out the exact SHA that produced it, and
runs the same `physics and (core or not slow)` nodes four times in A/B/B/A order:

1. `loadscope`
2. `worksteal`
3. `worksteal`
4. `loadscope`

Every trial uses `-n auto --maxprocesses=4`, a fresh `--basetemp`, and a
disabled pytest cache provider. The workflow is `workflow_dispatch` only so
ordinary pull requests never pay for four full physics runs. Dispatch requires
the ID of a successful Build and Publish workflow run and its exact source SHA.

`compare_scheduler_runs.py` consumes the four schema-v2 metrics artefacts. It
fails closed when node inventories, outcomes or effective worker counts differ;
when any worker assignment or timing fingerprint is inconsistent; or when the
hosted run does not resolve exactly four workers. `worksteal` is accepted only
when both its median finish spread and its median tail over the worker median
are strictly lower than `loadscope`, and its median session duration is no more
than 5% above `loadscope`. The median-session limit is configurable through the
comparison CLI, while the hosted workflow fixes it at 5% so dispatches cannot
silently weaken the decision rule.

Policy v2 uses the maximum process-tree RSS observed in either replicate, not
only the median. The hard memory gate requires at least 20% headroom against
the runner's measured `/proc/meminfo` capacity. The challenger's maximum
peak-RSS regression is still calculated and reported against a 10% advisory;
exceeding that advisory produces a prominent summary warning but does not fail
an otherwise safe comparison. The CLI and workflow name this setting
`peak-rss-regression-advisory-fraction` so it cannot be mistaken for a hard
relative gate. The uploaded schema-v2 decision manifest records the policy
version, both memory signals, session and test-phase medians, worker-tail
deltas, source SHA, and the SHA-256 of the exact downloaded wheel.

The formal run `29462683875` and its uploaded schema-v1 manifest remain an
immutable failure under policy v1's 10% hard relative-RSS gate. Policy v2 is a
prospective correction: the raw trials may be re-evaluated under its
hosted-runner headroom criterion, but the v1 manifest must not be rewritten or
described as having passed.

### The `api-workers` lane

The same workflow has a second lane, selected with the `lane` dispatch input
(`physics-scheduler` is the default and runs the comparison above). `api-workers`
measures whether the api lane can leave one process: on Linux, Windows and macOS
it installs the exact `cp312-<platform>-<arch>` wheel and `suews-mcp-dist` from
the source run and runs the standard api selection
(`api and (core or not slow) and not qgis`) four times in S/P/P/S order:
serial, `-n N --dist worksteal`, `-n N --dist worksteal`, serial, with N = 4 on
Linux and Windows and 2 on macOS (7 GB runners). Every trial writes a schema-v2
metrics artefact under `api-abba/`.

`summarise_abba_trials.py` tabulates those artefacts (`--trial LABEL=PATH` in run
order) into the step summary and `api-abba/summary.json`: session and test-phase
wall time, exit code and outcome counts, the Linux process-tree peak RSS, the
largest single-process peak (a worker's, or the serial controller's), the sum of
worker peaks, and the worker finish skew and tail over the median. It reports
and does not gate; a trial that never wrote its artefact appears as `missing`,
which is what a runner-killed worker lane looks like. Per-process peaks come
from the plugin's `peak_rss_bytes` worker field and `controller_peak_rss_bytes`
resource (`getrusage` on POSIX, `GetProcessMemoryInfo` on Windows); they cover
one process each, so only the Linux process-tree sample bounds the whole tree.

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
