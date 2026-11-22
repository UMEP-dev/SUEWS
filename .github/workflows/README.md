# GitHub Actions Workflows for SUEWS

This directory contains GitHub Actions workflows for the SUEWS project.

## Available Workflows

### 1. Format Master (`format-master.yml`)
Post-merge formatting workflow that:
- Runs when Python/Fortran code is pushed to master
- Formats Python code using ruff v0.8.6
- Formats Fortran code using fprettify v0.3.7
- Commits formatting changes directly to master
- Simpler than PR-based formatting (no double-build overhead)

### 2. Build and Publish (`build-publish_to_pypi.yml`)
Automated build and publish workflow that:
- Builds wheels for multiple platforms (Linux, macOS, Windows)
- Supports Python 3.12-3.15
- Runs tests on each platform
- Publishes to TestPyPI on nightly/dev tag builds
- Publishes to PyPI on tagged releases
- Skips builds for auto-format commits (via `[skip ci]`)

**Manual Dispatch Options:**

When triggering via Actions tab → "Run workflow", you can configure:
- **matrix_config**: Choose build matrix
  - `full` - All platforms, all Python versions (default)
  - `pr` - Reduced: Linux + macOS ARM + Windows, Python 3.12 + 3.15
  - `minimal` - Linux only, Python 3.12 + 3.15
  - `custom` - Use individual platform/Python toggles
- **deploy_target**: `none` (validation) or `testpypi` (PyPI restricted to tags)
- **test_tier**: `smoke`, `core`, `cfg`, `standard`, or `all`

Every cp312-abi3 wheel also covers the current Windows QGIS 3 LTR and QGIS 4
Python line. Both use Python 3.12 on Windows; the runtime pin is
`numpy>=1.22` and the Rust bridge has no NumPy C-ABI dependency.

**Test tier routing:**
- Wheel-build jobs run `physics` tests once per selected platform/architecture.
- API cross-CPython jobs install the built wheel and run `api` tests across the
  selected Python versions. Each platform's api lane is chained behind that
  platform's wheel build (one `build-wheels-reusable.yml` call per platform),
  so it starts as soon as its own wheel exists.
- **Three layers.** Pull requests build and test on Linux only. The merge
  queue runs Linux at its full tier (standard, or physics-full for a physics-
  or numerics-change PR) with the cp312 api lane, and in parallel builds the
  Windows wheel and runs only its smoke tier, with no Windows api lane; macOS
  is not in the queue (a tests-only queue group stays Linux only). The
  nightly keeps the full matrix on master; its tracking issue
  (`report_scheduled_run`) names the PRs merged since the last green night (#2241). Tag releases build
  and test the full matrix, and every publish job needs `build_wheels` to
  succeed, so the full matrix is the release gate.
- **Linux tests run in parallel on ready pull requests and in the merge
  queue.** The Linux wheel uploads untested (`CIBW_TEST_SKIP`), then its
  tests fan out at once:
  - `physics | cp312 | manylinux x86_64 shard K-of-2` (`K-of-3` for
    physics-full) runs the physics tier in the same manylinux2014 image the
    wheel is built in, with the reference lane on, so the exact sample-output
    reproduction runs where it always has; `fortran probes | cp312 | manylinux
    x86_64` builds the Fortran probe libraries once, in parallel with the
    wheel;
  - `api | cp312 | manylinux x86_64 shard K-of-4` runs the build Python's api
    tier in four shards;
  - on a pull request, `api | cp315 | manylinux x86_64` runs the
    `core-bookend` api tier (every core api test, slow ones included, plus
    smoke) in one job; the non-core api tests on the newer Python wait for the
    nightly, and the merge queue runs the build Python only.

  `test layout` (`scripts/suews/ci_test_layout.py`) turns the call's inputs
  into those jobs; draft tiers keep the physics run inside the wheel job and
  are never sharded, and the nightly and releases are unchanged. The api lane
  and the physics shards install with uv and a cache. The lowest-direct
  dependency smoke tests run inside the Linux call as soon as its wheel exists.
- **Core tests always pass on Linux before a merge.** The `Core-test guard
  (Linux)` job (`scripts/suews/check_core_guard.py`) runs on every ready-tier
  run (standard or physics-full: ready pull requests and the merge queue) and
  `PR build validation` requires it. It reads the Linux lanes' per-test
  metrics and the suite inventory the Linux cp312 api cell records with no
  `-m` (`scripts/suews/pytest_inventory.py`), and fails unless:
  - each Linux lane ran exactly what its tier selects, with every shard
    present, disjoint and in its own hash bucket;
  - every `core` test of the lane's axis has the outcome `passed`; a skip is
    accepted only by exact node id with a reason in
    `scripts/suews/core_guard_allowlist.json`, and an entry that no longer
    applies fails;
  - `test_sample_run_reproduces_reference_exactly` passed in the physics lane.

  Two kinds of test cannot run in a Linux lane, and each needs its own
  allowlist entry: a core test marked `qgis` (no tier before merge runs
  `qgis`), by node id with the outcome `not run`, and a test module that skips
  while it is collected (`pytest.importorskip`), which contributes no node ids
  at all, by module path. **The single exemption** is a run whose paths need no build
  (`needs-build` false and no physics or numerics label), in practice a
  documentation-only change: it builds no wheel and runs no tests, so neither
  the lanes nor the guard run. Draft tiers run a subset by design and are not
  guarded; the ready run and the merge queue are.
- The `0-ci:full-matrix` label opts a pull request into every PR platform
  (Linux, macOS arm64, Windows); use it for build-system, Fortran I/O and
  path-handling changes. It is read live when the run starts, so add it and
  then re-run the workflow (or push). On such a PR the api lane runs no macOS
  cell, and the Windows cp312 cell runs as two shards (`api | cp312 | win AMD64
  shard 1-of-2` and `2-of-2`), each taking half of the selected tests by SHA-256 of
  the node id (`scripts/suews/pytest_shard.py`). The `pr_api_lane` dispatch
  input applies that pull-request api layout to a dispatch, so it can be
  measured on a branch.
- The nightly `Tolerance spread` group (`tolerance-spread-reusable.yml`, also on
  dispatch with the `tolerance_spread` input) installs the built wheel on the
  two CPython bookends per platform and records the zero-tolerance spread of
  the sample comparison; it records and never gates.
- The nightly `Python coverage (Linux cp312)` job (also on dispatch with the
  `python_coverage` input), chained inside the Linux `build-wheels-reusable.yml`
  call behind that wheel only, installs the Linux cp312 wheel, runs the whole suite
  except `qgis` on four workers under coverage, and uploads `coverage.json` and
  `coverage.xml` as the `python-coverage` artefact with a per-package summary;
  the configuration is in `pyproject.toml`, so `pytest --cov` gives the same
  numbers locally. It records and never gates.
- `smoke` excludes both `medium` and `slow`; `core` and `cfg` exclude `slow`.
  `standard` includes non-slow tests plus essential `core` regressions even
  when they are slow. All normal tiers exclude `qgis`; `all` is reserved for
  scheduled builds, tagged releases, and explicit manual validation.
- On `standard`-tier pull requests, the report-only `Test duration guard (Linux
  cp312)` job (`scripts/lint/check_test_duration.py`) normalises each API shard
  and the physics lane against `scripts/lint/test_duration_baseline.json` and
  warns when a lane grows by more than 10% or a changed test exceeds the
  cost-marker thresholds without the matching marker. It never gates merges.
  Regenerate the baseline after deliberate suite growth or hotspot speed-ups
  with `python scripts/lint/check_test_duration.py --write-baseline <run_dirs...>`.
- UMEP/QGIS tests are `api + qgis`: they are not physics backend tests, and
  run only when `all` reaches the Windows + Python 3.12 compatibility cell or
  when selected explicitly.

### 3. GitHub Pages Deploy (`pages-deploy.yml`)
Deploys static site content to GitHub Pages at suews.io:
- Deploys `site/` directory content on push to master
- Creates ephemeral PR previews at `/preview/pr-{number}/`
- Manual dispatch for force redeploys
- Simple, reliable design with no wget dependencies

**Content served:**
- Landing page (`site/index.html`)
- Brand assets (`site/brand/`)
- CNAME configuration for suews.io

**PR Previews:**
Previews are ephemeral - they exist only until the next deployment. Each deployment includes full production content from the repository (single source of truth).

### 4. Debug cibuildwheel (`cibuildwheel-debug.yml`)
Interactive debugging environment for cibuildwheel build issues with SSH access, Claude Code CLI integration, and comprehensive error capture.

**Key Features:**
- Manual triggering with validated platform/architecture combinations
- SSH sessions at multiple stages (before-build, after-failure, always)
- AI-assisted debugging with Claude Code CLI
- Detailed logging and artifact collection

**Quick Start:**
Actions tab → "Debug cibuildwheel with SSH" → Run workflow → Select platform-arch combination

> 📋 **Full documentation and usage examples** are available in the workflow file comments at the top of `cibuildwheel-debug.yml`

## Disabled Workflows

The following workflows have been disabled to prevent conflicts:
- `auto-format.yml.disabled` - Previously formatted master branch and created PRs
- `fprettify.yml.disabled` - Previously ran on all branches except master
- `ruff-format.yml.disabled` - Previously ran on PRs and non-master branches
- `check-master-files.yml.disabled` - Legacy workflow

These have been replaced by the unified `auto-format.yml` workflow that runs only on master.

## Configuration

### Required Secrets
- `RTD_PUSH_TOKEN`: PAT used only for syncing the protected `rtd` branch
- `SUEWS_AGENT_PUSH_TOKEN`: fine-grained PAT used only by
  `sync-agent-plugin.yml` to publish the generated `UMEP-dev/suews-agent`
  repository. Scope it to `UMEP-dev/suews-agent` with Contents read/write.


## Best Practices

1. **Security:**
   - Keep API keys secure in GitHub Secrets
   - Pin every external GitHub Action to a full commit SHA, never a tag or branch
   - Set `persist-credentials: false` on `actions/checkout` unless the job must push
   - Prefer Trusted Publishers (OIDC) over static registry tokens for package publishing

## Troubleshooting

- For build failures, check the tmate debugging session (15 min timeout)
- For auto-formatting issues, check the format-master workflow logs
