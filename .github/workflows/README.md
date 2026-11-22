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
