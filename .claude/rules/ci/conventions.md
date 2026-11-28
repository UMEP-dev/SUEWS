---
paths:
  - .github/workflows/**/*.yml
  - .github/workflows/**/*.yaml
  - .github/actions/**/*
  - .github/scripts/**/*
  - .github/path-filters.yml
  - .github/ci-metrics-needs.json
---

# CI/GitHub Actions Conventions

Patterns and guidelines for GitHub Actions workflows in this repository.

---

## Fork PR Security

GitHub's security model prevents forks from obtaining OIDC tokens for `pull_request` events. This affects deployment actions like `actions/deploy-pages@v4`.

**Fork Detection Pattern:**

```yaml
- name: Check if PR is from fork
  id: fork-check
  if: github.event_name == 'pull_request'
  run: |
    if [ "${{ github.event.pull_request.head.repo.full_name }}" != "${{ github.repository }}" ]; then
      echo "is_fork=true" >> $GITHUB_OUTPUT
      echo "::notice::PR from fork detected - deployment will be skipped"
    else
      echo "is_fork=false" >> $GITHUB_OUTPUT
    fi
```

**Conditional Step Execution:**

```yaml
- name: Deploy to GitHub Pages
  if: steps.fork-check.outputs.is_fork != 'true'
  uses: actions/deploy-pages@v4
```

---

## Workflow Re-run Behaviour

Re-running a GitHub Actions workflow uses the **same workflow file version** from the original run. To apply updated workflow logic:

- Rebase the PR branch onto the target branch
- Push a new commit
- Close and reopen the PR

**Avoid**: Assuming re-runs pick up latest workflow changes from master.

---

## Fork PR Feedback

When deployment is skipped for fork PRs, provide clear feedback via PR comments:

```yaml
- name: Comment on fork PR
  if: steps.fork-check.outputs.is_fork == 'true'
  uses: actions/github-script@v7
  continue-on-error: true  # Token limitations on fork PRs
  with:
    script: |
      github.rest.issues.createComment({
        owner: context.repo.owner,
        repo: context.repo.repo,
        issue_number: context.issue.number,
        body: '**Note:** GitHub Pages deployment was skipped because this PR is from a fork. This is a GitHub security restriction. You can test the docs locally with `make docs`.'
      })
```

Use `continue-on-error: true` due to potential token limitations on fork PRs.

---

## Maintainer Workflow Patterns

### Handling Contributor PRs

When `maintainerCanModify` is enabled on a PR, maintainers can directly rebase or push changes to the contributor's fork branch.

**Maintainer Actions:**
- Rebase to trigger fresh CI with updated workflows
- Push fixes directly to the contributor's branch
- Merge master changes before re-triggering CI

### PR Management Strategy

When fixing CI issues that affect existing PRs:

1. Create a separate PR for the workflow fix
2. Merge the fix PR first
3. Rebase the original problematic PR to trigger fresh CI
4. The rebased PR will use the corrected workflow

### Break-Glass Recovery for Faulty Base-Read Scripts

Several jobs in `.github/workflows/build-publish_to_pypi.yml` sparse-checkout `.github/scripts` at `github.event.pull_request.base.sha` (some with a `|| github.sha` fallback). [fact: git grep -n -A 5 'ref: \${{ github\.event\.pull_request\.base\.sha' .github/workflows/build-publish_to_pypi.yml]
If a faulty base-read script under `.github/scripts/**` on a required validation path (such as `determine_matrix` or `pr-gate`) lands on `master`, subsequent pull requests -- including a plain revert or fix PR for that script -- still execute the broken copy from `pull_request.base.sha`, deadlocking `PR build validation`.

When that deadlock occurs, a maintainer-approved break-glass recovery is permitted under the following rules: [decision: software maintainer, 2026-11-26]

1. **Trigger and scope:** Use this procedure only when `pull_request.base.sha` holds a faulty base-read script under `.github/scripts/**` that prevents a normal revert or fix PR from passing required CI checks. Do not use it to bypass a failing check on a feature, physics, or general code PR (`.claude/rules/autonomous-workflow.md`, "Gates"). [decision: software maintainer, 2026-11-26]
2. **Emergency PR (pin to a known-good `master` SHA):** Open a dedicated emergency PR that temporarily pins the affected `.github/scripts` checkout `ref:` in `.github/workflows/build-publish_to_pypi.yml` from `pull_request.base.sha` (with any `|| github.sha` fallback) to an immutable, full 40-character commit SHA already on `master` that predates the fault (never a branch name, tag, or unmerged commit), together with the revert or minimal fix to the faulty script under `.github/scripts/**` (or the pin alone, immediately followed by the script revert or fix PR). If pinning `pr-gate`'s checkout `ref:` trips a workflow-contract test asserting that exact expression (currently `test/core/test_dependency_floors.py`), the emergency PR may include the minimal temporary assertion update for that exact pinned SHA so the emergency PR can pass CI. State the faulty `master` commit, the known-good `master` SHA, and why a normal revert is deadlocked in the PR description, and bundle no unrelated code, test, or `.claude/**` changes. [decision: software maintainer, 2026-11-26]
3. **Human approval gate:** Explicit software maintainer approval (`MAINTAINERS.md`) is required before merge. An agent may draft the emergency PR when escalated, but never merges or enqueues it (`.claude/rules/autonomous-workflow.md`, "Gating model"). [decision: software maintainer, 2026-11-26]
4. **Immediate restore PR:** Once `master` holds the repaired script, follow immediately with a tiny restore PR that reverts the temporary `ref:` pin in `.github/workflows/build-publish_to_pypi.yml` back to its original `pull_request.base.sha` expression (`${{ github.event.pull_request.base.sha || github.sha }}` or `${{ github.event.pull_request.base.sha }}`) and restores verbatim any workflow-contract test assertion adjusted in step 2 (and nothing else). Because the restore PR runs against the updated `pull_request.base.sha`, its green `PR build validation` check confirms the repaired base-read script works before normal PR flow resumes. [decision: software maintainer, 2026-11-26]
5. **No unrelated merges while pinned:** Do not merge unrelated PRs while the `ref:` pin is active on `master`, so no unrelated change skips the current `master` gate scripts. [decision: software maintainer, 2026-11-26]

---

## Best Practices

- **Explicit fork detection**: Always check for fork PRs in workflows involving authenticated operations
- **Informative feedback**: Explain why actions are skipped (GitHub security, token limitations)
- **Avoid hardcoded assumptions**: Don't assume all PRs have the same permissions
- **Test locally first**: Fork contributors should test locally before relying on CI

---

## Anti-Patterns to Avoid

- Assuming `id-token: write` works for fork PRs
- Re-running workflows expecting new logic to apply
- Silent failures without user feedback
- Blocking fork PRs entirely when only deployment is affected
- Using negation patterns (`!`) in `dorny/paths-filter` -- they act as catch-alls (see Path Filtering section)
- Including `.github/releases/` files in the `docs` filter (they are release metadata, not documentation)

---

## Path Filtering with `dorny/paths-filter`

Filter definitions live in `.github/path-filters.yml` (external file, referenced by workflows). Use **positive include patterns only**. Do NOT use negation patterns (`!`).

### CRITICAL: Negation patterns do not work as exclusions

`dorny/paths-filter` evaluates patterns with **OR logic** (the default `some` predicate quantifier). Each pattern is evaluated independently via picomatch. A negation like `!foo/bar.py` is interpreted as "match every file that is NOT `foo/bar.py`" -- which matches virtually everything, turning the entire category into a catch-all that fires on any change.

Example of what NOT to do:

```yaml
python:
  - 'src/supy/*.py'
  - '!src/supy/run_make.py'   # BUG: matches ALL non-run_make.py files
```

The `!` pattern alone returns `true` for `.github/path-filters.yml`, `Makefile`, etc. -- any file that isn't `run_make.py`. [fact: git ls-files --error-unmatch src/supy/run_make.py]

**Why `predicate-quantifier: 'every'` doesn't help:** This v3.0.2 option requires ALL patterns to match a file. It fixes negation for single-directory categories, but breaks multi-directory categories (a file in `src/supy/` would fail because it doesn't match `src/supy/data_model/**`). [fact: git grep -n src/supy/data_model/ .github/path-filters.yml]

See [dorny/paths-filter#184](https://github.com/dorny/paths-filter/issues/184) and [#97](https://github.com/dorny/paths-filter/issues/97).

### Correct approach: positive-only patterns

Define each category with explicit positive patterns. Accept minor overlap between categories rather than attempting exclusions:

```yaml
# In .github/path-filters.yml
fortran:
  - 'src/suews/src/**'
  - 'src/suews/ext_lib/**'
  - 'src/supy/run_make.py'  # also matches python -- harmless

rust:
  - 'src/suews_bridge/src/**'
  - 'src/suews_bridge/c_api/**'

python:
  - 'src/supy/*.py'         # includes run_make.py -- acceptable overlap
  - 'src/supy/data_model/**'
  - 'src/supy/cmd/**'
```

Referenced from workflows as:

```yaml
- uses: dorny/paths-filter@v3
  id: filter
  with:
    filters: .github/path-filters.yml
```

### Current filter categories

Categories are defined in `.github/path-filters.yml`:

- **fortran** -- Fortran physics and extension source (triggers `physics-full` on PRs) [fact: git grep -n src/suews/ext_lib .github/path-filters.yml]
- **rust** -- Rust FFI bridge, `bridge-manifest.json`, and `c_api` wrappers (`src/suews_bridge/src/**`, `src/suews_bridge/c_api/**`, `src/suews_bridge/bridge-manifest.json`, `Cargo.toml`, `Cargo.lock`, `build.rs`; triggers `physics-full` on PRs) [fact: git grep -n "bridge-manifest.json" .github/path-filters.yml]
- **python** -- pure Python source and `mcp/**` (`physics-core` on PRs) [fact: git grep -n "'mcp/" .github/path-filters.yml]
- **util** -- utility modules (`src/supy/util/**`, `physics-core` on PRs) [fact: git grep -n src/supy/util/ .github/path-filters.yml]
- **build** -- build system files affecting wheel output: `meson.build`, `meson_options.txt`, `src/supy/meson.build`, `get_ver_git.py` (triggers `physics-full` on PRs) [fact: git grep -n get_ver_git.py .github/path-filters.yml]
- **pyproject** -- `pyproject.toml` (classified by content via `.github/scripts/classify_pyproject.py` in workflow) [fact: git grep -n classify_pyproject.py .github/workflows/build-publish_to_pypi.yml]
- **ci** -- CI workflows, `.github/scripts/**`, `Makefile`, `scripts/lint/**`, `scripts/security/**`, `scripts/suews/**` (`physics-core` on PRs) [fact: git grep -n scripts/lint/ .github/path-filters.yml]
- **tests** -- test files (`test/**`, `physics-core` on PRs) [fact: git grep -n "'test/" .github/path-filters.yml]
- **docs** -- documentation source
- **site** -- static site content

### Overlapping filter categories

A single file can match multiple categories. The build decision logic ORs categories and picks the strictest requirement (`fortran`, `rust`, and `build` set `NEEDS_MULTIPLATFORM=true`, selecting `physics-full` on PRs and retaining `MERGE_QUEUE_PLATFORMS` in the merge queue). [fact: git grep -n FORTRAN_CHANGED.*RUST_CHANGED.*BUILD_CHANGED .github/scripts/determine-matrix.sh]

### Testing path filters locally

```bash
./scripts/suews/test-detect-changes.sh              # compare HEAD vs origin/master
./scripts/suews/test-detect-changes.sh abc123       # compare HEAD vs specific commit
```

Requires Docker and `act` (`brew install act`).

---

## Build Caches (`actions/cache`)

The wheel build (`.github/actions/build-suews/action.yml`) caches the Rust
registry (`CARGO_HOME`) and the cargo target directory (`CARGO_TARGET_DIR`)
across jobs. Inside cibuildwheel both live where `CIBW_ENVIRONMENT` points
them: `.cargo-cache` and `.rust-target-cache` in the workspace on macOS and
Windows, and the bind-mounted `/cargo-cache` and `/rust-target-cache` on
Linux (`CIBW_CONTAINER_ENGINE` mounts the same host directories). The keying
rule below applies to this cache and to any build cache added later.

- **The key is a content hash of everything that determines the cached
  output**: platform and architecture, build profile, the compiler version
  that produced the objects (`rustc -V`, read on the host before the cache
  step; the Linux container installs that same version through
  `CIBW_ENVIRONMENT_PASS_LINUX`), and the inputs (`Cargo.lock`, `Cargo.toml`,
  `build.rs`, the bridge sources). Never a branch name, a date or a manual
  epoch.
- **A primary-key hit is never re-saved**, so anything that can change the
  output has to be in the key. The `cargo-*` entries saved on 2026-08-08 were
  restored on every run for a month while cargo recompiled all ~100 crates:
  the key omitted the compiler, and rustc had moved from 1.97.1 to 1.98.0
  with the runner image (nightly 34075203398, 7 Sep 2026).
- **`restore-keys` fall back within the same platform, profile and compiler
  only** (bridge inputs changed). There is deliberately no any-compiler
  fallback: cargo ignores objects built by another compiler but does not
  delete them, so a fallback restore would be saved back and the entry would
  grow by one object set per fallback (seen on #1787: 116 MB against 199 MB
  for the same key with and without a fallback). A fallback costs time,
  never correctness.
- **The scheduled nightly restores nothing but still saves.** Restoring
  nothing means one cold build per day exists on every platform, a
  staleness canary. Saving creates the master-scope entry whenever the key
  changes (a primary-key hit is never re-saved, so on a stable key the
  nightly's save is a no-op): pull requests and the merge queue can only
  read caches saved on their own ref or on master, and the nightly is the
  only event that builds wheels on master, so the queue warms the night
  after a key change and stays warm until the next one. Only the release
  profile is saved; the checked profile is built by the nightly alone with
  its restore skipped, so a checked entry could never be read.
- **Merge-queue runs never save.** The `gh-readonly-queue/...` ref is deleted
  with its queue entry, so a cache saved there is unreadable and only spends
  the repository's 10 GB budget.
- **The job log and step summary record the lookup** (primary key, exact hit
  or fallback key, or miss). Read those lines before believing a build was
  warm; a restored cache is not a reused cache.
- **Fortran objects are not cached.** Neither ccache nor sccache caches
  Fortran (module files are not handled; gfortran is passed through), as
  gh#1604 recorded. A direct cache of the `src/suews` objects would need the
  gfortran version in its key, which is unknown until `pacman` runs inside
  cibuildwheel, and a `touch` after restore because tar preserves the
  archived mtimes and `make` would otherwise rebuild everything.

---

## Build Workflow Triggers (`build-publish_to_pypi.yml`)

The main build workflow responds to these events:

- **Nightly cron** (2 AM UTC): Full matrix build, deployed to TestPyPI
- **Tag push** (production, e.g. `2026.1.28`): Full matrix build, deployed to PyPI
- **Tag push** (dev, e.g. `2026.1.28.dev`): Full matrix build, deployed to TestPyPI
- **Pull request**: Conditional build (path-filtered), no deployment
- **Merge queue**: Reduced matrix, no deployment
- **Manual dispatch**: Configurable matrix and deployment target
- **Push to master**: **Skipped** (PR already validated, no deployment needed)

### Docs-only changes skip wheel builds

The `needs-build` condition includes `fortran`, `rust`, `python`, `util`, `build`, `pyproject`, `ci`, and `tests` but NOT `docs` or `site`. [fact: git grep -n "RUST\" == \"true\"" .github/workflows/build-publish_to_pypi.yml] Changes that only affect documentation paths do not trigger wheel builds. Docs are validated by the separate `pages-deploy.yml` workflow.

### Release notes are not documentation

Files in `.github/releases/` are release metadata consumed by the GitHub Release creation step. They must NOT be included in the `docs` filter category. If release notes changes need to trigger builds, place them in `cfg` or a dedicated filter.

### Concurrency and cancel-in-progress

The workflow uses `cancel-in-progress: true` scoped to `workflow-PR_number_or_ref`. When pushing multiple commits in quick succession (e.g., during a rebase), earlier runs are cancelled before later runs start. This can cause a race condition where no run completes if pushes arrive faster than jobs start.

Mitigate by:
- Waiting for CI to begin before pushing again
- Squashing commits before pushing
- Using `git push --force-with-lease` once rather than multiple pushes

---

## Multi-Axis CI Topology

`build-publish_to_pypi.yml` runs the `4+1` dependency-axis topology under
`GATE_AXIS_SCHEMA: '2'` (#2349), with canonical per-lane marker expressions in
`scripts/suews/ci_tiers.py` (`--schema 2`), mirrored inline for the wheel-build
physics step in `.github/actions/build-suews/action.yml` and checked for parity
by `test/core/test_core_guard.py`. [fact: git grep -n "GATE_AXIS_SCHEMA: '2'" .github/workflows/build-publish_to_pypi.yml && git grep -n 'SCHEMA_V2_AXES = ("tooling", "unit", "native", "physics")' scripts/suews/ci_tiers.py && git grep -n "inputs.test_tier == 'physics-core'" .github/actions/build-suews/action.yml]

- **One required check.** Every axis reports through the existing required
  check `PR build validation`; no required check is added or renamed. [decision: software maintainer, 2026-11-26] [fact: git grep -n "name: PR build validation" .github/workflows/build-publish_to_pypi.yml]
- **From source at t=0 (`tooling`, `unit`).** `tooling` and `unit` start with
  the workflow in `test-source-lane-reusable.yml` on Linux (`ubuntu-latest`,
  `cp312`) directly from a source checkout (`SUEWS_SOURCE_LANE_ONLY=1`),
  without waiting for a wheel or MCP build (`unit` runs in 2 shards on pull
  requests and the merge queue). [decision: software maintainer, 2026-11-26] [fact: git grep -n "uses: \$/.github/workflows/test-source-lane-reusable.yml" .github/workflows/build-publish_to_pypi.yml && git grep -n "SUEWS_SOURCE_LANE_ONLY" .github/workflows/test-source-lane-reusable.yml]
- **`native` on one Python per OS.** Chained behind each platform's wheel build
  in `build-wheels-reusable.yml` (via `test-wheel-lane-reusable.yml`), `native`
  runs once per built OS on the abi3 build floor (`cp312`; 2 shards on Linux
  for pull requests and the merge queue), not across Python versions, because
  the same abi3 binary is under test. [decision: software maintainer, 2026-11-26] [fact: git grep -n "uses: \$/.github/workflows/test-wheel-lane-reusable.yml" .github/workflows/build-wheels-reusable.yml]
- **`abi3_smoke` across non-floor CPythons.** An installed-wheel `abi3_smoke`
  lane (`-m "smoke_bridge and not (medium or slow) and not qgis"`) covers the
  other interpreters: `cp315` on pull requests, `cp313` to `cp315` on the
  nightly, full manual dispatch, and tag releases (omitting `macos-15-intel` on
  those triggers due to runner scarcity), and skipped on the merge queue
  (`abi3_smoke_buildplat=[]`, since ready-PR CI already exercises the newest
  bookend). [decision: software maintainer, 2026-11-26] [fact: git grep -n "ABI3_SMOKE_EXPRESSION" scripts/suews/ci_tiers.py && git grep -n "abi3_smoke_python=" .github/scripts/determine-matrix.sh]
- **`os_compat` on built OSes.** The `os_compat` slice runs on `cp312` across
  every built platform (Windows and macOS as well as Linux); the rest of
  `tooling` and `unit` runs on Linux only. [decision: software maintainer, 2026-11-26] [fact: git grep -n "run_os_compat:" .github/workflows/build-publish_to_pypi.yml]
- **Full `physics` selected automatically on exactly four triggers:** (1) pull
  requests that touch Fortran, Rust or the build (the `fortran`, `rust` and
  `build` filter categories); (2) pull requests labelled `0-physics:change` or
  `0-numerics:change`; (3) the merge queue; (4) the nightly. Every other
  wheel-building Python pull request runs `physics-core`: the smoke tests plus
  `test/core/test_sample_output_exact.py` (about 50 s). Outside that automatic
  selection, tag releases keep the full matrix at tier `all`, a manual
  dispatch keeps its `test_tier` input, and the publish jobs require every axis
  to have succeeded. [decision: software maintainer, 2026-11-26] [fact: git grep -n "PHYSICS_FULL_TRIGGER=" .github/scripts/determine-matrix.sh && git grep -n "test_sample_run_reproduces_reference_exactly" .github/actions/build-suews/action.yml]

---

## Test Tier Definitions

Test tiers control which pytest markers run in each lane under
`GATE_AXIS_SCHEMA=2`. Markers are declared in `pyproject.toml`; per-lane marker
expressions live in `scripts/suews/ci_tiers.py` (`--schema 2`) and are consumed
by `test-source-lane-reusable.yml`, `test-wheel-lane-reusable.yml`,
`build-wheels-reusable.yml`, and `.github/actions/build-suews/action.yml`. [fact: git grep -n "ci_tiers.py --schema 2" .github/workflows/test-source-lane-reusable.yml .github/workflows/test-wheel-lane-reusable.yml]

The `<lane>` expressions below apply to the four primary axes (`tooling`,
`unit`, `native`, `physics`) and `os_compat`; the `abi3_smoke` lane always runs
the fixed expression `smoke_bridge and not (medium or slow) and not qgis` at
every tier (`ABI3_SMOKE_TIERS`). [fact: git grep -n 'ABI3_SMOKE_EXPRESSION = "smoke_bridge and not (medium or slow) and not qgis"' scripts/suews/ci_tiers.py && git grep -n "ABI3_SMOKE_TIERS:" scripts/suews/ci_tiers.py]

- **smoke** (`<lane> and smoke and not (medium or slow)`, plus `and not qgis` on non-physics lanes) -- minimal fail-fast wheel validation (used on manual dispatch and for the Windows wheel's physics step in the merge queue) [fact: git grep -n '"smoke": "physics and smoke and not (medium or slow)"' scripts/suews/ci_tiers.py && git grep -n '"smoke": f"{axis} and smoke and not (medium or slow) and not qgis"' scripts/suews/ci_tiers.py]
- **core** (`<lane> and (smoke or core) and not slow`, plus `and not qgis` on non-physics lanes) -- manual-dispatch core subset [fact: git grep -n '"core": "physics and (smoke or core) and not slow"' scripts/suews/ci_tiers.py && git grep -n '"core": f"{axis} and (smoke or core) and not slow and not qgis"' scripts/suews/ci_tiers.py]
- **cfg** (`<lane> and (smoke or cfg) and not slow`, plus `and not qgis` on non-physics lanes) -- manual-dispatch config/schema subset [fact: git grep -n '"cfg": "physics and (smoke or cfg) and not slow"' scripts/suews/ci_tiers.py && git grep -n '"cfg": f"{axis} and (smoke or cfg) and not slow and not qgis"' scripts/suews/ci_tiers.py]
- **physics-core** -- default PR tier outside the automatic full-physics triggers: physics runs `physics and smoke and not (medium or slow)` plus `test/core/test_sample_output_exact.py::test_sample_run_reproduces_reference_exactly` (~50 s); `tooling`, `unit`, `native`, and `os_compat` run `(core or not slow) and not qgis` [fact: git grep -n '"physics-core": "physics and smoke and not (medium or slow)"' scripts/suews/ci_tiers.py && git grep -n '"physics-core": f"{axis} and (core or not slow) and not qgis"' scripts/suews/ci_tiers.py && git grep -n "test_sample_run_reproduces_reference_exactly" .github/actions/build-suews/action.yml]
- **standard** (`<lane> and (core or not slow)`, plus `and not qgis` on non-physics lanes) -- manual-dispatch tier running all non-slow tests plus essential `core` regressions on primary and `os_compat` lanes [fact: git grep -n '"standard": "physics and (core or not slow)"' scripts/suews/ci_tiers.py && git grep -n '"standard": f"{axis} and (core or not slow) and not qgis"' scripts/suews/ci_tiers.py]
- **physics-full** -- widens the physics axis to full `-m physics` (including `slow`, split into 3 shards on Linux PRs and the merge queue) while `tooling`, `unit`, `native`, and `os_compat` keep `(core or not slow) and not qgis`. Selected automatically on pull requests touching `fortran`/`rust`/`build`, pull requests carrying `0-physics:change` or `0-numerics:change` (gh#1576), and the merge queue; the fourth automatic full-physics coverage trigger, the nightly schedule, runs tier **all**. [fact: git grep -n '"physics-full": "physics"' scripts/suews/ci_tiers.py && git grep -n "0-numerics:change" .github/workflows/build-publish_to_pypi.yml && git grep -n "MERGE_TIER=physics-full" .github/scripts/determine-matrix.sh && git grep -n "test_tier=all" .github/scripts/determine-matrix.sh]
- **all** (`<lane>` with no cost or `qgis` filter on primary and `os_compat` lanes; `abi3_smoke` retains its fixed `smoke_bridge` expression) -- full suite including `slow` and `qgis` tests (nightly, tag push, default manual dispatch) [fact: git grep -n '"all": "physics"' scripts/suews/ci_tiers.py && git grep -n '"all": axis,' scripts/suews/ci_tiers.py]

Importance and cost are independent. The cost markers are defined by the CPU
seconds of the test body on the Linux reference runner (`cp312`), which the
metrics plugin records per test: `medium` is 10 to 30 CPU seconds, `slow` is 30
or more, or unsuitable for routine fast runs for a stated non-CPU reason
(`pytest.mark.slow(reason="...")`). Absence of either cost marker means fast.
The nightly `cost_markers` job (`scripts/lint/check_cost_markers.py`, also on
`workflow_dispatch` with the `cost_markers` input) checks all four Linux
`cp312` lanes (`tooling`, `unit`, `native`, `physics`), reports every marker
that disagrees with the measurement, and never gates; on a pull request that
changes test files, `cost_markers_pr` runs the same lint on those files alone
and adds the result to the CI summary comment, again without gating. [fact: git grep -n "cost_markers_pr:" .github/workflows/build-publish_to_pypi.yml && git grep -n -- "--require-axis physics" .github/workflows/build-publish_to_pypi.yml] The
thresholds and their reasoning are in `.claude/rules/tests/patterns.md`.

---

## Test Tier Assignment by Event

The `determine_matrix` job (`build-publish_to_pypi.yml`, invoking
`.github/scripts/determine-matrix.sh` with `GATE_AXIS_SCHEMA=2`) assigns
platforms, Python versions, and tiers based on event type and change
categories:

### Pull requests (draft and ready)

Under `GATE_AXIS_SCHEMA=2`, pull requests (both draft and ready) follow the
same automatic trigger rules with no draft exemption: [fact: git grep -n "PR_KIND=" .github/scripts/determine-matrix.sh]

- **Platform matrix:** `MINIMAL_PLATFORMS` (1: Linux x86_64) by default; adding
  the `0-ci:full-matrix` label opts the PR into `PR_PLATFORMS` (3: Linux
  x86_64, macOS ARM64, Windows AMD64). [fact: git grep -n 'PR_MATRIX_PLATFORMS="$MINIMAL_PLATFORMS"' .github/scripts/determine-matrix.sh && git grep -n 'PR_MATRIX_PLATFORMS="$PR_PLATFORMS"' .github/scripts/determine-matrix.sh]
- `fortran`, `rust`, or `build`-system changes: test tier **physics-full**
  (`full_physics=true`, `physics_full_trigger=compiled_or_build_change`). [fact: git grep -n "PHYSICS_FULL_TRIGGER=compiled_or_build_change" .github/scripts/determine-matrix.sh]
- `python`, `util`, `tests`, or `ci` changes: test tier **physics-core**
  (`tooling`, `unit`, `native`, and `os_compat` run their full ready suites;
  `physics` runs smoke + exact sample-output check). [fact: git grep -n "TIER=physics-core" .github/scripts/determine-matrix.sh]
- **`0-physics:change` / `0-numerics:change` label override (gh#1576):** forces
  `build_wheels` (even when `needs-build` is false) and test tier
  **physics-full** (`physics_full_trigger=classification_label`), requiring a
  green wheel build in `pr-gate` so the full `-m physics` suite (incl. `slow`)
  gates the PR. [fact: git grep -n "physics_change:" .github/workflows/build-publish_to_pypi.yml && git grep -n "PHYSICS_FULL_TRIGGER=classification_label" .github/scripts/determine-matrix.sh]
- **Python versions:** `source_python` (`tooling`, `unit`) and `native_python`
  (`native`, `os_compat`, `physics`) run on the abi3 build floor (`cp312`);
  `abi3_smoke_python` runs on the non-floor bookend (`cp315`). [fact: git grep -n "exclude_floor_python" .github/scripts/determine-matrix.sh && git grep -n "abi3_smoke_python=" .github/scripts/determine-matrix.sh]

### Merge queue (`merge_group`)

- Package, build, or CI changes (`fortran`, `rust`, `build`, `python`, `util`,
  or `ci`), or PRs carrying `0-physics:change` / `0-numerics:change`:
  `MERGE_QUEUE_PLATFORMS` (2: Linux x86_64 and Windows AMD64). Linux runs the
  full **physics-full** tier (split into 3 shards) plus `native` and
  `os_compat`; Windows builds its wheel with **smoke** physics while
  `lane_test_tier` keeps **physics-full** for Windows `native` and `os_compat`. [fact: git grep -n 'MERGE_PLATFORMS="$MERGE_QUEUE_PLATFORMS"' .github/scripts/determine-matrix.sh && git grep -n "lane_test_tier:" .github/workflows/build-publish_to_pypi.yml && git grep -n "event_name == 'merge_group'.*'smoke'" .github/workflows/build-publish_to_pypi.yml]
- Tests-only or other non-package changes across the queued group (without a
  classification label): `MINIMAL_PLATFORMS` (1: Linux x86_64), test tier
  **physics-full** -- wheels are byte-for-byte identical to what `master`
  already validated across OS families, so one Linux build runs the new tests
  while the nightly still covers every platform. [fact: git grep -n 'MERGE_PLATFORMS="$MINIMAL_PLATFORMS"' .github/scripts/determine-matrix.sh]
- Under `GATE_AXIS_SCHEMA=2`, the merge queue is one of the four automatic
  full-physics coverage triggers (`MERGE_TIER=physics-full`); `abi3_smoke` is
  skipped (`abi3_smoke_buildplat=[]`) because ready-PR CI already exercises the
  newest Python bookend, while `tooling`, `unit`, `native`, `os_compat`, and
  `physics` run on the abi3 build floor (`cp312`). [fact: git grep -n "MERGE_TIER=physics-full" .github/scripts/determine-matrix.sh && git grep -n 'ABI3_SMOKE_BUILDPLAT=' .github/scripts/determine-matrix.sh]
- **Classification-label attribution (gh#1576):** when the queued PR carries
  `0-physics:change` or `0-numerics:change`, `build_wheels` is forced on
  `MERGE_QUEUE_PLATFORMS` (2) even if no build-filtered code paths changed.
  The PR number is derived from `gh-readonly-queue/<base>/pr-<n>-<sha>` when
  `<sha>` matches `merge_group.base_sha` (a single-PR queue diff); if the queue
  ref spans multiple PRs or cannot be parsed, the queue fails safe to
  `physics_change=true` and **physics-full**. [fact: git grep -n "ref_base_sha" .github/workflows/build-publish_to_pypi.yml]

### Nightly schedule / tag push

Always: `FULL_PLATFORMS` (4), `ALL_PYTHON` (4, clamped to the `>=3.12` floor --
`cp312`/`cp313`/`cp314`/`cp315`), test tier **all**. `tooling` and `unit` run
on Linux `cp312`; `native` and `os_compat` run on all four `FULL_PLATFORMS`
(`cp312`); `abi3_smoke` runs on non-floor CPythons (`cp313`/`cp314`/`cp315`). [fact: git grep -n "test_tier=all" .github/scripts/determine-matrix.sh && git grep -n 'OS_COMPAT_BUILDPLAT="$FULL_PLATFORMS"' .github/scripts/determine-matrix.sh]

**`macos-15-intel` trim for `abi3_smoke` (runs 28990965739 and 43734399764):**
on the nightly schedule, `full` manual dispatch, and tag releases,
`abi3_smoke_buildplat` uses `NIGHTLY_API_PLATFORMS` (`FULL_PLATFORMS` minus
`macos-15-intel`). This avoids `macos-15-intel` runner scarcity and timeouts
that previously stalled or cancelled runs before publish jobs could execute.
Wheel builds, `physics`, `native`, and `os_compat` still run on all four
`FULL_PLATFORMS` (including macOS Intel), so Intel wheels keep shipping with
full single-Python OS and physics coverage. [fact: git grep -n 'ABI3_SMOKE_BUILDPLAT="$NIGHTLY_API_PLATFORMS"' .github/scripts/determine-matrix.sh && git grep -n 'NATIVE_BUILDPLAT="$FULL_PLATFORMS"' .github/scripts/determine-matrix.sh]

### Manual dispatch (`workflow_dispatch`)

Tier is user-selectable via the `test_tier` input (`all` / `physics-full` /
`standard` / `physics-core` / `core` / `cfg` / `smoke`, defaulting to `all`),
independently of the `matrix_config` input (`full` / `pr` / `minimal` /
`custom`). [fact: git grep -n 'test_tier=${INPUT_TEST_TIER}' .github/scripts/determine-matrix.sh && git grep -n "matrix_config:" .github/workflows/build-publish_to_pypi.yml]

### Platform presets

Defined in `.github/scripts/determine-matrix.sh`: [fact: git grep -n "_PLATFORMS=" .github/scripts/determine-matrix.sh]

- **FULL_PLATFORMS** (4): Linux x86_64, macOS Intel, macOS ARM, Windows
- **PR_PLATFORMS** (3): Linux x86_64, macOS ARM, Windows (omits Intel Mac; used on PRs with `0-ci:full-matrix` or manual `pr` dispatch)
- **MERGE_QUEUE_PLATFORMS** (2): Linux x86_64, Windows (used on merge queue for package/build/CI/physics-labelled changes)
- **MINIMAL_PLATFORMS** (1): Linux x86_64 only (default PR matrix and tests-only merge queue)

### Python version presets

- **PYTHON_CANDIDATES** (candidate window): cp39, cp310, cp311, cp312, cp313, cp314, cp315
- **ALL_PYTHON** / **BOOKEND_PYTHON** are this window clamped to `requires-python`.
  With the current `>=3.12` floor the effective sets are `ALL_PYTHON` = cp312,
  cp313, cp314, cp315 and `BOOKEND_PYTHON` = cp312, cp315 (floor + newest); the abi3
  build matrix is the floor alone (cp312).

`determine-matrix.sh` clamps the candidate window's lower edge to
`pyproject.toml`'s `requires-python` (via `clamp_python_floor.py`), and the
build matrix is the abi3 *floor* (lowest supported CPython). `requires-python` is the single source of truth for the
floor, so raising it automatically drops the now-unsupported versions from
both the build target and the test matrices -- the build matrix can no longer
drift from the declared support floor (gh#1553).

Because `determine_matrix` runs the matrix script from the **base** branch
(trusted), it additionally sparse-checks the *built ref's* `pyproject.toml`
(`github.sha`) as data only and passes it via `PR_PYPROJECT`. This is what
lets a PR that raises `requires-python` build and test on its own corrected
matrix; without it, the floor would always be read from base and a
requires-python bump would fail CI with `cibuildwheel: No build identifiers
selected`.

---

## Merge Queue Coverage Policy

The merge queue deliberately uses a reduced platform/Python matrix rather than the full matrix (exact platform, Python, and tier selection are stated in "Test Tier Assignment by Event -> Merge queue (`merge_group`)" above). This is an intentional trade-off:

**What merge queue omits:**

- macOS ARM64 (available on PRs via `0-ci:full-matrix`, plus nightly and tag releases) and macOS Intel x86_64 (covered nightly, on tag releases, and on `full`/`custom` manual dispatch -- excluded from `PR_PLATFORMS`) [fact: git grep -n "PR_PLATFORMS=" .github/scripts/determine-matrix.sh && git grep -n "FULL_PLATFORMS=" .github/scripts/determine-matrix.sh]
- Non-floor Python versions (`cp313`, `cp314`, and `cp315`; see "Pull requests (draft and ready)" and "Nightly schedule / tag push" above for where `abi3_smoke` covers them) [fact: git grep -n 'ABI3_SMOKE_BUILDPLAT=' .github/scripts/determine-matrix.sh]
- Non-core `slow` tests on non-physics axes (`tooling`, `unit`, `native`, `os_compat`), `qgis` tests, and full physics on Windows (see "Merge queue (`merge_group`)" and "Nightly schedule / tag push" above for the tier expressions and Windows `smoke` split) [fact: git grep -n '"physics-full": f"{axis} and (core or not slow) and not qgis"' scripts/suews/ci_tiers.py && git grep -n "event_name == 'merge_group'.*'smoke'" .github/workflows/build-publish_to_pypi.yml]

**Rationale:** Running every platform and CPython version in the merge queue
duplicates coverage already performed on ready PRs or the nightly while
lengthening queue latency. The queue keeps Linux and Windows wheel builds plus
full Linux physics on the combined tree; nightly and release CI retain full
four-platform and cross-version coverage. [decision: software maintainer, 2026-11-17; updated for GATE_AXIS_SCHEMA=2, 2026-11-26]

**Safety net:**

- **Nightly builds** (2 AM UTC) and **tag releases** run the full four-platform
  matrix at tier `all` (see "Nightly schedule / tag push" above, including the
  `abi3_smoke` `macos-15-intel` trim), catching any platform-specific
  regression that slips through the merge queue within 24 hours and before
  publishing to PyPI. [fact: git grep -n -F "0 2 * * *" .github/workflows/build-publish_to_pypi.yml && git grep -n "test_tier=all" .github/scripts/determine-matrix.sh]
- If a nightly fails, `scheduled-run-tracker.yml` opens a tracking issue naming
  the PRs merged since the last green nightly so it can be addressed before the
  next release. [fact: git grep -n "nightly-tracker.js" .github/workflows/scheduled-run-tracker.yml]

**When to reconsider:** If a platform-specific bug reaches master via the merge queue and causes user impact before the nightly catches it, consider widening the merge-queue platform set (e.g. adding macOS ARM or macOS Intel) or running a fuller platform matrix on merge queue for `fortran`- or `rust`-touching changes only. [decision: software maintainer, 2026-11-17]

---

## The Nightly as the Scientific Tier

The nightly scheduled run (2 AM UTC, `build-publish_to_pypi.yml`) is where the
expensive science lives. The PR and merge-queue tiers stay fast on purpose; the
checks that cost real runner minutes -- a second Fortran build profile, a
full-year cross-platform comparison, per-test CPU accounting -- run once a day on
master instead of on every push. That trade only holds while the nightly is
trusted, which is what the alert job below is for.

### What runs only on the schedule

| Job | Runs on | Produces | Who reads it |
|---|---|---|---|
| `build_wheels` at test tier `all` | every event, but `all` only on schedule, tag and manual dispatch (`test_tier: all`, the dispatch default) | the `cp*` wheels and the `ci-metrics-*` artefacts | anyone diagnosing a lane's cost with `scripts/suews/analyse_ci_run.py` or `benchmark_ci_metrics.py` |
| `build_wheels_checked` | `schedule` only | `checked-cp*` wheels, built `-O0 -fcheck=all`, running the full physics tier | whoever investigates a Fortran runtime error; the release-profile run in the same night is the comparison |
| `python_coverage` | `schedule`, or `workflow_dispatch` with the `python_coverage` input | the `python-coverage` artefact (`coverage.json`, `coverage.xml`, 30-day retention) and a job-summary table of line and branch coverage per package, for `supy` and `suews_mcp` over the whole suite except `qgis` on the Linux cp312 wheel | whoever asks which code no test reaches; `pytest --cov` reproduces it locally from the `[tool.coverage.*]` tables in `pyproject.toml` |
| `tolerance_spread` | `schedule`, or `workflow_dispatch` with the `tolerance_spread` input | one `tolerance-spread-<platform>-<arch>-<cpXY>` JSON per matrix cell, 30-day retention | the tolerance derivation for `test/core/test_sample_output.py`, via `tolerance_spread.py summarise` (see `.claude/rules/tests/patterns.md`) |
| `cost_markers` | `schedule`, or `workflow_dispatch` with the `cost_markers` input | a step-summary table and histogram of every `medium` / `slow` marker that disagrees with the measured CPU seconds, and a drift status and count that `report_scheduled_run` prints in its job summary | whoever adds or changes a cost marker; thresholds in `.claude/rules/tests/patterns.md`. Pull requests get the same check on the test files they change from `cost_markers_pr`, in the CI summary comment |
| `report_scheduled_run` | `schedule` only | the cost-marker drift line in its job summary; it no longer writes issues | whoever reads the run |
| `scheduled-run-tracker.yml` (separate workflow, `workflow_run` completed) | every finished scheduled run of every scheduled workflow | one `2-infra:ci` tracking issue per workflow ("Nightly scheduled run is failing" for this one), assigned to the maintainer named in `NIGHTLY_ASSIGNEE`, listing the failed job leaves and the PRs merged since the last green scheduled run (`.github/scripts/nightly-tracker.js`) | the assignee, notified by GitHub; everyone else through the issue. The first green scheduled run closes it |

`create_nightly_tag` and the TestPyPI publish are schedule-side infrastructure
rather than science, but they gate the same way the test tiers do: a failure
there fails the run, so a broken nightly tag is an alert, not a silent skip.

One further nightly-only behaviour is documented in its own section above:
the build cache, which the nightly restores nothing from but still saves,
under "Build Caches" (the `macos-15-intel` trim for `abi3_smoke` applies to
nightly, full manual dispatch, and tag releases alike, under "Test Tier
Assignment by Event"). [fact: git grep -n 'ABI3_SMOKE_BUILDPLAT="$NIGHTLY_API_PLATFORMS"' .github/scripts/determine-matrix.sh]

### What may redden the nightly

The alert is `scheduled-run-tracker.yml`, which runs after a scheduled run has
finished and judges it by the run's own conclusion: `success` is green;
`failure`, `cancelled`, `timed_out` and `startup_failure` are red. Judging the
whole run rather than a list of needed jobs means a job cancelled late -- a
recording job reaching its timeout, as on 17 and 21 November 2026 -- is a red
night too, where the earlier in-run alert finished before it and stayed silent.
The same tracker covers every scheduled workflow, each with its own issue.
[fact: git grep -n "workflow_run" .github/workflows/scheduled-run-tracker.yml]

The recording jobs -- `tolerance_spread`, `cost_markers` and `python_coverage`
-- carry `continue-on-error: true`, so a failing recording job still leaves the
run `success` and never opens the issue: a wide spread, a drifted marker or
lower coverage is information to act on, not a red night. A recording job that
is cancelled or times out does turn the run red, and the report lists it marked
"recording job", so the cure is to fix its runtime or timeout, not to ignore
the alert. Neither feeds `pr-gate`. A new recording job must be wired the same
way, or the nightly stops being trusted and the fast PR tier stops being
defensible. `scripts/suews/nightly_streak.py` lists recording-job failures
separately from gating ones.

A gating job that is skipped because an upstream job (`determine_matrix`,
`create_nightly_tag`) failed reaches the alert through that upstream failure,
which fails the run.

### Where a new expensive check goes

A new check that costs more than a minute or two of runner time goes to the
nightly first, as a recording job that never gates. Give it a PR-tier
counterpart only when it must gate merges -- that is, when landing the failure
it detects would be worse than the delay it adds to every PR.

The corollary is that the nightly's own reliability is a first-class concern.
The verification criterion for this arrangement is thirty consecutive green
scheduled runs with the alert job silent; read the current streak with

```bash
python scripts/suews/nightly_streak.py
```

which lists the last thirty scheduled runs with their conclusion and the display
names of any failing job, counting the recording jobs separately so their
failures do not read as a broken nightly.

The benchmark candidate evaluation (Stage 3 in `benchmark/README.md`) belongs
in this tier too, once data governance for the observations is settled. It is
deliberately not wired in yet.

---

## Conditional Job Execution

**Dependent Jobs That Should Always Run:**

Use this pattern for jobs that should run even if a dependency was skipped:

```yaml
jobs:
  build:
    if: needs.changes.outputs.core == 'true'
    # ...

  deploy:
    needs: [build]
    if: always() && (needs.build.result == 'success' || needs.build.result == 'skipped')
    # ...
```

**Event-Based Conditionals:**

Use `github.event_name` and ref checks for event-specific behaviour:

```yaml
- name: Upload release artefacts
  if: startsWith(github.ref, 'refs/tags/20')
  uses: actions/upload-artifact@v4
```

---

## Repository Rulesets

**Avoid Overly Broad Tag Protection:**

When configuring repository rulesets, avoid using `~ALL` for tag protection patterns, which can block CI/CD operations like nightly tag creation.

**Better Approach:**

- Protect specific production CalVer tag patterns (e.g., `[0-9]*` release tags without a `v` prefix; SUEWS uses CalVer tags such as `2026.1.28`) [decision: software maintainer, 2026-11-17]
- Allow CI workflows with explicit permissions to create internal dev tags (e.g., `YYYY.M.D.dev` / `*.dev*`) and keep plugin tags (`agent/v*`) separate [decision: software maintainer, 2026-11-17]
- Test ruleset impact on automated workflows before enabling

---

## Cross-Platform Shell Script Compatibility

GitHub Actions workflows run on different operating systems (Linux, macOS, Windows). Shell commands that work on one platform may fail on another.

### BSD vs GNU Tool Differences

macOS uses BSD variants of common Unix tools, while Linux uses GNU variants. These often have incompatible syntax.

**Common Problem Areas:**

- **sed**: BSD sed (macOS) differs significantly from GNU sed (Linux)
  - `-i` flag: BSD requires `-i ''` or `-i.bak`, GNU accepts `-i` alone
  - Address ranges with braces: `/start/,/end/{cmd}` works in GNU, fails in BSD
  - Extended regex: BSD uses `-E`, GNU uses `-r` (though `-E` works in modern GNU)

- **date**: Format specifiers differ between BSD and GNU
- **find**: Some flags like `-printf` are GNU-only
- **grep**: BSD lacks some GNU extensions

### Preferred Solutions

**1. Use Python for text manipulation (RECOMMENDED)**

Python is available on all GitHub Actions runners and provides consistent behaviour:

```yaml
- name: Modify config file
  shell: bash
  run: |
    python3 << 'EOF'
    import re
    with open('config.toml', 'r') as f:
        content = f.read()
    content = re.sub(r'old_pattern', 'new_value', content)
    with open('config.toml', 'w') as f:
        f.write(content)
    EOF
```

**2. Use platform-agnostic tools**

- `awk` is more portable than `sed` for many operations
- `perl -i -pe` works consistently across platforms
- Node.js scripts for complex JSON/YAML manipulation

**3. Install GNU tools on macOS (last resort)**

```yaml
- name: Install GNU tools
  if: runner.os == 'macOS'
  run: brew install gnu-sed coreutils
```

Then use `gsed` instead of `sed`.

### Anti-Patterns to Avoid

- Using GNU sed syntax without testing on macOS
- Assuming shell built-ins behave identically across platforms
- Complex `sed` one-liners in CI workflows
- Using `sed -i` without platform detection

### When Reviewing CI Changes

Before approving workflow changes that include shell commands:

1. Check if commands use `sed`, `date`, `find`, or other platform-specific tools
2. Verify syntax works on both BSD (macOS) and GNU (Linux)
3. Consider whether Python would be more robust
4. Test on all target platforms if possible

---

## Release Notes Best Practices

- List only **completed** work in release notes
- Move work-in-progress to appropriate sections (e.g., "Build System", "Known Issues")
- Avoid listing features for which underlying code/build integration is incomplete

---

## Matrix Job Configuration

**Use `fail-fast: false` for independent matrix jobs** to get complete feedback about which platforms succeed or fail, rather than stopping at the first failure.

### Matrix jobs render as one group

A matrix of test or report jobs is one reusable-workflow call named for the
group, never N top-level jobs. GitHub lists every job of a run as one entry in
the checks list and on the run page, and it nests the jobs of a called
workflow under the calling job's name (`<caller name> / <inner job name>`),
so a bare `strategy.matrix` job in `build-publish_to_pypi.yml` scatters its
cells across the top level while the same matrix inside a reusable workflow
folds into one line. Today's nightly (run 34428109328, 10 Sep 2026) showed
eight top-level `Tolerance spread cpXYZ-<platform> <arch>` entries beside one
`Build standard wheels` group of four; this is the convention that keeps every
future test group on the second shape.

- **In the caller, a job with `strategy.matrix` is a `uses:` call**, never a
  `runs-on:` job. The matrix lives in the reusable workflow
  (`build-wheels-reusable.yml`, `test-source-lane-reusable.yml`,
  `test-wheel-lane-reusable.yml`, `tolerance-spread-reusable.yml` are the
  pattern); [fact: git grep -n "uses: \$/.github/workflows/test-source-lane-reusable.yml" .github/workflows/build-publish_to_pypi.yml && git grep -n "uses: \$/.github/workflows/test-wheel-lane-reusable.yml" .github/workflows/build-wheels-reusable.yml] the caller passes the
  matrix axes as JSON-string inputs and carries a **static `name:`** that is
  the group's name (`Tolerance spread`, `Build checked wheels (nightly
  physics tier)`). The inner job's `name:` is the cell
  (`${{ matrix.python_version }}-${{ matrix.buildplat[1] }} ${{ matrix.buildplat[2] }}`).
- **Fan-out at the caller is for chaining only.** When a caller has to run
  one call per platform so a downstream lane can start as soon as its own
  input exists (the per-platform wheel-then-test-lane chain #1792 introduced,
  now running `native`, `os_compat`, and `abi3_smoke` inside
  `build-wheels-reusable.yml`), [fact: git grep -n "uses: \$/.github/workflows/build-wheels-reusable.yml" .github/workflows/build-publish_to_pypi.yml]
  each call is still a `uses:` job and its name carries the group and the
  platform (`Build and test (<platform> <arch>)`); the cells inside stay
  nested.
- **`continue-on-error` and `timeout-minutes` go on the inner job.** GitHub
  does not accept them on a caller job that `uses:` a reusable workflow (the
  allowed keys there are `name`, `uses`, `with`, `secrets`, `needs`, `if`,
  `permissions`, `strategy`, `concurrency`). A `continue-on-error: true`
  inner job still keeps the calling run green, so a recording job that must
  never redden the nightly loses nothing by moving.
- **Trigger and dispatch conditions stay on the caller.** `inputs.<name>`
  inside a reusable workflow is the `workflow_call` input, not the dispatch
  input, so `github.event_name` and `inputs.tolerance_spread` checks belong
  in the caller's `if:`; a false `if` renders as one skipped group entry.
- **Artefact names do not change when a job moves**, so the scripts that read
  them (`scripts/suews/tolerance_spread.py summarise`,
  `scripts/lint/check_cost_markers.py`, the publish jobs' `cp[0-9][0-9][0-9]-*`
  download pattern) keep working; check the reader before renaming.
- **Bookkeeping that follows a move**: the reusable file goes into the `ci`
  filter in `.github/path-filters.yml` (a PR touching only it must still
  build), `.github/ci-metrics-needs.json` gains a `<Group> / *` pattern with
  the group's dependencies so `analyse_ci_run.py` resolves the children, and
  `test/core/test_ci_run_metrics.py` asserts the shape
  (`test_caller_matrix_jobs_are_reusable_workflow_calls` fails on any new
  bare matrix job in the caller).

A single job that is not a matrix (`Build MCP package`, the cost-marker drift
check) renders as one entry already and needs no reusable workflow.

---

## Project-Specific Workflow Details

For versioning strategy (nightly `.dev` builds, production CalVer tags), UMEP/QGIS compatibility (single `cp312-abi3` wheel strategy), and merge queue behaviour, see the top header comment block in: [fact: git grep -n cp312-abi3 .github/workflows/build-publish_to_pypi.yml]

- `.github/workflows/build-publish_to_pypi.yml`
