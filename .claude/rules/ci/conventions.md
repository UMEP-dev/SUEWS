---
paths:
  - .github/workflows/**/*.yml
  - .github/workflows/**/*.yaml
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
  - '!src/supy/_supy_driver_wrapper.py'   # BUG: matches ALL non-wrapper files
```

The `!` pattern alone returns `true` for `.github/path-filters.yml`, `scripts/test.sh`, etc. -- any file that isn't `_supy_driver_wrapper.py`.

**Why `predicate-quantifier: 'every'` doesn't help:** This v3.0.2 option requires ALL patterns to match a file. It fixes negation for single-directory categories, but breaks multi-directory categories (a file in `src/supy/` would fail because it doesn't match `src/supy/dts/**`).

See [dorny/paths-filter#184](https://github.com/dorny/paths-filter/issues/184) and [#97](https://github.com/dorny/paths-filter/issues/97).

### Correct approach: positive-only patterns

Define each category with explicit positive patterns. Accept minor overlap between categories rather than attempting exclusions:

```yaml
# In .github/path-filters.yml
fortran:
  - 'src/suews/src/**'
  - 'src/supy_driver/**'
  - 'src/supy/_supy_driver_wrapper.py'  # also matches python -- harmless

python:
  - 'src/supy/*.py'    # includes _supy_driver_wrapper.py -- acceptable overlap
  - 'src/supy/dts/**'
  - 'src/supy/data_model/**'
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

- **fortran** -- compiled extension source (triggers multiplatform build)
- **python** -- pure Python source (single-platform build)
- **util** -- utility modules (single-platform build)
- **build** -- build system files: meson, env.yml (multiplatform build)
- **pyproject** -- pyproject.toml (classified by content in workflow)
- **ci** -- CI workflows and tooling (single-platform build)
- **tests** -- test files (single-platform build)
- **docs** -- documentation source
- **site** -- static site content

### Overlapping filter categories

A single file can match multiple categories. The build decision logic ORs categories and picks the strictest platform requirement (fortran triggers multiplatform).

**Key overlap: `_supy_driver_wrapper.py`**

This file appears in both `fortran` and `python` categories. This is intentional:

- **Why fortran?** The file is the Python-side loader for the compiled f90wrap Fortran extension (`_supy_driver`). It handles platform-specific dynamic library loading (`dlopen`/`importlib`), error recovery for missing binaries, and import path resolution. Changes can break on specific OS/arch combinations (e.g., macOS dylib paths vs Linux .so).
- **Why python?** It is a `.py` file in `src/supy/` and participates in the Python package's import machinery. A syntax error or API change affects all platforms.
- **Net effect:** Any edit triggers `fortran=true`, which triggers multiplatform builds. This is the desired behaviour -- even a seemingly Python-only edit to this bridge file should be validated across platforms.

Do not attempt to exclude this file from either category using negation patterns (see the negation pitfall above).

### Testing path filters locally

```bash
./scripts/test-detect-changes.sh              # compare HEAD vs origin/master
./scripts/test-detect-changes.sh abc123       # compare HEAD vs specific commit
```

Requires Docker and `act` (`brew install act`).

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

The `needs-build` condition includes `fortran`, `python`, `util`, `build`, `pyproject`, `ci`, and `tests` but NOT `docs` or `site`. Changes that only affect documentation paths do not trigger wheel builds. Docs are validated by the separate `pages-deploy.yml` workflow.

### Release notes are not documentation

Files in `.github/releases/` are release metadata consumed by the GitHub Release creation step. They must NOT be included in the `docs` filter category. If release notes changes need to trigger builds, place them in `cfg` or a dedicated filter.

### Concurrency and cancel-in-progress

The workflow uses `cancel-in-progress: true` scoped to `workflow-PR_number_or_ref`. When pushing multiple commits in quick succession (e.g., during a rebase), earlier runs are cancelled before later runs start. This can cause a race condition where no run completes if pushes arrive faster than jobs start.

Mitigate by:
- Waiting for CI to begin before pushing again
- Squashing commits before pushing
- Using `git push --force-with-lease` once rather than multiple pushes

---

## Test Tier Definitions

Test tiers control which pytest markers run during CI builds. Defined via pytest markers in `pyproject.toml`; consumed by `.github/actions/build-suews/action.yml` (`CIBW_TEST_COMMAND`).

- **smoke** (`-m smoke`) -- minimal wheel validation (~6 tests, ~60s)
- **core** (`-m "smoke or core"`) -- core physics and logic, includes smoke (~2-3 min)
- **cfg** (`-m "smoke or cfg"`) -- config/schema validation, includes smoke (~2-3 min)
- **standard** (`-m "not slow"`) -- all tests except slow-marked ones (~5-10 min)
- **all** (no filter) -- full suite including slow tests (~15-30 min)

Each higher tier is a superset of smoke. The `standard` tier excludes only `slow`-marked tests (>30s each).

---

## Test Tier Assignment by Event

The `determine_matrix` job (`build-publish_to_pypi.yml`) assigns tiers based on event type and change categories:

### Draft pull requests (fastest feedback)

- fortran changes: PR_PLATFORMS (3), test tier **core**
- build-system changes: PR_PLATFORMS (3), test tier **cfg**
- python / util / ci / tests: MINIMAL (1), test tier **smoke**
- Python versions: BOOKEND (2)

### Ready (non-draft) pull requests

- fortran or build-system: PR_PLATFORMS (3), test tier **standard**
- python or util: MINIMAL (1), test tier **standard**
- ci or tests only: MINIMAL (1), test tier **smoke**
- Python versions: BOOKEND (2)

### Merge queue (`merge_group`)

Always: PR_PLATFORMS (3), BOOKEND (2), test tier **standard**. No path-based differentiation -- the merge queue uses a fixed reduced matrix to validate the merge commit.

### Nightly schedule / tag push

Always: FULL_PLATFORMS (4), ALL_PYTHON (6), test tier **all**.

### Manual dispatch (`workflow_dispatch`)

Tier is user-selectable via the `test_tier` input. Matrix depends on `matrix_config` input (full / pr / minimal / custom).

### Platform presets

- **FULL_PLATFORMS** (4): Linux x86_64, macOS Intel, macOS ARM, Windows
- **PR_PLATFORMS** (3): Linux x86_64, macOS ARM, Windows (omits Intel Mac)
- **MINIMAL_PLATFORMS** (1): Linux x86_64 only (fastest feedback)

### Python version presets

- **ALL_PYTHON** (6): cp39, cp310, cp311, cp312, cp313, cp314
- **BOOKEND_PYTHON** (2): cp39, cp314 (oldest + newest)

---

## Merge Queue Coverage Policy

The merge queue deliberately uses a reduced matrix rather than the full matrix. This is an intentional trade-off:

**What merge queue covers:**

- 3 platforms: Linux x86_64, macOS ARM64, Windows AMD64
- 2 Python versions: 3.9 (oldest supported), 3.14 (newest)
- standard test tier: all tests except `slow`-marked

**What merge queue omits:**

- macOS Intel x86_64 (legacy platform, declining user base)
- Python 3.10-3.13 (intermediate versions)
- `slow`-marked tests (>30s each)

**Rationale:** Full matrix (4 platforms x 6 Python = 24 jobs) would make the merge queue too slow for its purpose as a fast gatekeeper before landing on master. The reduced matrix covers all three major OS families and version boundary extremes where compatibility issues most commonly surface.

**Safety net:**

- **Nightly builds** (2 AM UTC) run the full matrix with all tests. Any platform-specific regression that slips through the merge queue is caught within 24 hours.
- **Tag releases** also run the full matrix before publishing to PyPI.
- If a nightly fails, the issue is visible in the Actions tab and can be addressed before the next release.

**When to reconsider:** If a platform-specific bug reaches master via the merge queue and causes user impact before the nightly catches it, consider either adding macOS Intel to PR_PLATFORMS (increases merge queue from 6 to 8 jobs) or running the full matrix on merge queue for fortran-touching changes only.

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
- name: Build with DTS
  if: github.event_name == 'schedule' || startsWith(github.ref, 'refs/tags/')
  run: make build-dts

- name: Upload release artifacts
  if: startsWith(github.ref, 'refs/tags/v')
  uses: actions/upload-artifact@v4
```

---

## Repository Rulesets

**Avoid Overly Broad Tag Protection:**

When configuring repository rulesets, avoid using `~ALL` for tag protection patterns, which can block CI/CD operations like nightly tag creation.

**Better Approach:**

- Protect specific production tag patterns (e.g., `v*` for releases)
- Allow CI to create internal tags (e.g., `nightly-*`, `dev-*`)
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

---

## Project-Specific Workflow Details

For versioning strategy (nightly `.devN` builds, production tags), UMEP/QGIS compatibility (dual wheel builds), and merge queue behaviour, see the header comments in:

- `.github/workflows/build-publish_to_pypi.yml` (lines 1-147)
