# Workflow Details

Full stage definitions for all workflows, organised by category. Each workflow is launched via `/start-work`.

---

# Create Workflows

## Feature Development

**Requires**: GitHub issue number

```
Stage 1: Issue Analysis
  /examine-issue <N>  -->  complexity: Simple or Complex
  (Complex only) gh subissues  -->  sub-issues with dependencies

Stage 2: Workspace Setup  [handled by /start-work]
  /gh-link <N>  -->  branch linked to issue
  make dev  -->  build
  make test-smoke  -->  baseline

Stage 3: Implementation
  Auto-loaded rules guide coding (no skill invocation needed)
  Fortran: src/suews/src/ -- rules/fortran/conventions.md
  Python:  src/supy/      -- rules/python/conventions.md
  Docs:    docs/          -- rules/docs/conventions.md

Stage 4: Quality Gate
  git commit  -->  pre-commit hook auto-runs file-type checks + smoke tests

Stage 5: Documentation
  /sync-docs    -->  detect doc-code mismatches
  /log-changes  -->  CHANGELOG entry ([feature] category)
  make docs     -->  build documentation

Stage 6: PR Creation
  /gh-sync  -->  rebase on master
  gh pr create  -->  open PR (approval before posting)

Stage 7: PR Review
  /audit-pr <N>  -->  full review (style + scientific + testing + docs + build)

Stage 8: Merge & Debrief
  /gh-debrief  -->  verify issue closure, CI status
```

---

## Bug Fix

**Requires**: GitHub issue number

```
Stage 1: /examine-issue <N>    -->  triage + root cause analysis
Stage 2: /start-work setup     -->  branch + build + baseline
Stage 3: Implement fix + write regression test
Stage 4: git commit            -->  hook auto-runs quality checks
Stage 5: /log-changes          -->  [bugfix] entry
Stage 6: gh pr create           -->  PR with bug description and fix rationale
Stage 7: /audit-pr <N>         -->  review
```

**Key difference from Feature**: Always write a regression test. The commit hook will run `make test-smoke`; consider running full `make test` manually if the fix touches physics modules.

---

## Refactoring

**Requires**: GitHub issue number

```
Stage 1: /examine-issue <N>  -->  understand scope
Stage 2: make test (full baseline -- capture pass/fail counts)
Stage 3: Implement refactoring
Stage 4: make test (full -- compare with baseline, all must still pass)
Stage 5: git commit           -->  hook auto-runs lint + verify-build + smoke
Stage 6: /sync-docs           -->  update docs if API changed
Stage 7: /log-changes         -->  [change] or [maintenance] entry
Stage 8: /audit-pr (refactoring mode -- flag unintended behavioural changes)
```

**Key difference**: Run full `make test` both before AND after refactoring. The test baseline is critical -- any test count change or failure indicates unintended behavioural change.

---

## Documentation Only

```
Stage 1: Create branch, make dev, make docs (baseline)
Stage 2: Edit docs (rules/docs/conventions.md auto-loads)
Stage 3: /sync-docs   -->  verify content matches code
Stage 4: /lint-code    -->  RST formatting
Stage 5: make docs     -->  build, verify no warnings
Stage 6: /log-changes  -->  [doc] entry
Stage 7: PR
```

**Note**: The commit hook skips `make test-smoke` for docs-only commits (no Python/Fortran staged).

---

# Review Workflows

## Examine Issue

1. Ask for issue number (GitHub `#N`)
2. Run `/examine-issue <N>` -- full analysis with complexity assessment
3. After analysis, suggest `/start-work` -> Create to begin implementation

## Audit PR

1. Ask for PR number
2. Run `/audit-pr <N>` -- comprehensive review across 7 dimensions (code style, scientific, testing, docs, build, merge criteria, reviewer mapping)
3. Draft comments for approval before posting

## Check GitHub

1. Run `/gh-check` -- scan for items needing attention
2. Show assigned issues, review requests, mentions, stale PRs
3. Suggest next action for each item

---

# Maintenance Workflows

## Release

**Requires**: Decision to release (assessed via scoring)

```
Stage 1: /prep-release  -->  assess necessity (scoring)
         /gh-check      -->  open issues needing attention
Stage 2: /verify-build + /sync-docs + /lint-code  -->  pre-flight checks
Stage 3: /log-changes   -->  finalise CHANGELOG
         Update version-history RST + GitHub Release notes
Stage 4: Create PR, wait CI, merge, tag via /gh-release
Stage 5: Monitor PyPI, GitHub Release, Zenodo
```

Full release process is documented in `/prep-release` skill and `dev-ref/RELEASE_MANUAL.md`.

---

## CI/Build

```
Stage 1: /verify-build  -->  identify current issues
Stage 2: Create branch, implement fixes
Stage 3: /verify-build  -->  confirm fixes resolve issues
Stage 4: /lint-code      -->  if Python/Fortran also touched
Stage 5: make test-smoke -->  verify builds still work
Stage 6: /log-changes    -->  [maintenance] entry
Stage 7: PR
```

**Typical scenarios**: dependency version updates, CI workflow fixes, meson.build changes, Python version matrix updates, f90wrap pin changes.

---

## Checks (Quick Health Check)

Run all verification skills together:

1. `/verify-build` -- build config consistency
2. `/sync-docs` -- doc-code content consistency
3. `/lint-code` -- code style compliance
4. Report combined summary of all findings

---

# Decision Gates

## Complexity Gate (Create workflows, Stage 1)

After `/examine-issue`, the assessment determines the approach:

- **Simple** (score 5+): Clear problem, obvious solution, 1-3 files --> single branch, direct implementation
- **Complex** (score 3+ on complex indicators): Ambiguous, multiple approaches, architectural --> sub-issue decomposition via `gh subissues`, possibly multiple branches

## Change Type Gate (Feature, Stage 3)

Determines which auto-loaded rules apply and which test tier to use:

- **Fortran** (`src/suews/src/`): fprettify, IMPLICIT NONE, KIND(1D0), unit annotations, add to meson.build
- **Python** (`src/supy/`): ruff, type hints, NumPy docstrings, pathlib, logger_supy
- **Documentation** (`docs/`): RST conventions, max 3 heading levels, :math: for equations
- **Tests** (`test/`): pytest markers, AAA pattern, scientific tolerances

## Test Tier Gate (Stage 4/5)

- **Smoke** (`make test-smoke`): Default for Feature, Bug Fix, CI/Build -- fast validation (~30-60s)
- **Standard** (`make test`): When physics modules or test files change (~2-3 min)
- **Full** (`make test-all`): Refactoring baseline comparison, release pre-flight (~4-5 min)

---

# Post-Merge Verification Checklist

After PR is merged, verify via `/gh-debrief`:

- [ ] Issue closed (if linked with "Fixes #N")
- [ ] CI green on master
- [ ] Documentation deployed (if docs changed)
- [ ] No regression in dev tag CI
