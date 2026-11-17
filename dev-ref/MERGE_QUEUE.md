# Merge Queue Guide

> **Canonical Rules Notice**
>
> Active workflow contracts, CI matrix rules, and coordination skills supersede this guide whenever details drift. Always read:
> - Rules: [`.claude/rules/autonomous-workflow.md`](../.claude/rules/autonomous-workflow.md), [`.claude/rules/ci/conventions.md`](../.claude/rules/ci/conventions.md), [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md), and [`.claude/rules/work-sizing.md`](../.claude/rules/work-sizing.md)
> - Skills: `/queue-pr` ([`.claude/skills/queue-pr/SKILL.md`](../.claude/skills/queue-pr/SKILL.md)), `/audit-pr` ([`.claude/skills/audit-pr/SKILL.md`](../.claude/skills/audit-pr/SKILL.md)), and `/triage-pr` ([`.claude/skills/triage-pr/SKILL.md`](../.claude/skills/triage-pr/SKILL.md))

## Overview

Branch protection on `UMEP-dev/SUEWS` prohibits direct pushes to `master`. **Every change to `master` -- including documentation updates, release preparation PRs, and hotfixes -- lands through a pull request and GitHub's merge queue.**

GitHub's merge queue tests each PR (or batch of PRs) against the latest `master` plus any PRs ahead of it in the queue, preventing semantic merge conflicts where two PRs pass CI individually but break when combined:
1. GitHub constructs a temporary `merge_group` commit (`master` + queued PRs).
2. CI runs against that combined state (`merge_group: types: [checks_requested]`).
3. If `pr-gate` and all required checks pass, GitHub merges the PR automatically.
4. If checks fail, GitHub removes the failing PR from the queue for diagnosis.

**Human-Gated Terminal Rule (`autonomous-workflow.md`)**: Adding a PR to the merge queue (`gh pr merge --auto` / `--merge-queue` / `queue-ready-prs.sh --enqueue`) is **always human-gated**. Autonomous agents may scan, order, and preflight PRs, but never enqueue without explicit per-run human approval, and **never** pass `--admin` to bypass branch protection or the queue.

## CI Matrix Selection (`.github/scripts/determine-matrix.sh`)

`.github/scripts/determine-matrix.sh` computes the wheel-build platforms (`buildplat`), cross-Python API test platforms (`api_buildplat`), CPython versions (`python` / `api_python`), and `test_tier` for each event:

### Platform Presets in `determine-matrix.sh`
- **`MINIMAL_PLATFORMS`**: `ubuntu-latest` (`manylinux` `x86_64`) only.
- **`PR_PLATFORMS`**: `ubuntu-latest` (`manylinux` `x86_64`), `macos-15` (`arm64`), and `windows-2025` (`AMD64`).
- **`FULL_PLATFORMS`**: `PR_PLATFORMS` plus `macos-15-intel` (`x86_64`). (`NIGHTLY_API_PLATFORMS` drops `macos-15-intel` from nightly `api` cross-Python tests due to Intel runner scarcity while still building and gating the Intel wheel.)

### Matrix by Event and Change Scope

| Event & Change Scope | Wheel / API Platforms | `api_python` Versions | `test_tier` |
|----------------------|-----------------------|-----------------------|-------------|
| **Draft PR** (Fortran or Rust changed) | `PR_PLATFORMS` | Bookends (`cp312`, newest) | `core` |
| **Draft PR** (build-system changed) | `PR_PLATFORMS` | Bookends (`cp312`, newest) | `cfg` |
| **Draft PR** (Python / util / CI / tests only) | `MINIMAL_PLATFORMS` | Bookends (`cp312`, newest) | `smoke` |
| **Ready PR** (Fortran, Rust, or build changed) | `PR_PLATFORMS` | Bookends (`cp312`, newest) | `standard` (`physics-full` if `0-physics:change`) |
| **Ready PR** (Python or util only) | `MINIMAL_PLATFORMS` | Bookends (`cp312`, newest) | `standard` (`physics-full` if `0-physics:change`) |
| **Ready PR** (CI or tests only) | `MINIMAL_PLATFORMS` | Bookends (`cp312`, newest) | `smoke` (`physics-full` if `0-physics:change`) |
| **Merge Queue** (Fortran, Rust, Python, util, build, or CI changed) | `PR_PLATFORMS` | `cp312` (`requires-python` / `abi3` floor) | `standard` (`physics-full` if `0-physics:change`) |
| **Merge Queue** (tests or non-package files only) | `MINIMAL_PLATFORMS` | `cp312` (`requires-python` / `abi3` floor) | `standard` |
| **Nightly (`schedule`)** | `FULL_PLATFORMS` (wheels) / `NIGHTLY_API_PLATFORMS` (`api`) | All supported (`cp312..cp315`) | `all` (+ checked Fortran profile, tolerance spread, cost markers) |
| **Release / `.dev` Tag Push** | `FULL_PLATFORMS` | All supported (`cp312..cp315`) | `all` |

- **Documentation-only PRs**: `detect-changes` skips wheel builds when no code/test/build paths changed; `pr-gate` still runs and passes so docs PRs traverse the merge queue quickly.
- **`0-physics:change` PRs**: Both Ready PR and Merge Queue runs automatically upgrade `test_tier` to `physics-full` (gh#1576) so all `-m physics` tests (including `slow`) run before merge.
- **UMEP/QGIS (`qgis` marker)**: Excluded from PR and Merge Queue `standard` expressions (`and not qgis`); runs under `all` on Nightly, Tag pushes, and `full` manual dispatches (Windows + Python 3.12).

## Using the Merge Queue

### Coordinating Multiple PRs with `/queue-pr`

When several PRs or worktrees are ready at once, use `.claude/skills/queue-pr/SKILL.md` to detect shared high-risk files (`CHANGELOG.md`, `meson.build`, `src/supy/meson.build`, `.github/workflows/*`, generated docs/schema files, reference fixtures) and run a temporary-worktree merge preflight before queueing:

```bash
# Dry-run scan and low-risk ordering across open ready-for-review PRs
git fetch origin
.claude/skills/queue-pr/scripts/queue-ready-prs.sh --base master

# Local branch overlap & merge preflight
.claude/skills/queue-pr/scripts/preflight.sh --base origin/master branch-a branch-b

# Enqueue in coordinated order (ONLY with explicit human approval)
.claude/skills/queue-pr/scripts/queue-ready-prs.sh --base master --enqueue
```

### Enqueuing a Single Approved PR

```bash
# Enqueue with head-SHA guard (recommended -- enters merge queue when checks/reviews pass)
gh pr merge <PR_NUMBER> --auto --squash --match-head-commit <HEAD_SHA>

# Or via GitHub Web UI: click "Merge when ready" / "Add to merge queue"
```

### Checking and Managing Queue Status

```bash
# View PR merge state and check rollup
gh pr view <PR_NUMBER> --json mergeStateStatus,statusCheckRollup

# Watch CI checks on a PR
gh pr checks <PR_NUMBER> --watch

# List open PRs currently in the merge queue
gh pr list --state open --json number,title,mergeStateStatus | \
  jq '.[] | select(.mergeStateStatus | contains("QUEUE"))'

# Remove a failing or blocked PR from the merge queue
gh pr edit <PR_NUMBER> --remove-from-merge-queue
```

## Best Practices

### Before Queueing
1. **Confirm PR CI (`pr-gate`) and reviews are complete** -- including `/audit-pr` convergence and any required `0-physics:change` domain-owner sign-off (`@yiqing1021` for STEBBS; `@sunt05` default).
2. **Update stale branches cleanly** (only on branches you own, or with explicit per-branch approval):
   ```bash
   git fetch origin master
   git rebase origin/master
   make test-smoke
   git push --force-with-lease
   ```
3. **Order overlapping PRs**:
   - **Independent PRs** (disjoint subsystems, no shared high-risk files): safe to batch together.
   - **Shared-file PRs** (`CHANGELOG.md`, `meson.build`, schema/sample config, reference shards): queue sequentially, rebasing the second PR after the first lands.
   - **Stacked PRs** (`PR2` depends on `PR1`): target `PR2` at `PR1`'s branch until `PR1` merges through the queue, then rebase `PR2` onto `origin/master` and enqueue `PR2`. Never enqueue dependent stacked PRs simultaneously.

### Queue Etiquette
- **Do not push while queued**: Pushing to a queued PR branch ejects it from the queue and cancels in-flight `merge_group` runs.
- **Unblock promptly**: If a queued PR fails due to a semantic conflict with an earlier merged PR, rebase onto `origin/master`, run `make test-smoke`, wait for PR CI, and re-enqueue.
- **Never bypass branch protection**: Do not push commits directly to `master` and do not use `--admin`. (Tag pushes for formal `YYYY.M.D` releases point at commits already merged to `master` via the release PR.)

## See Also

- [`FEATURE_DEVELOPMENT_WORKFLOW.md`](FEATURE_DEVELOPMENT_WORKFLOW.md) - Stacked PRs and `git worktree` workflows
- [`REVIEW_PROCESS.md`](REVIEW_PROCESS.md) - Code governance and `0-physics:change` sign-off
- [`RELEASE_MANUAL.md`](RELEASE_MANUAL.md) - Release PR and tag workflow
- [`.github/scripts/determine-matrix.sh`](../.github/scripts/determine-matrix.sh) - Authoritative CI matrix logic
