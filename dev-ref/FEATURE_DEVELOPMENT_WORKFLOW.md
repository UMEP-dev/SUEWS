# Feature Development Workflow

> **Canonical Rules Notice**
>
> Active sizing, workflow, and code-design rules supersede this guide whenever an example drifts. Always read:
> - Rules: [`.claude/rules/work-sizing.md`](../.claude/rules/work-sizing.md), [`.claude/rules/autonomous-workflow.md`](../.claude/rules/autonomous-workflow.md), [`.claude/rules/code-design.md`](../.claude/rules/code-design.md), [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md), and [`.claude/rules/labels.md`](../.claude/rules/labels.md)
> - Skills: `/triage-issue` ([`.claude/skills/triage-issue/SKILL.md`](../.claude/skills/triage-issue/SKILL.md)), `/fix-issue` ([`.claude/skills/fix-issue/SKILL.md`](../.claude/skills/fix-issue/SKILL.md)), `/audit-pr` ([`.claude/skills/audit-pr/SKILL.md`](../.claude/skills/audit-pr/SKILL.md)), `/split-pr` ([`.claude/skills/split-pr/SKILL.md`](../.claude/skills/split-pr/SKILL.md)), and `/queue-pr` ([`.claude/skills/queue-pr/SKILL.md`](../.claude/skills/queue-pr/SKILL.md))

## Overview

This guide describes how to decompose substantial SUEWS features into focused, independently testable, and revertible pull requests while keeping `master` green through GitHub's merge queue.

**Core Branch Rule**: Never push directly to `master`. Every slice -- preparatory refactor, schema bump, kernel change, or documentation update -- lands via a pull request reviewed and merged through the GitHub merge queue (see [`MERGE_QUEUE.md`](MERGE_QUEUE.md)).

## When to Split Work (`work-sizing.md`)

### Decompose into Incremental PRs When:
- `[OK]` The change exceeds a focused single-sitting review (roughly a few hundred lines of meaningful logic).
- `[OK]` A preparatory refactor, formatting sweep, or schema version bump can land cleanly ahead of behaviour changes.
- `[OK]` Multiple subsystems (`src/suews/`, `src/suews_bridge/`, `src/supy/data_model/`, `docs/`) can be staged safely or developed in parallel.
- `[OK]` An issue has multiple separable deliverables (decompose into a parent umbrella issue + GitHub native sub-issues per `.claude/rules/work-sizing.md`).

### Keep as a Single PR When:
- One coherent bug fix, validator contract, or small feature (< ~200-300 lines) where code, tests, and docs belong together atomically.
- Splitting would leave intermediate commits broken or untestable.

If an existing branch has already grown too large, use `/split-pr` to carve it into a completeness-checked stack of small PRs.

## Decomposition Strategies

### 1. Preparatory Refactor First
When fixing a bug or adding an option inside an oversized procedure or module, land a pure behaviour-preserving refactor PR first (proven bit-identical by tests), then land the behaviour change on top:

```text
PR1: Extract helper / consolidate module (bit-identical output, no schema bump)
  |
  v
PR2: Implement new physics or validation behaviour + tests + docs
```

### 2. Bottom-Up Layer Staging
Build and verify foundations before exposing user-facing surface area:

```text
PR1: Fortran kernel routine & derived-type components (src/suews/src/)
  |  + Makefile.deps regenerated via scripts/suews/gen_fortran_deps.py
  v
PR2: C-ABI facade & Rust bridge wiring (src/suews_bridge/, src/supy/_run_rust.py)
  |  (see dev-ref/RUST_BRIDGE_ADD_PARAMETER.md and .claude/rules/rust/bridge-contract.md)
  v
PR3: Pydantic data model, schema version bump & YAML upgrader (src/supy/data_model/)
  |  (see .claude/rules/python/schema-versioning.md)
  v
PR4: User tutorials, docs, and CHANGELOG entry
```

*(Note: When a new parameter requires all eight bridge steps end-to-end to avoid dead/unwired fields, keep a single scalar parameter's kernel + bridge + Pydantic wiring together as one vertical slice, and split multiple parameter groups across separate PRs.)*

### 3. Horizontal Slices
Implement one complete vertical slice for a single scheme or option, then extend to remaining options in follow-up PRs.

## Managing Stacked and Parallel PRs with `git worktree`

Using `git worktree` avoids rebuilding compiled Fortran and Rust artefacts on every branch switch and prevents cross-branch state accidents.

### Stacked PRs (Sequential Dependencies)

```bash
# Worktree 1: Foundation PR targeting master
git fetch origin
git worktree add ../suews-step1 -b feat/step1-foundation origin/master
cd ../suews-step1
# ... implement, run make dev && make test-smoke ...
git push -u origin feat/step1-foundation
gh pr create --base master --title "Refactor resistance helper"

# Worktree 2: Dependent PR targeting feat/step1-foundation (NOT master yet)
git worktree add ../suews-step2 -b feat/step2-scheme feat/step1-foundation
cd ../suews-step2
# ... implement step 2 ...
git push -u origin feat/step2-scheme
gh pr create --base feat/step1-foundation --title "Add new resistance option"
```

#### Landing a Stack Safely
1. Complete review on `PR1` (reviewers of `PR2` see only `PR2`'s incremental diff while its base is `feat/step1-foundation`).
2. Enqueue **only `PR1`** into the GitHub merge queue (`gh pr merge <PR1> --auto --squash` or `/queue-pr`). Never enqueue `PR2` before `PR1` merges.
3. Once `PR1` merges into `master`, GitHub retargets `PR2` to `master` automatically. Rebase `PR2` onto fresh `origin/master`, verify `make test-smoke`, push with `--force-with-lease`, wait for PR CI, and then enqueue `PR2`:
   ```bash
   cd ../suews-step2
   git fetch origin master
   git rebase origin/master
   make test-smoke
   git push --force-with-lease origin feat/step2-scheme
   ```

### Parallel Independent PRs

When slices touch disjoint files (for example independent physics modules or isolated validator fixes), create separate worktrees directly from `origin/master` and coordinate merge-queue ordering with `/queue-pr`:

```bash
git worktree add ../suews-fix-a -b fix/issue-a origin/master
git worktree add ../suews-fix-b -b fix/issue-b origin/master

# Clean up worktrees after PRs merge
git worktree remove ../suews-fix-a
git worktree remove ../suews-fix-b
```

## CI Matrix Awareness (`determine-matrix.sh`)

Understanding how `.github/scripts/determine-matrix.sh` scales CI helps keep iteration fast:
- **Python / util / tests / CI-only PRs** run on **`MINIMAL_PLATFORMS`** (`ubuntu-latest` `manylinux` `x86_64` only) during Draft (`smoke`) and Ready PR (`standard` or `smoke`) validation, saving macOS and Windows runner minutes.
- **Fortran, Rust bridge, or build-system PRs** automatically widen to **`PR_PLATFORMS`** (Linux + macOS `arm64` + Windows `AMD64`) on Draft (`core` / `cfg`), Ready PR (`standard`), and Merge Queue runs.
- **`0-physics:change` PRs** widen the physics axis to `physics-full` on Ready PRs and in the Merge Queue, and require a `## Scientific evidence` section, refreshed reference fixtures (`provenance.json` + shards), and subsystem-owner sign-off (see `.claude/rules/physics-change-evidence.md`).
- **Nightly and Tag builds** run **`FULL_PLATFORMS`** (adding `macos-15-intel`) across the `all` test tier.

Isolating pure Python/schema/docs preparatory PRs from Fortran/Rust PRs therefore reduces CI turnaround time as well as review complexity.

## Umbrella Tracking for Multi-PR Features

Before starting a multi-PR feature, decompose the parent umbrella issue (human-gated via `/triage-issue`) using GitHub native sub-issues and wave checklists (`.claude/rules/work-sizing.md`):

```markdown
## Summary
Implement [feature] across independent, reviewable waves.

## Waves
### Wave 1 (no dependencies)
- [ ] #101 Preparatory refactor of module X
- [ ] #102 Schema field definitions and migration handler

### Wave 2 (depends on Wave 1)
- [ ] #103 Kernel + Rust bridge integration (pause for 4-needs-science review)
- [ ] #104 User documentation, tutorial, and CHANGELOG
```

Label each child with the repository's four-tier labels (`1-*`, `2-*`, `3-*`, `4-*` per `.claude/rules/labels.md`).

## Pre-PR Checklist for Each Slice

- [ ] Scoped to one logical concern (`git diff origin/master --stat` is focused and free of unrelated edits)
- [ ] New files added to `meson.build`; `python scripts/suews/gen_fortran_deps.py` run if Fortran `USE` edges changed
- [ ] Every new `test/**/*.py` file carries `pytestmark = pytest.mark.api` or `pytest.mark.physics`
- [ ] `make format` and `make test-smoke` (plus `make test` where applicable) pass locally
- [ ] `0-physics:change` + `## Scientific evidence` + fixture refresh included if any model output moved
- [ ] Schema version bumped and upgrader registered if `src/supy/data_model/` changed structurally

## See Also

- [`.claude/rules/work-sizing.md`](../.claude/rules/work-sizing.md) - Issue and PR right-sizing and umbrella structure
- [`.claude/rules/code-design.md`](../.claude/rules/code-design.md) - Layer ownership and design principles
- [`RUST_BRIDGE_ADD_PARAMETER.md`](RUST_BRIDGE_ADD_PARAMETER.md) - End-to-end parameter checklist across Fortran, Rust, and Python
- [`MERGE_QUEUE.md`](MERGE_QUEUE.md) - Merge queue behaviour and coordination
- [`REVIEW_PROCESS.md`](REVIEW_PROCESS.md) - Governance and scientific review routing
