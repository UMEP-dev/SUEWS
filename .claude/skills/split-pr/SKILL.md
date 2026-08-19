---
name: split-pr
description: Use whenever an oversized or bundled SUEWS pull request (or a fat local branch) needs to be carved into a stacked series of smaller, single-concern PRs that each build and pass tests on their own. Trigger for requests like "split this PR", "this PR is too big, break it up", "carve this branch into stacked PRs", "separate the refactor from the fix", "split off the formatting/generated changes", or when audit-pr's Size Gate recommends a split. This skill assesses the diff, proposes a split plan, creates per-child worktrees/branches, carves by concern or by commit while preserving authorship, verifies each child independently green, checks completeness (every change lands in exactly one child, nothing dropped), opens the stacked PRs for approval, and hands off to queue-pr. It does not merge and does not rewrite the original branch without explicit approval.
---

# Split PR

Carve one oversized or bundled SUEWS pull request (or a fat local branch) into a
stacked series of smaller, single-concern PRs. Each child must build and pass
tests on its own, and the union of the children must equal the original diff with
nothing dropped or duplicated.

The terminal state is **a verified stack of PR-ready branches**, not merged. This
skill does not review (`audit-pr`) and does not merge (`queue-pr` / merge queue).
It preserves the original branch and PR untouched until the stack is verified.

## Triggers

- Use for: "split this PR", "this PR is too big", "carve this branch into stacked
  PRs", "separate the refactor from the fix", "split off the formatting/generated
  changes", or when `audit-pr`'s Size Gate recommends a split.
- Size criteria live in `.claude/rules/work-sizing.md`. If the user only wants a
  judgement on whether to split, use `audit-pr` (Size Gate) instead.

## Safety Rules

- Never rewrite or force-push the original branch/PR without explicit approval for
  that exact branch. Carve into new branches; leave the original intact until the
  stack is verified and the user approves retiring it.
- Pushing the new branches and opening the child PRs requires approval. Local
  carving, worktree creation, and verification are allowed when clearly scoped.
- Every change in the original must land in exactly one child (see Completeness
  check). Never silently drop or duplicate a hunk.
- Each child must leave the tree green: it builds and passes `make test-smoke`
  independently (a stacked child is verified on top of its parent).
- Do not auto-resolve semantic, scientific, or schema conflicts that arise while
  carving. Stop and report.
- Preserve authorship via the by-commit/cherry-pick route, which keeps the
  original author. The by-path route creates fresh commits authored by you; those
  carry the AI-collaboration trailer for the assistant that made them (Codex or
  Claude), never the wrong one. If single-author attribution must be retained for
  a by-path child, prefer cherry-pick or pass `--author=`.
- Never reuse or delete a pre-existing `split/<pr>-*` branch or worktree without
  first reporting it and getting explicit approval; never delete the original
  branch/PR.
- Privacy on every outward write: never let internal tracker IDs (e.g. Linear
  PER-XXX), absolute local/worktree paths (`/Users/...`, `../split-<pr>/...`),
  internal branch names, or private host names reach a child PR body, commit
  message, the stack/concern descriptions, or the retire recommendation. These
  surfaces are public; scrub model-authored text (operationalises the privacy
  invariant in `.claude/rules/autonomous-workflow.md`).
- Stop if the diff cannot be cleanly separated into independent concerns, or if
  splitting would require a broad redesign.

## Scripts

No bundled scripts. Use `git worktree`, `gh`, and the `queue-pr` scripts for the
downstream coordination of the resulting stack.

## Workflow

### 1. Snapshot and assess

1. Identify the source: a PR number or a local branch. Fetch and inspect:
   ```bash
   git fetch origin
   gh pr view <pr> --json number,title,headRefName,headRefOid,baseRefName,author,files,url
   gh pr diff <pr> --name-only
   git diff --name-status origin/<base> <source-head>   # D = deletion, R = rename: special carve in step 3
   ```
2. Group the diff into separable concerns by file/subsystem and by commit
   (`git log --oneline origin/<base>..<head>`). Watch for a mix of refactor +
   behaviour change + formatting, bundled unrelated fixes, or generated/mechanical
   changes that should be carved out first.
3. Classify each child's base using `queue-pr`'s shape model, reduced to the two
   shapes that apply when carving a fresh stack: a child is independent (off
   `<base>`, no logical dependency) or stacked (off its parent child). Shared
   build/schema/docs edits do not get their own shape term here; they become a
   dedicated high-risk child (step 6 and "SUEWS high-risk files"), which is this
   skill's form of `queue-pr`'s integration PR. `queue-pr`'s `serial` shape has no
   analogue: a carved child's dependency is fixed by how it is carved.
4. Propose the split plan: which files/commits go in which child, the order, and
   independent-vs-stacked. Present it for approval before any surgery.

### 2. Set up isolation

1. Keep the original branch/PR untouched. Do not check out PR branches in an
   active worktree.
2. Pre-flight for leftovers from a prior interrupted run (never silent, never
   auto-delete):
   ```bash
   git worktree list
   git branch --list 'split/<pr>-*'
   ```
   If any `split/<pr>-*` branch or `../split-<pr>/*` worktree already exists, STOP
   and report it. Then take one human-gated path: (a) resume, if the leftovers
   match the approved plan; or (b) clean and re-carve, only on explicit approval:
   ```bash
   git worktree remove --force ../split-<pr>/<x>
   git worktree prune
   git branch -D split/<pr>-<x>          # branch persists after path removal
   ```
3. Create a coordinator area and per-child worktrees. Fresh carve (new branch off
   the agreed base):
   ```bash
   mkdir -p ../split-<pr>
   git worktree add -b split/<pr>-a ../split-<pr>/a origin/<base>
   ```
   For a stacked child, use `-b split/<pr>-<x>` off its parent child instead of
   the base. The `-b` form fails with `fatal: a branch named '...' already exists`
   (exit 255) if the branch is still present, so it must not be used on the resume
   path. Resume instead re-attaches a worktree to an EXISTING leftover branch
   (no `-b`):
   ```bash
   git worktree add ../split-<pr>/a split/<pr>-a
   ```
   After re-attaching, confirm the branch tip matches the approved plan before
   continuing. Tear down the per-child worktrees after the stack is pushed
   (`git worktree remove ../split-<pr>/<x>`), leaving the branches intact.

### 3. Carve each child

- **By path** (files group cleanly): in the child worktree, bring in only that
  child's paths from the source head:
  ```bash
  git checkout <source-head> -- <path> [<path> ...]
  git commit  # focused subject. NOTE: authored by you, not the original author --
              # carving by path cannot preserve per-hunk authorship. Use the
              # by-commit/cherry-pick route below to retain attribution, or pass
              # --author="Name <email>". This fresh commit carries the
              # AI-collaboration trailer for the assistant that made it.
  ```
  Caveat -- by-path carve cannot capture deletions or renames. `git checkout
  <source-head> -- <path>` only copies files that exist at `<source-head>`, so a
  path DELETED between base and head fails with `error: pathspec '<path>' did not
  match any file(s) known to git` and the deletion is never staged. Carve a
  deletion with `git rm <path>` in the child; for a rename, carve both sides
  (`git rm <old-path>` and `git checkout <source-head> -- <new-path>`). The D/R
  statuses from step 1's `git diff --name-status` tell you which paths need this.
- **By commit** (the source has clean per-concern commits): cherry-pick the
  commit group onto the child branch (cherry-pick preserves the original author).
- Carve out mechanical/generated changes (formatting sweeps, regenerated
  artefacts, large fixture updates) into their own child PR first so the semantic
  children stay small and reviewable.

### 4. Verify each child independently

Run the green-tree check on each child on its own (a stacked child on top of its
parent): build if needed, then `make test-smoke`, plus any targeted check the
child's files warrant (`pytest`, `ruff check`, `fprettify --diff`). If a child is
not green in isolation, the boundary is wrong; re-carve rather than papering over
it.

### 5. Completeness check (critical gate)

Confirm the stack reproduces the original exactly, with nothing dropped or
duplicated, using a single shape-general recombine that covers stacked,
independent, and mixed shapes. Identify the LEAF branches that jointly cover every
concern: every independent child, plus the TIP of each stacked sub-series (never a
non-tip stacked child, whose commits its tip already contains). In a throwaway
worktree off the base, octopus-merge exactly that leaf set, diff against the source
head, then tear the worktree down:

```bash
git worktree add ../split-<pr>/_recombine origin/<base>
git -C ../split-<pr>/_recombine merge --no-edit <leaf-1> <leaf-2> ...   # each independent child + each stacked sub-series TIP
git -C ../split-<pr>/_recombine diff <source-head> HEAD                 # must be empty
git worktree remove --force ../split-<pr>/_recombine
```

Fast path: for a PURELY stacked stack with no independent siblings, the single tip
child already contains everything, so `git diff <source-head> <tip-child-branch>`
suffices. Do not use the fast path in a mixed shape -- it false-flags the
independent child's hunks as a phantom diff.

If any recombine merge reports CONFLICT (non-zero exit, or `git status` shows
`UU`), the children share an overlapping hunk -- the boundary is wrong. Run
`git merge --abort` and re-carve; do NOT hand-resolve (that masks a bad split),
and do not trust the subsequent `git diff`, which would run against a half-merged
state. If the merge succeeds cleanly but the diff is non-empty, a change was lost
or duplicated. Resolve before proceeding; never report a split complete while this
diff is non-empty.

### 6. Open the stack

After approval, push the child branches and open the PRs:

- Independent children: base each on `<base>`.
- Stacked children: base each PR on its parent child branch; note the stacking in
  the PR body so a reviewer reads them in order.
- Link every child to the original (`Refs #<original>`), and list the stack in
  each child body. Schema/sample-config/build/CHANGELOG changes should be their
  own child (schema bumps already warrant a dedicated change per
  `.claude/rules/python/schema-versioning.md`; see also the "one PR per logical
  group" rule in `.claude/rules/naming-convention.md`).

### 7. Retire the original and hand off

- Recommend closing or converting the original PR to a tracking/umbrella issue
  once the stack is up, preserving its discussion. This is human-gated; do not
  auto-close.
- Hand the stack to `queue-pr` for ordering and merge-queue entry. Handing off
  does not transfer merge authority: enqueue/merge stays a separate,
  explicitly-approved step under `queue-pr`'s own human gate.

## SUEWS high-risk files

Carve these into their own child PR rather than scattering them across children:
schema/public API and sample configuration, `meson.build` / `src/supy/meson.build`,
`.github/workflows/*`, `CHANGELOG.md`, dependency/lock files, generated docs
indexes, and shared test fixtures/baselines. See the full list in `queue-pr`.

## Decision Rules

- Prefer carving by path when files group cleanly; carve by commit when the
  history is already concern-clean.
- Independent children become parallel PRs; dependent children become a stack.
  Do not hide a dependency by basing a dependent child directly on the base.
- A child that cannot be made green in isolation signals a wrong boundary, not a
  reason to bundle.
- Stop and report on any semantic/scientific/schema conflict; do not invent a
  resolution to make Git clean.

## Output Format

End with:

```text
[split-pr] stack plan
Source: #<pr> <title> (or branch)
Shape: independent | stacked | mixed
Concerns: <N identified in step 1> -> Children: <M created>   # must match or explain
Children:
  1. split/<pr>-a - <concern> - checks: test-smoke pass/blocked - PR <url|not pushed>
  2. split/<pr>-b - <concern> - checks: ... - PR <url|not pushed>
Completeness: empty-diff confirmed | MISMATCH: <what>
Original: untouched | retire recommended (human) | closed (approved)
Approvals: <none | push+open approved | retire approved>
Remaining: <none | explicit blocker>
Next: hand the stack to queue-pr for ordering and merge.
```

If the workflow stops early, use the same format and set `Remaining` to the
specific blocker.
