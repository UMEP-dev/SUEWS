---
name: worktree-merge-queue
description: Use whenever coordinating multiple Git worktrees, stacked PRs, ready-for-review PRs, or GitHub merge queue entries for SUEWS. This skill scans open ready-for-review PRs, orders them for merge queue entry, can enqueue them with gh, detects shared-file conflict risk, runs temporary-worktree merge preflights, and decides whether PRs should merge independently, serially, or as a stack. Use it even if the user only mentions parallel worktrees, merge queue bounce-backs, rebasing before queueing, PRs conflicting with each other, or putting all ready PRs into the merge queue.
---

# Worktree Merge Queue

Coordinate concurrent SUEWS worktrees so PRs do not surprise each other in the
GitHub merge queue. The aim is not to avoid all conflicts; it is to discover
them before queue entry, choose a sensible ordering, and keep history updates
intentional.

## Quick Start

Scan ready-for-review PRs and propose a merge queue order:

```bash
git fetch origin
.claude/skills/worktree-merge-queue/scripts/queue-ready-prs.sh --base master
```

After the user explicitly asks to queue them, enqueue in the proposed order:

```bash
.claude/skills/worktree-merge-queue/scripts/queue-ready-prs.sh --base master --enqueue
```

For the current local branch:

```bash
git fetch origin
.claude/skills/worktree-merge-queue/scripts/preflight.sh --base origin/master
```

For several local branches:

```bash
git fetch origin
.claude/skills/worktree-merge-queue/scripts/preflight.sh --base origin/master branch-a branch-b branch-c
```

Run the relevant project check before queueing:

```bash
make test-smoke
```

## Workflow

1. **Scan ready-for-review PRs**
   - Prefer the queue scanner when the user asks about all ready PRs, merge
     queue ordering, or queue bounce-backs across several PRs.
   - Run `git fetch origin`, then:
     `scripts/queue-ready-prs.sh --base master`.
   - Treat "ready for review" as open and non-draft. The scanner skips PRs
     with changes requested, merge conflicts, dirty merge state, unknown
     mergeability, or failing checks unless a future operator deliberately
     opts into a broader policy.
   - Read the generated order before acting. It sorts lower-risk PRs first:
     fewer high-risk SUEWS files, fewer shared files with other PRs, then
     smaller file count and older creation time.

2. **Enqueue only on explicit request**
   - If the user says "put them in the merge queue", "enqueue ready PRs", or
     equivalent, rerun the queue scanner with `--enqueue`.
   - The script uses `gh pr merge <N> --auto --match-head-commit <SHA>`.
     On merge-queue-protected branches, this adds the PR to the queue when
     requirements are satisfied, or enables auto-merge so it enters the queue
     once requirements pass.
   - Never pass `--admin`, never delete branches, and stop if GitHub rejects
     a PR instead of trying to force it through.

3. **Gather branch context for local worktrees**
   - Confirm the intended base is `origin/master` unless the user says
     otherwise.
   - Run `git fetch origin` so local merge simulation uses current remote
     state.
   - Check whether the current worktree is dirty with `git status --short`.
     Uncommitted changes are not part of merge queue validation; ask the user
     before stashing, committing, or discarding them.
   - Identify the candidate branches or PR heads. If the user gives PR
     numbers, inspect them with `gh pr view <N> --json number,title,headRefName,baseRefName,mergeStateStatus,files`.

4. **Detect overlap**
   - Run the preflight script when branches are local.
   - For remote PRs, compare changed files with `gh pr diff <N> --name-only`
     or `git diff --name-only origin/master...<branch>`.
   - Treat overlapping files as queue risk even when Git can auto-merge; the
     semantic conflict may still be real.

5. **Classify the PR set**
   - **Independent**: no shared files, separate subsystems, no logical
     dependency. Rebase each branch onto fresh `origin/master`, run checks,
     and queue in parallel if desired.
   - **Serial**: shared files or nearby logic, but no strict dependency.
     Queue the safest/lowest-level PR first. After it lands, rebase the next
     branch onto fresh `origin/master`.
   - **Stacked**: later PRs depend on earlier code or tests. Keep B based on A
     until A lands, then rebase B onto `origin/master`.
   - **Integration PR**: several PRs touch shared build, generated, or docs
     files. Move the shared edits into one PR or prepare a final integration
     branch.

6. **Run merge preflight**
   - Use the script to create a temporary detached worktree at the base and
     merge candidate branches in the proposed order.
   - If the script reports conflicts, do not queue the whole set together.
     Either serialise them, restack them, or make an integration PR.
   - If the script reports shared high-risk files, review those files manually
     before queueing even if the merge simulation succeeds.

7. **Prepare queue entry**
   - Rebase or merge from latest `origin/master` only after confirming the
     intended history policy for the branch.
   - Prefer `git push --force-with-lease` after a rebase.
   - Run `make test-smoke` before requesting merge queue entry.
   - Record the queue order and any known shared files in the PR comment or
     workspace note when coordinating several agents.

## SUEWS High-Risk Files

Treat these as coordination points:

- `CHANGELOG.md`
- `meson.build`
- `src/supy/meson.build`
- `.github/workflows/*`
- dependency or lock files
- generated docs indexes and tutorial outputs
- shared test fixtures and baseline outputs
- public API schema or sample configuration files

If two PRs edit any of these, assume the queue order matters until proven
otherwise.

## Decision Rules

- Queue independent PRs together only after fresh-base checks pass.
- Queue shared-file PRs one at a time unless a temporary integration merge has
  already succeeded and the shared edits are semantically compatible.
- Do not hide dependencies by rebasing dependent branches directly onto
  `origin/master`; use a stacked PR until the dependency lands.
- Do not run destructive commands such as `git reset --hard` or branch
  checkouts in another active worktree.
- Do not force-push unless the user asked you to update that PR branch or the
  branch is clearly yours to maintain.

## Report Format

When reporting a local-worktree preflight, use this structure:

```text
=== MERGE QUEUE PLAN ===
Base: origin/master
Branches: branch-a, branch-b, branch-c
Verdict: independent | serial | stacked | integration-needed

Order:
1. branch-a - reason
2. branch-b - reason

Shared files:
- path - branches touching it

Checks:
- preflight: pass/fail/not run
- make test-smoke: pass/fail/not run

Next commands:
- command
- command
```

Keep the final recommendation concrete: which branch goes first, which branch
waits, and what the next agent should run.

When reporting a repo-wide queue scan, use this structure:

```text
=== READY PR MERGE QUEUE PLAN ===
Base: master
Mode: dry-run | enqueue
Candidates: N open PRs, M ready-for-review, K queueable

Queue order:
1. #123 title - reason
2. #124 title - reason

Skipped:
- #125 title - reason

Shared files:
- path - #123, #124

Actions:
- queued #123 | enabled auto-merge for #123 | skipped #123
```

Always include the PR numbers. The user needs to see exactly what was queued
and what was left alone.
