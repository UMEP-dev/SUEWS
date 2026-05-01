---
name: queue-pr
description: Use whenever coordinating multiple Git worktrees, stacked PRs, ready-for-review PRs, repair waves, or GitHub merge queue entries for SUEWS. This skill scans open ready-for-review PRs, orders them for merge queue entry, comments on each non-draft PR with its current queue position and worktree-specific fix suggestions, can coordinate a repair wave from one central workspace using dedicated per-PR worktrees, detects shared-file conflict risk, runs temporary-worktree merge preflights, and only enqueues after the coordinated PRs have been fixed and rechecked. Use it even if the user only mentions parallel worktrees, merge queue bounce-backs, rebasing before queueing, PRs conflicting with each other, fixing ready PRs as a batch, or putting ready PRs into the merge queue.
---

# Queue PR

Coordinate concurrent SUEWS worktrees so PRs do not surprise each other in the
GitHub merge queue. The aim is not to avoid all conflicts; it is to discover
them before queue entry, choose a sensible ordering, and keep history updates
intentional.

## Quick Start

Scan ready-for-review PRs and propose a coordination order:

```bash
git fetch origin
.claude/skills/queue-pr/scripts/queue-ready-prs.sh --base master
```

Write or update one coordination comment on each non-draft PR:

```bash
.claude/skills/queue-pr/scripts/queue-ready-prs.sh --base master --comment
```

For a central repair wave, keep the coordinator worktree on its own branch and
create dedicated sibling worktrees for PRs that need action:

```bash
mkdir -p ../mq-repair
git worktree add -b mq/pr-1391 ../mq-repair/pr-1391 <PR_HEAD_SHA>
```

Only after the PR worktrees have acted on those comments and a fresh scan is
clean, enqueue in the proposed order:

```bash
.claude/skills/queue-pr/scripts/queue-ready-prs.sh --base master --enqueue
```

For the current local branch:

```bash
git fetch origin
.claude/skills/queue-pr/scripts/preflight.sh --base origin/master
```

For several local branches:

```bash
git fetch origin
.claude/skills/queue-pr/scripts/preflight.sh --base origin/master branch-a branch-b branch-c
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

2. **Comment before queueing**
   - Treat comments as the normal coordination mechanism. Run:
     `scripts/queue-ready-prs.sh --base master --comment`.
   - The script writes or updates one marker-based comment on each non-draft
     PR. The comment includes current queue position, why the PR has that
     position, shared/high-risk files, and specific suggestions for the PR's
     own worktree.
   - Use the comments to fan work out to the separate PR worktrees. Those
     agents should rebase, fix conflicts, reduce shared-file overlap, run
     checks, and push their own branches.
   - After the PR worktrees report back, rerun the dry-run scan. Do not rely
     on stale queue numbers; they are snapshots.

3. **Enqueue only after coordination converges**
   - If the user says "put them in the merge queue", "enqueue ready PRs", or
     equivalent, first make sure a fresh dry-run still shows the intended
     order and no unexpected conflicts.
   - Then rerun the queue scanner with `--enqueue`.
   - The script uses `gh pr merge <N> --auto --match-head-commit <SHA>`.
     On merge-queue-protected branches, this adds the PR to the queue when
     requirements are satisfied, or enables auto-merge so it enters the queue
     once requirements pass.
   - Never pass `--admin`, never delete branches, and stop if GitHub rejects
     a PR instead of trying to force it through.

4. **Coordinate a central repair wave**
   - Use this when the user wants the skill-running workspace to coordinate
     several PRs in one pass.
   - Keep the coordinator worktree on its existing branch. Do not repeatedly
     `git checkout` PR branches in the coordinator directory; that loses
     state and collides with other active worktrees.
   - For each PR that needs action, create a dedicated sibling worktree such
     as `../mq-repair/pr-1391`. Use the PR head SHA from:
     `gh pr view 1391 --json headRefName,headRefOid,headRepositoryOwner,headRepository`.
   - Prefer detached or `mq/pr-<number>` local branches for diagnosis. Before
     pushing back to a PR branch, verify that the PR head is in this
     repository, the head SHA still matches the scan, and the user explicitly
     asked for the coordinator to push fixes.
   - In each PR worktree, perform the smallest safe action:
     `git fetch origin`, rebase or merge current `origin/master` according to
     branch policy, resolve conflicts, run focused checks, then `make test-smoke`.
   - If the conflict is semantic, scientific, or touches high-risk files such
     as schema/sample data/build files, stop and comment with the finding
     rather than inventing a resolution.
   - After each PR worktree is fixed or diagnosed, return to the coordinator
     worktree and rerun the dry-run scan. Update coordination comments so the
     PR-specific agents see current instructions.

5. **Gather branch context for local worktrees**
   - Confirm the intended base is `origin/master` unless the user says
     otherwise.
   - Run `git fetch origin` so local merge simulation uses current remote
     state.
   - Check whether the current worktree is dirty with `git status --short`.
     Uncommitted changes are not part of merge queue validation; ask the user
     before stashing, committing, or discarding them.
   - Identify the candidate branches or PR heads. If the user gives PR
     numbers, inspect them with `gh pr view <N> --json number,title,headRefName,baseRefName,mergeStateStatus,files`.

6. **Detect overlap**
   - Run the preflight script when branches are local.
   - For remote PRs, compare changed files with `gh pr diff <N> --name-only`
     or `git diff --name-only origin/master...<branch>`.
   - Treat overlapping files as queue risk even when Git can auto-merge; the
     semantic conflict may still be real.

7. **Classify the PR set**
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

8. **Run merge preflight**
   - Use the script to create a temporary detached worktree at the base and
     merge candidate branches in the proposed order.
   - If the script reports conflicts, do not queue the whole set together.
     Either serialise them, restack them, or make an integration PR.
   - If the script reports shared high-risk files, review those files manually
     before queueing even if the merge simulation succeeds.

9. **Prepare queue entry**
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
- Do not auto-resolve scientific or schema conflicts just because Git can be
  made clean. Leave a coordination comment and let the owner worktree handle
  the domain decision.

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
Mode: dry-run | comment | enqueue
Candidates: N open PRs, M ready-for-review, K queueable

Queue order:
1. #123 title - reason
2. #124 title - reason

Skipped:
- #125 title - reason

Shared files:
- path - #123, #124

Actions:
- comment updated #123 | queued #123 | skipped #123
```

Always include the PR numbers. The user needs to see exactly which PRs received
coordination comments, which PRs are currently queueable, and which PRs were
left alone because they are draft or blocked.
