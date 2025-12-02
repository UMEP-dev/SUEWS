# Merge Queue Guide

> **Purpose**: Best practices for using GitHub's merge queue to ensure safe, efficient PR merging
>
> **Audience**: All contributors with merge permissions
>
> **Last Updated**: 1 December 2025

## Overview

GitHub's merge queue ensures PRs are tested against the latest `master` branch before merging. This prevents "semantic merge conflicts" where two PRs pass CI individually but break when combined.

When you add a PR to the merge queue:
1. GitHub creates a temporary merge commit (your PR + current `master`)
2. CI runs against this merged state
3. If all checks pass, the PR merges automatically
4. If checks fail, the PR is removed from the queue for investigation

## How SUEWS Merge Queue Works

The merge queue is configured in `.github/workflows/build-publish_to_pypi.yml`:

### Trigger
```yaml
merge_group:
  types: [checks_requested]
```

### Validation Matrix
- **Platforms**: Linux (manylinux) + ARM Mac + Windows
- **Python versions**: 3.9 and 3.14 (bookend versions)
- **Test tier**: `standard` (more thorough than PR smoke tests)

### Gate Job
The `pr-gate` job consolidates all build results into a single required check for branch protection.

## Using the Merge Queue

### Adding a PR to the Queue

**Via GitHub CLI:**
```bash
# Add PR to merge queue
gh pr merge <PR_NUMBER> --merge-queue

# Or with specific merge method
gh pr merge <PR_NUMBER> --merge-queue --squash
```

**Via GitHub Web UI:**
1. Navigate to the PR
2. Click "Merge when ready" (or "Add to merge queue")
3. PR enters the queue

### Checking Queue Status

```bash
# View your PR's queue status
gh pr view <PR_NUMBER> --json mergeStateStatus,statusCheckRollup

# List PRs currently in queue
gh pr list --state open --json number,title,mergeStateStatus | \
  jq '.[] | select(.mergeStateStatus | contains("QUEUE"))'

# Check queue position (requires gh extension or API)
gh api repos/UMEP-dev/SUEWS/queue
```

### Removing from Queue

```bash
# Remove PR from merge queue
gh pr edit <PR_NUMBER> --remove-from-merge-queue
```

## Best Practices

### Before Adding to Queue

1. **Ensure PR CI passes first** - All required checks must pass before queueing
2. **Rebase on latest master** - Reduces queue validation time:
   ```bash
   git fetch origin master
   git rebase origin/master
   git push --force-with-lease
   # Wait for PR CI to pass, then queue
   ```
3. **Check queue is clear** - Optional but reduces wait time

### PR Strategies for Queue Efficiency

| Scenario | Recommendation |
|----------|----------------|
| Independent PRs (different files) | Queue together - low conflict risk |
| Related PRs (same module) | Queue sequentially - test integration |
| Large refactoring PR | Queue alone - easier to debug failures |
| Docs-only PRs | Queue freely - builds are skipped |

### Batching PRs

GitHub can batch multiple PRs for faster throughput:

**Safe to batch:**
- Multiple small, independent bug fixes
- Separate documentation improvements
- Feature + its tests (if in separate PRs)

**Queue sequentially:**
- PRs modifying same Fortran physics modules
- PRs touching shared config/data models
- Major refactoring + dependent clean-up PRs

### Queue Etiquette

1. **Don't force-push while queued** - Removes PR from queue
2. **Monitor your PR** - Check for queue failures
3. **Unblock when failing** - Remove failing PRs promptly:
   ```bash
   gh pr edit <PR_NUMBER> --remove-from-merge-queue
   ```
4. **Communicate** - If investigating a queue failure, comment on the PR

## Troubleshooting

### Queue Validation Failed

**Symptoms:** PR passes PR CI but fails in merge queue

**Common causes:**
1. **Another PR merged that conflicts:**
   ```bash
   # Rebase and retry
   git fetch origin master
   git rebase origin/master
   git push --force-with-lease
   # Wait for PR CI, then re-queue
   ```

2. **Flaky test:**
   - Check if failure is intermittent
   - Re-add to queue if clearly flaky
   - Report flaky test in issue tracker

3. **Genuine conflict with queued PR:**
   - Wait for conflicting PR to merge or leave queue
   - Then rebase and re-queue

### PR Stuck in Queue

**Symptoms:** PR in queue but not progressing

**Solutions:**
```bash
# Check queue status
gh pr view <PR_NUMBER> --json mergeStateStatus

# Check if checks are running
gh pr checks <PR_NUMBER> --watch

# If stuck, remove and re-add
gh pr edit <PR_NUMBER> --remove-from-merge-queue
gh pr merge <PR_NUMBER> --merge-queue
```

### Queue "Poisoned" by Failing PR

**Symptoms:** Other PRs blocked by a failing PR in queue

**Solution:** Anyone with write access can remove the failing PR:
```bash
gh pr edit <FAILING_PR_NUMBER> --remove-from-merge-queue
```

### Merge Queue vs Direct Merge

**When merge queue is required:**
- All PRs to `master` (enforced by branch protection)

**What bypasses the queue:**
- Emergency fixes: Admins can bypass with appropriate permissions
- Automated releases: Tag-triggered workflows bypass PR flow

## CI Behaviour in Merge Queue

### Build Matrix Differences

| Event | Platforms | Python Versions | Test Tier |
|-------|-----------|-----------------|-----------|
| Draft PR | manylinux only | 3.9, 3.14 | smoke |
| Ready PR | Linux, ARM Mac, Win | 3.9, 3.14 | standard |
| **Merge Queue** | Linux, ARM Mac, Win | 3.9, 3.14 | standard |
| Nightly/Release | All (incl. x86 Mac) | 3.9-3.14 | all |

### Path-Based Skipping

Documentation-only PRs skip builds even in merge queue:
- `detect-changes` job runs first
- If no code changes detected, builds are skipped
- `pr-gate` still passes, allowing merge

### UMEP Validation

Merge queue includes UMEP variant testing:
- Windows Python 3.12 only (QGIS 3.40 LTR compatibility)
- Validates NumPy 1.x compatibility

## Workflow Integration

### With Stacked PRs

When using stacked PRs (see `FEATURE_DEVELOPMENT_WORKFLOW.md`):

```
PR1 (targets master) → Queue and merge first
PR2 (targets PR1's branch) → GitHub auto-retargets to master after PR1 merges
PR2 → Then queue PR2
```

**Important:** Don't add dependent PRs to queue simultaneously - they may fail due to missing base.

### With Scientific Review

From `REVIEW_PROCESS.md`:
1. Complete scientific review before queueing
2. Ensure domain expert has approved
3. Then add to merge queue

### With Release Process

From `RELEASE_MANUAL.md`:
- Production releases use tags, not merge queue
- Merge queue validates pre-release PRs
- Tag push triggers full build matrix (bypasses queue)

## Quick Reference

```bash
# Add to queue (squash merge)
gh pr merge <PR> --merge-queue --squash

# Check queue status
gh pr view <PR> --json mergeStateStatus

# Remove from queue
gh pr edit <PR> --remove-from-merge-queue

# Watch CI progress
gh pr checks <PR> --watch

# List queued PRs
gh pr list --json number,title,mergeStateStatus | \
  jq '.[] | select(.mergeStateStatus | contains("QUEUE"))'
```

## See Also

- `FEATURE_DEVELOPMENT_WORKFLOW.md` - Managing stacked and parallel PRs
- `REVIEW_PROCESS.md` - Scientific review before merging
- `RELEASE_MANUAL.md` - Release workflow (bypasses merge queue)
- `.github/workflows/build-publish_to_pypi.yml` - CI configuration

---

*This guide covers merge queue best practices for SUEWS contributors.*
*Last updated: 1 December 2025*
*Owner: SUEWS Development Team*
*Location: `/dev-ref/MERGE_QUEUE.md`*
