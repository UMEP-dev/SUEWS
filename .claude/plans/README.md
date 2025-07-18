# Claude Plans Directory

This directory contains branch-specific context and plans for Claude Code sessions when working with git worktrees or feature branches.

## Purpose

When working on complex features across multiple Claude Code sessions or in different git worktrees, context is lost between sessions. These plan files serve as a "context bridge" to maintain continuity.

## How It Works

1. **Claude Code automatically checks** for a plan file matching the current branch name
2. **Plans are committed to master** so they're available in all worktrees after pulling
3. **Plans track progress** and important decisions across sessions

### Why Plans Live in Master Branch

Plans are intentionally kept in the master branch rather than feature branches because:
- **Single source of truth**: All worktrees and Claude sessions see the same plan
- **No merge conflicts**: Plans don't interfere with code changes in feature branches
- **Easy sharing**: Multiple Claude sessions can work on the same feature with shared context
- **Clean feature branches**: Feature branches contain only code changes, not documentation

### Accessing Plans from Worktrees

When working in a worktree, plans are accessed via relative paths:
```bash
# From within worktrees/my-feature/
cat ../../.claude/plans/doing/feature-my-feature.md
```

### Updating Plans from Worktrees

Since plans live in master, updating them requires switching branches. See the detailed workflow in `CLAUDE.md` under "IMPORTANT: Updating Plans During Work". The basic flow is:
1. Edit plan using relative path from worktree
2. Switch to master in main repo to commit changes
3. Return to worktree to continue development

This workflow ensures all worktrees always have access to the latest plan information.

## File Naming Convention

Plans should be named: `feature-{branch-name}.md`

For example:
- Branch `feature/hide-internal-options` → `feature-hide-internal-options.md`
- Branch `feature/rsl-physics-fixes` → `feature-rsl-physics-fixes.md`

## Creating a New Plan

Use the template structure defined in CLAUDE.md:

```markdown
# Feature: [Feature Name]

## Context
Brief description of what this feature/fix addresses

## Progress Tracking
- [ ] Task 1
- [x] Task 2 (completed)
- [ ] Task 3

## Key Decisions
- Decision 1: Reasoning
- Decision 2: Reasoning

## Implementation Notes
Specific implementation details, gotchas, or important context

## Files to Modify
- `path/to/file1.py`
- `path/to/file2.py`
```

## Lifecycle

1. **Create** when starting a complex multi-session feature
2. **Update** progress as work proceeds
3. **Remove** when the feature branch is merged

## Cleanup

When a feature is complete and merged:
```bash
git rm .claude-plans/feature-{branch-name}.md
git commit -m "Remove completed plan for feature/{branch-name}"
```

## Current Plans

See the files in this directory for active feature plans.