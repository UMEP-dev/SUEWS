---
allowed-tools: Bash(git:*), Bash(cd:*), Bash(uv:*), Bash(make:*), Bash(ls:*), Bash(source:*), Bash(echo:*), Bash(cat:*), Bash(rm:*), Bash(test:*), Bash(python:*), LS, Read, Write
description: Manage git worktrees for SUEWS development
---

# Worktree Management for SUEWS

## Current Status
- **Current directory**: !`pwd`
- **Current branch**: !`git branch --show-current 2>/dev/null || echo "Not in git repo"`
- **Active worktrees**: !`git worktree list 2>/dev/null || echo "No worktrees"`
- **Current environment**: !`python -c "import sys; print(sys.prefix)" 2>/dev/null || echo "No Python"`

## Your task
Based on the arguments provided: $ARGUMENTS

Choose the appropriate action:

1. **Create new worktree** (if args contain "create", "new", or "add"):
   - Parse feature name from arguments
   - Create `worktrees/{feature}` directory with branch `feature/{feature}`
   - **Create plan file in master branch** (`.claude/plans/todo/feature-{feature}.md`)
   - Set up uv environment with core dependencies
   - Run `make dev` to build SUEWS
   - Create marker file and test setup
   - Show instructions for accessing the plan

2. **Switch to existing worktree** (if args contain "switch", "cd", or "goto"):
   - Change to specified worktree directory
   - Activate environment if needed
   - Show current status
   - Check if plan exists and show its location

3. **List worktrees** (if args contain "list", "ls", or "show"):
   - Show all active worktrees with their branches
   - Show current location if in a worktree
   - List associated plan files

4. **Clean up worktree** (if args contain "remove", "clean", or "delete"):
   - Remove specified worktree
   - Remove associated plan file from `.claude/plans/*/`
   - Clean up environment
   - Commit cleanup changes to master

5. **Status check** (if no specific action or args contain "status"):
   - Show current worktree status
   - Check if in worktree vs main repo
   - Show environment info
   - Show plan file status if applicable

Use the commands from `.claude/howto/setup-worktree.md` as reference for implementation.

## Important Notes:
- Plan files should be created in the master branch in `.claude/plans/todo/`
- When working on a feature, move plan from `todo/` to `doing/`
- After completion, move plan from `doing/` to `done/`
- Remove plan from `done/` when cleaning up merged worktrees