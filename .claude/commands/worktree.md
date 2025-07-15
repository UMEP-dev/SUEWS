---
allowed-tools: Bash(git:*), Bash(cd:*), Bash(uv:*), Bash(make:*), Bash(ls:*), Bash(source:*), Bash(echo:*), Bash(cat:*), Bash(rm:*), Bash(test:*), Bash(python:*), LS, Read
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
   - Set up uv environment with core dependencies
   - Run `make dev` to build SUEWS
   - Create marker file and test setup

2. **Switch to existing worktree** (if args contain "switch", "cd", or "goto"):
   - Change to specified worktree directory
   - Activate environment if needed
   - Show current status

3. **List worktrees** (if args contain "list", "ls", or "show"):
   - Show all active worktrees with their branches
   - Show current location if in a worktree

4. **Clean up worktree** (if args contain "remove", "clean", or "delete"):
   - Remove specified worktree
   - Remove associated plan file if exists
   - Clean up environment

5. **Status check** (if no specific action or args contain "status"):
   - Show current worktree status
   - Check if in worktree vs main repo
   - Show environment info

Use the commands from `.claude/howto/setup-worktree.md` as reference for implementation.