---
allowed-tools: Bash(git:*), Bash(cd:*), Bash(uv:*), Bash(make:*), Bash(ls:*), Bash(source:*), Bash(echo:*), Bash(cat:*), Bash(rm:*), Bash(test:*), Bash(python:*), Bash(gh:*), Bash(date:*), LS, Read, Write, Edit
description: Streamlined worktree management for SUEWS development
---

# Worktree Management for SUEWS

## Current Status
Let me check your current development environment:
- **Location**: Show current working directory
- **Branch**: Display current git branch (if in a git repository)
- **Worktree**: Check if we're in a git worktree
- **Active worktrees**: Count total number of worktrees
- **Environment**: Show active Python environment name
- **Uncommitted changes**: Count files with uncommitted changes

## Available Plans
Let me check the feature plans in .claude/plans/:
- **Todo**: Count planned features in todo/
- **Doing**: Count active features in doing/
- **Done**: Count completed features in done/

## Your Command: $ARGUMENTS

Based on the command provided, I'll help you with worktree management.

### Available Commands:

1. **`new`** - Start a new feature worktree
   - Interactive setup with feature name, issue, and lead developer
   - Creates worktree, plan, and environment automatically
   
2. **`sync`** - Synchronize with master
   - Pull latest changes from master
   - Update dependencies if needed
   - Show any conflicts
   
3. **`pr`** - Create a pull request
   - Push changes and create PR
   - Link to GitHub issue
   - Show PR URL
   
4. **`finish`** - Complete or abandon worktree
   - Option to complete via PR or abandon
   - Document outcome and clean up
   - Archive plan appropriately

### Quick Actions:

If no command specified, I'll:
- Show detailed status if in a worktree
- List all worktrees if in main repo
- Suggest next actions based on context

---

## Command Implementation

Based on your command: **$ARGUMENTS**

I'll help you manage your worktrees based on what you need.

### If you want to finish a worktree:

Navigate to the worktree directory first: `cd worktrees/FEATURE_NAME`

#### Enhanced Finish Flow
Let me check the current status:
- **Current worktree**: Check if we're in a worktree and get its name
- **Branch**: Get the current git branch name
- **Uncommitted changes**: Count any uncommitted files
- **PR status**: Check if there's a pull request for this branch and its status (open/merged)

**Finish options:**
1. **Merged via PR** - PR is merged on GitHub
2. **Merged locally** - Changes merged without PR
3. **Pause work** - Keep worktree, mark as paused
4. **Abandon** - Delete worktree without merging

Tell me your choice (1-4) and I'll:
- Update the plan with completion details
- Generate a cleanup script with recovery backup
- Archive the plan appropriately
- Clean up the worktree and environment

### For other commands:

- `/worktree new` - Create a new feature worktree
- `/worktree sync` - Sync with master branch
- `/worktree pr` - Create a pull request
- `/worktree` (no args) - Show current status

**Current context**: Let me check your current location and git branch

## Implementation Notes:

When creating a new worktree:
1. Check prerequisites (clean state, up-to-date master)
2. Create worktree with feature branch
3. Create plan file with lead developer info
4. Set up Python environment with uv
5. Run initial build
6. Move plan to "doing" state

When finishing a worktree:
1. Check completion method (PR or abandon)
2. Update plan with outcome and timestamp
3. Move plan to "done" directory
4. Clean up worktree and environment
5. Commit plan changes to master

Use interactive prompts to gather needed information rather than parsing complex arguments.