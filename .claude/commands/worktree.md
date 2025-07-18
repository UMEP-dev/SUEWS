---
allowed-tools: Bash(git:*), Bash(cd:*), Bash(uv:*), Bash(make:*), Bash(ls:*), Bash(source:*), Bash(echo:*), Bash(cat:*), Bash(rm:*), Bash(test:*), Bash(python:*), Bash(gh:*), Bash(date:*), LS, Read, Write, Edit
description: Streamlined worktree management for SUEWS development
---

# Worktree Management for SUEWS

## Current Status
- **Location**: !`pwd`
- **Branch**: !`git branch --show-current 2>/dev/null || echo "Not in git repo"`
- **Worktree**: !`git worktree list | grep -E "^\$(pwd)" | head -1 || echo "Not in worktree"`
- **Active worktrees**: !`git worktree list | wc -l | xargs -I {} echo "{} worktrees"`
- **Environment**: !`python -c "import sys; print(sys.prefix)" 2>/dev/null | xargs basename || echo "No Python env"`
- **Uncommitted changes**: !`git status --porcelain 2>/dev/null | wc -l | xargs -I {} echo "{} files" || echo "N/A"`

## Available Plans
- **Todo**: !`ls .claude/plans/todo/feature-*.md 2>/dev/null | wc -l | xargs -I {} echo "{} planned" || echo "0"`
- **Doing**: !`ls .claude/plans/doing/feature-*.md 2>/dev/null | wc -l | xargs -I {} echo "{} active" || echo "0"`
- **Done**: !`ls .claude/plans/done/feature-*.md 2>/dev/null | wc -l | xargs -I {} echo "{} completed" || echo "0"`

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
- **Current worktree**: !`if git worktree list | grep -q "$(pwd)"; then basename $(pwd); else echo "Not in worktree"; fi`
- **Branch**: !`git branch --show-current 2>/dev/null || echo "No branch"`
- **Uncommitted**: !`git status --porcelain 2>/dev/null | wc -l | xargs -I {} echo "{} files"`
- **PR status**: !`BRANCH=$(git branch --show-current 2>/dev/null); if [[ -n "$BRANCH" ]]; then PR_INFO=$(gh pr list --head "$BRANCH" --json number,state,merged --limit 1 2>/dev/null || echo "[]"); if [[ "$PR_INFO" != "[]" ]]; then echo "$PR_INFO" | jq -r '"PR #" + (.[0].number|tostring) + " - " + .[0].state + " (merged: " + (.[0].merged|tostring) + ")"'; else echo "No PR found"; fi; else echo "No branch"; fi`

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

**Current context**: !`pwd` on branch !`git branch --show-current 2>/dev/null || echo "No branch"`

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