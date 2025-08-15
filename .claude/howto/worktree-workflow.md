# Worktree Workflow Guide

This guide explains the streamlined worktree workflow for SUEWS development using the `/worktree` slash command.

## Overview

The `/worktree` command provides four simple subcommands for managing feature development:
- `new` - Start a new feature
- `sync` - Synchronize with master
- `pr` - Create a pull request
- `finish` - Complete or abandon the feature

## Recommended Launch Method

**Always launch Claude Code from the master branch** for the smoothest workflow:

```bash
# Navigate to main repository (master branch)
cd ~/Dropbox\ \(Personal\)/6.Repos/SUEWS
claude .  # Launch from master
```

Then Claude Code works primarily from master:
- Has full visibility of all files and plans
- Can edit any file using paths: `worktrees/feature-name/src/file.py`
- Can update plans directly: `.claude/plans/doing/feature-name.md`

When specific operations are needed:
```bash
cd worktrees/feature-name  # Enter worktree for focused work
make test                  # Run tests
git add -A                 # Stage changes
git commit                 # Commit
cd ../..                   # Return to master
```

This approach ensures:
- Claude Code maintains full project context from master
- Plans are always directly accessible
- Temporary cd for specific operations only
- Clean separation between overview (master) and focused work (worktree)

## Complete Workflow Example

### 1. Starting a New Feature

In Claude Code interactive mode (launched from master), type:
```
/worktree new
```

Claude will interactively ask for:
- **Feature name**: e.g., "user-authentication"
- **GitHub issue** (optional): e.g., "123"
- **Lead developer**: Your GitHub handle, e.g., "@sunt05"

What happens:
1. Creates worktree at `worktrees/user-authentication`
2. Creates feature branch `feature/user-authentication`
3. Sets up Python environment with uv
4. Creates simple plan file at `.claude/plans/doing/feature-user-authentication.md`
5. Moves plan to "doing" state
6. Runs initial build

**Note**: `/worktree new` always creates a simple plan. For complex features requiring detailed specifications, manually create the directory structure as described in `.claude/plans/README.md`

### 2. Development Workflow

While developing, you may need to:

**Sync with master** (pull latest changes):
```
/worktree sync
```

This will:
- Pull latest master changes
- Merge into your feature branch
- Update dependencies if needed
- Rebuild if necessary

**Check status**:
```
/worktree
```

Shows current worktree status and suggests next actions.

### 3. Creating a Pull Request

When ready to submit your work:

```
/worktree pr
```

This will:
- Check for uncommitted changes
- Push your branch to origin
- Create PR using GitHub CLI
- Link to the GitHub issue (if specified)
- Show the PR URL

### 4. Finishing the Feature

When done (either merged or abandoned):

```
/worktree finish
```

Claude will ask:
- "Complete via PR or abandon? [pr/abandon]"

**If completing via PR**:
- Ensures PR is created/merged
- Updates plan with completion status
- Moves plan to "done" with PR number
- Cleans up worktree and environment

**If abandoning**:
- Asks for abandonment reason
- Documents reason in plan file
- Moves plan to "done" with abandoned status
- Cleans up worktree and environment

## Lead Developer Tracking

Each feature plan tracks:
- **Lead developer**: GitHub handle of primary developer
- **Start date**: When work began
- **Completion date**: When finished
- **Outcome**: completed or abandoned
- **PR number**: If completed via PR
- **Abandonment reason**: If abandoned

This helps with:
- Accountability and ownership
- Historical tracking
- Knowledge transfer
- Project management

## Best Practices

### When to Create a Worktree
- Any feature requiring more than trivial changes
- Bug fixes that need extensive testing
- Experimental features
- Anything linked to a GitHub issue

### Choosing Plan Complexity

**Simple Plan** (single file):
- Bug fixes (< 1 day)
- Small enhancements
- Documentation updates
- Single developer work
- No API changes

**Complex Plan** (directory):
- New features with user-facing changes
- API or breaking changes
- Multi-component modifications
- Features requiring formal requirements
- Multi-developer collaboration

### Complex Feature Workflow

For features requiring detailed specifications:

1. **Manually create complex plan structure**:
   ```bash
   # Create directory in todo
   mkdir -p .claude/plans/todo/feature-{name}
   
   # Create README.md, requirements.md, design.md, tasks.md
   # See .claude/plans/README.md for templates
   ```

2. **Move to doing when ready**:
   ```bash
   # When starting implementation
   mv .claude/plans/todo/feature-{name} .claude/plans/doing/
   ```

3. **Create worktree for implementation**:
   ```bash
   /worktree new
   # Use same feature name
   ```

4. **Update progress during implementation**:
   ```bash
   # From worktree, update status
   cd worktrees/feature-name
   $EDITOR ../../.claude/plans/doing/feature-name/README.md
   ```

### Worktree Naming
- Use descriptive names: `user-auth` not `feature1`
- Match GitHub issue titles when applicable
- Keep names short but meaningful

### Plan Management
- Update progress regularly in README.md
- For complex features, keep requirements/design/tasks stable
- Document key decisions in design.md
- Note blocking issues in README.md

### Collaboration
- One lead developer per feature
- Others can contribute via PRs to feature branch
- For complex features, share requirements.md for review
- Handover by updating lead developer in plan

## Common Scenarios

### Scenario 1: Quick Bug Fix
```bash
claude -p "/worktree new"
# Feature name: fix-validation-error
# Issue: 456
# Lead: @sunt05

# ... make fixes ...

claude -p "/worktree pr"
claude -p "/worktree finish"
# Complete via PR
```

### Scenario 2: Long-Running Feature
```bash
claude -p "/worktree new"
# Feature name: yaml-configuration
# Issue: 123
# Lead: @sunt05

# Work for several days...
claude -p "/worktree sync"  # Stay updated

# Multiple commits...
claude -p "/worktree pr"

# After review and merge...
claude -p "/worktree finish"
```

### Scenario 3: Abandoned Experiment
```bash
claude -p "/worktree new"
# Feature name: experimental-solver
# Lead: @sunt05

# After testing...
claude -p "/worktree finish"
# Abandon
# Reason: Performance worse than current implementation
```

## Troubleshooting

### "Worktree already exists"
- Use a different feature name
- Or remove old worktree first

### "Uncommitted changes"
- Commit or stash changes before sync/finish
- Use `git status` to see what's changed

### "Merge conflicts"
- After sync, resolve conflicts manually
- Commit the merge resolution
- Continue with development

### "Environment issues"
- Ensure you're in the worktree directory
- Check `.venv` exists
- Rebuild with `make clean && make dev` (or `make uv-dev` for fresh setup)

## Manual Commands Reference

If you prefer manual control:

```bash
# Create worktree
git worktree add worktrees/my-feature feature/my-feature

# Create plan
cp .claude/templates/feature-plan.md .claude/plans/todo/feature-my-feature.md
# Edit plan with your info

# Setup environment
cd worktrees/my-feature
uv venv
source .venv/bin/activate
# Option 1: Use the convenient recipe
make uv-dev

# Option 2: Manual setup
uv pip install -r requirements-dev.txt
make dev

# Clean up
git worktree remove worktrees/my-feature
rm -rf .venv
```

But the `/worktree` command handles all this automatically!