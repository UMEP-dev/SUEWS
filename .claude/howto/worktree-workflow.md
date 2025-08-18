# Worktree Workflow Guide

This guide covers the complete workflow for feature development using git worktrees, from setup to completion.

## Quick Setup

```bash
# Set your feature name
FEATURE="my-feature"

# Create worktree
git worktree add worktrees/$FEATURE feature/$FEATURE || exit 1

# Navigate and setup environment
cd worktrees/$FEATURE
make setup  # Creates .venv with uv
source .venv/bin/activate
make dev    # Install in editable mode

# Create marker file
echo "$FEATURE" > .worktree-name

# Test your setup
python -c "import supy; print(f'✓ SuPy {supy.__version__} ready')"
```

## Complete Workflow with /worktree Command

The `/worktree` command provides a streamlined workflow for managing features:

### Available Subcommands
- `new` - Start a new feature
- `sync` - Synchronize with master
- `pr` - Create a pull request
- `finish` - Complete or abandon the feature

### 1. Starting a New Feature

```
/worktree new
```

Claude will interactively ask for:
- **Feature name**: e.g., "user-authentication"
- **GitHub issue** (optional): e.g., "123"
- **Lead developer**: Your GitHub handle, e.g., "@sunt05"

What happens automatically:
1. Creates worktree at `worktrees/user-authentication`
2. Creates feature branch `feature/user-authentication`
3. Sets up Python environment with uv
4. Links to GitHub issue for tracking
5. Runs initial build

**Note**: Feature tracking and specifications are managed through GitHub issues and PRs

### 2. Development Workflow

**Sync with master** (pull latest changes):
```
/worktree sync
```

**Check status**:
```
/worktree
```

### 3. Creating a Pull Request

```
/worktree pr
```

This will:
- Check for uncommitted changes
- Push your branch to origin
- Create PR using GitHub CLI
- Link to the GitHub issue
- Show the PR URL

### 4. Completing the Feature

```
/worktree finish
```

Choose to either:
- Complete via PR (if PR was created)
- Abandon with reason (archives plan with explanation)

## Recommended Launch Method

**Always launch Claude Code from the master branch** for the smoothest workflow:

```bash
# Navigate to main repository (master branch)
cd ~/Dropbox\ \(Personal\)/6.Repos/SUEWS
claude .  # Launch from master
```

Benefits of launching from master:
- Full visibility of all files and plans
- Edit any file using paths: `worktrees/feature-name/src/file.py`
- Update GitHub issue with progress and notes
- No need to switch branches for plan updates

When specific operations are needed:
```bash
cd worktrees/feature-name  # Enter worktree for focused work
make test                  # Run tests
git add -A                 # Stage changes
git commit                 # Commit
cd ../..                   # Return to master
```

## Manual Setup (Without /worktree Command)

### Create Worktree

```bash
# Set feature name
FEATURE="my-feature"

# Create worktree and branch
git worktree add worktrees/$FEATURE feature/$FEATURE

# Create plan from template
# Create GitHub issue for tracking the feature
# Edit plan with your information

# Navigate to worktree
cd worktrees/$FEATURE
```

### Setup Environment

#### Option 1: Using Makefile (Recommended)
```bash
make setup  # Creates .venv with uv
source .venv/bin/activate
make dev    # Install in editable mode
```

#### Option 2: Manual uv Setup
```bash
uv venv
source .venv/bin/activate
uv pip install -e ".[dev,docs]"
```

#### Option 3: Standard venv
```bash
python -m venv .venv
source .venv/bin/activate
make dev
```

#### Option 4: Mamba
```bash
mamba create -n suews-dev-$FEATURE --clone suews-dev
mamba activate suews-dev-$FEATURE
make dev
```

## Working with Issues and PRs

Feature tracking and context are managed through GitHub issues and pull requests.

### Checking Related Issue
```bash
gh issue view <issue-number>
```

### Updating Issue
```bash
gh issue comment <issue-number> --body "Progress update..."
```

## Cleanup

When feature is complete:

```bash
# From root directory
FEATURE="my-feature"

# Remove worktree
git worktree remove worktrees/$FEATURE --force

# Close related GitHub issue (if applicable)
# gh issue close <issue-number>

# List remaining worktrees
git worktree list
```

## Quick Activation Script

Create this in any worktree for easy activation:

```bash
# Create activate.sh in your worktree
cat > activate.sh << 'EOF'
#!/bin/bash
source .venv/bin/activate
echo "Activated environment for: $(cat .worktree-name 2>/dev/null || echo 'unknown')"
echo "Branch: $(git branch --show-current)"
EOF
chmod +x activate.sh

# Use it
./activate.sh
```

## Troubleshooting

### "Command not found: /worktree"
- Ensure you're using Claude Code in interactive mode
- The command is defined in `.claude/commands/worktree.md`

### "Already exists" error
- Remove existing worktree: `git worktree remove worktrees/NAME`
- Check for stale worktrees: `git worktree prune`

### "Environment issues"
- Ensure you're in the worktree directory
- Check `.venv` exists
- Rebuild: `make clean && make dev`

### "Plan not found"
- Track progress via GitHub issues and PRs
- Fetch latest: `git fetch origin master`

### Build Conflicts Between Worktrees
- Each worktree MUST have its own Python environment
- Never share `.venv` between worktrees
- See `setup-environment.md` for environment isolation

## Best Practices

1. **Always use worktrees** under the `worktrees/` directory
2. **Name worktrees** to match their feature branch
3. **Launch Claude Code from master** for best experience
4. **One environment per worktree** to avoid conflicts
5. **Update plans promptly** as you make progress
6. **Clean up** both worktree and plan when done
7. **Run tests** after setup with `make test`

## Manual Commands Reference

```bash
# Worktree management
git worktree add worktrees/NAME feature/NAME  # Create
git worktree list                             # List all
git worktree remove worktrees/NAME            # Remove
git worktree prune                            # Clean stale

# Environment setup
make setup                                     # Create .venv
source .venv/bin/activate                     # Activate
make dev                                       # Install SUEWS
make test                                      # Run tests
make clean                                     # Clean build

# Plan management
# Create GitHub issue → Work on feature → Create PR → Merge
```

## Complete Example

```bash
# Start new feature
/worktree new
# Enter: api-improvements
# Enter: 456
# Enter: @your-github-handle

# Claude creates everything, then you work
cd worktrees/api-improvements
# ... make changes ...
git add -A
git commit -m "feat: implement new API endpoints"

# Sync with master
/worktree sync

# Create PR when ready
/worktree pr

# After PR is merged
/worktree finish
# Choose: Complete via PR
```

