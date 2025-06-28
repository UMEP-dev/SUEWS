# Claude Code Automation Scripts

This directory contains scripts to streamline worktree management for Claude Code, reducing friction when working with multiple features in parallel.

## Available Scripts

### worktree-setup.sh
Creates a new worktree with an isolated Python virtual environment.

**Usage:**
```bash
./.claude/scripts/worktree-setup.sh feature-name
```

**What it does:**
1. Creates git worktree at `worktrees/feature-name`
2. Sets up Python venv (faster than mamba)
3. Installs package in development mode
4. Creates quick activation script
5. Runs initial build

**Example:**
```bash
./.claude/scripts/worktree-setup.sh yaml-validation
cd worktrees/yaml-validation
source .venv/bin/activate  # or ./activate.sh
make test
```

### worktree-cleanup.sh
Removes a worktree and associated resources.

**Usage:**
```bash
./.claude/scripts/worktree-cleanup.sh feature-name
```

**What it does:**
1. Removes git worktree
2. Cleans up any legacy mamba environments
3. Removes worktree plan from `.claude/worktree-plans/`
4. Shows remaining worktrees

## Benefits for Claude Code

1. **Single Command Setup**: No need to remember multiple steps
2. **Fast Environment Creation**: Uses venv instead of slow mamba clone
3. **No Shell Issues**: Works reliably without shell configuration
4. **Clean Isolation**: Each worktree is completely independent
5. **Easy Cleanup**: One command removes everything

## Quick Start for New Feature

```bash
# 1. Create feature branch
git checkout -b feature/my-awesome-feature
git push -u origin feature/my-awesome-feature

# 2. Setup worktree
./.claude/scripts/worktree-setup.sh my-awesome-feature

# 3. Start working
cd worktrees/my-awesome-feature
./activate.sh
make test

# 4. When done
./.claude/scripts/worktree-cleanup.sh my-awesome-feature
```

## Troubleshooting

**"Failed to create worktree"**
- Ensure the feature branch exists: `git branch -r | grep feature/name`

**"Failed to install package"**
- Check Python version: `python --version` (needs 3.9+)
- Try manual install: `pip install -e .`

**"make dev failed"**
- This is often OK on first setup
- Try running `make dev` manually after activation

## Legacy Mamba Environments

The cleanup script will detect and remove old mamba environments named `suews-dev-{feature}`. This helps transition from the old workflow to the new streamlined approach.