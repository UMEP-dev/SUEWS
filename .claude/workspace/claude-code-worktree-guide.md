# Streamlined Worktree Guide for Claude Code

## Quick Setup (Copy-Paste Ready)

### Creating a New Worktree with Environment

```bash
# Set feature name
FEATURE="my-feature"

# Create worktree
git worktree add worktrees/$FEATURE feature/$FEATURE

# Create and use venv (faster than mamba clone)
cd worktrees/$FEATURE
python -m venv .venv
source .venv/bin/activate
pip install -e .
```

### Resuming Work in Existing Worktree

```bash
# Navigate and activate
cd worktrees/my-feature
source .venv/bin/activate

# Verify correct environment
python -c "import supy; print(supy.__file__)"
```

## Alternative: Simplified Mamba Approach

If mamba is required, use this streamlined approach:

```bash
# Create worktree
git worktree add worktrees/$FEATURE feature/$FEATURE
cd worktrees/$FEATURE

# Use pip install directly in base environment with --target
pip install -e . --target ./.local-env
export PYTHONPATH="$PWD/.local-env:$PYTHONPATH"
```

## Environment Detection for Claude Code

At the start of each session, run this detection:

```bash
# Quick environment check
if [ -d ".venv" ]; then
    echo "✓ Virtual environment detected"
    source .venv/bin/activate
elif [ -d ".local-env" ]; then
    echo "✓ Local install detected"
    export PYTHONPATH="$PWD/.local-env:$PYTHONPATH"
else
    echo "⚠️  No environment found - creating venv..."
    python -m venv .venv
    source .venv/bin/activate
    pip install -e .
fi

# Verify
python -c "import supy; print(f'Using supy from: {supy.__file__}')"
```

## Automated Worktree Script

Create this script at `.claude/scripts/worktree-setup.sh`:

```bash
#!/bin/bash
# Usage: ./.claude/scripts/worktree-setup.sh feature-name

FEATURE=$1
if [ -z "$FEATURE" ]; then
    echo "Usage: $0 feature-name"
    exit 1
fi

# Create worktree
echo "Creating worktree for $FEATURE..."
git worktree add worktrees/$FEATURE feature/$FEATURE || exit 1

# Setup environment
cd worktrees/$FEATURE
echo "Setting up Python environment..."
python -m venv .venv
source .venv/bin/activate
pip install -e . || exit 1

# Create marker file
echo "$FEATURE" > .worktree-name

echo "✓ Worktree ready at: $PWD"
echo "✓ Environment activated"
echo "Run 'make test' to verify setup"
```

## Cleanup Script

Create `.claude/scripts/worktree-cleanup.sh`:

```bash
#!/bin/bash
# Usage: ./.claude/scripts/worktree-cleanup.sh feature-name

FEATURE=$1
if [ -z "$FEATURE" ]; then
    echo "Usage: $0 feature-name"
    exit 1
fi

# Remove worktree
echo "Removing worktree $FEATURE..."
git worktree remove worktrees/$FEATURE --force

# Remove plan if exists
if [ -f ".claude/worktree-plans/feature-$FEATURE.md" ]; then
    git rm .claude/worktree-plans/feature-$FEATURE.md
    git commit -m "chore: remove worktree plan for $FEATURE"
fi

echo "✓ Cleanup complete"
```

## Quick Reference for Claude Code

### Start of Session Checklist
```bash
# 1. Check current location
pwd

# 2. If in worktree, activate environment
[ -d .venv ] && source .venv/bin/activate

# 3. Verify setup
make test
```

### Common Commands
- **New worktree**: `./.claude/scripts/worktree-setup.sh feature-name`
- **Activate env**: `source .venv/bin/activate`
- **Build**: `make dev`
- **Test**: `make test`
- **Cleanup**: `./.claude/scripts/worktree-cleanup.sh feature-name`

### Environment Variables for Claude Code
```bash
# Add to session if needed
export CLAUDE_WORKTREE=1  # Marker for worktree context
export PYTHONPATH="$PWD:$PYTHONPATH"  # Ensure local code is found
```

## Advantages of This Approach

1. **Faster Setup**: venv creation is much faster than mamba clone
2. **No Shell Integration Issues**: venv works reliably without shell config
3. **Self-Contained**: Everything lives within the worktree directory
4. **Simple Cleanup**: Just remove the worktree directory
5. **Claude Code Friendly**: Copy-paste commands work reliably

## Fallback for Complex Dependencies

If the project has complex compiled dependencies that require mamba:

```bash
# Use mamba but with explicit paths
/opt/homebrew/bin/mamba create -n temp-$FEATURE python=3.11 -y
/opt/homebrew/bin/mamba run -n temp-$FEATURE pip install -e .
```

This guide prioritises reducing friction for Claude Code while maintaining proper isolation between worktrees.