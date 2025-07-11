# Environment Isolation Guide for SUEWS Development

## Visual Overview

```
SUEWS Repository Structure + Environments
=========================================

/SUEWS/ (root)
├── Environment: suews-dev (BASE)
├── Purpose: Main development, testing, releases
├── Command: mamba activate suews-dev
│
├── worktrees/
│   ├── core-bugs/
│   │   ├── Environment: suews-dev-core-fixes ✓
│   │   ├── Purpose: Fix runtime bugs
│   │   └── Command: mamba activate suews-dev-core-fixes
│   │
│   ├── default-values/
│   │   ├── Environment: suews-dev-defaults ✓
│   │   ├── Purpose: Adjust default values
│   │   └── Command: mamba activate suews-dev-defaults
│   │
│   ├── rsl-fixes/
│   │   ├── Environment: suews-dev-rsl-fixes ✓
│   │   ├── Purpose: RSL physics fixes
│   │   └── Command: mamba activate suews-dev-rsl-fixes
│   │
│   └── [other worktrees...]
│       └── Environment: suews-dev-{feature} ✓
│
└── .claude/
    └── worktree-plans/
        └── (shared across all environments)
```

## Key Rules

### 🚫 NEVER DO THIS:
```bash
cd worktrees/any-feature
mamba activate suews-dev  # ❌ WRONG - This is the base environment!
make dev                  # ❌ This will conflict with root installation
```

### ✅ ALWAYS DO THIS:
```bash
cd worktrees/my-feature
mamba activate suews-dev-my-feature  # ✓ Correct - Feature-specific environment
make dev                             # ✓ Installs to isolated environment
```

## Complete Workflow Example

### Starting Fresh with a New Feature

1. **Create worktree and environment together:**
```bash
# From root directory
FEATURE="awesome-feature"

# Create worktree
git worktree add worktrees/$FEATURE feature/$FEATURE

# Create environment
mamba create -n suews-dev-$FEATURE --clone suews-dev -y

# Navigate and activate
cd worktrees/$FEATURE
mamba activate suews-dev-$FEATURE

# Build
make dev

# Verify installation
python -c "import supy; print(f'Loaded supy from: {supy.__file__}')"
```

2. **Resuming work later:**
```bash
# Always activate the matching environment
cd worktrees/awesome-feature
mamba activate suews-dev-awesome-feature
# Continue development...
```

## Environment Management Commands

### List all SUEWS environments:
```bash
mamba env list | grep suews-dev
```

Expected output:
```
suews-dev                 /Users/you/mambaforge/envs/suews-dev
suews-dev-core-fixes      /Users/you/mambaforge/envs/suews-dev-core-fixes
suews-dev-defaults        /Users/you/mambaforge/envs/suews-dev-defaults
suews-dev-rsl-fixes       /Users/you/mambaforge/envs/suews-dev-rsl-fixes
```

### Check current environment:
```bash
echo $CONDA_DEFAULT_ENV
# Should show: suews-dev-{feature} when in a worktree
```

### Clean up after feature is merged:
```bash
# Remove worktree
git worktree remove worktrees/my-feature

# Remove environment
mamba env remove -n suews-dev-my-feature
```

## Troubleshooting

### "Import supy" fails in worktree
- **Cause**: Wrong environment active
- **Fix**: `mamba activate suews-dev-{feature}` then `make dev`

### "Another git process seems to be running"
- **Cause**: Build conflict from shared environment
- **Fix**: Ensure each worktree has its own environment

### Changes in one worktree affect another
- **Cause**: Sharing the same environment
- **Fix**: Create separate environments for each worktree

## Quick Reference Card

| Location | Environment | Purpose |
|----------|------------|---------|
| `/SUEWS/` | `suews-dev` | Main development |
| `/worktrees/core-bugs/` | `suews-dev-core-fixes` | Bug fixes |
| `/worktrees/default-values/` | `suews-dev-defaults` | Default value changes |
| `/worktrees/rsl-fixes/` | `suews-dev-rsl-fixes` | RSL physics |
| `/worktrees/{name}/` | `suews-dev-{name}` | Feature development |

## Remember

- **One environment per worktree** - No exceptions!
- **Name environments to match worktrees** - Avoid confusion
- **Clean up both worktree AND environment** - When done
- **Never use base environment in worktrees** - It's for root only