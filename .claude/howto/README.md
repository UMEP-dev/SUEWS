# How-To Guides

Step-by-step guides for common SUEWS development tasks.

## Core Guides

### worktree-workflow.md
**Complete worktree workflow from setup to PR**
- Quick setup with git worktrees
- Using the `/worktree` command for streamlined workflow
- Manual setup alternatives
- Plan management and cleanup procedures
- Troubleshooting common issues

### setup-environment.md  
**Python environment setup for all scenarios**
- Quick start with uv (ultra-fast, recommended)
- Standard Python venv alternative
- Mamba for complex dependencies
- Compiler configuration
- Package name differences (mamba vs pip)
- Troubleshooting guide


## Quick Decision Tree

**"I need to set up a new feature"**
→ See `worktree-workflow.md`

**"I'm having environment/package issues"**
→ See `setup-environment.md`

## Best Practices Summary

1. **Use the simplified Makefile commands**:
   - `make setup` - Create virtual environment
   - `make dev` - Install SUEWS in editable mode
   - `make test` - Run tests
   - `make clean` - Smart clean

2. **One environment per worktree** - Never share `.venv` between worktrees

3. **Launch Claude Code from master** - Best visibility and plan access

4. **Use `/worktree` commands** - Streamlined workflow for feature development

5. **Keep issues updated** - Document progress in GitHub issues/PRs

## Common Command Reference

```bash
# Quick setup for new feature
git worktree add worktrees/my-feature feature/my-feature
cd worktrees/my-feature
make setup && source .venv/bin/activate && make dev

# Using /worktree command
/worktree new     # Start new feature
/worktree sync    # Sync with master
/worktree pr      # Create pull request
/worktree finish  # Complete feature

# Environment management
make setup        # Create .venv with uv
make dev          # Install SUEWS
make test         # Run tests
make clean        # Clean build artifacts
```

## Documentation Maintenance

These guides follow the DRY (Don't Repeat Yourself) principle:
- Each piece of information exists in ONE place
- Guides reference each other instead of duplicating content
- Updates should maintain this single source of truth