# How-To Guides

This directory contains step-by-step guides for common development tasks in SUEWS.

## Available Guides

### setup-worktree.md
Complete guide for setting up git worktrees for feature development, including:
- Quick start with uv (recommended)
- Environment setup
- Cleanup procedures
- Alternative approaches

### setup-environment.md
Comprehensive guide for Python environment management, covering:
- uv (ultra-fast, recommended)
- Standard Python venv
- mamba/conda environments
- Compiler configuration
- Troubleshooting

### parallel-development.md
Guide for running multiple Claude Code agents simultaneously:
- When to use parallel development
- Environment isolation requirements
- Agent launch procedures
- Resource management

### worktree-workflow.md
Complete workflow guide for the `/worktree` slash command:
- Interactive worktree creation with lead developer tracking
- Synchronizing with master
- Creating pull requests
- Completing or abandoning features
- Best practices and troubleshooting

## Quick Reference

**Need to set up a new feature?**
→ See `worktree-workflow.md` (using `/worktree` command)
→ See `setup-worktree.md` (manual setup)

**Environment issues?**
→ See `setup-environment.md`

**Want to run multiple agents?**
→ See `parallel-development.md`

**Using the /worktree command?**
→ See `worktree-workflow.md`

## Best Practices

1. Always use isolated environments for each worktree
2. Follow the recommended uv workflow for speed
3. Clean up worktrees and environments when done
4. Document your setup if it deviates from these guides