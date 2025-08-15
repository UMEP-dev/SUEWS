# How-To Guides

This directory contains step-by-step guides for common development tasks in SUEWS.

## Available Guides

### setup-worktree.md
Complete guide for setting up git worktrees for feature development, including:
- Quick start with `make uv-dev` (fastest, recommended)
- Manual uv setup alternative
- Environment setup options
- Cleanup procedures
- Alternative approaches

### setup-environment.md  
Comprehensive guide for Python environment management, covering:
- uv with `make uv-dev` recipe (ultra-fast, recommended)
- Standard Python venv with `make dev`
- mamba/conda environments
- Compiler configuration
- Troubleshooting

### parallel-development.md
Guide for running multiple Claude Code agents simultaneously:
- When to use parallel development
- Environment isolation requirements
- Agent launch procedures
- Resource management

## Quick Reference

**Need to set up a new feature?**
→ See `setup-worktree.md`

**Environment issues?**
→ See `setup-environment.md`

**Want to run multiple agents?**
→ See `parallel-development.md`

## Best Practices

1. Always use isolated environments for each worktree
2. Use `make uv-dev` for the fastest setup experience
3. Clean up worktrees and environments when done (`make uv-clean`)
4. Document your setup if it deviates from these guides