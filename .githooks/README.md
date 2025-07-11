# Git Hooks for SUEWS

This directory contains git hooks to enforce development policies.

## Master Branch Protection

The `pre-commit` hook prevents committing non-documentation files directly to the master branch.

### Allowed files on master:
- Markdown files (`*.md`)
- YAML configuration files (`*.yml`, `*.yaml`)
- GitHub workflow files (`.github/workflows/*`)
- Git hook files (`.githooks/*`)

### Setup

To enable these hooks locally, run:

```bash
git config core.hooksPath .githooks
```

Or use the global git hooks directory:

```bash
cp .githooks/* .git/hooks/
```

### Bypass (Emergency Only)

If you absolutely need to commit other files to master (not recommended):

```bash
git commit --no-verify -m "your message"
```

**Note**: The GitHub Actions workflow will still check file types on push, so bypassing locally won't help with remote pushes.