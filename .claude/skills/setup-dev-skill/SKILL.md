---
name: setup-dev-skill
description: Set up SUEWS development environment with uv, venv, or mamba.
---

# Setup Dev

Set up SUEWS development environment.

## Quick Start (Recommended: uv)

```bash
uv venv && source .venv/bin/activate && make dev && make test
```

## Setup Options

| Option | Commands |
|--------|----------|
| **uv** (fastest) | `uv venv && source .venv/bin/activate && make dev` |
| **venv** | `python -m venv .venv && source .venv/bin/activate && make dev` |
| **mamba** | `mamba activate suews-dev && make dev` |
| **Makefile** | `make setup && source .venv/bin/activate && make dev` |

Details: `references/setup-options.md`

## Compiler Requirements

```bash
# macOS
brew install gcc

# Linux
sudo apt-get install gfortran  # or: sudo dnf install gcc-gfortran

# Verify
which gfortran && gfortran --version
```

## Key Commands

```bash
make setup     # Create .venv with uv
make dev       # Install editable mode
make test      # Run tests
make clean     # Smart clean
```

## Common Workflows

```bash
# Fresh start
make clean && make dev

# Update and rebuild
git pull && make dev

# Build and test
make dev && make test
```

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Import errors | `source .venv/bin/activate && make clean && make dev` |
| Package conflicts | `uv cache clean && rm -rf .venv && make setup` |
| Compiler issues | macOS: `xcode-select --install` |
| Python version | `uv venv --python 3.12` |

## Best Practices

1. **One environment per worktree**
2. **Use uv when available**
3. **Use Makefile recipes**
4. **Test after setup**: `make test`

## References

- `references/setup-options.md` - Detailed setup instructions
- `references/troubleshooting.md` - Common issues
