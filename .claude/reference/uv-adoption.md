# Adopting uv Throughout SUEWS

## Benefits of Full uv Adoption

1. **Single tool** for all Python needs
2. **10-100x faster** than pip/conda
3. **Built-in workspace support** - perfect for worktrees
4. **Lock files** - reproducible environments
5. **No more mamba/conda complexity**

## Current Status & Python 3.13 Compatibility

**Important**: UV defaults to the latest Python (3.13), which has some compatibility considerations:

1. **Package Availability**: Some packages don't yet have Python 3.13 wheels
2. **UV Handling**: UV automatically finds compatible versions (e.g., scipy 1.16.0 instead of 1.13.1)
3. **`uv run` Limitation**: Currently requires environment activation to avoid dependency rebuilds
4. **Temporary Workaround**: Use `source .venv/bin/activate` instead of `uv run` for now

This is a temporary limitation that will resolve as packages add Python 3.13 support.

## Quick Worktree Setup with uv

For complete worktree setup instructions, see `.claude/howto/setup-worktree.md`.

Key points for UV usage:
- Install: `brew install uv` or `curl -LsSf https://astral.sh/uv/install.sh | sh`
- Currently requires environment activation due to Python 3.13
- Package names: Use `matplotlib` (not `matplotlib-base`), `tables` (not `pytables`)

## Migration Path

### Step 1: Replace env.yml with pyproject.toml

```bash
# Current approach
mamba env create -f env.yml

# With uv
uv sync  # That's it!
```

### Step 2: Update Makefile (Already Done!)

The Makefile now auto-detects uv:

```makefile
# make dev - automatically uses uv if available
# make test - can use UV_RUN=1 make test for uv run
```

### Step 3: CI/CD Benefits

```yaml
# GitHub Actions with uv
- uses: astral-sh/setup-uv@v2
- run: uv sync
- run: uv run pytest
```

## Key Commands

### Development
```bash
# Install all dependencies
uv sync

# Add a dependency
uv add numpy

# Add dev dependency
uv add --dev pytest

# Run commands
uv run python script.py
uv run pytest
uv run ruff check
```

### Worktrees
```bash
# Each worktree just needs
cd worktrees/feature
uv sync
# Done! No env activation needed
```

## Advantages Over Current Setup

1. **No environment activation** - `uv run` handles it
2. **Faster CI** - uv caches aggressively
3. **Better dependency resolution** - modern resolver
4. **Simpler commands** - less to remember
5. **Cross-platform** - same commands everywhere

## Minimal Adoption (If Full Migration Too Big)

At minimum, use uv for worktrees:

```bash
# In CLAUDE.md
"For worktrees, just run:
cd worktrees/feature
uv venv && source .venv/bin/activate
uv pip install -r requirements.txt
uv pip install -e ."
```

This gives speed benefits without changing the whole project.