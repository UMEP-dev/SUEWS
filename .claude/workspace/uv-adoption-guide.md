# Adopting uv Throughout SUEWS

## Benefits of Full uv Adoption

1. **Single tool** for all Python needs
2. **10-100x faster** than pip/conda
3. **Built-in workspace support** - perfect for worktrees
4. **Lock files** - reproducible environments
5. **No more mamba/conda complexity**

## Migration Path

### Step 1: Replace env.yml with pyproject.toml

```bash
# Current approach
mamba env create -f env.yml

# With uv
uv sync  # That's it!
```

### Step 2: Simplify Worktree Workflow

```bash
# Create worktree
git worktree add worktrees/feature feature/feature

# Setup with uv
cd worktrees/feature
uv sync
uv run pytest  # No activation needed!
```

### Step 3: Update Makefile

```makefile
# Before
dev:
	pip install -e .

# After
dev:
	uv sync
	uv pip install -e .

test:
	uv run pytest test -v
```

### Step 4: CI/CD Benefits

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