# Adopting uv Throughout SUEWS

## Benefits of Full uv Adoption

1. **Single tool** for all Python needs
2. **10-100x faster** than pip/conda
3. **Built-in workspace support** - perfect for worktrees
4. **Lock files** - reproducible environments
5. **No more mamba/conda complexity**

## Quick Worktree Setup with uv

Here's the complete workflow for setting up a new worktree:

```bash
# One-time: Install uv
brew install uv  # or: curl -LsSf https://astral.sh/uv/install.sh | sh

# Create and setup worktree
FEATURE="my-feature"
git worktree add worktrees/$FEATURE feature/$FEATURE
cd worktrees/$FEATURE

# Setup environment (seconds, not minutes!)
uv venv
source .venv/bin/activate  # Optional - can use uv run instead

# Install all requirements
uv pip install pandas scipy matplotlib-base matplotlib-inline scikit-learn scikit-image \
    geopandas rtree openpyxl pytables psutil salem==0.3.8 floweaver==2.0.0 \
    f90nml click pydantic ipykernel jupyter_client jupyter_core \
    pytest pytest-cov ruff f90wrap==0.2.16 atmosp meson-python>=0.17.0

# Install SUEWS in development mode
make dev  # Auto-detects uv and uses it!

# Ready to work!
uv run python         # No activation needed
uv run pytest        # Run tests
uv run make test     # Run full test suite
```

## Cleanup Script

```bash
# Clean up when done
FEATURE="my-feature"

# Remove worktree
git worktree remove worktrees/$FEATURE --force

# Remove plan if exists
if [ -f ".claude/worktree-plans/feature-$FEATURE.md" ]; then
    git rm .claude/worktree-plans/feature-$FEATURE.md
    git commit -m "chore: remove worktree plan for $FEATURE"
fi
```

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