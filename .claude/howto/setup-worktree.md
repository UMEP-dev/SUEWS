# Worktree Setup Guide

This guide provides all the commands and workflows for setting up and managing git worktrees for SUEWS development.

## Quick Start with uv (Recommended)

The fastest way to set up a new worktree with all dependencies:

```bash
# Set your feature name
FEATURE="my-feature"

# Create the worktree
git worktree add worktrees/$FEATURE feature/$FEATURE || exit 1

# Navigate to the worktree
cd worktrees/$FEATURE

# Create virtual environment with uv (ultra-fast!)
uv venv
source .venv/bin/activate

# Install core requirements
# Package list maintained in .claude/reference/core-requirements.txt
# IMPORTANT: Use 'matplotlib' not 'matplotlib-base', 'tables' not 'pytables'
uv pip install pandas scipy matplotlib matplotlib-inline scikit-learn scikit-image \
    geopandas rtree openpyxl tables psutil salem==0.3.8 floweaver==2.0.0 \
    f90nml click pydantic ipykernel jupyter_client jupyter_core \
    pytest pytest-cov ruff f90wrap==0.2.16 atmosp "meson-python>=0.17.0"

# Install SUEWS in development mode
make dev  # This will auto-detect uv and use it!

# Create a marker file
echo "$FEATURE" > .worktree-name

# Test your setup
python -c "import supy; print(f'✓ SuPy {supy.__version__} ready')"
```

## Working Without Environment Activation

**Note**: Currently requires environment activation due to Python 3.13 compatibility.
See `.claude/reference/uv-adoption.md` for details on UV capabilities and current limitations.

## Cleanup

When you're done with a feature:

```bash
# From the root directory
FEATURE="my-feature"

# Remove the worktree
git worktree remove worktrees/$FEATURE --force

# Remove the worktree plan (if exists)
if [ -f ".claude/worktree-plans/feature-$FEATURE.md" ]; then
    git rm .claude/worktree-plans/feature-$FEATURE.md
    git commit -m "chore: remove worktree plan for $FEATURE"
fi

# List remaining worktrees
git worktree list
```

## Alternative: Standard Python venv

If uv is not available, use Python's built-in venv:

```bash
FEATURE="my-feature"

# Create worktree
git worktree add worktrees/$FEATURE feature/$FEATURE
cd worktrees/$FEATURE

# Create and activate venv
python -m venv .venv
source .venv/bin/activate

# Install dependencies (slower than uv)
pip install --upgrade pip setuptools wheel
# Package list maintained in .claude/reference/core-requirements.txt
pip install pandas scipy matplotlib matplotlib-inline scikit-learn scikit-image \
    geopandas rtree openpyxl tables psutil salem==0.3.8 floweaver==2.0.0 \
    f90nml click pydantic ipykernel jupyter_client jupyter_core \
    pytest pytest-cov ruff f90wrap==0.2.16 atmosp "meson-python>=0.17.0"

# Install SUEWS
make dev
```

## Alternative: Using mamba environment

For complex dependencies or when system packages are needed:

```bash
FEATURE="my-feature"

# Create worktree
git worktree add worktrees/$FEATURE feature/$FEATURE

# Create dedicated mamba environment
mamba create -n suews-dev-$FEATURE --clone suews-dev
mamba activate suews-dev-$FEATURE

# Navigate and build
cd worktrees/$FEATURE
make dev
```

## Quick Activation Script

Create this in any worktree for easy activation:

```bash
# Create activate.sh in your worktree
cat > activate.sh << 'EOF'
#!/bin/bash
source .venv/bin/activate
echo "Activated environment for: $(cat .worktree-name 2>/dev/null || echo 'unknown')"
echo "Branch: $(git branch --show-current)"
EOF
chmod +x activate.sh

# Use it
./activate.sh
```

## Core Requirements

The essential Python packages for SUEWS development are listed below.
**Important**: Package names differ between conda and pip:
- conda: `matplotlib-base` → pip: `matplotlib`
- conda: `pytables` → pip: `tables`

See the package installation commands in the Quick Start section above for the complete list.

### Full Package Installation Commands

For a complete development environment with uv:

**Note:** Always check `env.yml` in the repository root for the most up-to-date package versions and any new dependencies.

```bash
# Core packages (always needed)
# Package list maintained in .claude/reference/core-requirements.txt
uv pip install pandas scipy matplotlib matplotlib-inline scikit-learn scikit-image \
    geopandas rtree openpyxl tables psutil salem==0.3.8 floweaver==2.0.0 \
    f90nml click pydantic ipykernel jupyter_client jupyter_core \
    pytest pytest-cov ruff f90wrap==0.2.16 atmosp "meson-python>=0.17.0"

# Documentation packages (optional - only if working on docs)
uv pip install "sphinx>=4.0,<8.2" sphinx-autobuild pybtex nbsphinx recommonmark \
    "docutils>=0.16,<0.17" "jinja2>=3.0,<3.1" urlpath sphinxcontrib_programoutput \
    sphinx-jsonschema "sphinxcontrib.bibtex~=2.4" sphinx_comments \
    "sphinx-rtd-theme>=0.5" sphinx-book-theme sphinx-panels sphinxcontrib.email \
    sphinx-last-updated-by-git sphinx-click jsonschema2rst
```

## Working with Plans in Worktrees

Plans are stored in the master branch at `.claude/plans/`, not in your worktree. This ensures all worktrees share the same plan.

### Recommended Workflow: Launch Claude Code from Master

The smoothest way to work with worktrees and plans:

```bash
# Always start Claude Code from the main repo (master branch)
cd ~/Dropbox\ \(Personal\)/6.Repos/SUEWS
claude .
```

Then work primarily from master:
- Claude Code can access all files from the master location
- Edit worktree files using full paths: `worktrees/my-feature/src/file.py`
- Update plans directly: `.claude/plans/doing/feature-my-feature.md`
- When specific work is needed, temporarily cd into the worktree:
  ```bash
  cd worktrees/my-feature  # For focused operations
  make test               # Run tests
  git commit             # Commit changes
  cd ../..               # Return to master
  ```

This approach gives Claude Code full visibility while allowing focused work when needed.

### Alternative: If Already in a Worktree

If you've already launched Claude Code from within a worktree:

**Reading plans:**
```bash
cat ../../.claude/plans/doing/feature-my-feature.md
```

**Updating plans:**
Requires switching to master - see CLAUDE.md for detailed steps. This is why launching from master is recommended.

**Getting plan updates:**
```bash
git fetch origin master  # Gets latest plans from master
```

## Tips

1. **Always use worktrees** under the `worktrees/` directory
2. **Name worktrees** to match their feature branch
3. **Use uv when available** - it's 10-100x faster
4. **Check compiler** - macOS provides gfortran, or use `brew install gcc`
5. **Run tests** after setup with `make test`
6. **Clean up** both worktree and plan file when done
7. **Plans live in master** - remember to commit plan updates to master branch