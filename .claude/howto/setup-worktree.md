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

# Install core requirements (matches env.yml)
uv pip install pandas scipy matplotlib-base matplotlib-inline scikit-learn scikit-image \
    geopandas rtree openpyxl pytables psutil salem==0.3.8 floweaver==2.0.0 \
    f90nml click pydantic ipykernel jupyter_client jupyter_core \
    pytest pytest-cov ruff f90wrap==0.2.16 atmosp meson-python>=0.17.0

# Install SUEWS in development mode
make dev  # This will auto-detect uv and use it!

# Create a marker file
echo "$FEATURE" > .worktree-name

# Test your setup
python -c "import supy; print(f'✓ SuPy {supy.__version__} ready')"
```

## Working Without Environment Activation

With uv, you don't need to activate environments:

```bash
cd worktrees/my-feature

# Run commands directly with uv
uv run python script.py
uv run pytest
uv run make test
```

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
pip install pandas scipy matplotlib # ... (same list as above)

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

These are the essential Python packages for SUEWS development. The complete package list is maintained in `env.yml` at the repository root. This section provides the pip-installable versions for use with uv or standard Python environments:

```
# Build tools
pip>=22.0
setuptools>=65.0
wheel
meson-python>=0.17.0
doxygen  # For documentation generation

# Core data science
pandas
scipy
matplotlib-base
matplotlib-inline
scikit-learn
scikit-image

# Geospatial and data handling
geopandas
rtree
openpyxl
pytables
psutil
salem==0.3.8
floweaver==2.0.0

# Configuration and CLI
f90nml
click
pydantic

# Jupyter support
ipykernel
jupyter_client
jupyter_core

# Testing and code quality
pytest
pytest-cov
ruff

# Fortran wrapper and atmospheric science
f90wrap==0.2.16
atmosp

# Documentation system (optional for worktree development)
# Only install if working on documentation:
# sphinx>=4.0,<8.2
# sphinx-autobuild
# pybtex
# nbsphinx
# recommonmark
# docutils>=0.16,<0.17
# jinja2>=3.0,<3.1
# urlpath
# sphinxcontrib_programoutput
# sphinx-jsonschema
# sphinxcontrib.bibtex~=2.4
# sphinx_comments
# sphinx-rtd-theme>=0.5
# sphinx-book-theme
# sphinx-panels
# sphinxcontrib.email
# sphinx-last-updated-by-git
# sphinx-click
# jsonschema2rst
```

### Full Package Installation Commands

For a complete development environment with uv:

**Note:** Always check `env.yml` in the repository root for the most up-to-date package versions and any new dependencies.

```bash
# Core packages (always needed)
uv pip install pandas scipy matplotlib-base matplotlib-inline scikit-learn scikit-image \
    geopandas rtree openpyxl pytables psutil salem==0.3.8 floweaver==2.0.0 \
    f90nml click pydantic ipykernel jupyter_client jupyter_core \
    pytest pytest-cov ruff f90wrap==0.2.16 atmosp meson-python>=0.17.0

# Documentation packages (optional - only if working on docs)
uv pip install "sphinx>=4.0,<8.2" sphinx-autobuild pybtex nbsphinx recommonmark \
    "docutils>=0.16,<0.17" "jinja2>=3.0,<3.1" urlpath sphinxcontrib_programoutput \
    sphinx-jsonschema "sphinxcontrib.bibtex~=2.4" sphinx_comments \
    "sphinx-rtd-theme>=0.5" sphinx-book-theme sphinx-panels sphinxcontrib.email \
    sphinx-last-updated-by-git sphinx-click jsonschema2rst
```

## Tips

1. **Always use worktrees** under the `worktrees/` directory
2. **Name worktrees** to match their feature branch
3. **Use uv when available** - it's 10-100x faster
4. **Check compiler** - macOS provides gfortran, or use `brew install gcc`
5. **Run tests** after setup with `make test`
6. **Clean up** both worktree and plan file when done