# Setting Up uv Environment for SUEWS Development

This guide provides the best practices for setting up a uv environment for SUEWS development, properly aligned with the project's `pyproject.toml` and `env.yml` files.

## Why uv?

- **10-100x faster** than pip/conda for package installation
- **Single tool** for all Python needs
- **Built-in lock file support** for reproducible environments
- **Automatic dependency resolution** without conda complexity

## Installation

First, install uv if you haven't already:

```bash
# macOS
brew install uv

# Or via installer script
curl -LsSf https://astral.sh/uv/install.sh | sh
```

## Best Practice Setup Method

### Recommended: Using the Makefile Recipe

The easiest way is to use the provided Makefile recipe which handles everything:

```bash
# One command to set up everything
make uv-dev

# Then activate the environment
source .venv/bin/activate  # Required due to Python 3.13 compatibility

# Verify installation
python -c "import supy; print(f'✓ SuPy {supy.__version__} ready')"
```

This recipe:
- Creates a uv virtual environment if it doesn't exist
- Installs SUEWS with development + documentation dependencies
- Uses the project's `pyproject.toml` configuration

### Alternative: Manual Setup with pyproject.toml

If you prefer manual control:

```bash
# 1. Create and activate virtual environment
uv venv
source .venv/bin/activate

# 2. Install SUEWS with development + documentation dependencies
uv pip install -e ".[dev,docs]"

# Or install everything including all optional dependencies
# uv pip install -e ".[all]"

# 3. Verify installation
python -c "import supy; print(f'✓ SuPy {supy.__version__} ready')"
```

### Method 2: Direct Command (If pyproject.toml method fails)

For a quick setup without creating files:

```bash
# Create and activate environment
uv venv
source .venv/bin/activate

# Install all dependencies in one command (aligned with env.yml)
uv pip install pandas scipy matplotlib matplotlib-inline scikit-learn scikit-image \
    geopandas rtree openpyxl tables psutil salem==0.3.8 floweaver==2.0.0 \
    f90nml click pydantic ipykernel jupyter_client jupyter_core \
    pytest pytest-cov ruff f90wrap==0.2.16 atmosp "meson-python>=0.17.0" \
    chardet dask seaborn cdsapi xarray multiprocess lmfit numdifftools \
    pvlib platypus-opt==1.0.4 timezonefinder==6.5.9 pytz pyarrow

# Install SUEWS in development mode
uv pip install -e . --no-deps
```

## Working with Git Worktrees

For parallel development using git worktrees:

```bash
# Set your feature name
FEATURE="my-feature"

# Create the worktree
git worktree add worktrees/$FEATURE feature/$FEATURE

# Navigate to the worktree
cd worktrees/$FEATURE

# Set up uv environment using the Makefile recipe (recommended)
make uv-dev
source .venv/bin/activate

# Or manually if you prefer:
# uv venv
# source .venv/bin/activate
# uv pip install -e ".[dev,docs]"

# Test your setup
make test
```

## Important Notes

### Package Name Differences

When migrating from conda/mamba, note these package name differences:
- conda: `matplotlib-base` → pip/uv: `matplotlib`
- conda: `pytables` → pip/uv: `tables`

### Python 3.13 Compatibility

Currently, you need to activate the environment (`source .venv/bin/activate`) rather than using `uv run` due to Python 3.13 compatibility. This is temporary and will be resolved as packages add Python 3.13 support.

### Build System Integration

The Makefile provides convenient recipes for uv:
- `make uv-dev` - One-stop setup of uv environment with all dependencies
- `make uv-clean` - Clean the uv virtual environment
- `make dev` - Builds SUEWS using uv if available (auto-detects environment)
- `make test` - Runs tests in the active environment

## Verification

After setup, verify your environment:

```bash
# Check SuPy is installed
python -c "import supy; print(f'SuPy version: {supy.__version__}')"

# Check key dependencies
python -c "import pandas, scipy, matplotlib, pydantic; print('Core packages: OK')"

# Run tests
make test

# Or with pytest directly
pytest test -v --tb=short
```

## Advantages Over conda/mamba

1. **Speed**: Package installation takes seconds, not minutes
2. **Simplicity**: No channel management or environment solving issues
3. **Reproducibility**: Built-in lock file support
4. **Python-focused**: Designed specifically for Python projects
5. **Modern resolver**: Better dependency conflict resolution

## Troubleshooting

If you encounter issues:

1. **Check Python version**: uv defaults to Python 3.13
   ```bash
   python --version
   ```

2. **Clear cache if needed**:
   ```bash
   uv cache clean
   ```

3. **For specific Python version**:
   ```bash
   uv venv --python 3.12
   ```

4. **If packages fail to install**, check for Python 3.13 compatibility and consider using Python 3.12:
   ```bash
   uv venv --python 3.12
   source .venv/bin/activate
   uv pip install -e ".[dev]"
   ```

## Summary

The recommended approach for SUEWS development with uv:

1. Use `make uv-dev` for the simplest setup experience
2. Alternatively, use `uv pip install -e ".[dev,docs]"` for manual control
3. Always activate the environment with `source .venv/bin/activate`
4. The setup takes seconds compared to minutes with conda/mamba
5. All dependencies are properly aligned with `pyproject.toml` and `env.yml`
6. Use `make uv-clean` to remove the environment when needed