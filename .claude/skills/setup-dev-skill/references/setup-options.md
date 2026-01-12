# Setup Options - Detailed

## Why uv?

- **10-100x faster** than pip/mamba
- **Single tool** for all Python needs
- **Built-in lock file support**
- **Automatic dependency resolution**

| Tool | Create Env | Install 50 packages |
|------|------------|-------------------|
| uv | 1s | 5-10s |
| pip | 3s | 60-120s |
| mamba | 5s | 30-60s |

---

## Option 1: Using Makefile (Simplest)

```bash
make setup  # Creates .venv with uv (if available)
source .venv/bin/activate
make dev    # Installs SUEWS with all dependencies
make test   # Run tests to verify
```

---

## Option 2: Manual uv Setup

```bash
uv venv
source .venv/bin/activate

# Install with all development dependencies
uv pip install -e ".[dev,docs]"

# Or install specific packages manually
uv pip install pandas scipy matplotlib matplotlib-inline scikit-learn scikit-image \
    geopandas rtree openpyxl tables psutil salem==0.3.8 floweaver==2.0.0 \
    f90nml click pydantic ipykernel jupyter_client jupyter_core \
    pytest pytest-cov ruff f90wrap==0.2.16 atmosp "meson-python>=0.17.0"
```

---

## Option 3: Standard venv

```bash
python -m venv .venv
source .venv/bin/activate
make dev
make test
```

---

## Option 4: Mamba Environment

For complex dependencies or system packages:

```bash
mamba env create -f env.yml -n suews-dev
mamba activate suews-dev
make dev
```

---

## Compiler Requirements

SUEWS requires a Fortran compiler:

```bash
# macOS
brew install gcc  # Provides gfortran

# Linux
sudo apt-get install gfortran  # Ubuntu/Debian
sudo dnf install gcc-gfortran  # Fedora/RHEL

# Verify
which gfortran
gfortran --version
```

**macOS Note**: Makefile uses Homebrew's gfortran:
```bash
FC=/opt/homebrew/bin/gfortran make dev
```

---

## Package Name Differences

mamba vs pip/uv:
- `matplotlib-base` → `matplotlib`
- `pytables` → `tables`

---

## Python 3.13 Compatibility

Use `source .venv/bin/activate` rather than `uv run`.

For specific Python version:
```bash
uv venv --python 3.12
```

---

## Working with Git Worktrees

Each worktree needs its own environment:

```bash
# In new worktree
uv venv
source .venv/bin/activate
make dev
```
