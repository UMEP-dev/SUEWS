# Environment Setup Guide

This guide covers different approaches for setting up Python environments for SUEWS development, with a focus on handling compiled dependencies.

## Quick Decision Tree

```
Need fastest setup? → Use uv
Need specific system packages? → Use mamba
Need simple, standard approach? → Use venv
Working in CI/CD? → Use uv
```

## Option 1: uv (Recommended - Ultra Fast)

### Installation

```bash
# macOS/Linux
brew install uv
# or
curl -LsSf https://astral.sh/uv/install.sh | sh

# Verify installation
uv --version
```

### Basic Setup

```bash
# Create virtual environment
uv venv

# Activate (optional - can use uv run instead)
source .venv/bin/activate

# Install packages (10-100x faster than pip!)
uv pip install pandas scipy matplotlib
```

### Working with uv

```bash
# No activation needed!
uv run python script.py
uv run pytest
uv run jupyter notebook

# Or activate traditionally
source .venv/bin/activate
python script.py  # Works as normal
```

### Installing from Requirements

Create a requirements file inline:

```bash
uv pip install -r <(cat << 'EOF'
# Build tools
pip>=22.0
setuptools>=65.0
wheel
meson-python>=0.17.0

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
EOF
)
```

## Option 2: Standard venv

### Basic Setup

```bash
# Create virtual environment
python -m venv .venv

# Activate
source .venv/bin/activate  # Linux/macOS
# or
.venv\Scripts\activate  # Windows

# Upgrade pip
pip install --upgrade pip setuptools wheel

# Install packages
pip install pandas scipy matplotlib  # etc.
```

### Deactivate

```bash
deactivate
```

## Option 3: mamba/conda

### When to Use mamba

- Need specific Fortran compilers
- Complex scientific stack with C/Fortran dependencies
- Working with HPC systems
- Need environment reproducibility across platforms

### Setup with mamba

```bash
# Create new environment
mamba create -n suews-dev-feature python=3.12 -y

# Activate
mamba activate suews-dev-feature

# Install from env.yml
mamba env update -n suews-dev-feature -f env.yml

# Or clone existing environment
mamba create -n suews-dev-feature --clone suews-dev
```

## Compiler Configuration

SUEWS requires Fortran compilers. Here's how to ensure they're available:

### macOS

```bash
# Check if gfortran is available
which gfortran || echo "Not found"

# Install if needed
brew install gcc

# Set compiler environment variables
export FC=gfortran
export CC=gcc
export F90=gfortran
export F77=gfortran
```

### Linux

```bash
# Ubuntu/Debian
sudo apt-get install gfortran

# Fedora/RHEL
sudo dnf install gcc-gfortran
```

### With mamba

```bash
# Compilers included in environment
mamba install gfortran_osx-64  # macOS
mamba install gfortran_linux-64  # Linux
```

## Environment Detection Script

Add this to your `.bashrc` or `.zshrc` for automatic environment detection:

```bash
# Auto-activate Python environments
auto_activate_env() {
    if [[ -f .venv/bin/activate ]]; then
        source .venv/bin/activate
        echo "Activated: $(basename $VIRTUAL_ENV)"
    elif [[ -f environment.yml ]] && command -v mamba &> /dev/null; then
        env_name=$(grep "name:" environment.yml | cut -d' ' -f2)
        mamba activate $env_name 2>/dev/null && echo "Activated: $env_name"
    fi
}

# Run when entering directories
cd() {
    builtin cd "$@" && auto_activate_env
}
```

## Performance Comparison

| Tool | Create Env | Install 50 packages | Pros | Cons |
|------|------------|-------------------|------|------|
| uv | 1s | 5-10s | Blazing fast, modern | Newer tool |
| venv+pip | 3s | 60-120s | Standard, everywhere | Slower |
| mamba | 30s | 30-60s | Scientific packages | Heavy, complex |

## Troubleshooting

### "No module named '_bz2'"
```bash
# Ubuntu/Debian
sudo apt-get install libbz2-dev
# Then rebuild Python or use mamba
```

### Fortran module import errors
```bash
# Ensure Fortran compiler available during install
FC=gfortran pip install -e .
```

### SSL Certificate errors with pip
```bash
# Use trusted host
pip install --trusted-host pypi.org --trusted-host files.pythonhosted.org package_name

# Or upgrade certificates
pip install --upgrade certifi
```

## Best Practices

1. **One environment per worktree** - Avoid conflicts
2. **Use requirements files** - Track dependencies
3. **Document special needs** - Note any system dependencies
4. **Regular updates** - Keep pip/uv updated
5. **Clean up** - Remove unused environments

## Quick Reference

```bash
# uv commands
uv venv                    # Create environment
uv pip install package     # Install package
uv run python script.py    # Run without activation

# venv commands  
python -m venv .venv       # Create environment
source .venv/bin/activate  # Activate
pip install package        # Install package
deactivate                 # Deactivate

# mamba commands
mamba create -n name       # Create environment
mamba activate name        # Activate
mamba install package      # Install package
mamba deactivate          # Deactivate
```