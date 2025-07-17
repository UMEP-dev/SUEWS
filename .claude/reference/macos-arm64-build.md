# macOS ARM64 (Apple Silicon) Build Guide

This guide addresses common build issues when developing SUEWS on macOS ARM64 (M1/M2/M3) systems.

## Common Issues and Solutions

### 1. Architecture-Specific Tool Errors

**Problem**: Build fails with errors like:
- `arm64-apple-darwin20.0.0-ar: command not found`
- `arm64-apple-darwin20.0.0-ranlib: command not found`
- `Unknown linker(s): [['arm64-apple-darwin20.0.0-ar']]`

**Cause**: The build system (meson) is looking for cross-compilation tools from conda/mamba environments that don't exist on ARM64 native builds.

**Solution**: Use system tools and Homebrew's native gfortran:

```bash
# Set environment variables to use system tools
export AR=/usr/bin/ar
export RANLIB=/usr/bin/ranlib
export FC=/opt/homebrew/bin/gfortran
export CC=clang

# Install with pip (not uv for this specific case)
pip install --no-build-isolation --editable .
```

### 2. Python Version Compatibility

**Problem**: Water balance test fails with NaN values on Python 3.11+ (see issue #508)

**Testing Environment Setup**:
```bash
# Create Python 3.11 environment to reproduce/test the issue
uv venv --python 3.11
source .venv/bin/activate

# Verify version
python --version  # Should show Python 3.11.x
```

### 3. Complete Build Process for ARM64

Here's the complete, tested process for building SUEWS on macOS ARM64:

```bash
# 1. Ensure you have Homebrew's gcc installed
brew install gcc

# 2. Create and activate virtual environment
uv venv --python 3.11  # or your preferred Python version
source .venv/bin/activate

# 3. Install dependencies
uv pip install pandas scipy matplotlib scikit-learn scikit-image \
    geopandas rtree openpyxl tables psutil salem==0.3.8 floweaver==2.0.0 \
    f90nml click pydantic ipykernel jupyter_client jupyter_core \
    pytest pytest-cov ruff f90wrap==0.2.16 atmosp meson-python>=0.17.0 \
    pip>=22.0 setuptools>=65.0 wheel

# 4. Build with proper environment variables
export AR=/usr/bin/ar
export RANLIB=/usr/bin/ranlib
export FC=/opt/homebrew/bin/gfortran
export CC=clang

# Clean any previous build attempts
rm -rf build

# Install
pip install --no-build-isolation --editable .

# 5. Verify installation
python -c "import supy; print('supy version:', supy.__version__)"
```

### 4. Alternative: Using make dev

The Makefile is already configured to handle some of these issues:

```bash
# The Makefile sets FC=/opt/homebrew/bin/gfortran automatically
make clean
make dev
```

However, if this fails, use the manual pip install method above.

### 5. Debugging Build Issues

If the build fails, check:

1. **Compiler paths**:
   ```bash
   which gfortran  # Should be /opt/homebrew/bin/gfortran
   which ar        # Should be /usr/bin/ar
   which clang     # Should be /usr/bin/clang
   ```

2. **Environment isolation**:
   - Ensure you're not in a conda/mamba environment that might override tools
   - Use `env | grep -E '(CC|FC|AR|RANLIB)'` to check for conflicting variables

3. **Build logs**:
   - Check `build/meson-logs/meson-log.txt` for detailed error messages
   - Look for "command not found" errors indicating missing tools

## Known Issues

1. **uv and editable installs**: On ARM64, uv may cache failed builds. If you encounter persistent issues:
   ```bash
   pip uninstall -y supy
   rm -rf build
   # Then rebuild using the pip method above
   ```

2. **Conda/Mamba environments**: These often include cross-compilation tools that conflict with native ARM64 builds. Always use venv or uv-created virtual environments for SUEWS development on ARM64.

## Testing

After successful build:

```bash
# Run specific test
python -m pytest test/test_supy.py::TestSuPy::test_water_balance_closed -v

# Run all tests
make test
```

## References

- Issue #508: Water balance test failure on macOS ARM64 Python 3.11+
- Homebrew GCC: https://formulae.brew.sh/formula/gcc
- Meson Python: https://meson-python.readthedocs.io/