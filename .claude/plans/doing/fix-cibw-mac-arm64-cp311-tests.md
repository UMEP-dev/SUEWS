# Fix: CIBuildWheel macOS ARM64 Python 3.11 Test Failures

## Context
Two tests are failing on macOS ARM64 in cibuildwheel but pass when run individually. While test isolation fixes were applied, we need to investigate if there's an underlying Fortran issue specific to ARM64 architecture that the tests are exposing.

## GitHub Issues
- Related to CI/CD build failures on ARM64
- CIBuildWheel test failures specific to macOS ARM64 + Python 3.11

## Progress Tracking

### Investigation Phase
- [x] Review debug log and understand test isolation issue
- [x] Note exact compiler versions from failed CI (Apple clang 15.0.0, gfortran 15.1.0)
- [x] Reproduce exact cibuildwheel environment (Python 3.11.9, ARM64)
- [x] Remove test isolation fixes temporarily to expose original issue
- [x] Debug with original failing tests to understand root cause
- [x] Investigate potential Fortran numerical differences on ARM64

### Fortran Analysis
- [x] Check floating point handling differences on ARM64 - No Fortran issues found
- [x] Confirm issue is test isolation, not architecture-specific
- [x] Both tests pass individually on ARM64
- [x] Issue only manifests when running full test suite
- [x] Verified test isolation fix resolves the problem

### Root Cause Fix
- [x] Confirmed no Fortran routines causing issues
- [x] Test isolation fix (already in d5053de4) is the correct solution
- [x] No architecture-specific fixes needed
- [x] Documented findings in detailed investigation summary

### Validation
- [x] Run full test suite in exact cibuildwheel environment
- [x] Both problematic tests now pass in full suite
- [x] Test isolation fix is appropriate, not a workaround
- [x] Issue confirmed as test contamination, not ARM64-specific

## Key Decisions
- Focus on Fortran root cause rather than test workarounds
- Reproduce exact cibuildwheel environment for debugging
- Consider ARM64 floating-point and memory alignment differences
- Match exact compiler versions from CI

## Implementation Notes
- CIBuildWheel uses specific Python 3.11 on macOS ARM64
- **CI Compiler versions**:
  - Apple clang version 15.0.0 (clang-1500.3.9.4)
  - GNU Fortran (Homebrew GCC 15.1.0) 15.1.0
  - Target: arm64-apple-darwin23.6.0
- Water balance calculations show NaN propagation issues
- Test contamination might be exposing real numerical instability
- Performance warnings in site.py might be related

## Files to Investigate
- `src/suews/suews_phys_waterdist.f95` - Water distribution physics
- `src/suews/suews_phys_evap.f95` - Evaporation calculations
- `src/suews/suews_ctrl_driver.f95` - Main driver routines
- `test/test_supy.py` - Temporarily revert isolation fixes for debugging
- `src/supy/data_model/site.py` - Performance warnings location
- `pyproject.toml` / `meson.build` - Check build flags for ARM64

## Environment Setup Commands
```bash
# Check current compiler versions
which gfortran && gfortran --version
which clang && clang --version

# Exact Python 3.11 version used in cibuildwheel
python3.11 -m venv .venv-cibw
source .venv-cibw/bin/activate

# Install build dependencies exactly as cibuildwheel
pip install --upgrade pip setuptools wheel
pip install "cython>=3,<4" "numpy>=2.0,<3" meson-python ninja
pip install pandas scipy matplotlib scikit-learn tables click

# Build with same flags as cibuildwheel
ARCHFLAGS="-arch arm64" python -m pip install -e . -v

# Run tests to reproduce issue
pytest test/test_supy.py::TestSuPy::test_is_supy_running_multi_step -xvs
pytest test/test_supy.py::TestSuPy::test_water_balance_closed -xvs
pytest test -v --tb=short
```

## Current Status
- Investigation complete: Issue was test isolation, not Fortran/ARM64
- Test isolation fix in d5053de4 is the correct solution
- Both tests pass individually and in full suite with the fix
- No ARM64-specific Fortran issues found
- Ready to close the investigation