# SUEWS Test Suite

This directory contains the test suite for SUEWS/SuPy, organised by functionality to improve maintainability and clarity.

## Test Organisation

### Core Tests (`core/`)
Essential core functionality tests including:
- **test_sample_output.py** - Tolerance-based validation (runs first in CI fast-fail)
- **test_fortran_state_persistence.py** - Ensures Fortran state isolation between runs
- **test_floating_point_stability.py** - Numerical stability and reproducibility tests
- **test_suews_simulation.py** - High-level API interface tests
- **test_supy.py** - Comprehensive test suite (runs during wheel building)
- **test_spurious_warnings.py** - Ensures clean imports without warnings

### Data Model Tests (`data_model/`)
Configuration and data model validation tests:
- **test_data_model.py** - Data model structure and conversion tests
- **test_precheck.py** - Pre-run validation and checks
- **test_conditional_validation.py** - Physics option compatibility validation
- **test_validation_topdown.py** - Top-down configuration validation
- **test_validation_utils.py** - Validation utility functions
- **test_flexible_refvalue_clean.py** - RefValue wrapper functionality

### Physics Tests (`physics/`)
Scientific and physics validation tests:
- **test_core_physics.py** - Physical consistency checks (runs during wheel building)

### I/O Tests (`io_tests/`)
Input/output and data handling tests:
- **test_output_config.py** - Output configuration options
- **test_save_supy.py** - Output saving functionality
- **test_resample_output.py** - Output resampling capabilities
- **test_dailystate_output.py** - Daily state output handling
- **test_forcing_file_list.py** - Forcing file list handling
- **test_yaml_annotation.py** - YAML annotation features

### Test Fixtures (`fixtures/`)
Test data and resources:
- **benchmark1/** - Benchmark test configuration and data
- **data_test/** - Sample data for various tests
- **precheck_testcase/** - Test cases for precheck functionality

## Running Tests

```bash
# Run all tests
pytest test/ -v

# Run tests by category
pytest test/core/ -v              # Core functionality
pytest test/data_model/ -v        # Data model tests
pytest test/physics/ -v           # Physics validation
pytest test/io_tests/ -v          # I/O tests

# Run specific key tests
pytest test/core/test_sample_output.py -v    # Fast validation
pytest test/physics/test_core_physics.py -v  # Physics checks
```

## Test Order

The test suite uses `conftest.py` to ensure `test_sample_output.py` runs first. This is necessary because the Fortran model maintains internal state between test runs, and running this benchmark test first ensures consistent results.

## Adding New Tests

When adding new tests:
1. Place them in the appropriate category directory
2. Follow the existing naming convention: `test_<functionality>.py`
3. Use descriptive test names that explain what is being tested
4. Add docstrings to explain complex test logic
5. Update this README if adding a new test category

For detailed testing approach, see docstrings in test files or `docs/source/contributing/testing_guide.rst`.