# SUEWS Test Suite

This directory contains the comprehensive test suite for SUEWS/SuPy, organised by functionality.

## Test Organisation

```
test/
├── core/           # Core SUEWS engine tests
├── data_model/     # Configuration and data model tests  
├── physics/        # Physics validation tests
├── io/             # Essential I/O (forcing, output, state)
└── fixtures/       # Test data and configurations
```

## Quick Start

```bash
# Run all tests (recommended before committing)
make test

# Run specific test categories
pytest test/core/ -v          # Core functionality
pytest test/physics/ -v       # Physics validation
pytest test/data_model/ -v    # Data model tests

# Run critical tests only (fast)
pytest test/core/test_sample_output.py -v
```

## Key Test Files

### Core Tests (`test/core/`)
- `test_sample_output.py` - Benchmark validation (CI fail-fast)
- `test_supy.py` - Comprehensive SuPy functionality
- `test_fortran_state_persistence.py` - State isolation testing
- `test_floating_point_stability.py` - Numerical stability
- `test_load.py` - Data loading functionality
- `test_post.py` - Post-processing functionality
- `test_util_*.py` - Utility function tests

### Physics Tests (`test/physics/`)
- `test_core_physics.py` - Energy/water balance, physical constraints

### Data Model Tests (`test/data_model/`)
- `test_data_model.py` - Pydantic model validation
- `test_precheck.py` - Pre-run validation
- `test_conditional_validation.py` - Physics option compatibility

## Writing Tests

When adding new functionality:

1. **Add tests in the appropriate category** - core/, physics/, data_model/, or io/
2. **Follow existing patterns** - see similar tests for examples
3. **Use descriptive names** - test names should explain what they validate
4. **Test edge cases** - include boundary conditions and error handling
5. **Keep tests fast** - use short test data where possible

Example test structure:

```python
import pytest
import supy as sp

class TestNewFeature:
    """Test suite for new feature."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.df_state = sp.init_supy(path_init="test/fixtures/config.yml")
    
    def test_normal_operation(self):
        """Test feature under normal conditions."""
        result = new_feature(self.df_state)
        assert result is not None
        
    def test_edge_case(self):
        """Test feature handles edge cases."""
        with pytest.raises(ValueError):
            new_feature(invalid_input)
```

## Test Data

Test data is stored in `test/fixtures/`:

- `benchmark1/` - Full benchmark configuration (full year)
- `benchmark1_short.yml` - Fast benchmark (7 days) for development
- Various test-specific data files

## Performance Tips

- Use `benchmark1_short.yml` for faster development cycles
- Run full benchmark tests before final commits
- Profile slow tests with `pytest --durations=10`

## Debugging Failed Tests

```bash
# Run with verbose output
pytest test/failing_test.py -v -s

# Drop into debugger on failure
pytest test/failing_test.py --pdb

# Show local variables
pytest test/failing_test.py --showlocals
```

## Comprehensive Guide

For detailed information about testing philosophy, patterns, and best practices, see:

📖 **[Contributing: Testing Guide](../docs/source/contributing/testing_guide.rst)**

## CI Integration

Tests run automatically on:
- Every push and pull request
- Multiple platforms (Linux, macOS, Windows)
- Python versions 3.9-3.13

The CI runs tests in order:
1. Fast validation (`test_sample_output.py`)
2. Core physics tests
3. Full test suite