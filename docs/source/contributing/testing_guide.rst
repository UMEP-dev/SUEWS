================
Testing Guide
================

This guide provides comprehensive information about testing in SUEWS/SuPy, including test organisation, running tests, writing new tests, and best practices.

Test Organisation
=================

The SUEWS/SuPy test suite is organised into several categories:

Core Tests
----------

**Location**: ``test/core/``

These tests validate the fundamental SUEWS engine functionality:

- ``test_sample_output.py`` - Benchmark validation against known good outputs
- ``test_fortran_state_persistence.py`` - State isolation between runs
- ``test_floating_point_stability.py`` - Numerical stability and consistency
- ``test_supy.py`` - Core SuPy functionality and integration
- ``test_suews_simulation.py`` - High-level simulation interface
- ``test_load.py`` - Data loading functionality
- ``test_post.py`` - Post-processing functionality
- ``test_util_*.py`` - Utility function tests (atmospheric, surface conductance, OHM, etc.)

Data Model Tests
----------------

**Location**: ``test/data_model/``

These tests ensure the configuration and data structures work correctly:

- ``test_data_model.py`` - Pydantic data model validation
- ``test_precheck.py`` - Pre-run validation checks
- ``test_conditional_validation.py`` - Physics option compatibility
- ``test_validation_*.py`` - Various validation utilities

Physics Tests
-------------

**Location**: ``test/physics/``

Tests that validate scientific accuracy and physical consistency:

- ``test_core_physics.py`` - Energy balance, water balance, physical constraints
- Physics-related tests from ``test_supy.py`` (e.g., water balance closure)

I/O Tests
---------

**Location**: ``test/io/``

Tests for input/output operations:

- ``test_output_config.py`` - Output configuration handling
- ``test_save_supy.py`` - Output saving functionality
- ``test_resample_output.py`` - Temporal resampling
- ``test_forcing_file_list.py`` - Multiple forcing file handling

Running Tests
=============

Quick Start
-----------

.. code-block:: bash

    # Run all tests
    make test
    
    # Run specific test file
    pytest test/core/test_sample_output.py -v
    
    # Run specific test function
    pytest test/core/test_supy.py::TestSuPy::test_benchmark1_same -v
    
    # Run with coverage
    pytest test/ --cov=supy --cov-report=html

Test Categories
---------------

**Fast Validation** (runs first in CI for fail-fast):

.. code-block:: bash

    pytest test/core/test_sample_output.py -v

**Physics Checks** (critical for model accuracy):

.. code-block:: bash

    pytest test/physics/test_core_physics.py -v

**Full Test Suite**:

.. code-block:: bash

    pytest test/ -v

Performance Considerations
--------------------------

- Use ``test/fixtures/benchmark1/benchmark1_short.yml`` for faster testing (7 days vs full year)
- Original benchmark tests use full year data and take ~25 seconds
- Short benchmark tests complete in ~2 seconds

Writing Tests
=============

Test Structure
--------------

Follow the standard pytest structure:

.. code-block:: python

    import pytest
    import numpy as np
    import pandas as pd
    import supy as sp
    
    class TestMyFeature:
        """Test suite for my feature."""
        
        def setup_method(self):
            """Set up test fixtures."""
            self.df_state = sp.init_supy(path_init="path/to/config.yml")
            
        def test_basic_functionality(self):
            """Test basic feature behaviour."""
            result = my_function(self.df_state)
            assert result is not None
            
        def test_edge_case(self):
            """Test edge case handling."""
            with pytest.raises(ValueError):
                my_function(invalid_input)

Best Practices
--------------

1. **Use Descriptive Names**: Test names should clearly indicate what is being tested
   
   .. code-block:: python
   
       # Good
       def test_energy_balance_closure_within_tolerance(self):
       
       # Bad
       def test_eb(self):

2. **Test One Thing**: Each test should validate a single behaviour

3. **Use Fixtures**: Share setup code using pytest fixtures or setup methods

4. **Test Edge Cases**: Include tests for boundary conditions, error cases, and missing data

5. **Use Appropriate Assertions**: Be specific about what you're testing
   
   .. code-block:: python
   
       # Good - specific assertion with tolerance
       np.testing.assert_allclose(actual, expected, rtol=1e-6)
       
       # Bad - too general
       assert actual is not None

6. **Document Complex Tests**: Add docstrings explaining the test purpose and methodology

Common Test Patterns
--------------------

**Testing Numerical Accuracy**:

.. code-block:: python

    def test_calculation_accuracy(self):
        """Test calculation matches expected values within tolerance."""
        result = calculate_something(input_data)
        expected = 42.0
        np.testing.assert_allclose(result, expected, rtol=1e-6, atol=1e-9)

**Testing DataFrame Operations**:

.. code-block:: python

    def test_dataframe_processing(self):
        """Test DataFrame processing preserves structure."""
        df_input = pd.DataFrame({'col1': [1, 2, 3]})
        df_output = process_dataframe(df_input)
        
        # Check structure
        assert len(df_output) == len(df_input)
        assert list(df_output.columns) == ['col1', 'col2']
        
        # Check values
        pd.testing.assert_frame_equal(df_output, expected_df)

**Testing Error Handling**:

.. code-block:: python

    def test_invalid_input_raises_error(self):
        """Test appropriate error for invalid input."""
        with pytest.raises(ValueError, match="Invalid parameter"):
            function_under_test(invalid_param=-1)

**Testing File I/O**:

.. code-block:: python

    def test_file_loading(self, tmp_path):
        """Test file loading functionality."""
        # Create temporary test file
        test_file = tmp_path / "test_data.csv"
        test_file.write_text("col1,col2\n1,2\n3,4")
        
        # Test loading
        data = load_file(test_file)
        assert len(data) == 2

Floating-Point Considerations
=============================

When testing floating-point calculations:

1. **Avoid Exact Equality**: Never use ``==`` for floating-point comparisons
2. **Use Appropriate Tolerances**: Choose tolerances based on expected precision
3. **Consider Numerical Stability**: Test with values that might cause instability

.. code-block:: python

    # Good - uses tolerance
    np.testing.assert_allclose(result, expected, rtol=1e-6)
    
    # Bad - exact equality
    assert result == expected

State Management
================

SUEWS uses Fortran modules that can maintain state between calls. To ensure test isolation:

1. **Test Order Independence**: Tests should pass regardless of execution order
2. **State Reset**: Consider if state needs resetting between tests
3. **Use Fresh Instances**: Create new model instances for each test when possible

CI/CD Integration
=================

The test suite is integrated with GitHub Actions:

- **Fast Fail**: ``test_sample_output.py`` runs first to catch major issues quickly
- **Platform Testing**: Tests run on Linux, macOS, and Windows
- **Python Versions**: Tests run on Python 3.9-3.13
- **Wheel Building**: Physics tests run during wheel building process

Debugging Failed Tests
======================

When tests fail:

1. **Run Locally**: Reproduce the failure locally first
2. **Check Logs**: Review full error messages and stack traces
3. **Isolate the Test**: Run the specific failing test in isolation
4. **Check Environment**: Ensure correct Python version and dependencies
5. **Use Debugging Tools**: 

   .. code-block:: bash
   
       # Run with debugging
       pytest test/failing_test.py -v -s --pdb
       
       # Show local variables on failure
       pytest test/failing_test.py -v --showlocals

Performance Testing
===================

For performance-sensitive code:

.. code-block:: python

    import time
    
    def test_performance(self):
        """Test function completes within time limit."""
        start_time = time.time()
        
        result = expensive_function(large_input)
        
        elapsed_time = time.time() - start_time
        assert elapsed_time < 10.0, f"Function too slow: {elapsed_time:.2f}s"

Test Data
=========

Test data is stored in ``test/fixtures/``:

- ``benchmark1/`` - Standard benchmark configuration and forcing data
- ``benchmark1_short.yml`` - Shortened benchmark for faster testing
- Other test-specific data files

When adding new test data:

1. Keep files small where possible
2. Use meaningful names
3. Document the data source and purpose
4. Consider using generated data for simple cases

Continuous Improvement
======================

The test suite should evolve with the codebase:

1. **Add Tests for New Features**: Every new feature should include tests
2. **Add Tests for Bug Fixes**: Prevent regression by adding tests for fixed bugs
3. **Refactor Tests**: Keep tests clean and maintainable
4. **Monitor Coverage**: Aim for high test coverage but focus on meaningful tests
5. **Update Documentation**: Keep this guide updated with new patterns and practices

Common Issues and Solutions
===========================

**Import Errors**:

- Ensure ``make dev`` has been run to install the package in development mode
- Check that the virtual environment is activated

**Numerical Differences**:

- Different platforms/compilers may produce slightly different results
- Use appropriate tolerances (typically 1e-6 for relative, 1e-9 for absolute)

**State Pollution**:

- Tests fail when run together but pass individually
- Ensure proper state isolation between tests
- Check for global variables or module-level state

**Slow Tests**:

- Use shortened test data where appropriate
- Consider marking slow tests and skipping in quick test runs
- Profile tests to identify bottlenecks

Summary
=======

Good tests are:

- **Fast**: Complete quickly to encourage frequent running
- **Isolated**: Don't depend on other tests or external state
- **Repeatable**: Produce the same results every time
- **Self-Documenting**: Clear about what they test and why
- **Thorough**: Cover normal cases, edge cases, and error conditions

Remember: tests are code too. Apply the same quality standards to tests as to production code.