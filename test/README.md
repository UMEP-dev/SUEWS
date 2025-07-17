# SUEWS test files

This folder contains test files for SUEWS versions delivered as fortran binaries.

## Key Test Files

- `test_sample_output.py` - Tolerance-based validation (runs first in CI fast-fail)
- `test_core_physics.py` - Physical consistency checks (runs during wheel building)
- `test_supy.py` - Comprehensive test suite (runs during wheel building)

## Running Tests

```bash
pytest test/test_sample_output.py -v   # Fast validation
pytest test/test_core_physics.py -v     # Physics checks
pytest test/ -v                         # All tests
```

For detailed testing approach, see docstrings in test files or `docs/source/contributing/testing_guide.rst`.