"""pytest configuration for SUEWS test suite."""

import supy
from supy._supy_module import (
    _init_config,
    _init_supy,
    _load_forcing_grid,
    _load_sample_data,
    _run_supy,
    _run_supy_sample,
    _save_supy,
)


def pytest_configure():
    """Monkey patch legacy public functions to private implementations for tests."""
    supy._deprecated_init_supy = supy.init_supy
    supy._deprecated_load_forcing_grid = supy.load_forcing_grid
    supy._deprecated_run_supy = supy.run_supy
    supy._deprecated_run_supy_sample = supy.run_supy_sample
    supy._deprecated_save_supy = supy.save_supy
    supy._deprecated_load_sample_data = supy.load_sample_data
    supy._deprecated_init_config = supy.init_config

    supy.init_supy = _init_supy
    supy.load_forcing_grid = _load_forcing_grid
    supy.load_sample_data = _load_sample_data
    supy.run_supy = _run_supy
    supy.run_supy_sample = _run_supy_sample
    supy.save_supy = _save_supy
    supy.init_config = _init_config


def pytest_collection_modifyitems(items):
    """Ensure test_sample_output runs first, then API equivalence tests, to avoid Fortran state interference.

    The sample output validation test must run FIRST before any other tests because:
    - The Fortran model maintains internal state between test runs
    - Even API equivalence tests can pollute the Fortran state
    - This causes small numerical differences that accumulate over simulations
    - Uninitialized memory from previous runs can pollute output arrays

    Test ordering:
    1. test_sample_output.py tests (highest priority - must be completely clean)
    2. API equivalence tests (need clean state, but less critical than sample output)
    3. All other tests
    """
    # Separate into three priority groups
    sample_output_tests = []
    api_equivalence_tests = []
    other_tests = []

    for item in items:
        # Priority 1: test_sample_output.py must run first
        if "test_sample_output" in str(item.fspath) or "core/test_sample_output" in str(
            item.fspath
        ):
            sample_output_tests.append(item)
        # Priority 2: API equivalence tests need clean state but less critical
        elif (
            "test_functional_matches_oop" in item.nodeid
            or "TestPublicAPIEquivalence" in item.nodeid
        ):
            api_equivalence_tests.append(item)
        # Priority 3: All other tests
        else:
            other_tests.append(item)

    # Run in priority order: sample_output → API equivalence → others
    items[:] = sample_output_tests + api_equivalence_tests + other_tests
