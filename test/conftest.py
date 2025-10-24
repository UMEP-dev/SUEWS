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
    """Ensure test_sample_output runs first to avoid Fortran state interference.

    The sample output validation test must run before other tests because:
    - The Fortran model maintains internal state between test runs
    - Other tests can leave the model in a different state
    - This causes small numerical differences that accumulate over simulations
    """
    # Separate sample output tests from others
    sample_output_tests = []
    other_tests = []

    for item in items:
        if "test_sample_output" in str(item.fspath) or "core/test_sample_output" in str(
            item.fspath
        ):
            sample_output_tests.append(item)
        else:
            other_tests.append(item)

    # Run sample output tests first, then others
    items[:] = sample_output_tests + other_tests
