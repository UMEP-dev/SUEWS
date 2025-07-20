"""pytest configuration for SUEWS test suite."""


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
        if "test_sample_output" in str(item.fspath) or "core/test_sample_output" in str(item.fspath):
            sample_output_tests.append(item)
        else:
            other_tests.append(item)

    # Run sample output tests first, then others
    items[:] = sample_output_tests + other_tests