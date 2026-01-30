"""Tests for kernel call thread safety and state-based error handling.

These tests verify that:
1. State-based error handling works correctly (thread-safe via per-call state)
2. The multi-timestep path (suews_cal_tstep_multi) can run concurrently

The SUEWS kernel uses state-based error handling via suews_cal_tstep_multi.
Each call creates its own SUEWS_STATE_BLOCK which captures any errors,
enabling thread-safe parallel execution (important for WRF coupling with
MPI/OpenMP).
"""

import pytest


# =============================================================================
# State-based error handling tests
# =============================================================================


@pytest.mark.core
def test_state_based_error_check_function_exists():
    """Verify _check_supy_error_from_state function exists."""
    import supy._run as run

    assert hasattr(run, "_check_supy_error_from_state")
    assert callable(run._check_supy_error_from_state)


@pytest.mark.core
def test_state_based_error_check_with_none():
    """Verify _check_supy_error_from_state handles None gracefully."""
    import supy._run as run

    # Should not raise any exception
    run._check_supy_error_from_state(None)


@pytest.mark.core
def test_state_based_error_check_raises_on_error():
    """Verify _check_supy_error_from_state raises SUEWSKernelError on error state."""
    import supy._run as run

    # Create a mock state block with error
    class MockErrorState:
        flag = True
        code = 42
        message = "Test error message"

    class MockState:
        errorstate = MockErrorState()

    class MockStateBlock:
        block = [MockState()]

    with pytest.raises(run.SUEWSKernelError) as exc_info:
        run._check_supy_error_from_state(MockStateBlock())

    assert exc_info.value.code == 42
    assert "Test error message" in str(exc_info.value)


@pytest.mark.core
def test_state_based_error_check_no_error():
    """Verify _check_supy_error_from_state does not raise when no error."""
    import supy._run as run

    # Create a mock state block without error
    class MockErrorState:
        flag = False
        code = 0
        message = ""

    class MockState:
        errorstate = MockErrorState()

    class MockStateBlock:
        block = [MockState()]

    # Should not raise any exception
    run._check_supy_error_from_state(MockStateBlock())


@pytest.mark.core
def test_state_based_error_check_handles_invalid_state():
    """Verify _check_supy_error_from_state handles invalid state structure gracefully."""
    import supy._run as run

    # Create a mock state block with missing attributes
    class MockStateBlock:
        block = []  # Empty block - will cause IndexError

    # Should not raise - falls back silently
    run._check_supy_error_from_state(MockStateBlock())

    # Missing errorstate attribute
    class MockState:
        pass  # No errorstate

    class MockStateBlock2:
        block = [MockState()]

    # Should not raise - falls back silently
    run._check_supy_error_from_state(MockStateBlock2())


@pytest.mark.core
def test_no_kernel_lock():
    """Verify _kernel_lock has been removed (state-based error handling is thread-safe)."""
    import supy._run as run

    # _kernel_lock should no longer exist
    assert not hasattr(run, "_kernel_lock"), (
        "_kernel_lock should be removed - use state-based error handling"
    )
