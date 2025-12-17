"""Tests for kernel call serialisation and thread safety.

These tests verify that:
1. The kernel lock correctly serialises concurrent calls to the Fortran kernel
   (required while module-level SAVE variables are still used for error state)
2. State-based error handling works correctly (path towards lock-free operation)

The Fortran kernel uses module-level SAVE variables for error state which are
not thread-safe. The _kernel_lock serialises access to protect these variables.

Future: When state-based error handling is used exclusively (error info read
from SUEWS_STATE.errorState instead of module-level variables), the lock can
be removed for truly parallel execution (important for WRF coupling).
"""

import threading
import time

import pytest


@pytest.mark.core
def test_kernel_lock_exists_for_serialisation():
    """Verify _kernel_lock exists and is a threading.Lock."""
    import supy._run as run

    assert hasattr(run, "_kernel_lock")
    assert isinstance(run._kernel_lock, type(threading.Lock()))


@pytest.mark.core
def test_concurrent_kernel_calls_are_serialised(monkeypatch):
    """Verify suews_cal_tstep serialises concurrent kernel calls.

    This test ensures that even when multiple threads attempt to call
    the kernel simultaneously, only one executes at a time due to the lock.
    """
    import supy._run as run

    monkeypatch.setattr(run, "list_var_input", [])
    monkeypatch.setattr(run, "list_var_inout", [])

    current_calls = 0
    max_concurrent_calls = 0
    counter_lock = threading.Lock()

    def fake_suews_cal_main(**_kwargs):
        nonlocal current_calls, max_concurrent_calls
        with counter_lock:
            current_calls += 1
            max_concurrent_calls = max(max_concurrent_calls, current_calls)
        time.sleep(0.05)
        with counter_lock:
            current_calls -= 1

        class Result:
            pass

        return Result()

    monkeypatch.setattr(run.sd, "suews_cal_main", fake_suews_cal_main)

    dict_state_start = {"tstep": 1, "dt_since_start": 0}
    barrier = threading.Barrier(2)

    def worker():
        barrier.wait()
        run.suews_cal_tstep(dict_state_start, {})

    t1 = threading.Thread(target=worker)
    t2 = threading.Thread(target=worker)
    t1.start()
    t2.start()
    t1.join(timeout=5)
    t2.join(timeout=5)

    assert not t1.is_alive()
    assert not t2.is_alive()
    assert max_concurrent_calls == 1


@pytest.mark.core
def test_concurrent_multitstep_calls_are_serialised(monkeypatch):
    """Verify suews_cal_tstep_multi also serialises concurrent kernel calls.

    This tests the primary production code path (used by run_supy_ser).
    """
    import pandas as pd
    import supy._run as run

    # Minimal input structure for suews_cal_tstep_multi
    monkeypatch.setattr(run, "list_var_input_multitsteps", [])
    monkeypatch.setattr(run, "list_var_inout_multitsteps", [])

    current_calls = 0
    max_concurrent_calls = 0
    counter_lock = threading.Lock()

    def fake_suews_cal_multitsteps(**_kwargs):
        nonlocal current_calls, max_concurrent_calls
        with counter_lock:
            current_calls += 1
            max_concurrent_calls = max(max_concurrent_calls, current_calls)
        time.sleep(0.05)
        with counter_lock:
            current_calls -= 1

        class Result:
            pass

        return Result()

    monkeypatch.setattr(run.sd, "suews_cal_multitsteps", fake_suews_cal_multitsteps)

    # Create minimal forcing DataFrame
    df_forcing = pd.DataFrame(
        {
            "metforcingdata_grid": [0],
            "ts5mindata_ir": [0],
            "isec": [0],
        },
        index=pd.DatetimeIndex(["2020-01-01 00:00:00"]),
    )

    dict_state_start = {"tstep": 1, "dt_since_start": 0}
    barrier = threading.Barrier(2)

    def worker():
        barrier.wait()
        try:
            run.suews_cal_tstep_multi(dict_state_start, df_forcing, debug_mode=False)
        except Exception:
            # We expect this to fail since we're not providing full inputs,
            # but we're only testing that the lock works
            pass

    t1 = threading.Thread(target=worker)
    t2 = threading.Thread(target=worker)
    t1.start()
    t2.start()
    t1.join(timeout=5)
    t2.join(timeout=5)

    assert not t1.is_alive()
    assert not t2.is_alive()
    # Due to the mocked function, max_concurrent_calls should be 1
    assert max_concurrent_calls == 1


@pytest.mark.core
def test_kernel_lock_released_on_error(monkeypatch):
    """Verify the kernel lock is released even when an error occurs.

    This prevents deadlocks when the kernel raises an exception.
    """
    import supy._run as run

    monkeypatch.setattr(run, "list_var_input", [])
    monkeypatch.setattr(run, "list_var_inout", [])

    call_count = 0

    def failing_kernel(**_kwargs):
        nonlocal call_count
        call_count += 1
        if call_count == 1:
            raise RuntimeError("Simulated kernel failure")

        class Result:
            pass

        return Result()

    monkeypatch.setattr(run.sd, "suews_cal_main", failing_kernel)

    dict_state_start = {"tstep": 1, "dt_since_start": 0}

    # First call should fail
    with pytest.raises(RuntimeError, match="kernel"):
        run.suews_cal_tstep(dict_state_start, {})

    # Second call should succeed (lock was released)
    run.suews_cal_tstep(dict_state_start, {})

    assert call_count == 2


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
