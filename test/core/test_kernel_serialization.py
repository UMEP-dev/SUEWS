"""Tests for kernel call serialisation and thread safety.

These tests verify that the kernel lock correctly serialises concurrent
calls to the Fortran kernel, which uses SAVE variables that are not thread-safe.
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
