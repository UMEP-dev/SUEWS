import threading
import time


def test_kernel_lock_exists_for_serialisation():
    import supy._run as run

    assert hasattr(run, "_kernel_lock")
    assert isinstance(run._kernel_lock, type(threading.Lock()))


def test_concurrent_kernel_calls_are_serialised(monkeypatch):
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
