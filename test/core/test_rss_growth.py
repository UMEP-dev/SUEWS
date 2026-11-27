"""Contracts for the bounded memory witness; real controls run in the nightly."""

import json
from pathlib import Path
import subprocess
from types import SimpleNamespace
import warnings
import zipfile

import numpy as np
import pytest

from scripts.suews import rss_growth as rss

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-MEMORY", "C-TOOLING")]
LIMIT = 256 * 1024


def trace(rss_rate=0, heap_rate=0):
    return [
        {
            "unit": i,
            "rss_bytes": 100_000_000 + i * rss_rate,
            "glibc_in_use_bytes": 50_000_000 + i * heap_rate,
        }
        for i in range(1, 33)
    ]


def report(rss_rate=0, heap_rate=0):
    samples = trace(rss_rate, heap_rate)
    return {
        "signals": {key: rss.signal_summary(samples, key, LIMIT) for key in rss.SIGNALS}
    }


def controls():
    return {
        "baseline": report(),
        "neutral": report(),
        "near_boundary": report(8 * 1024, 320 * 1024),
        "late_boundary": report(0, 320 * 1024),
        "high_signal": report(2 * 1024 * 1024, 2 * 1024 * 1024),
    }


@pytest.mark.parametrize("rss_rate", [0, 8 * 1024])
def test_allocator_slack_cannot_hide_near_boundary_heap_growth(rss_rate):
    # Traces encode the reported RSS holdouts, not a fresh physics measurement.
    reports = controls()
    reports["near_boundary"] = report(rss_rate, 320 * 1024)
    assert not reports["near_boundary"]["signals"]["rss_bytes"]["exceeds_limit"]
    assert all(rss.evaluate_controls(reports).values())
    reports["near_boundary"] = report(rss_rate, 0)
    assert not rss.evaluate_controls(reports)["near_boundary"]


@pytest.mark.parametrize("signal", rss.SIGNALS)
def test_clean_boundary_is_inclusive_and_one_byte_above_fails(signal):
    samples = trace(LIMIT, LIMIT)
    result = rss.signal_summary(samples, signal, LIMIT)
    assert not result["exceeds_limit"]
    assert not result["sustained_full_exceeds_limit"]
    for row in samples:
        row[signal] += row["unit"]
    result = rss.signal_summary(samples, signal, LIMIT)
    assert result["exceeds_limit"]
    assert result["sustained_full_exceeds_limit"]


@pytest.mark.parametrize("mode", ["baseline", "neutral"])
def test_either_signal_rejects_unplanted_growth(mode):
    reports = controls()
    reports[mode] = report(0, LIMIT + 1)
    assert not rss.evaluate_controls(reports)[mode]
    reports[mode] = report(LIMIT + 1, 0)
    assert not rss.evaluate_controls(reports)[mode]


def test_missing_signal_and_heap_only_high_control_fail_closed():
    reports = controls()
    del reports["neutral"]["signals"]["glibc_in_use_bytes"]
    reports["high_signal"] = report(0, 2 * 1024 * 1024)
    checks = rss.evaluate_controls(reports)
    assert not checks["neutral"]
    assert not checks["high_signal_rss_separation"]


def test_far_end_spike_is_reported_even_when_robust_gate_passes():
    samples = trace()
    samples[-1]["rss_bytes"] += 8 * 1024 * 1024
    result = rss.signal_summary(samples, "rss_bytes", LIMIT)
    assert not result["exceeds_limit"]
    assert not result["sustained_tail_exceeds_limit"]
    assert result["endpoint_spike_over_unit_limit"]
    assert result["last_minus_previous_four_median_bytes"] == 8 * 1024 * 1024


@pytest.mark.parametrize("signal", rss.SIGNALS)
@pytest.mark.parametrize("mode", ["baseline", "neutral"])
def test_twelve_late_allocations_fail_the_clean_run(signal, mode):
    samples = trace()
    for row in samples:
        row[signal] += max(0, row["unit"] - 20) * 320 * 1024
    result = rss.signal_summary(samples, signal, LIMIT)
    assert not result["exceeds_limit"]
    assert result["sustained_tail_exceeds_limit"]
    reports = controls()
    reports[mode]["signals"][signal] = result
    assert not rss.evaluate_controls(reports)[mode]


@pytest.mark.parametrize("step", [23, 24, 25, 26, 27, 28, 29])
def test_step_to_a_plateau_cannot_mimic_sustained_tail_growth(step):
    samples = trace()
    for row in samples:
        if row["unit"] >= step:
            row["rss_bytes"] += 5 * 1024 * 1024
    result = rss.signal_summary(samples, "rss_bytes", LIMIT)
    assert not result["sustained_tail_exceeds_limit"]
    assert not result["exceeds_limit"]
    if step == 25:
        assert result["tail_exceeds_limit"]


def test_two_late_allocation_steps_are_not_continuous_growth():
    samples = trace()
    for row in samples:
        steps = int(row["unit"] >= 27) + int(row["unit"] >= 31)
        row["rss_bytes"] += steps * 4 * 1024**2
    result = rss.signal_summary(samples, "rss_bytes", LIMIT)
    assert result["tail_exceeds_limit"]
    assert result["terminal_block_median_increments_bytes_per_unit"] == [0, 0]
    assert not result["sustained_tail_exceeds_limit"]
    assert not result["exceeds_limit"]


@pytest.mark.parametrize("steps", [(17,), (11, 23)])
@pytest.mark.parametrize("signal", rss.SIGNALS)
def test_midrun_allocation_steps_do_not_fail_the_clean_run(steps, signal):
    samples = trace()
    for row in samples:
        completed = sum(row["unit"] >= step for step in steps)
        row[signal] += completed * (8 * 1024**2 // len(steps))
    result = rss.signal_summary(samples, signal, LIMIT)
    assert result["exceeds_limit"]
    assert result["step_limited_slope_bytes_per_unit"] == 0
    assert not result["sustained_full_exceeds_limit"]
    reports = controls()
    reports["baseline"]["signals"][signal] = result
    assert rss.evaluate_controls(reports)["baseline"]


def test_high_control_cannot_use_one_resident_jump_to_claim_growth():
    samples = trace()
    for row in samples[16:]:
        row["rss_bytes"] += 32 * 1024**2
    result = rss.signal_summary(samples, "rss_bytes", LIMIT)
    assert result["slope_bytes_per_unit"] > 1024**2
    reports = controls()
    reports["high_signal"]["signals"]["rss_bytes"] = result
    checks = rss.evaluate_controls(reports)
    assert checks["high_signal"]
    assert not checks["high_signal_rss_separation"]


@pytest.mark.parametrize("batch", [1, 4, 8])
def test_whole_window_preserves_bursty_and_steady_growth(batch):
    samples = trace()
    for row in samples:
        row["rss_bytes"] += (row["unit"] // batch) * batch * 320 * 1024
    result = rss.signal_summary(samples, "rss_bytes", LIMIT)
    assert result["sustained_full_exceeds_limit"]
    reports = controls()
    reports["baseline"]["signals"]["rss_bytes"] = result
    assert not rss.evaluate_controls(reports)["baseline"]


def test_growth_confined_to_first_half_survives_confirmation():
    samples = trace()
    for row in samples:
        row["rss_bytes"] += min(row["unit"], 16) * 640 * 1024
    result = rss.signal_summary(samples, "rss_bytes", LIMIT)
    assert result["sustained_full_exceeds_limit"]
    assert not result["sustained_tail_exceeds_limit"]


def test_released_temporary_spike_does_not_hide_underlying_growth():
    samples = trace(rss_rate=320 * 1024)
    samples[15]["rss_bytes"] += 8 * 1024**2
    result = rss.signal_summary(samples, "rss_bytes", LIMIT)
    assert result["sustained_full_exceeds_limit"]


def test_late_control_requires_positive_terminal_evidence():
    reports = controls()
    for signal in reports["late_boundary"]["signals"].values():
        signal["exceeds_limit"] = True
        signal["sustained_tail_exceeds_limit"] = False
    assert not rss.evaluate_controls(reports)["late_boundary"]


@pytest.mark.parametrize(
    "values",
    [
        np.empty((0, 3)),
        np.full((3, 3), -999.0),
        np.full((3, 3), np.nan),
        np.array([[1.0, -999.0, 2.0]]),
        np.array([[1.0, np.inf, 2.0]]),
        np.array([[1.0, 0.0, 2.0]]),
        np.array([[1.0, np.nan, 2.0]]),
        np.array([[1.0, 20.0, 2.0], [1.0, -999.0, 2.0]]),
    ],
)
def test_spartacus_rejects_empty_fill_nonfinite_and_inactive(values):
    with pytest.raises(RuntimeError, match="SPARTACUS"):
        rss.validate_spartacus(values)


def test_spartacus_accepts_finite_active_ground_flux():
    evidence = rss.validate_spartacus(np.array([[80.0, 100.0, -20.0]]))
    assert evidence == {"rows": 1, "qn_mean_w_m2": 80.0, "knet_ground_max_w_m2": 100.0}


@pytest.mark.parametrize("release", [False, True])
@pytest.mark.parametrize("delay", [0, 8])
def test_control_retains_only_after_specified_units_and_neutral_frees(
    tmp_path, monkeypatch, release, delay
):
    # Exercise the Windows contract even on hosts that provide sysconf.
    monkeypatch.delattr(rss.os, "sysconf", raising=False)
    monkeypatch.setattr(rss, "HeapSampler", lambda: SimpleNamespace(sample=lambda: {}))
    monkeypatch.setattr(rss, "current_rss_bytes", lambda: 100)
    args = SimpleNamespace(
        output=tmp_path / "samples.json",
        warmup=4,
        samples=16,
        retain_bytes=320 * 1024,
        retain_after=delay,
        release_control=release,
    )
    recorder = rss.Recorder(args)
    for _ in range(args.warmup + delay):
        recorder.sample()
    assert not recorder.retained
    recorder.sample()
    assert len(recorder.retained) == (0 if release else 1)
    assert recorder.samples[-1]["warmup"] is False


def test_wheel_provenance_checks_actual_binary_bytes(tmp_path):
    bridge = tmp_path / "suews_bridge.abi3.so"
    bridge.write_bytes(b"binary fixture")
    wheel = tmp_path / "fixture.whl"
    with zipfile.ZipFile(wheel, "w") as archive:
        archive.writestr("supy/suews_bridge.abi3.so", b"binary fixture")
    assert rss.wheel_bridge_identity(wheel, bridge)["matches_imported_bridge"]
    bridge.write_bytes(b"different binary")
    with pytest.raises(RuntimeError, match="does not match"):
        rss.wheel_bridge_identity(wheel, bridge)


def test_matching_bridge_cannot_mask_different_python_package(tmp_path):
    package = tmp_path / "supy"
    package.mkdir()
    bridge = package / "suews_bridge.abi3.so"
    bridge.write_bytes(b"same bridge")
    source = package / "__init__.py"
    source.write_bytes(b"wheel Python")
    libraries = tmp_path / "supy.libs"
    libraries.mkdir()
    runtime = libraries / "libgfortran.so"
    runtime.write_bytes(b"wheel runtime")
    wheel = tmp_path / "fixture.whl"
    with zipfile.ZipFile(wheel, "w") as archive:
        archive.writestr("supy/__init__.py", b"wheel Python")
        archive.writestr("supy/suews_bridge.abi3.so", b"same bridge")
        archive.writestr("supy.libs/libgfortran.so", b"wheel runtime")
    assert rss.wheel_package_identity(wheel, package)["matches_installed_package"]
    source.write_bytes(b"checkout Python")
    assert rss.wheel_bridge_identity(wheel, bridge)["matches_imported_bridge"]
    with pytest.raises(RuntimeError, match=r"supy/__init__\.py"):
        rss.wheel_package_identity(wheel, package)
    source.write_bytes(b"wheel Python")
    runtime.write_bytes(b"different runtime")
    with pytest.raises(RuntimeError, match=r"supy\.libs/libgfortran\.so"):
        rss.wheel_package_identity(wheel, package)


@pytest.mark.parametrize("failure", ["warning", "exception", "missing", "empty"])
def test_forcing_setup_fails_at_origin(failure):
    def factory():
        message = "scipy _superlu: failed to map segment from shared object"
        if failure == "warning":
            warnings.warn(
                f"Could not load forcing from config: {message}", stacklevel=2
            )
        if failure == "exception":
            raise ImportError(message)
        forcing = (
            None
            if failure in {"warning", "missing"}
            else SimpleNamespace(df=SimpleNamespace(empty=True))
        )
        return SimpleNamespace(forcing=forcing)

    supy = SimpleNamespace(SUEWSSimulation=SimpleNamespace(from_sample_data=factory))
    with pytest.raises(RuntimeError, match="forcing setup failed") as error:
        rss.prepare_simulation(supy)
    assert error.value.__cause__ is not None
    if failure in {"warning", "exception"}:
        assert "_superlu: failed to map segment" in str(error.value)


def fake_resource(soft=-1, hard=-1):
    state = [soft, hard]

    def set_limit(_kind, limits):
        state[:] = limits

    return SimpleNamespace(
        RLIMIT_AS=9,
        RLIM_INFINITY=-1,
        getrlimit=lambda _kind: tuple(state),
        setrlimit=set_limit,
    )


@pytest.mark.parametrize("soft,hard", [(-1, -1), (100, -1), (-1, 150), (100, 150)])
def test_headroom_uses_loaded_virtual_size_and_preserves_external_limits(soft, hard):
    mib = 1024**2
    current = 6 * 1024**3  # Reservations above the old cap are not RSS.
    resource = fake_resource(
        *(current + value * mib if value != -1 else -1 for value in (soft, hard))
    )
    observed = {"VmSize": current, "VmPeak": current + 192 * mib}
    result = rss.bound_address_space(observed, 32, resource)
    assert result["requested_headroom_bytes"] == (384 + 64) * mib
    expected = min([448] + [value for value in (soft, hard) if value != -1]) * mib
    assert result["effective_headroom_bytes"] == expected
    assert resource.getrlimit(9)[0] == current + expected


def test_transient_workspace_can_use_the_existing_ceiling():
    # A real streaming warm-up reserved 1,108,332,544 bytes above its final size.
    current = 3 * 1024**3
    gap = 1_108_332_544
    resource = fake_resource()
    result = rss.bound_address_space(
        {"VmSize": current, "VmPeak": current + gap}, 32, resource
    )
    assert result["workspace_headroom_bytes"] == 2 * 1024**3
    assert (
        result["limit_bytes"]
        == current + 2 * 1024**3 + 32 * rss.CONTROL_BYTES["high_signal"]
    )


def test_no_headroom_and_excessive_workspace_fail_closed():
    current = 6 * 1024**3
    with pytest.raises(RuntimeError, match="No address-space headroom"):
        rss.bound_address_space(
            {"VmSize": current, "VmPeak": current}, 32, fake_resource(current)
        )
    with pytest.raises(RuntimeError, match="observed workspace"):
        rss.bound_address_space(
            {"VmSize": current, "VmPeak": current + 2 * 1024**3}, 32, fake_resource()
        )


def test_limit_established_after_real_warmup_before_retention(tmp_path, monkeypatch):
    monkeypatch.setattr(rss, "HeapSampler", lambda: SimpleNamespace(sample=lambda: {}))
    monkeypatch.setattr(rss, "current_rss_bytes", lambda: 100)
    calls = []

    def establish():
        calls.append(len(recorder.samples))
        assert not recorder.retained
        return {"limit_bytes": 123}

    args = SimpleNamespace(
        output=tmp_path / "samples.json",
        warmup=4,
        samples=16,
        retain_bytes=320 * 1024,
        retain_after=0,
        release_control=False,
    )
    recorder = rss.Recorder(args, establish)
    for _ in range(3):
        recorder.sample()
    assert not calls
    recorder.sample()
    assert calls == [4]
    assert json.loads(
        args.output.with_suffix(".limits.json").read_text(encoding="utf-8")
    ) == {"limit_bytes": 123}
    recorder.sample()
    assert len(recorder.retained) == 1
    assert calls == [4]
    for _ in range(15):
        recorder.sample()
    with pytest.raises(RuntimeError, match="bounded number"):
        recorder.sample()
    assert len(recorder.retained) == 16


def test_direct_lane_collection_is_time_bounded(tmp_path, monkeypatch):
    monkeypatch.setattr(
        rss.sys,
        "argv",
        [
            "rss_growth.py",
            "--lane",
            "streaming",
            "--output",
            str(tmp_path / "result.json"),
        ],
    )

    def run(cmd, **kwargs):
        assert "--worker" in cmd
        assert kwargs["timeout"] == 240
        raise subprocess.TimeoutExpired(cmd, kwargs["timeout"])

    monkeypatch.setattr(rss.subprocess, "run", run)
    # main pins these variables; keep those changes scoped to this test.
    for key in rss.THREAD_ENV:
        monkeypatch.setenv(key, "1")
    assert rss.main() == 1


@pytest.mark.parametrize("samples,delay", [(16, 8), (20, 12), (32, 20), (128, 80)])
def test_suite_runs_both_lanes_all_controls_and_pins_threads(
    tmp_path, monkeypatch, samples, delay
):
    calls = []

    def run(cmd, **kwargs):
        lane = cmd[cmd.index("--lane") + 1]
        path = Path(cmd[cmd.index("--output") + 1])
        calls.append((lane, path.stem, kwargs))
        assert all(kwargs["env"][key] == "1" for key in rss.THREAD_ENV)
        assert kwargs["timeout"] == 240
        mode = path.stem.removeprefix(lane + "-")
        assert int(cmd[cmd.index("--retain-bytes") + 1]) == rss.CONTROL_BYTES[mode]
        assert ("--release-control" in cmd) == (mode == "neutral")
        if mode == "late_boundary":
            assert int(cmd[cmd.index("--retain-after") + 1]) == delay
        else:
            assert "--retain-after" not in cmd
        if lane == "streaming" and mode == "baseline":
            raise subprocess.TimeoutExpired(cmd, kwargs["timeout"])
        path.write_text(json.dumps(controls()[mode]), encoding="utf-8")
        return SimpleNamespace(returncode=0)

    monkeypatch.setenv("OMP_NUM_THREADS", "64")
    monkeypatch.setattr(rss.subprocess, "run", run)
    args = SimpleNamespace(output=tmp_path, warmup=8, samples=samples, limit=LIMIT)
    assert rss.suite(args) == 1
    assert len(calls) == 10
    summary = json.loads((tmp_path / "summary.json").read_text(encoding="utf-8"))
    assert not summary["lanes"]["streaming"]["passed"]
    assert summary["lanes"]["spartacus"]["passed"]
