"""Contract tests for the standalone pytest CI metrics plugin."""

from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
from typing import Any

import pytest

from scripts.suews.pytest_ci_metrics import ProcfsSampler, read_proc_process

pytestmark = pytest.mark.api

PROJECT_ROOT = Path(__file__).resolve().parents[2]
SCHEMA_V2_FIXTURE = PROJECT_ROOT / "test/fixtures/ci_metrics/schema-v2-xdist.json"


@pytest.mark.smoke
def test_plugin_writes_parseable_metrics_and_step_summary(tmp_path: Path) -> None:
    """A pytest run records its stable inventory, timings, workers and warnings."""
    test_file = tmp_path / "test_sample.py"
    test_file.write_text(
        """\
import warnings

import pytest


def test_pass():
    assert True


def test_warn():
    warnings.warn("group me", UserWarning)


@pytest.mark.slow(reason="probe the recorded reason")
def test_burn():
    # Enough arithmetic to register on the 10 ms os.times() clock.
    assert sum(range(10_000_000)) > 0


def test_skipped():
    pytest.skip("probe a skipped node's record")
""",
        encoding="utf-8",
    )
    metrics_path = tmp_path / "ci-metrics.json"
    summary_path = tmp_path / "step-summary.md"
    env = os.environ.copy()
    env.update({
        "GITHUB_STEP_SUMMARY": str(summary_path),
        "PYTHONPATH": os.pathsep.join(
            part for part in (str(PROJECT_ROOT), env.get("PYTHONPATH", "")) if part
        ),
        "SUEWS_CI_METRICS": str(metrics_path),
    })

    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--confcutdir",
            str(tmp_path),
            "-p",
            "scripts.suews.pytest_ci_metrics",
            str(test_file),
            "-q",
            # The temporary tree has no ini file, so register the marker the
            # sample uses; an unknown mark would add a warning record.
            "-o",
            "markers=slow",
        ],
        cwd=tmp_path,
        env=env,
        check=False,
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, result.stdout + result.stderr
    metrics = json.loads(metrics_path.read_text(encoding="utf-8"))
    node_ids = [
        "test_sample.py::test_burn",
        "test_sample.py::test_pass",
        "test_sample.py::test_skipped",
        "test_sample.py::test_warn",
    ]
    expected_hash = hashlib.sha256("\n".join(node_ids).encode()).hexdigest()

    assert metrics["schema_version"] == 2
    assert metrics["result"] == {
        "exit_code": 0,
        "outcomes": {
            "failed": 0,
            "passed": 3,
            "skipped": 1,
            "xfailed": 0,
            "xpassed": 0,
        },
    }
    assert metrics["inventory"] == {
        "node_count": 4,
        "node_id_sha256": expected_hash,
    }
    _assert_per_test_records(metrics["tests"], node_ids)
    assert metrics["execution"] == {
        "effective_worker_count": 1,
        "worker_finish_skew_seconds": 0.0,
        "worker_tail_over_median_seconds": 0.0,
        "workers": [],
        "xdist": False,
    }
    assert set(metrics["phases"]) == {"collection", "session", "tests"}
    assert all(phase["duration_seconds"] >= 0 for phase in metrics["phases"].values())
    _assert_resource_contract(metrics["resources"])
    assert metrics["warnings"] == [
        {
            "category": "UserWarning",
            "count": 1,
            "fingerprint": hashlib.sha256(b"UserWarning\ngroup me").hexdigest(),
            "message": "group me",
            "normalised_message": "group me",
        }
    ]

    summary = summary_path.read_text(encoding="utf-8")
    assert "Pytest CI metrics" in summary
    assert expected_hash in summary
    assert "UserWarning: group me" in summary


def _assert_per_test_records(records: list[dict[str, Any]], node_ids: list[str]) -> None:
    """Check the additive per-test records: one per node, phases, markers, CPU."""
    assert [record["node_id"] for record in records] == node_ids
    by_node = {record["node_id"]: record for record in records}
    phase_keys = {"setup", "call", "teardown", "total"}
    for record in records:
        assert record["cpu_method"] == "os.times"
        assert set(record["cpu_seconds"]) == phase_keys
        assert set(record["wall_seconds"]) == phase_keys
        for measurement in (record["cpu_seconds"], record["wall_seconds"]):
            assert all(value >= 0 for value in measurement.values())
            assert measurement["total"] == pytest.approx(
                measurement["setup"] + measurement["call"] + measurement["teardown"],
                abs=1e-5,
            )

    burn = by_node["test_sample.py::test_burn"]
    assert burn["markers"] == ["slow"]
    assert burn["marker_reasons"] == {"slow": "probe the recorded reason"}
    assert burn["outcome"] == "passed"
    # The busy loop runs in the test body, so the call phase carries the CPU
    # and dominates the setup and teardown of a fixture-free test.
    assert burn["cpu_seconds"]["call"] > 0
    assert burn["cpu_seconds"]["call"] > burn["cpu_seconds"]["setup"]
    assert burn["wall_seconds"]["call"] > 0

    assert by_node["test_sample.py::test_pass"]["markers"] == []
    assert by_node["test_sample.py::test_pass"]["marker_reasons"] == {}
    assert by_node["test_sample.py::test_pass"]["outcome"] == "passed"

    skipped = by_node["test_sample.py::test_skipped"]
    assert skipped["outcome"] == "skipped"
    # pytest.skip() inside the body ends the call phase; the record still
    # carries all three phases, with the missing ones at zero rather than
    # absent, so a consumer never has to special-case skipped nodes.
    assert skipped["cpu_seconds"]["total"] >= 0


def _assert_resource_contract(resources: dict[str, Any]) -> None:
    """Check stable availability metadata for process-tree measurements."""
    assert resources["sample_interval_seconds"] > 0
    assert resources["sample_count"] >= 0
    for name, unit in (
        ("process_tree_cpu_seconds", "seconds"),
        ("process_tree_peak_rss_bytes", "bytes"),
    ):
        measurement = resources[name]
        assert measurement["unit"] == unit
        assert isinstance(measurement["available"], bool)
        if sys.platform.startswith("linux"):
            assert resources["sample_count"] > 0
            assert measurement["available"] is True
            assert measurement["method"] == "linux-procfs-sampling"
            assert measurement["reason"] is None
            assert measurement["status"] == "sampled"
            assert measurement["value"] >= 0
        else:
            assert measurement["available"] is False
            assert measurement["value"] is None
            assert measurement["method"] is None
            assert measurement["reason"]
            assert measurement["status"] == "unavailable"


def _write_proc_process(
    proc_root: Path,
    pid: int,
    *,
    children: tuple[int, ...] = (),
    cpu_ticks: tuple[int, int] = (0, 0),
    rss_pages: int = 0,
    start_time: int = 1,
) -> None:
    """Write the minimal stat and children files consumed by the sampler."""
    process_root = proc_root / str(pid)
    task_root = process_root / "task" / str(pid)
    task_root.mkdir(parents=True, exist_ok=True)
    fields = ["S", *("0" for _ in range(21))]
    fields[11], fields[12] = map(str, cpu_ticks)
    fields[19] = str(start_time)
    fields[21] = str(rss_pages)
    (process_root / "stat").write_text(
        f"{pid} (worker with spaces) {' '.join(fields)}\n",
        encoding="utf-8",
    )
    (task_root / "children").write_text(" ".join(map(str, children)), encoding="utf-8")


def test_procfs_stat_parser_handles_process_names_with_spaces(tmp_path: Path) -> None:
    """The stat parser indexes fields after the parenthesised process name."""
    _write_proc_process(
        tmp_path,
        100,
        cpu_ticks=(125, 75),
        rss_pages=64,
        start_time=999,
    )

    assert read_proc_process(100, 100.0, 4096, proc_root=tmp_path) == (
        999,
        2.0,
        262144,
    )


def test_procfs_sampler_aggregates_tree_and_retains_exited_child_cpu(
    tmp_path: Path,
) -> None:
    """One sample aggregates descendants and keeps final cumulative child CPU."""
    _write_proc_process(
        tmp_path,
        100,
        children=(101, 102),
        cpu_ticks=(100, 0),
        rss_pages=10,
        start_time=1000,
    )
    _write_proc_process(
        tmp_path,
        101,
        children=(103,),
        cpu_ticks=(50, 0),
        rss_pages=20,
        start_time=1001,
    )
    _write_proc_process(
        tmp_path,
        102,
        cpu_ticks=(25, 0),
        rss_pages=30,
        start_time=1002,
    )
    _write_proc_process(
        tmp_path,
        103,
        cpu_ticks=(75, 0),
        rss_pages=40,
        start_time=1003,
    )
    sampler = ProcfsSampler(0.25, proc_root=tmp_path)
    sampler.supported = True
    sampler.root_pid = 100
    sampler.clock_ticks = 100.0
    sampler.page_size = 4096

    sampler._sample()

    first = sampler.measurements()
    assert first["sample_count"] == 1
    assert first["process_tree_cpu_seconds"]["value"] == pytest.approx(2.5)
    assert first["process_tree_peak_rss_bytes"]["value"] == 100 * 4096

    (tmp_path / "101/task/101/children").write_text("", encoding="utf-8")
    _write_proc_process(
        tmp_path,
        100,
        children=(101, 102),
        cpu_ticks=(200, 0),
        rss_pages=10,
        start_time=1000,
    )
    sampler._sample()

    second = sampler.measurements()
    assert second["sample_count"] == 2
    assert second["process_tree_cpu_seconds"]["value"] == pytest.approx(3.5)
    assert second["process_tree_peak_rss_bytes"]["value"] == 100 * 4096


def test_xdist_records_assignments_busy_duration_and_finish_skew(
    tmp_path: Path,
) -> None:
    """The controller records every xdist node on its assigned worker."""
    test_file = tmp_path / "test_parallel.py"
    test_file.write_text(
        """\
import time

import pytest


@pytest.mark.parametrize("case", range(8))
def test_parallel(case):
    time.sleep(0.01)
    assert case >= 0
""",
        encoding="utf-8",
    )
    metrics_path = tmp_path / "ci-metrics-xdist.json"
    env = os.environ.copy()
    env.update({
        "PYTHONPATH": os.pathsep.join(
            part for part in (str(PROJECT_ROOT), env.get("PYTHONPATH", "")) if part
        ),
        "SUEWS_CI_METRICS": str(metrics_path),
    })

    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--confcutdir",
            str(tmp_path),
            "-p",
            "scripts.suews.pytest_ci_metrics",
            str(test_file),
            "-q",
            "-n",
            "2",
            "--dist",
            "worksteal",
        ],
        cwd=tmp_path,
        env=env,
        check=False,
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, result.stdout + result.stderr
    metrics = json.loads(metrics_path.read_text(encoding="utf-8"))
    workers = metrics["execution"]["workers"]
    assigned_node_ids = {
        node_id for worker in workers for node_id in worker["node_ids"]
    }

    assert metrics["schema_version"] == 2
    assert metrics["execution"]["xdist"] is True
    assert metrics["execution"]["effective_worker_count"] == 2
    assert {worker["worker_id"] for worker in workers} == {"gw0", "gw1"}
    assert len(assigned_node_ids) == 8
    assert all(worker["node_count"] == len(worker["node_ids"]) for worker in workers)
    assert all(
        worker["node_id_sha256"]
        == hashlib.sha256("\n".join(sorted(worker["node_ids"])).encode()).hexdigest()
        for worker in workers
    )
    assert all(worker["busy_duration_seconds"] > 0 for worker in workers)
    assert all(worker["finished_at_seconds"] >= 0 for worker in workers)
    finish_times = sorted(worker["finished_at_seconds"] for worker in workers)
    assert metrics["execution"]["worker_finish_skew_seconds"] == round(
        finish_times[-1] - finish_times[0], 6
    )
    assert metrics["execution"]["worker_tail_over_median_seconds"] == round(
        finish_times[-1]
        - (
            finish_times[len(finish_times) // 2 - 1]
            + finish_times[len(finish_times) // 2]
        )
        / 2,
        6,
    )
    assert metrics["result"]["outcomes"]["passed"] == 8
    # Per-test records cross the worker boundary as report attributes.
    records = metrics["tests"]
    assert {record["node_id"] for record in records} == assigned_node_ids
    assert all(record["outcome"] == "passed" for record in records)
    assert all("parametrize" in record["markers"] for record in records)
    assert all(record["cpu_seconds"]["total"] >= 0 for record in records)
    # time.sleep holds wall time without burning CPU: the wall column records
    # it and the CPU column does not.
    assert all(record["wall_seconds"]["call"] >= 0.01 for record in records)
    assert all(
        record["cpu_seconds"]["call"] <= record["wall_seconds"]["call"]
        for record in records
    )


def _warning_test_source(temp_root: str) -> str:
    """Build importable warning tests for paths containing Python escapes."""
    warning_a = (
        f"missing {temp_root}/tmp-alpha/forcing.csv at "
        "0x123abc for 123e4567-e89b-12d3-a456-426614174000"
    )
    warning_b = (
        f"missing {temp_root}/tmp-beta/forcing.csv at "
        "0x987def for 123e4567-e89b-12d3-a456-426614174999"
    )
    return f"""\
import warnings


def test_warning_a():
    warnings.warn(
        {warning_a!r},
        UserWarning,
    )


def test_warning_b():
    warnings.warn(
        {warning_b!r},
        UserWarning,
    )
"""


def test_warning_fingerprint_normalises_only_volatile_components(
    tmp_path: Path,
) -> None:
    """Temporary roots, UUIDs and addresses do not fragment warning groups."""
    test_file = tmp_path / "test_warnings.py"
    test_file.write_text(_warning_test_source(tempfile.gettempdir()), encoding="utf-8")
    metrics_path = tmp_path / "warning-metrics.json"
    env = os.environ.copy()
    env.update({
        "PYTHONPATH": os.pathsep.join(
            part for part in (str(PROJECT_ROOT), env.get("PYTHONPATH", "")) if part
        ),
        "SUEWS_CI_METRICS": str(metrics_path),
    })

    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--confcutdir",
            str(tmp_path),
            "-p",
            "scripts.suews.pytest_ci_metrics",
            str(test_file),
            "-q",
        ],
        cwd=tmp_path,
        env=env,
        check=False,
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, result.stdout + result.stderr
    warnings = json.loads(metrics_path.read_text(encoding="utf-8"))["warnings"]
    assert len(warnings) == 1
    assert warnings[0]["count"] == 2
    assert warnings[0]["normalised_message"] == (
        "missing <temp>/forcing.csv at 0x<address> for <uuid>"
    )
    assert "tmp-alpha" in warnings[0]["message"]


def test_schema_v2_xdist_fixture_is_a_deterministic_consumer_contract() -> None:
    """Schedulers can consume the checked-in provider fixture without live pytest."""
    metrics = json.loads(SCHEMA_V2_FIXTURE.read_text(encoding="utf-8"))
    workers = metrics["execution"]["workers"]

    assert metrics["schema_version"] == 2
    assert metrics["inventory"]["node_count"] == sum(
        worker["node_count"] for worker in workers
    )
    assert all(
        worker["node_id_sha256"]
        == hashlib.sha256("\n".join(worker["node_ids"]).encode()).hexdigest()
        for worker in workers
    )
    assert metrics["resources"]["sample_count"] > 0
