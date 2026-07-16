"""Contract tests for matched hosted scheduler comparisons."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
from statistics import median

import pytest

from scripts.suews.compare_scheduler_runs import ComparisonError, compare_abba, main

pytestmark = pytest.mark.api

SCHEMA_V2_XDIST_FIXTURE = (
    Path(__file__).parents[1] / "fixtures/ci_metrics/schema-v2-xdist.json"
)


def _metrics(
    path: Path,
    *,
    wall_seconds: float,
    finish_skew_seconds: float,
    peak_rss_bytes: int,
    node_ids: tuple[str, str] = ("test/a.py::test_a", "test/b.py::test_b"),
    skipped: int = 0,
    worker_count: int = 2,
) -> Path:
    """Write one schema-v2 metrics fixture with a complete worker inventory."""
    workers = []
    for index in range(worker_count):
        assigned = [node_ids[index]] if index < len(node_ids) else []
        workers.append({
            "worker_id": f"gw{index}",
            "node_count": len(assigned),
            "node_id_sha256": hashlib.sha256("\n".join(assigned).encode()).hexdigest(),
            "node_ids": assigned,
            "busy_duration_seconds": 8.0 - min(index, 1),
            "finished_at_seconds": 9.0 + finish_skew_seconds if index == 0 else 9.0,
        })
    payload = {
        "schema_version": 2,
        "result": {
            "exit_code": 0,
            "outcomes": {
                "passed": 2 - skipped,
                "failed": 0,
                "skipped": skipped,
                "xfailed": 0,
                "xpassed": 0,
            },
        },
        "phases": {
            "collection": {"duration_seconds": 1.0},
            "tests": {"duration_seconds": wall_seconds - 1.0},
            "session": {"duration_seconds": wall_seconds},
        },
        "inventory": {
            "node_count": len(node_ids),
            "node_id_sha256": hashlib.sha256("\n".join(node_ids).encode()).hexdigest(),
        },
        "execution": {
            "xdist": True,
            "effective_worker_count": len(workers),
            "workers": workers,
            "worker_finish_skew_seconds": finish_skew_seconds,
            "worker_tail_over_median_seconds": (
                max(worker["finished_at_seconds"] for worker in workers)
                - median(worker["finished_at_seconds"] for worker in workers)
            ),
        },
        "resources": {
            "process_tree_cpu_seconds": {
                "available": True,
                "status": "sampled",
                "value": 15.0,
                "unit": "seconds",
                "method": "linux-procfs-sampling",
                "reason": None,
            },
            "process_tree_peak_rss_bytes": {
                "available": True,
                "status": "sampled",
                "value": peak_rss_bytes,
                "unit": "bytes",
                "method": "linux-procfs-sampling",
                "reason": None,
            },
            "sample_count": 100,
            "sample_interval_seconds": 0.1,
        },
    }
    path.write_text(json.dumps(payload), encoding="utf-8")
    return path


def _memory_policy_paths(
    tmp_path: Path,
    *,
    loadscope_peak_rss_bytes: int,
    worksteal_peak_rss_bytes: int,
) -> list[Path]:
    """Create a valid ABBA set for memory policy boundary tests."""
    return [
        _metrics(
            tmp_path / "loadscope-a.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=loadscope_peak_rss_bytes,
        ),
        _metrics(
            tmp_path / "worksteal-a.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=worksteal_peak_rss_bytes,
        ),
        _metrics(
            tmp_path / "worksteal-b.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=worksteal_peak_rss_bytes,
        ),
        _metrics(
            tmp_path / "loadscope-b.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=loadscope_peak_rss_bytes,
        ),
    ]


def test_compare_abba_accepts_merged_schema_v2_provider_fixture(
    tmp_path: Path,
) -> None:
    """The comparator consumes the exact schema-v2 xdist provider contract."""
    trials = [
        ("loadscope-a", 12.0, 4.0, 300_000_000),
        ("worksteal-a", 10.0, 1.0, 290_000_000),
        ("worksteal-b", 10.2, 1.2, 295_000_000),
        ("loadscope-b", 12.2, 4.2, 305_000_000),
    ]
    paths = []
    for name, session_seconds, finish_skew_seconds, peak_rss_bytes in trials:
        payload = json.loads(SCHEMA_V2_XDIST_FIXTURE.read_text(encoding="utf-8"))
        payload["phases"]["session"]["duration_seconds"] = session_seconds
        payload["phases"]["tests"]["duration_seconds"] = session_seconds - 0.5
        payload["execution"]["workers"][0]["finished_at_seconds"] = (
            8.0 + finish_skew_seconds
        )
        payload["execution"]["workers"][1]["finished_at_seconds"] = 8.0
        payload["execution"]["worker_finish_skew_seconds"] = finish_skew_seconds
        payload["execution"]["worker_tail_over_median_seconds"] = (
            finish_skew_seconds / 2
        )
        payload["resources"]["process_tree_peak_rss_bytes"]["value"] = peak_rss_bytes
        path = tmp_path / f"{name}.json"
        path.write_text(json.dumps(payload), encoding="utf-8")
        paths.append(path)

    comparison = compare_abba(paths, runner_memory_bytes=2_000_000_000)

    provider_fixture = json.loads(SCHEMA_V2_XDIST_FIXTURE.read_text(encoding="utf-8"))
    assert comparison["selected_scheduler"] == "worksteal"
    assert comparison["invariants"]["inventory"] == provider_fixture["inventory"]
    assert comparison["invariants"]["effective_worker_count"] == 2


def test_compare_abba_selects_lower_skew_with_fixed_worker_budget(
    tmp_path: Path,
) -> None:
    """Matched ABBA trials select worksteal without adding workers."""
    paths = [
        _metrics(
            tmp_path / "loadscope-a.json",
            wall_seconds=120.0,
            finish_skew_seconds=20.0,
            peak_rss_bytes=1_000_000,
        ),
        _metrics(
            tmp_path / "worksteal-a.json",
            wall_seconds=100.0,
            finish_skew_seconds=5.0,
            peak_rss_bytes=1_020_000,
        ),
        _metrics(
            tmp_path / "worksteal-b.json",
            wall_seconds=102.0,
            finish_skew_seconds=7.0,
            peak_rss_bytes=1_030_000,
        ),
        _metrics(
            tmp_path / "loadscope-b.json",
            wall_seconds=122.0,
            finish_skew_seconds=22.0,
            peak_rss_bytes=1_010_000,
        ),
    ]

    comparison = compare_abba(paths, runner_memory_bytes=8_000_000)

    assert comparison["selected_scheduler"] == "worksteal"
    assert comparison["invariants"]["effective_worker_count"] == 2
    assert comparison["schedulers"]["loadscope"][
        "median_finish_spread_seconds"
    ] == pytest.approx(21.0)
    assert comparison["schedulers"]["worksteal"][
        "median_finish_spread_seconds"
    ] == pytest.approx(6.0)
    assert comparison["session_guardrail"][
        "maximum_regression_fraction"
    ] == pytest.approx(0.05)


def test_compare_abba_rejects_median_session_regression(tmp_path: Path) -> None:
    """A lower worker tail cannot justify a catastrophic wall-time regression."""
    paths = [
        _metrics(
            tmp_path / "loadscope-a.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "worksteal-a.json",
            wall_seconds=1_000.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "worksteal-b.json",
            wall_seconds=1_000.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "loadscope-b.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=100,
        ),
    ]

    with pytest.raises(ComparisonError, match="median session regression"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_rejects_different_node_inventory(tmp_path: Path) -> None:
    """All four trials must execute the same collected nodes."""
    paths = [
        _metrics(
            tmp_path / "a.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "b1.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
            node_ids=("test/a.py::test_a", "test/c.py::test_c"),
        ),
        _metrics(
            tmp_path / "b2.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "a2.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        ),
    ]

    with pytest.raises(ComparisonError, match="inventory mismatch"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_rejects_different_outcomes(tmp_path: Path) -> None:
    """Passing, skipped and expected-failure outcomes must all match."""
    paths = [
        _metrics(
            tmp_path / "a.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "b1.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
            skipped=1,
        ),
        _metrics(
            tmp_path / "b2.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "a2.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        ),
    ]

    with pytest.raises(ComparisonError, match="outcome mismatch"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_rejects_different_effective_worker_counts(tmp_path: Path) -> None:
    """Scheduler candidates must use the same resolved worker budget."""
    paths = [
        _metrics(
            tmp_path / "a.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "b1.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
            worker_count=3,
        ),
        _metrics(
            tmp_path / "b2.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "a2.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        ),
    ]

    with pytest.raises(ComparisonError, match="worker count mismatch"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_enforces_expected_hosted_worker_count(tmp_path: Path) -> None:
    """The hosted workflow fails closed unless all four workers resolve."""
    paths = [
        _metrics(
            tmp_path / f"run-{index}.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        )
        for index in range(4)
    ]

    with pytest.raises(ComparisonError, match="expected 4 effective workers"):
        compare_abba(paths, runner_memory_bytes=1_000, expected_worker_count=4)


def test_compare_abba_rejects_invalid_worker_assignment_hash(tmp_path: Path) -> None:
    """Each worker record must fingerprint its own assigned nodes."""
    paths = [
        _metrics(
            tmp_path / f"run-{index}.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        )
        for index in range(4)
    ]
    payload = json.loads(paths[1].read_text(encoding="utf-8"))
    payload["execution"]["workers"][0]["node_id_sha256"] = "wrong"
    paths[1].write_text(json.dumps(payload), encoding="utf-8")

    with pytest.raises(ComparisonError, match="worker gw0 assignment fingerprint"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_rejects_inconsistent_finish_skew(tmp_path: Path) -> None:
    """Reported finish skew must equal the spread of worker finish times."""
    paths = [
        _metrics(
            tmp_path / f"run-{index}.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        )
        for index in range(4)
    ]
    payload = json.loads(paths[2].read_text(encoding="utf-8"))
    payload["execution"]["worker_finish_skew_seconds"] = 99.0
    paths[2].write_text(json.dumps(payload), encoding="utf-8")

    with pytest.raises(ComparisonError, match="finish skew does not match"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_requires_complete_outcome_counts(tmp_path: Path) -> None:
    """All five terminal pytest outcome counts are required."""
    paths = [
        _metrics(
            tmp_path / f"run-{index}.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        )
        for index in range(4)
    ]
    for path in paths:
        payload = json.loads(path.read_text(encoding="utf-8"))
        del payload["result"]["outcomes"]["xfailed"]
        path.write_text(json.dumps(payload), encoding="utf-8")

    with pytest.raises(ComparisonError, match="complete outcome counts"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_rejects_inconsistent_tail_over_median(tmp_path: Path) -> None:
    """Reported tail must equal the slowest finish minus the median finish."""
    paths = [
        _metrics(
            tmp_path / f"run-{index}.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        )
        for index in range(4)
    ]
    payload = json.loads(paths[0].read_text(encoding="utf-8"))
    payload["execution"]["worker_tail_over_median_seconds"] = 99.0
    paths[0].write_text(json.dumps(payload), encoding="utf-8")

    with pytest.raises(ComparisonError, match="tail over median does not match"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_rejects_scheduler_tie(tmp_path: Path) -> None:
    """Worksteal requires a strict finish-tail improvement over loadscope."""
    paths = [
        _metrics(
            tmp_path / f"run-{index}.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        )
        for index in range(4)
    ]

    with pytest.raises(
        ComparisonError, match="strictly lower median finish spread and tail"
    ):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_rejects_peak_rss_without_runner_headroom(tmp_path: Path) -> None:
    """One memory spike fails even when the other worksteal run is small."""
    paths = [
        _metrics(
            tmp_path / "a1.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "b1.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=850,
        ),
        _metrics(
            tmp_path / "b2.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "a2.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=100,
        ),
    ]

    with pytest.raises(ComparisonError, match="memory headroom"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_peak_rss_advisory_excess_passes_with_safe_headroom(tmp_path: Path) -> None:
    """>10% relative RSS passes and warns when hosted headroom is safe."""
    paths = _memory_policy_paths(
        tmp_path,
        loadscope_peak_rss_bytes=700,
        worksteal_peak_rss_bytes=800,
    )

    comparison = compare_abba(paths, runner_memory_bytes=1_000)

    assert comparison["schema_version"] == 2
    assert comparison["policy_version"] == 2
    assert comparison["memory_guardrail"]["headroom_fraction"] == pytest.approx(0.20)
    assert comparison["memory_guardrail"]["passed"] is True
    assert comparison["peak_rss_regression_advisory"] == {
        "threshold_fraction": pytest.approx(0.10),
        "observed_fraction": pytest.approx(1 / 7),
        "exceeded": True,
    }
    assert comparison["warnings"] == [
        "worksteal maximum peak RSS regression exceeds the advisory threshold"
    ]


def test_memory_headroom_hard_gate_rejects_small_relative_delta(
    tmp_path: Path,
) -> None:
    """Below 20% hosted headroom fails despite a small relative RSS delta."""
    paths = _memory_policy_paths(
        tmp_path,
        loadscope_peak_rss_bytes=790,
        worksteal_peak_rss_bytes=801,
    )

    with pytest.raises(ComparisonError, match="memory headroom"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_peak_rss_advisory_is_clean_at_ten_percent(tmp_path: Path) -> None:
    """The relative RSS advisory is clean at its inclusive 10% boundary."""
    paths = _memory_policy_paths(
        tmp_path,
        loadscope_peak_rss_bytes=100,
        worksteal_peak_rss_bytes=110,
    )

    comparison = compare_abba(paths, runner_memory_bytes=1_000)

    assert comparison["peak_rss_regression_advisory"] == {
        "threshold_fraction": pytest.approx(0.10),
        "observed_fraction": pytest.approx(0.10),
        "exceeded": False,
    }
    assert comparison["warnings"] == []


def test_compare_abba_rejects_zero_workers(tmp_path: Path) -> None:
    """A zero-worker artefact is invalid even when it claims xdist was active."""
    paths = [
        _metrics(
            tmp_path / f"run-{index}.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        )
        for index in range(4)
    ]
    for path in paths:
        payload = json.loads(path.read_text(encoding="utf-8"))
        payload["execution"].update({
            "effective_worker_count": 0,
            "workers": [],
            "worker_finish_skew_seconds": 0.0,
            "worker_tail_over_median_seconds": 0.0,
        })
        path.write_text(json.dumps(payload), encoding="utf-8")

    with pytest.raises(
        ComparisonError, match="effective worker count must be between 1 and 4"
    ):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_rejects_nonfinite_timing(tmp_path: Path) -> None:
    """NaN timings cannot participate in a scheduler decision."""
    paths = [
        _metrics(
            tmp_path / "a1.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "b1.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "b2.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "a2.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=100,
        ),
    ]
    payload = json.loads(paths[1].read_text(encoding="utf-8"))
    payload["phases"]["session"]["duration_seconds"] = float("nan")
    paths[1].write_text(json.dumps(payload), encoding="utf-8")

    with pytest.raises(
        ComparisonError, match="session duration must be finite and non-negative"
    ):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_reports_malformed_schema_as_contract_error(
    tmp_path: Path,
) -> None:
    """Missing schema-v2 fields never escape as raw KeyError exceptions."""
    paths = [
        _metrics(
            tmp_path / f"run-{index}.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        )
        for index in range(4)
    ]
    payload = json.loads(paths[0].read_text(encoding="utf-8"))
    del payload["resources"]
    paths[0].write_text(json.dumps(payload), encoding="utf-8")

    with pytest.raises(ComparisonError, match="malformed schema-v2 metrics"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_requires_resource_samples(tmp_path: Path) -> None:
    """Peak RSS evidence must come from at least one process-tree sample."""
    paths = [
        _metrics(
            tmp_path / f"run-{index}.json",
            wall_seconds=10.0,
            finish_skew_seconds=2.0,
            peak_rss_bytes=100,
        )
        for index in range(4)
    ]
    for path in paths:
        payload = json.loads(path.read_text(encoding="utf-8"))
        payload["resources"]["sample_count"] = 0
        path.write_text(json.dumps(payload), encoding="utf-8")

    with pytest.raises(ComparisonError, match="resource sample count must be positive"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_requires_outcomes_to_cover_inventory(tmp_path: Path) -> None:
    """Four identically incomplete outcome records still fail closed."""
    paths = [
        _metrics(
            tmp_path / "a1.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "b1.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "b2.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "a2.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=100,
        ),
    ]
    for path in paths:
        payload = json.loads(path.read_text(encoding="utf-8"))
        payload["result"]["outcomes"]["passed"] = 1
        path.write_text(json.dumps(payload), encoding="utf-8")

    with pytest.raises(ComparisonError, match="outcome total does not match inventory"):
        compare_abba(paths, runner_memory_bytes=1_000)


def test_compare_abba_rejects_source_sha_mismatch(tmp_path: Path) -> None:
    """The downloaded wheel run and checked-out source must share one SHA."""
    paths = [
        _metrics(
            tmp_path / "a1.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "b1.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "b2.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=100,
        ),
        _metrics(
            tmp_path / "a2.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=100,
        ),
    ]
    provenance = {
        "source_run_id": 123,
        "source_sha": "a" * 40,
        "expected_source_sha": "b" * 40,
        "wheel_artifact_name": "cp312-manylinux-x86_64",
        "wheel_sha256": "c" * 64,
    }

    with pytest.raises(ComparisonError, match="source SHA mismatch"):
        compare_abba(paths, runner_memory_bytes=1_000, provenance=provenance)


def test_cli_writes_provenance_manifest_and_step_summary(tmp_path: Path) -> None:
    """The hosted CLI independently fingerprints its one downloaded wheel."""
    paths = [
        _metrics(
            tmp_path / "a1.json",
            wall_seconds=10.0,
            finish_skew_seconds=4.0,
            peak_rss_bytes=100,
            worker_count=4,
        ),
        _metrics(
            tmp_path / "b1.json",
            wall_seconds=9.0,
            finish_skew_seconds=1.0,
            peak_rss_bytes=112,
            worker_count=4,
        ),
        _metrics(
            tmp_path / "b2.json",
            wall_seconds=9.2,
            finish_skew_seconds=1.2,
            peak_rss_bytes=113,
            worker_count=4,
        ),
        _metrics(
            tmp_path / "a2.json",
            wall_seconds=10.2,
            finish_skew_seconds=4.2,
            peak_rss_bytes=101,
            worker_count=4,
        ),
    ]
    wheel = tmp_path / "supy.whl"
    wheel.write_bytes(b"exact wheel")
    output = tmp_path / "comparison.json"
    summary = tmp_path / "summary.md"
    source_sha = "a" * 40

    exit_code = main([
        *(str(path) for path in paths),
        "--runner-memory-bytes",
        "10000",
        "--source-run-id",
        "123",
        "--source-sha",
        source_sha,
        "--expected-source-sha",
        source_sha,
        "--wheel",
        str(wheel),
        "--max-median-session-regression-fraction",
        "0.20",
        "--peak-rss-regression-advisory-fraction",
        "0.10",
        "--min-memory-headroom-fraction",
        "0.20",
        "--output",
        str(output),
        "--summary",
        str(summary),
    ])

    assert exit_code == 0
    manifest = json.loads(output.read_text(encoding="utf-8"))
    assert manifest["schema_version"] == 2
    assert manifest["policy_version"] == 2
    assert manifest["status"] == "passed"
    assert manifest["session_guardrail"][
        "maximum_regression_fraction"
    ] == pytest.approx(0.20)
    assert (
        manifest["provenance"]["wheel_sha256"]
        == hashlib.sha256(b"exact wheel").hexdigest()
    )
    assert set(manifest["memory_guardrail"]) == {
        "runner_capacity_bytes",
        "highest_peak_rss_bytes",
        "headroom_bytes",
        "headroom_fraction",
        "minimum_headroom_fraction",
        "passed",
    }
    assert manifest["peak_rss_regression_advisory"] == {
        "threshold_fraction": pytest.approx(0.10),
        "observed_fraction": pytest.approx(12 / 101),
        "exceeded": True,
    }
    assert manifest["warnings"]
    summary_text = summary.read_text(encoding="utf-8")
    assert "worksteal" in summary_text
    assert "Warning: worksteal maximum peak RSS regression" in summary_text
    assert "hard memory gate is runner headroom" in summary_text
