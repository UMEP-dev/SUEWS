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
