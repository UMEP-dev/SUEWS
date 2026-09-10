"""Contract tests for the serial/parallel ABBA trial summariser."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from scripts.suews.summarise_abba_trials import (
    build_summary,
    main,
    render_markdown,
    runner_memory_bytes,
    summarise_trial,
)

pytestmark = pytest.mark.api

PROJECT_ROOT = Path(__file__).resolve().parents[2]
SCHEMA_V2_FIXTURE = PROJECT_ROOT / "test/fixtures/ci_metrics/schema-v2-xdist.json"


def _measurement(value: int | None, method: str | None = "getrusage-ru-maxrss") -> dict:
    available = value is not None
    return {
        "available": available,
        "method": method if available else None,
        "reason": None if available else "not sampled",
        "status": "sampled" if available else "unavailable",
        "unit": "bytes",
        "value": value,
    }


def _serial_metrics(session_seconds: float, controller_peak: int) -> dict:
    """A serial run has no worker records and no process-tree sample off Linux."""
    fixture = json.loads(SCHEMA_V2_FIXTURE.read_text(encoding="utf-8"))
    return {
        "schema_version": 2,
        "environment": {"runner_os": "Windows", "python": "3.12.0"},
        "result": fixture["result"],
        "inventory": fixture["inventory"],
        "phases": {
            "collection": {"duration_seconds": 1.0},
            "session": {"duration_seconds": session_seconds},
            "tests": {"duration_seconds": session_seconds - 1.0},
        },
        "execution": {
            "effective_worker_count": 1,
            "worker_finish_skew_seconds": 0.0,
            "worker_tail_over_median_seconds": 0.0,
            "workers": [],
            "xdist": False,
        },
        "resources": {
            "sample_count": 0,
            "sample_interval_seconds": 0.25,
            "process_tree_cpu_seconds": _measurement(None),
            "process_tree_peak_rss_bytes": _measurement(None),
            "controller_peak_rss_bytes": _measurement(controller_peak),
        },
        "warnings": [],
    }


def _parallel_metrics(session_seconds: float) -> dict:
    metrics = json.loads(SCHEMA_V2_FIXTURE.read_text(encoding="utf-8"))
    metrics["phases"]["session"]["duration_seconds"] = session_seconds
    return metrics


def _write(path: Path, payload: dict) -> Path:
    path.write_text(json.dumps(payload), encoding="utf-8")
    return path


def test_summary_tabulates_abba_order_and_derives_speedup(tmp_path: Path) -> None:
    """Serial and parallel trials keep their order; medians give the speedup."""
    fixture = json.loads(SCHEMA_V2_FIXTURE.read_text(encoding="utf-8"))
    worker_peaks = [
        worker["peak_rss_bytes"]["value"] for worker in fixture["execution"]["workers"]
    ]
    trials = [
        summarise_trial(
            "S1", _write(tmp_path / "s1.json", _serial_metrics(600.0, 900 * 2**20))
        ),
        summarise_trial("P1", _write(tmp_path / "p1.json", _parallel_metrics(200.0))),
        summarise_trial("P2", _write(tmp_path / "p2.json", _parallel_metrics(220.0))),
        summarise_trial(
            "S2", _write(tmp_path / "s2.json", _serial_metrics(640.0, 950 * 2**20))
        ),
    ]

    summary = build_summary(trials, title="API lane", memory_bytes=16 * 2**30)

    assert [trial["label"] for trial in summary["trials"]] == ["S1", "P1", "P2", "S2"]
    assert summary["missing_trials"] == []
    assert summary["same_inventory"] is True
    assert summary["median_serial_session_seconds"] == pytest.approx(620.0)
    assert summary["median_parallel_session_seconds"] == pytest.approx(210.0)
    assert summary["speedup"] == pytest.approx(620.0 / 210.0)
    assert summary["max_worker_peak_rss_bytes"] == max(worker_peaks)
    assert trials[1]["sum_worker_peak_rss_bytes"] == sum(worker_peaks)
    assert trials[1]["workers_reporting_peak"] == len(worker_peaks)
    assert trials[0]["process_tree_peak_rss_bytes"] is None
    assert trials[0]["controller_peak_rss_bytes"] == 900 * 2**20
    tree_peak = fixture["resources"]["process_tree_peak_rss_bytes"]["value"]
    assert summary["max_process_tree_peak_rss_bytes"] == tree_peak
    assert summary["process_tree_memory_headroom_fraction"] == pytest.approx(
        1.0 - tree_peak / (16 * 2**30)
    )

    markdown = render_markdown(summary)
    assert "| S1 | serial | 0 |" in markdown
    assert f"| P1 | {len(worker_peaks)}/{len(worker_peaks)} | 0 |" in markdown
    assert "speedup 2.95x" in markdown
    assert "Max single-worker peak RSS across parallel trials" in markdown


def test_missing_artefact_is_reported_not_raised(tmp_path: Path) -> None:
    """A trial killed before pytest finished leaves a visible gap in the table."""
    trials = [
        summarise_trial(
            "S1", _write(tmp_path / "s1.json", _serial_metrics(600.0, 2**30))
        ),
        summarise_trial("P1", tmp_path / "absent.json"),
    ]

    summary = build_summary(trials, title="API lane", memory_bytes=None)
    markdown = render_markdown(summary)

    assert summary["missing_trials"] == ["P1"]
    assert summary["speedup"] is None
    assert "| P1 | missing |" in markdown
    assert "Missing artefacts" in markdown
    assert "Runner memory: unknown" in markdown


def test_cli_writes_json_record_and_appends_summary(tmp_path: Path) -> None:
    """The CLI emits the JSON record and appends the Markdown to a summary file."""
    _write(tmp_path / "s1.json", _serial_metrics(600.0, 2**30))
    _write(tmp_path / "p1.json", _parallel_metrics(200.0))
    output = tmp_path / "out" / "summary.json"
    summary_md = tmp_path / "step-summary.md"

    exit_code = main([
        "--trial",
        f"S1={tmp_path / 's1.json'}",
        "--trial",
        f"P1={tmp_path / 'p1.json'}",
        "--title",
        "API lane ubuntu-latest",
        "--runner-memory-bytes",
        str(16 * 2**30),
        "--output",
        str(output),
        "--summary",
        str(summary_md),
    ])

    assert exit_code == 0
    record = json.loads(output.read_text(encoding="utf-8"))
    assert record["schema_version"] == 1
    assert record["runner_memory_bytes"] == 16 * 2**30
    assert [trial["label"] for trial in record["trials"]] == ["S1", "P1"]
    assert summary_md.read_text(encoding="utf-8").startswith(
        "## API lane ubuntu-latest"
    )


def test_cli_rejects_malformed_trial_argument(tmp_path: Path, capsys) -> None:
    """A trial without LABEL=PATH is an argument error, not a traceback."""
    assert main(["--trial", "no-separator"]) == 2
    assert "LABEL=PATH" in capsys.readouterr().err


def test_runner_memory_is_known_on_ci_platforms() -> None:
    """Linux, macOS and Windows all report total physical memory."""
    memory = runner_memory_bytes()

    assert isinstance(memory, int)
    assert memory >= 2**30
