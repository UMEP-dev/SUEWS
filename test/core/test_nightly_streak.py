"""Contract tests for the nightly scheduled-run streak view."""

from __future__ import annotations

import pytest

from scripts.suews.nightly_streak import (
    MAX_LISTED_FAILURES,
    _failure_lines,
    summarise_run,
    summarise_runs,
)

pytestmark = pytest.mark.api


def _run(run_id: int, conclusion: str, number: int = 1) -> dict:
    """Build a minimal workflow-run payload."""
    return {
        "id": run_id,
        "run_number": number,
        "created_at": "2026-09-10T02:00:00Z",
        "head_sha": "abcdef1234567890",
        "conclusion": conclusion,
        "html_url": f"https://example.invalid/runs/{run_id}",
    }


def _job(name: str, conclusion: str) -> dict:
    """Build a minimal job payload."""
    return {"name": name, "conclusion": conclusion}


@pytest.mark.core
def test_green_run_with_no_failing_job_keeps_the_alert_silent() -> None:
    """A successful run without job failures does not expect an alert."""
    record = summarise_run(
        _run(1, "success"),
        [_job("Build standard wheels / cp312-manylinux x86_64", "success")],
    )

    assert record["alert_expected"] is False
    assert record["gating_failures"] == []


@pytest.mark.core
def test_recording_job_failure_does_not_break_the_streak() -> None:
    """The continue-on-error recording jobs are reported, not counted as red."""
    record = summarise_run(
        _run(2, "success"),
        [
            _job("Tolerance spread cp312-manylinux x86_64", "failure"),
            _job("Cost-marker drift (Linux cp312 CPU seconds)", "failure"),
        ],
    )

    assert record["alert_expected"] is False
    assert record["gating_failures"] == []
    assert record["recording_failures"] == [
        "Cost-marker drift (Linux cp312 CPU seconds)",
        "Tolerance spread cp312-manylinux x86_64",
    ]


@pytest.mark.core
def test_gating_job_failure_expects_the_alert() -> None:
    """A failing test tier is a gating failure and names the job."""
    record = summarise_run(
        _run(3, "failure"),
        [_job("Build checked wheels (nightly physics tier) / cp313-win", "failure")],
    )

    assert record["alert_expected"] is True
    assert record["gating_failures"] == [
        "Build checked wheels (nightly physics tier) / cp313-win"
    ]


@pytest.mark.core
def test_run_that_built_nothing_is_not_green() -> None:
    """An upstream failure that skips every build still expects the alert."""
    record = summarise_run(
        _run(4, "failure"), [_job("Determine build matrix", "failure")]
    )

    assert record["alert_expected"] is True


@pytest.mark.core
def test_streak_counts_only_the_leading_green_runs() -> None:
    """The streak stops at the newest run that expected an alert."""
    runs = [_run(10, "success", 3), _run(9, "success", 2), _run(8, "failure", 1)]
    jobs = {
        10: [_job("Build standard wheels / cp312", "success")],
        9: [_job("Tolerance spread cp312-manylinux x86_64", "failure")],
        8: [_job("API cross-CPython tests / cp314-win", "failure")],
    }

    summary = summarise_runs(runs, jobs)

    assert summary["runs_examined"] == 3
    assert summary["green_streak"] == 2
    assert summary["runs"][2]["gating_failures"] == [
        "API cross-CPython tests / cp314-win"
    ]


@pytest.mark.core
def test_whole_matrix_failure_is_truncated_with_a_count() -> None:
    """A long failing-job list is capped so the streak stays readable."""
    names = [f"API cross-CPython tests / cell {index}" for index in range(12)]

    lines = _failure_lines(names, "failing")

    assert len(lines) == MAX_LISTED_FAILURES + 1
    assert lines[-1].strip() == f"... and {12 - MAX_LISTED_FAILURES} more"
    assert _failure_lines([], "failing") == []
