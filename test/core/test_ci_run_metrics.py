"""Contract tests for workflow timing analysis and overhead calculations."""

from __future__ import annotations

from datetime import UTC, datetime
from pathlib import Path
import re

import pytest

from scripts.suews.analyse_ci_run import analyse_run
from scripts.suews.benchmark_ci_metrics import (
    compute_overhead,
    file_sha256,
    validate_command,
)

pytestmark = pytest.mark.api


@pytest.mark.core
def test_analysis_separates_dependency_queue_and_execution_time() -> None:
    """Workflow jobs retain distinct readiness, queue and execution durations."""
    run = {
        "id": 42,
        "created_at": "2026-07-15T00:00:00Z",
        "updated_at": "2026-07-15T00:01:54Z",
        "event": "pull_request",
        "head_sha": "abc123",
    }
    jobs = {
        "jobs": [
            _job("Detect", 0, 2, 10),
            _job("Build", 10, 15, 75, steps=[("Checkout", 15, 20), ("Build", 20, 75)]),
            _job("API", 75, 78, 108),
            _job("Gate", 108, 109, 114),
        ]
    }

    metrics = analyse_run(
        run,
        jobs,
        declared_needs={
            "Build": ["Detect"],
            "API": ["Build"],
            "Gate": ["API"],
        },
        target_job_pattern="Gate",
    )

    assert metrics["schema_version"] == 1
    assert metrics["workflow"]["elapsed_seconds"] == pytest.approx(114.0)
    assert metrics["observed_critical_path"]["job_names"] == [
        "Detect",
        "Build",
        "API",
        "Gate",
    ]
    assert metrics["observed_critical_path"][
        "orchestration_delay_seconds"
    ] == pytest.approx(0.0)
    assert metrics["observed_critical_path"]["runner_queue_seconds"] == pytest.approx(
        11.0
    )
    assert metrics["observed_critical_path"]["execution_seconds"] == pytest.approx(
        103.0
    )
    by_name = {job["name"]: job for job in metrics["jobs"]}
    assert by_name["Build"]["orchestration_delay_seconds"] == pytest.approx(0.0)
    assert by_name["Build"]["ready_offset_seconds"] == pytest.approx(10.0)
    assert by_name["Build"]["fan_in_spread_seconds"] == pytest.approx(0.0)
    assert by_name["Build"]["runner_queue_seconds"] == pytest.approx(5.0)
    assert by_name["Build"]["execution_seconds"] == pytest.approx(60.0)
    assert by_name["Build"]["steps"] == [
        {"duration_seconds": 5.0, "name": "Checkout"},
        {"duration_seconds": 55.0, "name": "Build"},
    ]


@pytest.mark.core
def test_analysis_follows_dependency_fan_in_and_ignores_later_non_gate_job() -> None:
    """The latest declared predecessor, not a later summary, sets the gate path."""
    run = {
        "id": 43,
        "created_at": "2026-07-15T00:00:00Z",
        "updated_at": "2026-07-15T00:01:20Z",
        "event": "pull_request",
        "head_sha": "def456",
    }
    jobs = {
        "jobs": [
            _job("Build / linux", 0, 0, 20),
            _job("Build / windows", 0, 1, 40),
            _job("Gate", 42, 45, 55),
            _job("Later diagnostics", 55, 56, 80),
        ]
    }

    metrics = analyse_run(
        run,
        jobs,
        declared_needs={"Gate": ["Build / *"]},
        target_job_pattern="Gate",
    )

    gate = {job["name"]: job for job in metrics["jobs"]}["Gate"]
    assert gate["ready_offset_seconds"] == pytest.approx(40.0)
    assert gate["orchestration_delay_seconds"] == pytest.approx(2.0)
    assert gate["fan_in_spread_seconds"] == pytest.approx(20.0)
    assert gate["critical_predecessor"] == "Build / windows"
    assert metrics["workflow"]["gate_completed_at"] == "2026-07-15T00:00:55Z"
    assert metrics["observed_critical_path"]["job_names"] == [
        "Build / windows",
        "Gate",
    ]


@pytest.mark.core
def test_overhead_uses_medians_and_reports_percentage() -> None:
    """Alternating trials are reduced to a stable median overhead result."""
    result = compute_overhead(
        baseline_seconds=[10.0, 10.2, 9.8],
        instrumented_seconds=[10.1, 10.3, 9.9],
        threshold_percent=2.0,
    )

    assert result == {
        "baseline_median_seconds": 10.0,
        "instrumented_median_seconds": 10.1,
        "overhead_percent": pytest.approx(1.0),
        "passed": True,
        "threshold_percent": 2.0,
    }


@pytest.mark.core
def test_overhead_command_must_enact_declared_fixed_scheduler() -> None:
    """Harness metadata cannot claim a scheduler the pytest command does not use."""
    command = [
        "python",
        "-m",
        "pytest",
        "test/core/test_ehc_regression.py",
        "-n",
        "4",
        "--maxprocesses=4",
        "--dist",
        "worksteal",
    ]

    validate_command(command, worker_count=4, scheduler="worksteal")
    with pytest.raises(ValueError, match="--dist"):
        validate_command(command, worker_count=4, scheduler="loadscope")
    with pytest.raises(ValueError, match="between 1 and 4"):
        validate_command(command, worker_count=5, scheduler="worksteal")


@pytest.mark.core
def test_wheel_provenance_hashes_exact_download(tmp_path: Path) -> None:
    """The comparison manifest can identify the exact installed wheel bytes."""
    wheel = tmp_path / "supy.whl"
    wheel.write_bytes(b"controlled-wheel")

    assert file_sha256(wheel) == (
        "3a5e31bfeb5d9c690917610ae054fb563fb0c896704bf93dba8c7ea0202f74d1"
    )


@pytest.mark.core
def test_manual_overhead_workflow_uses_realistic_fixed_capacity_contract() -> None:
    """Post-merge overhead evidence uses the full standard physics workload."""
    workflow = (
        Path(__file__).resolve().parents[2]
        / ".github/workflows/ci-metrics-overhead.yml"
    ).read_text(encoding="utf-8")

    assert "timeout-minutes: 90" in workflow
    assert "-- python -m pytest test" in workflow
    assert '-m "physics and not slow"' in workflow
    assert "-n 4 --maxprocesses=4 --dist worksteal" in workflow
    assert '--source-sha "${{ github.sha }}"' in workflow
    assert workflow.count("CONTROLLED_WHEEL: ${{ steps.wheel.outputs.path }}") == 2
    assert 'python -m pip install "$CONTROLLED_WHEEL"' in workflow
    assert '--wheel "$CONTROLLED_WHEEL"' in workflow


@pytest.mark.core
def test_new_ci_observability_surfaces_trigger_normal_ci() -> None:
    """Metrics workflow/config-only changes remain inside the positive CI filter."""
    path_filters = (
        Path(__file__).resolve().parents[2] / ".github/path-filters.yml"
    ).read_text(encoding="utf-8")

    assert "- '.github/workflows/ci-metrics-overhead.yml'" in path_filters
    assert "- '.github/workflows/build-wheels-reusable.yml'" in path_filters
    assert "- '.github/ci-metrics-needs.json'" in path_filters


@pytest.mark.core
def test_api_lane_installs_xdist_contract_without_parallelising_main_suite() -> None:
    """The nested xdist contract has its plugin while API tests stay serial."""
    workflow = (
        Path(__file__).resolve().parents[2]
        / ".github/workflows/test-api-cross-python-reusable.yml"
    ).read_text(encoding="utf-8")

    assert "python -m pip install pytest==9.1.1 pytest-xdist==3.8.0" in workflow
    main_invocation = re.search(
        r"^[ \t]*python -m pytest -p scripts\.suews\.pytest_ci_metrics test \\\n"
        r"[ \t]+-m \"\$MARKER_EXPR\" -v --tb=short --durations=25[ \t]*$",
        workflow,
        flags=re.MULTILINE,
    )
    assert main_invocation is not None
    assert re.search(r"(?:^|\s)-n(?:\s|$)", main_invocation.group()) is None


def _job(
    name: str,
    created: int,
    started: int,
    completed: int,
    *,
    steps: list[tuple[str, int, int]] | None = None,
) -> dict[str, object]:
    """Build a minimal GitHub Actions job payload relative to midnight."""
    return {
        "name": name,
        "created_at": _timestamp(created),
        "started_at": _timestamp(started),
        "completed_at": _timestamp(completed),
        "status": "completed",
        "conclusion": "success",
        "steps": [
            {
                "name": step_name,
                "started_at": _timestamp(step_started),
                "completed_at": _timestamp(step_completed),
            }
            for step_name, step_started, step_completed in (steps or [])
        ],
    }


def _timestamp(offset_seconds: int) -> str:
    """Return an ISO timestamp at the requested midnight offset."""
    timestamp = datetime(2026, 7, 15, tzinfo=UTC).timestamp() + offset_seconds
    return datetime.fromtimestamp(timestamp, tz=UTC).isoformat().replace("+00:00", "Z")
