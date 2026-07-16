"""Contracts for wheel-job phase evidence and cibuildwheel artefact transport."""

from __future__ import annotations

from pathlib import Path
import re

import pytest

from scripts.suews.ci_phase_metrics import (
    append_step_summary,
    finalise_evidence,
    read_pytest_metrics,
    start_phase,
    stop_phase,
)
from scripts.suews.run_ci_tests import metrics_paths

pytestmark = pytest.mark.api

PROJECT_ROOT = Path(__file__).resolve().parents[2]
REQUIRED_PHASES = ("checkout", "toolchain_setup", "build", "repair", "install")


@pytest.mark.core
def test_wheel_evidence_contains_explicit_build_and_pytest_phases(
    tmp_path: Path,
) -> None:
    """A completed wheel job publishes all required phase records in one artefact."""
    state_path = tmp_path / "phases.json"
    boundaries = {
        "checkout": (10.0, 14.0),
        "toolchain_setup": (15.0, 20.0),
        "build": (20.0, 30.0),
        "repair": (30.0, 32.0),
        "install": (32.0, 35.0),
    }
    for phase, (started, completed) in boundaries.items():
        start_phase(state_path, phase, now=started, source="test-boundary")
        stop_phase(state_path, phase, now=completed)
    pytest_metrics = {
        "schema_version": 2,
        "phases": {
            "collection": {"duration_seconds": 1.25},
            "session": {"duration_seconds": 8.0},
            "tests": {"duration_seconds": 6.5},
        },
        "inventory": {"node_count": 8, "node_id_sha256": "abc"},
        "result": {"exit_code": 0, "outcomes": {"passed": 8}},
    }

    evidence = finalise_evidence(state_path, pytest_metrics, job_name="physics-cp312")

    assert evidence["schema_version"] == 1
    assert evidence["kind"] == "wheel-job-ci-metrics"
    assert evidence["job_name"] == "physics-cp312"
    for phase in REQUIRED_PHASES:
        assert evidence["phases"][phase] == {
            "available": True,
            "duration_seconds": boundaries[phase][1] - boundaries[phase][0],
            "reason": None,
            "source": "test-boundary",
            "status": "measured",
        }
    assert evidence["phases"]["collection"]["source"] == "pytest-hook"
    assert evidence["phases"]["tests"]["duration_seconds"] == pytest.approx(6.5)
    assert evidence["pytest_metrics"] == pytest_metrics


@pytest.mark.core
def test_missing_wheel_phase_is_explicitly_unavailable(tmp_path: Path) -> None:
    """An absent hook can never be mistaken for a measured zero duration."""
    state_path = tmp_path / "phases.json"
    start_phase(state_path, "checkout", now=1.0, source="test-boundary")
    stop_phase(state_path, "checkout", now=2.0)

    evidence = finalise_evidence(
        state_path,
        {
            "schema_version": 2,
            "phases": {
                "collection": {"duration_seconds": 0.1},
                "session": {"duration_seconds": 0.3},
                "tests": {"duration_seconds": 0.2},
            },
        },
        job_name="incomplete",
    )

    assert evidence["phases"]["repair"] == {
        "available": False,
        "duration_seconds": None,
        "reason": "No completed repair boundary was recorded.",
        "source": None,
        "status": "unavailable",
    }


@pytest.mark.core
def test_pretest_failure_still_produces_explicit_pytest_unavailability(
    tmp_path: Path,
) -> None:
    """Missing pytest JSON remains publishable evidence after an earlier failure."""
    metrics = read_pytest_metrics(tmp_path / "missing-pytest.json")

    assert metrics["available"] is False
    assert metrics["schema_version"] is None
    assert metrics["result"] is None
    assert "before pytest metrics" in metrics["reason"]


@pytest.mark.core
def test_host_finaliser_appends_combined_wheel_job_summary(tmp_path: Path) -> None:
    """The host summary covers build, test, resource and inventory evidence."""
    state_path = tmp_path / "phases.json"
    for phase, started in zip(REQUIRED_PHASES, range(5), strict=True):
        start_phase(state_path, phase, now=float(started), source="test-boundary")
        stop_phase(state_path, phase, now=float(started + 1))
    evidence = finalise_evidence(
        state_path,
        {
            "schema_version": 2,
            "phases": {
                "collection": {"duration_seconds": 0.5},
                "session": {"duration_seconds": 9.0},
                "tests": {"duration_seconds": 8.25},
            },
            "inventory": {"node_count": 4, "node_id_sha256": "abc123"},
            "resources": {
                "process_tree_cpu_seconds": {
                    "available": True,
                    "status": "sampled",
                    "value": 12.5,
                },
                "process_tree_peak_rss_bytes": {
                    "available": True,
                    "status": "sampled",
                    "value": 268435456,
                },
            },
        },
        job_name="physics-cp312",
    )
    summary_path = tmp_path / "step-summary.md"

    append_step_summary(summary_path, evidence)

    summary = summary_path.read_text(encoding="utf-8")
    assert "Wheel job CI metrics: physics-cp312" in summary
    for phase in (*REQUIRED_PHASES, "collection", "tests"):
        assert phase.replace("_", " ").title() in summary
    assert "Process-tree CPU | 12.500 s" in summary
    assert "Process-tree peak RSS | 256.0 MiB" in summary
    assert "Collected tests | 4" in summary
    assert "Coverage fingerprint: `abc123`" in summary


@pytest.mark.core
def test_physics_metrics_path_and_linux_transport_are_stable(tmp_path: Path) -> None:
    """The wrapper path matches the host-mounted directory uploaded by the action."""
    metrics_path, phases_path = metrics_paths(
        PROJECT_ROOT,
        {
            "SUEWS_CI_METRICS_DIR": str(tmp_path),
            "SUEWS_CI_METRICS_NAME": "physics-cp312-manylinux-x86_64",
            "SUEWS_CI_PHASES": str(
                tmp_path / "physics-cp312-manylinux-x86_64-phases.json"
            ),
        },
    )
    action = (PROJECT_ROOT / ".github/actions/build-suews/action.yml").read_text(
        encoding="utf-8"
    )

    assert metrics_path == tmp_path / "physics-cp312-manylinux-x86_64-pytest.json"
    assert phases_path == tmp_path / "physics-cp312-manylinux-x86_64-phases.json"
    assert "${{ inputs.metrics_host_dir }}:/ci-metrics" in action
    assert "SUEWS_CI_METRICS_DIR=/ci-metrics" in action
    assert "ci-metrics-physics-${{ inputs.python }}" in action
    assert "METRICS_HOST_DIR: ${{ inputs.metrics_host_dir }}" in action
    assert (
        'mkdir -p .cargo-cache .rust-target-cache .pip-cache "$METRICS_HOST_DIR"'
        in action
    )
    assert "PHYSICS_JOB_NAME: physics-${{ inputs.python }}" in action
    assert '--job-name "$PHYSICS_JOB_NAME"' in action
    assert "CIBW_TEST_ENVIRONMENT" not in action
    assert "start toolchain_setup" in action
    assert "start build" in action
    assert "transition build repair" in action
    assert "start install" in action
    assert "{project}/scripts/suews/ci_phase_metrics.py" not in action
    assert action.count("if: always()") >= 2


@pytest.mark.core
def test_every_local_build_action_caller_passes_metrics_host_dir() -> None:
    """Adding metrics transport cannot break a less common workflow caller."""
    callers = []
    for workflow in (PROJECT_ROOT / ".github/workflows").glob("*.yml"):
        lines = workflow.read_text(encoding="utf-8").splitlines()
        for index, line in enumerate(lines):
            if "uses: ./.github/actions/build-suews" in line:
                callers.append((workflow.name, index + 1))
                nearby = "\n".join(lines[index + 1 : index + 16])
                assert "metrics_host_dir:" in nearby, (
                    f"{workflow.name}:{index + 1} must pass metrics_host_dir"
                )
    assert callers


@pytest.mark.smoke
def test_wheel_checkout_skips_empty_submodules_but_keeps_version_history() -> None:
    """Wheel checkout avoids empty submodules without weakening git describe."""
    workflow = (PROJECT_ROOT / ".github/workflows/build-wheels-reusable.yml").read_text(
        encoding="utf-8"
    )
    checkout = re.search(
        r"uses: actions/checkout@[^\n]+\n(?P<inputs>(?:[ \t]+[^\n]+\n){1,8})",
        workflow,
    )

    assert checkout is not None
    checkout_inputs = checkout.group("inputs")
    assert "fetch-depth: 0" in checkout_inputs
    assert "persist-credentials: false" in checkout_inputs
    assert "submodules:" not in checkout_inputs
    assert not (PROJECT_ROOT / ".gitmodules").read_text(encoding="utf-8").strip()
