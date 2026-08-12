"""Contract tests for workflow timing analysis and overhead calculations."""

from __future__ import annotations

from datetime import UTC, datetime
from fnmatch import fnmatchcase
from pathlib import Path
import re
import zipfile

import pytest
import yaml


pytestmark = pytest.mark.api


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


@pytest.mark.core
def test_publish_jobs_download_only_cpython_wheel_artifacts() -> None:
    """PyPI publishers must not merge metrics or MCP files into ``dist``."""
    workflow_path = (
        Path(__file__).resolve().parents[2]
        / ".github/workflows/build-publish_to_pypi.yml"
    )
    workflow = yaml.safe_load(workflow_path.read_text(encoding="utf-8"))
    wheel_pattern = "cp[0-9][0-9][0-9]-*"

    for job_name in ("deploy_testpypi", "deploy_pypi"):
        steps = workflow["jobs"][job_name]["steps"]
        download = next(
            step
            for step in steps
            if str(step.get("uses", "")).startswith("actions/download-artifact@")
        )
        assert download["with"]["pattern"] == wheel_pattern
        assert download["with"]["merge-multiple"] is True

    assert fnmatchcase("cp312-manylinux-x86_64", wheel_pattern)
    assert fnmatchcase("cp314-macosx-arm64", wheel_pattern)
    assert not fnmatchcase("ci-metrics-api-cp312-manylinux-x86_64", wheel_pattern)
    assert not fnmatchcase("ci-metrics-physics-cp312-macosx-arm64", wheel_pattern)
    assert not fnmatchcase("suews-mcp-dist", wheel_pattern)


def _write_test_wheel(path: Path) -> Path:
    """Create a minimal deterministic wheel archive for provenance contracts."""
    with zipfile.ZipFile(path, "w") as archive:
        archive.writestr(
            "supy-2026.7.16.dist-info/METADATA",
            "Metadata-Version: 2.4\nName: supy\nVersion: 2026.7.16.dev3\n",
        )
        archive.writestr(
            "supy-2026.7.16.dist-info/WHEEL",
            "Wheel-Version: 1.0\nTag: cp312-abi3-win_amd64\n",
        )
        archive.writestr(
            "supy/_version_scm.py",
            "__version__ = '2026.7.16.dev3'\n__commit_hash__ = 'aaaaaaa'\n",
        )
    return path


def _measured_wheel_metrics() -> dict[str, object]:
    phases = {
        phase: {"available": True, "status": "measured", "duration_seconds": 1.0}
        for phase in (
            "checkout",
            "toolchain_setup",
            "build",
            "repair",
            "install",
            "tests",
        )
    }
    return {
        "schema_version": 1,
        "kind": "wheel-job-ci-metrics",
        "job_name": "physics-cp312-win-AMD64",
        "phases": phases,
        "pytest_metrics": {
            "result": {"exit_code": 0},
            "inventory": {"node_id_sha256": "f" * 64},
        },
    }


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


@pytest.mark.smoke
def test_checkout_abba_workflow_is_manual_sequential_and_non_production() -> None:
    """The blob-filter experiment stays on one Windows runner and out of CI."""
    root = Path(__file__).resolve().parents[2]
    workflow = (root / ".github/workflows/benchmark-checkout-provenance.yml").read_text(
        encoding="utf-8"
    )
    path_filters = (root / ".github/path-filters.yml").read_text(encoding="utf-8")

    assert "workflow_dispatch:" in workflow
    assert "pull_request:" not in workflow
    assert "push:" not in workflow
    assert workflow.count("runs-on: windows-2025") == 1
    assert "A1 - full checkout" in workflow
    assert "B1 - blob:none checkout" in workflow
    assert "B2 - blob:none checkout" in workflow
    assert "A2 - full checkout" in workflow
    assert (
        workflow.index("A1 - full checkout")
        < workflow.index("B1 - blob:none checkout")
        < workflow.index("B2 - blob:none checkout")
        < workflow.index("A2 - full checkout")
    )
    assert workflow.count("filter: blob:none") == 3  # B1, B2, final build source
    assert "fetch-depth: 0" in workflow
    assert "expected_sha must equal the dispatched default-branch SHA" in workflow
    assert "test_tier: smoke" in workflow
    assert "uses: ./.github/actions/build-suews" in workflow
    assert "- '.github/workflows/benchmark-checkout-provenance.yml'" in path_filters
