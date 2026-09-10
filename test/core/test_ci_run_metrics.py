"""Contract tests for workflow timing analysis and overhead calculations."""

from __future__ import annotations

from datetime import UTC, datetime
from fnmatch import fnmatchcase
import json
from pathlib import Path
import re

import pytest
import yaml

from scripts.suews.analyse_ci_run import analyse_run

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
def test_new_ci_observability_surfaces_trigger_normal_ci() -> None:
    """Metrics workflow/config-only changes remain inside the positive CI filter."""
    path_filters = (
        Path(__file__).resolve().parents[2] / ".github/path-filters.yml"
    ).read_text(encoding="utf-8")

    assert "- '.github/workflows/ci-metrics-overhead.yml'" in path_filters
    assert "- '.github/workflows/build-wheels-reusable.yml'" in path_filters
    assert "- '.github/workflows/test-api-cross-python-reusable.yml'" in path_filters
    assert "- '.github/workflows/tolerance-spread-reusable.yml'" in path_filters
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


@pytest.mark.smoke
def test_api_lane_consumes_mcp_artifact_after_build() -> None:
    """The API matrix installs the MCP wheel only after both builds succeed."""
    root = Path(__file__).resolve().parents[2]
    caller = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    api_job = caller["jobs"]["test_api_cross_python"]
    assert {"determine_matrix", "build_wheels", "build_mcp"} <= set(api_job["needs"])
    assert "needs.build_mcp.result == 'success'" in api_job["if"]

    api_workflow = (
        root / ".github/workflows/test-api-cross-python-reusable.yml"
    ).read_text(encoding="utf-8")
    assert "name: suews-mcp-dist" in api_workflow
    assert "path: mcp-dist/" in api_workflow
    assert "python -m pip install wheelhouse/*.whl mcp-dist/*.whl" in api_workflow

    declared_needs = json.loads(
        (root / ".github/ci-metrics-needs.json").read_text(encoding="utf-8")
    )
    assert "Build MCP package" in declared_needs["API cross-CPython tests / *"]


@pytest.mark.smoke
def test_api_lane_requires_nonempty_mcp_protocol_collection() -> None:
    """Missing SDK, executable or protocol nodes cannot silently pass CI."""
    root = Path(__file__).resolve().parents[2]
    api_workflow = (
        root / ".github/workflows/test-api-cross-python-reusable.yml"
    ).read_text(encoding="utf-8")

    for required in (
        "from mcp.client.session import ClientSession",
        "from mcp.client.stdio import StdioServerParameters, stdio_client",
        "import sysconfig",
        'active_bin_dir = Path(sysconfig.get_path("scripts"))',
        'shutil.which("suews-mcp", path=str(active_bin_dir))',
        "python -m pytest --collect-only test/mcp/test_protocol_handshake.py",
        '-m "$MARKER_EXPR" -q',
    ):
        assert required in api_workflow

    protocol_test = (root / "test/mcp/test_protocol_handshake.py").read_text(
        encoding="utf-8"
    )
    assert "pytestmark = [pytest.mark.api, pytest.mark.smoke]" in protocol_test
    assert '_ACTIVE_BIN_DIR = Path(sysconfig.get_path("scripts"))' in protocol_test


@pytest.mark.smoke
def test_caller_matrix_jobs_are_reusable_workflow_calls() -> None:
    """A matrix of test or report jobs is one reusable call, never N top-level jobs.

    GitHub nests the jobs of a called workflow under the caller's name, so a
    matrix inside a reusable workflow renders as one group in the checks list
    while a bare ``strategy.matrix`` job in the caller scatters its cells
    across the top level (``.claude/rules/ci/conventions.md``, "Matrix jobs
    render as one group"). Every job in the build workflow that carries a
    matrix must therefore be a ``uses:`` call, and a call whose name does not
    fan out over the matrix must be static.
    """
    root = Path(__file__).resolve().parents[2]
    caller = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )

    bare_matrix_jobs = [
        job_id
        for job_id, job in caller["jobs"].items()
        if "matrix" in (job.get("strategy") or {}) and "uses" not in job
    ]
    assert bare_matrix_jobs == []

    for job_id, job in caller["jobs"].items():
        if "uses" not in job:
            continue
        assert "runs-on" not in job, job_id
        assert "steps" not in job, job_id
        # GitHub rejects these keys on a caller job; they belong on the inner job.
        assert "continue-on-error" not in job, job_id
        assert "timeout-minutes" not in job, job_id
        if "matrix" not in (job.get("strategy") or {}):
            assert "matrix." not in job["name"], job_id


@pytest.mark.smoke
def test_tolerance_spread_group_is_one_reusable_call() -> None:
    """The spread matrix lives in its reusable workflow behind one static name."""
    root = Path(__file__).resolve().parents[2]
    caller = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    group = caller["jobs"]["tolerance_spread"]
    assert group["name"] == "Tolerance spread"
    assert group["uses"].endswith("/tolerance-spread-reusable.yml")
    assert "strategy" not in group
    assert {"determine_matrix", "build_wheels"} <= set(group["needs"])
    # The trigger decision stays on the caller: `inputs.*` inside the reusable
    # workflow is the workflow_call input, not the dispatch input.
    assert "github.event_name == 'schedule'" in group["if"]
    assert "inputs.tolerance_spread == true" in group["if"]
    assert group["with"]["buildplat_json"] == (
        "${{ needs.determine_matrix.outputs.buildplat }}"
    )
    assert group["with"]["python_json"] == (
        "${{ needs.determine_matrix.outputs.bookend_python }}"
    )
    # Recording only: the group feeds neither the nightly report nor the gate.
    assert "tolerance_spread" not in caller["jobs"]["report_scheduled_run"]["needs"]
    assert "tolerance_spread" not in caller["jobs"]["pr-gate"]["needs"]

    reusable = yaml.safe_load(
        (root / ".github/workflows/tolerance-spread-reusable.yml").read_text(
            encoding="utf-8"
        )
    )
    triggers = reusable[True] if True in reusable else reusable["on"]
    assert set(triggers) == {"workflow_call"}
    assert set(triggers["workflow_call"]["inputs"]) == {
        "buildplat_json",
        "python_json",
        "build_profile",
    }
    (cell,) = reusable["jobs"].values()
    assert cell["name"] == (
        "${{ matrix.python_version }}-${{ matrix.buildplat[1] }} "
        "${{ matrix.buildplat[2] }}"
    )
    assert cell["continue-on-error"] is True
    assert cell["timeout-minutes"] == 45
    assert cell["strategy"]["fail-fast"] is False
    assert cell["strategy"]["matrix"] == {
        "buildplat": "${{ fromJson(inputs.buildplat_json) }}",
        "python_version": "${{ fromJson(inputs.python_json) }}",
    }

    download = next(
        step
        for step in cell["steps"]
        if str(step.get("uses", "")).startswith("actions/download-artifact@")
    )
    assert download["with"]["name"] == (
        "cp312-${{ matrix.buildplat[1] }}-${{ matrix.buildplat[2] }}"
    )
    upload = cell["steps"][-1]
    assert upload["if"] == "always()"
    assert upload["with"]["name"] == (
        "tolerance-spread-${{ matrix.buildplat[1] }}-${{ matrix.buildplat[2] }}"
        "-${{ matrix.python_version }}"
    )
    assert upload["with"]["if-no-files-found"] == "error"
    measure = next(
        step
        for step in cell["steps"]
        if "tolerance_spread.py" in str(step.get("run", ""))
    )
    assert measure["env"]["SUEWS_BUILD_PROFILE"] == "${{ inputs.build_profile }}"

    declared_needs = json.loads(
        (root / ".github/ci-metrics-needs.json").read_text(encoding="utf-8")
    )
    child = "Tolerance spread / cp312-win AMD64"
    patterns = [k for k in declared_needs if fnmatchcase(child, k)]
    assert patterns == ["Tolerance spread / *"]
    dependencies = declared_needs[patterns[0]]
    assert "Determine build matrix" in dependencies
    assert any(
        fnmatchcase("Build standard wheels / cp312-win AMD64", d) for d in dependencies
    )


@pytest.mark.core
def test_standard_marker_expressions_preserve_core_slow_override() -> None:
    """All duplicated standard selectors keep importance independent of cost."""
    root = Path(__file__).resolve().parents[2]
    action = (root / ".github/actions/build-suews/action.yml").read_text(
        encoding="utf-8"
    )
    api_workflow = (
        root / ".github/workflows/test-api-cross-python-reusable.yml"
    ).read_text(encoding="utf-8")
    overhead = (root / ".github/workflows/ci-metrics-overhead.yml").read_text(
        encoding="utf-8"
    )
    scheduler = (root / ".github/workflows/benchmark-pytest-scheduler.yml").read_text(
        encoding="utf-8"
    )

    physics_standard = "physics and (core or not slow)"
    assert action.count(physics_standard) == 1
    assert overhead.count(physics_standard) == 1
    assert scheduler.count(physics_standard) == 4

    api_standard = "EXPR='api and (core or not slow) and not qgis'"
    assert api_workflow.count(api_standard) == 2  # standard and physics-full
    assert "physics and smoke and not (medium or slow)" in action
    assert "api and smoke and not (medium or slow) and not qgis" in api_workflow


def test_api_workers_abba_lane_is_manual_matched_and_per_platform() -> None:
    """The api-workers lane runs S/P/P/S per platform with fixed worker counts."""
    root = Path(__file__).resolve().parents[2]
    workflow = (root / ".github/workflows/benchmark-pytest-scheduler.yml").read_text(
        encoding="utf-8"
    )
    parsed = yaml.safe_load(workflow)
    dispatch = (
        parsed[True]["workflow_dispatch"]
        if True in parsed
        else parsed["on"]["workflow_dispatch"]
    )
    api_job = parsed["jobs"]["api_workers"]

    assert set(parsed[True] if True in parsed else parsed["on"]) == {
        "workflow_dispatch"
    }
    assert dispatch["inputs"]["lane"]["default"] == "physics-scheduler"
    assert dispatch["inputs"]["lane"]["options"] == ["physics-scheduler", "api-workers"]
    assert parsed["jobs"]["compare"]["if"] == "inputs.lane == 'physics-scheduler'"
    assert api_job["if"] == "inputs.lane == 'api-workers'"
    assert parsed["jobs"]["compare"]["needs"] == "source"
    assert api_job["needs"] == "source"

    cells = {
        (cell["runner"], cell["platform"], cell["arch"]): cell["workers"]
        for cell in api_job["strategy"]["matrix"]["include"]
    }
    assert cells == {
        ("ubuntu-latest", "manylinux", "x86_64"): 4,
        ("windows-2025", "win", "AMD64"): 4,
        ("macos-15", "macosx", "arm64"): 2,
    }
    assert api_job["strategy"]["fail-fast"] is False
    assert api_job["env"]["MARKER_EXPR"] == "api and (core or not slow) and not qgis"

    trial_names = [
        step["name"]
        for step in api_job["steps"]
        if step.get("continue-on-error") is True
    ]
    assert trial_names == [
        "S1 - serial",
        "P1 - xdist worksteal",
        "P2 - xdist worksteal",
        "S2 - serial",
    ]
    trials = {
        step["name"]: step for step in api_job["steps"] if step["name"] in trial_names
    }
    for name, step in trials.items():
        assert "-p scripts.suews.pytest_ci_metrics test" in step["run"]
        assert '-m "$MARKER_EXPR"' in step["run"]
        assert "-p no:cacheprovider" in step["run"]
        assert step["env"]["SUEWS_CI_METRICS"].startswith("api-abba/")
        if name.startswith("P"):
            assert '-n "$WORKERS" --dist worksteal' in step["run"]
        else:
            assert "-n " not in step["run"]
            assert "--dist" not in step["run"]
    assert len({step["env"]["SUEWS_CI_METRICS"] for step in trials.values()}) == 4
    assert len({step["run"].split("--basetemp=")[1] for step in trials.values()}) == 4

    downloads = [
        step["with"]["name"]
        for step in api_job["steps"]
        if step.get("uses", "").startswith("actions/download-artifact@")
    ]
    assert downloads == [
        "cp312-${{ matrix.platform }}-${{ matrix.arch }}",
        "suews-mcp-dist",
    ]

    tabulate = next(
        step for step in api_job["steps"] if step["name"] == "Tabulate the four trials"
    )
    assert tabulate["if"] == "always()"
    assert "summarise_abba_trials.py" in tabulate["run"]
    assert tabulate["run"].count("--trial") == 4
    upload = api_job["steps"][-1]
    assert upload["if"] == "always()"
    assert upload["with"]["if-no-files-found"] == "error"


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
