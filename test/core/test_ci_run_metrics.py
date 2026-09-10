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
    assert "- '.github/ci-metrics-needs.json'" in path_filters


@pytest.mark.core
def test_api_lane_runs_measured_xdist_workers_with_a_serial_escape_hatch() -> None:
    """The api lane runs -n 4/4/2 worksteal per platform unless serialised."""
    root = Path(__file__).resolve().parents[2]
    workflow = (
        root / ".github/workflows/test-api-cross-python-reusable.yml"
    ).read_text(encoding="utf-8")

    assert "python -m pip install pytest==9.1.1 pytest-xdist==3.8.0" in workflow
    main_invocation = re.search(
        r"^[ \t]*python -m pytest -p scripts\.suews\.pytest_ci_metrics test \\\n"
        r"[ \t]+-m \"\$MARKER_EXPR\" -v --tb=short --durations=25 \$XDIST_ARGS[ \t]*$",
        workflow,
        flags=re.MULTILINE,
    )
    assert main_invocation is not None

    # Measured in #1786: the worker counts are per platform, never -n auto.
    workers = dict(
        re.findall(
            r"^\s+(manylinux|win|macosx)\)\s+WORKERS=(\d+)", workflow, re.MULTILINE
        )
    )
    assert workers == {"manylinux": "4", "win": "4", "macosx": "2"}
    assert "-n auto" not in workflow
    assert 'echo "xdist_args=-n ${WORKERS} --dist worksteal"' in workflow

    # The escape hatch is a workflow_call input, matched per platform name.
    parsed = yaml.safe_load(workflow)
    call_inputs = parsed[True]["workflow_call"]["inputs"]
    assert "default" in call_inputs["serial_platforms"]
    assert not call_inputs["serial_platforms"]["default"]
    assert 'if [[ "$SERIAL_LIST" == *",${PLATFORM},"* ]]; then' in workflow

    caller = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    dispatch_inputs = caller[True]["workflow_dispatch"]["inputs"]
    assert "default" in dispatch_inputs["api_serial_platforms"]
    assert not dispatch_inputs["api_serial_platforms"]["default"]
    # The api lane is chained inside each platform's build-wheels call
    # (#1792), so the caller hands the escape hatch to that call and the
    # reusable workflow forwards it; an input accepted and then dropped there
    # would silently run every platform on its workers.
    chain_with = caller["jobs"]["build_wheels"]["with"]
    assert chain_with["serial_platforms"] == (
        "${{ inputs.api_serial_platforms || vars.SUEWS_API_SERIAL_PLATFORMS || '' }}"
    )
    wheels_workflow = yaml.safe_load(
        (root / ".github/workflows/build-wheels-reusable.yml").read_text(
            encoding="utf-8"
        )
    )
    wheels_inputs = wheels_workflow[True]["workflow_call"]["inputs"]
    assert "default" in wheels_inputs["serial_platforms"]
    assert not wheels_inputs["serial_platforms"]["default"]
    assert (
        wheels_workflow["jobs"]["api_cross_python"]["with"]["serial_platforms"]
        == "${{ inputs.serial_platforms }}"
    )


@pytest.mark.smoke
def test_api_lane_consumes_mcp_artifact_after_build() -> None:
    """The API matrix installs the MCP wheel only after both builds succeed."""
    root = Path(__file__).resolve().parents[2]
    caller = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    chain_job = caller["jobs"]["build_wheels"]
    assert {"determine_matrix", "build_mcp"} <= set(chain_job["needs"])
    assert chain_job["with"]["run_api_tests"].startswith(
        "${{ needs.build_mcp.result == 'success' && "
    )

    wheels_workflow = yaml.safe_load(
        (root / ".github/workflows/build-wheels-reusable.yml").read_text(
            encoding="utf-8"
        )
    )
    api_job = wheels_workflow["jobs"]["api_cross_python"]
    assert api_job["needs"] == "build"
    assert api_job["if"] == "inputs.run_api_tests"
    assert api_job["uses"].endswith("/test-api-cross-python-reusable.yml")

    api_workflow = (
        root / ".github/workflows/test-api-cross-python-reusable.yml"
    ).read_text(encoding="utf-8")
    assert "name: suews-mcp-dist" in api_workflow
    assert "path: mcp-dist/" in api_workflow
    assert "python -m pip install wheelhouse/*.whl mcp-dist/*.whl" in api_workflow

    declared_needs = json.loads(
        (root / ".github/ci-metrics-needs.json").read_text(encoding="utf-8")
    )
    api_patterns = [
        pattern
        for pattern in declared_needs
        if "/ API cross-CPython tests / " in pattern
    ]
    assert api_patterns
    for pattern in api_patterns:
        assert "Build MCP package" in declared_needs[pattern]


@pytest.mark.smoke
def test_api_lane_waits_for_its_own_platform_wheel_only() -> None:
    """Each platform's api lane is chained behind that platform's wheel build.

    The caller runs one reusable-workflow call per platform, so the api lane
    inside it depends on one wheel, and the declared-needs graph read by
    analyse_ci_run.py says the same per platform.
    """
    root = Path(__file__).resolve().parents[2]
    caller = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    chain_job = caller["jobs"]["build_wheels"]
    assert chain_job["strategy"]["matrix"]["buildplat"] == (
        "${{ fromJson(needs.determine_matrix.outputs.buildplat) }}"
    )
    assert chain_job["strategy"]["fail-fast"] is False
    assert "matrix.buildplat[1]" in chain_job["name"]
    assert chain_job["with"]["buildplat_json"].startswith("${{ format('[[")
    assert "test_api_cross_python" not in caller["jobs"]

    declared_needs = json.loads(
        (root / ".github/ci-metrics-needs.json").read_text(encoding="utf-8")
    )
    wheel_jobs = [
        f"Build and test ({platform}) / cp312-{platform}"
        for platform in ("manylinux x86_64", "macosx arm64", "macosx x86_64", "win AMD64")
    ]
    for platform in ("manylinux x86_64", "macosx arm64", "macosx x86_64", "win AMD64"):
        api_job = f"Build and test ({platform}) / API cross-CPython tests / cp312-x"
        patterns = [k for k in declared_needs if fnmatchcase(api_job, k)]
        dependencies = {d for k in patterns for d in declared_needs[k]}
        wheels_waited_for = [
            wheel
            for wheel in wheel_jobs
            if any(fnmatchcase(wheel, d) for d in dependencies)
        ]
        assert wheels_waited_for == [f"Build and test ({platform}) / cp312-{platform}"]
    gate = declared_needs["PR build validation"]
    assert any(fnmatchcase(wheel_jobs[0], d) for d in gate)
    assert any(
        fnmatchcase(
            "Build and test (win AMD64) / API cross-CPython tests / cp312-win AMD64", d
        )
        for d in gate
    )


@pytest.mark.smoke
def test_api_platform_gate_finds_every_preset_runner_label() -> None:
    """Every platform preset spells its runner label the way the gate looks it up.

    The api lane now runs inside each platform's own reusable call, gated by
    `run_api_tests`, which asks whether the api platform list contains that
    platform's runner label wrapped in double quotes. The platform lists are
    plain text built in determine-matrix.sh, so a preset written in any other
    quoting shape makes the containment false: the lane is skipped, a skipped
    inner job reports success, and the publish gate stays green over a run
    that ran no api test at all. This pins the two shapes together, including
    the closing quote that keeps `macos-15` from matching `macos-15-intel`.
    """
    root = Path(__file__).resolve().parents[2]
    caller = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    run_api_tests = caller["jobs"]["build_wheels"]["with"]["run_api_tests"]

    # The gate searches the api platform list (with the buildplat fallback for
    # a PR that predates an api_buildplat change) for this rendering of the
    # runner label.
    assert "needs.determine_matrix.outputs.api_buildplat" in run_api_tests
    assert "needs.determine_matrix.outputs.buildplat" in run_api_tests
    key = re.search(r"format\('([^']*)', matrix\.buildplat\[0\]\)", run_api_tests)
    assert key is not None, run_api_tests
    template = key.group(1)
    assert "{0}" in template

    script = (root / ".github/scripts/determine-matrix.sh").read_text(
        encoding="utf-8"
    )
    # Named presets, plus the triples the custom dispatch branch appends one
    # at a time; both reach api_buildplat, so both are held to the same shape.
    sources = dict(
        re.findall(r"^([A-Z_]+PLATFORMS)='(\[.*\])'$", script, re.MULTILINE)
    )
    assert {
        "FULL_PLATFORMS",
        "PR_PLATFORMS",
        "MINIMAL_PLATFORMS",
        "NIGHTLY_API_PLATFORMS",
    } <= set(sources)
    for index, triple in enumerate(
        re.findall(r"PLATFORMS\+='(\[[^']*\]),'", script)
    ):
        sources[f"custom dispatch triple {index}"] = triple

    runners: dict[str, set[str]] = {}
    for name, literal in sources.items():
        # fromJson consumes these, so JSON is itself part of the contract.
        parsed = json.loads(literal)
        triples = [parsed] if parsed and isinstance(parsed[0], str) else parsed
        assert triples, name
        runners[name] = {runner for runner, _platform, _arch in triples}
        for runner in runners[name]:
            assert template.replace("{0}", runner) in literal, (name, runner)

    # The rendered key must identify one runner and not read as a prefix of
    # another: `macos-15` sits inside `macos-15-intel`, so a key without the
    # closing quote would match a list holding only the Intel runner and run
    # a lane on the wrong platform list. The quotes are what rule that out.
    every_runner = set().union(*runners.values())
    for one in every_runner:
        for other in every_runner - {one}:
            assert template.replace("{0}", one) not in template.replace(
                "{0}", other
            ), (one, other)

    # The nightly trim is the only case where the api list differs from the
    # build list, so it is the only case where the lookup has to discriminate.
    dropped = runners["FULL_PLATFORMS"] - runners["NIGHTLY_API_PLATFORMS"]
    assert dropped == {"macos-15-intel"}
    for runner in dropped:
        assert (
            template.replace("{0}", runner)
            not in sources["NIGHTLY_API_PLATFORMS"]
        )


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
