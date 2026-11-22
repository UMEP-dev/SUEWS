"""Contract tests for workflow timing analysis and overhead calculations."""

from __future__ import annotations

from datetime import UTC, datetime
from fnmatch import fnmatchcase
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

import pytest
import yaml

from scripts.suews import analyse_ci_run
from scripts.suews.analyse_ci_run import TargetNotCompletedError, analyse_run

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]


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
def test_target_not_yet_completed_is_a_distinct_error() -> None:
    """A target the Jobs API still reports in progress is not config drift."""
    run = {"id": 42, "created_at": "2026-07-15T00:00:00Z"}
    gate = _job("Gate", 108, 109, 114)
    gate.update(status="in_progress", conclusion=None, completed_at=None)
    jobs = {"jobs": [_job("Build", 0, 2, 100), gate]}

    with pytest.raises(TargetNotCompletedError, match="'Gate' is 'in_progress'"):
        analyse_run(run, jobs, declared_needs={"Gate": ["Build"]}, target_job_pattern="Gate")

    # Marked completed but without a completion time yet: also still settling.
    gate.update(status="completed", conclusion="success")
    with pytest.raises(TargetNotCompletedError, match="'Gate' is 'completed'"):
        analyse_run(run, jobs, declared_needs={"Gate": ["Build"]}, target_job_pattern="Gate")

    # A missing target still fails: the workflow and the pattern have drifted.
    with pytest.raises(ValueError, match="found 0") as missing:
        analyse_run(
            run,
            {"jobs": [_job("Build", 0, 2, 100)]},
            declared_needs={},
            target_job_pattern="Gate",
        )
    assert not isinstance(missing.value, TargetNotCompletedError)


@pytest.mark.core
def test_cli_skips_cleanly_when_target_not_yet_completed(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """The summary job reports a skip, not a failure, for a lagging Jobs API."""
    gate = _job("PR build validation", 108, 109, 114)
    gate.update(status="in_progress", conclusion=None, completed_at=None)
    payloads = {
        "run.json": {"id": 42, "created_at": "2026-07-15T00:00:00Z"},
        "jobs.json": {"jobs": [_job("Build", 0, 2, 100), gate]},
        "needs.json": {"PR build validation": ["Build"]},
    }
    for name, payload in payloads.items():
        (tmp_path / name).write_text(json.dumps(payload), encoding="utf-8")
    output = tmp_path / "metrics.json"
    summary = tmp_path / "summary.md"
    monkeypatch.setattr(
        "sys.argv",
        [
            "analyse_ci_run.py",
            "--run-json", str(tmp_path / "run.json"),
            "--jobs-json", str(tmp_path / "jobs.json"),
            "--needs-json", str(tmp_path / "needs.json"),
            "--output", str(output),
            "--summary", str(summary),
        ],
    )

    assert analyse_ci_run.main() == 0
    assert not output.exists()
    assert "Skipped: Target 'PR build validation' is 'in_progress'" in summary.read_text(
        encoding="utf-8"
    )


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
def test_api_lane_runs_measured_xdist_workers_with_a_serial_escape_hatch() -> None:
    """The api lane runs -n 4/4/2 worksteal per platform unless serialised."""
    root = Path(__file__).resolve().parents[2]
    workflow = (
        root / ".github/workflows/test-api-cross-python-reusable.yml"
    ).read_text(encoding="utf-8")

    assert "python -m pip install pytest==9.1.1 pytest-xdist==3.8.0" in workflow
    main_invocation = re.search(
        r"^[ \t]*python -m pytest -p scripts\.suews\.pytest_ci_metrics test \\\n"
        r"[ \t]+-m \"\$MARKER_EXPR\" -v --tb=short --durations=25 \$XDIST_ARGS \$SHARD_ARGS[ \t]*$",
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


@pytest.mark.core
def test_pull_request_api_layout_drops_macos_and_shards_windows_cp312() -> None:
    """Pull requests run no macOS api cell and split the Windows cp312 cell.

    The nightly keeps every cell whole and the macOS api coverage, and the
    merge queue runs the api lane on Linux only (its own term), so the PR
    conditions name the pull_request event (or the pr_api_lane dispatch input
    that measures the layout) explicitly. The
    sharded cell gets its own job name and artefact name per shard, while an
    unsharded cell keeps the names the cost-marker and coverage jobs read.
    """
    root = Path(__file__).resolve().parents[2]
    caller = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    layout = "(github.event_name == 'pull_request' || inputs.pr_api_lane)"
    chain_with = caller["jobs"]["build_wheels"]["with"]
    assert chain_with["run_api_tests"].endswith(
        f"&& !({layout} && matrix.buildplat[1] == 'macosx') "
        "&& !(github.event_name == 'merge_group' && matrix.buildplat[1] != 'manylinux') }}"
    )
    assert chain_with["api_shard_python"] == (
        f"${{{{ {layout} && matrix.buildplat[1] == 'win' && 'cp312' || '' }}}}"
    )
    dispatch_inputs = caller[True]["workflow_dispatch"]["inputs"]
    assert dispatch_inputs["pr_api_lane"]["default"] is False

    wheels = yaml.safe_load(
        (root / ".github/workflows/build-wheels-reusable.yml").read_text(
            encoding="utf-8"
        )
    )
    assert not wheels[True]["workflow_call"]["inputs"]["api_shard_python"]["default"]
    assert wheels["jobs"]["api_cross_python"]["with"]["shard_python"] == (
        "${{ inputs.api_shard_python }}"
    )

    api_text = (
        root / ".github/workflows/test-api-cross-python-reusable.yml"
    ).read_text(encoding="utf-8")
    api = yaml.safe_load(api_text)
    assert not api[True]["workflow_call"]["inputs"]["shard_python"]["default"]
    job = api["jobs"]["api_cross_python"]
    matrix = job["strategy"]["matrix"]
    assert matrix["shard"] == [""]
    guard = (
        "inputs.shard_python && contains(inputs.api_python_json, "
        "format('\"{0}\"', inputs.shard_python)) &&"
    )
    assert guard in matrix["exclude"]
    assert guard in matrix["include"]
    assert '"shard":"1-of-2"' in matrix["include"]
    assert '"shard":"2-of-2"' in matrix["include"]
    assert "toJSON(fromJson(inputs.buildplat_json)[0])" in matrix["include"]
    suffix = "${{ matrix.shard && format('-shard-{0}', matrix.shard) || '' }}"
    run_step = next(
        s for s in job["steps"] if s.get("name") == "Install wheel and run api tests"
    )
    assert run_step["env"]["SHARD"] == "${{ matrix.shard }}"
    assert run_step["env"]["SUEWS_CI_METRICS"].endswith(f"{suffix}.json")
    upload = next(s for s in job["steps"] if s.get("name") == "Upload API test metrics")
    assert upload["with"]["name"] == (
        "ci-metrics-api-${{ matrix.python_version }}-${{ matrix.buildplat[1] }}-"
        "${{ matrix.buildplat[2] }}" + suffix
    )
    assert "matrix.shard && format(' shard {0}', matrix.shard)" in job["name"]
    # The protocol collection pass stays unsharded: it would collect nothing
    # in the shard that does not own the protocol test.
    assert (
        'SHARD_ARGS="-p scripts.suews.pytest_shard --suews-shard ${SHARD/-of-//}"'
        in run_step["run"]
    )
    collect_line = next(
        line for line in run_step["run"].splitlines() if "--collect-only" in line
    )
    assert "SHARD" not in collect_line


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
        f"Build and test ({platform}) / wheel | cp312 | {platform}"
        for platform in ("manylinux x86_64", "macosx arm64", "macosx x86_64", "win AMD64")
    ]
    for platform in ("manylinux x86_64", "macosx arm64", "macosx x86_64", "win AMD64"):
        api_job = f"Build and test ({platform}) / API cross-CPython tests / api | cp312 | x"
        patterns = [k for k in declared_needs if fnmatchcase(api_job, k)]
        dependencies = {d for k in patterns for d in declared_needs[k]}
        wheels_waited_for = [
            wheel
            for wheel in wheel_jobs
            if any(fnmatchcase(wheel, d) for d in dependencies)
        ]
        assert wheels_waited_for == [
            f"Build and test ({platform}) / wheel | cp312 | {platform}"
        ]
    gate = declared_needs["PR build validation"]
    assert any(fnmatchcase(wheel_jobs[0], d) for d in gate)
    assert any(
        fnmatchcase(
            "Build and test (win AMD64) / API cross-CPython tests / api | cp312 | win AMD64",
            d,
        )
        for d in gate
    )


@pytest.mark.smoke
# determine-matrix.sh runs on the Linux determine_matrix job only. Under the
# `bash` a Windows runner finds on PATH it exits 1 (merge-queue run
# 43745949704), so the test holds the script to the platforms it runs on.
@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="determine-matrix.sh runs under bash on Linux CI only",
)
@pytest.mark.parametrize(
    ("event_name", "ref"),
    [
        ("workflow_dispatch", "refs/heads/master"),
        ("push", "refs/tags/2026.11.1"),
    ],
    ids=["full-dispatch", "tag-release"],
)
def test_full_matrices_build_macos_intel_but_skip_its_api_lanes(
    tmp_path: Path, event_name: str, ref: str
) -> None:
    """A full dispatch and a tag release build every wheel, test api as nightly.

    The macOS Intel api lanes run past the 45-minute job timeout (dispatched
    run 43734399764), and a cancelled lane fails build_wheels and skips every
    job that needs it, the publish jobs of a release among them. Both
    triggers therefore keep macos-15-intel in `buildplat` and drop it from
    `api_buildplat`, matching the nightly schedule.
    """
    root = Path(__file__).resolve().parents[2]
    output = tmp_path / "github_output"
    output.write_text("", encoding="utf-8")
    env = {
        **os.environ,
        "GITHUB_OUTPUT": str(output),
        "GITHUB_REF": ref,
        "EVENT_NAME": event_name,
        "INPUT_MATRIX_CONFIG": "full",
        "IS_DRAFT": "false",
        "FORTRAN_CHANGED": "false",
        "RUST_CHANGED": "false",
        "PYTHON_CHANGED": "false",
        "UTIL_CHANGED": "false",
        "BUILD_CHANGED": "false",
        "CI_CHANGED": "false",
        "INPUT_TEST_TIER": "all",
        "PHYSICS_CHANGE": "false",
    }
    subprocess.run(
        ["bash", str(root / ".github/scripts/determine-matrix.sh")],
        env=env,
        check=True,
        capture_output=True,
    )
    outputs = dict(
        line.split("=", 1)
        for line in output.read_text(encoding="utf-8").splitlines()
        if "=" in line
    )
    built = {runner for runner, _p, _a in json.loads(outputs["buildplat"])}
    api = {runner for runner, _p, _a in json.loads(outputs["api_buildplat"])}
    assert "macos-15-intel" in built
    assert api == built - {"macos-15-intel"}


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
    # The lane markers must lead the list; later markers (contract tags) may follow.
    assert re.search(
        r"^pytestmark = \[pytest\.mark\.api, pytest\.mark\.smoke[,\]]",
        protocol_test,
        re.MULTILINE,
    )
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
            assert "matrix." not in job.get("name", ""), job_id


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
        fnmatchcase("Build and test (win AMD64) / wheel | cp312 | win AMD64", d)
        for d in dependencies
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


def test_python_coverage_job_records_with_the_api_lane_pins() -> None:
    """The nightly coverage job never gates and tests with the api lane's pins.

    It must stay out of the nightly alert and the PR gate like the other
    recording jobs, carry continue-on-error and a timeout, and install the same
    exact test requirements as the api lane plus exactly pinned pytest-cov and
    coverage, so its figures describe the suite the lanes actually run.
    """
    root = Path(__file__).resolve().parents[2]
    caller = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(encoding="utf-8")
    )
    wheels = yaml.safe_load(
        (root / ".github/workflows/build-wheels-reusable.yml").read_text(encoding="utf-8")
    )
    # The job is chained behind the Linux wheel build inside the per-platform
    # reusable call, so it no longer waits for every platform's api lane.
    assert "python_coverage" not in caller["jobs"]
    job = wheels["jobs"]["python_coverage"]
    trigger = caller["jobs"]["build_wheels"]["with"]["run_python_coverage"]

    assert job["name"] == "Python coverage (Linux cp312)"
    assert job["needs"] == "build"
    assert job["if"] == "inputs.run_python_coverage"
    assert job["continue-on-error"] is True
    assert 0 < job["timeout-minutes"] <= 120
    run_step = next(s for s in job["steps"] if "--cov" in str(s.get("run", "")))
    # The step times out before the job does, so the summary and upload run.
    assert 0 < run_step["timeout-minutes"] < job["timeout-minutes"]
    assert "matrix.buildplat[1] == 'manylinux'" in trigger
    assert "needs.build_mcp.result == 'success'" in trigger
    assert "github.event_name == 'schedule'" in trigger
    assert "inputs.python_coverage == true" in trigger
    assert "python_coverage" not in caller["jobs"]["report_scheduled_run"]["needs"]
    assert "python_coverage" not in caller["jobs"]["pr-gate"]["needs"]

    def pins(text: str) -> set[str]:
        return set(re.findall(r"\b[\w.-]+==[\w.]+", text))

    install = next(
        step["run"] for step in job["steps"] if "pytest-cov==" in str(step.get("run", ""))
    )
    api_workflow = (root / ".github/workflows/test-api-cross-python-reusable.yml").read_text(
        encoding="utf-8"
    )
    api_install = next(
        line for line in api_workflow.splitlines() if "pip install pytest==" in line
    )
    assert pins(api_install) <= pins(install)
    assert {p.split("==")[0] for p in pins(install) - pins(api_install)} == {
        "pytest-cov",
        "coverage",
    }

    run = next(
        step["run"] for step in job["steps"] if "--cov" in str(step.get("run", ""))
    )
    assert '-m "not qgis"' in run
    assert "-n 4" in run
    assert "--cov-report=json:coverage/coverage.json" in run
    upload = job["steps"][-1]
    assert upload["if"] == "always()"
    assert upload["with"]["name"] == "python-coverage"


def _write_gh_stub(bin_dir: Path) -> None:
    """Write a minimal executable ``gh`` stub into ``bin_dir`` for workflow step tests."""
    stub = bin_dir / "gh"
    stub.write_text(
        "#!/usr/bin/env python3\n"
        "import json\n"
        "import os\n"
        "import re\n"
        "import sys\n"
        "argv = sys.argv[1:]\n"
        "args = ' '.join(argv)\n"
        "paginate = '--paginate' in argv\n"
        "m_lbl = re.search(r'/issues/(\\d+)/labels', args)\n"
        "m_pr_files = re.search(r'/pulls/(\\d+)/files', args)\n"
        "if m_lbl:\n"
        "    by_pr = json.loads(os.environ.get('FAKE_GH_LABELS_BY_PR', '{}'))\n"
        "    pr = m_lbl.group(1)\n"
        "    raw = (\n"
        "        by_pr[pr]\n"
        "        if pr in by_pr\n"
        "        else json.loads(os.environ.get('FAKE_GH_LABELS_JSON', '[]'))\n"
        "    )\n"
        "    pages = raw if (raw and isinstance(raw[0], list)) else [raw]\n"
        "    shown = pages if paginate else pages[:1]\n"
        "    for page in shown:\n"
        "        sys.stdout.write(json.dumps(page) + '\\n')\n"
        "elif m_pr_files or '/compare/' in args:\n"
        "    sys.stdout.write(os.environ.get('FAKE_GH_FILES', '') + '\\n')\n"
        "else:\n"
        "    sys.exit(f'unexpected gh call: {args}')\n",
        encoding="utf-8",
    )
    stub.chmod(0o755)


@pytest.mark.smoke
@pytest.mark.skipif(
    sys.platform == "win32"
    or shutil.which("bash") is None
    or shutil.which("jq") is None,
    reason="Workflow shell steps run under bash + jq on Linux CI",
)
@pytest.mark.parametrize(
    ("event_name", "api_labels", "head_ref", "base_sha", "expected"),
    [
        ("pull_request", ["0-physics:change"], "", "", "true"),
        ("pull_request", ["1-maintenance", "0-numerics:change"], "", "", "true"),
        (
            "pull_request",
            [["1-maintenance"], ["0-numerics:change"]],
            "",
            "",
            "true",
        ),
        (
            "pull_request",
            [["0-physics:change"], ["0-numerics:change"]],
            "",
            "",
            "true",
        ),
        ("pull_request", ["1-maintenance", "2-meta:governance"], "", "", "false"),
        (
            "merge_group",
            ["0-numerics:change"],
            "refs/heads/gh-readonly-queue/master/pr-42-ba5e000",
            "ba5e000",
            "true",
        ),
        (
            "merge_group",
            [["1-maintenance"], ["0-numerics:change"]],
            "refs/heads/gh-readonly-queue/master/pr-42-ba5e000",
            "ba5e000",
            "true",
        ),
        (
            "merge_group",
            ["1-maintenance"],
            "refs/heads/gh-readonly-queue/master/pr-42-ba5e000",
            "ba5e000",
            "false",
        ),
        (
            "merge_group",
            ["1-maintenance"],
            "refs/heads/gh-readonly-queue/master/pr-42-a11ce41",
            "ba5e000",
            "true",
        ),
        (
            "merge_group",
            [],
            "refs/heads/gh-readonly-queue/master/batch-deadbeef",
            "ba5e000",
            "true",
        ),
    ],
)
def test_determine_matrix_label_step_reads_live_api_labels(
    tmp_path: Path,
    event_name: str,
    api_labels: list[str] | list[list[str]],
    head_ref: str,
    base_sha: str,
    expected: str,
) -> None:
    """Live API labels drive physics-full selection across PR and merge_group contexts."""
    # ARRANGE
    root = Path(__file__).resolve().parents[2]
    workflow = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    step = next(
        s
        for s in workflow["jobs"]["determine_matrix"]["steps"]
        if s.get("id") == "physics-label"
    )
    assert "PR_LABELS_JSON" not in step["env"]
    assert json.loads(step["env"]["LABELS_JSON"]) == [
        "0-physics:change",
        "0-numerics:change",
    ]

    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    _write_gh_stub(bin_dir)
    gh_out = tmp_path / "github_output"
    gh_out.write_text("", encoding="utf-8")

    env = {
        **os.environ,
        "PATH": f"{bin_dir}:{os.environ.get('PATH', '')}",
        "GITHUB_OUTPUT": str(gh_out),
        "EVENT_NAME": event_name,
        "PR_NUMBER": "42",
        "FAKE_GH_LABELS_JSON": json.dumps(api_labels),
        "MERGE_HEAD_REF": head_ref,
        "MERGE_BASE_SHA": base_sha,
        "REPO": "UMEP-dev/SUEWS",
        "LABELS_JSON": step["env"]["LABELS_JSON"],
    }

    # ACT
    result = subprocess.run(
        ["bash", "-c", step["run"]],
        env=env,
        check=True,
        capture_output=True,
        text=True,
    )

    # ASSERT
    outputs = dict(
        line.split("=", 1)
        for line in gh_out.read_text(encoding="utf-8").splitlines()
        if "=" in line
    )
    assert outputs["found"] == expected
    if api_labels == [["0-physics:change"], ["0-numerics:change"]]:
        assert "(0-physics:change, 0-numerics:change)" in result.stdout


@pytest.mark.smoke
@pytest.mark.skipif(
    sys.platform == "win32"
    or shutil.which("bash") is None
    or shutil.which("jq") is None,
    reason="Workflow shell steps run under bash + jq on Linux CI",
)
@pytest.mark.parametrize(
    "case",
    [
        pytest.param(
            {
                "event_name": "pull_request",
                "is_draft": "false",
                "test_tier": "smoke",
                "head_ref": "",
                "base_sha": "",
                "files": "test/fixtures/data_test/sample_output_2012-01.csv",
                "labels": ["1-maintenance"],
                "expected_rc": 1,
                "expected_substring": "without 0-physics:change or 0-numerics:change",
            },
            id="pr-unlabelled-ref-fails",
        ),
        pytest.param(
            {
                "event_name": "pull_request",
                "is_draft": "false",
                "test_tier": "physics-full",
                "head_ref": "",
                "base_sha": "",
                "files": (
                    "test/fixtures/data_test/sample_output_2012-01.csv\n"
                    "test/fixtures/data_test/provenance.json"
                ),
                "labels": ["1-maintenance", "0-numerics:change"],
                "expected_rc": 0,
                "expected_substring": (
                    "[OK] Reference and physics changes carry #42: 0-numerics:change"
                ),
            },
            id="pr-numerics-physics-full-passes",
        ),
        pytest.param(
            {
                "event_name": "pull_request",
                "is_draft": "false",
                "test_tier": "physics-full",
                "head_ref": "",
                "base_sha": "",
                "files": "test/fixtures/w16_most.csv",
                "labels": ["1-bug", "0-physics:change"],
                "expected_rc": 0,
                "expected_substring": (
                    "[OK] Reference and physics changes carry #42: 0-physics:change"
                ),
            },
            id="pr-physics-physics-full-passes",
        ),
        pytest.param(
            {
                "event_name": "pull_request",
                "is_draft": "false",
                "test_tier": "smoke",
                "head_ref": "",
                "base_sha": "",
                "files": "test/fixtures/data_test/sample_output_2012-01.csv",
                "labels": ["1-maintenance", "0-numerics:change"],
                "expected_rc": 1,
                "expected_substring": (
                    "requires test_tier=physics-full, but this workflow run used test_tier=smoke"
                ),
            },
            id="pr-late-label-without-physics-full-fails",
        ),
        pytest.param(
            {
                "event_name": "pull_request",
                "is_draft": "true",
                "test_tier": "smoke",
                "head_ref": "",
                "base_sha": "",
                "files": "test/fixtures/data_test/sample_output_2012-01.csv",
                "labels": ["1-maintenance", "0-numerics:change"],
                "expected_rc": 0,
                "expected_substring": (
                    "[OK] Reference and physics changes carry #42: 0-numerics:change"
                ),
            },
            id="draft-pr-numerics-smoke-passes",
        ),
        pytest.param(
            {
                "event_name": "pull_request",
                "is_draft": "false",
                "test_tier": "physics-full",
                "head_ref": "",
                "base_sha": "",
                "files": "test/fixtures/w16_most.csv",
                "labels": ["0-physics:change", "0-numerics:change"],
                "expected_rc": 1,
                "expected_substring": "mutually exclusive",
            },
            id="pr-both-labels-fails",
        ),
        pytest.param(
            {
                "event_name": "pull_request",
                "is_draft": "false",
                "test_tier": "physics-full",
                "head_ref": "",
                "base_sha": "",
                "files": "test/fixtures/w16_most.csv",
                "labels": [
                    ["1-maintenance", "0-physics:change"],
                    ["2-infra:ci", "0-numerics:change"],
                ],
                "expected_rc": 1,
                "expected_substring": "mutually exclusive",
            },
            id="pr-split-pages-both-labels-fails",
        ),
        pytest.param(
            {
                "event_name": "pull_request",
                "is_draft": "false",
                "test_tier": "physics-full",
                "head_ref": "",
                "base_sha": "",
                "files": "test/fixtures/w16_most.csv",
                "labels": [["1-maintenance"], ["0-numerics:change"]],
                "expected_rc": 0,
                "expected_substring": (
                    "[OK] Reference and physics changes carry #42: 0-numerics:change"
                ),
            },
            id="pr-split-pages-second-page-label-passes",
        ),
        pytest.param(
            {
                "event_name": "pull_request",
                "is_draft": "false",
                "test_tier": "standard",
                "head_ref": "",
                "base_sha": "",
                "files": "src/suews/src/suews_phys_ohm.f95",
                "labels": ["1-maintenance"],
                "expected_rc": 0,
                "expected_substring": (
                    "::warning::PR #42 touches physics source without 0-physics:change or 0-numerics:change"
                ),
            },
            id="pr-unlabelled-physics-warns",
        ),
        pytest.param(
            {
                "event_name": "merge_group",
                "is_draft": "false",
                "test_tier": "physics-full",
                "head_ref": "refs/heads/gh-readonly-queue/master/pr-42-ba5e000",
                "base_sha": "ba5e000",
                "files": "test/fixtures/w16_most.csv",
                "labels": ["0-numerics:change"],
                "expected_rc": 0,
                "expected_substring": (
                    "[OK] Reference and physics changes carry #42: 0-numerics:change"
                ),
            },
            id="mq-single-pr-labelled-passes",
        ),
        pytest.param(
            {
                "event_name": "merge_group",
                "is_draft": "false",
                "test_tier": "physics-full",
                "head_ref": "refs/heads/gh-readonly-queue/master/pr-42-ba5e000",
                "base_sha": "ba5e000",
                "files": "test/fixtures/w16_most.csv",
                "labels": ["1-maintenance"],
                "expected_rc": 1,
                "expected_substring": (
                    "PR #42 changes stored reference outputs without 0-physics:change or 0-numerics:change"
                ),
            },
            id="mq-single-pr-unlabelled-ref-fails",
        ),
        pytest.param(
            {
                "event_name": "merge_group",
                "is_draft": "false",
                "test_tier": "physics-full",
                "head_ref": "refs/heads/gh-readonly-queue/master/pr-42-ba5e000",
                "base_sha": "ba5e000",
                "files": "test/fixtures/w16_most.csv",
                "labels": ["0-physics:change", "0-numerics:change"],
                "expected_rc": 1,
                "expected_substring": "mutually exclusive",
            },
            id="mq-single-pr-both-labels-fails",
        ),
        pytest.param(
            {
                "event_name": "merge_group",
                "is_draft": "false",
                "test_tier": "physics-full",
                "head_ref": "refs/heads/gh-readonly-queue/master/pr-42-a11ce41",
                "base_sha": "ba5e000",
                "files": "README.md\ntest/fixtures/w16_most.csv",
                "labels": ["1-maintenance"],
                "expected_rc": 0,
                "expected_substring": (
                    "[OK] Multi-PR or unparsed merge_group ref gated by physics-full"
                ),
            },
            id="mq-multi-pr-ref-does-not-misattribute-to-tail-pr",
        ),
        pytest.param(
            {
                "event_name": "merge_group",
                "is_draft": "false",
                "test_tier": "standard",
                "head_ref": "refs/heads/gh-readonly-queue/master/pr-42-a11ce41",
                "base_sha": "ba5e000",
                "files": "README.md\ntest/fixtures/w16_most.csv",
                "labels": ["1-maintenance"],
                "expected_rc": 1,
                "expected_substring": "requires test_tier=physics-full",
            },
            id="mq-multi-pr-ref-without-physics-full-fails",
        ),
        pytest.param(
            {
                "event_name": "merge_group",
                "is_draft": "false",
                "test_tier": "physics-full",
                "head_ref": "refs/heads/gh-readonly-queue/master/batch-deadbeef",
                "base_sha": "ba5e000",
                "files": "README.md",
                "labels": [],
                "expected_rc": 0,
                "expected_substring": (
                    "[OK] Multi-PR or unparsed merge_group ref gated by physics-full"
                ),
            },
            id="mq-unparsed-batch-ref-gated-by-physics-full",
        ),
    ],
)
def test_check_physics_label_gate_enforces_exclusivity_tier_and_queue_refs(
    tmp_path: Path,
    case: dict[str, object],
) -> None:
    """Fixture refreshes require exactly one classification label and a physics-full run."""
    # ARRANGE
    root = Path(__file__).resolve().parents[2]
    workflow = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    job = workflow["jobs"]["check_physics_label"]
    assert job["needs"] == ["determine_matrix"]
    step = job["steps"][0]
    assert json.loads(step["env"]["LABELS_JSON"]) == [
        "0-physics:change",
        "0-numerics:change",
    ]

    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    _write_gh_stub(bin_dir)

    event_name = str(case["event_name"])
    env = {
        **os.environ,
        "PATH": f"{bin_dir}:{os.environ.get('PATH', '')}",
        "EVENT_NAME": event_name,
        "REPO": "UMEP-dev/SUEWS",
        "PR_NUMBER": "42",
        "IS_DRAFT": str(case["is_draft"]),
        "DETERMINE_MATRIX_RESULT": "success",
        "TEST_TIER": str(case["test_tier"]),
        "MERGE_HEAD_REF": str(case["head_ref"]),
        "MERGE_BASE_SHA": str(case["base_sha"]),
        "MERGE_HEAD_SHA": "head111" if event_name == "merge_group" else "",
        "LABELS_JSON": step["env"]["LABELS_JSON"],
        "FAKE_GH_FILES": str(case["files"]),
        "FAKE_GH_LABELS_BY_PR": json.dumps({"42": case["labels"]}),
    }

    # ACT
    result = subprocess.run(
        ["bash", "-c", step["run"]],
        env=env,
        check=False,
        capture_output=True,
        text=True,
    )

    # ASSERT
    assert result.returncode == case["expected_rc"], result.stdout + result.stderr
    assert str(case["expected_substring"]) in result.stdout


@pytest.mark.smoke
@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="determine-matrix.sh and pr-gate.sh run under bash on Linux CI only",
)
def test_determine_matrix_and_pr_gate_enforce_physics_or_numerics_override(
    tmp_path: Path,
) -> None:
    """Classification labels force wheel builds, physics-full tiers, and pr-gate wheel checks."""
    # ARRANGE
    root = Path(__file__).resolve().parents[2]
    assert not (root / ".github/workflows/physics-label-rerun.yml").exists()
    workflow = yaml.safe_load(
        (root / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    assert workflow["jobs"]["determine_matrix"]["outputs"]["physics_change"] == (
        "${{ steps.physics-label.outputs.found || 'false' }}"
    )
    assert (
        "needs.determine_matrix.outputs.physics_change == 'true'"
        in workflow["jobs"]["build_wheels"]["if"]
    )
    pr_gate_job = workflow["jobs"]["pr-gate"]
    assert "determine_matrix" in pr_gate_job["needs"]
    gate_step = next(s for s in pr_gate_job["steps"] if "run" in s)
    assert gate_step["env"]["PHYSICS_CHANGE"] == (
        "${{ needs.determine_matrix.outputs.physics_change || 'false' }}"
    )
    declared_needs = json.loads(
        (root / ".github/ci-metrics-needs.json").read_text(encoding="utf-8")
    )
    assert "Determine build matrix" in declared_needs["PR build validation"]

    base_env = {
        **os.environ,
        "IS_DRAFT": "false",
        "FORTRAN_CHANGED": "false",
        "RUST_CHANGED": "false",
        "PYTHON_CHANGED": "false",
        "UTIL_CHANGED": "false",
        "BUILD_CHANGED": "false",
        "CI_CHANGED": "false",
    }

    # ACT: ready PR and merge_group with PHYSICS_CHANGE=true
    pr_out = tmp_path / "pr_out"
    pr_out.write_text("", encoding="utf-8")
    subprocess.run(
        ["bash", str(root / ".github/scripts/determine-matrix.sh")],
        env={
            **base_env,
            "GITHUB_OUTPUT": str(pr_out),
            "EVENT_NAME": "pull_request",
            "PHYSICS_CHANGE": "true",
        },
        check=True,
        capture_output=True,
        text=True,
    )
    mq_out = tmp_path / "mq_out"
    mq_out.write_text("", encoding="utf-8")
    subprocess.run(
        ["bash", str(root / ".github/scripts/determine-matrix.sh")],
        env={
            **base_env,
            "GITHUB_OUTPUT": str(mq_out),
            "EVENT_NAME": "merge_group",
            "PHYSICS_CHANGE": "true",
        },
        check=True,
        capture_output=True,
        text=True,
    )

    gate_env = {
        **os.environ,
        "DETECT_CHANGES_RESULT": "success",
        "BUILD_WHEELS_RESULT": "skipped",
        "BUILD_MCP_RESULT": "skipped",
        "TEST_BRIDGE_RESULT": "skipped",
        "CHECK_MARKERS_RESULT": "success",
        "NEEDS_BUILD": "false",
        "PHYSICS_CHANGE": "false",
    }
    gate_fail = subprocess.run(
        ["bash", str(root / ".github/scripts/pr-gate.sh")],
        env={**gate_env, "PHYSICS_LABEL_RESULT": "failure"},
        check=False,
        capture_output=True,
        text=True,
    )
    gate_noncode_ok = subprocess.run(
        ["bash", str(root / ".github/scripts/pr-gate.sh")],
        env={**gate_env, "PHYSICS_LABEL_RESULT": "success"},
        check=False,
        capture_output=True,
        text=True,
    )
    gate_labelled_skipped_wheels = subprocess.run(
        ["bash", str(root / ".github/scripts/pr-gate.sh")],
        env={
            **gate_env,
            "PHYSICS_LABEL_RESULT": "success",
            "PHYSICS_CHANGE": "true",
        },
        check=False,
        capture_output=True,
        text=True,
    )
    gate_labelled_built_wheels = subprocess.run(
        ["bash", str(root / ".github/scripts/pr-gate.sh")],
        env={
            **gate_env,
            "PHYSICS_LABEL_RESULT": "success",
            "PHYSICS_CHANGE": "true",
            "BUILD_WHEELS_RESULT": "success",
            "TEST_BRIDGE_RESULT": "success",
        },
        check=False,
        capture_output=True,
        text=True,
    )

    # ASSERT
    pr_kv = dict(
        line.split("=", 1)
        for line in pr_out.read_text(encoding="utf-8").splitlines()
        if "=" in line
    )
    mq_kv = dict(
        line.split("=", 1)
        for line in mq_out.read_text(encoding="utf-8").splitlines()
        if "=" in line
    )
    assert pr_kv["test_tier"] == "physics-full"
    assert mq_kv["test_tier"] == "physics-full"
    # The queue builds Linux (full tier) and Windows (smoke tier, set by the
    # caller), and runs the api lane on Linux only.
    assert [p[1] for p in json.loads(mq_kv["buildplat"])] == ["manylinux", "win"]
    assert [p[1] for p in json.loads(mq_kv["api_buildplat"])] == ["manylinux"]

    assert gate_fail.returncode == 1
    assert "0-physics:change or 0-numerics:change" in gate_fail.stdout
    assert gate_noncode_ok.returncode == 0
    assert "[OK] Code builds correctly skipped" in gate_noncode_ok.stdout
    assert gate_labelled_skipped_wheels.returncode == 1
    assert (
        "[X] Classification label requires wheel and physics build"
        in gate_labelled_skipped_wheels.stdout
    )
    assert gate_labelled_built_wheels.returncode == 0
    assert "[OK] Wheel and physics build passed" in gate_labelled_built_wheels.stdout
