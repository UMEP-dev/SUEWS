"""Contract tests for declared runtime dependency floors and lowest-direct CI."""

from __future__ import annotations

import copy
from fnmatch import fnmatchcase
import importlib.util
import json
import os
from pathlib import Path, PurePosixPath
import shutil
import subprocess
import sys
import tomllib
from typing import Any

from packaging.requirements import Requirement
import pytest
import yaml

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]

REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts/security/check_lowest_direct_resolution.py"


def _load_checker_module():
    """Load scripts/security/check_lowest_direct_resolution.py as a module."""
    spec = importlib.util.spec_from_file_location(
        "check_lowest_direct_resolution", SCRIPT_PATH
    )
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


@pytest.mark.smoke
@pytest.mark.core
def test_every_core_runtime_dependency_declares_a_lower_bound() -> None:
    """Every entry in [project].dependencies must declare an explicit lower bound."""
    checker = _load_checker_module()
    deps = checker.read_core_dependencies(REPO_ROOT / "pyproject.toml")
    assert deps, "[project].dependencies must not be empty"

    missing = checker.find_missing_lower_bounds(deps)
    assert missing == [], (
        "Core runtime dependencies missing explicit lower bounds: " + ", ".join(missing)
    )

    # Cross-check with packaging.requirements.Requirement so PEP 508 syntax stays valid.
    for raw in deps:
        req = Requirement(raw)
        ops = {spec.operator for spec in req.specifier}
        assert ops & {">=", ">", "==", "~="}, (
            f"Dependency {raw!r} has no lower-bound operator in {req.specifier!s}"
        )


@pytest.mark.smoke
@pytest.mark.core
def test_pandas_and_scipy_preserve_caps_and_declare_floors_on_every_marker() -> None:
    """pandas and scipy keep manylinux2014 caps while declaring lower bounds everywhere."""
    pyproject = tomllib.loads(
        (REPO_ROOT / "pyproject.toml").read_text(encoding="utf-8")
    )
    deps = [Requirement(raw) for raw in pyproject["project"]["dependencies"]]

    pandas_reqs = [req for req in deps if req.name.lower() == "pandas"]
    scipy_reqs = [req for req in deps if req.name.lower() == "scipy"]
    assert len(pandas_reqs) == 2
    assert len(scipy_reqs) == 2

    for req in pandas_reqs:
        ops = {spec.operator for spec in req.specifier}
        assert ">=" in ops, f"pandas entry missing lower bound: {req}"
    assert any(
        any(spec.operator == "<" and spec.version == "3" for spec in req.specifier)
        and "Linux" in str(req.marker)
        for req in pandas_reqs
    ), "Linux cp312 manylinux2014 pandas<3 cap must remain intact"

    for req in scipy_reqs:
        ops = {spec.operator for spec in req.specifier}
        assert ">=" in ops, f"scipy entry missing lower bound: {req}"
    assert any(
        any(spec.operator == "<" and spec.version == "1.15" for spec in req.specifier)
        and "Linux" in str(req.marker)
        for req in scipy_reqs
    ), "Linux cp312 manylinux2014 scipy<1.15 cap must remain intact"


@pytest.mark.core
def test_checker_helpers_flag_unpinned_dependencies_and_uv_warnings(
    tmp_path: Path,
) -> None:
    """Static floor checker and uv output parser catch missing bounds and warnings."""
    checker = _load_checker_module()

    sample_reqs = [
        "pandas<3; python_version < '3.13' and platform_system == 'Linux'",
        "scipy",
        "numpy>=1.22",
        "pydantic[email]>=2.12",
        "click~=8.1",
        "packaging!=22.0",
    ]
    assert checker.find_missing_lower_bounds(sample_reqs) == [
        "pandas<3; python_version < '3.13' and platform_system == 'Linux'",
        "scipy",
        "packaging!=22.0",
    ]

    resolved, errors = checker.parse_uv_compile_result(
        0,
        "pandas==2.2.2\nscipy==1.13.0\n",
        "Resolved 2 packages in 10ms\n",
    )
    assert errors == []
    assert resolved == {"pandas": "2.2.2", "scipy": "1.13.0"}

    _, warn_errors = checker.parse_uv_compile_result(
        0,
        "pandas==2.2.2\n",
        "warning: The direct dependency `scipy` is unpinned.\nResolved 1 package\n",
    )
    assert any("unpinned" in err for err in warn_errors)

    _, fail_errors = checker.parse_uv_compile_result(1, "", "No solution found")
    assert any("exited with status 1" in err for err in fail_errors)

    bad_pyproject = tmp_path / "pyproject.toml"
    bad_pyproject.write_text(
        '[project]\nname = "demo"\ndependencies = ["pandas<3", "scipy>=1.13"]\n',
        encoding="utf-8",
    )
    assert checker.main(["--pyproject", str(bad_pyproject), "--check-floors-only"]) == 1
    assert (
        checker.main([
            "--pyproject",
            str(REPO_ROOT / "pyproject.toml"),
            "--check-floors-only",
        ])
        == 0
    )


@pytest.mark.core
def test_ci_workflows_wire_lowest_direct_resolution_and_smoke_tests() -> None:
    """CI audits lowest-direct resolution and gates built-wheel floor smoke tests."""
    dep_audit = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/dependency-audit.yml").read_text(
            encoding="utf-8"
        )
    )
    assert dep_audit["permissions"] == {"contents": "read"}
    dep_triggers = dep_audit[True] if True in dep_audit else dep_audit["on"]
    assert "pull_request" in dep_triggers
    assert "schedule" in dep_triggers
    crons = [entry["cron"] for entry in dep_triggers["schedule"]]
    assert any(cron.strip().endswith("* * *") for cron in crons), (
        f"dependency-audit.yml schedule must run nightly, got: {crons}"
    )

    lowest_job = dep_audit["jobs"]["lowest-direct-resolution"]
    assert "if" not in lowest_job, (
        "lowest-direct-resolution must run unconditionally on schedule and PR triggers"
    )
    dep_runs = "\n".join(str(step.get("run", "")) for step in lowest_job["steps"])
    assert "scripts/security/check_lowest_direct_resolution.py" in dep_runs
    for pyver in ("3.12", "3.13", "3.14", "3.15"):
        assert f"--python-version {pyver}" in dep_runs
    assert "--only-binary-all" in dep_runs
    assert "--resolution=lowest-direct" in dep_runs
    assert "--only-binary :all:" in dep_runs
    assert "scripts/security/audit_python_startup.py" in dep_runs

    build_wf = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    # The floor job is chained inside the Linux build_wheels call, behind that
    # wheel only, so it starts when the wheel exists; its result rolls into
    # build_wheels, which report_scheduled_run and pr-gate read.
    wheels_wf = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-wheels-reusable.yml").read_text(
            encoding="utf-8"
        )
    )
    wheel_job = wheels_wf["jobs"]["lowest_direct_deps"]
    assert wheel_job["name"] == "Lowest-direct dependency smoke tests (Linux cp312)"
    assert wheel_job["needs"] == "build"
    assert wheel_job["if"] == "inputs.run_lowest_direct_deps"
    assert "continue-on-error" not in wheel_job
    upload_marker = next(
        step
        for step in wheel_job["steps"]
        if str(step.get("uses", "")).startswith("actions/upload-artifact@")
    )
    assert upload_marker["with"]["name"] == "lowest-direct-deps-result"
    assert upload_marker["with"]["path"] == "lowest-direct-deps-result.json"
    assert upload_marker["with"]["if-no-files-found"] == "error"

    guard_job = wheels_wf["jobs"]["lowest_direct_deps_guard"]
    assert guard_job["name"] == "Require lowest-direct dependency smoke tests"
    assert guard_job["needs"] == ["build", "lowest_direct_deps"]
    assert (
        guard_job["if"]
        == "always() && inputs.run_lowest_direct_deps && needs.build.result == 'success'"
    )
    assert "continue-on-error" not in guard_job

    trigger = build_wf["jobs"]["build_wheels"]["with"]["run_lowest_direct_deps"]
    assert "matrix.buildplat[1] == 'manylinux'" in trigger
    assert "dependency-audit-changed == 'true'" in trigger
    assert "github.event_name == 'schedule'" in trigger
    assert "inputs.lowest_direct_deps == true" in trigger
    assert "lowest_direct_deps" not in build_wf["jobs"]
    assert "build_wheels" in build_wf["jobs"]["report_scheduled_run"]["needs"]

    set_matrix_step = next(
        step
        for step in build_wf["jobs"]["determine_matrix"]["steps"]
        if step.get("id") == "set-matrix"
    )
    assert (
        set_matrix_step["env"]["INPUT_LOWEST_DIRECT_DEPS"]
        == "${{ inputs.lowest_direct_deps }}"
    )

    pr_gate = build_wf["jobs"]["pr-gate"]
    assert "build_wheels" in pr_gate["needs"]
    pr_gate_checkout = next(
        step
        for step in pr_gate["steps"]
        if str(step.get("uses", "")).startswith("actions/checkout@")
    )
    assert (
        pr_gate_checkout["with"]["ref"]
        == "${{ github.event.pull_request.base.sha || github.sha }}"
    )
    marker_download = next(
        step
        for step in pr_gate["steps"]
        if str(step.get("uses", "")).startswith("actions/download-artifact@")
        and step.get("with", {}).get("name") == "lowest-direct-deps-result"
    )
    assert marker_download["continue-on-error"] is True
    assert (
        marker_download["if"]
        == "needs.build_wheels.result == 'success' && needs.detect-changes.outputs.dependency-audit-changed == 'true'"
    )
    resolve_step = next(
        step for step in pr_gate["steps"] if step.get("id") == "lowest-direct-result"
    )
    assert "lowest-direct-deps-result.json" in resolve_step["run"]
    pr_gate_step = next(
        step for step in pr_gate["steps"] if step.get("name") == "Check build results"
    )
    assert (
        pr_gate_step["env"]["LOWEST_DIRECT_DEPS_RESULT"]
        == "${{ steps.lowest-direct-result.outputs.result }}"
    )
    assert (
        pr_gate_step["env"]["LOWEST_DIRECT_DEPS_REQUIRED"]
        == "${{ needs.build_wheels.result == 'success' && needs.detect-changes.outputs.dependency-audit-changed == 'true' }}"
    )

    filters = yaml.safe_load(
        (REPO_ROOT / ".github/path-filters.yml").read_text(encoding="utf-8")
    )
    assert ".github/scripts/pr-gate.sh" in filters["dependency_audit"]
    assert all(
        not str(pat).startswith("!") for pats in filters.values() for pat in pats
    )

    wheel_runs = "\n".join(str(step.get("run", "")) for step in wheel_job["steps"])
    assert "--resolution=lowest-direct" in wheel_runs
    assert "lowest-constraints.txt" in wheel_runs
    assert "uv pip check" in wheel_runs
    assert 'export PATH="$PWD/.lowest-venv/bin:$PATH"' in wheel_runs
    assert "command -v suews-validate" in wheel_runs
    assert "$PWD/.lowest-venv/bin/suews-validate" in wheel_runs
    assert '-m "(smoke or core) and not slow and not qgis"' in wheel_runs

    declared_needs = json.loads(
        (REPO_ROOT / ".github/ci-metrics-needs.json").read_text(encoding="utf-8")
    )
    leaf = f"Build and test (manylinux x86_64) / {wheel_job['name']}"
    assert leaf in declared_needs
    assert "Build and test (manylinux x86_64) / wheel | *" in declared_needs[leaf]
    guard_leaf = f"Build and test (manylinux x86_64) / {guard_job['name']}"
    assert guard_leaf in declared_needs
    assert leaf in declared_needs[guard_leaf]
    # The gate reaches it through the build_wheels group.
    assert "Build and test (*) / *" in declared_needs["PR build validation"]


@pytest.mark.core
@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="pr-gate and determine-matrix shell steps run under bash on Linux CI",
)
def test_pr_gate_independent_lowest_direct_result_fails_closed_on_wheel_pass_floor_skip(
    tmp_path: Path,
) -> None:
    """When build_wheels passes on a dependency-audit change without the floor marker, pr-gate fails closed (#2284)."""
    build_wf = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    resolve_step = next(
        step
        for step in build_wf["jobs"]["pr-gate"]["steps"]
        if step.get("id") == "lowest-direct-result"
    )
    resolve_script = resolve_step["run"]

    def _resolve(workdir: Path, *, wheel_result: str, dep_changed: str) -> str:
        out_file = workdir / "gh_output.txt"
        out_file.write_text("", encoding="utf-8")
        env = {
            **os.environ,
            "GITHUB_OUTPUT": str(out_file),
            "BUILD_WHEELS_RESULT": wheel_result,
            "DEPENDENCY_AUDIT_CHANGED": dep_changed,
        }
        subprocess.run(
            ["bash", "-euo", "pipefail", "-c", resolve_script],
            cwd=workdir,
            env=env,
            check=True,
            capture_output=True,
            text=True,
        )
        return _parse_github_output(out_file)["result"]

    # Case 1: wheel succeeded, dependency-audit changed, but floor job was skipped (no marker) -> skipped -> pr-gate fails.
    case_skip = tmp_path / "wheel_pass_floor_skip"
    case_skip.mkdir()
    resolved_skip = _resolve(case_skip, wheel_result="success", dep_changed="true")
    assert resolved_skip == "skipped"

    filters = yaml.safe_load(
        (REPO_ROOT / ".github/path-filters.yml").read_text(encoding="utf-8")
    )
    pipeline_skip = _run_workflow_path_pipeline(
        build_wf,
        filters,
        event_name="pull_request",
        changed_paths=["pyproject.toml"],
        tmp_path=tmp_path,
        case_tag="wheel_pass_floor_skip_gate",
        pyproject_flags=(False, True, False),
        build_wheels_outcome="success",
        lowest_direct_outcome=resolved_skip,
    )
    assert pipeline_skip["lowest_direct_required"] is True
    assert pipeline_skip["gate_returncode"] == 1, pipeline_skip["gate_stdout"]

    # Case 2: wheel succeeded and floor job uploaded valid success marker -> success -> pr-gate passes.
    case_ok = tmp_path / "wheel_pass_floor_ok"
    marker_dir = case_ok / "_lowest_direct_deps"
    marker_dir.mkdir(parents=True)
    (marker_dir / "lowest-direct-deps-result.json").write_text(
        '{"result": "success"}\n', encoding="utf-8"
    )
    resolved_ok = _resolve(case_ok, wheel_result="success", dep_changed="true")
    assert resolved_ok == "success"
    pipeline_ok = _run_workflow_path_pipeline(
        build_wf,
        filters,
        event_name="pull_request",
        changed_paths=["pyproject.toml"],
        tmp_path=tmp_path,
        case_tag="wheel_pass_floor_ok_gate",
        pyproject_flags=(False, True, False),
        build_wheels_outcome="success",
        lowest_direct_outcome=resolved_ok,
    )
    assert pipeline_ok["gate_returncode"] == 0, pipeline_ok["gate_stdout"]

    # Case 3: corrupted/non-success marker payload also fails closed.
    case_bad = tmp_path / "wheel_pass_floor_bad_marker"
    bad_dir = case_bad / "_lowest_direct_deps"
    bad_dir.mkdir(parents=True)
    (bad_dir / "lowest-direct-deps-result.json").write_text(
        '{"result": "skipped"}\n', encoding="utf-8"
    )
    assert _resolve(case_bad, wheel_result="success", dep_changed="true") == "skipped"


@pytest.mark.core
@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="determine-matrix.sh runs under bash on Linux CI",
)
def test_custom_dispatch_rejects_lowest_direct_deps_without_linux(
    tmp_path: Path,
) -> None:
    """Custom workflow_dispatch with lowest_direct_deps=true must include Linux (#2284)."""
    out_file = tmp_path / "matrix_out.txt"
    base_env = {
        **os.environ,
        "GITHUB_OUTPUT": str(out_file),
        "EVENT_NAME": "workflow_dispatch",
        "INPUT_MATRIX_CONFIG": "custom",
        "INPUT_PLAT_MACOS_INTEL": "false",
        "INPUT_PLAT_MACOS_ARM": "false",
        "INPUT_PLAT_WINDOWS": "true",
        "INPUT_PY312": "true",
        "INPUT_PY313": "false",
        "INPUT_PY314": "false",
        "INPUT_PY315": "false",
        "INPUT_TEST_TIER": "smoke",
        "INPUT_LOWEST_DIRECT_DEPS": "true",
        "FORTRAN_CHANGED": "false",
        "RUST_CHANGED": "false",
        "PYTHON_CHANGED": "false",
        "UTIL_CHANGED": "false",
        "BUILD_CHANGED": "false",
        "PR_PYPROJECT": str(REPO_ROOT / "pyproject.toml"),
    }

    out_file.write_text("", encoding="utf-8")
    proc_no_linux = subprocess.run(
        ["bash", str(REPO_ROOT / ".github/scripts/determine-matrix.sh")],
        env={**base_env, "INPUT_PLAT_LINUX": "false"},
        check=False,
        capture_output=True,
        text=True,
    )
    assert proc_no_linux.returncode == 1
    assert "lowest_direct_deps=true requires plat_linux=true" in (
        proc_no_linux.stdout + proc_no_linux.stderr
    )

    out_file.write_text("", encoding="utf-8")
    proc_with_linux = subprocess.run(
        ["bash", str(REPO_ROOT / ".github/scripts/determine-matrix.sh")],
        env={**base_env, "INPUT_PLAT_LINUX": "true"},
        check=False,
        capture_output=True,
        text=True,
    )
    assert proc_with_linux.returncode == 0, proc_with_linux.stdout + proc_with_linux.stderr
    matrix_out = _parse_github_output(out_file)
    assert ["ubuntu-latest", "manylinux", "x86_64"] in json.loads(
        matrix_out["buildplat"]
    )


@pytest.mark.core
def test_checker_cli_supports_multiple_python_versions_and_binary_only(
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """CLI iterates over every --python-version and propagates --only-binary-all."""
    checker = _load_checker_module()
    calls: list[tuple[str, str, bool]] = []

    def _fake_check(
        requirements, *, python_version: str, platform: str, only_binary_all: bool
    ):
        assert requirements
        calls.append((python_version, platform, only_binary_all))
        return {
            "pandas": "2.2.2",
            "scipy": "1.13.0",
            "numpy": "1.26.0",
            "pydantic": "2.12.0",
            "pyarrow": "20.0.0",
        }, []

    monkeypatch.setattr(checker, "check_platform_resolution", _fake_check)
    rc = checker.main([
        "--pyproject",
        str(REPO_ROOT / "pyproject.toml"),
        "--python-version",
        "3.12",
        "--python-version",
        "3.14",
        "--platform",
        "x86_64-unknown-linux-gnu",
        "--only-binary-all",
    ])
    assert rc == 0
    assert calls == [
        ("3.12", "x86_64-unknown-linux-gnu", True),
        ("3.14", "x86_64-unknown-linux-gnu", True),
    ]
    out = capsys.readouterr().out
    assert "Python 3.12 (x86_64-unknown-linux-gnu, binary-only)" in out
    assert "Python 3.14 (x86_64-unknown-linux-gnu, binary-only)" in out


_PATH_FILTERED_PRODUCERS = (
    ("RUST_TEST_RESULT", "RUST_TEST_CHANGED", "rust_test"),
    ("RUST_CLIPPY_RESULT", "RUST_CLIPPY_CHANGED", "rust_clippy"),
    ("ENCODING_AUDIT_RESULT", "ENCODING_AUDIT_CHANGED", "encoding_audit"),
    ("WORKFLOW_SECURITY_RESULT", "WORKFLOW_SECURITY_CHANGED", "workflow_security"),
    ("DEPENDENCY_AUDIT_RESULT", "DEPENDENCY_AUDIT_CHANGED", "dependency_audit"),
    ("FORTRAN_UNINIT_RESULT", "FORTRAN_UNINIT_CHANGED", "fortran_uninit"),
    ("RUST_CAPI_OBSERVER_RESULT", "RUST_CAPI_OBSERVER_CHANGED", "rust_capi_observer"),
)


def _load_classify_pyproject_module():
    """Load .github/scripts/classify_pyproject.py as a module."""
    script_path = REPO_ROOT / ".github/scripts/classify_pyproject.py"
    spec = importlib.util.spec_from_file_location(
        "classify_pyproject_floors", script_path
    )
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _match_path_filter_pattern(path: str, pattern: str) -> bool:
    """Match a POSIX repository path against a positive path-filter glob."""
    assert not pattern.startswith("!"), f"negation pattern forbidden: {pattern!r}"
    if pattern.endswith("/**"):
        prefix = pattern[:-3]
        return path.startswith(prefix + "/")
    if pattern == "**/*.py":
        return path.endswith(".py")
    if pattern == "*.py":
        return "/" not in path and path.endswith(".py")
    if "**" in pattern:
        return PurePosixPath(path).match(pattern)
    return fnmatchcase(path, pattern)


def _classify_changed_paths(
    filters: dict[str, list[str]],
    changed_paths: list[str],
) -> dict[str, bool]:
    """Evaluate every path-filter category against a list of changed files."""
    return {
        category: any(
            _match_path_filter_pattern(path, pat)
            for path in changed_paths
            for pat in patterns
        )
        for category, patterns in filters.items()
    }


def _parse_github_output(path: Path) -> dict[str, str]:
    """Parse key=value pairs written to a GITHUB_OUTPUT file."""
    return dict(
        line.split("=", 1)
        for line in path.read_text(encoding="utf-8").splitlines()
        if "=" in line
    )


def _run_workflow_path_pipeline(
    build_wf: dict[str, Any],
    filters: dict[str, list[str]],
    *,
    event_name: str,
    changed_paths: list[str],
    tmp_path: Path,
    case_tag: str,
    pyproject_flags: tuple[bool, bool, bool] = (False, False, False),
    build_wheels_outcome: str = "success",
    lowest_direct_outcome: str = "skipped",
    dependency_audit_outcome: str | None = None,
) -> dict[str, Any]:
    """Run path filters, decide step, determine-matrix.sh, and pr-gate.sh end to end."""
    flags = _classify_changed_paths(filters, changed_paths)
    decide_step = next(
        step
        for step in build_wf["jobs"]["detect-changes"]["steps"]
        if step.get("id") == "decide"
    )
    decide_out_file = tmp_path / f"{case_tag}_decide_out"
    decide_out_file.write_text("", encoding="utf-8")
    build_rel, pkg_rel, test_rel = pyproject_flags
    decide_env = {
        **os.environ,
        "GITHUB_OUTPUT": str(decide_out_file),
        "FILTER_BUILD": str(flags["build"]).lower(),
        "BUILD_RELEVANT": str(build_rel).lower(),
        "FILTER_PYTHON": str(flags["python"]).lower(),
        "PACKAGE_RELEVANT": str(pkg_rel).lower(),
        "FILTER_FORTRAN": str(flags["fortran"]).lower(),
        "FILTER_RUST": str(flags["rust"]).lower(),
        "FILTER_UTIL": str(flags["util"]).lower(),
        "FILTER_CI": str(flags["ci"]).lower(),
        "FILTER_TESTS": str(flags["tests"]).lower(),
        "TEST_RELEVANT": str(test_rel).lower(),
    }
    subprocess.run(
        ["bash", "-c", decide_step["run"]],
        env=decide_env,
        check=True,
        capture_output=True,
        text=True,
    )
    decide_out = _parse_github_output(decide_out_file)

    matrix_out_file = tmp_path / f"{case_tag}_matrix_out"
    matrix_out_file.write_text("", encoding="utf-8")
    matrix_env = {
        **os.environ,
        "GITHUB_OUTPUT": str(matrix_out_file),
        "EVENT_NAME": event_name,
        "IS_DRAFT": "false",
        "FORTRAN_CHANGED": str(flags["fortran"]).lower(),
        "RUST_CHANGED": str(flags["rust"]).lower(),
        "PYTHON_CHANGED": decide_out["python-changed"],
        "UTIL_CHANGED": str(flags["util"]).lower(),
        "BUILD_CHANGED": decide_out["build-changed"],
        "CI_CHANGED": str(flags["ci"]).lower(),
        "TESTS_CHANGED": decide_out["tests-changed"],
        "PHYSICS_CHANGE": "false",
        "PR_PYPROJECT": str(REPO_ROOT / "pyproject.toml"),
    }
    subprocess.run(
        ["bash", str(REPO_ROOT / ".github/scripts/determine-matrix.sh")],
        env=matrix_env,
        check=True,
        capture_output=True,
        text=True,
    )
    matrix_out = _parse_github_output(matrix_out_file)

    needs_build = decide_out["needs-build"] == "true"
    dep_changed = flags["dependency_audit"]
    wheel_result = build_wheels_outcome if needs_build else "skipped"
    mcp_result = "success" if needs_build else "skipped"
    lowest_direct_triggered = (wheel_result == "success") and dep_changed
    lowest_direct_required = lowest_direct_triggered

    gate_env = dict(os.environ)
    for var in (
        "EVENT_NAME",
        "LOWEST_DIRECT_DEPS_RESULT",
        "LOWEST_DIRECT_DEPS_REQUIRED",
        "SCHEMA_AUDIT_RESULT",
        "DATA_INTERFACE_AUDIT_RESULT",
        *(v for triple in _PATH_FILTERED_PRODUCERS for v in triple[:2]),
    ):
        gate_env.pop(var, None)

    gate_env.update({
        "EVENT_NAME": event_name,
        "DETECT_CHANGES_RESULT": "success",
        "NEEDS_BUILD": decide_out["needs-build"],
        "PHYSICS_CHANGE": "false",
        "BUILD_WHEELS_RESULT": wheel_result,
        "BUILD_MCP_RESULT": mcp_result,
        "TEST_BRIDGE_RESULT": wheel_result,
        "CHECK_MARKERS_RESULT": "success",
        "PHYSICS_LABEL_RESULT": "success",
        "SCHEMA_AUDIT_RESULT": "success",
        "DATA_INTERFACE_AUDIT_RESULT": "success",
        "LOWEST_DIRECT_DEPS_REQUIRED": str(lowest_direct_required).lower(),
        "LOWEST_DIRECT_DEPS_RESULT": lowest_direct_outcome,
    })
    for result_var, changed_var, category in _PATH_FILTERED_PRODUCERS:
        cat_changed = flags[category]
        gate_env[changed_var] = str(cat_changed).lower()
        if category == "dependency_audit" and dependency_audit_outcome is not None:
            gate_env[result_var] = dependency_audit_outcome
        else:
            gate_env[result_var] = "success" if cat_changed else "skipped"

    gate_proc = subprocess.run(
        ["bash", str(REPO_ROOT / ".github/scripts/pr-gate.sh")],
        env=gate_env,
        check=False,
        capture_output=True,
        text=True,
    )
    return {
        "flags": flags,
        "decide": decide_out,
        "matrix": matrix_out,
        "wheel_result": wheel_result,
        "lowest_direct_triggered": lowest_direct_triggered,
        "lowest_direct_required": lowest_direct_required,
        "gate_returncode": gate_proc.returncode,
        "gate_stdout": gate_proc.stdout,
        "gate_stderr": gate_proc.stderr,
    }


@pytest.mark.core
@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="detect-changes decide step, determine-matrix.sh, and pr-gate.sh run under bash on Linux CI",
)
@pytest.mark.parametrize("event_name", ["pull_request", "merge_group"])
def test_path_classification_and_gate_triggers_for_normal_docs_and_gate_only_changes(
    tmp_path: Path,
    event_name: str,
) -> None:
    """Path filters, build decisions, matrix selection, and pr-gate agree across PR and merge_group."""
    build_wf = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    filters = yaml.safe_load(
        (REPO_ROOT / ".github/path-filters.yml").read_text(encoding="utf-8")
    )

    # Confirm workflow output and trigger wiring match the evaluated chain.
    detect_outputs = build_wf["jobs"]["detect-changes"]["outputs"]
    assert detect_outputs["dependency-audit-changed"] == (
        "${{ steps.filter.outputs.dependency_audit }}"
    )
    assert detect_outputs["needs-build"] == "${{ steps.decide.outputs.needs-build }}"
    assert (
        "needs.detect-changes.outputs.needs-build == 'true'"
        in build_wf["jobs"]["build_wheels"]["if"]
    )
    dep_job_if = build_wf["jobs"]["dependency_audit"]["if"]
    assert "pull_request" in dep_job_if and "merge_group" in dep_job_if
    assert (
        "needs.detect-changes.outputs.dependency-audit-changed == 'true'" in dep_job_if
    )
    wheel_job_if = build_wf["jobs"]["build_wheels"]["with"]["run_lowest_direct_deps"]
    assert "pull_request" in wheel_job_if and "merge_group" in wheel_job_if
    assert (
        "needs.detect-changes.outputs.dependency-audit-changed == 'true'"
        in wheel_job_if
    )

    # 1. Normal Python source change: builds Linux cp312 wheel, skips floor lane.
    normal_skip = _run_workflow_path_pipeline(
        build_wf,
        filters,
        event_name=event_name,
        changed_paths=["src/supy/_load.py"],
        tmp_path=tmp_path,
        case_tag=f"{event_name}_normal_skip",
        lowest_direct_outcome="skipped",
    )
    assert normal_skip["flags"]["python"] is True
    assert normal_skip["flags"]["dependency_audit"] is False
    assert normal_skip["decide"]["needs-build"] == "true"
    assert ["ubuntu-latest", "manylinux", "x86_64"] in json.loads(
        normal_skip["matrix"]["buildplat"]
    )
    assert json.loads(normal_skip["matrix"]["python"]) == ["cp312"]
    assert normal_skip["lowest_direct_required"] is False
    assert normal_skip["gate_returncode"] == 0, normal_skip["gate_stdout"]

    normal_upstream_fail = _run_workflow_path_pipeline(
        build_wf,
        filters,
        event_name=event_name,
        changed_paths=["src/supy/_load.py"],
        tmp_path=tmp_path,
        case_tag=f"{event_name}_normal_wheel_fail",
        build_wheels_outcome="failure",
        lowest_direct_outcome="skipped",
    )
    assert normal_upstream_fail["gate_returncode"] == 1

    # 2. Docs-only change: skips wheel build and floor lane; gate accepts skip.
    docs_only = _run_workflow_path_pipeline(
        build_wf,
        filters,
        event_name=event_name,
        changed_paths=["docs/source/index.rst"],
        tmp_path=tmp_path,
        case_tag=f"{event_name}_docs",
        lowest_direct_outcome="skipped",
    )
    assert docs_only["flags"]["docs"] is True
    assert docs_only["flags"]["dependency_audit"] is False
    assert docs_only["decide"]["needs-build"] == "false"
    assert docs_only["wheel_result"] == "skipped"
    assert docs_only["lowest_direct_required"] is False
    assert docs_only["gate_returncode"] == 0, docs_only["gate_stdout"]

    # 3. Gate-only change (.github/scripts/pr-gate.sh): builds Linux cp312 wheel,
    # triggers dependency_audit and lowest_direct_deps, and fails closed on non-success.
    for outcome, expected_rc in (
        ("success", 0),
        ("skipped", 1),
        ("failure", 1),
        ("cancelled", 1),
    ):
        gate_only = _run_workflow_path_pipeline(
            build_wf,
            filters,
            event_name=event_name,
            changed_paths=[".github/scripts/pr-gate.sh"],
            tmp_path=tmp_path,
            case_tag=f"{event_name}_gate_only_{outcome}",
            lowest_direct_outcome=outcome,
        )
        assert gate_only["flags"]["ci"] is True
        assert gate_only["flags"]["dependency_audit"] is True
        assert gate_only["decide"]["needs-build"] == "true"
        buildplat = json.loads(gate_only["matrix"]["buildplat"])
        assert ["ubuntu-latest", "manylinux", "x86_64"] in buildplat
        assert len(buildplat) == (1 if event_name == "pull_request" else 2)
        assert json.loads(gate_only["matrix"]["python"]) == ["cp312"]
        assert gate_only["matrix"]["test_tier"] == "standard"
        assert gate_only["lowest_direct_triggered"] is True
        assert gate_only["lowest_direct_required"] is True
        assert gate_only["gate_returncode"] == expected_rc, (
            event_name,
            outcome,
            gate_only["gate_stdout"],
        )

    # 4. Generic build plumbing stays intentionally outside the dependency filter.
    generic_ci = _run_workflow_path_pipeline(
        build_wf,
        filters,
        event_name=event_name,
        changed_paths=[".github/workflows/build-wheels-reusable.yml"],
        tmp_path=tmp_path,
        case_tag=f"{event_name}_reusable_wheels",
        lowest_direct_outcome="skipped",
    )
    assert generic_ci["flags"]["ci"] is True
    assert generic_ci["flags"]["dependency_audit"] is False
    assert generic_ci["decide"]["needs-build"] == "true"
    assert generic_ci["lowest_direct_required"] is False
    assert generic_ci["gate_returncode"] == 0, generic_ci["gate_stdout"]


@pytest.mark.core
@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="detect-changes decide step, determine-matrix.sh, and pr-gate.sh run under bash on Linux CI",
)
@pytest.mark.parametrize("event_name", ["pull_request", "merge_group"])
def test_pyproject_content_classification_gates_lowest_direct_floor_check(
    tmp_path: Path,
    event_name: str,
) -> None:
    """Runtime-floor pyproject edits require lowest_direct_deps while description-only edits skip it."""
    build_wf = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    filters = yaml.safe_load(
        (REPO_ROOT / ".github/path-filters.yml").read_text(encoding="utf-8")
    )
    base_pyproject = tomllib.loads(
        (REPO_ROOT / "pyproject.toml").read_text(encoding="utf-8")
    )
    classifier = _load_classify_pyproject_module()

    runtime_head = copy.deepcopy(base_pyproject)
    runtime_head["project"]["dependencies"] = [
        dep.replace("pandas>=2.2.2", "pandas>=2.2.3")
        for dep in runtime_head["project"]["dependencies"]
    ]
    runtime_cls = classifier.classify_pyproject(base_pyproject, runtime_head)
    assert (
        runtime_cls.build_relevant,
        runtime_cls.package_relevant,
        runtime_cls.test_relevant,
    ) == (False, True, False)

    for outcome, expected_rc in (("success", 0), ("skipped", 1)):
        py_runtime = _run_workflow_path_pipeline(
            build_wf,
            filters,
            event_name=event_name,
            changed_paths=["pyproject.toml"],
            tmp_path=tmp_path,
            case_tag=f"{event_name}_pyproject_runtime_{outcome}",
            pyproject_flags=(
                runtime_cls.build_relevant,
                runtime_cls.package_relevant,
                runtime_cls.test_relevant,
            ),
            lowest_direct_outcome=outcome,
        )
        assert py_runtime["decide"]["needs-build"] == "true"
        assert py_runtime["lowest_direct_required"] is True
        assert py_runtime["gate_returncode"] == expected_rc

    desc_head = copy.deepcopy(base_pyproject)
    desc_head["project"]["description"] = "Metadata-only description change"
    desc_cls = classifier.classify_pyproject(base_pyproject, desc_head)
    assert (
        desc_cls.build_relevant,
        desc_cls.package_relevant,
        desc_cls.test_relevant,
    ) == (False, False, False)

    py_desc_ok = _run_workflow_path_pipeline(
        build_wf,
        filters,
        event_name=event_name,
        changed_paths=["pyproject.toml"],
        tmp_path=tmp_path,
        case_tag=f"{event_name}_pyproject_desc_ok",
        pyproject_flags=(
            desc_cls.build_relevant,
            desc_cls.package_relevant,
            desc_cls.test_relevant,
        ),
        lowest_direct_outcome="skipped",
        dependency_audit_outcome="success",
    )
    assert py_desc_ok["flags"]["dependency_audit"] is True
    assert py_desc_ok["decide"]["needs-build"] == "false"
    assert py_desc_ok["wheel_result"] == "skipped"
    assert py_desc_ok["lowest_direct_required"] is False
    assert py_desc_ok["gate_returncode"] == 0, py_desc_ok["gate_stdout"]

    py_desc_audit_skip = _run_workflow_path_pipeline(
        build_wf,
        filters,
        event_name=event_name,
        changed_paths=["pyproject.toml"],
        tmp_path=tmp_path,
        case_tag=f"{event_name}_pyproject_desc_audit_skip",
        pyproject_flags=(
            desc_cls.build_relevant,
            desc_cls.package_relevant,
            desc_cls.test_relevant,
        ),
        lowest_direct_outcome="skipped",
        dependency_audit_outcome="skipped",
    )
    assert py_desc_audit_skip["gate_returncode"] == 1
