"""Contract tests for declared runtime dependency floors and lowest-direct CI."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import sys
import tomllib

from packaging.requirements import Requirement
import pytest
import yaml

pytestmark = pytest.mark.api

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
def test_dependency_audit_wires_nightly_lowest_direct_resolution() -> None:
    """dependency-audit.yml audits lowest-direct resolution on PRs and nightly schedule."""
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
