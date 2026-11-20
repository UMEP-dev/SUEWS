"""Contract tests for CI classifiers, path filters, ready test tiers, and folded audits."""

from __future__ import annotations

import copy
from datetime import UTC, datetime
from fnmatch import fnmatchcase
import importlib.util
import json
import os
from pathlib import Path, PurePosixPath
import re
import shutil
import subprocess
import sys
import tomllib
from typing import Any

import pytest
import yaml

from scripts.suews.analyse_ci_run import analyse_run

pytestmark = pytest.mark.api

REPO_ROOT = Path(__file__).resolve().parents[2]


def _load_classify_pyproject_module():
    script_path = REPO_ROOT / ".github/scripts/classify_pyproject.py"
    spec = importlib.util.spec_from_file_location("classify_pyproject", script_path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _base_pyproject() -> dict[str, Any]:
    return tomllib.loads((REPO_ROOT / "pyproject.toml").read_text(encoding="utf-8"))


def _parse_github_output(path: Path) -> dict[str, str]:
    return dict(
        line.split("=", 1)
        for line in path.read_text(encoding="utf-8").splitlines()
        if "=" in line
    )


def _match_picomatch_pattern(path: str, pattern: str) -> bool:
    """Match repository-relative POSIX path against positive path-filter globs."""
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


def _matches_category(filters: dict[str, list[str]], category: str, path: str) -> bool:
    return any(_match_picomatch_pattern(path, pat) for pat in filters[category])


def _write_gh_stub(bin_dir: Path) -> None:
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
        "if not m_lbl:\n"
        "    sys.exit(f'unexpected gh call: {args}')\n"
        "raw = json.loads(os.environ.get('FAKE_GH_LABELS_JSON', '[]'))\n"
        "pages = raw if (raw and isinstance(raw[0], list)) else [raw]\n"
        "shown = pages if paginate else pages[:1]\n"
        "for page in shown:\n"
        "    sys.stdout.write(json.dumps(page) + '\\n')\n",
        encoding="utf-8",
    )
    stub.chmod(0o755)


def _run_determine_matrix(
    tmp_path: Path,
    env_overrides: dict[str, str],
    *,
    drop_vars: tuple[str, ...] = (),
) -> dict[str, str]:
    gh_out = tmp_path / "github_output"
    gh_out.write_text("", encoding="utf-8")
    env = {
        **os.environ,
        "GITHUB_OUTPUT": str(gh_out),
        "EVENT_NAME": "pull_request",
        "IS_DRAFT": "false",
        "FORTRAN_CHANGED": "false",
        "RUST_CHANGED": "false",
        "PYTHON_CHANGED": "false",
        "UTIL_CHANGED": "false",
        "BUILD_CHANGED": "false",
        "CI_CHANGED": "false",
        "TESTS_CHANGED": "false",
        "PHYSICS_CHANGE": "false",
    }
    for key in drop_vars:
        env.pop(key, None)
    env.update(env_overrides)
    subprocess.run(
        ["bash", str(REPO_ROOT / ".github/scripts/determine-matrix.sh")],
        env=env,
        check=True,
        capture_output=True,
        text=True,
    )
    return _parse_github_output(gh_out)


# ---- classify_pyproject.py tests ----


@pytest.mark.parametrize(
    ("mutator", "expected"),
    [
        pytest.param(
            lambda d: d["build-system"]["requires"].append("ninja"),
            (True, False, False),
            id="build-system",
        ),
        pytest.param(
            lambda d: d.setdefault("tool", {}).update({
                "cibuildwheel": {"skip": "pp*"}
            }),
            (True, False, False),
            id="tool-cibuildwheel",
        ),
        pytest.param(
            lambda d: d["tool"]["meson-python"].update({"limited-api": False}),
            (True, False, False),
            id="tool-meson-python",
        ),
        pytest.param(
            lambda d: d["tool"]["meson-python"]["args"]["setup"].append(
                "-Dbuildtype=debug"
            ),
            (True, False, False),
            id="tool-meson-python-args",
        ),
        pytest.param(
            lambda d: d["project"]["dependencies"].append("tomli"),
            (False, True, False),
            id="project-dependencies",
        ),
        pytest.param(
            lambda d: d["project"].update({"requires-python": ">=3.13"}),
            (False, True, False),
            id="project-requires-python",
        ),
        pytest.param(
            lambda d: d["project"]["scripts"].update({
                "suews-new": "supy.cmd.suews_cli:main"
            }),
            (False, True, False),
            id="project-scripts",
        ),
        pytest.param(
            lambda d: d["project"]["optional-dependencies"]["dev"].append(
                "pytest-randomly"
            ),
            (False, True, False),
            id="project-optional-dependencies",
        ),
        pytest.param(
            lambda d: d["tool"]["pytest"]["ini_options"].update({"timeout": 900}),
            (False, False, True),
            id="tool-pytest-ini-options",
        ),
        pytest.param(
            lambda d: d["tool"]["ruff"].update({"line-length": 100}),
            (False, False, False),
            id="tool-ruff-non-build",
        ),
        pytest.param(
            lambda d: d["tool"]["coverage"]["report"].update({"precision": 2}),
            (False, False, False),
            id="tool-coverage-non-build",
        ),
        pytest.param(
            lambda d: d["project"].update({"description": "Updated description"}),
            (False, False, False),
            id="project-description-non-build",
        ),
    ],
)
def test_classify_pyproject_sections(
    mutator, expected: tuple[bool, bool, bool]
) -> None:
    """Every build, packaging, script, optional-dep, and pytest table is classified."""
    mod = _load_classify_pyproject_module()
    base = _base_pyproject()
    head = copy.deepcopy(base)
    mutator(head)

    result = mod.classify_pyproject(base, head)

    assert (
        result.build_relevant,
        result.package_relevant,
        result.test_relevant,
    ) == expected


def test_classify_pyproject_cli_git_diff_and_fallbacks(tmp_path: Path) -> None:
    """CLI compares git BASE_SHA against HEAD and fails safe when base is absent."""
    script = REPO_ROOT / ".github/scripts/classify_pyproject.py"
    repo = tmp_path / "repo"
    repo.mkdir()

    def git(*args: str) -> str:
        res = subprocess.run(
            ["git", *args],
            cwd=repo,
            check=True,
            capture_output=True,
            text=True,
        )
        return res.stdout.strip()

    git("init")
    git("config", "user.name", "Test")
    git("config", "user.email", "test@example.com")

    base_toml = (
        "[build-system]\n"
        'requires = ["meson-python>=0.18"]\n'
        "\n"
        "[tool.meson-python]\n"
        "limited-api = true\n"
        "\n"
        "[project]\n"
        'name = "supy"\n'
        'requires-python = ">=3.12"\n'
        'dependencies = ["numpy>=1.22"]\n'
        "\n"
        "[project.optional-dependencies]\n"
        'dev = ["pytest"]\n'
        "\n"
        "[project.scripts]\n"
        'suews = "supy.cmd.suews_cli:main"\n'
        "\n"
        "[tool.pytest.ini_options]\n"
        "timeout = 600\n"
    )
    (repo / "pyproject.toml").write_text(base_toml, encoding="utf-8")
    git("add", "pyproject.toml")
    git("commit", "-m", "base")
    base_sha = git("rev-parse", "HEAD")

    # Mutate [tool.pytest.ini_options] only -> test_relevant=true
    (repo / "pyproject.toml").write_text(
        base_toml.replace("timeout = 600", "timeout = 300"),
        encoding="utf-8",
    )
    out_pytest = tmp_path / "out_pytest"
    out_pytest.write_text("", encoding="utf-8")
    subprocess.run(
        [sys.executable, str(script)],
        cwd=repo,
        env={**os.environ, "BASE_SHA": base_sha, "GITHUB_OUTPUT": str(out_pytest)},
        check=True,
        capture_output=True,
        text=True,
    )
    assert _parse_github_output(out_pytest) == {
        "build_relevant": "false",
        "package_relevant": "false",
        "test_relevant": "true",
    }

    # Missing BASE_SHA -> conservative fallback (all true)
    out_fallback = tmp_path / "out_fallback"
    out_fallback.write_text("", encoding="utf-8")
    env_no_base = {**os.environ, "GITHUB_OUTPUT": str(out_fallback)}
    env_no_base.pop("BASE_SHA", None)
    subprocess.run(
        [sys.executable, str(script)],
        cwd=repo,
        env=env_no_base,
        check=True,
        capture_output=True,
        text=True,
    )
    assert _parse_github_output(out_fallback) == {
        "build_relevant": "true",
        "package_relevant": "true",
        "test_relevant": "true",
    }


@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="detect-changes decide step runs under bash on Linux CI",
)
def test_detect_changes_decide_step_wires_pyproject_outputs(tmp_path: Path) -> None:
    """detect-changes maps classify_pyproject outputs into build/python/tests flags."""
    wf = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    decide = next(
        s for s in wf["jobs"]["detect-changes"]["steps"] if s.get("id") == "decide"
    )
    assert decide["env"]["TEST_RELEVANT"] == (
        "${{ steps.classify-pyproject.outputs.test_relevant || 'false' }}"
    )

    cases = [
        (
            {"BUILD_RELEVANT": "true"},
            {
                "build-changed": "true",
                "python-changed": "false",
                "tests-changed": "false",
                "needs-build": "true",
            },
        ),
        (
            {"PACKAGE_RELEVANT": "true"},
            {
                "build-changed": "false",
                "python-changed": "true",
                "tests-changed": "false",
                "needs-build": "true",
            },
        ),
        (
            {"TEST_RELEVANT": "true"},
            {
                "build-changed": "false",
                "python-changed": "false",
                "tests-changed": "true",
                "needs-build": "true",
            },
        ),
        (
            {},
            {
                "build-changed": "false",
                "python-changed": "false",
                "tests-changed": "false",
                "needs-build": "false",
            },
        ),
    ]

    for idx, (overrides, expected) in enumerate(cases):
        gh_out = tmp_path / f"decide_{idx}"
        gh_out.write_text("", encoding="utf-8")
        env = {
            **os.environ,
            "GITHUB_OUTPUT": str(gh_out),
            "FILTER_BUILD": "false",
            "BUILD_RELEVANT": "false",
            "FILTER_PYTHON": "false",
            "PACKAGE_RELEVANT": "false",
            "FILTER_FORTRAN": "false",
            "FILTER_RUST": "false",
            "FILTER_UTIL": "false",
            "FILTER_CI": "false",
            "FILTER_TESTS": "false",
            "TEST_RELEVANT": "false",
            **overrides,
        }
        subprocess.run(
            ["bash", "-c", decide["run"]],
            env=env,
            check=True,
            capture_output=True,
            text=True,
        )
        assert _parse_github_output(gh_out) == expected


# ---- path-filters.yml tests ----


def test_path_filters_positive_only_and_closes_classifier_gaps() -> None:
    """Path filters use positive patterns only and cover all hardened surfaces."""
    filters = yaml.safe_load(
        (REPO_ROOT / ".github/path-filters.yml").read_text(encoding="utf-8")
    )

    for category, patterns in filters.items():
        assert isinstance(patterns, list) and patterns, category
        for pat in patterns:
            assert not str(pat).startswith("!"), (
                f"negation forbidden: {category}: {pat}"
            )

    # W1-1c gaps from gh#2094
    assert _matches_category(filters, "rust", "src/suews_bridge/bridge-manifest.json")
    assert _matches_category(filters, "ci", "scripts/lint/check_schema_version_bump.py")
    assert _matches_category(filters, "ci", "scripts/security/audit_python_startup.py")

    # Consolidated reusable workflows live in `ci` so workflow-only PRs still validate
    for wf in (
        "rust-test.yml",
        "rust-clippy.yml",
        "schema-version-audit.yml",
        "data-interface-version-audit.yml",
        "encoding-audit.yml",
        "workflow-security.yml",
        "dependency-audit.yml",
    ):
        assert _matches_category(filters, "ci", f".github/workflows/{wf}"), wf

    # Dedicated path-filtered producer categories
    assert _matches_category(
        filters, "rust_test", "src/suews_bridge/tests/yaml_jacobian.rs"
    )
    assert _matches_category(
        filters, "rust_test", "src/suews_bridge/bridge-manifest.json"
    )
    assert _matches_category(filters, "rust_test", "test/fixtures/bridge/sample.yml")
    assert _matches_category(filters, "rust_test", "src/supy/data_model/output/vars.py")
    assert not _matches_category(filters, "rust_test", "docs/source/index.rst")

    assert _matches_category(filters, "rust_clippy", "src/suews_bridge/src/lib.rs")
    assert _matches_category(
        filters, "rust_clippy", "src/suews/src/suews_ctrl_const.f95"
    )
    assert not _matches_category(filters, "rust_clippy", "docs/source/index.rst")

    assert _matches_category(filters, "encoding_audit", "get_ver_git.py")
    assert _matches_category(
        filters, "encoding_audit", "scripts/lint/check_test_markers.py"
    )
    assert _matches_category(
        filters, "encoding_audit", ".github/scripts/classify_pyproject.py"
    )
    assert not _matches_category(filters, "encoding_audit", "docs/source/index.rst")

    assert _matches_category(
        filters, "workflow_security", ".github/workflows/rust-test.yml"
    )
    assert _matches_category(
        filters, "workflow_security", ".github/scripts/check_action_pins.py"
    )
    assert not _matches_category(filters, "workflow_security", "src/supy/suews_sim.py")

    assert _matches_category(filters, "dependency_audit", "pyproject.toml")
    assert _matches_category(
        filters, "dependency_audit", "scripts/security/audit_python_startup.py"
    )
    assert not _matches_category(filters, "dependency_audit", "src/supy/suews_sim.py")


# ---- Ready tests-only PR tier selection & pytest collection parity ----


@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="determine-matrix.sh runs under bash on Linux CI only",
)
def test_ready_tests_only_pr_matches_merge_queue_standard_tier_and_collects_api_tests(
    tmp_path: Path,
) -> None:
    """Ready tests-only PRs select standard tier so non-smoke API tests execute on PR CI."""
    ready_tests = _run_determine_matrix(
        tmp_path,
        {"EVENT_NAME": "pull_request", "IS_DRAFT": "false", "TESTS_CHANGED": "true"},
    )
    ready_old_base = _run_determine_matrix(
        tmp_path,
        {"EVENT_NAME": "pull_request", "IS_DRAFT": "false"},
        drop_vars=("TESTS_CHANGED",),
    )
    ready_ci_only = _run_determine_matrix(
        tmp_path,
        {"EVENT_NAME": "pull_request", "IS_DRAFT": "false", "CI_CHANGED": "true"},
    )
    draft_tests = _run_determine_matrix(
        tmp_path,
        {"EVENT_NAME": "pull_request", "IS_DRAFT": "true", "TESTS_CHANGED": "true"},
    )
    mq_tests = _run_determine_matrix(
        tmp_path,
        {"EVENT_NAME": "merge_group", "TESTS_CHANGED": "true"},
    )

    # Ready tests-only PRs and merge_group agree on `standard` and minimal Linux platform.
    assert ready_tests["test_tier"] == "standard"
    assert ready_old_base["test_tier"] == "standard"
    assert ready_ci_only["test_tier"] == "standard"
    assert mq_tests["test_tier"] == "standard"
    assert json.loads(ready_tests["buildplat"]) == json.loads(mq_tests["buildplat"])
    assert len(json.loads(ready_tests["buildplat"])) == 1
    # Draft PRs retain fast smoke feedback.
    assert draft_tests["test_tier"] == "smoke"

    # Extract the real marker expressions from test-api-cross-python-reusable.yml.
    api_wf = (
        REPO_ROOT / ".github/workflows/test-api-cross-python-reusable.yml"
    ).read_text(encoding="utf-8")
    smoke_match = re.search(r"smoke\)\s+EXPR='([^']+)'", api_wf)
    standard_match = re.search(r"standard\)\s+EXPR='([^']+)'", api_wf)
    assert smoke_match is not None and standard_match is not None
    smoke_expr = smoke_match.group(1)
    standard_expr = standard_match.group(1)

    # Prove on a planted non-smoke API test module that smoke silently deselects it
    # while the newly selected standard tier executes and catches the failure.
    planted = tmp_path / "test_planted_non_smoke_api.py"
    planted.write_text(
        "import pytest\n"
        "pytestmark = pytest.mark.api\n"
        "def test_planted_failure():\n"
        "    assert False, 'planted defect'\n",
        encoding="utf-8",
    )

    smoke_collect = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--collect-only",
            str(planted),
            "-m",
            smoke_expr,
            "-q",
        ],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    assert smoke_collect.returncode == 5, smoke_collect.stdout + smoke_collect.stderr

    standard_collect = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--collect-only",
            str(planted),
            "-m",
            standard_expr,
            "-q",
        ],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    assert standard_collect.returncode == 0, (
        standard_collect.stdout + standard_collect.stderr
    )
    assert "test_planted_failure" in standard_collect.stdout

    standard_exec = subprocess.run(
        [sys.executable, "-m", "pytest", str(planted), "-m", standard_expr, "-q"],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    assert standard_exec.returncode == 1, standard_exec.stdout + standard_exec.stderr
    assert "planted defect" in standard_exec.stdout


# ---- Live API bypass-label lookup & labeled/unlabeled triggers ----


@pytest.mark.skipif(
    sys.platform == "win32"
    or shutil.which("bash") is None
    or shutil.which("jq") is None,
    reason="Workflow shell steps run under bash + jq on Linux CI",
)
@pytest.mark.parametrize(
    ("workflow_name", "job_id", "bypass_label"),
    [
        ("schema-version-audit.yml", "check-schema-bump", "0-ci:schema-audit-ok"),
        ("encoding-audit.yml", "check-encoding", "0-ci:encoding-audit-ok"),
        ("rust-clippy.yml", "clippy", "0-ci:clippy-ok"),
    ],
)
def test_bypass_label_workflows_trigger_on_label_and_use_live_api(
    tmp_path: Path,
    workflow_name: str,
    job_id: str,
    bypass_label: str,
) -> None:
    """Bypass-capable audits re-trigger on label edits and query live PR labels safely."""
    wf = yaml.safe_load(
        (REPO_ROOT / ".github/workflows" / workflow_name).read_text(encoding="utf-8")
    )
    triggers = wf[True] if True in wf else wf["on"]
    pr_types = set(triggers["pull_request"]["types"])
    assert {"opened", "synchronize", "reopened", "labeled", "unlabeled"} <= pr_types
    assert "workflow_call" in triggers

    step = next(s for s in wf["jobs"][job_id]["steps"] if s.get("id") == "bypass")
    assert step["env"]["BYPASS_LABEL"] == bypass_label

    bin_dir = tmp_path / "bin"
    bin_dir.mkdir(exist_ok=True)
    _write_gh_stub(bin_dir)

    cases = [
        ("pull_request", "42", "", "", [bypass_label], "true"),
        ("pull_request", "42", "", "", [["1-maintenance"], [bypass_label]], "true"),
        ("pull_request", "42", "", "", ["1-maintenance"], "false"),
        (
            "merge_group",
            "",
            "refs/heads/gh-readonly-queue/master/pr-42-ba5e000",
            "ba5e000",
            [bypass_label],
            "true",
        ),
        (
            "merge_group",
            "",
            "refs/heads/gh-readonly-queue/master/pr-42-a11ce41",
            "ba5e000",
            [bypass_label],
            "false",
        ),
        (
            "merge_group",
            "",
            "refs/heads/gh-readonly-queue/master/batch-deadbeef",
            "ba5e000",
            [bypass_label],
            "false",
        ),
    ]

    for idx, (event_name, pr_num, head_ref, base_sha, labels, expected) in enumerate(
        cases
    ):
        gh_out = tmp_path / f"out_{idx}"
        gh_out.write_text("", encoding="utf-8")
        env = {
            **os.environ,
            "PATH": f"{bin_dir}:{os.environ.get('PATH', '')}",
            "GITHUB_OUTPUT": str(gh_out),
            "GH_TOKEN": "dummy",
            "REPO": "UMEP-dev/SUEWS",
            "EVENT_NAME": event_name,
            "PR_NUMBER": pr_num,
            "MERGE_HEAD_REF": head_ref,
            "MERGE_BASE_SHA": base_sha,
            "BYPASS_LABEL": bypass_label,
            "FAKE_GH_LABELS_JSON": json.dumps(labels),
        }
        subprocess.run(
            ["bash", "-c", step["run"]],
            env=env,
            check=True,
            capture_output=True,
            text=True,
        )
        assert _parse_github_output(gh_out)["bypass"] == expected, (
            workflow_name,
            event_name,
            head_ref,
        )


# ---- Explicit base_sha resolution for pull_request and merge_group ----


@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="Workflow shell steps run under bash on Linux CI",
)
@pytest.mark.parametrize(
    ("workflow_name", "job_id"),
    [
        ("schema-version-audit.yml", "check-schema-bump"),
        ("data-interface-version-audit.yml", "check-interface-version-history"),
    ],
)
def test_audit_base_resolution_uses_explicit_base_sha(
    tmp_path: Path,
    workflow_name: str,
    job_id: str,
) -> None:
    """Explicit base_sha resolves cleanly without BASE_REF (merge_group compatibility)."""
    wf = yaml.safe_load(
        (REPO_ROOT / ".github/workflows" / workflow_name).read_text(encoding="utf-8")
    )
    step = next(s for s in wf["jobs"][job_id]["steps"] if s.get("id") == "base")

    repo = tmp_path / "repo"
    repo.mkdir()
    subprocess.run(["git", "init"], cwd=repo, check=True, capture_output=True)
    subprocess.run(["git", "config", "user.name", "Test"], cwd=repo, check=True)
    subprocess.run(
        ["git", "config", "user.email", "test@example.com"], cwd=repo, check=True
    )
    (repo / "README.md").write_text("base\n", encoding="utf-8")
    subprocess.run(["git", "add", "README.md"], cwd=repo, check=True)
    subprocess.run(
        ["git", "commit", "-m", "base"], cwd=repo, check=True, capture_output=True
    )
    base_sha = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], cwd=repo, text=True
    ).strip()

    gh_out = tmp_path / "base_out"
    gh_out.write_text("", encoding="utf-8")
    subprocess.run(
        ["bash", "-c", step["run"]],
        cwd=repo,
        env={
            **os.environ,
            "GITHUB_OUTPUT": str(gh_out),
            "BASE_SHA": base_sha,
            "BASE_REF": "",
        },
        check=True,
        capture_output=True,
        text=True,
    )
    assert _parse_github_output(gh_out)["ref"] == base_sha

    # Fails closed when neither BASE_SHA nor BASE_REF is available.
    fail_out = tmp_path / "fail_out"
    fail_out.write_text("", encoding="utf-8")
    res = subprocess.run(
        ["bash", "-c", step["run"]],
        cwd=repo,
        env={
            **os.environ,
            "GITHUB_OUTPUT": str(fail_out),
            "BASE_SHA": "",
            "BASE_REF": "",
        },
        check=False,
        capture_output=True,
        text=True,
    )
    assert res.returncode == 1


# ---- Consolidated workflow wiring & ci-metrics-needs.json parity ----


def _synth_job(name: str, created: int, started: int, completed: int) -> dict[str, Any]:
    def ts(offset: int) -> str:
        t = datetime(2026, 11, 20, tzinfo=UTC).timestamp() + offset
        return datetime.fromtimestamp(t, tz=UTC).isoformat().replace("+00:00", "Z")

    return {
        "name": name,
        "created_at": ts(created),
        "started_at": ts(started),
        "completed_at": ts(completed),
        "status": "completed",
        "conclusion": "success",
        "steps": [],
    }


def test_consolidated_producers_wired_into_build_workflow_and_metrics_graph() -> None:
    """All 7 consolidated producers use self-repository workflow_call and match metrics."""
    caller = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    declared_needs = json.loads(
        (REPO_ROOT / ".github/ci-metrics-needs.json").read_text(encoding="utf-8")
    )

    expected_producers = {
        "rust_test": (
            "Rust test",
            "$/.github/workflows/rust-test.yml",
            False,
            "RUST_TEST_RESULT",
        ),
        "rust_clippy": (
            "Rust clippy",
            "$/.github/workflows/rust-clippy.yml",
            True,
            "RUST_CLIPPY_RESULT",
        ),
        "schema_version_audit": (
            "Schema version audit",
            "$/.github/workflows/schema-version-audit.yml",
            True,
            "SCHEMA_AUDIT_RESULT",
        ),
        "data_interface_audit": (
            "Data-interface version audit",
            "$/.github/workflows/data-interface-version-audit.yml",
            False,
            "DATA_INTERFACE_AUDIT_RESULT",
        ),
        "encoding_audit": (
            "Encoding audit",
            "$/.github/workflows/encoding-audit.yml",
            True,
            "ENCODING_AUDIT_RESULT",
        ),
        "workflow_security": (
            "Workflow security",
            "$/.github/workflows/workflow-security.yml",
            False,
            "WORKFLOW_SECURITY_RESULT",
        ),
        "dependency_audit": (
            "Dependency and startup audit",
            "$/.github/workflows/dependency-audit.yml",
            False,
            "DEPENDENCY_AUDIT_RESULT",
        ),
    }

    pr_gate = caller["jobs"]["pr-gate"]
    pr_gate_step = next(s for s in pr_gate["steps"] if "run" in s)
    report_needs = set(caller["jobs"]["report_scheduled_run"]["needs"])

    for job_id, (
        display_name,
        uses_ref,
        needs_pr_read,
        result_env,
    ) in expected_producers.items():
        job = caller["jobs"][job_id]
        assert job["name"] == display_name
        assert job["uses"] == uses_ref
        assert "pull_request" in job["if"] and "merge_group" in job["if"]
        assert "head_sha" in job["with"]
        if needs_pr_read:
            assert job["permissions"]["pull-requests"] == "read"
            assert "base_sha" in job["with"]
            assert "pr_number" in job["with"]
            assert "merge_head_ref" in job["with"]
        assert job_id in pr_gate["needs"]
        assert pr_gate_step["env"][result_env] == f"${{{{ needs.{job_id}.result }}}}"
        assert job_id not in report_needs
        assert f"{display_name} / *" in declared_needs
        assert f"{display_name} / *" in declared_needs["PR build validation"]

    # benchmark-regression.yml stays deferred per gh#2094
    assert all(
        "benchmark-regression.yml" not in str(job.get("uses", ""))
        for job in caller["jobs"].values()
    )

    # Verify determine_matrix forwards TESTS_CHANGED to determine-matrix.sh
    set_matrix = next(
        s
        for s in caller["jobs"]["determine_matrix"]["steps"]
        if s.get("id") == "set-matrix"
    )
    assert set_matrix["env"]["TESTS_CHANGED"] == (
        "${{ needs.detect-changes.outputs.tests-changed || 'false' }}"
    )

    # Verify analyse_run resolves the full consolidated DAG without config drift
    run = {
        "id": 99,
        "created_at": "2026-11-20T00:00:00Z",
        "event": "merge_group",
        "head_sha": "abc",
    }
    full_jobs = {
        "jobs": [
            _synth_job("Detect code changes", 0, 1, 10),
            _synth_job("Determine build matrix", 10, 11, 15),
            _synth_job("Build MCP package", 10, 11, 20),
            _synth_job("Check pytest marker axis", 0, 1, 12),
            _synth_job("Physics-change label", 15, 16, 20),
            _synth_job("Schema version audit / Require schema version bump", 0, 2, 18),
            _synth_job(
                "Data-interface version audit / Validate data-interface version histories",
                0,
                2,
                25,
            ),
            _synth_job("Rust test / cargo test (suews_bridge)", 10, 12, 60),
            _synth_job("Rust clippy / cargo clippy (suews_bridge)", 10, 12, 35),
            _synth_job(
                "Encoding audit / Require explicit encoding on text I/O", 10, 12, 28
            ),
            _synth_job("Workflow security / Enforce SHA-pinned actions", 10, 12, 22),
            _synth_job(
                "Dependency and startup audit / Audit Python dependency intake (3.13)",
                10,
                12,
                45,
            ),
            _synth_job(
                "Build and test (manylinux x86_64) / cp312-manylinux x86_64",
                20,
                22,
                100,
            ),
            _synth_job(
                "Build and test (manylinux x86_64) / API cross-CPython tests / cp312-manylinux x86_64",
                100,
                102,
                140,
            ),
            _synth_job("PR build validation", 141, 142, 146),
        ]
    }
    metrics = analyse_run(run, full_jobs, declared_needs=declared_needs)
    assert metrics["observed_critical_path"]["target_job"] == "PR build validation"
    by_name = {j["name"]: j for j in metrics["jobs"]}
    assert (
        "Rust test / cargo test (suews_bridge)"
        in by_name["PR build validation"]["declared_dependencies"]
    )
    assert (
        "Schema version audit / Require schema version bump"
        in by_name["PR build validation"]["declared_dependencies"]
    )
    assert by_name["Rust test / cargo test (suews_bridge)"][
        "declared_dependencies"
    ] == ["Detect code changes"]

    # Docs-only run (only unconditional audits execute; path-filtered jobs skipped)
    docs_jobs = {
        "jobs": [
            _synth_job("Detect code changes", 0, 1, 8),
            _synth_job("Determine build matrix", 8, 9, 12),
            _synth_job("Check pytest marker axis", 0, 1, 10),
            _synth_job("Physics-change label", 12, 13, 16),
            _synth_job("Schema version audit / Require schema version bump", 0, 2, 15),
            _synth_job(
                "Data-interface version audit / Validate data-interface version histories",
                0,
                2,
                22,
            ),
            _synth_job("PR build validation", 23, 24, 27),
        ]
    }
    docs_metrics = analyse_run(run, docs_jobs, declared_needs=declared_needs)
    assert docs_metrics["observed_critical_path"]["job_names"] == [
        "Data-interface version audit / Validate data-interface version histories",
        "PR build validation",
    ]


def test_reusable_workflows_with_concurrency_isolate_group_by_workflow() -> None:
    """Dual-trigger workflow_call files must isolate concurrency by github.workflow.

    Without github.workflow in concurrency.group, a folded workflow_call run
    inside build-publish_to_pypi.yml shares a group with its standalone
    pull_request run and cancels it (or gets cancelled by a label event).
    """
    workflows_dir = REPO_ROOT / ".github/workflows"
    checked: list[str] = []
    for path in sorted(workflows_dir.glob("*.yml")):
        wf = yaml.safe_load(path.read_text(encoding="utf-8"))
        triggers = wf[True] if True in wf else wf.get("on", {})
        if not isinstance(triggers, dict) or "workflow_call" not in triggers:
            continue
        concurrency = wf.get("concurrency")
        if concurrency is None:
            continue
        group = (
            concurrency["group"] if isinstance(concurrency, dict) else str(concurrency)
        )
        assert "github.workflow" in str(group), (
            f"{path.name}: concurrency.group {group!r} must include "
            "github.workflow to prevent standalone vs workflow_call self-cancellation"
        )
        checked.append(path.name)

    assert {
        "encoding-audit.yml",
        "rust-clippy.yml",
        "rust-test.yml",
        "schema-version-audit.yml",
    } <= set(checked)
