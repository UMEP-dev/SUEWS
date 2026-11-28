"""Contract tests for CI classifiers, path filters, ready test tiers, and folded audits."""

from __future__ import annotations

import copy
from datetime import UTC, datetime
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

import pytest
import yaml

from scripts.suews import ci_test_layout, ci_tiers
from scripts.suews.analyse_ci_run import analyse_run

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-TOOLING")]

REPO_ROOT = Path(__file__).resolve().parents[2]


@pytest.fixture(autouse=True)
def _isolate_axis_schema_env(monkeypatch: pytest.MonkeyPatch) -> None:
    """Keep tests isolated from an ambient GATE_AXIS_SCHEMA=2 CI environment."""
    monkeypatch.delenv("GATE_AXIS_SCHEMA", raising=False)


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


def _run_determine_matrix_raw(
    tmp_path: Path,
    env_overrides: dict[str, str],
    *,
    drop_vars: tuple[str, ...] = (),
) -> tuple[subprocess.CompletedProcess[str], dict[str, str]]:
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
    env.pop("GATE_AXIS_SCHEMA", None)
    for key in drop_vars:
        env.pop(key, None)
    env.update(env_overrides)
    proc = subprocess.run(
        ["bash", str(REPO_ROOT / ".github/scripts/determine-matrix.sh")],
        env=env,
        check=False,
        capture_output=True,
        text=True,
    )
    return proc, _parse_github_output(gh_out)


def _run_determine_matrix(
    tmp_path: Path,
    env_overrides: dict[str, str],
    *,
    drop_vars: tuple[str, ...] = (),
) -> dict[str, str]:
    proc, parsed = _run_determine_matrix_raw(
        tmp_path, env_overrides, drop_vars=drop_vars
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    return parsed


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
        "fortran-uninit.yml",
        "rust-capi-observer.yml",
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

    assert _matches_category(
        filters, "fortran_uninit", "src/suews/src/suews_phys_resist.f95"
    )
    assert _matches_category(
        filters, "fortran_uninit", "scripts/lint/fortran_uninit_baseline.json"
    )
    assert _matches_category(filters, "fortran_uninit", "src/suews_bridge/build.rs")
    assert not _matches_category(filters, "fortran_uninit", "docs/source/index.rst")

    assert _matches_category(
        filters, "rust_capi_observer", "scripts/suews/gen_capi_field_observer.py"
    )
    assert _matches_category(
        filters, "rust_capi_observer", "src/suews_bridge/tests/capi_field_observer.rs"
    )
    assert not _matches_category(filters, "rust_capi_observer", "docs/source/index.rst")


@pytest.mark.parametrize(
    "path",
    [
        "test/fixtures/bridge/sample_resolved.yml",
        "test/fixtures/bridge/stebbs_resolved.yml",
        "src/supy/data_model/output/version.py",
        "src/supy/data_model/output/artefacts/2.0.0/catalogue.json",
    ],
)
def test_rust_observer_runs_when_its_input_data_changes(path: str) -> None:
    """Both PR routes must run the witness when only its input changes."""
    workflow = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/rust-capi-observer.yml").read_text(
            encoding="utf-8"
        )
    )
    triggers = workflow[True] if True in workflow else workflow["on"]
    standalone = triggers["pull_request"]["paths"]
    assert any(_match_picomatch_pattern(path, pattern) for pattern in standalone)
    filters = yaml.safe_load(
        (REPO_ROOT / ".github/path-filters.yml").read_text(encoding="utf-8")
    )
    assert _matches_category(filters, "rust_capi_observer", path)


def _sample_paths_for_workflow_glob(pattern: str) -> list[str]:
    """Generate representative concrete repo-relative paths for a GitHub paths glob."""
    if pattern == "**.py":
        return ["top.py", "src/supy/mod.py", ".github/scripts/tool.py"]
    if pattern.endswith("/**"):
        prefix = pattern[:-3]
        return [f"{prefix}/probe.txt", f"{prefix}/nested/probe.txt"]
    if "*" in pattern:
        return [pattern.replace("*", "probe")]
    return [pattern]


def test_folded_path_filters_cover_every_workflow_trigger_path_and_wiring_files() -> (
    None
):
    """Every folded path-filter covers its workflow's pull_request.paths and wiring files."""
    filters = yaml.safe_load(
        (REPO_ROOT / ".github/path-filters.yml").read_text(encoding="utf-8")
    )
    folded_path_workflows = {
        "rust_test": "rust-test.yml",
        "rust_clippy": "rust-clippy.yml",
        "encoding_audit": "encoding-audit.yml",
        "workflow_security": "workflow-security.yml",
        "dependency_audit": "dependency-audit.yml",
        "fortran_uninit": "fortran-uninit.yml",
        "rust_capi_observer": "rust-capi-observer.yml",
    }

    for category, workflow_name in folded_path_workflows.items():
        wf = yaml.safe_load(
            (REPO_ROOT / ".github/workflows" / workflow_name).read_text(
                encoding="utf-8"
            )
        )
        triggers = wf[True] if True in wf else wf["on"]
        wf_paths = triggers["pull_request"]["paths"]
        assert wf_paths, workflow_name

        # Every path trigger declared on the standalone workflow must also match
        # the corresponding category in .github/path-filters.yml.
        for wf_pattern in wf_paths:
            for sample in _sample_paths_for_workflow_glob(str(wf_pattern)):
                assert _matches_category(filters, category, sample), (
                    f"{workflow_name} path {wf_pattern!r} (sample {sample!r}) "
                    f"is not covered by path-filters.yml category {category!r}"
                )

        # Changes to the caller workflow or path-filters itself must exercise
        # every path-filtered folded check instead of skipping it.
        for wiring_file in (
            ".github/workflows/build-publish_to_pypi.yml",
            ".github/path-filters.yml",
        ):
            assert _matches_category(filters, category, wiring_file), (
                f"{category!r} in .github/path-filters.yml must match {wiring_file!r}"
            )


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

    # Read the Schema 1 marker expressions from scripts/suews/ci_tiers.py
    # (test-api-cross-python-reusable.yml was retired in W1-7a).
    smoke_expr = ci_tiers.API_TIERS["smoke"]
    standard_expr = ci_tiers.API_TIERS["standard"]

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
            "EVENT_NAME": "merge_group",
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
            "EVENT_NAME": "merge_group",
            "BASE_SHA": "",
            "BASE_REF": "",
        },
        check=False,
        capture_output=True,
        text=True,
    )
    assert res.returncode == 1


def _build_moving_base_synthetic_pr_repo(
    tmp_path: Path,
) -> tuple[Path, str, str, str]:
    """Build a synthetic 2-parent PR merge commit (B2, pr_head) over stale base B1."""
    repo = tmp_path / "moving_base_repo"
    repo.mkdir()

    def git(*args: str) -> str:
        return subprocess.check_output(
            ["git", *args],
            cwd=repo,
            text=True,
            stderr=subprocess.STDOUT,
        ).strip()

    git("init", "-b", "master")
    git("config", "user.name", "Test")
    git("config", "user.email", "test@example.com")

    version_file = repo / "src/supy/data_model/configuration/version.py"
    core_file = repo / "src/supy/data_model/core/hydro.py"
    doc_file = repo / "docs/source/contributing/schema/schema_versioning.rst"
    for path in (version_file, core_file, doc_file):
        path.parent.mkdir(parents=True, exist_ok=True)

    # B1: initial base
    version_file.write_text('CURRENT_SCHEMA_VERSION = "2026.11.0"\n', encoding="utf-8")
    core_file.write_text("X = 1\n", encoding="utf-8")
    doc_file.write_text("Schema v2026.11.0\n", encoding="utf-8")
    git("add", ".")
    git("commit", "-m", "B1 base")
    b1_sha = git("rev-parse", "HEAD")

    # PR branch from B1: modifies watched core model WITHOUT bumping schema version
    git("checkout", "-b", "pr-branch")
    core_file.write_text("X = 2\n", encoding="utf-8")
    git("add", ".")
    git("commit", "-m", "PR unbumped data_model edit")
    pr_head_sha = git("rev-parse", "HEAD")

    # master advances to B2: unrelated PR bumps schema version + updates docs
    git("checkout", "master")
    version_file.write_text('CURRENT_SCHEMA_VERSION = "2026.11.1"\n', encoding="utf-8")
    doc_file.write_text("Schema v2026.11.1\n", encoding="utf-8")
    git("add", ".")
    git("commit", "-m", "B2 unrelated schema bump on master")
    b2_sha = git("rev-parse", "HEAD")

    # Synthetic GitHub PR merge commit M with parents (B2, pr_head_sha)
    git("merge", "--no-ff", "-m", "Synthetic PR merge", pr_head_sha)
    assert git("rev-parse", "HEAD^1") == b2_sha
    assert git("rev-parse", "HEAD^2") == pr_head_sha
    return repo, b1_sha, b2_sha, pr_head_sha


def _run_resolve_base_step(
    step_run: str,
    repo: Path,
    out_file: Path,
    env_extra: dict[str, str],
) -> str:
    """Execute a workflow's Resolve comparison base step and return its ref output."""
    out_file.write_text("", encoding="utf-8")
    subprocess.run(
        ["bash", "-c", step_run],
        cwd=repo,
        env={**os.environ, "GITHUB_OUTPUT": str(out_file), **env_extra},
        check=True,
        capture_output=True,
        text=True,
    )
    return _parse_github_output(out_file)["ref"]


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
def test_moving_base_synthetic_pr_merge_commit_uses_head_first_parent(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    workflow_name: str,
    job_id: str,
) -> None:
    """Synthetic 2-parent PR merge commits diff against HEAD^1, not stale pull_request.base.sha.

    When master advances from B1 to B2 (e.g. an unrelated PR bumping
    CURRENT_SCHEMA_VERSION + docs) after a PR branched from B1, GitHub's
    synthetic PR merge commit M has parents (B2, PR_head) while
    pull_request.base.sha can still report stale B1. Diffing B1...M attributes
    B2's bump to the PR and fails open; matching HEAD^2 == PR_HEAD_SHA and
    resolving HEAD^1 (= B2) keeps the audit strictly scoped to the PR without
    misclassifying a non-synthetic PR merge commit or a merge_group batch.
    """
    wf = yaml.safe_load(
        (REPO_ROOT / ".github/workflows" / workflow_name).read_text(encoding="utf-8")
    )
    step = next(s for s in wf["jobs"][job_id]["steps"] if s.get("id") == "base")
    assert step["env"]["EVENT_NAME"] == "${{ github.event_name }}"
    assert step["env"]["PR_HEAD_SHA"] == "${{ github.event.pull_request.head.sha }}"

    repo, b1_sha, b2_sha, pr_head_sha = _build_moving_base_synthetic_pr_repo(tmp_path)
    merge_head_sha = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], cwd=repo, text=True
    ).strip()

    # 1. Synthetic PR merge commit (HEAD^2 == PR_HEAD_SHA) with stale BASE_SHA=B1
    #    must resolve to HEAD^1 (= B2).
    resolved_pr_base = _run_resolve_base_step(
        step["run"],
        repo,
        tmp_path / "pr_base_out",
        {
            "EVENT_NAME": "pull_request",
            "PR_HEAD_SHA": pr_head_sha,
            "BASE_SHA": b1_sha,
            "BASE_REF": "master",
        },
    )
    assert resolved_pr_base == b2_sha

    # 2. Direct checkout of a merge commit whose HEAD^2 != PR_HEAD_SHA (e.g. a PR
    #    branch tip that merged master) must fall back to explicit BASE_SHA=B1.
    non_synthetic_base = _run_resolve_base_step(
        step["run"],
        repo,
        tmp_path / "non_synthetic_out",
        {
            "EVENT_NAME": "pull_request",
            "PR_HEAD_SHA": merge_head_sha,
            "BASE_SHA": b1_sha,
            "BASE_REF": "master",
        },
    )
    assert non_synthetic_base == b1_sha

    # 3. merge_group (even on a 2-parent merge commit) must keep explicit BASE_SHA=B1.
    mq_base = _run_resolve_base_step(
        step["run"],
        repo,
        tmp_path / "mq_base_out",
        {
            "EVENT_NAME": "merge_group",
            "PR_HEAD_SHA": "",
            "BASE_SHA": b1_sha,
            "BASE_REF": "",
        },
    )
    assert mq_base == b1_sha

    # 4. End-to-end proof with check_schema_version_bump.py on the moving-base merge commit:
    #    stale B1 fails open (exit 0), whereas resolved HEAD^1 (= B2) catches the unbumped edit (exit 1).
    if workflow_name == "schema-version-audit.yml":
        lint_root = REPO_ROOT / "scripts" / "lint"
        if str(lint_root) not in sys.path:
            sys.path.insert(0, str(lint_root))
        spec = importlib.util.spec_from_file_location(
            "check_schema_version_bump_moving_base",
            lint_root / "check_schema_version_bump.py",
        )
        assert spec is not None and spec.loader is not None
        schema_mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(schema_mod)

        monkeypatch.chdir(repo)
        assert schema_mod.main(["--base", b1_sha]) == 0
        assert schema_mod.main(["--base", resolved_pr_base]) == 1


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
    """All 9 consolidated producers use self-repository workflow_call and match metrics."""
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
            ("RUST_TEST_CHANGED", "rust-test-changed"),
        ),
        "rust_clippy": (
            "Rust clippy",
            "$/.github/workflows/rust-clippy.yml",
            True,
            "RUST_CLIPPY_RESULT",
            ("RUST_CLIPPY_CHANGED", "rust-clippy-changed"),
        ),
        "schema_version_audit": (
            "Schema version audit",
            "$/.github/workflows/schema-version-audit.yml",
            True,
            "SCHEMA_AUDIT_RESULT",
            None,
        ),
        "data_interface_audit": (
            "Data-interface version audit",
            "$/.github/workflows/data-interface-version-audit.yml",
            False,
            "DATA_INTERFACE_AUDIT_RESULT",
            None,
        ),
        "encoding_audit": (
            "Encoding audit",
            "$/.github/workflows/encoding-audit.yml",
            True,
            "ENCODING_AUDIT_RESULT",
            ("ENCODING_AUDIT_CHANGED", "encoding-audit-changed"),
        ),
        "workflow_security": (
            "Workflow security",
            "$/.github/workflows/workflow-security.yml",
            False,
            "WORKFLOW_SECURITY_RESULT",
            ("WORKFLOW_SECURITY_CHANGED", "workflow-security-changed"),
        ),
        "dependency_audit": (
            "Dependency and startup audit",
            "$/.github/workflows/dependency-audit.yml",
            False,
            "DEPENDENCY_AUDIT_RESULT",
            ("DEPENDENCY_AUDIT_CHANGED", "dependency-audit-changed"),
        ),
        "fortran_uninit": (
            "Fortran uninitialized lint",
            "$/.github/workflows/fortran-uninit.yml",
            False,
            "FORTRAN_UNINIT_RESULT",
            ("FORTRAN_UNINIT_CHANGED", "fortran-uninit-changed"),
        ),
        "rust_capi_observer": (
            "Rust C-API field observer",
            "$/.github/workflows/rust-capi-observer.yml",
            False,
            "RUST_CAPI_OBSERVER_RESULT",
            ("RUST_CAPI_OBSERVER_CHANGED", "rust-capi-observer-changed"),
        ),
    }

    pr_gate = caller["jobs"]["pr-gate"]
    pr_gate_step = next(
        s for s in pr_gate["steps"] if s.get("name") == "Check build results"
    )
    assert pr_gate_step["env"]["EVENT_NAME"] == "${{ github.event_name }}"
    report_needs = set(caller["jobs"]["report_scheduled_run"]["needs"])

    for job_id, (
        display_name,
        uses_ref,
        needs_pr_read,
        result_env,
        changed_spec,
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
        if changed_spec is not None:
            changed_env, output_key = changed_spec
            assert pr_gate_step["env"][changed_env] == (
                f"${{{{ needs.detect-changes.outputs.{output_key} }}}}"
            )
        assert job_id not in report_needs
        assert f"{display_name} / *" in declared_needs
        assert f"{display_name} / *" in declared_needs["PR build validation"]

        # None of the 9 folded workflows should also declare standalone merge_group,
        # otherwise merge_group double-executes them (and runs them on docs-only merges).
        wf_file = REPO_ROOT / uses_ref.removeprefix("$/")
        wf_data = yaml.safe_load(wf_file.read_text(encoding="utf-8"))
        wf_triggers = wf_data[True] if True in wf_data else wf_data["on"]
        assert "workflow_call" in wf_triggers, wf_file.name
        assert "merge_group" not in wf_triggers, wf_file.name

    # benchmark-regression.yml stays deferred per gh#2094
    assert all(
        "benchmark-regression.yml" not in str(job.get("uses", ""))
        for job in caller["jobs"].values()
    )

    # Verify determine_matrix forwards TESTS_CHANGED and fortran_uninit passes base_sha
    assert "base_sha" in caller["jobs"]["fortran_uninit"]["with"]
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
                "Fortran uninitialized lint / Fortran -Wuninitialized ratchet (GCC 14.2.0)",
                10,
                12,
                55,
            ),
            _synth_job(
                "Rust C-API field observer / C-API field-binding observer",
                10,
                12,
                65,
            ),
            _synth_job(
                "Build and test (manylinux x86_64) / test layout",
                20,
                21,
                25,
            ),
            _synth_job(
                "Build and test (manylinux x86_64) / wheel | cp312 | manylinux x86_64",
                20,
                22,
                100,
            ),
            _synth_job(
                "Build and test (manylinux x86_64) / Native tests / native | cp312 | manylinux x86_64",
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
    gate_deps = by_name["PR build validation"]["declared_dependencies"]
    assert "Rust test / cargo test (suews_bridge)" in gate_deps
    assert "Schema version audit / Require schema version bump" in gate_deps
    assert (
        "Fortran uninitialized lint / Fortran -Wuninitialized ratchet (GCC 14.2.0)"
        in gate_deps
    )
    assert "Rust C-API field observer / C-API field-binding observer" in gate_deps
    assert by_name[
        "Fortran uninitialized lint / Fortran -Wuninitialized ratchet (GCC 14.2.0)"
    ]["declared_dependencies"] == ["Detect code changes"]
    assert by_name["Rust C-API field observer / C-API field-binding observer"][
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
        "fortran-uninit.yml",
        "rust-capi-observer.yml",
        "rust-clippy.yml",
        "rust-test.yml",
        "schema-version-audit.yml",
    } <= set(checked)


# ---- Three-layer platform layout: Linux-first PRs and queue ----


@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="determine-matrix.sh runs under bash on Linux CI only",
)
def test_pull_requests_run_linux_only_unless_labelled_full_matrix(
    tmp_path: Path,
) -> None:
    """Every PR is Linux only; 0-ci:full-matrix opts it into the three PR platforms.

    The label widens the platforms whatever the PR changes (CI- or test-only
    PRs have failed only on Windows), and never changes the tier.
    """
    linux = [["ubuntu-latest", "manylinux", "x86_64"]]
    cases = {
        "draft-fortran": {"IS_DRAFT": "true", "FORTRAN_CHANGED": "true"},
        "draft-build": {"IS_DRAFT": "true", "BUILD_CHANGED": "true"},
        "draft-python": {"IS_DRAFT": "true", "PYTHON_CHANGED": "true"},
        "ready-fortran": {"FORTRAN_CHANGED": "true"},
        "ready-build": {"BUILD_CHANGED": "true"},
        "ready-ci": {"CI_CHANGED": "true"},
        "ready-tests": {"TESTS_CHANGED": "true"},
        "ready-physics": {"FORTRAN_CHANGED": "true", "PHYSICS_CHANGE": "true"},
    }
    for name, overrides in cases.items():
        plain = _run_determine_matrix(
            tmp_path, {"EVENT_NAME": "pull_request", **overrides}
        )
        labelled = _run_determine_matrix(
            tmp_path,
            {"EVENT_NAME": "pull_request", "FULL_MATRIX": "true", **overrides},
        )
        assert json.loads(plain["buildplat"]) == linux, name
        assert json.loads(plain["api_buildplat"]) == linux, name
        assert [p[1] for p in json.loads(labelled["buildplat"])] == [
            "manylinux",
            "macosx",
            "win",
        ], name
        assert labelled["api_buildplat"] == labelled["buildplat"], name
        assert labelled["test_tier"] == plain["test_tier"], name
        assert labelled["api_python"] == plain["api_python"], name


@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="determine-matrix.sh runs under bash on Linux CI only",
)
def test_merge_queue_runs_linux_full_and_windows_smoke_without_macos(
    tmp_path: Path,
) -> None:
    """The queue: Linux full tier plus api, Windows wheel at smoke, no macOS.

    The label is not read on merge_group, and a tests-only group stays Linux
    only. The nightly and tag releases keep the full matrix.
    """
    for overrides, tier in (
        ({"FORTRAN_CHANGED": "true"}, "standard"),
        ({"CI_CHANGED": "true"}, "standard"),
        ({"PYTHON_CHANGED": "true", "FULL_MATRIX": "true"}, "standard"),
        ({"FORTRAN_CHANGED": "true", "PHYSICS_CHANGE": "true"}, "physics-full"),
    ):
        mq = _run_determine_matrix(tmp_path, {"EVENT_NAME": "merge_group", **overrides})
        assert [p[1] for p in json.loads(mq["buildplat"])] == ["manylinux", "win"]
        assert [p[1] for p in json.loads(mq["api_buildplat"])] == ["manylinux"]
        assert mq["test_tier"] == tier
    tests_only = _run_determine_matrix(
        tmp_path, {"EVENT_NAME": "merge_group", "TESTS_CHANGED": "true"}
    )
    assert [p[1] for p in json.loads(tests_only["buildplat"])] == ["manylinux"]

    for event in ("schedule", "push"):
        full = _run_determine_matrix(
            tmp_path, {"EVENT_NAME": event, "GITHUB_REF": "refs/tags/v2026.11.22"}
        )
        assert len(json.loads(full["buildplat"])) == 4, event
        assert [p[1] for p in json.loads(full["api_buildplat"])] == [
            "manylinux",
            "macosx",
            "win",
        ], event
        assert full["test_tier"] == "all", event

    caller = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    jobs = caller["jobs"]
    # Windows runs the smoke tier in the queue; every other cell keeps the
    # tier determine_matrix chose.
    assert jobs["build_wheels"]["with"]["test_tier"] == (
        "${{ github.event_name == 'merge_group' && matrix.buildplat[1] == 'win' "
        "&& 'smoke' || needs.determine_matrix.outputs.test_tier }}"
    )
    # Schema 2 wheel lanes follow determine_matrix's per-lane buildplat outputs
    # (abi3_smoke is skipped on merge_group because abi3_smoke_python == '[]').
    assert "abi3_smoke_python != '[]'" in jobs["build_wheels"]["with"]["run_abi3_smoke"]
    assert "native_buildplat" in jobs["build_wheels"]["with"]["run_native_tests"]
    assert "os_compat_buildplat" in jobs["build_wheels"]["with"]["run_os_compat"]
    # The label is read live from the API on pull requests and fed to the script.
    steps = jobs["determine_matrix"]["steps"]
    label_step = next(s for s in steps if s.get("id") == "full-matrix-label")
    assert label_step["if"] == "github.event_name == 'pull_request'"
    assert "0-ci:full-matrix" in label_step["run"]
    set_matrix = next(s for s in steps if s.get("id") == "set-matrix")
    assert set_matrix["env"]["FULL_MATRIX"] == (
        "${{ steps.full-matrix-label.outputs.found || 'false' }}"
    )
    # PR build validation is still the one consolidated gate and its inputs
    # do not depend on the platform count.
    assert jobs["pr-gate"]["name"] == "PR build validation"


def test_nested_leaf_job_names_say_what_the_job_does() -> None:
    """The wheel build and wheel-backed lanes under one caller group have distinct leaves.

    The checks list shows "<caller group> / <leaf>", and both reusable jobs
    used to name their leaf "cp312-<platform> <arch>", so the wheel build and
    the test lane read the same. Render the wheel leaf and each Schema 2
    wheel-lane leaf for one platform and check that they differ, that each says
    what it does, and that the critical-path needs map tells them apart.
    """
    base_cell = {
        "${{ matrix.python }}": "cp312",
        "${{ matrix.cell.python_version }}": "cp312",
        "${{ matrix.buildplat[1] }}": "win",
        "${{ matrix.buildplat[2] }}": "AMD64",
        "${{ matrix.cell.shard && format(' shard {0}', matrix.cell.shard) || '' }}": "",
    }

    def render(workflow: str, job: str, extra: dict[str, str] | None = None) -> str:
        data = yaml.safe_load(
            (REPO_ROOT / ".github/workflows" / workflow).read_text(encoding="utf-8")
        )
        name = data["jobs"][job]["name"]
        for expr, value in {**base_cell, **(extra or {})}.items():
            name = name.replace(expr, value)
        assert "${{" not in name, name
        return name

    wheel_leaf = render("build-wheels-reusable.yml", "build")
    assert wheel_leaf.startswith("wheel")
    assert "cp312" in wheel_leaf and "win AMD64" in wheel_leaf
    assert wheel_leaf.isascii()

    needs = json.loads(
        (REPO_ROOT / ".github/ci-metrics-needs.json").read_text(encoding="utf-8")
    )
    group = "Build and test (win AMD64)"
    wheel_job = f"{group} / {wheel_leaf}"
    wheel_patterns = {d for deps in needs.values() for d in deps if "/ wheel" in d}
    assert wheel_patterns

    for lane_name, lane_group in (
        ("native", "Native tests"),
        ("os_compat", "OS compat tests"),
        ("abi3_smoke", "ABI3 smoke tests"),
    ):
        lane_leaf = render(
            "test-wheel-lane-reusable.yml",
            "wheel_lane",
            {"${{ matrix.cell.lane }}": lane_name},
        )
        assert wheel_leaf != lane_leaf
        assert lane_leaf.startswith(lane_name)
        assert "cp312" in lane_leaf and "win AMD64" in lane_leaf
        assert lane_leaf.isascii()

        lane_job = f"{group} / {lane_group} / {lane_leaf}"
        lane_deps = {
            d for k, deps in needs.items() if fnmatchcase(lane_job, k) for d in deps
        }
        # Each wheel lane waits on its own platform's wheel build, and no wheel
        # pattern mistakes a wheel-lane cell for a wheel build.
        assert any(fnmatchcase(wheel_job, d) for d in lane_deps), lane_job
        assert not any(fnmatchcase(lane_job, d) for d in wheel_patterns), lane_job


# ---- Dual-schema determine-matrix.sh contracts (#2349 W1-3b) ----


@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="determine-matrix.sh runs under bash on Linux CI only",
)
def test_determine_matrix_schema1_parity_and_invalid_schema_fail_closed(
    tmp_path: Path,
) -> None:
    """Unset, empty, and '1' match pinned Schema 1 raw output; invalid values fail closed."""
    linux = '[["ubuntu-latest", "manylinux", "x86_64"]]'
    mq_plat = (
        '[["ubuntu-latest", "manylinux", "x86_64"], ["windows-2025", "win", "AMD64"]]'
    )
    full_plat = (
        '[["ubuntu-latest", "manylinux", "x86_64"], '
        '["macos-15-intel", "macosx", "x86_64"], '
        '["macos-15", "macosx", "arm64"], '
        '["windows-2025", "win", "AMD64"]]'
    )
    nightly_api_plat = (
        '[["ubuntu-latest", "manylinux", "x86_64"], '
        '["macos-15", "macosx", "arm64"], '
        '["windows-2025", "win", "AMD64"]]'
    )
    pinned_scenarios = (
        (
            {
                "EVENT_NAME": "pull_request",
                "IS_DRAFT": "false",
                "PYTHON_CHANGED": "true",
            },
            [
                f"buildplat={linux}",
                f"api_buildplat={linux}",
                'python=["cp312"]',
                "test_tier=standard",
                'api_python=["cp312", "cp315"]',
                'bookend_python=["cp312", "cp315"]',
            ],
        ),
        (
            {
                "EVENT_NAME": "pull_request",
                "IS_DRAFT": "true",
                "FORTRAN_CHANGED": "true",
            },
            [
                "test_tier=core",
                f"buildplat={linux}",
                f"api_buildplat={linux}",
                'python=["cp312"]',
                'api_python=["cp312", "cp315"]',
                'bookend_python=["cp312", "cp315"]',
            ],
        ),
        (
            {"EVENT_NAME": "merge_group", "FORTRAN_CHANGED": "true"},
            [
                f"buildplat={mq_plat}",
                f"api_buildplat={linux}",
                'python=["cp312"]',
                "test_tier=standard",
                'api_python=["cp312"]',
                'bookend_python=["cp312", "cp315"]',
            ],
        ),
        (
            {"EVENT_NAME": "schedule"},
            [
                f"buildplat={full_plat}",
                f"api_buildplat={nightly_api_plat}",
                'python=["cp312"]',
                "test_tier=all",
                'api_python=["cp312", "cp313", "cp314", "cp315"]',
                'bookend_python=["cp312", "cp315"]',
            ],
        ),
        (
            {"EVENT_NAME": "push", "GITHUB_REF": "refs/tags/2026.11.26"},
            [
                f"buildplat={full_plat}",
                f"api_buildplat={nightly_api_plat}",
                'python=["cp312"]',
                "test_tier=all",
                'api_python=["cp312", "cp313", "cp314", "cp315"]',
                'bookend_python=["cp312", "cp315"]',
            ],
        ),
        (
            {
                "EVENT_NAME": "workflow_dispatch",
                "INPUT_MATRIX_CONFIG": "minimal",
                "INPUT_TEST_TIER": "standard",
            },
            [
                f"buildplat={linux}",
                f"api_buildplat={linux}",
                'python=["cp312"]',
                "test_tier=standard",
                'api_python=["cp312", "cp315"]',
                'bookend_python=["cp312", "cp315"]',
            ],
        ),
    )
    for scenario, expected_lines in pinned_scenarios:
        for schema_override in (
            {},
            {"GATE_AXIS_SCHEMA": ""},
            {"GATE_AXIS_SCHEMA": "1"},
        ):
            proc, parsed = _run_determine_matrix_raw(
                tmp_path, {**scenario, **schema_override}
            )
            assert proc.returncode == 0, proc.stdout + proc.stderr
            raw_lines = (
                (tmp_path / "github_output").read_text(encoding="utf-8").splitlines()
            )
            assert raw_lines == expected_lines, (scenario, schema_override)
            assert len(parsed) == 6

    for bad in ("0", "3", "v2", "true", " 2", "2 "):
        proc, parsed = _run_determine_matrix_raw(
            tmp_path, {"GATE_AXIS_SCHEMA": bad, "PYTHON_CHANGED": "true"}
        )
        assert proc.returncode == 1, (bad, proc.stdout, proc.stderr)
        assert "invalid GATE_AXIS_SCHEMA" in proc.stderr
        assert parsed == {}


@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="determine-matrix.sh runs under bash on Linux CI only",
)
def test_determine_matrix_schema2_four_full_physics_triggers_and_physics_core(
    tmp_path: Path,
) -> None:
    """Schema 2 selects full physics (incl. slow) on the exact 4 automatic triggers and physics-core otherwise."""
    slow_non_core_physics = ("physics", "slow")
    smoke_physics = ("physics", "smoke")

    # Trigger 1: PRs touching fortran, rust, or build -> physics-full (includes non-core slow physics).
    for flag in ("FORTRAN_CHANGED", "RUST_CHANGED", "BUILD_CHANGED"):
        out = _run_determine_matrix(
            tmp_path,
            {"GATE_AXIS_SCHEMA": "2", "EVENT_NAME": "pull_request", flag: "true"},
        )
        assert out["gate_axis_schema"] == "2"
        assert out["test_tier"] == "physics-full", flag
        assert out["full_physics"] == "true", flag
        assert out["physics_full_trigger"] == "compiled_or_build_change", flag
        expr = ci_tiers.tier_expression("physics", out["test_tier"], schema=2)
        assert ci_tiers.matches(expr, slow_non_core_physics), flag
        assert json.loads(out["source_lanes"]) == ["tooling", "unit"]
        assert json.loads(out["native_python"]) == ["cp312"]
        assert json.loads(out["abi3_smoke_python"]) == ["cp315"]
        assert json.loads(out["os_compat_buildplat"]) == json.loads(out["buildplat"])

    # Trigger 2: PRs carrying 0-physics:change or 0-numerics:change -> physics-full.
    for extra in ({}, {"FORTRAN_CHANGED": "true"}, {"PYTHON_CHANGED": "true"}):
        out = _run_determine_matrix(
            tmp_path,
            {
                "GATE_AXIS_SCHEMA": "2",
                "EVENT_NAME": "pull_request",
                "PHYSICS_CHANGE": "true",
                **extra,
            },
        )
        assert out["test_tier"] == "physics-full", extra
        assert out["full_physics"] == "true", extra
        assert out["physics_full_trigger"] == "classification_label", extra
        expr = ci_tiers.tier_expression("physics", out["test_tier"], schema=2)
        assert ci_tiers.matches(expr, slow_non_core_physics), extra

    # Other wheel-building Python PRs (python, util, tests, ci, other) -> physics-core.
    for extra in (
        {"PYTHON_CHANGED": "true"},
        {"UTIL_CHANGED": "true"},
        {"TESTS_CHANGED": "true"},
        {"CI_CHANGED": "true"},
        {},
    ):
        out = _run_determine_matrix(
            tmp_path,
            {"GATE_AXIS_SCHEMA": "2", "EVENT_NAME": "pull_request", **extra},
        )
        assert out["test_tier"] == "physics-core", extra
        assert out["full_physics"] == "false", extra
        assert out["physics_full_trigger"] == "none", extra
        expr = ci_tiers.tier_expression("physics", out["test_tier"], schema=2)
        assert ci_tiers.matches(expr, smoke_physics), extra
        assert not ci_tiers.matches(expr, slow_non_core_physics), extra
        assert json.loads(out["source_lanes"]) == ["tooling", "unit"]
        assert [p[1] for p in json.loads(out["source_buildplat"])] == ["manylinux"]
        assert json.loads(out["abi3_smoke_python"]) == ["cp315"]
        assert [p[1] for p in json.loads(out["abi3_smoke_buildplat"])] == ["manylinux"]

    # 0-ci:full-matrix widens native, abi3_smoke, and os_compat to Linux + macOS ARM + Windows,
    # while source lanes (tooling/unit) remain Linux-only.
    full_pr = _run_determine_matrix(
        tmp_path,
        {
            "GATE_AXIS_SCHEMA": "2",
            "EVENT_NAME": "pull_request",
            "PYTHON_CHANGED": "true",
            "FULL_MATRIX": "true",
        },
    )
    assert full_pr["test_tier"] == "physics-core"
    assert [p[1] for p in json.loads(full_pr["source_buildplat"])] == ["manylinux"]
    assert [p[1] for p in json.loads(full_pr["native_buildplat"])] == [
        "manylinux",
        "macosx",
        "win",
    ]
    assert [p[1] for p in json.loads(full_pr["os_compat_buildplat"])] == [
        "manylinux",
        "macosx",
        "win",
    ]
    assert [p[1] for p in json.loads(full_pr["abi3_smoke_buildplat"])] == [
        "manylinux",
        "macosx",
        "win",
    ]

    # Trigger 3: merge_group always runs full physics (physics-full, incl. slow), no abi3_smoke,
    # and native + os_compat on built OSes (Linux + Windows for code/CI, Linux for tests-only).
    mq_py = _run_determine_matrix(
        tmp_path,
        {
            "GATE_AXIS_SCHEMA": "2",
            "EVENT_NAME": "merge_group",
            "PYTHON_CHANGED": "true",
        },
    )
    assert mq_py["test_tier"] == "physics-full"
    assert mq_py["full_physics"] == "true"
    assert mq_py["physics_full_trigger"] == "merge_group"
    assert ci_tiers.matches(
        ci_tiers.tier_expression("physics", mq_py["test_tier"], schema=2),
        slow_non_core_physics,
    )
    assert json.loads(mq_py["abi3_smoke_python"]) == []
    assert json.loads(mq_py["abi3_smoke_buildplat"]) == []
    assert [p[1] for p in json.loads(mq_py["native_buildplat"])] == [
        "manylinux",
        "win",
    ]
    assert [p[1] for p in json.loads(mq_py["os_compat_buildplat"])] == [
        "manylinux",
        "win",
    ]

    mq_tests = _run_determine_matrix(
        tmp_path,
        {"GATE_AXIS_SCHEMA": "2", "EVENT_NAME": "merge_group", "TESTS_CHANGED": "true"},
    )
    assert mq_tests["test_tier"] == "physics-full"
    assert [p[1] for p in json.loads(mq_tests["native_buildplat"])] == ["manylinux"]
    assert [p[1] for p in json.loads(mq_tests["os_compat_buildplat"])] == ["manylinux"]

    mq_phys = _run_determine_matrix(
        tmp_path,
        {
            "GATE_AXIS_SCHEMA": "2",
            "EVENT_NAME": "merge_group",
            "PHYSICS_CHANGE": "true",
        },
    )
    assert mq_phys["test_tier"] == "physics-full"
    assert mq_phys["full_physics"] == "true"
    assert mq_phys["physics_full_trigger"] == "classification_label"

    # Trigger 4: nightly schedule -> all (includes non-core slow physics), 4 built OSes, abi3_smoke cp313..cp315.
    nightly = _run_determine_matrix(
        tmp_path, {"GATE_AXIS_SCHEMA": "2", "EVENT_NAME": "schedule"}
    )
    assert nightly["test_tier"] == "all"
    assert nightly["full_physics"] == "true"
    assert nightly["physics_full_trigger"] == "schedule"
    assert ci_tiers.matches(
        ci_tiers.tier_expression("physics", nightly["test_tier"], schema=2),
        slow_non_core_physics,
    )
    assert len(json.loads(nightly["buildplat"])) == 4
    assert len(json.loads(nightly["native_buildplat"])) == 4
    assert len(json.loads(nightly["os_compat_buildplat"])) == 4
    assert json.loads(nightly["abi3_smoke_python"]) == ["cp313", "cp314", "cp315"]
    assert [p[1] for p in json.loads(nightly["abi3_smoke_buildplat"])] == [
        "manylinux",
        "macosx",
        "win",
    ]


@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="determine-matrix.sh runs under bash on Linux CI only",
)
def test_determine_matrix_schema2_draft_tag_dispatch_and_ci_tiers_parity(
    tmp_path: Path,
) -> None:
    """Schema 2 applies PR triggers to drafts, keeps tags/dispatch, and aligns with ci_tiers."""
    # Under Schema 2, draft PRs follow the same four-trigger / physics-core rules as ready PRs
    # (no draft exemption in Multi-Axis CI Topology).
    for overrides, expected_tier, expected_full, expected_trigger in (
        (
            {"FORTRAN_CHANGED": "true"},
            "physics-full",
            "true",
            "compiled_or_build_change",
        ),
        (
            {"RUST_CHANGED": "true"},
            "physics-full",
            "true",
            "compiled_or_build_change",
        ),
        (
            {"BUILD_CHANGED": "true"},
            "physics-full",
            "true",
            "compiled_or_build_change",
        ),
        ({"PHYSICS_CHANGE": "true"}, "physics-full", "true", "classification_label"),
        ({"PYTHON_CHANGED": "true"}, "physics-core", "false", "none"),
    ):
        draft = _run_determine_matrix(
            tmp_path,
            {
                "GATE_AXIS_SCHEMA": "2",
                "EVENT_NAME": "pull_request",
                "IS_DRAFT": "true",
                **overrides,
            },
        )
        assert draft["test_tier"] == expected_tier, overrides
        assert draft["full_physics"] == expected_full, overrides
        assert draft["physics_full_trigger"] == expected_trigger, overrides
        assert ci_tiers.tier_expression("physics", expected_tier, schema=2)
        assert ci_test_layout.native_cells("cp312", expected_tier)

    # Tag release keeps full matrix at tier all and cp313..cp315 abi3_smoke.
    tag = _run_determine_matrix(
        tmp_path,
        {
            "GATE_AXIS_SCHEMA": "2",
            "EVENT_NAME": "push",
            "GITHUB_REF": "refs/tags/2026.11.26",
        },
    )
    assert tag["test_tier"] == "all"
    assert tag["full_physics"] == "true"
    assert tag["physics_full_trigger"] == "tag_release"
    assert len(json.loads(tag["buildplat"])) == 4
    assert json.loads(tag["abi3_smoke_python"]) == ["cp313", "cp314", "cp315"]

    # Manual workflow_dispatch keeps INPUT_TEST_TIER (including physics-core)
    # and drops abi3_smoke when custom selects only cp312.
    dispatch_core = _run_determine_matrix(
        tmp_path,
        {
            "GATE_AXIS_SCHEMA": "2",
            "EVENT_NAME": "workflow_dispatch",
            "INPUT_MATRIX_CONFIG": "custom",
            "INPUT_PLAT_LINUX": "true",
            "INPUT_PLAT_MACOS_INTEL": "false",
            "INPUT_PLAT_MACOS_ARM": "false",
            "INPUT_PLAT_WINDOWS": "true",
            "INPUT_PY312": "true",
            "INPUT_PY313": "false",
            "INPUT_PY314": "false",
            "INPUT_PY315": "false",
            "INPUT_TEST_TIER": "physics-core",
        },
    )
    assert dispatch_core["test_tier"] == "physics-core"
    assert dispatch_core["full_physics"] == "false"
    assert json.loads(dispatch_core["abi3_smoke_python"]) == []
    assert json.loads(dispatch_core["abi3_smoke_buildplat"]) == []
    assert [p[1] for p in json.loads(dispatch_core["os_compat_buildplat"])] == [
        "manylinux",
        "win",
    ]

    # Invalid dispatch tier or matrix config fails closed under Schema 2.
    bad_tier_proc, _ = _run_determine_matrix_raw(
        tmp_path,
        {
            "GATE_AXIS_SCHEMA": "2",
            "EVENT_NAME": "workflow_dispatch",
            "INPUT_MATRIX_CONFIG": "minimal",
            "INPUT_TEST_TIER": "physics_core",
        },
    )
    assert bad_tier_proc.returncode == 1
    assert "invalid INPUT_TEST_TIER" in bad_tier_proc.stderr

    bad_cfg_proc, _ = _run_determine_matrix_raw(
        tmp_path,
        {
            "GATE_AXIS_SCHEMA": "2",
            "EVENT_NAME": "workflow_dispatch",
            "INPUT_MATRIX_CONFIG": "unknown",
            "INPUT_TEST_TIER": "standard",
        },
    )
    assert bad_cfg_proc.returncode == 1
    assert "invalid INPUT_MATRIX_CONFIG" in bad_cfg_proc.stderr

    # Every tier emitted in Schema 2 is valid in scripts/suews/ci_tiers.py and ci_test_layout.py.
    for tier in (
        "smoke",
        "core",
        "cfg",
        "physics-core",
        "standard",
        "physics-full",
        "all",
    ):
        for lane in ("tooling", "unit", "native", "physics", "os_compat", "abi3_smoke"):
            assert ci_tiers.tier_expression(lane, tier, schema=2)
        assert ci_test_layout.native_cells("cp312", tier)

    # End-to-end matrix -> pr-gate regression: requires-python >=3.15 produces an empty
    # abi3_smoke matrix on a Schema 2 PR, which pr-gate.sh accepts when ABI3_SMOKE_PYTHON
    # is forwarded from determine-matrix.sh; >=3.12 produces ["cp315"] and still fails closed
    # if abi3_smoke is skipped.
    py315_pyproject = tmp_path / "pyproject_315.toml"
    py315_pyproject.write_text(
        '[project]\nname = "supy"\nrequires-python = ">=3.15"\n',
        encoding="utf-8",
    )
    py315_matrix = _run_determine_matrix(
        tmp_path,
        {
            "GATE_AXIS_SCHEMA": "2",
            "EVENT_NAME": "pull_request",
            "PYTHON_CHANGED": "true",
            "PR_PYPROJECT": str(py315_pyproject),
        },
    )
    assert json.loads(py315_matrix["python"]) == ["cp315"]
    assert json.loads(py315_matrix["abi3_smoke_python"]) == []
    assert json.loads(py315_matrix["abi3_smoke_buildplat"]) == []

    py312_matrix = _run_determine_matrix(
        tmp_path,
        {
            "GATE_AXIS_SCHEMA": "2",
            "EVENT_NAME": "pull_request",
            "PYTHON_CHANGED": "true",
        },
    )
    assert json.loads(py312_matrix["abi3_smoke_python"]) == ["cp315"]

    base_gate_env = {
        **os.environ,
        "GATE_AXIS_SCHEMA": "2",
        "EVENT_NAME": "pull_request",
        "DETECT_CHANGES_RESULT": "success",
        "NEEDS_BUILD": "true",
        "BUILD_WHEELS_RESULT": "success",
        "BUILD_MCP_RESULT": "success",
        "CHECK_MARKERS_RESULT": "success",
        "PHYSICS_LABEL_RESULT": "success",
        "PHYSICS_CHANGE": "false",
        "CORE_GUARD_RESULT": "success",
        "TOOLING_RESULT": "success",
        "UNIT_RESULT": "success",
        "NATIVE_RESULT": "success",
        "PHYSICS_RESULT": "success",
        "OS_COMPAT_RESULT": "success",
        "ABI3_SMOKE_RESULT": "skipped",
        "SCHEMA_AUDIT_RESULT": "success",
        "DATA_INTERFACE_AUDIT_RESULT": "success",
        "RUST_TEST_RESULT": "skipped",
        "RUST_TEST_CHANGED": "false",
        "RUST_CLIPPY_RESULT": "skipped",
        "RUST_CLIPPY_CHANGED": "false",
        "ENCODING_AUDIT_RESULT": "skipped",
        "ENCODING_AUDIT_CHANGED": "false",
        "WORKFLOW_SECURITY_RESULT": "skipped",
        "WORKFLOW_SECURITY_CHANGED": "false",
        "DEPENDENCY_AUDIT_RESULT": "skipped",
        "DEPENDENCY_AUDIT_CHANGED": "false",
        "FORTRAN_UNINIT_RESULT": "skipped",
        "FORTRAN_UNINIT_CHANGED": "false",
        "RUST_CAPI_OBSERVER_RESULT": "skipped",
        "RUST_CAPI_OBSERVER_CHANGED": "false",
    }
    gate_315 = subprocess.run(
        ["bash", str(REPO_ROOT / ".github/scripts/pr-gate.sh")],
        env={
            **base_gate_env,
            "TEST_TIER": py315_matrix["test_tier"],
            "ABI3_SMOKE_PYTHON": py315_matrix["abi3_smoke_python"],
        },
        capture_output=True,
        text=True,
        check=False,
    )
    assert gate_315.returncode == 0, gate_315.stdout + gate_315.stderr

    gate_312_skip = subprocess.run(
        ["bash", str(REPO_ROOT / ".github/scripts/pr-gate.sh")],
        env={
            **base_gate_env,
            "TEST_TIER": py312_matrix["test_tier"],
            "ABI3_SMOKE_PYTHON": py312_matrix["abi3_smoke_python"],
        },
        capture_output=True,
        text=True,
        check=False,
    )
    assert gate_312_skip.returncode == 1, gate_312_skip.stdout + gate_312_skip.stderr


def test_check_test_markers_wires_python_literals_ratchet() -> None:
    """check_test_markers runs check_python_literals.py as a PR/queue gate."""
    wf = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    jobs = wf["jobs"]
    lint_job = jobs["check_test_markers"]
    assert not lint_job.get("continue-on-error", False)
    assert "pull_request" in lint_job["if"]
    assert "merge_group" in lint_job["if"]
    assert "check_test_markers" in jobs["pr-gate"]["needs"]

    steps = lint_job["steps"]
    py_steps = [
        s
        for s in steps
        if str(s.get("run", "")).strip()
        == "python scripts/lint/check_python_literals.py"
    ]
    assert len(py_steps) == 1
    py_step = py_steps[0]
    assert py_step.get("if") == "always()"
    assert py_step.get("shell") == "bash"
    assert not py_step.get("continue-on-error", False)

    filters = yaml.safe_load(
        (REPO_ROOT / ".github/path-filters.yml").read_text(encoding="utf-8")
    )
    assert _matches_category(filters, "ci", "scripts/lint/check_python_literals.py")
    assert _matches_category(
        filters, "ci", "scripts/lint/python_literals_baseline.json"
    )


@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="workflow layout shell step runs under bash on Linux CI only",
)
def test_windows_merge_group_os_compat_keeps_non_smoke_witness(
    tmp_path: Path,
) -> None:
    """Windows merge_group narrows wheel physics to smoke while keeping ready-tier os_compat and native."""
    caller = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    wheels = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-wheels-reusable.yml").read_text(
            encoding="utf-8"
        )
    )
    bw_with = caller["jobs"]["build_wheels"]["with"]
    assert bw_with["test_tier"] == (
        "${{ github.event_name == 'merge_group' && matrix.buildplat[1] == 'win' "
        "&& 'smoke' || needs.determine_matrix.outputs.test_tier }}"
    )
    assert bw_with["lane_test_tier"] == "${{ needs.determine_matrix.outputs.test_tier }}"
    for lane_job_id in ("native_tests", "os_compat_tests", "abi3_smoke_tests"):
        assert (
            wheels["jobs"][lane_job_id]["with"]["test_tier"]
            == "${{ inputs.lane_test_tier || inputs.test_tier }}"
        )

    mq_out = _run_determine_matrix(
        tmp_path,
        {
            "GATE_AXIS_SCHEMA": "2",
            "EVENT_NAME": "merge_group",
            "PYTHON_CHANGED": "true",
        },
    )
    assert mq_out["test_tier"] == "physics-full"
    assert ["windows-2025", "win", "AMD64"] in json.loads(
        mq_out["os_compat_buildplat"]
    )

    # Execute the reusable workflow's real layout step for the Windows merge_group cell:
    # TIER='smoke' (wheel physics override) and LANE_TIER='physics-full'.
    layout_step = next(
        s
        for s in wheels["jobs"]["layout"]["steps"]
        if s.get("id") == "layout"
    )
    gh_out = tmp_path / "layout_out.txt"
    gh_out.write_text("", encoding="utf-8")
    proc = subprocess.run(
        ["bash", "-euo", "pipefail", "-c", layout_step["run"]],
        cwd=REPO_ROOT,
        env={
            **os.environ,
            "GITHUB_OUTPUT": str(gh_out),
            "TIER": "smoke",
            "LANE_TIER": mq_out["test_tier"],
            "FULL_PYTHON": "cp312",
            "SPLIT_TESTS": "false",
            "PHYSICS_SHARDS": "2",
            "NATIVE_SHARDS": "1",
            "ABI3_SMOKE_PYTHON": mq_out["abi3_smoke_python"],
            "RUN_OS_COMPAT": "true",
        },
        capture_output=True,
        text=True,
        check=False,
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    layout = dict(
        line.split("=", 1)
        for line in gh_out.read_text(encoding="utf-8").splitlines()
        if "=" in line
    )
    assert layout["effective_split_tests"] == "false"
    assert json.loads(layout["physics_shards"]) == [""]
    os_cells = json.loads(layout["os_compat_cells"])
    native_cells = json.loads(layout["native_cells"])
    assert len(os_cells) == 1 and os_cells[0]["tier"] == "physics-full"
    assert len(native_cells) == 1 and native_cells[0]["tier"] == "physics-full"

    # Confirm the resulting os_compat marker expression selects the non-smoke
    # Windows FileInputPath witness (while smoke would have dropped it).
    witness = (
        "test/data_model/test_convert_table_paths.py::"
        "test_convert_table_rewrites_windows_fileinputpath_on_flat_run_dir"
    )
    tier_expr = subprocess.run(
        [
            sys.executable,
            "scripts/suews/ci_tiers.py",
            "--schema",
            "2",
            "os_compat",
            os_cells[0]["tier"],
        ],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()
    smoke_expr = subprocess.run(
        [
            sys.executable,
            "scripts/suews/ci_tiers.py",
            "--schema",
            "2",
            "os_compat",
            "smoke",
        ],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()
    collect_ready = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--collect-only",
            "-q",
            "-m",
            tier_expr,
            "test/data_model/test_convert_table_paths.py",
        ],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    assert collect_ready.returncode == 0, collect_ready.stdout + collect_ready.stderr
    assert witness in collect_ready.stdout

    collect_smoke = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--collect-only",
            "-q",
            "-m",
            smoke_expr,
            "test/data_model/test_convert_table_paths.py",
        ],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    assert witness not in collect_smoke.stdout
