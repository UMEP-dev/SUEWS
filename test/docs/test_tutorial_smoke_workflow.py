"""Contract tests for the unprivileged PR tutorial smoke check and #1431 boundary."""

from __future__ import annotations

import ast
import importlib.util
import os
from pathlib import Path
import re
import subprocess
import sys
import venv

import pytest
import yaml

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]

REPO_ROOT = Path(__file__).resolve().parents[2]
PAGES_WORKFLOW = REPO_ROOT / ".github/workflows/pages-deploy.yml"
TUTORIAL_WORKFLOW = REPO_ROOT / ".github/workflows/tutorial-smoke.yml"
RUNNER_SCRIPT = REPO_ROOT / "scripts/suews/run_tutorial_smoke.py"
TUTORIALS_DIR = REPO_ROOT / "docs/source/tutorials"

_SHA_REF_RE = re.compile(r"^[^@\s]+@[0-9a-fA-F]{40}$")


def _load_runner_module():
    """Load scripts/suews/run_tutorial_smoke.py as a Python module."""
    spec = importlib.util.spec_from_file_location("run_tutorial_smoke", RUNNER_SCRIPT)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _load_workflow(path: Path) -> dict:
    """Parse a GitHub Actions YAML workflow file."""
    return yaml.safe_load(path.read_text(encoding="utf-8"))


@pytest.mark.smoke
@pytest.mark.core
def test_pages_deploy_preserves_gh1431_privilege_boundary() -> None:
    """Privileged pages-deploy.yml must never build docs or deploy on pull_request."""
    wf = _load_workflow(PAGES_WORKFLOW)
    triggers = wf[True] if True in wf else wf["on"]
    pr_paths = triggers["pull_request"]["paths"]

    # gh#1431 removed docs/** and src/supy/data_model/** from pull_request triggers.
    assert "docs/**" not in pr_paths
    assert "src/supy/data_model/**" not in pr_paths

    deploy_steps = wf["jobs"]["deploy"]["steps"]
    build_docs = next(
        step for step in deploy_steps if step.get("name") == "Build documentation"
    )
    assert build_docs.get("if") == "github.event_name != 'pull_request'"

    # gh#1432: Pages deployment steps must stay guarded against pull_request.
    for action_prefix in (
        "actions/configure-pages@",
        "actions/upload-pages-artifact@",
        "actions/deploy-pages@",
    ):
        step = next(
            s for s in deploy_steps if str(s.get("uses", "")).startswith(action_prefix)
        )
        assert "github.event_name != 'pull_request'" in str(step.get("if", ""))


@pytest.mark.smoke
@pytest.mark.core
def test_tutorial_smoke_workflow_is_unprivileged_and_sha_pinned() -> None:
    """tutorial-smoke.yml stays read-only, SHA-pinned, and unions changed tutorials."""
    wf = _load_workflow(TUTORIAL_WORKFLOW)
    assert wf["permissions"] == {"contents": "read"}

    triggers = wf[True] if True in wf else wf["on"]
    assert "pull_request" in triggers
    assert "workflow_dispatch" in triggers
    pr_paths = triggers["pull_request"]["paths"]
    assert "docs/source/tutorials/**" in pr_paths
    assert "src/supy/**" in pr_paths
    assert "src/suews/src/**" in pr_paths
    assert "src/suews/ext_lib/**" in pr_paths
    assert "src/suews_bridge/**" in pr_paths
    assert "scripts/suews/run_tutorial_smoke.py" in pr_paths
    assert ".github/workflows/tutorial-smoke.yml" in pr_paths

    for job_id, job in wf["jobs"].items():
        # No job-level privilege escalation beyond top-level contents: read.
        assert "permissions" not in job or job["permissions"] == {"contents": "read"}, (
            job_id
        )
        for step in job["steps"]:
            uses = step.get("uses")
            if uses:
                assert _SHA_REF_RE.match(uses), f"Unpinned action in {job_id}: {uses}"
            if uses and uses.startswith("actions/checkout@"):
                assert step.get("with", {}).get("persist-credentials") is False
                assert step.get("with", {}).get("fetch-depth") == 0
                assert step.get("with", {}).get("fetch-tags") is True
            if uses and uses.startswith("astral-sh/setup-uv@"):
                assert step.get("with", {}).get("enable-cache") is False

    exec_step = next(
        step
        for step in wf["jobs"]["tutorial_smoke"]["steps"]
        if "scripts/suews/run_tutorial_smoke.py" in str(step.get("run", ""))
    )
    assert (
        exec_step.get("env", {}).get("PR_BASE_SHA")
        == "${{ github.event.pull_request.base.sha }}"
    )
    run_script = str(exec_step["run"])
    # Never interpolate github.event expressions directly into shell code.
    assert "${{" not in run_script
    assert "--changed-from-git-base" in run_script
    assert "--require-prefix" in run_script
    assert "--skip-provenance" not in run_script


@pytest.mark.smoke
@pytest.mark.core
def test_curated_smoke_tutorials_exist_and_are_valid_ascii_python() -> None:
    """Every curated smoke tutorial exists, parses cleanly, and stays ASCII-only."""
    runner = _load_runner_module()
    assert runner.SMOKE_TUTORIALS, "SMOKE_TUTORIALS must not be empty"

    paths = runner.resolve_tutorial_paths(TUTORIALS_DIR)
    assert len(paths) == len(runner.SMOKE_TUTORIALS)
    for path in paths:
        assert path.name.startswith("tutorial_") and path.suffix == ".py"
        raw = path.read_bytes()
        assert all(b <= 0x7F for b in raw), f"Non-ASCII byte found in {path.name}"
        ast.parse(raw.decode("ascii"), filename=str(path))


@pytest.mark.core
def test_run_tutorial_smoke_runner_reports_pass_and_failure(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Runner executes scripts in isolated temp dirs and propagates exits/timeouts."""
    runner = _load_runner_module()
    ok_script = tmp_path / "tutorial_ok.py"
    ok_script.write_text(
        "import os\n"
        "from pathlib import Path\n"
        "assert os.environ.get('MPLBACKEND') == 'Agg'\n"
        "assert os.environ.get('CI') == 'true'\n"
        "assert Path.cwd().resolve() != Path(__file__).resolve().parent\n"
        "print('tutorial ok')\n",
        encoding="utf-8",
    )
    fail_script = tmp_path / "tutorial_fail.py"
    fail_script.write_text(
        "import sys\n"
        "print('synthetic failure', file=sys.stderr)\n"
        "raise SystemExit(2)\n",
        encoding="utf-8",
    )
    slow_script = tmp_path / "tutorial_slow.py"
    slow_script.write_text(
        "import time\ntime.sleep(5)\n",
        encoding="utf-8",
    )

    assert (
        runner.main([
            "--tutorials-dir",
            str(tmp_path),
            "--tutorial",
            "tutorial_ok.py",
            "--workers",
            "1",
            "--skip-provenance",
        ])
        == 0
    )
    assert (
        runner.main([
            "--tutorials-dir",
            str(tmp_path),
            "--tutorial",
            "tutorial_ok.py",
            "--tutorial",
            "tutorial_fail.py",
            "--workers",
            "2",
            "--skip-provenance",
        ])
        == 1
    )
    assert (
        runner.main([
            "--tutorials-dir",
            str(tmp_path),
            "--tutorial",
            "tutorial_slow.py",
            "--workers",
            "1",
            "--timeout",
            "0.2",
            "--skip-provenance",
        ])
        == 1
    )
    captured = capsys.readouterr()
    assert "TIMEOUT" in captured.err
    assert (
        runner.main([
            "--tutorials-dir",
            str(tmp_path),
            "--tutorial",
            "tutorial_missing.py",
            "--skip-provenance",
        ])
        == 1
    )


@pytest.mark.core
def test_changed_tutorial_union_from_git_and_flags(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Changed runnable tutorials union onto the smoke subset and fail on error."""
    runner = _load_runner_module()
    repo = tmp_path / "repo"
    tut_dir = repo / "docs" / "source" / "tutorials"
    data_dir = tut_dir / "data"
    outside_dir = repo / "outside"
    data_dir.mkdir(parents=True)
    outside_dir.mkdir(parents=True)

    (tut_dir / "tutorial_01_base.py").write_text("print('base ok')\n", encoding="utf-8")
    (tut_dir / "tutorial_08_extra.py").write_text(
        "print('extra v1')\n", encoding="utf-8"
    )
    (tut_dir / "tutorial_09_deleted.py").write_text(
        "print('deleted')\n", encoding="utf-8"
    )
    (tut_dir / "helper_module.py").write_text("raise SystemExit(9)\n", encoding="utf-8")
    (tut_dir / "__init__.py").write_text("raise SystemExit(9)\n", encoding="utf-8")
    (data_dir / "sample.txt").write_text("1 2 3\n", encoding="utf-8")
    (outside_dir / "tutorial_outside.py").write_text(
        "raise SystemExit(9)\n", encoding="utf-8"
    )

    def _git(*args: str) -> str:
        proc = subprocess.run(
            ["git", "-C", str(repo), *args],
            check=True,
            capture_output=True,
            text=True,
        )
        return proc.stdout.strip()

    _git("init", "-q")
    _git("config", "user.name", "Test")
    _git("config", "user.email", "test@example.com")
    _git("add", "-A")
    _git("commit", "-q", "-m", "base")
    base_sha = _git("rev-parse", "HEAD")

    # PR modifies tutorial_08_extra.py, non-tutorial files, and deletes tutorial_09_deleted.py.
    (tut_dir / "tutorial_08_extra.py").write_text(
        "print('extra v2 ok')\n", encoding="utf-8"
    )
    (tut_dir / "tutorial_09_deleted.py").unlink()
    (tut_dir / "helper_module.py").write_text(
        "raise SystemExit(9)  # touched\n", encoding="utf-8"
    )
    (data_dir / "sample.txt").write_text("4 5 6\n", encoding="utf-8")
    _git("add", "-A")
    _git("commit", "-q", "-m", "pr edit")

    # --list should report base UNION tutorial_08_extra.py only (ignoring helper, data, deleted).
    rc = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--repo-root",
        str(repo),
        "--tutorial",
        "tutorial_01_base.py",
        "--changed-from-git-base",
        base_sha,
        "--changed-file",
        str(outside_dir / "tutorial_outside.py"),
        "--list",
    ])
    assert rc == 0
    listed = [Path(line).name for line in capsys.readouterr().out.splitlines() if line]
    assert listed == ["tutorial_01_base.py", "tutorial_08_extra.py"]

    # Network-only exclusion still filters matching filenames cleanly.
    filtered = runner.resolve_tutorial_paths(
        tut_dir,
        ["tutorial_01_base.py"],
        changed_paths=[tut_dir / "tutorial_08_extra.py"],
        repo_root=repo,
        network_only=frozenset({"tutorial_08_extra.py"}),
    )
    assert [p.name for p in filtered] == ["tutorial_01_base.py"]

    # Executing with a passing changed tutorial succeeds and runs both scripts.
    rc_ok = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--repo-root",
        str(repo),
        "--tutorial",
        "tutorial_01_base.py",
        "--changed-from-git-base",
        base_sha,
        "--workers",
        "2",
        "--skip-provenance",
    ])
    assert rc_ok == 0
    out_ok = capsys.readouterr().out
    assert "[OK] tutorial_01_base.py" in out_ok
    assert "[OK] tutorial_08_extra.py" in out_ok
    assert "2/2 passed" in out_ok

    # Breaking the changed non-smoke tutorial fails the run.
    (tut_dir / "tutorial_08_extra.py").write_text(
        "raise RuntimeError('broken changed tutorial')\n", encoding="utf-8"
    )
    _git("add", "-A")
    _git("commit", "-q", "-m", "break tutorial 08")
    rc_fail = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--repo-root",
        str(repo),
        "--tutorial",
        "tutorial_01_base.py",
        "--changed-from-git-base",
        base_sha,
        "--workers",
        "1",
        "--skip-provenance",
    ])
    assert rc_fail == 1
    captured_fail = capsys.readouterr()
    assert "[X] tutorial_08_extra.py" in captured_fail.err
    assert "broken changed tutorial" in captured_fail.err


@pytest.mark.core
def test_package_provenance_logs_imported_paths_and_enforces_prefix(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Runner logs supy and supy.suews_bridge provenance and guards --require-prefix."""
    runner = _load_runner_module()
    fake_venv = tmp_path / "fake-venv"

    class _CapturingEnvBuilder(venv.EnvBuilder):
        """Minimal venv builder that records the created interpreter path."""

        def __init__(self) -> None:
            super().__init__(with_pip=False, symlinks=(os.name != "nt"))
            self.env_exe = ""

        def post_setup(self, context) -> None:
            self.env_exe = context.env_exe

    builder = _CapturingEnvBuilder()
    builder.create(fake_venv)
    assert builder.env_exe and Path(builder.env_exe).is_file()

    stub_site = fake_venv / "stub-site"
    site_pkg = stub_site / "supy"
    site_pkg.mkdir(parents=True)
    (site_pkg / "__init__.py").write_text(
        "__version__ = '9999.0.0.dev0'\n", encoding="utf-8"
    )
    (site_pkg / "suews_bridge.py").write_text("SENTINEL = True\n", encoding="utf-8")

    tut_dir = tmp_path / "tutorials"
    tut_dir.mkdir()
    (tut_dir / "tutorial_01_demo.py").write_text("print('demo')\n", encoding="utf-8")

    # Use the clean fake_venv interpreter so host editable-install .pth finders
    # (e.g. _supy_editable_loader.pth) cannot shadow the stub package.
    monkeypatch.setattr(sys, "executable", builder.env_exe)
    monkeypatch.setenv("PYTHONPATH", str(stub_site))

    rc_ok = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--tutorial",
        "tutorial_01_demo.py",
        "--workers",
        "1",
        "--require-prefix",
        str(fake_venv),
    ])
    assert rc_ok == 0
    out = capsys.readouterr().out
    assert "Package provenance:" in out
    assert "supy.__version__:  9999.0.0.dev0" in out
    assert str((site_pkg / "__init__.py").resolve()) in out
    assert str((site_pkg / "suews_bridge.py").resolve()) in out

    other_venv = tmp_path / "other-venv"
    other_venv.mkdir()
    rc_mismatch = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--tutorial",
        "tutorial_01_demo.py",
        "--workers",
        "1",
        "--require-prefix",
        str(other_venv),
    ])
    assert rc_mismatch == 1
    assert "stale/external path" in capsys.readouterr().err


@pytest.mark.core
def test_changed_data_file_selects_consuming_tutorials_and_surfaces_failure(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Data-only PR edits under tutorials/data/ select and run consuming tutorials."""
    runner = _load_runner_module()

    # Real repository check: editing US-AR1_2010_data_60.txt selects tutorial_02.
    real_union = runner.resolve_tutorial_paths(
        TUTORIALS_DIR,
        changed_paths=[TUTORIALS_DIR / "data" / "US-AR1_2010_data_60.txt"],
    )
    assert [p.name for p in real_union] == [
        *runner.SMOKE_TUTORIALS,
        "tutorial_02_setup_own_site.py",
    ]

    # Synthetic git repo: a data-only commit selects only the consuming tutorial
    # and fails loudly when that tutorial errors.
    repo = tmp_path / "repo"
    tut_dir = repo / "docs" / "source" / "tutorials"
    data_dir = tut_dir / "data"
    data_dir.mkdir(parents=True)

    (tut_dir / "tutorial_01_base.py").write_text("print('base ok')\n", encoding="utf-8")
    (tut_dir / "tutorial_02_consumer.py").write_text(
        "DATA = 'US-AR1_2010_data_60.txt'\n"
        "raise ValueError('Legacy soil-moisture input requires explicit migration')\n",
        encoding="utf-8",
    )
    (tut_dir / "tutorial_08_unrelated.py").write_text(
        "print('unrelated')\n", encoding="utf-8"
    )
    data_file = data_dir / "US-AR1_2010_data_60.txt"
    data_file.write_text("iy id it imin xsmd\n", encoding="utf-8")

    def _git(*args: str) -> str:
        proc = subprocess.run(
            ["git", "-C", str(repo), *args],
            check=True,
            capture_output=True,
            text=True,
        )
        return proc.stdout.strip()

    _git("init", "-q")
    _git("config", "user.name", "Test")
    _git("config", "user.email", "test@example.com")
    _git("add", "-A")
    _git("commit", "-q", "-m", "base")
    base_sha = _git("rev-parse", "HEAD")

    # Data-only edit (no .py files changed).
    data_file.write_text("iy id it imin sm\n", encoding="utf-8")
    _git("add", str(data_file))
    _git("commit", "-q", "-m", "edit forcing data only")

    rc_list = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--repo-root",
        str(repo),
        "--tutorial",
        "tutorial_01_base.py",
        "--changed-from-git-base",
        base_sha,
        "--list",
    ])
    assert rc_list == 0
    listed = [Path(line).name for line in capsys.readouterr().out.splitlines() if line]
    assert listed == ["tutorial_01_base.py", "tutorial_02_consumer.py"]

    rc_fail = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--repo-root",
        str(repo),
        "--tutorial",
        "tutorial_01_base.py",
        "--changed-from-git-base",
        base_sha,
        "--workers",
        "1",
        "--skip-provenance",
    ])
    assert rc_fail == 1
    err = capsys.readouterr().err
    assert "[X] tutorial_02_consumer.py" in err
    assert "Legacy soil-moisture input" in err


def _init_data_lifecycle_repo(tmp_path: Path) -> tuple[Path, Path, Path, str]:
    """Create a synthetic git repo with tutorial scripts and data files."""
    repo = tmp_path / "repo"
    tut_dir = repo / "docs" / "source" / "tutorials"
    data_dir = tut_dir / "data"
    data_dir.mkdir(parents=True)

    (tut_dir / "tutorial_01_base.py").write_text("print('base ok')\n", encoding="utf-8")
    (tut_dir / "tutorial_02_del_consumer.py").write_text(
        "from pathlib import Path\n"
        "data = Path(__file__).resolve().parent / 'data' / 'site_del.txt'\n"
        "data.read_text(encoding='utf-8')\n",
        encoding="utf-8",
    )
    (tut_dir / "tutorial_03_ren_consumer.py").write_text(
        "from pathlib import Path\n"
        "data = Path(__file__).resolve().parent / 'data' / 'site_ren_old.txt'\n"
        "data.read_text(encoding='utf-8')\n",
        encoding="utf-8",
    )
    (tut_dir / "tutorial_04_new_consumer.py").write_text(
        "from pathlib import Path\n"
        "data = Path(__file__).resolve().parent / 'data' / 'site_ren_new.txt'\n"
        "assert data.is_file()\n",
        encoding="utf-8",
    )
    (tut_dir / "tutorial_09_removed.py").write_text(
        "raise SystemExit(9)\n", encoding="utf-8"
    )
    (data_dir / "site_del.txt").write_text("col1\n1\n", encoding="utf-8")
    (data_dir / "site_ren_old.txt").write_text("col1\n2\n", encoding="utf-8")

    for args in (
        ("init", "-q"),
        ("config", "user.name", "Test"),
        ("config", "user.email", "test@example.com"),
        ("add", "-A"),
        ("commit", "-q", "-m", "base"),
    ):
        subprocess.run(["git", "-C", str(repo), *args], check=True, capture_output=True)
    base_sha = subprocess.run(
        ["git", "-C", str(repo), "rev-parse", "HEAD"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout.strip()
    return repo, tut_dir, data_dir, base_sha


@pytest.mark.core
def test_deleted_data_file_selects_and_fails_consuming_tutorial(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Deleting a tutorials/data/* file runs its consumer and catches the missing path."""
    runner = _load_runner_module()
    repo, tut_dir, data_dir, base_sha = _init_data_lifecycle_repo(tmp_path)

    (data_dir / "site_del.txt").unlink()
    subprocess.run(["git", "-C", str(repo), "add", "-A"], check=True)
    subprocess.run(
        ["git", "-C", str(repo), "commit", "-q", "-m", "delete data file"], check=True
    )

    rc_list = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--repo-root",
        str(repo),
        "--tutorial",
        "tutorial_01_base.py",
        "--changed-from-git-base",
        base_sha,
        "--list",
    ])
    assert rc_list == 0
    listed = [Path(line).name for line in capsys.readouterr().out.splitlines() if line]
    assert listed == ["tutorial_01_base.py", "tutorial_02_del_consumer.py"]

    rc_run = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--repo-root",
        str(repo),
        "--tutorial",
        "tutorial_01_base.py",
        "--changed-from-git-base",
        base_sha,
        "--workers",
        "1",
        "--skip-provenance",
    ])
    assert rc_run == 1
    err = capsys.readouterr().err
    assert "[X] tutorial_02_del_consumer.py" in err
    assert "FileNotFoundError" in err


@pytest.mark.core
def test_renamed_data_file_resolves_both_sides_and_fails_broken_consumer(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Renaming a tutorials/data/* file selects consumers of both old and new paths."""
    runner = _load_runner_module()
    repo, tut_dir, _data_dir, base_sha = _init_data_lifecycle_repo(tmp_path)

    subprocess.run(
        [
            "git",
            "-C",
            str(repo),
            "mv",
            "docs/source/tutorials/data/site_ren_old.txt",
            "docs/source/tutorials/data/site_ren_new.txt",
        ],
        check=True,
    )
    subprocess.run(
        ["git", "-C", str(repo), "commit", "-q", "-m", "rename data file"], check=True
    )

    rc_list = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--repo-root",
        str(repo),
        "--tutorial",
        "tutorial_01_base.py",
        "--changed-from-git-base",
        base_sha,
        "--list",
    ])
    assert rc_list == 0
    listed = [Path(line).name for line in capsys.readouterr().out.splitlines() if line]
    assert listed == [
        "tutorial_01_base.py",
        "tutorial_03_ren_consumer.py",
        "tutorial_04_new_consumer.py",
    ]

    rc_run = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--repo-root",
        str(repo),
        "--tutorial",
        "tutorial_01_base.py",
        "--changed-from-git-base",
        base_sha,
        "--workers",
        "1",
        "--skip-provenance",
    ])
    assert rc_run == 1
    captured = capsys.readouterr()
    assert "[OK] tutorial_04_new_consumer.py" in captured.out
    assert "[X] tutorial_03_ren_consumer.py" in captured.err
    assert "FileNotFoundError" in captured.err


@pytest.mark.core
def test_deleted_tutorial_script_is_skipped_cleanly(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Deleting a tutorial_*.py script skips it without error."""
    runner = _load_runner_module()
    repo, tut_dir, _data_dir, base_sha = _init_data_lifecycle_repo(tmp_path)

    (tut_dir / "tutorial_09_removed.py").unlink()
    subprocess.run(["git", "-C", str(repo), "add", "-A"], check=True)
    subprocess.run(
        ["git", "-C", str(repo), "commit", "-q", "-m", "delete tutorial script"],
        check=True,
    )

    rc_list = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--repo-root",
        str(repo),
        "--tutorial",
        "tutorial_01_base.py",
        "--changed-from-git-base",
        base_sha,
        "--list",
    ])
    assert rc_list == 0
    listed = [Path(line).name for line in capsys.readouterr().out.splitlines() if line]
    assert listed == ["tutorial_01_base.py"]

    rc_run = runner.main([
        "--tutorials-dir",
        str(tut_dir),
        "--repo-root",
        str(repo),
        "--tutorial",
        "tutorial_01_base.py",
        "--changed-from-git-base",
        base_sha,
        "--workers",
        "1",
        "--skip-provenance",
    ])
    assert rc_run == 0
    assert "1/1 passed" in capsys.readouterr().out
