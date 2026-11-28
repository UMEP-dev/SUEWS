"""Unit and subprocess tests for wheel-free source-lane bootstrap (gh#2366 W1-4s)."""

from __future__ import annotations

import doctest
import importlib.util
import os
from pathlib import Path
import shutil
import subprocess
import sys

import pytest

REPO_ROOT = Path(__file__).resolve().parents[2]
TEST_DIR = REPO_ROOT / "test"
BOOTSTRAP_PATH = (
    REPO_ROOT / "scripts" / "suews" / "source_lane_bootstrap.py"
).resolve()

_EXISTING_SLB = sys.modules.get("_suews_source_lane_bootstrap")
_EXISTING_SLB_FILE = (
    getattr(_EXISTING_SLB, "__file__", None) if _EXISTING_SLB is not None else None
)
if _EXISTING_SLB_FILE and Path(_EXISTING_SLB_FILE).resolve() == BOOTSTRAP_PATH:
    slb = _EXISTING_SLB
else:
    _SPEC = importlib.util.spec_from_file_location(
        "_suews_source_lane_bootstrap", BOOTSTRAP_PATH
    )
    assert _SPEC is not None and _SPEC.loader is not None
    slb = importlib.util.module_from_spec(_SPEC)
    sys.modules[_SPEC.name] = slb
    _SPEC.loader.exec_module(slb)

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-TOOLING")]

_SYNTHETIC_PYPROJECT = """\
[tool.pytest.ini_options]
markers = [
    "tooling: tooling test",
    "unit: unit test",
    "native: native test",
    "physics: physics test",
    "os_compat: os compat slice",
    "e2e: subprocess e2e slice",
    "api: temporary Schema-1 compatibility shim",
    "qgis: UMEP/QGIS test",
    "contract(*ids): test-contract ids",
]
"""


def _setup_synthetic_source_repo(tmp_path: Path) -> Path:
    """Create a minimal synthetic repo carrying real conftest + source_lane_bootstrap."""
    test_root = tmp_path / "test"
    scripts_dir = tmp_path / "scripts" / "suews"
    test_root.mkdir(parents=True, exist_ok=True)
    scripts_dir.mkdir(parents=True, exist_ok=True)
    (tmp_path / "pyproject.toml").write_text(_SYNTHETIC_PYPROJECT, encoding="utf-8")
    shutil.copy2(TEST_DIR / "conftest.py", test_root / "conftest.py")
    shutil.copy2(BOOTSTRAP_PATH, scripts_dir / "source_lane_bootstrap.py")
    return test_root


def _run_pytest(
    cwd: Path,
    args: list[str],
    *,
    source_lane_value: str | None,
    extra_pythonpath: list[Path] | None = None,
) -> subprocess.CompletedProcess[str]:
    """Run pytest in a subprocess with explicit ``SUEWS_SOURCE_LANE_ONLY`` control."""
    env = dict(os.environ)
    if source_lane_value is None:
        env.pop(slb.SOURCE_LANE_ENV_VAR, None)
        env.pop(slb.SOURCE_LANE_ROOT_ENV_VAR, None)
    else:
        env[slb.SOURCE_LANE_ENV_VAR] = source_lane_value
    if extra_pythonpath is not None:
        env["PYTHONPATH"] = os.pathsep.join(str(path) for path in extra_pythonpath)
    return subprocess.run(
        [sys.executable, "-m", "pytest", *args],
        cwd=cwd,
        env=env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )


@pytest.mark.parametrize(
    ("raw", "expected"),
    [
        (None, False),
        ("", False),
        ("   ", False),
        ("0", False),
        ("false", False),
        ("False", False),
        ("no", False),
        ("off", False),
        ("1", True),
        ("true", True),
        ("TRUE", True),
        ("yes", True),
        ("on", True),
    ],
)
def test_is_source_lane_only_parses_boolean_tokens(
    raw: str | None, expected: bool
) -> None:
    """``is_source_lane_only`` accepts canonical truthy/falsy values."""
    env = {} if raw is None else {slb.SOURCE_LANE_ENV_VAR: raw}
    assert slb.is_source_lane_only(env) is expected


@pytest.mark.parametrize("raw", ["2", "maybe", "enabled", "source"])
def test_is_source_lane_only_fails_closed_on_invalid_value(raw: str) -> None:
    """Unrecognised values raise ``ValueError`` rather than silently disabling guards."""
    with pytest.raises(ValueError, match=slb.SOURCE_LANE_ENV_VAR):
        slb.is_source_lane_only({slb.SOURCE_LANE_ENV_VAR: raw})


@pytest.mark.parametrize(
    ("module_name", "expected"),
    [
        ("suews_bridge", True),
        ("supy.suews_bridge", True),
        ("supy.suews_bridge.extra", True),
        ("supy._suews_bridge", True),
        ("supy._supy_driver", True),
        ("_supy_driver", True),
        ("supy._suews_driver", True),
        ("supy._run_rust", False),
        ("supy.data_model", False),
        ("supy", False),
    ],
)
def test_is_blocked_compiled_module_targets_extensions_only(
    module_name: str, expected: bool
) -> None:
    """Compiled extension names are blocked while pure-Python ``supy._run_rust`` stays importable."""
    assert slb.is_blocked_compiled_module(module_name) is expected


def test_compiled_extension_blocker_blocks_and_uninstalls_cleanly() -> None:
    """``CompiledExtensionBlocker`` raises ``SourceLaneViolationError`` and cleans up cleanly."""
    blocker = slb.CompiledExtensionBlocker()
    with pytest.raises(slb.SourceLaneViolationError, match="suews_bridge"):
        blocker.find_spec("supy.suews_bridge", None)
    assert blocker.find_spec("supy._run_rust", None) is None
    assert not issubclass(slb.SourceLaneViolationError, ImportError)


def test_activate_if_enabled_is_noop_when_unset(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Normal wheel mode (env unset or ``0``) installs no source-lane meta-path finders."""
    meta_snapshot = list(sys.meta_path)
    monkeypatch.setattr(sys, "meta_path", list(meta_snapshot))
    monkeypatch.setitem(
        slb._STATE, "disabled_finders", list(slb._STATE["disabled_finders"])
    )
    slb.uninstall_guards()

    assert slb.activate_if_enabled(REPO_ROOT, env={}) is False
    assert (
        slb.activate_if_enabled(REPO_ROOT, env={slb.SOURCE_LANE_ENV_VAR: "0"}) is False
    )
    assert not any(
        isinstance(f, (slb.CompiledExtensionBlocker, slb.VersionScmFallbackFinder))
        for f in sys.meta_path
    )


@pytest.mark.e2e
@pytest.mark.os_compat
def test_source_lane_blocks_compiled_extension_even_behind_importorskip(
    tmp_path: Path,
) -> None:
    """``SUEWS_SOURCE_LANE_ONLY=1`` fails loudly on compiled imports even with ``importorskip``."""
    test_root = _setup_synthetic_source_repo(tmp_path)
    (test_root / "test_accidental_bridge.py").write_text(
        "import importlib\n"
        "import pytest\n\n"
        "pytestmark = pytest.mark.unit\n\n"
        "def test_direct_import():\n"
        "    importlib.import_module('supy.suews_bridge')\n\n"
        "def test_importorskip_cannot_swallow():\n"
        "    pytest.importorskip('supy.suews_bridge')\n\n"
        "def test_except_import_error_cannot_swallow():\n"
        "    try:\n"
        "        import suews_bridge  # noqa: F401\n"
        "    except ImportError:\n"
        "        pass\n",
        encoding="utf-8",
    )

    proc = _run_pytest(
        tmp_path,
        ["-q", "test/test_accidental_bridge.py"],
        source_lane_value="1",
        extra_pythonpath=[REPO_ROOT / "src", REPO_ROOT / "mcp" / "src"],
    )

    combined = proc.stdout + proc.stderr
    assert proc.returncode == pytest.ExitCode.TESTS_FAILED, combined
    assert "3 failed" in combined, combined
    assert "SourceLaneViolationError" in combined, combined
    assert "skipped" not in combined, combined


@pytest.mark.e2e
@pytest.mark.os_compat
def test_source_lane_blocks_compiled_only_conftest_fixtures(tmp_path: Path) -> None:
    """Compiled-only fixtures (e.g. ``suews_engine_bin``, ``completed_sample_sim``) fail loudly."""
    test_root = _setup_synthetic_source_repo(tmp_path)
    (test_root / "test_compiled_fixture.py").write_text(
        "import pytest\n\n"
        "pytestmark = pytest.mark.unit\n\n"
        "def test_uses_engine_bin(suews_engine_bin):\n"
        "    assert suews_engine_bin\n\n"
        "def test_uses_completed_sim(completed_sample_sim):\n"
        "    assert completed_sample_sim\n",
        encoding="utf-8",
    )

    proc = _run_pytest(
        tmp_path,
        ["-q", "test/test_compiled_fixture.py"],
        source_lane_value="1",
        extra_pythonpath=[REPO_ROOT / "src", REPO_ROOT / "mcp" / "src"],
    )

    combined = proc.stdout + proc.stderr
    assert proc.returncode == pytest.ExitCode.TESTS_FAILED, combined
    assert "2 errors" in combined, combined
    assert "SourceLaneViolationError" in combined, combined


@pytest.mark.e2e
@pytest.mark.os_compat
@pytest.mark.parametrize("axis", ["native", "physics"])
def test_source_lane_rejects_executing_native_and_physics_tests(
    tmp_path: Path, axis: str
) -> None:
    """Running ``native`` or ``physics`` tests under ``SUEWS_SOURCE_LANE_ONLY=1`` fails with UsageError."""
    test_root = _setup_synthetic_source_repo(tmp_path)
    (test_root / "test_tooling_ok.py").write_text(
        "import pytest\npytestmark = pytest.mark.tooling\ndef test_ok():\n    assert True\n",
        encoding="utf-8",
    )
    (test_root / f"test_{axis}_blocked.py").write_text(
        f"import pytest\npytestmark = pytest.mark.{axis}\ndef test_blocked():\n    assert True\n",
        encoding="utf-8",
    )

    proc = _run_pytest(
        tmp_path,
        ["-q", "-m", axis, "test/"],
        source_lane_value="1",
        extra_pythonpath=[REPO_ROOT / "src", REPO_ROOT / "mcp" / "src"],
    )

    combined = proc.stdout + proc.stderr
    assert proc.returncode == pytest.ExitCode.USAGE_ERROR, combined
    assert f"test_{axis}_blocked.py::test_blocked ({axis})" in combined, combined


@pytest.mark.e2e
@pytest.mark.os_compat
def test_source_lane_allows_collect_only_and_tooling_unit_execution(
    tmp_path: Path,
) -> None:
    """``--collect-only`` (inventory) and ``tooling``/``unit`` runs succeed under ``SUEWS_SOURCE_LANE_ONLY=1``."""
    test_root = _setup_synthetic_source_repo(tmp_path)
    (test_root / "test_tooling_ok.py").write_text(
        "import pytest\npytestmark = pytest.mark.tooling\ndef test_t():\n    assert True\n",
        encoding="utf-8",
    )
    (test_root / "test_unit_ok.py").write_text(
        "import pytest\npytestmark = pytest.mark.unit\ndef test_u():\n    assert True\n",
        encoding="utf-8",
    )
    (test_root / "test_native_skip.py").write_text(
        "import pytest\npytestmark = pytest.mark.native\ndef test_n():\n    assert True\n",
        encoding="utf-8",
    )
    (test_root / "test_physics_skip.py").write_text(
        "import pytest\npytestmark = pytest.mark.physics\ndef test_p():\n    assert True\n",
        encoding="utf-8",
    )

    collect_proc = _run_pytest(
        tmp_path,
        ["--collect-only", "-q", "test/"],
        source_lane_value="1",
        extra_pythonpath=[REPO_ROOT / "src", REPO_ROOT / "mcp" / "src"],
    )
    assert collect_proc.returncode == 0, collect_proc.stdout + collect_proc.stderr

    run_proc = _run_pytest(
        tmp_path,
        ["-q", "-m", "tooling or unit", "test/"],
        source_lane_value="1",
        extra_pythonpath=[REPO_ROOT / "src", REPO_ROOT / "mcp" / "src"],
    )
    combined = run_proc.stdout + run_proc.stderr
    assert run_proc.returncode == 0, combined
    assert "2 passed" in combined, combined


@pytest.mark.e2e
@pytest.mark.os_compat
def test_normal_mode_unchanged_when_env_unset(tmp_path: Path) -> None:
    """Unset ``SUEWS_SOURCE_LANE_ONLY`` leaves ``native``/``physics`` execution untouched."""
    test_root = _setup_synthetic_source_repo(tmp_path)
    (test_root / "test_native_allowed.py").write_text(
        "import sys\n"
        "import pytest\n\n"
        "pytestmark = pytest.mark.native\n\n"
        "def test_native_runs_without_blocker():\n"
        "    assert not any(type(f).__name__ == 'CompiledExtensionBlocker' for f in sys.meta_path)\n",
        encoding="utf-8",
    )

    proc = _run_pytest(
        tmp_path,
        ["-q", "test/test_native_allowed.py"],
        source_lane_value=None,
        extra_pythonpath=[REPO_ROOT / "src", REPO_ROOT / "mcp" / "src"],
    )
    combined = proc.stdout + proc.stderr
    assert proc.returncode == 0, combined
    assert "1 passed" in combined, combined


@pytest.mark.e2e
@pytest.mark.os_compat
def test_source_lane_invalid_env_value_fails_closed_in_pytest(tmp_path: Path) -> None:
    """Invalid ``SUEWS_SOURCE_LANE_ONLY`` value aborts pytest with ``USAGE_ERROR``."""
    test_root = _setup_synthetic_source_repo(tmp_path)
    (test_root / "test_tooling_ok.py").write_text(
        "import pytest\npytestmark = pytest.mark.tooling\ndef test_t():\n    assert True\n",
        encoding="utf-8",
    )

    proc = _run_pytest(
        tmp_path,
        ["-q", "test/test_tooling_ok.py"],
        source_lane_value="bogus",
        extra_pythonpath=[REPO_ROOT / "src", REPO_ROOT / "mcp" / "src"],
    )
    combined = proc.stdout + proc.stderr
    assert proc.returncode == pytest.ExitCode.USAGE_ERROR, combined
    assert slb.SOURCE_LANE_ENV_VAR in combined, combined


@pytest.mark.e2e
@pytest.mark.os_compat
def test_version_scm_fallback_provides_clean_checkout_metadata(tmp_path: Path) -> None:
    """Clean checkout without ``_version_scm.py`` imports ``supy`` and ``supy._version_scm``."""
    fake_src = tmp_path / "src"
    fake_pkg = fake_src / "supy"
    fake_pkg.mkdir(parents=True)
    # Copy only tracked __init__.py (no generated _version_scm.py).
    shutil.copy2(REPO_ROOT / "src" / "supy" / "__init__.py", fake_pkg / "__init__.py")

    env = {
        key: value
        for key, value in os.environ.items()
        if key not in {"PYTHONPATH", slb.SOURCE_LANE_ENV_VAR}
    }
    # 1. Pure stdlib import (-S skips site.py / editable .pth loaders) without bootstrap:
    plain_script = (
        f"import sys; sys.path.insert(0, {str(fake_src)!r}); "
        "import supy; "
        "assert supy.__version__ == '0+unknown', supy.__version__"
    )
    plain_proc = subprocess.run(
        [sys.executable, "-S", "-P", "-c", plain_script],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert plain_proc.returncode == 0, plain_proc.stdout + plain_proc.stderr

    # 2. With source-lane bootstrap activated: `from supy import _version_scm` also resolves.
    bootstrap_script = (
        "import importlib.util, pathlib; "
        f"spec = importlib.util.spec_from_file_location('slb', {str(BOOTSTRAP_PATH)!r}); "
        "mod = importlib.util.module_from_spec(spec); "
        "spec.loader.exec_module(mod); "
        f"mod.activate(pathlib.Path({str(tmp_path)!r})); "
        "import supy; "
        "from supy import _version_scm; "
        "assert supy.__version__ == '0+unknown', supy.__version__; "
        "assert _version_scm.__commit_hash__ == 'unknown', _version_scm.__commit_hash__"
    )
    env[slb.SOURCE_LANE_ENV_VAR] = "1"
    proc = subprocess.run(
        [sys.executable, "-S", "-P", "-c", bootstrap_script],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr


def test_resolve_suews_cli_command_dispatches_python_in_source_lane() -> None:
    """In source-lane mode, CLI routes dispatch via ``sys.executable -c`` instead of venv scripts."""
    cmd = slb.resolve_suews_cli_command(
        "suews",
        repo_root=REPO_ROOT,
        env={slb.SOURCE_LANE_ENV_VAR: "1"},
    )
    assert cmd[0] == sys.executable
    assert cmd[1:3] == ["-P", "-c"]
    assert "supy.cmd.suews_cli" in cmd[3]
    assert "main" in cmd[3]

    alias_cmd = slb.resolve_suews_cli_command(
        "suews-validate",
        repo_root=REPO_ROOT,
        env={slb.SOURCE_LANE_ENV_VAR: "1"},
    )
    assert alias_cmd[1:3] == ["-P", "-c"]
    assert "validate_alias" in alias_cmd[3]

    with pytest.raises(ValueError, match="Unknown SUEWS CLI route"):
        slb.resolve_suews_cli_command(
            "suews-nonexistent",
            repo_root=REPO_ROOT,
            env={slb.SOURCE_LANE_ENV_VAR: "1"},
        )


@pytest.mark.e2e
@pytest.mark.os_compat
def test_source_lane_blocks_run_rust_bridge_load_while_keeping_pure_helpers(
    tmp_path: Path,
) -> None:
    """``supy._run_rust`` imports cleanly, but ``_load_rust_module`` fails loudly under source-lane mode."""
    test_root = _setup_synthetic_source_repo(tmp_path)
    (test_root / "test_run_rust_guard.py").write_text(
        "import pytest\n"
        "from supy import _run_rust\n\n"
        "pytestmark = pytest.mark.unit\n\n"
        "def test_pure_helper_ok_and_bridge_load_blocked():\n"
        "    assert _run_rust.OUTPUT_ALL_COLS > 0\n"
        "    with pytest.raises(RuntimeError) as exc_info:\n"
        "        _run_rust._load_rust_module()\n"
        "    assert type(exc_info.value.__cause__).__name__ == 'SourceLaneViolationError'\n",
        encoding="utf-8",
    )

    proc = _run_pytest(
        tmp_path,
        ["-q", "test/test_run_rust_guard.py"],
        source_lane_value="1",
        extra_pythonpath=[REPO_ROOT / "src", REPO_ROOT / "mcp" / "src"],
    )
    combined = proc.stdout + proc.stderr
    assert proc.returncode == 0, combined
    assert "1 passed" in combined, combined


@pytest.mark.os_compat
def test_configure_source_paths_normalises_relative_pythonpath(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Relative ``PYTHONPATH`` entries are resolved to absolute paths for child subprocesses."""
    (tmp_path / "src").mkdir()
    (tmp_path / "mcp" / "src").mkdir(parents=True)
    extra_rel = tmp_path / "extra"
    extra_rel.mkdir()

    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(sys, "path", list(sys.path))
    monkeypatch.setenv("PYTHONPATH", f"src{os.pathsep}extra")
    monkeypatch.delenv(slb.SOURCE_LANE_ROOT_ENV_VAR, raising=False)

    src_dir, mcp_dir = slb.configure_source_paths(tmp_path)

    entries = os.environ["PYTHONPATH"].split(os.pathsep)
    assert entries[:2] == [str(src_dir), str(mcp_dir)]
    assert str(extra_rel.resolve()) in entries
    assert all(Path(entry).is_absolute() for entry in entries)
    assert os.environ[slb.SOURCE_LANE_ROOT_ENV_VAR] == str(tmp_path.resolve())


@pytest.mark.e2e
@pytest.mark.os_compat
def test_ordinary_child_python_subprocess_inherits_compiled_extension_guard(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Ordinary ``python -c`` children inherit ``CompiledExtensionBlocker`` via startup hook."""
    monkeypatch.setenv(slb.SOURCE_LANE_ENV_VAR, "1")
    monkeypatch.setenv("PYTHONPATH", "")
    monkeypatch.delenv(slb.SOURCE_LANE_ROOT_ENV_VAR, raising=False)
    monkeypatch.setattr(sys, "path", list(sys.path))
    slb.configure_source_paths(REPO_ROOT)

    proc = subprocess.run(
        [sys.executable, "-c", "import supy.suews_bridge"],
        cwd=tmp_path,
        env=dict(os.environ),
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
        timeout=30,
    )
    assert proc.returncode != 0
    assert "SourceLaneViolationError" in proc.stderr


@pytest.mark.e2e
@pytest.mark.os_compat
def test_source_checkout_overrides_editable_finder_and_rejects_conflicting_cache(
    tmp_path: Path,
) -> None:
    """Source mode resolves ``supy`` from ``repo_root``, skips editable rebuilds, and rejects foreign caches."""
    checkout_a = tmp_path / "checkout_a"
    pkg_a = checkout_a / "src" / "supy"
    pkg_a.mkdir(parents=True)
    (pkg_a / "__init__.py").write_text(
        "SOURCE_MARKER = 'checkout_a'\n"
        "try:\n"
        "    from ._version_scm import __version__\n"
        "except ModuleNotFoundError:\n"
        "    __version__ = '0+unknown'\n",
        encoding="utf-8",
    )

    foreign_dir = tmp_path / "foreign_supy"
    foreign_dir.mkdir()
    foreign_init = foreign_dir / "__init__.py"
    foreign_init.write_text("SOURCE_MARKER = 'foreign'\n", encoding="utf-8")

    # 1. With a Meson-style editable meta-path finder ahead of PathFinder,
    #    activate(checkout_a) must disable it and resolve supy from checkout_a
    #    without invoking the editable finder's build hook.
    override_script = (
        "import importlib.abc, importlib.util, pathlib, sys\n"
        "build_calls = []\n"
        "class _FakeMesonEditableFinder(importlib.abc.MetaPathFinder):\n"
        "    __module__ = '_supy_editable_loader'\n"
        "    def find_spec(self, fullname, path, target=None):\n"
        "        if fullname == 'supy' or fullname.startswith('supy.'):\n"
        "            build_calls.append(fullname)\n"
        "            raise AssertionError('editable loader should not be invoked')\n"
        "        return None\n"
        "fake_finder = _FakeMesonEditableFinder()\n"
        "sys.meta_path.insert(0, fake_finder)\n"
        f"spec = importlib.util.spec_from_file_location('slb', {str(BOOTSTRAP_PATH)!r})\n"
        "mod = importlib.util.module_from_spec(spec)\n"
        "spec.loader.exec_module(mod)\n"
        f"mod.activate(pathlib.Path({str(checkout_a)!r}))\n"
        "assert fake_finder not in sys.meta_path\n"
        "import supy\n"
        "from supy import _version_scm\n"
        "assert supy.SOURCE_MARKER == 'checkout_a', supy.SOURCE_MARKER\n"
        "assert supy.__version__ == '0+unknown', supy.__version__\n"
        "assert _version_scm.__commit_hash__ == 'unknown'\n"
        "assert build_calls == [], build_calls\n"
        "mod.uninstall_guards()\n"
        "assert fake_finder in sys.meta_path\n"
    )
    proc_override = subprocess.run(
        [sys.executable, "-S", "-P", "-c", override_script],
        cwd=tmp_path,
        env={
            key: value
            for key, value in os.environ.items()
            if key not in {"PYTHONPATH", slb.SOURCE_LANE_ENV_VAR}
        },
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert proc_override.returncode == 0, proc_override.stdout + proc_override.stderr

    # 2. If a foreign `supy` is already cached in sys.modules before activate(),
    #    activate(checkout_a) fails loudly with SourceLaneViolationError.
    conflict_script = (
        "import importlib.util, pathlib, sys, types; "
        "foreign = types.ModuleType('supy'); "
        f"foreign.__file__ = {str(foreign_init)!r}; "
        f"foreign.__path__ = [{str(foreign_dir)!r}]; "
        "sys.modules['supy'] = foreign; "
        f"spec = importlib.util.spec_from_file_location('slb', {str(BOOTSTRAP_PATH)!r}); "
        "mod = importlib.util.module_from_spec(spec); "
        "spec.loader.exec_module(mod); "
        f"mod.activate(pathlib.Path({str(checkout_a)!r}))"
    )
    proc_conflict = subprocess.run(
        [sys.executable, "-S", "-P", "-c", conflict_script],
        cwd=tmp_path,
        env={
            key: value
            for key, value in os.environ.items()
            if key not in {"PYTHONPATH", slb.SOURCE_LANE_ENV_VAR}
        },
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert proc_conflict.returncode != 0
    assert "SourceLaneViolationError" in proc_conflict.stderr


@pytest.mark.e2e
@pytest.mark.os_compat
def test_resolved_source_cli_command_resists_cwd_shadow_modules(
    tmp_path: Path,
) -> None:
    """``resolve_suews_cli_command`` uses ``-P`` so CWD ``pathlib.py``/``click.py`` cannot shadow imports."""
    (tmp_path / "pathlib.py").write_text(
        "raise RuntimeError('cwd pathlib.py shadowed stdlib')\n", encoding="utf-8"
    )
    (tmp_path / "click.py").write_text(
        "raise RuntimeError('cwd click.py shadowed dependency')\n", encoding="utf-8"
    )
    cmd = slb.resolve_suews_cli_command(
        "suews",
        repo_root=REPO_ROOT,
        env={slb.SOURCE_LANE_ENV_VAR: "1"},
    )
    child_env = {
        key: value
        for key, value in os.environ.items()
        if key not in {"PYTHONPATH", slb.SOURCE_LANE_ENV_VAR}
    }
    proc = subprocess.run(
        [*cmd, "--help"],
        cwd=tmp_path,
        env=child_env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
        timeout=60,
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "SUEWS unified command-line interface" in proc.stdout


@pytest.mark.e2e
@pytest.mark.os_compat
def test_resolved_source_cli_command_activates_deterministically_when_child_env_unset(
    tmp_path: Path,
) -> None:
    """A source-lane CLI command activates guards even if the child's env omits the var."""
    cmd = slb.resolve_suews_cli_command(
        "suews",
        repo_root=REPO_ROOT,
        env={slb.SOURCE_LANE_ENV_VAR: "1"},
    )
    child_env = {
        key: value
        for key, value in os.environ.items()
        if key not in {"PYTHONPATH", slb.SOURCE_LANE_ENV_VAR}
    }
    proc = subprocess.run(
        [*cmd, "--help"],
        cwd=tmp_path,
        env=child_env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
        timeout=60,
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "SUEWS unified command-line interface" in proc.stdout


@pytest.mark.e2e
@pytest.mark.os_compat
def test_standalone_conftest_fallback_blocks_cached_modules_and_cli_dispatch(
    tmp_path: Path,
) -> None:
    """Standalone ``conftest.py`` without bootstrap rejects cached compiled modules and CLI dispatch."""
    test_root = tmp_path / "test"
    test_root.mkdir(parents=True)
    (tmp_path / "pyproject.toml").write_text(_SYNTHETIC_PYPROJECT, encoding="utf-8")
    shutil.copy2(TEST_DIR / "conftest.py", test_root / "conftest.py")

    # 1. Pre-cached compiled module in sys.modules fails loudly on pytest_configure.
    cached_script = (
        "import importlib.util, sys, types; "
        "sys.modules['supy.suews_bridge'] = types.ModuleType('supy.suews_bridge'); "
        f"spec = importlib.util.spec_from_file_location('conftest_standalone', {str(test_root / 'conftest.py')!r}); "
        "mod = importlib.util.module_from_spec(spec); "
        "spec.loader.exec_module(mod); "
        "mod.pytest_configure(None)"
    )
    env_on = {
        key: value
        for key, value in os.environ.items()
        if key not in {"PYTHONPATH", slb.SOURCE_LANE_ROOT_ENV_VAR}
    }
    env_on[slb.SOURCE_LANE_ENV_VAR] = "1"
    cached_proc = subprocess.run(
        [sys.executable, "-c", cached_script],
        cwd=tmp_path,
        env=env_on,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert cached_proc.returncode != 0
    assert "SourceLaneViolationError" in cached_proc.stderr

    # 2. Fallback pytest_configure installs and pytest_unconfigure removes the blocker;
    #    resolve_suews_cli_command fails loudly under SUEWS_SOURCE_LANE_ONLY=1.
    cli_script = (
        "import importlib.util, sys\n"
        f"spec = importlib.util.spec_from_file_location('conftest_standalone', {str(test_root / 'conftest.py')!r})\n"
        "mod = importlib.util.module_from_spec(spec)\n"
        "spec.loader.exec_module(mod)\n"
        "mod.pytest_configure(None)\n"
        "assert any(type(f).__name__ == '_FallbackCompiledBlocker' for f in sys.meta_path)\n"
        "mod.pytest_unconfigure(None)\n"
        "assert not any(type(f).__name__ == '_FallbackCompiledBlocker' for f in sys.meta_path)\n"
        "mod.resolve_suews_cli_command('suews')\n"
    )
    cli_proc = subprocess.run(
        [sys.executable, "-c", cli_script],
        cwd=tmp_path,
        env=env_on,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert cli_proc.returncode != 0
    assert "SourceLaneViolationError" in cli_proc.stderr


@pytest.mark.e2e
@pytest.mark.os_compat
def test_code_examples_subprocess_blocks_bridge_and_keeps_host_pythonpath_isolated(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """``check_code_examples`` child runners activate source-lane guards without leaking host ``PYTHONPATH``."""
    checker_path = REPO_ROOT / "scripts" / "lint" / "check_code_examples.py"
    spec = importlib.util.spec_from_file_location(
        "_suews_check_code_examples", checker_path
    )
    assert spec is not None and spec.loader is not None
    checker = importlib.util.module_from_spec(spec)
    monkeypatch.setitem(sys.modules, spec.name, checker)
    spec.loader.exec_module(checker)

    # Create a host-only module on an unrelated PYTHONPATH directory.
    host_only_dir = tmp_path / "host_only_pkg"
    host_only_dir.mkdir()
    (host_only_dir / "host_secret_mod.py").write_text("SECRET = 42\n", encoding="utf-8")

    monkeypatch.setenv(slb.SOURCE_LANE_ENV_VAR, "1")
    monkeypatch.setenv("PYTHONPATH", str(host_only_dir))

    parser = doctest.DocTestParser()

    # 1. Host-only module must NOT leak into the isolated example runner.
    host_block = checker.ExampleBlock(
        block_index=0,
        examples=parser.get_examples(
            ">>> import host_secret_mod\n>>> host_secret_mod.SECRET\n42\n"
        ),
        lines=(1, 3),
        has_stated_output=True,
    )
    host_status, host_detail = checker.run_example_block(
        host_block, name="host_isolation", timeout=30.0
    )
    assert host_status == "prerequisite-missing", (host_status, host_detail)
    assert host_detail.get("exception_type") == "ModuleNotFoundError", host_detail

    # 2. Compiled bridge import inside an example MUST fail loudly with SourceLaneViolationError.
    bridge_block = checker.ExampleBlock(
        block_index=0,
        examples=parser.get_examples(">>> import supy.suews_bridge\n>>> 1\n1\n"),
        lines=(1, 3),
        has_stated_output=True,
    )
    bridge_status, bridge_detail = checker.run_example_block(
        bridge_block, name="bridge_blocked", timeout=30.0
    )
    assert bridge_status == "error", (bridge_status, bridge_detail)
    assert bridge_detail.get("exception_type") == "SourceLaneViolationError", (
        bridge_detail
    )

    # 3. Clean checkout `supy` + `supy._version_scm` still import inside the runner.
    supy_block = checker.ExampleBlock(
        block_index=0,
        examples=parser.get_examples(
            ">>> import supy\n>>> from supy import _version_scm\n>>> isinstance(supy.__version__, str)\nTrue\n"
        ),
        lines=(1, 4),
        has_stated_output=True,
    )
    supy_status, supy_detail = checker.run_example_block(
        supy_block, name="supy_from_source", timeout=30.0
    )
    assert supy_status == "ok", (supy_status, supy_detail)


@pytest.mark.e2e
@pytest.mark.os_compat
def test_sitecustomize_aborts_child_before_payload_on_startup_guard_failure(
    tmp_path: Path,
) -> None:
    """Startup hook exits non-zero before running user code on guard or env failures."""
    site_dir = REPO_ROOT / "scripts" / "suews" / "source_lane_site"
    base_env = {
        key: value
        for key, value in os.environ.items()
        if key
        not in {
            "PYTHONPATH",
            "PYTHONNOUSERSITE",
            "PYTHONUSERBASE",
            slb.SOURCE_LANE_ENV_VAR,
            slb.SOURCE_LANE_ROOT_ENV_VAR,
        }
    }

    # 1. A .pth file preloading supy.suews_bridge before sitecustomize must abort
    #    interpreter startup via SystemExit so the child payload never runs.
    import venv  # noqa: PLC0415

    tmp_venv = tmp_path / "pth_venv"
    builder = venv.EnvBuilder(symlinks=(os.name != "nt"), with_pip=False)
    builder.create(tmp_venv)
    context = builder.ensure_directories(tmp_venv)
    venv_site = next(tmp_venv.rglob("site-packages"))
    (venv_site / "preload_bridge.pth").write_text(
        "import sys, types; "
        "sys.modules['supy.suews_bridge'] = types.ModuleType('supy.suews_bridge')\n",
        encoding="utf-8",
    )
    pth_marker = tmp_path / "pth_payload_ran.txt"
    pth_env = {
        **base_env,
        "PYTHONPATH": str(site_dir),
        slb.SOURCE_LANE_ENV_VAR: "1",
        slb.SOURCE_LANE_ROOT_ENV_VAR: str(REPO_ROOT),
    }
    pth_proc = subprocess.run(
        [
            context.env_exe,
            "-c",
            f"from pathlib import Path; Path({str(pth_marker)!r}).write_text('ran', encoding='utf-8')",
        ],
        cwd=tmp_path,
        env=pth_env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
        timeout=30,
    )
    assert pth_proc.returncode != 0, pth_proc.stdout + pth_proc.stderr
    assert "SourceLaneViolationError" in pth_proc.stderr, pth_proc.stderr
    assert not pth_marker.exists(), "Child payload ran despite startup guard violation"

    # 2. An invalid SUEWS_SOURCE_LANE_ONLY token must also abort before payload runs.
    bad_marker = tmp_path / "invalid_env_payload_ran.txt"
    bad_env = {
        **base_env,
        "PYTHONPATH": str(site_dir),
        slb.SOURCE_LANE_ENV_VAR: "maybe",
        slb.SOURCE_LANE_ROOT_ENV_VAR: str(REPO_ROOT),
    }
    bad_proc = subprocess.run(
        [
            sys.executable,
            "-c",
            f"from pathlib import Path; Path({str(bad_marker)!r}).write_text('ran', encoding='utf-8')",
        ],
        cwd=tmp_path,
        env=bad_env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
        timeout=30,
    )
    assert bad_proc.returncode != 0, bad_proc.stdout + bad_proc.stderr
    assert "ValueError" in bad_proc.stderr, bad_proc.stderr
    assert not bad_marker.exists(), "Child payload ran despite invalid env token"


@pytest.mark.e2e
@pytest.mark.os_compat
def test_pytest_main_source_to_normal_transition_and_failed_collection_cleanup(
    tmp_path: Path,
) -> None:
    """``pytest.main()`` unwinds source-mode guards/paths/env/cache on both success and failure."""
    checkout = tmp_path / "checkout"
    test_root = _setup_synthetic_source_repo(checkout)
    pkg_dir = checkout / "src" / "supy"
    pkg_dir.mkdir(parents=True)
    (pkg_dir / "__init__.py").write_text(
        "SOURCE_MARKER = 'from_checkout'\nfrom ._version_scm import __version__\n",
        encoding="utf-8",
    )
    (test_root / "test_unit_ok.py").write_text(
        "import sys\n"
        "import pytest\n"
        "import supy\n"
        "from supy import _version_scm\n\n"
        "pytestmark = pytest.mark.unit\n\n"
        "def test_source_active():\n"
        "    assert supy.SOURCE_MARKER == 'from_checkout'\n"
        "    assert _version_scm.__commit_hash__ == 'unknown'\n"
        "    assert any(type(f).__name__ == 'CompiledExtensionBlocker' for f in sys.meta_path)\n",
        encoding="utf-8",
    )
    (test_root / "test_native_blocked.py").write_text(
        "import pytest\n"
        "pytestmark = pytest.mark.native\n"
        "def test_native():\n"
        "    assert True\n",
        encoding="utf-8",
    )

    foreign_dir = tmp_path / "foreign_supy"
    foreign_dir.mkdir()
    foreign_init = foreign_dir / "__init__.py"
    foreign_init.write_text("SOURCE_MARKER = 'foreign'\n", encoding="utf-8")

    driver = (
        "import importlib.abc, importlib.machinery, os, pathlib, sys, types\n"
        "import pytest\n"
        f"checkout = pathlib.Path({str(checkout)!r}).resolve()\n"
        "editable_hits = []\n"
        "class _FakeEditable(importlib.abc.MetaPathFinder):\n"
        "    __module__ = '_supy_editable_loader'\n"
        "    def find_spec(self, fullname, path, target=None):\n"
        "        if fullname == 'supy':\n"
        "            editable_hits.append(fullname)\n"
        "            return importlib.machinery.ModuleSpec('supy', _EditableLoader())\n"
        "        return None\n"
        "class _EditableLoader(importlib.abc.Loader):\n"
        "    def create_module(self, spec):\n"
        "        return None\n"
        "    def exec_module(self, module):\n"
        "        module.SOURCE_MARKER = 'from_editable'\n"
        "fake_editable = _FakeEditable()\n"
        "sys.meta_path[:] = [\n"
        "    f for f in sys.meta_path\n"
        "    if 'editable_loader' not in getattr(type(f), '__module__', '')\n"
        "]\n"
        "sys.meta_path.insert(1, fake_editable)\n"
        "orig_idx = sys.meta_path.index(fake_editable)\n"
        "os.environ['PYTHONPATH'] = 'sentinel_pp'\n"
        "os.environ.pop('SUEWS_SOURCE_LANE_ROOT', None)\n"
        "os.chdir(checkout)\n"
        # 1. Source-mode pytest.main() followed by normal-mode import in same interpreter
        "os.environ['SUEWS_SOURCE_LANE_ONLY'] = '1'\n"
        "rc1 = pytest.main(['-q', '-m', 'unit', 'test/'])\n"
        "assert rc1 == 0, rc1\n"
        "assert editable_hits == [], editable_hits\n"
        "os.environ.pop('SUEWS_SOURCE_LANE_ONLY', None)\n"
        "assert sys.meta_path.index(fake_editable) == orig_idx, sys.meta_path\n"
        "assert not any(type(f).__name__ in {'CompiledExtensionBlocker', 'SourceCheckoutFinder', 'VersionScmFallbackFinder'} for f in sys.meta_path)\n"
        "assert os.environ.get('PYTHONPATH') == 'sentinel_pp', os.environ.get('PYTHONPATH')\n"
        "assert 'SUEWS_SOURCE_LANE_ROOT' not in os.environ\n"
        "assert str(checkout / 'src') not in sys.path\n"
        "assert 'supy' not in sys.modules and 'supy._version_scm' not in sys.modules\n"
        "import supy as normal_supy\n"
        "assert normal_supy.SOURCE_MARKER == 'from_editable'\n"
        "assert editable_hits == ['supy']\n"
        "sys.modules.pop('supy', None)\n"
        # 2. Failed collection (UsageError on native axis) must also unwind completely
        "os.environ['SUEWS_SOURCE_LANE_ONLY'] = '1'\n"
        "rc2 = pytest.main(['-q', '-m', 'native', 'test/'])\n"
        "assert rc2 == pytest.ExitCode.USAGE_ERROR, rc2\n"
        "assert sys.meta_path.index(fake_editable) == orig_idx, sys.meta_path\n"
        "assert not any(type(f).__name__ in {'CompiledExtensionBlocker', 'SourceCheckoutFinder', 'VersionScmFallbackFinder'} for f in sys.meta_path)\n"
        "assert os.environ.get('PYTHONPATH') == 'sentinel_pp'\n"
        "assert 'SUEWS_SOURCE_LANE_ROOT' not in os.environ\n"
        # 3. Partial activate() failure leaves path/env/meta_path untouched
        "slb_mod = sys.modules['_suews_source_lane_bootstrap']\n"
        "foreign = types.ModuleType('supy')\n"
        f"foreign.__file__ = {str(foreign_init)!r}\n"
        f"foreign.__path__ = [{str(foreign_dir)!r}]\n"
        "sys.modules['supy'] = foreign\n"
        "before_path = list(sys.path)\n"
        "before_meta = list(sys.meta_path)\n"
        "try:\n"
        "    slb_mod.activate(checkout)\n"
        "    raise AssertionError('expected SourceLaneViolationError')\n"
        "except slb_mod.SourceLaneViolationError:\n"
        "    pass\n"
        "assert sys.path == before_path\n"
        "assert sys.meta_path == before_meta\n"
        "assert os.environ.get('PYTHONPATH') == 'sentinel_pp'\n"
        "assert 'SUEWS_SOURCE_LANE_ROOT' not in os.environ\n"
    )
    clean_env = {
        key: value
        for key, value in os.environ.items()
        if key
        not in {
            "PYTHONPATH",
            slb.SOURCE_LANE_ENV_VAR,
            slb.SOURCE_LANE_ROOT_ENV_VAR,
        }
    }
    proc = subprocess.run(
        [sys.executable, "-c", driver],
        cwd=tmp_path,
        env=clean_env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
        timeout=60,
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr


@pytest.mark.e2e
@pytest.mark.os_compat
def test_inherited_sitecustomize_bootstrap_reused_by_conftest_without_duplicates(
    tmp_path: Path,
) -> None:
    """Conftest reuses startup-loaded bootstrap without duplicating finders or losing ownership."""
    site_dir = REPO_ROOT / "scripts" / "suews" / "source_lane_site"
    script = (
        "import importlib.util, sys\n"
        "startup_mod = sys.modules.get('_suews_source_lane_bootstrap')\n"
        "assert startup_mod is not None and startup_mod.is_active()\n"
        f"spec = importlib.util.spec_from_file_location('conftest_inherited', {str(TEST_DIR / 'conftest.py')!r})\n"
        "conftest_mod = importlib.util.module_from_spec(spec)\n"
        "spec.loader.exec_module(conftest_mod)\n"
        "assert conftest_mod._SOURCE_LANE_BOOTSTRAP is startup_mod\n"
        "dummy_cfg = object()\n"
        "conftest_mod.pytest_configure(dummy_cfg)\n"
        "blockers = [f for f in sys.meta_path if isinstance(f, startup_mod.CompiledExtensionBlocker)]\n"
        "checkouts = [f for f in sys.meta_path if isinstance(f, startup_mod.SourceCheckoutFinder)]\n"
        "fallbacks = [f for f in sys.meta_path if isinstance(f, startup_mod.VersionScmFallbackFinder)]\n"
        "assert len(blockers) == 1, blockers\n"
        "assert len(checkouts) == 1, checkouts\n"
        "assert len(fallbacks) == 1, fallbacks\n"
        # conftest did not own startup activation, so pytest_unconfigure preserves startup guards
        "conftest_mod.pytest_unconfigure(dummy_cfg)\n"
        "assert startup_mod.is_active()\n"
        "assert any(isinstance(f, startup_mod.CompiledExtensionBlocker) for f in sys.meta_path)\n"
        # Explicit deactivate on the shared module tears down startup-installed guards completely
        "assert startup_mod.deactivate() is True\n"
        "assert not startup_mod.is_active()\n"
        "assert not any(type(f).__name__ in {'CompiledExtensionBlocker', 'SourceCheckoutFinder', 'VersionScmFallbackFinder'} for f in sys.meta_path)\n"
    )
    child_env = {
        key: value
        for key, value in os.environ.items()
        if key
        not in {"PYTHONPATH", slb.SOURCE_LANE_ENV_VAR, slb.SOURCE_LANE_ROOT_ENV_VAR}
    }
    child_env["PYTHONPATH"] = str(site_dir)
    child_env[slb.SOURCE_LANE_ENV_VAR] = "1"
    child_env[slb.SOURCE_LANE_ROOT_ENV_VAR] = str(REPO_ROOT)

    proc = subprocess.run(
        [sys.executable, "-c", script],
        cwd=tmp_path,
        env=child_env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
        timeout=30,
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
