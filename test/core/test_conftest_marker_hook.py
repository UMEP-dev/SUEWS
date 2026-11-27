"""Unit and collection tests for the gh#1300 / gh#2349 marker-enforcement hook in test/conftest.py."""

from __future__ import annotations

from pathlib import Path
import shutil
import subprocess
import sys

from _pytest.config import get_config  # noqa: PLC2701
import pytest

TEST_DIR = Path(__file__).resolve().parents[1]
REPO_ROOT = TEST_DIR.parent
if str(TEST_DIR) not in sys.path:
    sys.path.insert(0, str(TEST_DIR))

import conftest as root_conftest  # noqa: E402

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


def _setup_synthetic_repo(tmp_path: Path, *, include_lint_script: bool = False) -> Path:
    """Copy root conftest (and optionally check_test_markers.py) into a temp repo."""
    test_root = tmp_path / "test"
    test_root.mkdir(parents=True, exist_ok=True)
    (tmp_path / "pyproject.toml").write_text(_SYNTHETIC_PYPROJECT, encoding="utf-8")
    shutil.copy2(TEST_DIR / "conftest.py", test_root / "conftest.py")
    if include_lint_script:
        lint_dir = tmp_path / "scripts" / "lint"
        lint_dir.mkdir(parents=True, exist_ok=True)
        shutil.copy2(
            REPO_ROOT / "scripts" / "lint" / "check_test_markers.py",
            lint_dir / "check_test_markers.py",
        )
    return test_root


@pytest.mark.parametrize(
    ("argv", "expected"),
    [
        ([], True),
        (["-m", "physics"], True),
        (["-m", "physics", "test/"], True),
        (["-m", "unit and not slow", "test"], True),
        (["-k", "foo", "--timeout", "60", "test/"], True),
        (["test/core/test_conftest_marker_hook.py"], False),
        (["test/core/test_conftest_marker_hook.py::test_x"], False),
        (["-m", "api", "test/core/"], False),
        (["test/", "test/core/"], False),
    ],
)
def test_invocation_targets_full_test_tree_handles_option_values(
    argv: list[str], expected: bool
) -> None:
    # ARRANGE
    cfg = get_config(argv)
    cfg.parse(argv, addopts=False)

    # ACT
    actual = root_conftest._invocation_targets_full_test_tree(cfg)

    # ASSERT
    assert actual is expected, f"argv={argv!r}: expected {expected}, got {actual}"


@pytest.mark.parametrize(
    "extra_args",
    [
        ["test/"],
        ["-m", "physics", "test/"],
        ["-m", "unit", "test/"],
        ["-m", "api", "test/"],
        ["-k", "test_marked", "test/"],
    ],
)
def test_marker_hook_catches_unmarked_file_even_when_filtered(
    tmp_path: Path, extra_args: list[str]
) -> None:
    """Real root conftest rejects an unmarked test file under filtered full-tree runs."""
    # ARRANGE
    test_root = _setup_synthetic_repo(tmp_path)
    (test_root / "test_marked.py").write_text(
        "import pytest\n\npytestmark = pytest.mark.physics\n\ndef test_marked():\n    assert True\n",
        encoding="utf-8",
    )
    (test_root / "test_unmarked.py").write_text(
        "def test_unmarked():\n    assert True\n",
        encoding="utf-8",
    )

    # ACT
    proc = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--collect-only",
            "-q",
            *extra_args,
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    # ASSERT
    combined = proc.stdout + proc.stderr
    assert proc.returncode == pytest.ExitCode.USAGE_ERROR, combined
    assert "test_unmarked.py" in combined, combined


def test_marker_hook_rejects_dual_primary_axis_item(tmp_path: Path) -> None:
    """Full-tree collection rejects any item carrying two primary dependency axes."""
    test_root = _setup_synthetic_repo(tmp_path)
    (test_root / "test_dual.py").write_text(
        "import pytest\n\n"
        "pytestmark = pytest.mark.unit\n\n"
        "@pytest.mark.physics\n"
        "def test_dual_axis():\n"
        "    assert True\n",
        encoding="utf-8",
    )

    proc = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--collect-only",
            "-q",
            "-m",
            "physics",
            "test/",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    combined = proc.stdout + proc.stderr
    assert proc.returncode == pytest.ExitCode.USAGE_ERROR, combined
    assert "test_dual_axis" in combined, combined
    assert "physics, unit" in combined, combined


def test_schema1_api_shim_and_locked_pr_axis_mapping(tmp_path: Path) -> None:
    """Schema-1 api shim covers tooling/unit/native (not physics) and maps locked PR files."""
    test_root = _setup_synthetic_repo(tmp_path, include_lint_script=True)
    (test_root / "test_tooling.py").write_text(
        "import pytest\npytestmark = pytest.mark.tooling\ndef test_t():\n    assert True\n",
        encoding="utf-8",
    )
    (test_root / "test_unit.py").write_text(
        "import pytest\npytestmark = pytest.mark.unit\ndef test_u():\n    assert True\n",
        encoding="utf-8",
    )
    (test_root / "test_native.py").write_text(
        "import pytest\npytestmark = pytest.mark.native\ndef test_n():\n    assert True\n",
        encoding="utf-8",
    )
    (test_root / "test_physics.py").write_text(
        "import pytest\npytestmark = pytest.mark.physics\ndef test_p():\n    assert True\n",
        encoding="utf-8",
    )
    locked_file = test_root / "core" / "test_util_gap_filler_debug.py"
    locked_file.parent.mkdir(parents=True, exist_ok=True)
    locked_file.write_text(
        "import pytest\npytestmark = pytest.mark.api\ndef test_locked():\n    assert True\n",
        encoding="utf-8",
    )
    locked_engine = test_root / "io_tests" / "test_engine_anthro_snow_guards.py"
    locked_engine.parent.mkdir(parents=True, exist_ok=True)
    locked_engine.write_text(
        "import pytest\npytestmark = pytest.mark.api\ndef test_locked_engine():\n    assert True\n",
        encoding="utf-8",
    )
    umep_dir = test_root / "umep"
    umep_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy2(TEST_DIR / "umep" / "conftest.py", umep_dir / "conftest.py")
    (umep_dir / "test_umep_sample.py").write_text(
        "def test_umep():\n    assert True\n",
        encoding="utf-8",
    )

    def _collect(expr: str) -> list[str]:
        proc = subprocess.run(
            [
                sys.executable,
                "-m",
                "pytest",
                "--collect-only",
                "-q",
                "-m",
                expr,
                "test/",
            ],
            cwd=tmp_path,
            capture_output=True,
            text=True,
            check=False,
        )
        assert proc.returncode == 0, (
            f"-m {expr!r} failed:\n{proc.stdout}\n{proc.stderr}"
        )
        return [line.strip() for line in proc.stdout.splitlines() if "::" in line]

    api_nodes = set(_collect("api"))
    assert api_nodes == {
        "test/test_tooling.py::test_t",
        "test/test_unit.py::test_u",
        "test/test_native.py::test_n",
        "test/core/test_util_gap_filler_debug.py::test_locked",
        "test/io_tests/test_engine_anthro_snow_guards.py::test_locked_engine",
        "test/umep/test_umep_sample.py::test_umep",
    }
    assert set(_collect("physics")) == {
        "test/test_physics.py::test_p",
        "test/io_tests/test_engine_anthro_snow_guards.py::test_locked_engine",
    }
    assert set(_collect("e2e")) == {
        "test/io_tests/test_engine_anthro_snow_guards.py::test_locked_engine",
    }
    assert set(_collect("tooling")) == {"test/test_tooling.py::test_t"}
    assert set(_collect("unit")) == {
        "test/test_unit.py::test_u",
        "test/core/test_util_gap_filler_debug.py::test_locked",
    }
    assert set(_collect("native")) == {
        "test/test_native.py::test_n",
        "test/umep/test_umep_sample.py::test_umep",
    }
    assert set(_collect("os_compat")) == {"test/umep/test_umep_sample.py::test_umep"}


def test_marker_hook_stays_out_of_way_for_single_file_run(tmp_path: Path) -> None:
    """Narrow single-file invocations do not trigger full-tree marker lint."""
    # ARRANGE
    test_root = _setup_synthetic_repo(tmp_path)
    (test_root / "test_unmarked.py").write_text(
        "def test_unmarked():\n    assert True\n",
        encoding="utf-8",
    )

    # ACT
    proc = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--collect-only",
            "-q",
            "test/test_unmarked.py",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    # ASSERT
    assert proc.returncode == 0, proc.stdout + proc.stderr
