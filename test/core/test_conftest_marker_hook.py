"""Unit and collection tests for the gh#1300 marker-enforcement hook in test/conftest.py."""

from __future__ import annotations

from pathlib import Path
import shutil
import subprocess
import sys

from _pytest.config import get_config  # noqa: PLC2701
import pytest

TEST_DIR = Path(__file__).resolve().parents[1]
if str(TEST_DIR) not in sys.path:
    sys.path.insert(0, str(TEST_DIR))

import conftest as root_conftest  # noqa: E402

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]


@pytest.mark.parametrize(
    ("argv", "expected"),
    [
        ([], True),
        (["-m", "physics"], True),
        (["-m", "physics", "test/"], True),
        (["-m", "api and not slow", "test"], True),
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
        ["-m", "api", "test/"],
        ["-k", "test_marked", "test/"],
    ],
)
def test_marker_hook_catches_unmarked_file_even_when_filtered(
    tmp_path: Path, extra_args: list[str]
) -> None:
    """Real root conftest rejects an unmarked test file under filtered full-tree runs."""
    # ARRANGE
    test_root = tmp_path / "test"
    test_root.mkdir()
    (tmp_path / "pyproject.toml").write_text(
        '[tool.pytest.ini_options]\nmarkers = ["physics: physics test", "api: api test"]\n',
        encoding="utf-8",
    )
    shutil.copy2(TEST_DIR / "conftest.py", test_root / "conftest.py")
    (test_root / "test_marked.py").write_text(
        "import pytest\n\n@pytest.mark.physics\ndef test_marked():\n    assert True\n",
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


def test_marker_hook_stays_out_of_way_for_single_file_run(tmp_path: Path) -> None:
    """Narrow single-file invocations do not trigger full-tree marker lint."""
    # ARRANGE
    test_root = tmp_path / "test"
    test_root.mkdir()
    (tmp_path / "pyproject.toml").write_text(
        '[tool.pytest.ini_options]\nmarkers = ["physics: physics test", "api: api test"]\n',
        encoding="utf-8",
    )
    shutil.copy2(TEST_DIR / "conftest.py", test_root / "conftest.py")
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
