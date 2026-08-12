"""Tests for scripts/lint/check_packaged_data_paths.py.

A lint that has only been seen to pass is not tested, so this exercises
both directions: a tree carrying the anti-pattern must fail, and a swept
tree must pass.

Three properties matter beyond the basic pass/fail:

- ``conftest.py`` is covered. The sibling ``check_test_markers.py`` globs
  ``test_*.py``, which would miss it -- and ``test/conftest.py`` was one of
  the files carrying the anti-pattern, so a reintroduction there has to be
  caught.
- Bare ``__file__`` stays legal. Dozens of tests locate their own fixtures
  with ``Path(__file__).parent``; flagging those would make the lint
  unusable.
- Offenders are reported in line order. ``find_hits`` walks the AST
  breadth-first, so without an explicit sort the report would come out in
  tree order, which reads as noise in a CI log.
"""

from __future__ import annotations

import importlib.util
from pathlib import Path
import sys

import pytest

pytestmark = pytest.mark.api


SCRIPT_PATH = (
    Path(__file__).resolve().parents[2]
    / "scripts"
    / "lint"
    / "check_packaged_data_paths.py"
)
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "check_packaged_data_paths", SCRIPT_PATH
)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
check_packaged_data_paths = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = check_packaged_data_paths
SCRIPT_SPEC.loader.exec_module(check_packaged_data_paths)


CLEAN_SOURCE = '''\
"""A test module that locates packaged data correctly."""

from importlib.resources import as_file
from pathlib import Path

from supy._env import trv_supy_module

FIXTURES = Path(__file__).parent.parent / "fixtures"


def test_something():
    config = trv_supy_module / "sample_data" / "sample_config.yml"
    assert config.is_file()
    with as_file(config) as path_config:
        assert str(path_config)
'''

OFFENDING_SOURCE = '''\
"""A test module reaching packaged data through the module file."""

from pathlib import Path

import supy as sp


def test_something():
    sample_dir = Path(sp.__file__).parent / "sample_data"
    assert (sample_dir / "sample_config.yml").exists()
'''


def _make_repo(tmp_path: Path, files: dict[str, str]) -> Path:
    """Build a throwaway repo root containing ``test/`` with ``files``."""
    for rel, source in files.items():
        path = tmp_path / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(source, encoding="utf-8")
    return tmp_path


def _run(repo_root: Path) -> int:
    return check_packaged_data_paths.main(["check_packaged_data_paths", str(repo_root)])


def test_clean_tree_passes(tmp_path):
    """A tree using trv_supy_module and bare __file__ exits 0."""
    repo = _make_repo(tmp_path, {"test/test_clean.py": CLEAN_SOURCE})

    assert _run(repo) == 0


def test_offending_test_file_fails(tmp_path):
    """`Path(sp.__file__)` in a test module exits 1."""
    repo = _make_repo(tmp_path, {"test/test_offender.py": OFFENDING_SOURCE})

    assert _run(repo) == 1


def test_offending_conftest_fails(tmp_path):
    """The check must cover conftest.py, not only `test_*.py`.

    `check_test_markers.py` globs `test_*.py`; copying that glob here would
    leave `test/conftest.py` -- itself one of the swept files -- unguarded.
    """
    repo = _make_repo(tmp_path, {"test/conftest.py": OFFENDING_SOURCE})

    assert _run(repo) == 1


def test_bare_dunder_file_is_not_flagged(tmp_path):
    """`Path(__file__)` locates the test's own fixtures and stays legal."""
    source = (
        "from pathlib import Path\n"
        "\n"
        "FIXTURES = Path(__file__).resolve().parents[1] / 'fixtures'\n"
        "OWN_DIR = Path(__file__).parent\n"
    )
    repo = _make_repo(tmp_path, {"test/test_fixture_paths.py": source})

    assert _run(repo) == 0


def test_indirect_reach_is_flagged(tmp_path):
    """A reach that never calls `Path()` is still the same anti-pattern."""
    source = (
        "import os.path\n"
        "\n"
        "import supy\n"
        "\n"
        "SAMPLE = os.path.join(os.path.dirname(supy.__file__), 'sample_data')\n"
    )
    repo = _make_repo(tmp_path, {"test/test_indirect.py": source})

    assert _run(repo) == 1


def test_offenders_are_reported_in_line_order(tmp_path, capsys):
    """Offenders are listed by line number, not AST walk order.

    `find_hits` uses `ast.walk`, which is breadth-first, so a shallow late
    hit would otherwise be printed before a deeply nested early one.
    """
    source = (
        "import supy\n"
        "\n"
        "def f():\n"
        "    if True:\n"
        "        if True:\n"
        "            early = supy.__file__\n"
        "\n"
        "late = supy.__file__\n"
    )
    repo = _make_repo(tmp_path, {"test/test_order.py": source})

    assert _run(repo) == 1
    reported = [
        line for line in capsys.readouterr().err.splitlines() if "test_order.py:" in line
    ]
    assert reported == [
        "  - test/test_order.py:6: supy.__file__",
        "  - test/test_order.py:8: supy.__file__",
    ], reported


def test_missing_test_dir_fails(tmp_path):
    """Pointed at a tree with no `test/`, the lint fails rather than passing."""
    assert _run(tmp_path) == 1


def test_repo_tree_is_clean():
    """The real repository passes -- the sweep is complete."""
    repo_root = Path(__file__).resolve().parents[2]

    assert _run(repo_root) == 0
