"""Unit tests for the gh#1300 full-tree marker-enforcement hook in test/conftest.py."""

from __future__ import annotations

from pathlib import Path
import sys

from _pytest.config import get_config
import pytest

TEST_DIR = Path(__file__).resolve().parents[1]
if str(TEST_DIR) not in sys.path:
    sys.path.insert(0, str(TEST_DIR))

import conftest as root_conftest  # noqa: E402

pytestmark = pytest.mark.api


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
