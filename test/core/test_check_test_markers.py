"""Tests for scripts/lint/check_test_markers.py (gh#1300, gh#2349)."""

from __future__ import annotations

import errno
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest

pytestmark = [
    pytest.mark.tooling,
    pytest.mark.os_compat,
    pytest.mark.contract("C-TOOLING"),
]

REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "lint" / "check_test_markers.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location("check_test_markers", SCRIPT_PATH)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
check_test_markers = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = check_test_markers
SCRIPT_SPEC.loader.exec_module(check_test_markers)

LEGACY_API_TEST_SOURCE = (
    "import pytest\n\npytestmark = pytest.mark.api\n\ndef test_ok():\n    assert True\n"
)

TOOLING_TEST_SOURCE = (
    "import pytest\n\n"
    "pytestmark = pytest.mark.tooling\n\n"
    "def test_ok():\n"
    "    assert True\n"
)

UNIT_TEST_SOURCE = (
    "import pytest\n\n"
    "pytestmark = pytest.mark.unit\n\n"
    "def test_ok():\n"
    "    assert True\n"
)

NATIVE_TEST_SOURCE = (
    "import pytest\n\n"
    "pytestmark = [pytest.mark.native, pytest.mark.e2e]\n\n"
    "def test_ok():\n"
    "    assert True\n"
)

PHYSICS_TEST_SOURCE = (
    "import pytest\n\n"
    "pytestmark = [pytest.mark.physics]\n\n"
    "def test_ok():\n"
    "    assert True\n"
)

# Backward-compatible local fixture alias for lane-coverage tests below.
API_TEST_SOURCE = UNIT_TEST_SOURCE

UNMARKED_TEST_SOURCE = "def test_unmarked():\n    assert True\n"

DEFAULT_MAKEFILE = "TEST_DEFAULT_PATHS := test/test_surface.py test/core\n"


def _make_repo(
    tmp_path: Path,
    files: dict[str, str],
    *,
    makefile: str | None = DEFAULT_MAKEFILE,
) -> Path:
    """Create a synthetic repository layout under ``tmp_path``."""
    tmp_path.mkdir(parents=True, exist_ok=True)
    if makefile is not None:
        (tmp_path / "Makefile").write_text(makefile, encoding="utf-8")
    for rel, source in files.items():
        path = tmp_path / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(source, encoding="utf-8")
    return tmp_path


_SYMLINK_UNSUPPORTED_ERRNOS = frozenset({
    errno.EPERM,
    errno.EACCES,
    errno.EOPNOTSUPP,
    getattr(errno, "ENOTSUP", errno.EOPNOTSUPP),
})
_WINERROR_PRIVILEGE_NOT_HELD = 1314


def _create_symlink(
    link: Path,
    target: Path,
    *,
    target_is_directory: bool = False,
) -> None:
    """Create a symlink, skipping only on platform privilege or support limits."""
    try:
        link.symlink_to(target, target_is_directory=target_is_directory)
    except NotImplementedError as exc:
        pytest.skip(f"Symlink creation unsupported on this platform: {exc}")
    except PermissionError as exc:
        pytest.skip(f"Symlink creation requires elevated privileges: {exc}")
    except OSError as exc:
        if (
            exc.errno in _SYMLINK_UNSUPPORTED_ERRNOS
            or getattr(exc, "winerror", None) == _WINERROR_PRIVILEGE_NOT_HELD
        ):
            pytest.skip(f"Symlink creation unavailable on this runner: {exc}")
        raise


def _run(repo_root: Path) -> int:
    return check_test_markers.main(["check_test_markers.py", str(repo_root)])


def test_live_repository_passes(capsys) -> None:
    """Live repository tree satisfies marker-axis and lane-coverage checks."""
    rc = _run(REPO_ROOT)
    captured = capsys.readouterr()

    assert rc == 0, captured.err
    assert "[OK]" in captured.out
    assert captured.out.isascii()


def test_live_repository_cli_subprocess_passes() -> None:
    """Running the script directly as a subprocess exits 0 on the live repo."""
    proc = subprocess.run(
        [sys.executable, str(SCRIPT_PATH), str(REPO_ROOT)],
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "[OK]" in proc.stdout
    assert proc.stdout.isascii()


def test_parse_live_makefile_default_paths_exist() -> None:
    """Every entry parsed from the live Makefile exists under test/."""
    entries = check_test_markers.parse_makefile_test_default_paths(
        REPO_ROOT / "Makefile"
    )

    assert entries
    for entry in entries:
        assert (REPO_ROOT / entry).exists(), f"Missing path from Makefile: {entry}"


def test_default_paths_and_all_peripheral_dirs_pass(tmp_path: Path, capsys) -> None:
    """Files in TEST_DEFAULT_PATHS and all 5 peripheral dirs pass when valid."""
    files: dict[str, str] = {
        "test/test_surface.py": API_TEST_SOURCE,
        "test/core/test_core_unit.py": API_TEST_SOURCE,
        "test/core/nested/test_deep.py": PHYSICS_TEST_SOURCE,
        "test/cmd/test_cli.py": API_TEST_SOURCE,
        "test/mcp/test_mcp_tool.py": API_TEST_SOURCE,
        "test/docs/test_doc_links.py": API_TEST_SOURCE,
        "test/knowledge/test_kb.py": API_TEST_SOURCE,
        # test/umep/ picks up `api` automatically via AUTO_APPLIED_DIRS.
        "test/umep/test_plugin.py": UNMARKED_TEST_SOURCE,
    }
    repo = _make_repo(tmp_path, files)

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 0, captured.err
    assert "[OK]" in captured.out
    assert "1 files pick up a marker via conftest auto-apply" in captured.out


@pytest.mark.parametrize(
    "orphan_rel",
    [
        "test/orphan_dir/test_stray.py",
        "test/test_stray.py",
        "test/orphan_dir/stray_test.py",
        "test/orphan_dir/Test_example.py",
        "test/orphan_dir/example_TEST.py",
        "test/core_extra/test_prefix_collision.py",
        "test/fixtures/test_stray_in_fixtures.py",
    ],
)
def test_orphan_test_file_fails_with_diagnostic(
    tmp_path: Path, capsys, orphan_rel: str
) -> None:
    """Any pytest-discoverable file outside covered lanes fails with [X]."""
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_ok.py": API_TEST_SOURCE,
            orphan_rel: API_TEST_SOURCE,
        },
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert orphan_rel in captured.err
    assert captured.err.isascii()


def test_non_test_helpers_and_pycache_are_ignored(tmp_path: Path, capsys) -> None:
    """Fixture helpers, uppercase .PY files, and __pycache__ files are ignored."""
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_ok.py": API_TEST_SOURCE,
            "test/fixtures/helper_module.py": "VALUE = 1\n",
            "test/orphan_dir/test_UPPER.PY": UNMARKED_TEST_SOURCE,
            "test/orphan_dir/__pycache__/test_cached.py": UNMARKED_TEST_SOURCE,
        },
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 0, captured.err
    assert "[OK]" in captured.out


def test_makefile_continuations_comments_append_and_recipes(
    tmp_path: Path, capsys
) -> None:
    """Parser supports line continuations, comments, += and ignores recipes."""
    makefile = (
        "# Comment with TEST_DEFAULT_PATHS := test/ignored\n"
        "dummy: # explanation; avoid $(eval ...) here; FLAG := ignored\n"
        "\tTEST_DEFAULT_PATHS := test/recipe_ignored\n"
        "inline_eq: ; FOO=bar pytest\n"
        "\tTEST_DEFAULT_PATHS := test/recipe_ignored_2\n"
        "inline_prereq: dummy ; VAR := 1\n"
        "\tTEST_DEFAULT_PATHS := test/recipe_ignored_3\n"
        "export:\n"
        "\t@echo target named export\n"
        "vpath : dummy\n"
        "\t@echo target named vpath\n"
        "TEST_DEFAULT_PATHS := \\\n"
        "    test/test_surface.py \\\n"
        "    test/core # explanation; avoid $(eval ...) here\n"
        "TEST_DEFAULT_PATHS += test/io_tests\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_a.py": API_TEST_SOURCE,
            "test/io_tests/test_b.py": PHYSICS_TEST_SOURCE,
        },
        makefile=makefile,
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 0, captured.err
    assert "[OK]" in captured.out


@pytest.mark.parametrize(
    ("makefile_content", "expected_fragment"),
    [
        (None, "Makefile not found"),
        ("OTHER_VAR := test/core\n", "TEST_DEFAULT_PATHS assignment not found"),
        ("TEST_DEFAULT_PATHS :=   # nothing listed\n", "TEST_DEFAULT_PATHS is empty"),
        (
            "TEST_DEFAULT_PATHS := $(EXTRA_TESTS)\n",
            "variable reference or expansion is unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/missing_dir\n",
            "test/missing_dir",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core src/supy\n",
            "outside test/",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core ../outside\n",
            "parent traversal",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core\n"
            "ifeq (0,1)\n"
            "TEST_DEFAULT_PATHS += test/stray\n"
            "endif\n",
            "Makefile conditionals are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core\n"
            "ifeq (0,1)\n"
            "endif-extra := ignored\n"
            "TEST_DEFAULT_PATHS += test/orphan\n"
            "endif\n",
            "Makefile conditionals are unsupported",
        ),
        (
            "override TEST_DEFAULT_PATHS := test/core\n",
            "unsupported Make construct",
        ),
        (
            "define TEST_DEFAULT_PATHS\ntest/core\nendef\n",
            "Makefile define blocks are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core\n"
            "define UNUSED\n"
            "\tendef\n"
            "TEST_DEFAULT_PATHS += test/orphan\n"
            "endef\n",
            "Makefile define blocks are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core\nundefine TEST_DEFAULT_PATHS\n",
            "Makefile undefine directives are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core\ntest: TEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS != echo test/core\n",
            "unsupported Make construct",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "TEST_DEFAULT_PATHS :::= test/core\n",
            "unsupported Make construct",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "tab-indented construct outside a recipe",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "$(eval TEST_DEFAULT_PATHS := test/core)\n",
            "variable reference or expansion is unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "define TRIM\n"
            "TEST_DEFAULT_PATHS := test/core\n"
            "endef\n"
            "$(eval $(TRIM))\n",
            "Makefile define blocks are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "define TRIM\n"
            "$(eval TEST_DEFAULT_PATHS := test/core)\n"
            "endef\n"
            "UNUSED := $(TRIM)\n",
            "Makefile define blocks are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test:\n"
            "\t$(eval TEST_DEFAULT_PATHS := test/core)\n"
            "\tpytest $(TEST_DEFAULT_PATHS)\n",
            "eval/call inside recipe is unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test:\n"
            "\t$(call eval,TEST_DEFAULT_PATHS := test/core)\n"
            "\tpytest $(TEST_DEFAULT_PATHS)\n",
            "eval/call inside recipe is unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: ; # $(eval TEST_DEFAULT_PATHS := test/core)\n"
            "\tpytest $(TEST_DEFAULT_PATHS)\n",
            "eval/call inside inline recipe is unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "UNUSED := \\#$(eval TEST_DEFAULT_PATHS := test/core)\n",
            "variable reference or expansion is unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test:\n"
            "\t# $(eval TEST_DEFAULT_PATHS := test/core)\n",
            "eval/call inside recipe is unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core\n"
            " \tifeq (0,1)\n"
            "TEST_DEFAULT_PATHS += test/orphan\n"
            " \tendif\n",
            "Makefile conditionals are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "suffix := PATHS\n"
            "TEST_DEFAULT_$(suffix) := test/core\n",
            "variable reference or expansion is unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "suffix := PATHS\n"
            "TEST_DEFAULT_$(suffix:X=Y) := test/core\n",
            "variable reference or expansion is unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/aux\n",
            "Windows-reserved path component",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core.\n",
            "Windows-reserved path component",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "suffix := PATHS\n"
            "define TEST_DEFAULT_$(suffix)\n"
            "test/core\n"
            "endef\n",
            "Makefile define blocks are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core\\extra\n",
            "unsupported path characters",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core; true\n",
            "unsupported path characters",
        ),
        (
            "TEST_DEFAULT_PATHS := test/*\n",
            "unsupported path characters",
        ),
        (
            "include extra.mk\nTEST_DEFAULT_PATHS := test/core\n",
            "Makefile include directives are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: FLAG := literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: FLAG = literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: FLAG += literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: FLAG ?= literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: FLAG ::= literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: FLAG :::= literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: FLAG != echo literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: override FLAG := literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: private FLAG := literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: export FLAG := literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "%.o: CFLAGS := -O2\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "test: FLAG := a ; b\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile target-specific variable assignments are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "dummy:\n"
            "\t@echo ok\n"
            "export\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile export directives are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "dummy:\n"
            "\t@echo ok\n"
            "export FLAG\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile export directives are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "dummy:\n"
            "\t@echo ok\n"
            "override export FLAG := literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile export directives are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "dummy:\n"
            "\t@echo ok\n"
            "unexport\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile unexport directives are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "dummy:\n"
            "\t@echo ok\n"
            "unexport FLAG\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile unexport directives are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "dummy:\n"
            "\t@echo ok\n"
            "undefine FLAG\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile undefine directives are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "dummy:\n"
            "\t@echo ok\n"
            "override undefine FLAG\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile undefine directives are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "dummy:\n"
            "\t@echo ok\n"
            "vpath\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile vpath directives are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "dummy:\n"
            "\t@echo ok\n"
            "vpath %.py test/core:test/orphan\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "Makefile vpath directives are unsupported",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "dummy:\n"
            "\t@echo ok\n"
            "1FLAG := literal\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "tab-indented construct outside a recipe",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core test/orphan\n"
            "\tdummy:\n"
            "\tTEST_DEFAULT_PATHS := test/core\n",
            "tab-indented construct outside a recipe",
        ),
    ],
)
def test_invalid_makefile_fails_with_diagnostic(
    tmp_path: Path,
    capsys,
    makefile_content: str | None,
    expected_fragment: str,
) -> None:
    """Missing, unparseable, or broken Makefile TEST_DEFAULT_PATHS fails with [X]."""
    files = {
        "test/core/test_ok.py": API_TEST_SOURCE,
        "src/supy/__init__.py": "",
    }
    repo = _make_repo(tmp_path, files, makefile=makefile_content)

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert expected_fragment in captured.err
    assert captured.err.isascii()


def test_bare_carriage_return_in_comment_does_not_create_phantom_lane(
    tmp_path: Path, capsys
) -> None:
    """A bare CR inside a Makefile comment stays commented out like GNU Make."""
    repo = _make_repo(
        tmp_path,
        {
            "test/core/test_ok.py": API_TEST_SOURCE,
            "test/orphan/test_stray.py": API_TEST_SOURCE,
        },
        makefile=None,
    )
    (repo / "Makefile").write_bytes(
        b"TEST_DEFAULT_PATHS := test/core\n# note\rTEST_DEFAULT_PATHS += test/orphan\n"
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "test/orphan/test_stray.py" in captured.err
    assert captured.err.isascii()


def test_bare_carriage_return_at_eof_is_rejected(tmp_path: Path, capsys) -> None:
    """A trailing bare CR without LF is not stripped as CRLF."""
    repo = _make_repo(
        tmp_path,
        {"test/core/test_ok.py": API_TEST_SOURCE},
        makefile=None,
    )
    (repo / "Makefile").write_bytes(b"TEST_DEFAULT_PATHS := test/core\r")

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "unsupported control character" in captured.err
    assert captured.err.isascii()


@pytest.mark.parametrize(
    ("makefile_content", "expected_fragment"),
    [
        (
            "TEST_DEFAULT_PATHS := test/core\u00a0test/orphan\n",
            "unsupported path characters",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core\\\n\u00a0test/orphan\n",
            "unsupported path characters",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core\x0ctest/orphan\n",
            "unsupported control character",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core\n"
            "\fifeq (0,1)\n"
            "TEST_DEFAULT_PATHS += test/orphan\n"
            "endif\n",
            "unsupported control character",
        ),
        (
            "TEST_DEFAULT_PATHS := test/core\n"
            "\vifeq (0,1)\n"
            "TEST_DEFAULT_PATHS += test/orphan\n"
            "endif\n",
            "unsupported control character",
        ),
    ],
)
def test_non_make_whitespace_and_control_chars_are_rejected(
    tmp_path: Path, capsys, makefile_content: str, expected_fragment: str
) -> None:
    """Unicode NBSP and leading/inline ASCII control chars fail closed."""
    repo = _make_repo(
        tmp_path,
        {
            "test/core/test_ok.py": API_TEST_SOURCE,
            "test/orphan/test_stray.py": API_TEST_SOURCE,
        },
        makefile=makefile_content,
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert expected_fragment in captured.err
    assert captured.err.isascii()


def test_trailing_backslash_at_eof_without_newline_is_rejected(
    tmp_path: Path, capsys
) -> None:
    """A backslash at EOF with no following newline is preserved and rejected."""
    repo = _make_repo(
        tmp_path,
        {"test/core/test_ok.py": API_TEST_SOURCE},
        makefile=None,
    )
    (repo / "Makefile").write_bytes(b"TEST_DEFAULT_PATHS := test/core\\")

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "unsupported path characters" in captured.err
    assert captured.err.isascii()


@pytest.mark.parametrize(
    ("source", "should_pass"),
    [
        (TOOLING_TEST_SOURCE, True),
        (UNIT_TEST_SOURCE, True),
        (NATIVE_TEST_SOURCE, True),
        (PHYSICS_TEST_SOURCE, True),
        (
            "import pytest\n"
            'pytestmark = [pytest.mark.tooling, pytest.mark.os_compat, pytest.mark.contract("C-TOOLING")]\n',
            True,
        ),
        (
            "import pytest\npytestmark: object = pytest.mark.physics\n",
            True,
        ),
        (LEGACY_API_TEST_SOURCE, False),
        (
            "import pytest\npytestmark = [pytest.mark.unit, pytest.mark.physics]\n",
            False,
        ),
        (
            "import pytest\npytestmark = [pytest.mark.native, pytest.mark.physics]\n",
            False,
        ),
        (
            "import pytest\npytestmark = [pytest.mark.unit, pytest.mark.api]\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            "@pytest.mark.physics\n"
            "def test_conflict():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            "@pytest.mark.api\n"
            "def test_legacy_decorator():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            '@pytest.mark.parametrize("x", [pytest.param(1, marks=pytest.mark.physics)])\n'
            "def test_param_conflict(x):\n    assert x == 1\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            '@pytest.mark.parametrize(argnames="x", argvalues=[pytest.param(1, marks=pytest.mark.physics)])\n'
            "def test_kw_argvalues_conflict(x):\n    assert x == 1\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            '@pytest.mark.parametrize(argnames="x", argvalues=[pytest.param(1, marks=pytest.mark.cfg)])\n'
            "def test_kw_argvalues_orthogonal_ok(x):\n    assert x == 1\n",
            True,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            "CASES = [pytest.param(1, marks=pytest.mark.physics)]\n"
            '@pytest.mark.parametrize("x", CASES)\n'
            "def test_indirect_param_conflict(x):\n    assert x == 1\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            "class TestClassConflict:\n"
            "    pytestmark = pytest.mark.physics\n"
            "    def test_in_class(self):\n        assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            "class TestClassAnnotatedConflict:\n"
            "    pytestmark: object = [pytest.mark.native]\n"
            "    def test_in_class(self):\n        assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            "class TestClassOrthogonalOk:\n"
            "    pytestmark = pytest.mark.cfg\n"
            "    def test_in_class(self):\n        assert True\n",
            True,
        ),
        (
            "import pytest\n"
            "pytestmark = [pytest.mark.unit]\n"
            "pytestmark += [pytest.mark.physics]\n"
            "def test_augassign_conflict():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "OTHER = pytest.mark.physics\n"
            "pytestmark = [pytest.mark.unit, OTHER]\n"
            "def test_dynamic_pytestmark_rejected():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "OTHER = pytest.mark.physics\n"
            "pytestmark = pytest.mark.unit\n"
            "class TestDynamicClassMark:\n"
            "    pytestmark = OTHER\n"
            "    def test_in_class(self):\n        assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            '@getattr(pytest.mark, "physics")\n'
            "def test_dynamic_decorator_rejected():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            'axis = "physics"\n'
            "CASES = [pytest.param(1, marks=getattr(pytest.mark, axis))]\n"
            '@pytest.mark.parametrize("x", CASES)\n'
            "def test_indirect_dynamic_param_rejected(x):\n    assert x == 1\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = [pytest.mark.unit]\n"
            "pytestmark = []\n"
            "def test_reassigned_pytestmark_rejected():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = [pytest.mark.unit]\n"
            "pytestmark.append(pytest.mark.physics)\n"
            "def test_mutated_pytestmark_rejected():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            "if True:\n"
            "    pytestmark = pytest.mark.physics\n"
            "def test_conditional_pytestmark_rejected():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            "class TestCond:\n"
            "    if True:\n"
            "        pytestmark = pytest.mark.physics\n"
            "    def test_in_class(self):\n        assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            '@pytest.mark.parametrize("mark_value", [pytest.mark.physics])\n'
            "def test_marker_object_as_data_ok(mark_value):\n    assert mark_value is not None\n",
            True,
        ),
        (
            "import pytest\n"
            "pytestmark = [pytest.mark.unit]\n"
            "pytestmark, other = [], 0\n"
            "def test_destructured_pytestmark_rejected():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = [pytest.mark.unit]\n"
            "alias = pytestmark\n"
            "alias.clear()\n"
            "def test_aliased_pytestmark_rejected():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            "MARKS = []\n"
            "MARKS.append(pytest.mark.physics)\n"
            '@pytest.mark.parametrize("x", [pytest.param(1, marks=MARKS)])\n'
            "def test_mutated_module_marks_rejected(x):\n    assert x == 1\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            '@getattr(pytest.mark, "physics")()\n'
            "def test_called_dynamic_decorator_rejected():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            "@pytest.mark.physics.with_args()\n"
            "def test_with_args_decorator_conflict_rejected():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = (pytest.mark.unit,)\n"
            "def test_tuple_pytestmark_rejected():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = [[pytest.mark.unit]]\n"
            "def test_nested_list_pytestmark_rejected():\n    assert True\n",
            False,
        ),
        (
            "import pytest\n"
            "pytestmark = pytest.mark.unit\n"
            "def render(*, marks):\n    return marks\n"
            'def test_non_param_marks_kw_ok():\n    assert render(marks=["warning"]) == ["warning"]\n',
            True,
        ),
        (UNMARKED_TEST_SOURCE, False),
        (
            "import pytest\npytestmark = pytest.mark.smoke\n",
            False,
        ),
        (
            "import pytest\npytestmark = [pytest.mark.os_compat, pytest.mark.e2e]\n",
            False,
        ),
        ("def broken(:\n", False),
    ],
)
def test_axis_marker_detection(
    tmp_path: Path, capsys, source: str, should_pass: bool
) -> None:
    """Single-axis lint accepts one primary axis and rejects retired api, dual axes, or unmarked files."""
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": NATIVE_TEST_SOURCE,
            "test/core/test_subject.py": source,
        },
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    if should_pass:
        assert rc == 0, captured.err
        assert "[OK]" in captured.out
    else:
        assert rc == 1
        assert "[X]" in captured.err
        assert "test/core/test_subject.py" in captured.err
        assert captured.err.isascii()


def test_locked_pr_file_axes_policy_and_compatibility(tmp_path: Path, capsys) -> None:
    """LOCKED_PR_FILE_AXES maps only valid primary axes and allows legacy api on locked files."""
    assert check_test_markers.LOCKED_PR_FILE_AXES
    for rel_path, axis in check_test_markers.LOCKED_PR_FILE_AXES.items():
        assert rel_path.startswith("test/"), rel_path
        assert axis in check_test_markers.AXIS_MARKERS, (rel_path, axis)

    # Legacy `api` marker is accepted for a locked file, and migrated single axis is also accepted.
    locked_unit = "test/core/test_util_gap_filler_debug.py"
    locked_dual_phys = "test/io_tests/test_native_spartacus_ehc_guards.py"
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": NATIVE_TEST_SOURCE,
            locked_unit: LEGACY_API_TEST_SOURCE,
            locked_dual_phys: (
                "import pytest\n"
                "pytestmark = [pytest.mark.api, pytest.mark.physics]\n"
                "def test_ok():\n    assert True\n"
            ),
            "test/core/test_util_profile_manager_csv.py": UNIT_TEST_SOURCE,
        },
        makefile="TEST_DEFAULT_PATHS := test/test_surface.py test/core test/io_tests\n",
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 0, captured.err
    assert "3 locked open-PR compatibility files" in captured.out


def test_locked_pr_file_still_rejects_unmarked_or_dual_primary_axes(
    tmp_path: Path, capsys
) -> None:
    """Locked open-PR files still fail if unmarked or declaring two primary axes."""
    locked_unit = "test/core/test_util_gap_filler_debug.py"
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": NATIVE_TEST_SOURCE,
            locked_unit: (
                "import pytest\n"
                "pytestmark = [pytest.mark.unit, pytest.mark.native]\n"
                "def test_bad():\n    assert True\n"
            ),
        },
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert locked_unit in captured.err
    assert "multiple primary axis markers" in captured.err


def test_auto_applied_dir_rejects_explicit_axis_or_legacy_api(
    tmp_path: Path, capsys
) -> None:
    """Files under test/umep/ must not declare explicit axis or retired api markers."""
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": NATIVE_TEST_SOURCE,
            "test/umep/test_conflict.py": LEGACY_API_TEST_SOURCE,
        },
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "test/umep/test_conflict.py" in captured.err
    assert "auto-applied" in captured.err


def test_suffix_test_file_missing_marker_is_caught(tmp_path: Path, capsys) -> None:
    """Files named ``*_test.py`` are also checked for physics/api markers."""
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/sample_test.py": UNMARKED_TEST_SOURCE,
        },
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "test/core/sample_test.py" in captured.err


def test_mixed_case_test_file_missing_marker_is_caught(tmp_path: Path, capsys) -> None:
    """Windows-discoverable mixed-case .py test files are checked for markers."""
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/Test_unmarked.py": UNMARKED_TEST_SOURCE,
        },
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "test/core/Test_unmarked.py" in captured.err
    assert captured.err.isascii()


def test_invalid_utf8_in_auto_applied_dir_reports_read_error(
    tmp_path: Path, capsys
) -> None:
    """Invalid UTF-8 under test/umep/ still reports a read error."""
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_ok.py": API_TEST_SOURCE,
        },
    )
    bad_umep = repo / "test" / "umep" / "test_bad.py"
    bad_umep.parent.mkdir(parents=True, exist_ok=True)
    bad_umep.write_bytes(b"\xff\xfeinvalid utf8")

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "test/umep/test_bad.py" in captured.err
    assert captured.err.isascii()


def test_marker_and_orphan_failures_both_reported(tmp_path: Path, capsys) -> None:
    """A marker failure does not mask an orphan-file failure in the same run."""
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_unmarked.py": UNMARKED_TEST_SOURCE,
            "test/orphan_dir/test_stray.py": API_TEST_SOURCE,
        },
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "test/core/test_unmarked.py" in captured.err
    assert "test/orphan_dir/test_stray.py" in captured.err
    assert captured.err.count("[X]") >= 2


def test_covered_file_symlink_preserves_collection_identity(
    tmp_path: Path, capsys
) -> None:
    """A covered test symlink pointing inside the repo keeps its lane identity."""
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_ok.py": API_TEST_SOURCE,
            "test/fixtures/shared_case.py": API_TEST_SOURCE,
        },
    )
    alias = repo / "test" / "core" / "test_alias.py"
    _create_symlink(alias, repo / "test" / "fixtures" / "shared_case.py")

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 0, captured.err
    assert "[OK]" in captured.out


def test_escaping_file_symlink_reports_structured_error_without_masking(
    tmp_path: Path, capsys
) -> None:
    """An escaping symlink (even to non-UTF-8 bytes) emits [X] without masking."""
    outside_file = tmp_path / "outside_test.py"
    outside_file.write_bytes(b"\xff\xfeinvalid utf8")
    repo = _make_repo(
        tmp_path / "repo",
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_unmarked.py": UNMARKED_TEST_SOURCE,
        },
    )
    escape_link = repo / "test" / "core" / "test_escape.py"
    _create_symlink(escape_link, outside_file)

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "escapes repository root" in captured.err
    assert "test/core/test_escape.py" in captured.err
    assert "test/core/test_unmarked.py" in captured.err
    assert captured.err.isascii()


def test_invalid_utf8_in_repo_test_file_reports_structured_error(
    tmp_path: Path, capsys
) -> None:
    """An unreadable/non-UTF-8 test file emits [X] without masking orphan errors."""
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_ok.py": API_TEST_SOURCE,
            "test/orphan_dir/test_stray.py": API_TEST_SOURCE,
        },
    )
    (repo / "test" / "core" / "test_bad_utf8.py").write_bytes(b"\xff\xfe\x00")

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "unable to read test files as UTF-8" in captured.err
    assert "test/core/test_bad_utf8.py" in captured.err
    assert "test/orphan_dir/test_stray.py" in captured.err
    assert captured.err.isascii()


def test_directory_symlink_under_test_is_rejected(tmp_path: Path, capsys) -> None:
    """Directory symlinks under test/ fail closed rather than escaping discovery."""
    external_dir = tmp_path / "external_suite"
    external_dir.mkdir()
    (external_dir / "test_hidden.py").write_text(API_TEST_SOURCE, encoding="utf-8")
    repo = _make_repo(
        tmp_path / "repo",
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_ok.py": API_TEST_SOURCE,
        },
    )
    linked_dir = repo / "test" / "linked_suite"
    _create_symlink(linked_dir, external_dir, target_is_directory=True)

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "unsupported directory symlink" in captured.err
    assert "test/linked_suite" in captured.err
    assert captured.err.isascii()


def test_symlinked_test_root_is_rejected(tmp_path: Path, capsys) -> None:
    """A symlinked test/ root is rejected before traversal."""
    external_test = tmp_path / "external_test"
    (external_test / "core").mkdir(parents=True)
    (external_test / "core" / "test_ok.py").write_text(
        API_TEST_SOURCE, encoding="utf-8"
    )
    repo = tmp_path / "repo"
    repo.mkdir()
    (repo / "Makefile").write_text(
        "TEST_DEFAULT_PATHS := test/core\n", encoding="utf-8"
    )
    _create_symlink(repo / "test", external_test, target_is_directory=True)

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "unsupported directory symlink" in captured.err
    assert captured.err.isascii()


def test_nested_define_blocks_are_rejected(tmp_path: Path, capsys) -> None:
    """Makefile define blocks are rejected fail-closed rather than skipped."""
    makefile = (
        "TEST_DEFAULT_PATHS := test/core\n"
        "define OUTER\n"
        "define INNER\n"
        "unused\n"
        "endef\n"
        "TEST_DEFAULT_PATHS += test/orphan_dir\n"
        "endef\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/core/test_ok.py": API_TEST_SOURCE,
            "test/orphan_dir/test_stray.py": API_TEST_SOURCE,
        },
        makefile=makefile,
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "Makefile define blocks are unsupported" in captured.err


def test_cyclic_symlink_reports_structured_error_without_masking(
    tmp_path: Path, capsys
) -> None:
    """A self-referencing symlink emits [X] without crashing or masking markers."""
    repo = _make_repo(
        tmp_path / "repo",
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_unmarked.py": UNMARKED_TEST_SOURCE,
        },
    )
    loop_link = repo / "test" / "core" / "loop"
    _create_symlink(loop_link, loop_link)

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "cyclic or unresolvable symlink" in captured.err
    assert "test/core/loop" in captured.err
    assert "test/core/test_unmarked.py" in captured.err
    assert captured.err.isascii()


def test_broken_symlink_reports_structured_error(tmp_path: Path, capsys) -> None:
    """A broken symlink under test/ produces a structured [X] diagnostic."""
    repo = _make_repo(
        tmp_path / "repo",
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_ok.py": API_TEST_SOURCE,
        },
    )
    broken_link = repo / "test" / "core" / "test_broken.py"
    _create_symlink(broken_link, repo / "test" / "core" / "does_not_exist.py")

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "broken symlink under test/" in captured.err
    assert "test/core/test_broken.py" in captured.err
    assert captured.err.isascii()


def test_form_feed_inside_comment_does_not_split_line(tmp_path: Path, capsys) -> None:
    """Non-Make separators like form-feed stay inside comments."""
    makefile = (
        "TEST_DEFAULT_PATHS := test/core\n"
        "# note\fTEST_DEFAULT_PATHS += test/orphan_dir\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/core/test_ok.py": API_TEST_SOURCE,
            "test/orphan_dir/test_stray.py": API_TEST_SOURCE,
        },
        makefile=makefile,
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "test/orphan_dir/test_stray.py" in captured.err


def test_standalone_backslash_continuation_preserves_reassignment(
    tmp_path: Path, capsys
) -> None:
    """A standalone backslash joins the following tab-prefixed assignment."""
    makefile = (
        "TEST_DEFAULT_PATHS := test/core test/orphan_dir\n"
        "dummy:\n"
        "\\\n"
        "\tTEST_DEFAULT_PATHS := test/core\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/core/test_ok.py": API_TEST_SOURCE,
            "test/orphan_dir/test_stray.py": API_TEST_SOURCE,
        },
        makefile=makefile,
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "test/orphan_dir/test_stray.py" in captured.err


def test_even_trailing_backslashes_do_not_swallow_next_assignment(
    tmp_path: Path, capsys
) -> None:
    """Two trailing backslashes in a comment do not absorb the next assignment."""
    makefile = (
        "TEST_DEFAULT_PATHS := test/core test/orphan_dir\n"
        "# Two trailing backslashes: \\\\\n"
        "TEST_DEFAULT_PATHS := test/core\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/core/test_ok.py": API_TEST_SOURCE,
            "test/orphan_dir/test_stray.py": API_TEST_SOURCE,
        },
        makefile=makefile,
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "test/orphan_dir/test_stray.py" in captured.err


def test_non_searchable_default_lane_entry_reports_error_without_masking(
    tmp_path: Path, capsys
) -> None:
    """An inaccessible child explicitly listed in TEST_DEFAULT_PATHS does not crash."""
    if sys.platform == "win32" or (hasattr(os, "geteuid") and os.geteuid() == 0):
        pytest.skip("POSIX non-root directory permissions required for chmod(0o400)")

    repo = _make_repo(
        tmp_path,
        {
            "test/core/test_unmarked.py": UNMARKED_TEST_SOURCE,
            "test/non_searchable/test_hidden.py": API_TEST_SOURCE,
        },
        makefile=(
            "TEST_DEFAULT_PATHS := test/core test/non_searchable/test_hidden.py\n"
        ),
    )
    blocked = repo / "test" / "non_searchable"
    blocked.chmod(0o400)
    try:
        rc = _run(repo)
    finally:
        blocked.chmod(0o755)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "TEST_DEFAULT_PATHS entry cannot be inspected" in captured.err
    assert "test/non_searchable/test_hidden.py" in captured.err
    assert "test/core/test_unmarked.py" in captured.err
    assert captured.err.isascii()


def test_non_searchable_directory_reports_stat_error_without_masking(
    tmp_path: Path, capsys
) -> None:
    """A 0o400 directory (readable, non-searchable) reports stat errors without masking."""
    if sys.platform == "win32" or (hasattr(os, "geteuid") and os.geteuid() == 0):
        pytest.skip("POSIX non-root directory permissions required for chmod(0o400)")

    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_unmarked.py": UNMARKED_TEST_SOURCE,
            "test/non_searchable/test_hidden.py": API_TEST_SOURCE,
        },
    )
    blocked = repo / "test" / "non_searchable"
    blocked.chmod(0o400)
    try:
        rc = _run(repo)
    finally:
        blocked.chmod(0o755)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "unable to stat test entry" in captured.err
    assert "test/non_searchable/test_hidden.py" in captured.err
    assert "test/core/test_unmarked.py" in captured.err
    assert captured.err.isascii()


def test_unreadable_directory_reports_traversal_error(tmp_path: Path, capsys) -> None:
    """An unreadable directory under test/ fails instead of silently passing."""
    if sys.platform == "win32" or (hasattr(os, "geteuid") and os.geteuid() == 0):
        pytest.skip("POSIX non-root directory permissions required for chmod(0o000)")

    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_ok.py": API_TEST_SOURCE,
            "test/unreadable_dir/test_hidden.py": UNMARKED_TEST_SOURCE,
        },
    )
    blocked = repo / "test" / "unreadable_dir"
    blocked.chmod(0o000)
    try:
        rc = _run(repo)
    finally:
        blocked.chmod(0o755)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "unable to traverse test directory" in captured.err
    assert "test/unreadable_dir" in captured.err
    assert captured.err.isascii()


@pytest.mark.skipif(
    sys.platform != "win32",
    reason="Windows directory junctions exist only on Windows",
)
def test_windows_directory_junction_under_test_is_rejected(
    tmp_path: Path, capsys
) -> None:
    """A Windows directory junction under test/ fails closed instead of escaping."""
    external_dir = tmp_path / "external_suite"
    external_dir.mkdir()
    (external_dir / "test_hidden.py").write_text(API_TEST_SOURCE, encoding="utf-8")
    repo = _make_repo(
        tmp_path / "repo",
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_ok.py": API_TEST_SOURCE,
        },
    )
    junction_dir = repo / "test" / "core" / "external_junction"
    proc = subprocess.run(
        ["cmd", "/c", "mklink", "/J", str(junction_dir), str(external_dir)],
        capture_output=True,
        text=True,
        check=False,
    )
    if proc.returncode != 0:
        combined = f"{proc.stdout}\n{proc.stderr}"
        lowered = combined.lower()
        if "privilege" in lowered or "access is denied" in lowered:
            pytest.skip(f"mklink /J requires elevated privileges: {combined.strip()}")
        pytest.fail(f"mklink /J failed unexpectedly (rc={proc.returncode}): {combined}")

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert "unsupported directory symlink or junction" in captured.err
    assert "test/core/external_junction" in captured.err
    assert captured.err.isascii()


def test_non_ascii_orphan_path_is_escaped_in_diagnostics(
    tmp_path: Path, capsys
) -> None:
    """Diagnostics stay strictly ASCII even when a filename is non-ASCII."""
    orphan_name = "test/orphan_dir/test_caf\u00e9.py"
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
            "test/core/test_ok.py": API_TEST_SOURCE,
            orphan_name: API_TEST_SOURCE,
        },
    )

    rc = _run(repo)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err
    assert captured.err.isascii()
    assert "test_caf\\xe9.py" in captured.err


def test_missing_test_dir_fails(tmp_path: Path, capsys) -> None:
    """Repository root without test/ fails with [X]."""
    rc = _run(tmp_path)
    captured = capsys.readouterr()

    assert rc == 1
    assert "[X]" in captured.err


def test_find_orphan_test_files_accepts_custom_makefile_path(tmp_path: Path) -> None:
    """Helper accepts explicit makefile_path and test_root for isolated testing."""
    custom_mk = tmp_path / "alt.mk"
    custom_mk.write_text("TEST_DEFAULT_PATHS := test/core\n", encoding="utf-8")
    _make_repo(
        tmp_path,
        {
            "test/core/test_ok.py": API_TEST_SOURCE,
            "test/stray/test_bad.py": API_TEST_SOURCE,
        },
        makefile=None,
    )

    errors, orphans = check_test_markers.find_orphan_test_files(
        tmp_path,
        makefile_path=custom_mk,
        test_root=tmp_path / "test",
    )

    assert errors == []
    assert orphans == ["test/stray/test_bad.py"]


# =============================================================================
# Static AST guards (gh#2349 W1-8): parametrised simulation & subprocess e2e
# =============================================================================


@pytest.mark.smoke
@pytest.mark.core
def test_parametrised_simulation_guard_catches_direct_calls_and_allows_fixtures(
    tmp_path: Path, capsys
) -> None:
    """Direct from_sample_data()/.run() inside @parametrize fails; fixture-injected passes."""
    bad_direct = (
        "import pytest\n"
        "import supy as sp\n"
        "pytestmark = pytest.mark.physics\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_direct_sample(x):\n"
        "    sim = sp.SUEWSSimulation.from_sample_data()\n"
        "    sim.run()\n"
    )
    bad_class = (
        "import pytest\n"
        "pytestmark = pytest.mark.physics\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "class TestParamClass:\n"
        "    def test_method_run(self, x):\n"
        "        sim = build_sim(x)\n"
        "        sim.run()\n"
    )
    good_fixture = (
        "import asyncio\n"
        "import subprocess\n"
        "import subprocess as sub\n"
        "import pytest\n"
        "import supy as sp\n"
        "pytestmark = pytest.mark.physics\n\n"
        "@pytest.fixture(scope='module')\n"
        "def shared_output():\n"
        "    sim = sp.SUEWSSimulation.from_sample_data()\n"
        "    return sim.run()\n\n"
        "@pytest.mark.parametrize('col', ['QH', 'QE'])\n"
        "def test_reads_fixture(shared_output, col):\n"
        "    subprocess.run(['git', 'status'], check=False)\n"
        "    sub.run(['echo', 'ok'], check=False)\n"
        "    asyncio.run(dummy())\n"
        "    assert col in shared_output\n\n"
        "def test_non_parametrised_run():\n"
        "    sim = sp.SUEWSSimulation.from_sample_data()\n"
        "    sim.run()\n"
    )

    good_repo = _make_repo(
        tmp_path / "good",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_good.py": good_fixture,
        },
    )
    assert _run(good_repo) == 0, capsys.readouterr().err
    capsys.readouterr()

    bad_repo = _make_repo(
        tmp_path / "bad",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_bad_direct.py": bad_direct,
            "test/core/test_bad_class.py": bad_class,
        },
    )
    rc = _run(bad_repo)
    captured = capsys.readouterr()
    assert rc == 1
    assert "parametrised-simulation" in captured.err
    assert "test/core/test_bad_direct.py::test_direct_sample" in captured.err
    assert "test/core/test_bad_class.py::TestParamClass::test_method_run" in captured.err
    assert captured.err.isascii()


@pytest.mark.smoke
@pytest.mark.core
@pytest.mark.parametrize(
    ("call_snippet", "expected_cmd"),
    [
        ("subprocess.run(['suews', '--help'])", "suews"),
        ("subprocess.Popen(['suews-validate', 'cfg.yml'])", "suews-validate"),
        ("subprocess.check_output(['suews-mcp', '--version'])", "suews-mcp"),
        ("subprocess.check_call(['suews-engine', 'run', 'cfg.yml'])", "suews-engine"),
        ("subprocess.run(['suews-convert', 'in.nml'])", "suews-convert"),
        ("subprocess.run(['suews-run', 'cfg.yml'])", "suews-run"),
        ("subprocess.run(['suews-schema', 'export'])", "suews-schema"),
        ("subprocess.run([sys.executable, '-m', 'supy.cmd.suews_cli', '--help'])", "python -m supy"),
        ("subprocess.run([str(suews_engine_bin), 'run', str(cfg)])", "suews-engine"),
    ],
)
def test_subprocess_e2e_guard_requires_e2e_marker_for_suews_clis(
    tmp_path: Path, capsys, call_snippet: str, expected_cmd: str
) -> None:
    """Every SUEWS CLI subprocess call requires @pytest.mark.e2e."""
    unmarked_src = (
        "import subprocess\n"
        "import sys\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "def test_cli_unmarked(suews_engine_bin, cfg):\n"
        f"    {call_snippet}\n"
    )
    marked_src = (
        "import subprocess\n"
        "import sys\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "@pytest.mark.e2e\n"
        "def test_cli_marked(suews_engine_bin, cfg):\n"
        f"    {call_snippet}\n"
    )

    bad_repo = _make_repo(
        tmp_path / "unmarked",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_cli.py": unmarked_src,
        },
    )
    rc_bad = _run(bad_repo)
    err = capsys.readouterr().err
    assert rc_bad == 1
    assert "missing-e2e-marker" in err
    assert expected_cmd in err

    good_repo = _make_repo(
        tmp_path / "marked",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_cli.py": marked_src,
        },
    )
    assert _run(good_repo) == 0, capsys.readouterr().err


def test_subprocess_e2e_guard_handles_helpers_class_module_marks_and_ignores_non_cli(
    tmp_path: Path, capsys
) -> None:
    """Module/class e2e marks and helper forwarding work; non-CLI subprocesses stay exempt."""
    helper_unmarked = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "def _invoke(exe, cfg):\n"
        "    cmd = [str(exe), 'run', str(cfg)]\n"
        "    return subprocess.run(cmd)\n\n"
        "def test_via_helper(suews_engine_bin, cfg):\n"
        "    _invoke(suews_engine_bin, cfg)\n"
    )
    class_marked_and_non_cli = (
        "import shutil\n"
        "import subprocess\n"
        "import sys\n"
        "import pytest\n"
        "pytestmark = pytest.mark.tooling\n\n"
        "@pytest.mark.e2e\n"
        "class TestCliGroup:\n"
        "    def test_one(self):\n"
        "        subprocess.run(['suews', '--version'])\n\n"
        "def test_non_suews_subprocesses():\n"
        "    compiler = shutil.which('gfortran')\n"
        "    subprocess.run([compiler, 'probe.f95'])\n"
        "    subprocess.run([sys.executable, '-m', 'pytest', '--version'])\n"
        "    subprocess.run(['git', 'rev-parse', 'HEAD'])\n"
    )

    bad_repo = _make_repo(
        tmp_path / "helper_bad",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_helper.py": helper_unmarked,
        },
    )
    assert _run(bad_repo) == 1
    assert "missing-e2e-marker" in capsys.readouterr().err

    good_repo = _make_repo(
        tmp_path / "good",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_ok_cli.py": class_marked_and_non_cli,
        },
    )
    assert _run(good_repo) == 0, capsys.readouterr().err


def test_e2e_subprocess_cap_counts_parametrised_nodes_and_enforces_limit(
    tmp_path: Path, capsys
) -> None:
    """Stacked @parametrize nodes on @e2e CLI tests are counted against the per-command cap."""
    over_cap_src = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = [pytest.mark.native, pytest.mark.e2e]\n\n"
        "@pytest.mark.parametrize('a', [1, 2, 3])\n"
        "@pytest.mark.parametrize('b', [1, 2, 3, 4])\n"
        "def test_twelve_cli_nodes(a, b):\n"
        "    subprocess.run(['suews-validate', f'{a}_{b}.yml'])\n"
    )
    repo = _make_repo(
        tmp_path / "over_cap",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_many.py": over_cap_src,
        },
    )
    rc = _run(repo)
    err = capsys.readouterr().err
    assert rc == 1
    assert "e2e-subprocess-cap" in err
    assert "`suews-validate` has 12 @pytest.mark.e2e subprocess test node(s)" in err

    # With a raised cap of 12, the same tree passes cleanly.
    rc_raised = check_test_markers.main(
        ["check_test_markers.py", str(repo), "--e2e-cap", "12"]
    )
    assert rc_raised == 0, capsys.readouterr().err


@pytest.mark.smoke
@pytest.mark.core
def test_ast_guards_baseline_ratchet_detects_new_and_stale_entries(
    tmp_path: Path, capsys
) -> None:
    """Baseline ratchet accepts pinned debt, rejects new debt, and rejects stale entries."""
    legacy_src = (
        "import subprocess\n"
        "import pytest\n"
        "import supy as sp\n"
        "pytestmark = pytest.mark.physics\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_legacy_sim(x):\n"
        "    sim = sp.SUEWSSimulation.from_sample_data()\n"
        "    assert sim is not None\n\n"
        "def test_legacy_unmarked_cli():\n"
        "    subprocess.run(['suews-engine', 'run', 'cfg.yml'])\n"
    )
    repo = _make_repo(
        tmp_path / "ratchet",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_legacy.py": legacy_src,
        },
    )
    (repo / "scripts" / "lint").mkdir(parents=True)

    # Missing baseline inside a repo with scripts/lint/ fails closed.
    assert _run(repo) == 1
    assert "missing AST guards baseline" in capsys.readouterr().err

    # --write-ast-baseline refuses when no baseline exists yet (requires --init-ast-baseline).
    assert (
        check_test_markers.main(
            ["check_test_markers.py", str(repo), "--write-ast-baseline"]
        )
        == 1
    )
    assert "--init-ast-baseline" in capsys.readouterr().err

    # Initialising the baseline records both legacy entries and passes immediately.
    rc_init = check_test_markers.main(
        ["check_test_markers.py", str(repo), "--init-ast-baseline"]
    )
    assert rc_init == 0, capsys.readouterr().err
    capsys.readouterr()
    assert _run(repo) == 0, capsys.readouterr().err
    capsys.readouterr()

    # Re-running --init-ast-baseline refuses to overwrite an existing baseline.
    assert (
        check_test_markers.main(
            ["check_test_markers.py", str(repo), "--init-ast-baseline"]
        )
        == 1
    )
    assert "already exists" in capsys.readouterr().err

    # Adding a second call in the baselined function (1 -> 2) fails immediately,
    # and --write-ast-baseline REFUSES to ratchet the baseline upward.
    increased_src = legacy_src.replace(
        "assert sim is not None", "sim.run()"
    )
    (repo / "test/core/test_legacy.py").write_text(increased_src, encoding="utf-8")
    assert _run(repo) == 1
    err_inc = capsys.readouterr().err
    assert "parametrised-simulation" in err_inc
    assert "(1 -> 2)" in err_inc
    assert (
        check_test_markers.main(
            ["check_test_markers.py", str(repo), "--write-ast-baseline"]
        )
        == 1
    )
    assert "downward-only" in capsys.readouterr().err

    # Fixing the legacy debt without lowering the baseline fails as stale!
    fixed_src = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.physics\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_legacy_sim(shared_sim, x):\n"
        "    assert shared_sim is not None\n\n"
        "@pytest.mark.e2e\n"
        "def test_legacy_unmarked_cli():\n"
        "    subprocess.run(['suews-engine', 'run', 'cfg.yml'])\n"
    )
    (repo / "test/core/test_legacy.py").write_text(fixed_src, encoding="utf-8")
    assert _run(repo) == 1
    err_stale = capsys.readouterr().err
    assert "baseline entries are stale" in err_stale
    assert "--write-ast-baseline" in err_stale

    # Refreshing the baseline lowers it to zero and passes again.
    assert (
        check_test_markers.main(
            ["check_test_markers.py", str(repo), "--write-ast-baseline"]
        )
        == 0
    )
    capsys.readouterr()
    assert _run(repo) == 0, capsys.readouterr().err


def test_ast_guards_baseline_loader_rejects_malformed_payloads(tmp_path: Path) -> None:
    """Baseline loader fails closed on wrong schema, bool schema, unknown keys, or bad caps."""
    bad_payloads = [
        '["not-an-object"]',
        '{"schema_version": 99}',
        '{"schema_version": true}',
        '{"schema_version": 1, "extra_key": 1}',
        '{"schema_version": 1, "parametrized_simulations": {"a::b": true}}',
        '{"schema_version": 1, "unmarked_subprocess_e2e": {"a::b": ["unknown-cli"]}}',
        '{"schema_version": 1, "e2e_command_caps": {"suews": 5}}',
    ]
    for idx, text in enumerate(bad_payloads):
        path = tmp_path / f"bad_{idx}.json"
        path.write_text(text, encoding="utf-8")
        loaded, errs = check_test_markers.load_ast_guards_baseline(path)
        assert loaded is None, text
        assert errs, text


def test_ast_guards_resolve_module_constants_and_scope_pytestmark_caps(
    tmp_path: Path, capsys
) -> None:
    """Module constants and module/class pytestmark parametrisation count toward AST guards."""
    const_unmarked = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n"
        "ARGV = ['suews', '--help']\n\n"
        "def test_const_argv():\n"
        "    subprocess.run(ARGV)\n"
    )
    const_cap = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = [pytest.mark.native, pytest.mark.e2e]\n"
        "CASES = range(11)\n\n"
        "@pytest.mark.parametrize('x', CASES)\n"
        "def test_const_cases(x):\n"
        "    subprocess.run(['suews', str(x)])\n"
    )
    mod_pm_cap = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = [pytest.mark.native, pytest.mark.e2e, pytest.mark.parametrize('x', range(11))]\n\n"
        "def test_mod_pm(x):\n"
        "    subprocess.run(['suews-validate', str(x)])\n"
    )
    cls_pm_cap = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = [pytest.mark.native, pytest.mark.e2e]\n\n"
        "class TestClsPm:\n"
        "    pytestmark = pytest.mark.parametrize('x', range(11))\n"
        "    def test_cls_pm(self, x):\n"
        "        subprocess.run(['suews-mcp', str(x)])\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_const_unmarked.py": const_unmarked,
            "test/core/test_const_cap.py": const_cap,
            "test/core/test_mod_pm_cap.py": mod_pm_cap,
            "test/core/test_cls_pm_cap.py": cls_pm_cap,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "missing-e2e-marker: test/core/test_const_unmarked.py::test_const_argv" in err
    assert "`suews` has 11 @pytest.mark.e2e subprocess test node(s)" in err
    assert "`suews-validate` has 11 @pytest.mark.e2e subprocess test node(s)" in err
    assert "`suews-mcp` has 11 @pytest.mark.e2e subprocess test node(s)" in err


def test_ast_guards_statement_order_branches_multi_hop_helpers_and_bindings(
    tmp_path: Path, capsys
) -> None:
    """Statement-order bindings, branches, multi-hop helpers, binding-based .run(), and python -m."""
    reassigned_and_branched = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "def _invoke(argv):\n"
        "    return subprocess.run(argv)\n\n"
        "def _help():\n"
        "    return _invoke(['suews', '--help'])\n\n"
        "def test_later_reassign_cannot_hide():\n"
        "    argv = ['suews']\n"
        "    subprocess.run(argv)\n"
        "    argv = ['git']\n\n"
        "def test_branch_retained(flag):\n"
        "    if flag:\n"
        "        argv = ['suews-validate']\n"
        "    else:\n"
        "        argv = ['git']\n"
        "    subprocess.run(argv)\n\n"
        "def test_multi_hop_helper():\n"
        "    _help()\n"
    )
    binding_based_sim = (
        "import subprocess\n"
        "from unittest import TextTestRunner as TR\n"
        "import pytest\n"
        "pytestmark = pytest.mark.physics\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_runner_var_not_exempt(x):\n"
        "    runner = SUEWSSimulation(x)\n"
        "    runner.run()\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_shadowed_subprocess_not_exempt(x):\n"
        "    subprocess = SUEWSSimulation(x)\n"
        "    subprocess.run()\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_aliased_non_sim_runner_and_echo_dash_m_allowed(x):\n"
        "    TR().run(x)\n"
        "    tr = TR()\n"
        "    tr.run(x)\n"
        "    subprocess.run(['echo', '-m', 'supy'])\n"
        "    subprocess.run('echo -m supy', shell=True)\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_flow.py": reassigned_and_branched,
            "test/core/test_bindings.py": binding_based_sim,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "test/core/test_flow.py::test_later_reassign_cannot_hide" in err
    assert "test/core/test_flow.py::test_branch_retained" in err
    assert "test/core/test_flow.py::test_multi_hop_helper" in err
    assert "test/core/test_bindings.py::test_runner_var_not_exempt" in err
    assert "test/core/test_bindings.py::test_shadowed_subprocess_not_exempt" in err
    assert "test_aliased_non_sim_runner_and_echo_dash_m_allowed" not in err


def test_ast_baseline_symlink_containment_and_incomplete_discovery(
    tmp_path: Path, capsys
) -> None:
    """Symlinked baseline leaf/parent and unreadable test dirs fail closed on read/write."""
    external = tmp_path / "external"
    external.mkdir()
    outside_file = external / "outside.json"
    outside_file.write_text("UNTOUCHED", encoding="utf-8")

    # 1) Symlinked baseline leaf refuses --init-ast-baseline and does not clobber target.
    repo_leaf = _make_repo(
        tmp_path / "repo_leaf",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_ok.py": UNIT_TEST_SOURCE,
        },
    )
    (repo_leaf / "scripts" / "lint").mkdir(parents=True)
    _create_symlink(
        repo_leaf / "scripts" / "lint" / "test_ast_guards_baseline.json",
        outside_file,
    )
    assert (
        check_test_markers.main(
            ["check_test_markers.py", str(repo_leaf), "--init-ast-baseline"]
        )
        == 1
    )
    assert "symlink" in capsys.readouterr().err
    assert outside_file.read_text(encoding="utf-8") == "UNTOUCHED"
    assert _run(repo_leaf) == 1
    assert "symlink" in capsys.readouterr().err

    # 2) Symlinked parent directory (scripts/lint -> external) fails closed on read & write.
    repo_parent = _make_repo(
        tmp_path / "repo_parent",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_ok.py": UNIT_TEST_SOURCE,
        },
    )
    (repo_parent / "scripts").mkdir()
    _create_symlink(
        repo_parent / "scripts" / "lint", external, target_is_directory=True
    )
    assert (
        check_test_markers.main(
            ["check_test_markers.py", str(repo_parent), "--init-ast-baseline"]
        )
        == 1
    )
    assert "symlink" in capsys.readouterr().err
    assert _run(repo_parent) == 1
    assert "symlink" in capsys.readouterr().err

    # 3) Unreadable test directory blocks --init-ast-baseline / --write-ast-baseline.
    if sys.platform != "win32" and (not hasattr(os, "geteuid") or os.geteuid() != 0):
        repo_unreadable = _make_repo(
            tmp_path / "repo_unreadable",
            {
                "test/test_surface.py": UNIT_TEST_SOURCE,
                "test/core/test_ok.py": UNIT_TEST_SOURCE,
                "test/unreadable_dir/test_hidden.py": UNIT_TEST_SOURCE,
            },
        )
        blocked = repo_unreadable / "test" / "unreadable_dir"
        blocked.chmod(0o000)
        try:
            rc = check_test_markers.main(
                ["check_test_markers.py", str(repo_unreadable), "--init-ast-baseline"]
            )
        finally:
            blocked.chmod(0o755)
        assert rc == 1
        assert "discovery" in capsys.readouterr().err


def test_live_ast_guards_baseline_is_valid() -> None:
    """Checked-in scripts/lint/test_ast_guards_baseline.json parses cleanly."""
    baseline_path = REPO_ROOT / check_test_markers.AST_GUARDS_BASELINE_PATH
    assert baseline_path.is_file()
    loaded, errs = check_test_markers.load_ast_guards_baseline(baseline_path)
    assert errs == []
    assert loaded is not None


def test_module_exec_without_sys_modules_registration() -> None:
    """Loading check_test_markers.py like test/conftest.py (not in sys.modules) succeeds."""
    spec = importlib.util.spec_from_file_location(
        "_suews_isolated_check_test_markers", SCRIPT_PATH
    )
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    assert spec.name not in sys.modules
    spec.loader.exec_module(mod)
    assert mod.AXIS_MARKERS == check_test_markers.AXIS_MARKERS


def test_ast_guards_baseline_ratchet_rejects_manual_baseline_increases(
    tmp_path: Path, capsys
) -> None:
    """Manually bumping test_ast_guards_baseline.json is rejected against trusted base."""
    legacy_src = (
        "import pytest\n"
        "import supy as sp\n"
        "pytestmark = pytest.mark.physics\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_legacy(x):\n"
        "    sim = sp.SUEWSSimulation.from_sample_data()\n"
        "    assert sim is not None\n"
    )
    repo = _make_repo(
        tmp_path / "git_ratchet",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_legacy.py": legacy_src,
        },
    )
    (repo / "scripts" / "lint").mkdir(parents=True)
    assert (
        check_test_markers.main(
            ["check_test_markers.py", str(repo), "--init-ast-baseline"]
        )
        == 0
    )
    capsys.readouterr()

    # Commit the baseline on `master` in a real temporary git repo.
    git_env = {
        **os.environ,
        "GIT_AUTHOR_NAME": "Test",
        "GIT_AUTHOR_EMAIL": "test@example.com",
        "GIT_COMMITTER_NAME": "Test",
        "GIT_COMMITTER_EMAIL": "test@example.com",
    }
    for cmd in (
        ["git", "-C", str(repo), "init", "-b", "master"],
        ["git", "-C", str(repo), "add", "."],
        ["git", "-C", str(repo), "commit", "-m", "base"],
        ["git", "-C", str(repo), "checkout", "-b", "pr-branch"],
    ):
        subprocess.run(cmd, env=git_env, check=True, capture_output=True)

    # Save a copy of the trusted base baseline for explicit --base-ast-baseline test too.
    baseline_path = repo / "scripts" / "lint" / "test_ast_guards_baseline.json"
    base_copy = tmp_path / "base_baseline.json"
    base_copy.write_text(baseline_path.read_text(encoding="utf-8"), encoding="utf-8")

    # Add a second simulation call AND manually bump the baseline count 1 -> 2.
    bumped_src = legacy_src.replace("assert sim is not None", "sim.run()")
    (repo / "test/core/test_legacy.py").write_text(bumped_src, encoding="utf-8")
    payload = json.loads(baseline_path.read_text(encoding="utf-8"))
    payload["parametrized_simulations"]["test/core/test_legacy.py::test_legacy"] = 2
    baseline_path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    subprocess.run(
        ["git", "-C", str(repo), "commit", "-am", "manual bump"],
        env=git_env,
        check=True,
        capture_output=True,
    )

    # Automatic git base discovery (`master`) rejects the manual bump!
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "AST guards baseline increased parametrised-simulation debt" in err
    assert "(1 -> 2)" in err

    # Explicit --base-ast-baseline also rejects it.
    assert (
        check_test_markers.main([
            "check_test_markers.py",
            str(repo),
            "--base-ast-baseline",
            str(base_copy),
        ])
        == 1
    )
    assert "increased parametrised-simulation debt" in capsys.readouterr().err

    # Legitimate reduction (fixing the test and lowering the baseline) passes!
    fixed_src = (
        "import pytest\n"
        "pytestmark = pytest.mark.physics\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_legacy(shared_sim, x):\n"
        "    assert shared_sim is not None\n"
    )
    (repo / "test/core/test_legacy.py").write_text(fixed_src, encoding="utf-8")
    payload["parametrized_simulations"] = {}
    baseline_path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    assert _run(repo) == 0, capsys.readouterr().err


def test_ast_guards_eager_assignment_values_and_branch_overflow(
    tmp_path: Path, capsys
) -> None:
    """Stored command bindings keep assignment-time values; >8 and >64 branches stay sound."""
    # Construct a 9-branch and a 70-branch test to verify no silent SUEWS loss.
    nine_branches = "\n".join(
        f"    {'if' if i == 0 else 'elif'} x == {i}:\n        argv = ['echo', '{i}']"
        for i in range(9)
    ) + "\n    else:\n        argv = ['suews', '--help']\n"
    seventy_branches = "\n".join(
        f"    {'if' if i == 0 else 'elif'} x == {i}:\n        argv = ['echo', '{i}']"
        for i in range(70)
    ) + "\n    else:\n        argv = ['git', '--version']\n"

    src = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "def test_eager_captures_suews():\n"
        "    exe = 'suews'\n"
        "    argv = [exe, '--help']\n"
        "    exe = 'echo'\n"
        "    subprocess.run(argv)\n\n"
        "def test_eager_no_false_positive():\n"
        "    exe = 'echo'\n"
        "    argv = [exe, '--help']\n"
        "    exe = 'suews'\n"
        "    subprocess.run(argv)\n\n"
        "def test_tenth_branch_suews_detected(x):\n"
        f"{nine_branches}"
        "    subprocess.run(argv)\n\n"
        "def test_overflow_fails_closed(x):\n"
        f"{seventy_branches}"
        "    subprocess.run(argv)\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_eager.py": src,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "missing-e2e-marker: test/core/test_eager.py::test_eager_captures_suews" in err
    assert "test_eager_no_false_positive" not in err
    assert "missing-e2e-marker: test/core/test_eager.py::test_tenth_branch_suews_detected" in err
    assert "subprocess-analysis-overflow: test/core/test_eager.py::test_overflow_fails_closed" in err


def test_ast_guards_sound_non_sim_provenance_across_scopes_and_control_flow(
    tmp_path: Path, capsys
) -> None:
    """Module reassignment, zero-iteration loops, and failing try blocks cannot exempt .run()."""
    src = (
        "import subprocess as sim\n"
        "import unittest\n"
        "import pytest\n"
        "pytestmark = pytest.mark.physics\n"
        "sim = SUEWSSimulation()\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_module_reassigned_sim(x):\n"
        "    sim.run()\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_zero_iter_loop_cannot_exempt(x, runner):\n"
        "    for _ in []:\n"
        "        runner = unittest.TextTestRunner()\n"
        "    runner.run()\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_try_cannot_exempt_after_except(x, runner):\n"
        "    try:\n"
        "        maybe_fail()\n"
        "        runner = unittest.TextTestRunner()\n"
        "    except RuntimeError:\n"
        "        pass\n"
        "    runner.run()\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_with_target_shadows_import(x):\n"
        "    with open_sim() as unittest:\n"
        "        unittest.run()\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_proven_non_sim_before_loop_stays_exempt(x):\n"
        "    runner = unittest.TextTestRunner()\n"
        "    for _ in [1, 2]:\n"
        "        runner.run()\n"
        "    runner.run()\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_sound_sim.py": src,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "parametrised-simulation: test/core/test_sound_sim.py::test_module_reassigned_sim" in err
    assert "parametrised-simulation: test/core/test_sound_sim.py::test_zero_iter_loop_cannot_exempt" in err
    assert "parametrised-simulation: test/core/test_sound_sim.py::test_try_cannot_exempt_after_except" in err
    assert "parametrised-simulation: test/core/test_sound_sim.py::test_with_target_shadows_import" in err
    assert "test_proven_non_sim_before_loop_stays_exempt" not in err


def test_ast_guards_static_sequence_expansion_and_unknown_cardinality(
    tmp_path: Path, capsys
) -> None:
    """Starred ranges, list(range(11)), and dynamic e2e argvalues cannot bypass the cap."""
    src = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = [pytest.mark.native, pytest.mark.e2e]\n\n"
        "@pytest.mark.parametrize('x', [*range(11)])\n"
        "def test_starred_range(x):\n"
        "    subprocess.run(['suews', str(x)])\n\n"
        "@pytest.mark.parametrize('x', list(range(11)))\n"
        "def test_list_range(x):\n"
        "    subprocess.run(['suews-validate', str(x)])\n\n"
        "@pytest.mark.parametrize('x', get_dynamic_cases())\n"
        "def test_dynamic_cases(x):\n"
        "    subprocess.run(['suews-mcp', str(x)])\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_seq_caps.py": src,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "`suews` has 11 @pytest.mark.e2e subprocess test node(s)" in err
    assert "`suews-validate` has 11 @pytest.mark.e2e subprocess test node(s)" in err
    assert "unresolvable-e2e-parametrize: test/core/test_seq_caps.py::test_dynamic_cases" in err


def test_ast_guards_conditional_defs_inheritance_and_collection_eligibility(
    tmp_path: Path, capsys
) -> None:
    """Conditional test defs, inherited e2e methods, and pytest class eligibility are modelled."""
    subclasses = "\n".join(f"class TestSub{i}(_BaseCli):\n    pass\n" for i in range(11))
    src = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "if True:\n"
        "    def test_conditional_unmarked():\n"
        "        subprocess.run(['suews-run', 'cfg.yml'])\n\n"
        "class _BaseCli:\n"
        "    @pytest.mark.e2e\n"
        "    def test_inherited_cli(self):\n"
        "        subprocess.run(['suews', '--help'])\n\n"
        f"{subclasses}\n"
        "class HelperNotCollected:\n"
        "    def test_ignored_helper(self):\n"
        "        subprocess.run(['suews-schema', 'export'])\n\n"
        "class TestWithInitNotCollected:\n"
        "    def __init__(self):\n"
        "        pass\n"
        "    def test_ignored_init(self):\n"
        "        subprocess.run(['suews-convert', 'in.nml'])\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_discovery.py": src,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "missing-e2e-marker: test/core/test_discovery.py::test_conditional_unmarked" in err
    assert "`suews` has 11 @pytest.mark.e2e subprocess test node(s)" in err
    assert "HelperNotCollected" not in err
    assert "TestWithInitNotCollected" not in err


def test_ast_guards_parametrize_values_for_targets_helper_defaults_and_param_marks(
    tmp_path: Path, capsys
) -> None:
    """Parametrise values, for-loop targets, helper defaults, and per-case e2e marks work."""
    bad_src = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "def _invoke(argv=('suews-engine', 'run')):\n"
        "    return subprocess.run(argv)\n\n"
        "@pytest.mark.parametrize('exe', ['suews'])\n"
        "def test_param_value_unmarked(exe):\n"
        "    subprocess.run([exe, '--help'])\n\n"
        "def test_for_loop_unmarked():\n"
        "    for argv in [['suews-validate', 'cfg.yml']]:\n"
        "        subprocess.run(argv)\n\n"
        "def test_helper_default_unmarked():\n"
        "    _invoke()\n\n"
        "@pytest.mark.parametrize('x', [\n"
        "    pytest.param(1, marks=pytest.mark.e2e),\n"
        "    pytest.param(2),\n"
        "])\n"
        "def test_mixed_param_marks(x):\n"
        "    subprocess.run(['suews-mcp', str(x)])\n"
    )
    good_src = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "def _invoke(argv=('suews-engine', 'run')):\n"
        "    return subprocess.run(argv)\n\n"
        "@pytest.mark.parametrize('x', [pytest.param(1, marks=pytest.mark.e2e)])\n"
        "def test_all_param_cases_marked(x):\n"
        "    subprocess.run(['suews', str(x)])\n\n"
        "@pytest.mark.parametrize('exe', [\n"
        "    pytest.param('suews', marks=pytest.mark.e2e),\n"
        "    pytest.param('echo'),\n"
        "])\n"
        "def test_only_cli_case_needs_e2e(exe):\n"
        "    subprocess.run([exe, '--help'])\n\n"
        "def test_helper_override_non_suews():\n"
        "    _invoke(['echo', 'ok'])\n"
    )
    bad_repo = _make_repo(
        tmp_path / "bad",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_bindings_bad.py": bad_src,
        },
    )
    assert _run(bad_repo) == 1
    err = capsys.readouterr().err
    assert "missing-e2e-marker: test/core/test_bindings_bad.py::test_param_value_unmarked" in err
    assert "missing-e2e-marker: test/core/test_bindings_bad.py::test_for_loop_unmarked" in err
    assert "missing-e2e-marker: test/core/test_bindings_bad.py::test_helper_default_unmarked" in err
    assert "missing-e2e-marker: test/core/test_bindings_bad.py::test_mixed_param_marks" in err

    good_repo = _make_repo(
        tmp_path / "good",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_bindings_good.py": good_src,
        },
    )
    assert _run(good_repo) == 0, capsys.readouterr().err


def test_ast_guards_explicit_baseline_parent_symlink_and_cli_semantics(
    tmp_path: Path, capsys
) -> None:
    """Explicit --ast-baseline checks parent symlinks; resolved values, -c, as_posix, and quotes work."""
    # 1) Explicit --ast-baseline through symlinked scripts/lint must be rejected.
    external = tmp_path / "external"
    external.mkdir()
    (external / "base.json").write_text(
        json.dumps(check_test_markers._empty_ast_guards_baseline()), encoding="utf-8"
    )
    repo_sym = _make_repo(
        tmp_path / "repo_sym",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_ok.py": UNIT_TEST_SOURCE,
        },
    )
    (repo_sym / "scripts").mkdir()
    _create_symlink(repo_sym / "scripts" / "lint", external, target_is_directory=True)
    symlinked_explicit = repo_sym / "scripts" / "lint" / "base.json"
    assert (
        check_test_markers.main([
            "check_test_markers.py",
            str(repo_sym),
            "--ast-baseline",
            str(symlinked_explicit),
        ])
        == 1
    )
    assert "symlink" in capsys.readouterr().err

    # 2) Resolved value overrides naming heuristic; -c stops python -m scan;
    #    Path('suews').as_posix() and quoted shell paths are caught.
    cli_src = (
        "from pathlib import Path\n"
        "import subprocess\n"
        "import sys\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "def test_non_suews_resolved_name_and_dash_c():\n"
        "    engine_bin = 'git'\n"
        "    subprocess.run([engine_bin, '--version'])\n"
        "    subprocess.run([sys.executable, '-c', 'pass', '-m', 'supy'])\n\n"
        "def test_as_posix_unmarked():\n"
        "    subprocess.run([Path('suews').as_posix(), '--help'])\n\n"
        "def test_quoted_shell_path_unmarked():\n"
        "    subprocess.run('\"./dir with spaces/suews-validate\" --help', shell=True)\n"
    )
    repo_cli = _make_repo(
        tmp_path / "repo_cli",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_cli_edge.py": cli_src,
        },
    )
    assert _run(repo_cli) == 1
    err = capsys.readouterr().err
    assert "test_non_suews_resolved_name_and_dash_c" not in err
    assert "missing-e2e-marker: test/core/test_cli_edge.py::test_as_posix_unmarked" in err
    assert "missing-e2e-marker: test/core/test_cli_edge.py::test_quoted_shell_path_unmarked" in err


def test_ast_guards_shallow_ci_checkout_fails_closed_without_base_history(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, capsys
) -> None:
    """A depth-1 PR checkout with GITHUB_BASE_REF never compares baseline against HEAD."""
    git_env = {
        **os.environ,
        "GIT_AUTHOR_NAME": "Test",
        "GIT_AUTHOR_EMAIL": "test@example.com",
        "GIT_COMMITTER_NAME": "Test",
        "GIT_COMMITTER_EMAIL": "test@example.com",
    }
    legacy_src = (
        "import pytest\n"
        "import supy as sp\n"
        "pytestmark = pytest.mark.physics\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_legacy(x):\n"
        "    sim = sp.SUEWSSimulation.from_sample_data()\n"
        "    assert sim is not None\n"
    )
    upstream = _make_repo(
        tmp_path / "upstream",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_legacy.py": legacy_src,
        },
    )
    (upstream / "scripts" / "lint").mkdir(parents=True)
    assert (
        check_test_markers.main(
            ["check_test_markers.py", str(upstream), "--init-ast-baseline"]
        )
        == 0
    )
    capsys.readouterr()

    for cmd in (
        ["git", "-C", str(upstream), "init", "-b", "master"],
        ["git", "-C", str(upstream), "add", "."],
        ["git", "-C", str(upstream), "commit", "-m", "base"],
        ["git", "-C", str(upstream), "checkout", "-b", "pr-bump"],
    ):
        subprocess.run(cmd, env=git_env, check=True, capture_output=True)

    # Commit a manual increase on pr-bump.
    bumped_src = legacy_src.replace("assert sim is not None", "sim.run()")
    (upstream / "test/core/test_legacy.py").write_text(bumped_src, encoding="utf-8")
    bpath = upstream / "scripts" / "lint" / "test_ast_guards_baseline.json"
    payload = json.loads(bpath.read_text(encoding="utf-8"))
    payload["parametrized_simulations"]["test/core/test_legacy.py::test_legacy"] = 2
    bpath.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    subprocess.run(
        ["git", "-C", str(upstream), "commit", "-am", "manual bump"],
        env=git_env,
        check=True,
        capture_output=True,
    )

    # 1) Depth-1 shallow clone of pr-bump with NO reachable origin -> MUST fail closed.
    shallow_offline = tmp_path / "shallow_offline"
    subprocess.run(
        [
            "git",
            "clone",
            "--depth=1",
            "--single-branch",
            "--branch",
            "pr-bump",
            f"file://{upstream}",
            str(shallow_offline),
        ],
        env=git_env,
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "-C", str(shallow_offline), "remote", "remove", "origin"],
        env=git_env,
        check=True,
        capture_output=True,
    )
    monkeypatch.setenv("GITHUB_BASE_REF", "master")
    assert _run(shallow_offline) == 1
    assert "shallow checkout" in capsys.readouterr().err

    # 2) Depth-1 shallow clone WITH reachable origin -> fetches origin/master and catches bump!
    shallow_online = tmp_path / "shallow_online"
    subprocess.run(
        [
            "git",
            "clone",
            "--depth=1",
            "--single-branch",
            "--branch",
            "pr-bump",
            f"file://{upstream}",
            str(shallow_online),
        ],
        env=git_env,
        check=True,
        capture_output=True,
    )
    assert _run(shallow_online) == 1
    assert "increased parametrised-simulation debt" in capsys.readouterr().err

    # 3) Shallow merge_group checkout with GITHUB_BASE_REF unset must ALSO fail closed.
    monkeypatch.delenv("GITHUB_BASE_REF", raising=False)
    monkeypatch.setenv("GITHUB_EVENT_NAME", "merge_group")
    assert _run(shallow_offline) == 1
    assert "shallow checkout" in capsys.readouterr().err


def test_ast_guards_conditional_marks_diamond_mro_try_intermediate_and_helper_overflow(
    tmp_path: Path, capsys
) -> None:
    """Conditional param marks, diamond C3 MRO, intermediate try bindings, and helper overflow."""
    seventy_branches = "\n".join(
        f"    {'if' if i == 0 else 'elif'} x == {i}:\n        argv = ['echo', '{i}']"
        for i in range(70)
    ) + "\n    else:\n        argv = ['git', '--version']\n"

    src = (
        "import subprocess\n"
        "import unittest\n"
        "import pytest\n"
        "pytestmark = pytest.mark.physics\n\n"
        "def _helper_overflow(x):\n"
        f"{seventy_branches}"
        "    return subprocess.run(argv)\n\n"
        "@pytest.mark.parametrize(\n"
        "    'x', [pytest.param(1, marks=pytest.mark.e2e if False else ())]\n"
        ")\n"
        "def test_false_conditional_e2e_mark(x):\n"
        "    subprocess.run(['suews', '--help'])\n\n"
        "@pytest.mark.parametrize(\n"
        "    'x', [pytest.param(1, marks=pytest.mark.e2e if some_flag else ())]\n"
        ")\n"
        "def test_unresolved_conditional_e2e_mark(x):\n"
        "    subprocess.run(['suews-validate', 'cfg.yml'])\n\n"
        "class _Base:\n"
        "    def test_cli(self):\n"
        "        assert True\n\n"
        "class _Left(_Base):\n"
        "    pass\n\n"
        "class _Right(_Base):\n"
        "    def test_cli(self):\n"
        "        subprocess.run(['suews-mcp', '--version'])\n\n"
        "class TestDiamond(_Left, _Right):\n"
        "    pass\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_try_intermediate_sim_caught(x, sim):\n"
        "    runner = unittest.TextTestRunner()\n"
        "    try:\n"
        "        runner = sim\n"
        "        might_raise()\n"
        "        runner = unittest.TextTestRunner()\n"
        "    except RuntimeError:\n"
        "        runner.run()\n\n"
        "def test_calls_overflowing_helper(x):\n"
        "    _helper_overflow(x)\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_r3_edges.py": src,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "missing-e2e-marker: test/core/test_r3_edges.py::test_false_conditional_e2e_mark" in err
    assert "missing-e2e-marker: test/core/test_r3_edges.py::test_unresolved_conditional_e2e_mark" in err
    assert "missing-e2e-marker: test/core/test_r3_edges.py::TestDiamond::test_cli" in err
    assert "parametrised-simulation: test/core/test_r3_edges.py::test_try_intermediate_sim_caught" in err
    assert "subprocess-analysis-overflow: test/core/test_r3_edges.py::test_calls_overflowing_helper" in err


def test_ast_guards_loop_carried_try_intermediate_and_module_eager_bindings(
    tmp_path: Path, capsys
) -> None:
    """Loop-carried bindings, intermediate try bindings, and module eager bindings are caught."""
    src_detect = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "EXE = 'suews'\n"
        "ARGV = [EXE, '--help']\n"
        "EXE = 'git'\n\n"
        "DEF_EXE = 'suews-validate'\n"
        "def _invoke_default(argv=[DEF_EXE, 'cfg.yml']):\n"
        "    return subprocess.run(argv)\n"
        "DEF_EXE = 'git'\n\n"
        "def test_module_eager_captures_suews():\n"
        "    subprocess.run(ARGV)\n\n"
        "def test_module_helper_default_captures_definition_time():\n"
        "    _invoke_default()\n\n"
        "def test_loop_carried_suews():\n"
        "    argv = ['git', '--version']\n"
        "    for _ in range(2):\n"
        "        subprocess.run(argv)\n"
        "        argv = ['suews-mcp', '--help']\n\n"
        "def test_try_intermediate_suews():\n"
        "    argv = ['git', '--version']\n"
        "    try:\n"
        "        argv = ['suews-run', 'cfg.yml']\n"
        "        int('bad')\n"
        "        argv = ['git', '--version']\n"
        "    except ValueError:\n"
        "        subprocess.run(argv)\n\n"
        "def test_non_converging_loop_fails_closed():\n"
        "    argv = ['git']\n"
        "    for _ in range(10):\n"
        "        argv += ['--flag']\n"
        "    subprocess.run(argv)\n"
    )
    src_clean = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "EXE = 'git'\n"
        "ARGV = [EXE, '--version']\n"
        "EXE = 'suews'\n\n"
        "def test_module_eager_no_false_positive():\n"
        "    subprocess.run(ARGV)\n"
    )
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_r4_detect.py": src_detect,
            "test/core/test_r4_clean.py": src_clean,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "missing-e2e-marker: test/core/test_r4_detect.py::test_module_eager_captures_suews" in err
    assert "missing-e2e-marker: test/core/test_r4_detect.py::test_module_helper_default_captures_definition_time" in err
    assert "missing-e2e-marker: test/core/test_r4_detect.py::test_loop_carried_suews" in err
    assert "missing-e2e-marker: test/core/test_r4_detect.py::test_try_intermediate_suews" in err
    assert "subprocess-analysis-overflow: test/core/test_r4_detect.py::test_non_converging_loop_fails_closed" in err
    assert "test_module_eager_no_false_positive" not in err


def test_ast_guards_with_args_helper_returns_match_captures_and_parent_traversal(
    tmp_path: Path, capsys
) -> None:
    """Chained .with_args, assigned helper returns, match captures/guards, and '..' baseline paths."""
    src = (
        "import subprocess\n"
        "import unittest\n"
        "import pytest\n"
        "pytestmark = pytest.mark.physics\n\n"
        "def command():\n"
        "    return ['suews-validate', 'cfg.yml']\n\n"
        "@pytest.mark.e2e\n"
        "@pytest.mark.parametrize.with_args('x', range(11))\n"
        "def test_with_args_cap(x):\n"
        "    subprocess.run(['suews', str(x)])\n\n"
        "def test_helper_return_direct():\n"
        "    subprocess.run(command())\n\n"
        "def test_helper_return_assigned():\n"
        "    argv = command()\n"
        "    subprocess.run(argv)\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_match_capture_invalidates_non_sim(x, sim):\n"
        "    runner = unittest.TextTestRunner()\n"
        "    match sim:\n"
        "        case runner:\n"
        "            runner.run()\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_match_guard_sim_call_caught(x, sim):\n"
        "    runner = unittest.TextTestRunner()\n"
        "    match sim:\n"
        "        case runner if runner.run():\n"
        "            pass\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_match_literal_keeps_non_sim(x):\n"
        "    runner = unittest.TextTestRunner()\n"
        "    match x:\n"
        "        case 1:\n"
        "            runner.run()\n"
    )
    repo = _make_repo(
        tmp_path / "repo_r5",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_r5_edges.py": src,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "`suews` has 11 @pytest.mark.e2e subprocess test node(s)" in err
    assert "missing-e2e-marker: test/core/test_r5_edges.py::test_helper_return_direct" in err
    assert "missing-e2e-marker: test/core/test_r5_edges.py::test_helper_return_assigned" in err
    assert "parametrised-simulation: test/core/test_r5_edges.py::test_match_capture_invalidates_non_sim" in err
    assert "parametrised-simulation: test/core/test_r5_edges.py::test_match_guard_sim_call_caught" in err
    assert "test_match_literal_keeps_non_sim" not in err

    # Parent traversal through a symlink (repo/link/../baseline.json) rejected on read and init.
    outside = tmp_path / "outside"
    (outside / "child").mkdir(parents=True)
    (outside / "baseline.json").write_text(
        json.dumps(check_test_markers._empty_ast_guards_baseline()), encoding="utf-8"
    )
    repo_trav = _make_repo(
        tmp_path / "repo_trav",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_ok.py": UNIT_TEST_SOURCE,
        },
    )
    _create_symlink(repo_trav / "link", outside / "child", target_is_directory=True)
    tricky_path = repo_trav / "link" / ".." / "baseline.json"
    for extra in (["--ast-baseline", str(tricky_path)], ["--init-ast-baseline", "--ast-baseline", str(tricky_path)]):
        assert (
            check_test_markers.main(["check_test_markers.py", str(repo_trav), *extra])
            == 1
        )
        assert "parent traversal" in capsys.readouterr().err


def test_ast_guards_expansion_overflow_tuple_unpacking_and_explicit_test_flag(
    tmp_path: Path, capsys
) -> None:
    """Expansion overflow with param executable, tuple unpacking, and __test__ = True."""
    src = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "@pytest.mark.e2e\n"
        "@pytest.mark.parametrize('exe', ['suews'] * 2049)\n"
        "def test_expansion_overflow_param_exe(exe):\n"
        "    subprocess.run([exe, '--help'])\n\n"
        "def test_tuple_unpacking_suews():\n"
        "    exe, option = 'suews-validate', '--help'\n"
        "    subprocess.run([exe, option])\n\n"
        "def test_tuple_unpacking_harmless():\n"
        "    exe, option = 'git', '--version'\n"
        "    subprocess.run([exe, option])\n\n"
        "class CliChecks:\n"
        "    __test__ = True\n"
        "    def test_direct_flag(self):\n"
        "        subprocess.run(['suews-mcp', '--version'])\n\n"
        "class _ExplicitBase:\n"
        "    __test__ = True\n"
        "    def test_inherited_flag(self):\n"
        "        subprocess.run(['suews-run', 'cfg.yml'])\n\n"
        "class InheritedChecks(_ExplicitBase):\n"
        "    pass\n\n"
        "class ExplicitWithInitNotCollected:\n"
        "    __test__ = True\n"
        "    def __init__(self):\n"
        "        pass\n"
        "    def test_ignored_init(self):\n"
        "        subprocess.run(['suews-schema', 'export'])\n"
    )
    repo = _make_repo(
        tmp_path / "repo_r6",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_r6_edges.py": src,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "subprocess-analysis-overflow: test/core/test_r6_edges.py::test_expansion_overflow_param_exe" in err
    assert "missing-e2e-marker: test/core/test_r6_edges.py::test_tuple_unpacking_suews" in err
    assert "test_tuple_unpacking_harmless" not in err
    assert "missing-e2e-marker: test/core/test_r6_edges.py::CliChecks::test_direct_flag" in err
    assert "missing-e2e-marker: test/core/test_r6_edges.py::InheritedChecks::test_inherited_flag" in err
    assert "ExplicitWithInitNotCollected" not in err

    # Differential check against real pytest --collect-only for __test__ = True rules.
    probe_file = tmp_path / "test_collect_probe.py"
    probe_file.write_text(
        "class CliChecks:\n"
        "    __test__ = True\n"
        "    def test_direct(self):\n"
        "        assert True\n\n"
        "class _ExplicitBase:\n"
        "    __test__ = True\n"
        "    def test_inherited(self):\n"
        "        assert True\n\n"
        "class InheritedChecks(_ExplicitBase):\n"
        "    pass\n\n"
        "class ExplicitWithInit:\n"
        "    __test__ = True\n"
        "    def __init__(self):\n"
        "        pass\n"
        "    def test_skipped(self):\n"
        "        assert True\n",
        encoding="utf-8",
    )
    proc = subprocess.run(
        [sys.executable, "-m", "pytest", "--collect-only", "-q", "-o", "addopts=", str(probe_file)],
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert "CliChecks::test_direct" in proc.stdout
    assert "InheritedChecks::test_inherited" in proc.stdout
    assert "ExplicitWithInit::test_skipped" not in proc.stdout


def test_ast_guards_test_prefix_expression_scopes_and_helper_return_scope(
    tmp_path: Path, capsys
) -> None:
    """Pytest ``test*`` names, comprehension/lambda scopes, and callee-scoped helper returns."""
    src = (
        "import subprocess\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "CMD = 'echo'\n"
        "def command(CMD):\n"
        "    return CMD\n\n"
        "CMD_SUEWS = 'suews'\n"
        "def command_rev(CMD_SUEWS):\n"
        "    return CMD_SUEWS\n\n"
        "DEF_CLI = 'suews-mcp'\n"
        "def build_local(exe=DEF_CLI):\n"
        "    cmd = [exe, '--version']\n"
        "    return cmd\n"
        "DEF_CLI = 'echo'\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def testSimNoUnderscore(x, sim):\n"
        "    sim.run()\n\n"
        "@pytest.mark.e2e\n"
        "@pytest.mark.parametrize('i', range(11))\n"
        "def testCliCap(i):\n"
        "    subprocess.run(['suews-schema', 'export'])\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_comp_shadows_subprocess(x, sim):\n"
        "    _ = [subprocess.run() for subprocess in [sim]]\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_lambda_shadows_subprocess(x, sim):\n"
        "    fn = lambda subprocess: subprocess.run()\n"
        "    fn(sim)\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_comp_subprocess_exempt(x):\n"
        "    _ = [subprocess.run(['git', '--version']) for _ in range(1)]\n\n"
        "def test_comp_binds_suews_cli():\n"
        "    _ = [subprocess.run([cmd]) for cmd in ['suews']]\n\n"
        "def test_lambda_default_binds_suews_cli():\n"
        "    fn = lambda cmd='suews-validate': subprocess.run([cmd])\n"
        "    fn()\n\n"
        "def test_comp_shadows_outer_suews_harmless():\n"
        "    cmd = 'suews'\n"
        "    _ = [subprocess.run([cmd]) for cmd in ['git']]\n\n"
        "def test_lambda_shadows_outer_suews_harmless():\n"
        "    cmd = 'suews'\n"
        "    fn = lambda cmd: subprocess.run([cmd])\n"
        "    fn('git')\n\n"
        "def test_helper_return_binds_argument():\n"
        "    subprocess.run([command('suews')])\n\n"
        "def test_helper_return_shadows_global_harmless():\n"
        "    subprocess.run([command_rev('echo')])\n\n"
        "def test_helper_return_local_and_default():\n"
        "    subprocess.run(build_local())\n"
    )
    repo = _make_repo(
        tmp_path / "repo_r7",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_r7_edges.py": src,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "parametrised-simulation: test/core/test_r7_edges.py::testSimNoUnderscore" in err
    assert "e2e-subprocess-cap: `suews-schema`" in err
    assert "parametrised-simulation: test/core/test_r7_edges.py::test_comp_shadows_subprocess" in err
    assert "parametrised-simulation: test/core/test_r7_edges.py::test_lambda_shadows_subprocess" in err
    assert "test_comp_subprocess_exempt" not in err
    assert "missing-e2e-marker: test/core/test_r7_edges.py::test_comp_binds_suews_cli" in err
    assert "missing-e2e-marker: test/core/test_r7_edges.py::test_lambda_default_binds_suews_cli" in err
    assert "test_comp_shadows_outer_suews_harmless" not in err
    assert "test_lambda_shadows_outer_suews_harmless" not in err
    assert "missing-e2e-marker: test/core/test_r7_edges.py::test_helper_return_binds_argument" in err
    assert "test_helper_return_shadows_global_harmless" not in err
    assert "missing-e2e-marker: test/core/test_r7_edges.py::test_helper_return_local_and_default" in err

    # Differential check confirming pytest collects non-underscore test* functions.
    probe = tmp_path / "test_prefix_probe.py"
    probe.write_text("def testCli():\n    assert True\n", encoding="utf-8")
    proc = subprocess.run(
        [sys.executable, "-m", "pytest", "--collect-only", "-q", "-o", "addopts=", str(probe)],
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert "test_prefix_probe.py::testCli" in proc.stdout


def test_ast_guards_keyword_eager_alt_union_with_suppress_trystar_set_and_big_range(
    tmp_path: Path, capsys
) -> None:
    """Keyword eager bindings, alternative unions, with-suppress, except*, set dedup, big range."""
    src = (
        "import contextlib\n"
        "import subprocess\n"
        "import unittest\n"
        "import pytest\n"
        "pytestmark = pytest.mark.native\n\n"
        "def build_kw(exe='echo'):\n"
        "    return [exe, '--help']\n\n"
        "def engine_cli():\n"
        "    return 'suews-engine'\n\n"
        "def test_kw_eager_captures_suews():\n"
        "    name = 'suews'\n"
        "    argv = build_kw(exe=name)\n"
        "    name = 'echo'\n"
        "    subprocess.run(argv)\n\n"
        "def test_kw_eager_harmless_not_corrupted():\n"
        "    name = 'echo'\n"
        "    argv = build_kw(exe=name)\n"
        "    name = 'suews'\n"
        "    subprocess.run(argv)\n\n"
        "@pytest.mark.e2e\n"
        "@pytest.mark.parametrize('i', range(11))\n"
        "def test_alt_union_does_not_drop_helper(i, flag):\n"
        "    exe = 'suews-validate' if flag else engine_cli()\n"
        "    subprocess.run([exe, '--help'])\n\n"
        "@pytest.mark.parametrize('x', [1, 2])\n"
        "def test_with_suppress_preserves_sim_path(x, sim):\n"
        "    runner = sim\n"
        "    with contextlib.suppress(ValueError):\n"
        "        might_fail()\n"
        "        runner = unittest.TextTestRunner()\n"
        "    runner.run()\n\n"
        "def test_trystar_updates_cmd():\n"
        "    cmd = ['git', '--version']\n"
        "    try:\n"
        "        cmd = ['suews-mcp', '--version']\n"
        "        raise ExceptionGroup('eg', [ValueError('x')])\n"
        "    except* ValueError:\n"
        "        subprocess.run(cmd)\n\n"
        "try:\n"
        "    pass\n"
        "except* RuntimeError:\n"
        "    def test_discovered_in_trystar():\n"
        "        subprocess.run(['suews-convert', '--help'])\n\n"
        "@pytest.mark.e2e\n"
        "@pytest.mark.parametrize('x', {*range(10), False})\n"
        "def test_set_dedup_collapses_zero_and_false(x):\n"
        "    subprocess.run(['suews-run', 'cfg.yml'])\n\n"
        "@pytest.mark.e2e\n"
        "@pytest.mark.parametrize('x', range(10**30))\n"
        "def test_huge_range_no_overflow_error(x):\n"
        "    subprocess.run(['suews-schema', 'export'])\n"
    )
    repo = _make_repo(
        tmp_path / "repo_r8",
        {
            "test/test_surface.py": UNIT_TEST_SOURCE,
            "test/core/test_r8_edges.py": src,
        },
    )
    assert _run(repo) == 1
    err = capsys.readouterr().err
    assert "missing-e2e-marker: test/core/test_r8_edges.py::test_kw_eager_captures_suews" in err
    assert "test_kw_eager_harmless_not_corrupted" not in err
    assert "e2e-subprocess-cap: `suews-engine`" in err
    assert "e2e-subprocess-cap: `suews-validate`" in err
    assert "parametrised-simulation: test/core/test_r8_edges.py::test_with_suppress_preserves_sim_path" in err
    assert "missing-e2e-marker: test/core/test_r8_edges.py::test_trystar_updates_cmd" in err
    assert "missing-e2e-marker: test/core/test_r8_edges.py::test_discovered_in_trystar" in err
    # {*range(10), False} has 10 distinct Python elements (0 == False), so suews-run stays <= 10.
    assert "e2e-subprocess-cap: `suews-run`" not in err
    # range(10**30) must produce unresolvable-e2e-parametrize rather than raising OverflowError.
    assert "unresolvable-e2e-parametrize: test/core/test_r8_edges.py::test_huge_range_no_overflow_error" in err
