"""Tests for scripts/lint/check_test_markers.py (gh#1300, gh#2349)."""

from __future__ import annotations

import errno
import importlib.util
import os
from pathlib import Path
import subprocess
import sys

import pytest

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]

REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "lint" / "check_test_markers.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location("check_test_markers", SCRIPT_PATH)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
check_test_markers = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = check_test_markers
SCRIPT_SPEC.loader.exec_module(check_test_markers)

API_TEST_SOURCE = (
    "import pytest\n\npytestmark = pytest.mark.api\n\ndef test_ok():\n    assert True\n"
)

PHYSICS_TEST_SOURCE = (
    "import pytest\n\n"
    "pytestmark = [pytest.mark.physics]\n\n"
    "def test_ok():\n"
    "    assert True\n"
)

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
        (API_TEST_SOURCE, True),
        (PHYSICS_TEST_SOURCE, True),
        (
            "import pytest\n"
            'pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]\n',
            True,
        ),
        (
            "import pytest\npytestmark: object = pytest.mark.physics\n",
            True,
        ),
        (UNMARKED_TEST_SOURCE, False),
        (
            "import pytest\npytestmark = pytest.mark.smoke\n",
            False,
        ),
        ("def broken(:\n", False),
    ],
)
def test_nature_marker_detection(
    tmp_path: Path, capsys, source: str, should_pass: bool
) -> None:
    """Nature-marker lint accepts physics/api and rejects unmarked or invalid files."""
    repo = _make_repo(
        tmp_path,
        {
            "test/test_surface.py": API_TEST_SOURCE,
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
