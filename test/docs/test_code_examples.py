"""Contract tests for the reporting-only executable code-example checker."""

from __future__ import annotations

from collections import Counter
import ctypes
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
from types import ModuleType

import pytest

import supy
from supy.cmd.suews_cli import cli

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]

ROOT = Path(__file__).resolve().parents[2]
_SPEC = importlib.util.spec_from_file_location(
    "code_example_checks", ROOT / "scripts/lint/check_code_examples.py"
)
checks = importlib.util.module_from_spec(_SPEC)
sys.modules[_SPEC.name] = checks
_SPEC.loader.exec_module(checks)


def _init_git_repo(repo_dir: Path, files: dict[str, str]) -> str:
    """Create a committed temporary Git repository and return its HEAD SHA."""
    for rel_path, content in files.items():
        path_file = repo_dir / rel_path
        path_file.parent.mkdir(parents=True, exist_ok=True)
        path_file.write_text(content, encoding="utf-8")

    for args in (
        ["init", "-q"],
        ["add", "."],
        [
            "-c",
            "user.name=Fixture",
            "-c",
            "user.email=fixture@example.invalid",
            "commit",
            "-qm",
            "fixture",
        ],
    ):
        subprocess.run(
            ["git", "-C", str(repo_dir), *args],
            check=True,
            capture_output=True,
            text=True,
            encoding="utf-8",
        )
    return subprocess.check_output(
        ["git", "-C", str(repo_dir), "rev-parse", "HEAD"],
        text=True,
        encoding="utf-8",
    ).strip()


def test_python_worked_case_outcomes_and_isolation(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    """Cover pass, mismatch, missing prerequisites, errors, CWD isolation and exclusion."""
    sentinel_file = tmp_path / "host_only.txt"
    sentinel_file.write_text(
        "must not be visible to isolated example\n", encoding="utf-8"
    )

    source = '''\
__all__ = [
    "passing_example",
    "mismatch_example",
    "missing_name_example",
    "missing_file_example",
    "missing_import_example",
    "no_output_example",
    "runtime_error_example",
    "cross_block_isolation_example",
]


def passing_example():
    """Demonstrate a self-contained passing example.

    Examples
    --------
    >>> import math
    >>> math.isqrt(16)
    4
    """


def mismatch_example():
    """Demonstrate an output mismatch.

    Examples
    --------
    >>> 2 + 2
    5
    """


def missing_name_example():
    """Omit an import so execution stops as a missing prerequisite.

    Examples
    --------
    >>> missing_symbol(1)
    2
    """


def missing_file_example():
    """Attempt to open a file that exists only in the caller working directory.

    Examples
    --------
    >>> open("host_only.txt", encoding="utf-8").read().strip()
    'must not be visible to isolated example'
    """


def missing_import_example():
    """Import a module that does not exist.

    Examples
    --------
    >>> import nonexistent_suews_dependency_xyz
    >>> 1
    1
    """


def no_output_example():
    """Exclude examples without stated output without running them.

    Examples
    --------
    >>> raise RuntimeError("must never be executed")
    """


def runtime_error_example():
    """Classify non-prerequisite runtime exceptions as errors with tracebacks.

    Examples
    --------
    >>> 1 / 0
    0
    """


def cross_block_isolation_example():
    """Ensure files written by one block are invisible to the next block.

    First block creates a local file:

    >>> from pathlib import Path
    >>> _ = Path("leaked.txt").write_text("secret", encoding="utf-8")
    >>> Path("leaked.txt").read_text(encoding="utf-8")
    'secret'

    Second block must run in a fresh empty directory:

    >>> open("leaked.txt", encoding="utf-8").read()
    'secret'
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    module = ModuleType("supy")
    exec(compile(source, "fixture_py.py", "exec"), module.__dict__)

    # Change caller working directory to where host_only.txt actually exists.
    monkeypatch.chdir(tmp_path)
    assert Path("host_only.txt").is_file()

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(
        revision=revision,
        module=module,
        installed_version="0.0-test",
        installed_commit=sha,
    )
    by_key = {(r["object"], r["check"]): r for r in report["results"]}

    ok = by_key["supy.passing_example", "docstring-example:1"]
    assert ok["status"] == "ok"
    assert ok["revision"] == sha
    assert ok["file"] == "src/supy/__init__.py"
    assert ">>> import math" in ok["source"]
    assert ok["lines"][0] < ok["lines"][1]

    mismatch = by_key["supy.mismatch_example", "docstring-example:1"]
    assert mismatch["status"] == "mismatch"
    assert mismatch["detail"]["want"] == "5\n"
    assert mismatch["detail"]["got"] == "4\n"
    assert "Expected:" in mismatch["detail"]["diff"]

    assert (
        by_key["supy.missing_name_example", "docstring-example:1"]["status"]
        == "prerequisite-missing"
    )
    assert (
        by_key["supy.missing_name_example", "docstring-example:1"]["detail"][
            "prerequisite_type"
        ]
        == "NameError"
    )
    assert (
        by_key["supy.missing_file_example", "docstring-example:1"]["status"]
        == "prerequisite-missing"
    )
    assert (
        by_key["supy.missing_file_example", "docstring-example:1"]["detail"][
            "prerequisite_type"
        ]
        == "FileNotFoundError"
    )
    assert (
        by_key["supy.missing_import_example", "docstring-example:1"]["status"]
        == "prerequisite-missing"
    )
    assert (
        by_key["supy.missing_import_example", "docstring-example:1"]["detail"][
            "prerequisite_type"
        ]
        == "ModuleNotFoundError"
    )

    runtime_err = by_key["supy.runtime_error_example", "docstring-example:1"]
    assert runtime_err["status"] == "error"
    assert runtime_err["detail"]["exception_type"] == "ZeroDivisionError"
    assert "ZeroDivisionError" in runtime_err["detail"]["traceback"]
    assert "NoneType: None" not in runtime_err["detail"]["traceback"]

    # Cross-block filesystem isolation: block 1 writes leaked.txt, block 2 cannot see it.
    assert (
        by_key["supy.cross_block_isolation_example", "docstring-example:1"]["status"]
        == "ok"
    )
    block2 = by_key["supy.cross_block_isolation_example", "docstring-example:2"]
    assert block2["status"] == "prerequisite-missing"
    assert block2["detail"]["prerequisite_type"] == "FileNotFoundError"

    # No-stated-output block must be excluded and never executed.
    assert not any(r["object"] == "supy.no_output_example" for r in report["results"])
    excluded_objs = [
        item["object"]
        for item in report["excluded_out_of_scope"]["items"]
        if item["reason"] == "example has no stated output"
    ]
    assert excluded_objs == ["supy.no_output_example"]
    assert (
        report["excluded_out_of_scope"]["reasons"]["example has no stated output"] == 1
    )


def test_python_directives_tracebacks_fd_capture_and_external_members(tmp_path: Path):
    """Cover doctest exception normalisation, fd 1 capture, properties and external members."""
    source = '''\
from pathlib import Path
from pydantic import BaseModel

__all__ = [
    "expected_traceback_example",
    "ellipsis_and_skip_example",
    "fd_stdout_example",
    "malformed_doctest_example",
    "PropertyHolder",
    "ExternalModel",
    "Path",
]


def expected_traceback_example():
    """Verify standard doctest exception matching, IGNORE_EXCEPTION_DETAIL and SyntaxError.

    Matching traceback:

    >>> int("not-an-int")
    Traceback (most recent call last):
        ...
    ValueError: invalid literal for int() with base 10: 'not-an-int'

    Non-matching traceback:

    >>> int("not-an-int")
    Traceback (most recent call last):
        ...
    TypeError: wrong exception type

    IGNORE_EXCEPTION_DETAIL matches differing message text:

    >>> raise ValueError("actual detail")  # doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
        ...
    ValueError: documented detail

    Expected SyntaxError strips source and caret lines:

    >>> eval("1 +")
    Traceback (most recent call last):
        ...
    SyntaxError: invalid syntax
    """


def ellipsis_and_skip_example():
    """Support doctest directives without silently passing SKIP blocks.

    Ellipsis example:

    >>> list(range(5))  # doctest: +ELLIPSIS
    [0, ..., 4]

    Skipped example (never executed):

    >>> 1 + 1  # doctest: +SKIP
    99

    Partially executed block before SKIP:

    >>> x = 10
    >>> x + 1  # doctest: +SKIP
    99
    """


def fd_stdout_example():
    """Native or direct fd 1 stdout must be captured and compared, never ignored.

    Direct fd 1 write:

    >>> import os
    >>> _ = os.write(1, b"unexpected")
    >>> 1 + 1
    2

    Buffered C stdio output is flushed while capture is active:

    >>> import ctypes, os
    >>> libc = ctypes.CDLL("ucrtbase") if os.name == "nt" else ctypes.CDLL(None)
    >>> _ = libc.printf(b"native-buffered%c", 10)
    native-buffered
    """


def malformed_doctest_example():
    """Report malformed doctest indentation as unsupported.

        >>> 1 + 1
      2
    """


class PropertyHolder:
    """Class with a property getter and setter."""

    @property
    def value(self):
        """Return a constant.

        Examples
        --------
        >>> 3 * 3
        9
        """
        return 9

    @value.setter
    def value(self, new_value):
        self._value = new_value


class ExternalModel(BaseModel):
    """Model inheriting Pydantic methods defined outside supy."""

    x: int = 1
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    module = ModuleType("supy")
    exec(compile(source, "fixture_py_extra.py", "exec"), module.__dict__)

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(revision=revision, module=module)
    by_key = {(r["object"], r["check"]): r for r in report["results"]}

    assert (
        by_key["supy.expected_traceback_example", "docstring-example:1"]["status"]
        == "ok"
    )
    assert (
        by_key["supy.expected_traceback_example", "docstring-example:2"]["status"]
        == "mismatch"
    )
    assert (
        by_key["supy.expected_traceback_example", "docstring-example:3"]["status"]
        == "ok"
    )
    assert (
        by_key["supy.expected_traceback_example", "docstring-example:4"]["status"]
        == "ok"
    )
    assert (
        by_key["supy.ellipsis_and_skip_example", "docstring-example:1"]["status"]
        == "ok"
    )
    skip_unsup = by_key["supy.ellipsis_and_skip_example", "docstring-example:2"]
    assert skip_unsup["status"] == "unsupported"
    assert skip_unsup["detail"]["reason"] == "doctest SKIP directive"
    assert skip_unsup["detail"]["executed"] == 0
    assert skip_unsup["detail"]["partial"] is False

    partial_skip = by_key["supy.ellipsis_and_skip_example", "docstring-example:3"]
    assert partial_skip["status"] == "unsupported"
    assert partial_skip["detail"]["executed"] == 1
    assert partial_skip["detail"]["partial"] is True
    assert report["summary"]["blocks_partially_executed"] == 1
    assert report["summary"]["blocks_executed"] >= 8

    fd_mismatch = by_key["supy.fd_stdout_example", "docstring-example:1"]
    assert fd_mismatch["status"] == "mismatch"
    assert "unexpected" in fd_mismatch["detail"]["got"]

    native_ok = by_key["supy.fd_stdout_example", "docstring-example:2"]
    assert native_ok["status"] == "ok"

    parse_unsup = by_key["supy.malformed_doctest_example", "docstring-example:parse"]
    assert parse_unsup["status"] == "unsupported"
    assert "doctest parse error" in parse_unsup["detail"]["reason"]

    prop_ok = by_key["supy.PropertyHolder.value", "docstring-example:1"]
    assert prop_ok["status"] == "ok"
    assert ">>> 3 * 3" in prop_ok["source"]

    reasons = report["excluded_out_of_scope"]["reasons"]
    assert reasons["object defined outside supy"] == 1
    assert reasons["member defined outside supy"] > 0
    assert not any("model_post_init" in r["object"] for r in report["results"])


def _assert_pid_exited(pid: int) -> None:
    """Assert that a previously spawned child process is no longer alive."""
    if os.name == "nt":
        kernel32 = ctypes.windll.kernel32  # type: ignore[attr-defined]
        kernel32.OpenProcess.argtypes = [ctypes.c_uint32, ctypes.c_int, ctypes.c_uint32]
        kernel32.OpenProcess.restype = ctypes.c_void_p
        kernel32.GetExitCodeProcess.argtypes = [
            ctypes.c_void_p,
            ctypes.POINTER(ctypes.c_uint32),
        ]
        kernel32.GetExitCodeProcess.restype = ctypes.c_int
        kernel32.CloseHandle.argtypes = [ctypes.c_void_p]
        kernel32.CloseHandle.restype = ctypes.c_int
        # PROCESS_QUERY_LIMITED_INFORMATION = 0x1000, STILL_ACTIVE = 259
        handle = kernel32.OpenProcess(0x1000, 0, pid)
        if not handle:
            return
        try:
            code = ctypes.c_uint32(0)
            assert kernel32.GetExitCodeProcess(
                ctypes.c_void_p(handle), ctypes.byref(code)
            )
            assert code.value != 259
        finally:
            kernel32.CloseHandle(ctypes.c_void_p(handle))
    else:
        with pytest.raises(ProcessLookupError):
            os.kill(pid, 0)


def test_timeout_and_normal_exit_terminate_spawned_descendant_processes(tmp_path: Path):
    """Both timeout and normal completion must terminate and reap spawned descendants."""
    timeout_pid_file = tmp_path / "timeout_child.pid"
    normal_pid_file = tmp_path / "normal_child.pid"
    source = f'''\
__all__ = ["hanging_example", "background_descendant_example"]


def hanging_example():
    """Spawn a long-running child subprocess and hang until timed out.

    >>> from pathlib import Path
    >>> import subprocess, sys
    >>> p = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    >>> _ = Path({str(timeout_pid_file)!r}).write_text(str(p.pid), encoding="utf-8")
    >>> p.wait()
    0
    """


def background_descendant_example():
    """Spawn a detached long-lived child and finish normally.

    >>> from pathlib import Path
    >>> import subprocess, sys
    >>> p = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    >>> _ = Path({str(normal_pid_file)!r}).write_text(str(p.pid), encoding="utf-8")
    >>> 1 + 1
    2
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    module = ModuleType("supy")
    exec(compile(source, "fixture_timeout.py", "exec"), module.__dict__)

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(revision=revision, module=module, timeout=0.6)
    by_obj = {r["object"]: r for r in report["results"]}

    hanging = by_obj["supy.hanging_example"]
    assert hanging["status"] == "error"
    assert "timed out" in hanging["detail"]["reason"]
    assert timeout_pid_file.is_file()
    _assert_pid_exited(int(timeout_pid_file.read_text(encoding="utf-8").strip()))

    normal = by_obj["supy.background_descendant_example"]
    assert normal["status"] == "ok"
    assert normal_pid_file.is_file()
    _assert_pid_exited(int(normal_pid_file.read_text(encoding="utf-8").strip()))


def test_cli_worked_case_help_examples_and_exclusions(tmp_path: Path):
    """Cover CLI --help pass, mismatch, missing prerequisite, help=None and dynamic help."""
    source = '''\
import click


def _dynamic_help():
    return "Dynamic help: >>> 1 + 1\\n2"


@click.group()
def root_cli():
    """Root CLI group.

    Passing example:

    >>> import json
    >>> json.dumps({"ok": True})
    '{"ok": true}'

    No-output example:

    >>> raise RuntimeError("must not run")
    """


@root_cli.command(
    help="""Run a subcommand.

    Mismatch example:

    >>> len("suews")
    99

    Missing prerequisite example:

    >>> open("missing_config.yml", encoding="utf-8")
    <_io.TextIOWrapper>

    Shell example without stated output:

        $ suews demo config.yml

    Shell example with stated output:

        $ suews demo --format json
        {"ok": true}
    """
)
def demo():
    """Callback docstring ignored because help= overrides it.

    >>> 1 + 1
    999
    """


@root_cli.command(help=None)
def fallback():
    """Explicit help=None falls back to callback docstring just like Click.

    >>> 2 + 3
    5
    """


@root_cli.command(help=_dynamic_help())
def dynamic():
    """Dynamic command help must abstain as unsupported."""
'''
    sha = _init_git_repo(tmp_path, {"src/supy/cmd/fixture_cli.py": source})
    module = ModuleType("supy.cmd.fixture_cli")
    exec(compile(source, "fixture_cli.py", "exec"), module.__dict__)

    # Mutate installed help text to confirm revision source is used instead.
    module.root_cli.help = "Installed-only text\n>>> 1 + 1\n999\n"

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(
        revision=revision,
        cli_root=module.root_cli,
        installed_version="0.0-test",
        installed_commit=sha,
    )
    by_key = {(r["object"], r["check"]): r for r in report["results"]}

    root_ok = by_key["suews", "cli-help-example:1"]
    assert root_ok["status"] == "ok"
    assert root_ok["file"] == "src/supy/cmd/fixture_cli.py"
    assert root_ok["revision"] == sha
    assert ">>> import json" in root_ok["source"]

    demo_mismatch = by_key["suews demo", "cli-help-example:1"]
    assert demo_mismatch["status"] == "mismatch"
    assert demo_mismatch["detail"]["want"] == "99\n"
    assert demo_mismatch["detail"]["got"] == "5\n"

    demo_missing = by_key["suews demo", "cli-help-example:2"]
    assert demo_missing["status"] == "prerequisite-missing"
    assert demo_missing["detail"]["prerequisite_type"] == "FileNotFoundError"

    # Callback docstring of ``demo`` must not be checked because help= overrides it.
    assert ("suews demo", "cli-help-example:3") not in by_key

    # Explicit help=None must fall back to the callback docstring.
    fallback_ok = by_key["suews fallback", "cli-help-example:1"]
    assert fallback_ok["status"] == "ok"
    assert ">>> 2 + 3" in fallback_ok["source"]

    dynamic_unsup = by_key["suews dynamic", "cli-help-example"]
    assert dynamic_unsup["status"] == "unsupported"
    assert dynamic_unsup["detail"]["reason"] == "dynamic command help"

    shell_stated = by_key["suews demo", "cli-help-example:shell:2"]
    assert shell_stated["status"] == "unsupported"
    assert (
        shell_stated["detail"]["reason"]
        == "cli shell example with stated output is not executed"
    )
    assert '{"ok": true}' in shell_stated["source"]
    assert shell_stated["lines"][0] < shell_stated["lines"][1]

    reasons = report["excluded_out_of_scope"]["reasons"]
    assert reasons["example has no stated output"] == 1
    assert reasons["cli shell example has no stated output"] == 1
    excluded_by_reason = {
        item["reason"]: item for item in report["excluded_out_of_scope"]["items"]
    }
    assert excluded_by_reason["example has no stated output"]["object"] == "suews"
    assert (
        excluded_by_reason["cli shell example has no stated output"]["object"]
        == "suews demo"
    )
    assert (
        "$ suews demo config.yml"
        in excluded_by_reason["cli shell example has no stated output"]["source"]
    )


def test_real_public_python_object_and_cli_worked_cases():
    """Exercise real supy.show_version and real suews CLI against HEAD."""
    revision = checks.Revision(ROOT, "HEAD")

    # 1. Real Python object: supy.show_version (checks discovery & source binding
    #    without pinning any current docstring defect to stay broken).
    source = revision.locate(supy.show_version)
    doc_node = checks._docstring_node(source.node)
    assert doc_node is not None
    text_block = checks.TextBlock(
        source=source,
        text=doc_node.value,
        node_start=doc_node.lineno,
        node_end=doc_node.end_lineno or doc_node.lineno,
        origin="docstring-example",
    )
    excluded_counts: Counter = Counter()
    excluded_items: list[dict] = []
    findings = list(
        checks.evaluate_text_block(
            revision.sha,
            "python",
            "supy.show_version",
            text_block,
            excluded_counts=excluded_counts,
            excluded_items=excluded_items,
        )
    )
    assert len(findings) >= 1
    first = findings[0]
    assert first["revision"] == revision.sha
    assert first["file"] == "src/supy/_version.py"
    assert first["definition"] == "supy._version.show_version"
    assert first["lines"][0] <= first["lines"][1]
    assert ">>>" in first["source"]
    assert first["status"] in {"ok", "mismatch", "prerequisite-missing"}

    # Every evaluated finding and excluded item must bind back to HEAD source.
    for item in [*findings, *excluded_items]:
        assert item["revision"] == revision.sha
        assert item["file"] == "src/supy/_version.py"
        assert item["lines"] is not None

    # 2. Real CLI commands: suews group tree (including suews run and suews schema export)
    cli_report = checks.build_report(
        revision=revision,
        cli_root=cli,
        installed_version=supy.__version__,
        installed_commit=getattr(
            checks.importlib.import_module("supy._version_scm"),
            "__commit_hash__",
            "unknown",
        ),
    )
    assert cli_report["summary"]["objects_scanned"]["cli"] >= 10
    assert cli_report["counts"]["error"] == 0
    assert cli_report["counts"]["unsupported"] == 0
    cli_excluded_objects = {
        item["object"] for item in cli_report["excluded_out_of_scope"]["items"]
    }
    assert "suews run" in cli_excluded_objects
    assert "suews schema export" in cli_excluded_objects
    assert "suews summarise" in cli_excluded_objects
    run_item = next(
        item
        for item in cli_report["excluded_out_of_scope"]["items"]
        if item["object"] == "suews run"
    )
    assert run_item["revision"] == revision.sha
    assert run_item["file"] == "src/supy/cmd/SUEWS.py"
    assert "$ suews run" in run_item["source"]


def test_revision_source_survives_dirty_worktree_and_installed_doc_is_unread(
    tmp_path: Path,
):
    """Dirty worktree edits and installed __doc__ must never alter checked examples."""
    source = '''\
__all__ = ["example"]


def example():
    """Original committed example.

    >>> 6 * 7
    42
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    # Corrupt the working tree after committing.
    (tmp_path / "src/supy/__init__.py").write_text(
        "invalid syntax (((\n", encoding="utf-8"
    )

    class GuardedCallable:
        @property
        def __doc__(self):
            raise AssertionError("installed docstring was read")

        def __call__(self):
            return 42

    guarded = GuardedCallable()
    guarded.__module__ = "supy"
    guarded.__qualname__ = "example"

    module = ModuleType("supy")
    module.example = guarded

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(revision=revision, module=module)
    assert len(report["results"]) == 1
    finding = report["results"][0]
    assert finding["status"] == "ok"
    assert finding["lines"] == [7, 8]
    assert ">>> 6 * 7" in finding["source"]


def test_text_and_json_reports_are_ascii_and_complete(tmp_path: Path):
    """Verify machine-readable JSON and ASCII human-readable text rendering."""
    source = '''\
__all__ = ["ok_fn", "bad_fn"]


def ok_fn():
    """Pass cleanly.

    >>> 1 + 1
    2
    """


def bad_fn():
    """Fail with a mismatch.

    >>> "alpha"
    'beta'
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    module = ModuleType("supy")
    exec(compile(source, "fixture_report.py", "exec"), module.__dict__)

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(
        revision=revision,
        module=module,
        installed_version="2026.test",
        installed_commit=sha[:12],
    )
    assert report["installed_matches_revision"] is True
    assert report["counts"] == {
        "ok": 1,
        "mismatch": 1,
        "prerequisite-missing": 0,
        "error": 0,
        "unsupported": 0,
    }
    assert len(report["failures"]) == 1

    # Machine-readable JSON round-trip.
    serialised = json.dumps(report, ensure_ascii=True)
    assert serialised.isascii()
    parsed = json.loads(serialised)
    assert parsed["revision"] == sha

    # Readable ASCII text report.
    text = checks.render_text_report(report)
    assert text.isascii()
    assert f"Revision: {sha}" in text
    assert "ok=1" in text
    assert "mismatch=1" in text
    assert "[mismatch] supy.bad_fn" in text
    assert "src/supy/__init__.py:" in text


@pytest.mark.parametrize("installed", [None, "", "unknown", "deadbeef"])
def test_unknown_installed_revision_does_not_match(installed: str | None):
    """Missing or mismatched installed commit must never claim a revision match."""
    assert checks.installed_matches_revision("abcdef012345", installed) is False


def test_installed_revision_prefix_matches():
    """Both abbreviated and full commit hashes match the target revision."""
    assert checks.installed_matches_revision("abcdef012345", "abcdef") is True
    assert checks.installed_matches_revision("abcdef012345", "abcdef012345") is True


def test_cli_entrypoint_writes_json_and_text_reports_and_fails_on_bad_revision(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
):
    """Exercise main() argument parsing, report file output and operational failure."""
    json_out = tmp_path / "report.json"
    text_out = tmp_path / "report.txt"
    rc = checks.main([
        "--revision",
        "HEAD",
        "--repo",
        str(ROOT),
        "--no-python",
        "--output",
        str(json_out),
        "--text-output",
        str(text_out),
    ])
    assert rc == 0
    assert json_out.is_file()
    assert text_out.is_file()
    payload = json.loads(json_out.read_text(encoding="utf-8"))
    text = text_out.read_text(encoding="utf-8")
    stdout = capsys.readouterr().out
    assert text == stdout
    assert text.isascii()
    assert payload["summary"]["objects_scanned"]["cli"] >= 10
    assert payload["summary"]["objects_scanned"]["python"] == 0

    with pytest.raises(subprocess.CalledProcessError):
        checks.main([
            "--revision",
            "0000000000000000000000000000000000000000",
            "--repo",
            str(ROOT),
            "--no-python",
            "--no-cli",
        ])
