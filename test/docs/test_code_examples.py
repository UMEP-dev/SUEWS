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


def _load_fixture_module(
    file_path: Path, module_name: str, monkeypatch: pytest.MonkeyPatch
) -> ModuleType:
    """Load a file-backed fixture module into ``sys.modules`` for subprocess probing."""
    spec = importlib.util.spec_from_file_location(module_name, file_path)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    monkeypatch.setitem(sys.modules, module_name, mod)
    spec.loader.exec_module(mod)
    return mod


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
    """Native, direct fd 1, mixed subprocess and interactive _ semantics.

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

    Mixed Python print and subprocess stdout preserve chronological order:

    >>> import subprocess, sys
    >>> print("before"); _ = subprocess.run([sys.executable, "-c", "print('after')"], check=True)
    before
    after

    Interactive displayhook preserves builtins._ across statements:

    >>> 2 + 2
    4
    >>> _ + 1
    5

    Interactive stdin is closed (DEVNULL) so input() fails as prerequisite-missing:

    >>> input("prompt: ")
    'user'
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

    assert by_key["supy.fd_stdout_example", "docstring-example:2"]["status"] == "ok"
    assert by_key["supy.fd_stdout_example", "docstring-example:3"]["status"] == "ok"
    assert by_key["supy.fd_stdout_example", "docstring-example:4"]["status"] == "ok"
    stdin_missing = by_key["supy.fd_stdout_example", "docstring-example:5"]
    assert stdin_missing["status"] == "prerequisite-missing"
    assert stdin_missing["detail"]["prerequisite_type"] == "EOFError"

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
    r"""Spawn a long-running child subprocess and hang until timed out.

    >>> from pathlib import Path
    >>> import subprocess, sys
    >>> p = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    >>> _ = Path({timeout_pid_file.as_posix()!r}).write_text(str(p.pid), encoding="utf-8")
    >>> p.wait()
    0
    """


def background_descendant_example():
    r"""Spawn a background child in the runner's process group and finish normally.

    >>> from pathlib import Path
    >>> import subprocess, sys
    >>> p = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    >>> _ = Path({normal_pid_file.as_posix()!r}).write_text(str(p.pid), encoding="utf-8")
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


def test_cli_worked_case_help_examples_and_exclusions(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
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


@root_cli.command(
    help=(
        ">>> print('abc')\\n"
        "a"
        "bc\\n"
    )
)
def concat_help():
    """Verify adjacent literal spans preserve all output lines."""


@root_cli.command(help=_dynamic_help())
def dynamic():
    """Dynamic command help must abstain as unsupported."""
'''
    sha = _init_git_repo(tmp_path, {"src/supy/cmd/fixture_cli.py": source})
    cli_path = tmp_path / "src/supy/cmd/fixture_cli.py"
    # Mutate installed/working-tree help text to confirm revision source is used instead.
    cli_path.write_text(
        source + '\nroot_cli.help = "Installed-only text\\n>>> 1 + 1\\n999\\n"\n',
        encoding="utf-8",
    )
    module = _load_fixture_module(cli_path, "supy.cmd.fixture_cli", monkeypatch)

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

    # Adjacent string literals must not omit the trailing output fragment.
    concat_ok = by_key["suews concat-help", "cli-help-example:1"]
    assert concat_ok["status"] == "ok"
    assert '"bc\\n"' in concat_ok["source"]

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

    for bad_timeout in ("nan", "inf", "-inf", "0", "-1"):
        with pytest.raises(SystemExit) as exc_info:
            checks.main(["--timeout", bad_timeout, "--no-python", "--no-cli"])
        assert exc_info.value.code != 0
        with pytest.raises(ValueError, match="finite positive"):
            checks.build_report(
                revision=checks.Revision(ROOT, "HEAD"), timeout=float(bad_timeout)
            )


def test_cross_statement_child_stdout_and_empty_workdir_isolation(tmp_path: Path):
    """Capture inherited child output across statements and keep workdir/tempdir clean."""
    source = '''\
__all__ = [
    "delayed_child_stdout_ok",
    "delayed_child_stdout_mismatch",
    "empty_workdir_example",
]


def delayed_child_stdout_ok():
    r"""Child spawned in statement 1 prints when released in statement 2.

    >>> import subprocess, sys
    >>> p = subprocess.Popen(
    ...     [sys.executable, "-c", "import sys; sys.stdin.read(); print('delayed-child-output')"],
    ...     stdin=subprocess.PIPE,
    ... )
    >>> _ = p.communicate(b"go")
    delayed-child-output
    """


def delayed_child_stdout_mismatch():
    r"""Omitting expected output from a child released in statement 2 must fail.

    >>> import subprocess, sys
    >>> p = subprocess.Popen(
    ...     [sys.executable, "-c", "import sys; sys.stdin.read(); print('unmatched-child-output')"],
    ...     stdin=subprocess.PIPE,
    ... )
    >>> _ = p.communicate(b"go")
    >>> 1 + 1
    2
    """


def empty_workdir_example():
    """Initial working directory (and isolated tempdir) must contain no capture files.

    >>> import os, tempfile
    >>> sorted(os.listdir("."))
    []
    >>> sorted(os.listdir(tempfile.gettempdir()))
    []
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    module = ModuleType("supy")
    exec(compile(source, "fixture_capture.py", "exec"), module.__dict__)

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(revision=revision, module=module)
    by_obj = {r["object"]: r for r in report["results"]}

    assert by_obj["supy.delayed_child_stdout_ok"]["status"] == "ok"
    mismatch = by_obj["supy.delayed_child_stdout_mismatch"]
    assert mismatch["status"] == "mismatch"
    assert "unmatched-child-output" in mismatch["detail"]["got"]
    assert by_obj["supy.empty_workdir_example"]["status"] == "ok"


def test_orphaned_grandchild_is_terminated_and_reaped(tmp_path: Path):
    """Supervisor reaps orphaned grandchildren on both normal exit and timeout."""
    pid_file = tmp_path / "grandchild.pid"
    child_code = (
        "import pathlib, subprocess, sys; "
        "gc = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)']); "
        f"pathlib.Path({pid_file.as_posix()!r}).write_text(str(gc.pid), encoding='utf-8')"
    )
    source = f'''\
__all__ = ["orphan_grandchild_example"]


def orphan_grandchild_example():
    r"""Spawn a child that spawns a grandchild and exits, leaving an orphan.

    >>> import subprocess, sys
    >>> _ = subprocess.run([sys.executable, "-c", {child_code!r}], check=True)
    >>> 1 + 1
    2
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    module = ModuleType("supy")
    exec(compile(source, "fixture_grandchild.py", "exec"), module.__dict__)

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(revision=revision, module=module, timeout=5.0)
    by_obj = {r["object"]: r for r in report["results"]}

    assert by_obj["supy.orphan_grandchild_example"]["status"] == "ok"
    assert pid_file.is_file()
    gc_pid = int(pid_file.read_text(encoding="utf-8").strip())
    _assert_pid_exited(gc_pid)


def test_cli_unpacked_decorator_kwargs_abstain_without_docstring_fallback(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    """Dynamic ``**kwargs`` on Click decorators report unsupported instead of guessing."""
    source = '''\
import click

COMMAND_OPTIONS = {
    "help": "Overridden help:\\n\\n>>> 1 + 1\\n2",
    "epilog": "Overridden epilog:\\n\\n>>> 2 + 2\\n4",
}


@click.group()
def root_cli():
    """Root group."""


@root_cli.command(**COMMAND_OPTIONS)
def unpacked_dynamic():
    """Callback docstring must NOT be executed when **COMMAND_OPTIONS may override help.

    >>> 1 / 0
    0
    """


@root_cli.command(**{"help": "Literal dict help:\\n\\n>>> 2 + 3\\n5", "epilog": "Literal epilog:\\n\\n>>> 3 + 4\\n7"})
def unpacked_literal():
    """Unused callback docstring.

    >>> 1 / 0
    0
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/cmd/fixture_kwargs_cli.py": source})
    mod = _load_fixture_module(
        tmp_path / "src/supy/cmd/fixture_kwargs_cli.py",
        "supy.cmd.fixture_kwargs_cli",
        monkeypatch,
    )
    root_cli = mod.root_cli

    revision = checks.Revision(tmp_path, sha)
    results = list(checks.cli_example_results(revision, root_cli, prog_name="fixture"))

    dyn_results = [r for r in results if r["object"] == "fixture unpacked-dynamic"]
    assert len(dyn_results) == 1
    assert dyn_results[0]["status"] == "unsupported"
    assert "**kwargs" in dyn_results[0]["detail"]["reason"]

    lit_results = [r for r in results if r["object"] == "fixture unpacked-literal"]
    assert len(lit_results) == 2
    assert all(r["status"] == "ok" for r in lit_results)


def test_runner_does_not_preimport_urllib_error_or_socket(tmp_path: Path):
    """Harness must not pre-import urllib.error/socket while still classifying them."""
    source = '''\
__all__ = [
    "incomplete_urllib_example",
    "clean_sys_modules_example",
    "explicit_urllib_error_example",
    "explicit_socket_gaierror_example",
]


def incomplete_urllib_example():
    """Importing only urllib must not resolve urllib.error from runner pre-imports.

    >>> import urllib
    >>> urllib.error.URLError("missing import")
    URLError('missing import')
    """


def clean_sys_modules_example():
    """Runner-only modules must not be pre-loaded in sys.modules before user code.

    >>> import sys
    >>> any(m in sys.modules for m in ("urllib.error", "socket", "doctest"))
    False
    """


def explicit_urllib_error_example():
    """Explicitly raised urllib.error.URLError still classifies as prerequisite-missing.

    >>> import urllib.error
    >>> raise urllib.error.URLError("offline")
    1
    """


def explicit_socket_gaierror_example():
    """Explicitly raised socket.gaierror still classifies as prerequisite-missing.

    >>> import socket
    >>> raise socket.gaierror("dns lookup failed")
    1
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    module = ModuleType("supy")
    exec(compile(source, "fixture_preimports.py", "exec"), module.__dict__)

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(revision=revision, module=module)
    by_obj = {r["object"]: r for r in report["results"]}

    incomplete = by_obj["supy.incomplete_urllib_example"]
    assert incomplete["status"] == "error"
    assert incomplete["detail"]["exception_type"] == "AttributeError"

    assert by_obj["supy.clean_sys_modules_example"]["status"] == "ok"

    url_err = by_obj["supy.explicit_urllib_error_example"]
    assert url_err["status"] == "prerequisite-missing"
    assert url_err["detail"]["prerequisite_type"] == "URLError"

    gai_err = by_obj["supy.explicit_socket_gaierror_example"]
    assert gai_err["status"] == "prerequisite-missing"
    assert gai_err["detail"]["prerequisite_type"] == "gaierror"


@pytest.mark.skipif(
    os.name == "nt",
    reason="POSIX supervisor abnormal-exit process-group cleanup test",
)
def test_hostile_example_killing_posix_supervisor_terminates_worker_group(
    tmp_path: Path,
):
    """Outer process must kill worker PGID and descendants if supervisor gets SIGKILL."""
    worker_pid_file = tmp_path / "worker.pid"
    child_pid_file = tmp_path / "child.pid"
    source = f'''\
__all__ = ["kill_supervisor_example"]


def kill_supervisor_example():
    r"""Spawn a child, SIGKILL the supervisor parent, then sleep.

    >>> import os, signal, subprocess, sys, time
    >>> from pathlib import Path
    >>> child = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    >>> _ = Path({worker_pid_file.as_posix()!r}).write_text(str(os.getpid()), encoding="utf-8")
    >>> _ = Path({child_pid_file.as_posix()!r}).write_text(str(child.pid), encoding="utf-8")
    >>> os.kill(os.getppid(), signal.SIGKILL)
    >>> time.sleep(60)
    >>> 1
    1
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    module = ModuleType("supy")
    exec(compile(source, "fixture_kill_sup.py", "exec"), module.__dict__)

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(revision=revision, module=module, timeout=5.0)
    finding = report["results"][0]

    assert finding["status"] == "error"
    assert worker_pid_file.is_file()
    assert child_pid_file.is_file()
    _assert_pid_exited(int(worker_pid_file.read_text(encoding="utf-8").strip()))
    _assert_pid_exited(int(child_pid_file.read_text(encoding="utf-8").strip()))


def test_containment_operational_failure_exits_nonzero_and_excludes_unstarted_block(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
):
    """Containment/cleanup operational failures exit non-zero and track execution accurately."""
    source = '''\
__all__ = ["sample_example"]


def sample_example():
    """Simple example.

    >>> 1 + 1
    2
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    module = ModuleType("supy")
    exec(compile(source, "fixture_opfail.py", "exec"), module.__dict__)
    revision = checks.Revision(tmp_path, sha)

    # 1. Real failing runner executable that exits before gate release (started=False).
    if os.name == "nt":
        bad_exe = tmp_path / "fail_runner.cmd"
        bad_exe.write_text("@exit /b 1\r\n", encoding="utf-8")
    else:
        bad_exe = tmp_path / "fail_runner.sh"
        bad_exe.write_text("#!/bin/sh\nexit 1\n", encoding="utf-8")
        bad_exe.chmod(0o755)

    unstarted_report = checks.build_report(
        revision=revision,
        module=module,
        python_executable=str(bad_exe),
    )
    assert unstarted_report["counts"]["error"] == 1
    assert unstarted_report["summary"]["blocks_executed"] == 0
    assert unstarted_report["summary"]["operational_failures"] == 1
    assert len(unstarted_report["operational_failures"]) == 1
    unstarted_finding = unstarted_report["operational_failures"][0]
    assert unstarted_finding["detail"]["operational_failure"] is True
    assert unstarted_finding["detail"]["started"] is False
    assert unstarted_finding["detail"]["executed"] == 0
    assert "operational_failures=1" in checks.render_text_report(unstarted_report)

    # 2. Simulated Windows Job Object assignment failure through main() exits non-zero
    #    while still writing JSON and ASCII text reports.
    def _fake_win_job_failure(*_args, **_kwargs):
        return checks._RunnerOutcome(
            timed_out=False,
            returncode=1,
            containment_error="failed to assign Windows Job Object containment",
            started=False,
        )

    monkeypatch.setattr(checks, "_spawn_isolated_runner", _fake_win_job_failure)
    monkeypatch.setitem(sys.modules, "fixture_opfail_mod", module)
    json_out = tmp_path / "opfail.json"
    text_out = tmp_path / "opfail.txt"
    rc = checks.main([
        "--revision",
        sha,
        "--repo",
        str(tmp_path),
        "--module",
        "fixture_opfail_mod",
        "--no-cli",
        "--output",
        str(json_out),
        "--text-output",
        str(text_out),
    ])
    capsys.readouterr()
    assert rc == checks.OPERATIONAL_FAILURE_EXIT_CODE
    assert rc != 0
    saved = json.loads(json_out.read_text(encoding="utf-8"))
    assert saved["summary"]["blocks_executed"] == 0
    assert saved["summary"]["operational_failures"] == 1
    assert (
        saved["operational_failures"][0]["detail"]["reason"]
        == "failed to assign Windows Job Object containment"
    )
    assert "operational_failures=1" in text_out.read_text(encoding="utf-8")

    # 3. Post-execution cleanup failure counts as executed AND operational failure.
    def _fake_cleanup_failure(*_args, **_kwargs):
        return checks._RunnerOutcome(
            timed_out=False,
            returncode=125,
            containment_error="failed to terminate and reap isolated example process group",
            started=True,
        )

    monkeypatch.setattr(checks, "_spawn_isolated_runner", _fake_cleanup_failure)
    cleanup_report = checks.build_report(revision=revision, module=module)
    assert cleanup_report["summary"]["blocks_executed"] == 1
    assert cleanup_report["summary"]["operational_failures"] == 1
    assert cleanup_report["operational_failures"][0]["detail"]["started"] is True


def test_cli_help_probe_isolates_cwd_enforces_timeout_and_continues(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    """CLI --help probes run in isolated cwds with timeout and process-tree cleanup."""
    pid_file = tmp_path / "cli_help_child.pid"
    caller_cwd = tmp_path / "caller_cwd"
    caller_cwd.mkdir()

    source = f'''\
from pathlib import Path
import subprocess
import sys
import time

import click


class HangingHelpCommand(click.Command):
    def format_help(self, ctx, formatter):
        child = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
        Path({pid_file.as_posix()!r}).write_text(str(child.pid), encoding="utf-8")
        time.sleep(60)
        super().format_help(ctx, formatter)


@click.group()
def root_cli():
    """Root CLI writing a probe file in cwd to verify directory isolation."""


@root_cli.command()
def touch_cwd():
    """Check cwd isolation.

    >>> 2 + 2
    4
    """


class CwdWritingCommand(click.Command):
    def format_help(self, ctx, formatter):
        Path("leaked_from_help.txt").write_text("isolated\\n", encoding="utf-8")
        super().format_help(ctx, formatter)


@root_cli.command(cls=CwdWritingCommand)
def write_cwd():
    """Write a file during --help formatting.

    >>> 3 + 3
    6
    """


@root_cli.command(cls=HangingHelpCommand)
def hang_help():
    """Command whose --help hangs and spawns a child.

    >>> 1 + 1
    2
    """


@root_cli.command()
def after_hang():
    """Subcommand after a timed-out command must still be probed and checked.

    >>> 4 + 5
    9
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/cmd/fixture_probe_cli.py": source})
    module_file = tmp_path / "src/supy/cmd/fixture_probe_cli.py"
    spec = importlib.util.spec_from_file_location(
        "supy.cmd.fixture_probe_cli", module_file
    )
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    monkeypatch.setitem(sys.modules, "supy.cmd.fixture_probe_cli", mod)
    spec.loader.exec_module(mod)

    monkeypatch.chdir(caller_cwd)
    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(
        revision=revision,
        cli_root=mod.root_cli,
        prog_name="fixture",
        timeout=0.8,
    )
    by_obj = {r["object"]: r for r in report["results"]}

    assert not (caller_cwd / "leaked_from_help.txt").exists()
    assert by_obj["fixture write-cwd"]["status"] == "ok"
    assert by_obj["fixture touch-cwd"]["status"] == "ok"

    hung = by_obj["fixture hang-help"]
    assert hung["status"] == "unsupported"
    assert "timed out" in hung["detail"]["reason"]
    assert pid_file.is_file()
    _assert_pid_exited(int(pid_file.read_text(encoding="utf-8").strip()))

    assert by_obj["fixture after-hang"]["status"] == "ok"


def test_main_module_identity_future_flags_and_click_formfeed_truncation(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    """Cover dedicated __main__ identity, __future__ flags, and Click \\f truncation."""
    py_source = '''\
__all__ = ["main_identity_example", "future_annotations_example"]


def main_identity_example():
    """Examples run in a clean dedicated __main__ module.

    >>> import sys
    >>> from dataclasses import dataclass
    >>> __name__ == "__main__" and sys.modules["__main__"].__dict__ is globals()
    True
    >>> "_execute_capturing_fds" in dir(sys.modules["__main__"])
    False
    >>> @dataclass
    ... class Point:
    ...     x: int
    >>> Point(3)
    Point(x=3)
    """


def future_annotations_example():
    """__future__ compiler flags persist across statements in the same block.

    >>> from __future__ import annotations
    >>> def make(x: int) -> Box:
    ...     return Box(x)
    >>> class Box:
    ...     def __init__(self, value):
    ...         self.value = value
    >>> make(7).value
    7
    """
'''
    cli_source = '''\
import click


@click.group()
def root_cli():
    """Root CLI."""


@root_cli.command(
    help="Public help.\\n\\n>>> 1 + 2\\n3\\n\\f\\nHidden internal notes.\\n\\n>>> 1 / 0\\n0\\n"
)
def explicit_help():
    """Unused callback docstring."""


@root_cli.command()
def docstring_help():
    """Public callback help.

    >>> 2 + 2
    4

    \\f
    Hidden internal section not rendered in --help:

    >>> 1 / 0
    0
    """
'''
    sha = _init_git_repo(
        tmp_path,
        {
            "src/supy/__init__.py": py_source,
            "src/supy/cmd/fixture_ff_cli.py": cli_source,
        },
    )
    py_mod = ModuleType("supy")
    exec(compile(py_source, "fixture_minors_py.py", "exec"), py_mod.__dict__)

    cli_mod = _load_fixture_module(
        tmp_path / "src/supy/cmd/fixture_ff_cli.py",
        "supy.cmd.fixture_ff_cli",
        monkeypatch,
    )

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(
        revision=revision,
        module=py_mod,
        cli_root=cli_mod.root_cli,
        prog_name="fixture",
    )
    by_obj = {r["object"]: r for r in report["results"]}

    assert by_obj["supy.main_identity_example"]["status"] == "ok"
    assert by_obj["supy.future_annotations_example"]["status"] == "ok"

    explicit_findings = [
        r for r in report["results"] if r["object"] == "fixture explicit-help"
    ]
    assert len(explicit_findings) == 1
    assert explicit_findings[0]["status"] == "ok"

    doc_findings = [
        r for r in report["results"] if r["object"] == "fixture docstring-help"
    ]
    assert len(doc_findings) == 1
    assert doc_findings[0]["status"] == "ok"


def test_inmemory_cli_root_abstains_without_inprocess_invocation(tmp_path: Path):
    """Non-importable in-memory Click roots must never run --help in-process."""
    marker = tmp_path / "invoked_inprocess.txt"
    source = f'''\
from pathlib import Path
import time

import click


class HostileInMemoryCommand(click.Command):
    def format_help(self, ctx, formatter):
        Path({marker.as_posix()!r}).write_text("ran", encoding="utf-8")
        time.sleep(60)
        super().format_help(ctx, formatter)


@click.group()
def root_cli():
    """Root CLI."""


@root_cli.command(cls=HostileInMemoryCommand)
def hostile():
    """Never invoked in-process.

    >>> 1 + 1
    2
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/cmd/fixture_inmem_cli.py": source})
    mod = ModuleType("supy.cmd.fixture_inmem_cli")
    exec(compile(source, "fixture_inmem_cli.py", "exec"), mod.__dict__)

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(
        revision=revision, cli_root=mod.root_cli, prog_name="fixture", timeout=0.5
    )

    assert not marker.exists()
    assert report["counts"]["unsupported"] == 2
    assert all(
        "cannot be loaded in an isolated subprocess" in r["detail"]["reason"]
        for r in report["results"]
    )


def test_cli_help_probes_isolate_state_tempdir_and_descendants_per_command(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    """Each CLI --help probe gets a fresh interpreter, tempdir and process group."""
    child_pid_file = tmp_path / "probe_cmd1_child.pid"
    source = f'''\
from pathlib import Path
import subprocess
import sys
import tempfile

import click

MUTATED = False


class DirtyCommand(click.Command):
    def format_help(self, ctx, formatter):
        global MUTATED
        MUTATED = True
        (Path(tempfile.gettempdir()) / "leaked_temp.txt").write_text(
            "dirty", encoding="utf-8"
        )
        child = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
        Path({child_pid_file.as_posix()!r}).write_text(
            str(child.pid), encoding="utf-8"
        )
        super().format_help(ctx, formatter)


class CleanCheckCommand(click.Command):
    def format_help(self, ctx, formatter):
        if MUTATED:
            raise click.ClickException("module global leaked across --help probes")
        if (Path(tempfile.gettempdir()) / "leaked_temp.txt").exists():
            raise click.ClickException("tempdir leaked across --help probes")
        super().format_help(ctx, formatter)


@click.group()
def root_cli():
    """Root CLI."""


@root_cli.command(cls=DirtyCommand)
def first():
    """Mutates global state, writes tempdir, and spawns a child.

    >>> 1 + 1
    2
    """


@root_cli.command(cls=CleanCheckCommand)
def second():
    """Verifies clean module globals and tempdir in its own fresh subprocess.

    >>> 2 + 3
    5
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/cmd/fixture_iso_cli.py": source})
    mod = _load_fixture_module(
        tmp_path / "src/supy/cmd/fixture_iso_cli.py",
        "supy.cmd.fixture_iso_cli",
        monkeypatch,
    )

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(
        revision=revision, cli_root=mod.root_cli, prog_name="fixture", timeout=5.0
    )
    by_obj = {r["object"]: r for r in report["results"]}

    assert by_obj["fixture first"]["status"] == "ok"
    assert by_obj["fixture second"]["status"] == "ok"
    assert child_pid_file.is_file()
    _assert_pid_exited(int(child_pid_file.read_text(encoding="utf-8").strip()))


def test_cli_help_probe_catches_shutdown_hang_and_atexit_failure(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    """Help probes require clean process termination, not just a written result file."""
    source = '''\
import atexit
import os
import threading
import time

import click


class ShutdownHangCommand(click.Command):
    def format_help(self, ctx, formatter):
        threading.Thread(target=time.sleep, args=(60,), daemon=False).start()
        super().format_help(ctx, formatter)


class AtexitFailCommand(click.Command):
    def format_help(self, ctx, formatter):
        atexit.register(os._exit, 7)
        super().format_help(ctx, formatter)


@click.group()
def root_cli():
    """Root CLI."""


@root_cli.command(cls=ShutdownHangCommand)
def shutdown_hang():
    """Leaves a non-daemon thread alive past --help completion.

    >>> 1 + 1
    2
    """


@root_cli.command(cls=AtexitFailCommand)
def atexit_fail():
    """Registers an atexit hook that terminates abnormally.

    >>> 1 + 1
    2
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/cmd/fixture_shutdown_cli.py": source})
    mod = _load_fixture_module(
        tmp_path / "src/supy/cmd/fixture_shutdown_cli.py",
        "supy.cmd.fixture_shutdown_cli",
        monkeypatch,
    )

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(
        revision=revision, cli_root=mod.root_cli, prog_name="fixture", timeout=0.8
    )
    by_obj = {r["object"]: r for r in report["results"]}

    hang = by_obj["fixture shutdown-hang"]
    assert hang["status"] == "unsupported"
    assert "timed out" in hang["detail"]["reason"]

    atexit_res = by_obj["fixture atexit-fail"]
    assert atexit_res["status"] == "unsupported"
    assert "exited with code 7" in atexit_res["detail"]["reason"]


@pytest.mark.skipif(
    os.name == "nt",
    reason="POSIX shell script executable used to test post-gate/pre-ack failures",
)
def test_pre_acknowledgement_startup_failure_and_timeout_are_operational(
    tmp_path: Path,
):
    """Failures or timeouts before worker acknowledgement are operational failures."""
    source = '''\
__all__ = ["simple_example"]


def simple_example():
    """Simple example.

    >>> 1 + 1
    2
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    module = ModuleType("supy")
    exec(compile(source, "fixture_ack.py", "exec"), module.__dict__)
    revision = checks.Revision(tmp_path, sha)

    # 1. Worker crashes AFTER supervisor writes gate file, BEFORE writing ack file.
    post_gate_crash = tmp_path / "post_gate_crash.sh"
    post_gate_crash.write_text(
        "#!/bin/sh\n"
        'if [ "$3" = "-c" ]; then\n'
        f'  exec "{sys.executable}" "$@"\n'
        "fi\n"
        "exit 19\n",
        encoding="utf-8",
    )
    post_gate_crash.chmod(0o755)
    crash_report = checks.build_report(
        revision=revision,
        module=module,
        python_executable=str(post_gate_crash),
        timeout=5.0,
    )
    assert crash_report["summary"]["blocks_executed"] == 0
    assert crash_report["summary"]["operational_failures"] == 1
    assert crash_report["operational_failures"][0]["detail"]["started"] is False
    assert (
        "before execution started"
        in crash_report["operational_failures"][0]["detail"]["reason"]
    )

    # 2. Worker hangs BEFORE writing ack file -> operational timeout, not example timeout.
    pre_ack_hang = tmp_path / "pre_ack_hang.sh"
    pre_ack_hang.write_text(
        "#!/bin/sh\n"
        'if [ "$3" = "-c" ]; then\n'
        f'  exec "{sys.executable}" "$@"\n'
        "fi\n"
        "sleep 60\n",
        encoding="utf-8",
    )
    pre_ack_hang.chmod(0o755)
    hang_report = checks.build_report(
        revision=revision,
        module=module,
        python_executable=str(pre_ack_hang),
        timeout=0.5,
    )
    assert hang_report["summary"]["blocks_executed"] == 0
    assert hang_report["summary"]["operational_failures"] == 1
    assert hang_report["operational_failures"][0]["detail"]["started"] is False
    assert (
        "timed out" in hang_report["operational_failures"][0]["detail"]["reason"]
        and "before execution started"
        in hang_report["operational_failures"][0]["detail"]["reason"]
    )


def test_annotated_all_hidden_option_shell_overlap_and_cleanup_stdout(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    """Cover annotated __all__, hidden Click options, doctest/shell overlap, and trailing stdout."""
    py_source = '''\
__all__: list[str] = ["annotated_export", "trailing_sigterm_stdout"]


def annotated_export():
    """Exported via annotated __all__.

    >>> 5 + 5
    10
    """


def trailing_sigterm_stdout():
    r"""Child writes to inherited stdout inside its SIGTERM handler during cleanup.

    >>> import subprocess, sys
    >>> code = (
    ...     "import signal, sys, time; "
    ...     "signal.signal(signal.SIGTERM, lambda *a: (sys.stdout.write('cleanup-leak' + chr(10)), sys.stdout.flush(), sys.exit(0))); "
    ...     "sys.stderr.write('ready' + chr(10)); sys.stderr.flush(); "
    ...     "time.sleep(60)"
    ... )
    >>> p = subprocess.Popen([sys.executable, "-c", code], stderr=subprocess.PIPE)
    >>> p.stderr.readline()
    b'ready\\n'
    >>> 1 + 1
    2
    """
'''
    cli_source = '''\
import click


@click.group()
def root_cli():
    """Root CLI."""


@root_cli.command()
@click.option("--visible", help="Visible option:\\n\\n>>> 2 + 2\\n4")
@click.option("--secret", hidden=True, help="Hidden option:\\n\\n>>> 1 / 0\\n0")
def opt_cmd(visible, secret):
    """Doctest output containing a shell prompt must not be double-counted.

    >>> print("$ suews run config.yml")
    $ suews run config.yml
    """
'''
    sha = _init_git_repo(
        tmp_path,
        {
            "src/supy/__init__.py": py_source,
            "src/supy/cmd/fixture_polish_cli.py": cli_source,
        },
    )
    py_mod = ModuleType("supy")
    exec(compile(py_source, "fixture_polish_py.py", "exec"), py_mod.__dict__)
    cli_mod = _load_fixture_module(
        tmp_path / "src/supy/cmd/fixture_polish_cli.py",
        "supy.cmd.fixture_polish_cli",
        monkeypatch,
    )

    revision = checks.Revision(tmp_path, sha)
    report = checks.build_report(
        revision=revision,
        module=py_mod,
        cli_root=cli_mod.root_cli,
        prog_name="fixture",
        timeout=5.0,
    )
    by_key = {(r["object"], r["check"]): r for r in report["results"]}

    assert by_key["supy.annotated_export", "docstring-example:1"]["status"] == "ok"

    if os.name != "nt":
        trailing = by_key["supy.trailing_sigterm_stdout", "docstring-example:1"]
        assert trailing["status"] == "mismatch"
        assert "cleanup-leak" in trailing["detail"]["got"]

    opt_findings = [r for r in report["results"] if r["object"] == "fixture opt"]
    assert len(opt_findings) == 2
    assert all(r["status"] == "ok" for r in opt_findings)
    assert not any(
        item["object"] == "fixture opt"
        for item in report["excluded_out_of_scope"]["items"]
    )
