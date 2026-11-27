"""Contract tests for the reporting-only executable code-example checker."""

from __future__ import annotations

from collections import Counter
import importlib.util
import json
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


def test_python_worked_case_outcomes_and_isolation(tmp_path: Path):
    """Cover pass, mismatch, missing prerequisites, errors and no-output exclusion."""
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
    """Attempt to open a non-existent file in the isolated empty directory.

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
    """Classify non-prerequisite runtime exceptions as errors.

    Examples
    --------
    >>> 1 / 0
    0
    """
'''
    sha = _init_git_repo(tmp_path, {"src/supy/__init__.py": source})
    module = ModuleType("supy")
    exec(compile(source, "fixture_py.py", "exec"), module.__dict__)

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
    assert (
        by_key["supy.runtime_error_example", "docstring-example:1"]["status"] == "error"
    )
    assert (
        by_key["supy.runtime_error_example", "docstring-example:1"]["detail"][
            "exception_type"
        ]
        == "ZeroDivisionError"
    )

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


def test_python_directives_tracebacks_properties_and_external_members(tmp_path: Path):
    """Cover expected tracebacks, doctest directives, properties and external members."""
    source = '''\
from pathlib import Path
from pydantic import BaseModel

__all__ = [
    "expected_traceback_example",
    "ellipsis_and_skip_example",
    "malformed_doctest_example",
    "PropertyHolder",
    "ExternalModel",
    "Path",
]


def expected_traceback_example():
    """Verify matching and non-matching expected tracebacks.

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
    """


def ellipsis_and_skip_example():
    """Support doctest directives without silently passing SKIP blocks.

    Ellipsis example:

    >>> list(range(5))  # doctest: +ELLIPSIS
    [0, ..., 4]

    Skipped example:

    >>> 1 + 1  # doctest: +SKIP
    99
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
        by_key["supy.ellipsis_and_skip_example", "docstring-example:1"]["status"]
        == "ok"
    )
    skip_unsup = by_key["supy.ellipsis_and_skip_example", "docstring-example:2"]
    assert skip_unsup["status"] == "unsupported"
    assert skip_unsup["detail"]["reason"] == "doctest SKIP directive"

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


def test_cli_worked_case_help_examples_and_exclusions(tmp_path: Path):
    """Cover CLI --help pass, mismatch, missing prerequisite, dynamic help and exclusions."""
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

    Shell example:

        $ suews demo config.yml
    """
)
def demo():
    """Callback docstring ignored because help= overrides it.

    >>> 1 + 1
    999
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

    dynamic_unsup = by_key["suews dynamic", "cli-help-example"]
    assert dynamic_unsup["status"] == "unsupported"
    assert dynamic_unsup["detail"]["reason"] == "dynamic command help"

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

    # 1. Real Python object: supy.show_version
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
    by_check = {f["check"]: f for f in findings}
    assert by_check["docstring-example:1"]["revision"] == revision.sha
    assert by_check["docstring-example:1"]["file"] == "src/supy/_version.py"
    assert ">>> import supy as sp" in by_check["docstring-example:1"]["source"]
    assert by_check["docstring-example:1"]["status"] in {"ok", "mismatch"}

    # Second block omits `import supy as sp` and must fail as prerequisite-missing.
    assert by_check["docstring-example:2"]["status"] == "prerequisite-missing"
    assert by_check["docstring-example:2"]["detail"]["prerequisite_type"] == "NameError"

    # Third block (`sp.show_version(as_json=...)`) has no stated output -> excluded.
    assert excluded_counts["example has no stated output"] == 1
    assert excluded_items[0]["file"] == "src/supy/_version.py"
    assert "as_json" in excluded_items[0]["source"]

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
