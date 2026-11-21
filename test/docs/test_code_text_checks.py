"""Contract tests for the reporting-only static code-text checker."""

import ast
from collections import Counter
import importlib.util
from pathlib import Path
import subprocess
import sys
from types import ModuleType

import pytest

import supy
from supy.cmd.suews_cli import cli
import supy.util

pytestmark = pytest.mark.api
ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location(
    "code_text_checks", ROOT / "scripts/lint/check_code_text.py"
)
checks = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = checks
spec.loader.exec_module(checks)


def fixture_function(actual, optional=3, *args, **kwargs):
    """Provide a real introspectable signature for mismatch fixtures."""


def test_parameter_mismatch_both_directions():
    """A missing real argument and invented documented argument both surface."""
    status, detail = checks.parameter_check(
        fixture_function,
        """Fixture.

        Parameters
        ----------
        invented : int
            Not in the signature.
        optional : int
            Optional value.
        *args : tuple
            Positional extras.
        **kwargs : dict
            Keyword extras.
        """,
    )
    assert status == "mismatch"
    assert {e["code"] for e in detail["errors"]} == {"PR01", "PR02"}
    assert "actual" in detail["errors"][0]["message"]
    assert "invented" in detail["errors"][1]["message"]


def test_grouped_and_starred_names():
    """Grouped names and variadic arguments retain their signature meaning."""
    status, _ = checks.parameter_check(
        fixture_function,
        """Fixture.

        Parameters
        ----------
        actual,optional : int
            Values.
        *args : tuple
            Extras.
        **kwargs : dict
            Keywords.
        """,
    )
    assert status == "ok"


def test_bound_receiver():
    """The bound receiver need not be documented."""

    class Example:
        def method(self, value):
            """Accept a value."""

    status, _ = checks.parameter_check(
        Example().method, "Example.\n\nParameters\n----------\nvalue : int\n    Value."
    )
    assert status == "ok"


def test_unresolvable_signature():
    """Failed introspection must never become an empty successful comparison."""
    assert checks.parameter_check(object(), "Empty.")[0] == "unsupported"


def test_installed_docstring_is_never_read():
    """Only source prose may reach validation, even as unused metadata."""

    class Callable:
        @property
        def __doc__(self):
            raise AssertionError("installed docstring was read")

        def __call__(self, value):
            return value

    status, _ = checks.parameter_check(
        Callable(), "Source prose.\n\nParameters\n----------\nvalue : int\n    Value."
    )
    assert status == "ok"


@pytest.mark.parametrize(
    ("declared", "text", "expected"),
    [
        ("3", "Default: 3.", "ok"),
        ("3", "Defaults to 4.", "mismatch"),
        ("'on'", "Validation (default: on)", "ok"),
        ("None", "Defaults to the current schema version.", "needs-reading"),
        ("None", "Default: 3", "needs-reading"),
        ("runtime()", "Default: 3", "needs-reading"),
    ],
)
def test_default_contract(declared, text, expected):
    """Only literal comparisons can produce mismatches."""
    assert (
        checks.default_check(ast.parse(declared, mode="eval").body, text)[0] == expected
    )


def test_choice_list_both_directions():
    """Explicit lists expose omissions and additions in the same finding."""
    status, detail = checks.choice_check(["a", "b"], "Choices: a, c")
    assert status == "mismatch"
    assert detail == {"missing_from_text": ["b"], "extra_in_text": ["c"]}
    assert (
        checks.choice_check(["a"], "Choices: a only when enabled")[0] == "needs-reading"
    )


def test_options_both_directions():
    """Missing and stale documented option names are independent findings."""
    assert checks.option_difference({"--help", "--new"}, {"--help", "--old"}) == {
        "missing_from_docs": ["--new"],
        "extra_in_docs": ["--old"],
    }


def test_overview_does_not_attribute_generic_options_to_root():
    """A statement about most subcommands is not a root command option."""
    mentions = checks.described_options(
        "Run ``suews --help``. Most subcommands accept ``--format json``."
    )
    assert mentions[()] == {"--help": [1]}
    assert mentions[None] == {"--format": [1]}


def test_help_aliases_and_generic_documentation():
    """Aliases share coverage, negative flags remain distinct, and generic help applies."""
    rendered, aliases = checks.rendered_options(
        "Options:\n  -i, --input FILE  Input.\n"
        "  -b, --backup / --no-backup  Backup.\n  -h, --help  Show help.\n"
        "  Explanation mentions --not-an-option."
    )
    mentions = checks.described_options("Use ``suews <subcommand> --help``.")
    assert mentions["*",] == {"--help": [1]}
    assert rendered == {"--input", "--backup", "--no-backup", "--help"}
    documented = {aliases.get(s, s) for s in ("-i", "--backup", "--help")}
    assert checks.option_difference(rendered, documented) == {
        "missing_from_docs": ["--no-backup"],
        "extra_in_docs": [],
    }


def test_real_public_object_and_cli():
    """Bind a passing public utility and real exhaustive CLI choice help to Git."""
    revision = checks.Revision(ROOT, "HEAD")
    source = revision.locate(supy.util.cal_des_dta)
    status, _ = checks.parameter_check(
        supy.util.cal_des_dta, ast.get_docstring(source.node)
    )
    assert status == "ok"
    findings = list(checks.cli_results(revision, cli))
    choice = next(
        f
        for f in findings
        if f["object"] == "suews validate" and f["check"] == "choices:--science-fixes"
    )
    assert choice["status"] == "ok"
    assert choice["detail"] == {"missing_from_text": [], "extra_in_text": []}
    assert choice["revision"] == revision.sha
    assert choice["file"] == "src/supy/cmd/validate_config.py"
    assert "scientific transformations" in choice["source"]
    assert any(f["object"] == "suews schema export" for f in findings)
    conditional = next(f for f in findings if f["check"] == "choices:--source-unit")
    assert conditional["status"] == "needs-reading"
    effective = next(
        f
        for f in findings
        if f["object"] == "suews schema" and f["check"] == "default:--version"
    )
    assert effective["status"] == "needs-reading"


def test_export_scope_includes_private_implementation():
    """Re-exported functions remain public regardless of implementation filename."""
    revision = checks.Revision(ROOT, "HEAD")
    names = {name for name, _, _, _ in checks.public_objects(revision, supy)}
    assert "supy.util.attribute_t2" in names
    assert "supy.SUEWSSimulation" in names
    assert "supy.data_model.core.config.SUEWSConfig.from_yaml" in names


def test_revision_source_survives_worktree_edit(tmp_path):
    """A dirty worktree cannot change the text or lines the checker reads."""

    def git(*args):
        return subprocess.check_output(["git", "-C", str(tmp_path), *args], text=True)

    git("init", "-q")
    path = tmp_path / "src/supy"
    path.mkdir(parents=True)
    file = path / "__init__.py"
    file.write_text('def example(x):\n    """Original."""\n', encoding="utf-8")
    git("add", ".")
    git(
        "-c",
        "user.name=Fixture",
        "-c",
        "user.email=fixture@example.invalid",
        "commit",
        "-qm",
        "fixture",
    )
    sha = git("rev-parse", "HEAD").strip()
    file.write_text("invalid source", encoding="utf-8")
    revision = checks.Revision(tmp_path, sha)
    source = revision.sources["supy.example"][0]
    assert source.binding()["lines"] == [1, 2]
    assert ast.get_docstring(source.node) == "Original."


def test_source_fixture_reports_unsupported_and_literal_default(tmp_path):
    """Unresolved declarations stay unsupported; owned inherited text is checked."""
    source = '''\
__all__ = ["decorated", "overloaded", "Child", "literal_default"]
def identity(func):
    return func
@identity
def decorated(value):
    """Wrapped."""
def overloaded(value):
    """First declaration."""
def overloaded(value, other):
    """Second declaration."""
class Parent:
    def method(self, value):
        """Inherited."""
class Child(Parent):
    """Child."""
def literal_default(value=3):
    """Literal default fixture.

    Parameters
    ----------
    value : int
        Default: 4.
    """
'''
    package = tmp_path / "src/supy"
    package.mkdir(parents=True)
    (package / "__init__.py").write_text(source, encoding="utf-8")
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
            ["git", "-C", str(tmp_path), *args], check=True, capture_output=True
        )
    module = ModuleType("supy")
    exec(compile(source, "fixture.py", "exec"), module.__dict__)
    revision = checks.Revision(tmp_path, "HEAD")
    findings = list(checks.python_results(revision, module))
    by_key = {(f["object"], f["check"]): f for f in findings}
    for name in ("decorated", "overloaded"):
        assert by_key[f"supy.{name}", "parameters"]["status"] == "unsupported"
    assert by_key["supy.Parent.method", "parameters"]["status"] == "mismatch"
    default = by_key["supy.literal_default", "default:value"]
    assert default["status"] == "mismatch"
    assert default["detail"] == {"declared": 3, "stated": 4}
    assert default["revision"] == revision.sha
    assert default["file"] == "src/supy/__init__.py"
    assert "Default: 4." in default["source"]


def test_inherited_member_scope(tmp_path):
    """External members vanish; shared supy definitions are checked exactly once."""
    source = '''\
from pydantic import BaseModel
__all__ = ["Child", "Sibling", "Parent"]
class Parent(BaseModel):
    def owned(self, value):
        """Accept a value.

        Parameters
        ----------
        value : int
            Value to accept.
        """
class Child(Parent):
    """First exposure of inherited members."""
class Sibling(Parent):
    """Second exposure of inherited members."""
'''
    package = tmp_path / "src/supy"
    package.mkdir(parents=True)
    (package / "__init__.py").write_text(source, encoding="utf-8")
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
            ["git", "-C", str(tmp_path), *args], check=True, capture_output=True
        )
    module = ModuleType("supy")
    exec(compile(source, "fixture.py", "exec"), module.__dict__)
    revision = checks.Revision(tmp_path, "HEAD")
    excluded = Counter()
    findings = list(checks.python_results(revision, module, excluded))
    owned = [f for f in findings if f["object"] == "supy.Parent.owned"]
    assert len(owned) == 1
    assert owned[0]["status"] == "ok"
    assert owned[0]["definition"] == "supy.Parent.owned"
    assert "def owned(self, value)" in owned[0]["source"]
    assert not any(f["object"].endswith(".model_dump") for f in findings)
    assert not any(f["detail"].get("reason") == "inherited member" for f in findings)
    external_per_class = sum(
        not name.startswith("_")
        and callable(getattr(module.Parent, name))
        and name != "owned"
        for name in dir(module.Parent)
    )
    assert excluded == {"member defined outside supy": 3 * external_per_class}
    assert external_per_class > 0
