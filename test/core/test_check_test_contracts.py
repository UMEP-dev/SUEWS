"""Exercise the contract checker with small synthetic repositories."""

import importlib.util
import json
from pathlib import Path
import sys

import pytest
import yaml

pytestmark = pytest.mark.api

SCRIPT_PATH = (
    Path(__file__).resolve().parents[2] / "scripts/lint/check_test_contracts.py"
)
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "check_test_contracts", SCRIPT_PATH
)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
checker = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = checker
SCRIPT_SPEC.loader.exec_module(checker)


def _write(root, name, text):
    path = root / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


@pytest.fixture
def tree(tmp_path):
    _write(
        tmp_path,
        "test/contracts.yml",
        "contracts:\n"
        "  - id: C-A\n"
        "    title: Synthetic contract\n"
        "    definition: A synthetic operation preserves its input.\n"
        "    stages: [tooling]\n"
        "    implementation: [scripts/example.py]\n"
        "  - id: C-B\n"
        "    title: Another contract\n"
        "    definition: Another operation rejects invalid input.\n"
        "    stages: [surface, config]\n"
        "    implementation: [src/example.py]\n",
    )
    return tmp_path


def test_valid_tree_and_json_report(tree, capsys):
    _write(tree, "test/test_example.py", 'pytestmark = pytest.mark.contract("C-A")\n')
    output = tree / "report.json"

    assert checker.main(["--root", str(tree), "--strict", "--json", str(output)]) == 0
    report = json.loads(output.read_text(encoding="utf-8"))
    assert report["summary"] == {
        "files": 1,
        "tagged": 1,
        "untagged": 0,
        "unknown_ids": 0,
        "errors": 0,
    }
    assert report["contract_file_counts"] == {"C-A": 1, "C-B": 0}
    markdown = capsys.readouterr().out
    assert "| C-A | 1 |" in markdown
    assert "| C-B | 0 |" in markdown


@pytest.mark.parametrize(
    "source",
    [
        'pytestmark = pytest.mark.contract("C-A", "C-B")\n',
        'pytestmark = [pytest.mark.api, pytest.mark.contract("C-A", "C-B")]\n',
        'pytestmark: list = [pytest.mark.contract("C-A"), pytest.mark.contract("C-B")]\n',
        '@pytest.mark.contract("C-A", "C-B")\ndef test_example(): pass\n',
        '@pytest.mark.contract("C-A", "C-B")\nclass TestExample: pass\n',
        '@pytest.mark.contract("C-A", "C-B")\nasync def test_example(): pass\n',
    ],
)
def test_module_function_and_class_markers(tree, source):
    # No pytest import is needed: the scanner must parse without execution.
    _write(tree, "test/nested/test_example.py", source)
    report = checker.check_tree(tree)
    assert report["files"] == {"test/nested/test_example.py": ["C-A", "C-B"]}
    assert report["errors"] == []


def test_repeated_tags_count_each_file_once(tree):
    _write(
        tree,
        "test/test_one.py",
        'pytestmark = pytest.mark.contract("C-A", "C-A")\n'
        '@pytest.mark.contract("C-A")\ndef test_example(): pass\n',
    )
    _write(tree, "test/test_two.py", 'pytestmark = pytest.mark.contract("C-A")\n')
    assert checker.check_tree(tree)["contract_file_counts"] == {"C-A": 2, "C-B": 0}


def test_comments_strings_and_body_calls_are_not_markers(tree):
    _write(
        tree,
        "test/test_example.py",
        '# pytestmark = pytest.mark.contract("C-A")\n'
        "'pytestmark = pytest.mark.contract(\"C-A\")'\n"
        'def test_example():\n    pytest.mark.contract("C-A")\n',
    )
    assert checker.check_tree(tree)["untagged"] == ["test/test_example.py"]


def test_rust_tags_and_scan_boundaries(tree):
    source = "// contract: C-A, C-B\n#[test]\nfn example() {}\n"
    _write(tree, "src/suews_bridge/src/nested/example.rs", source)
    _write(tree, "src/suews_bridge/tests/example.rs", source)
    _write(tree, "src/suews_bridge/src/no_tests.rs", "// contract: C-A\n")
    _write(tree, "src/suews_bridge/tests/nested/mod.rs", source)
    _write(tree, "src/suews_bridge/examples/excluded.rs", source)
    _write(tree, "test/helper.py", 'pytestmark = pytest.mark.contract("C-A")\n')
    report = checker.check_tree(tree)
    assert report["summary"]["files"] == 3
    assert "src/suews_bridge/tests/nested/mod.rs" in report["files"]
    assert report["contract_file_counts"] == {"C-A": 3, "C-B": 3}
    assert report["errors"] == []


@pytest.mark.parametrize(
    "name, source",
    [
        ("test/test_example.py", 'pytestmark = pytest.mark.contract("C-UNKNOWN")\n'),
        ("src/suews_bridge/tests/example.rs", "// contract: C-UNKNOWN\n#[test]\n"),
    ],
)
def test_unknown_ids_report_and_strict_exit(tree, name, source):
    _write(tree, name, source)
    report = checker.check_tree(tree)
    assert report["unknown_ids"] == {name: ["C-UNKNOWN"]}
    assert report["summary"]["tagged"] == 1
    assert checker.main(["--root", str(tree), "--report"]) == 0
    assert checker.main(["--root", str(tree), "--strict"]) == 1


def test_untagged_files_default_report_and_strict_exit(tree):
    _write(tree, "test/test_example.py", "def test_example(): pass\n")
    _write(tree, "src/suews_bridge/src/example.rs", "#[test]\nfn example() {}\n")
    report = checker.check_tree(tree)
    assert report["summary"]["untagged"] == 2
    assert checker.main(["--root", str(tree)]) == 0
    assert checker.main(["--root", str(tree), "--strict"]) == 1


def test_duplicate_registry_id(tree):
    path = tree / "test/contracts.yml"
    path.write_text(
        path.read_text(encoding="utf-8").replace("id: C-B", "id: C-A"),
        encoding="utf-8",
    )
    report = checker.check_tree(tree)
    assert any("duplicate id C-A" in error for error in report["errors"])
    assert checker.main(["--root", str(tree), "--report"]) == 0
    assert checker.main(["--root", str(tree), "--strict"]) == 1


@pytest.mark.parametrize(
    "field, value, diagnostic",
    [
        ("id", "bad-id", "invalid id"),
        ("title", None, "title must be"),
        ("definition", "", "definition must be"),
        ("stages", ["unknown"], "unknown stage"),
        ("stages", "tooling", "stages must be"),
        ("stages", [[]], "stages must be"),
        ("implementation", [], "implementation must be"),
    ],
)
def test_invalid_registry_fields(tree, field, value, diagnostic):
    path = tree / "test/contracts.yml"
    registry = yaml.safe_load(path.read_text(encoding="utf-8"))
    registry["contracts"][0][field] = value
    path.write_text(yaml.safe_dump(registry), encoding="utf-8")
    assert any(diagnostic in error for error in checker.check_tree(tree)["errors"])
    assert checker.main(["--root", str(tree), "--strict"]) == 1


@pytest.mark.parametrize("contents", ["contracts: [", "contracts: []", "[]"])
def test_unreadable_registry_structure_is_reported(tree, contents):
    _write(tree, "test/contracts.yml", contents)
    assert checker.check_tree(tree)["errors"]
    assert checker.main(["--root", str(tree), "--report"]) == 0
    assert checker.main(["--root", str(tree), "--strict"]) == 1


@pytest.mark.parametrize(
    "source",
    [
        "pytestmark = pytest.mark.contract()\n",
        "pytestmark = pytest.mark.contract(variable)\n",
        'pytestmark = pytest.mark.contract(ids="C-A")\n',
        'pytestmark = pytest.mark.contract("bad-id")\n',
        "def broken(\n",
    ],
)
def test_malformed_python_tags_and_syntax_are_reported(tree, source):
    _write(tree, "test/test_example.py", source)
    assert checker.check_tree(tree)["errors"]
    assert checker.main(["--root", str(tree), "--report"]) == 0
    assert checker.main(["--root", str(tree), "--strict"]) == 1


def test_diagnostics_are_ascii(tree, capsys):
    _write(
        tree, "test/test_example.py", 'pytestmark = pytest.mark.contract("C-\u00e9")\n'
    )
    assert checker.main(["--root", str(tree)]) == 0
    out = capsys.readouterr().out
    assert "test/test_example.py: C-\\xe9" in out
    assert out.isascii()


def test_empty_rust_tag_does_not_consume_the_next_line(tree):
    _write(tree, "src/suews_bridge/tests/example.rs", "// contract:\n#[test]\n")
    report = checker.check_tree(tree)
    assert report["unknown_ids"] == {}
    assert report["summary"]["untagged"] == 1
    assert any("empty Rust contract id" in error for error in report["errors"])


def test_missing_registry_is_reported(tmp_path):
    assert checker.check_tree(tmp_path)["errors"]
    assert checker.main(["--root", str(tmp_path), "--report"]) == 0
    assert checker.main(["--root", str(tmp_path), "--strict"]) == 1
