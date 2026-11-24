"""Test debt governance through the strict CLI and isolated malformed ledgers."""

import ast
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import sys

import pytest
import yaml

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]
ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/lint/check_test_debt.py"
SPEC = importlib.util.spec_from_file_location("check_test_debt", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
checker = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(checker)


@pytest.mark.core
@pytest.mark.smoke
def test_tree_has_complete_valid_debt_links():
    """Every live record has exactly one contract and well-formed issue owner."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--strict"],
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr


@pytest.fixture
def tree(tmp_path):
    """Copy only static ledger inputs; no model installation or execution."""
    paths = [
        "test/contracts.yml",
        "test/known_defects.yml",
        checker.SCHEMA,
        checker.MUTATIONS,
        checker.JACOBIAN,
        checker.OBSERVER,
        checker.MANIFEST,
        "scripts/lint/rust_optional_reads_baseline.json",
        "scripts/lint/fortran_literals_baseline.json",
        "scripts/lint/fortran_uninit_baseline.json",
        "scripts/lint/fortran_snan_expected_failures.json",
        "scripts/lint/check_saved_fortran_locals.py",
        "test/data_model/test_legacy_table_roundtrip.py",
        checker.OPTION_SWEEP,
        "test/physics/test_calendar_profiles.py",
        "test/physics/test_parameter_sensitivity.py",
        "test/physics/test_spartacus_ground_budget.py",
        "test/physics/test_continuation_matrix.py",
    ]
    for name in paths:
        target = tmp_path / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / name, target)
    return tmp_path


def _links(tree):
    return yaml.safe_load((tree / checker.LINKS).read_text(encoding="utf-8"))


def _write_links(tree, links):
    (tree / checker.LINKS).write_text(yaml.safe_dump(links), encoding="utf-8")


def test_valid_inventory_counts_weights_and_generated_copies(tree, capsys):
    output = tree / "report.json"
    assert checker.main(["--root", str(tree), "--strict", "--json", str(output)]) == 0
    report = json.loads(output.read_text(encoding="utf-8"))
    assert report["summary"]["records"] == report["summary"]["mapped"]
    records = report["records"]
    counters = json.loads(
        (tree / "scripts/lint/rust_optional_reads_baseline.json").read_text(
            encoding="utf-8"
        )
    )
    assert sum(r["occurrences"] for r in records if r["ledger"] == "R") == sum(
        counters["totals"].values()
    )
    observer_ids = [r["id"] for r in records if r["ledger"] == "O"]
    assert observer_ids.count("suews_timer.iy_st") == 1
    assert not any("heatState%" in key for key in observer_ids)
    assert (
        "| Contract | Defects | Coverage gaps | Accepted exceptions |"
        in capsys.readouterr().out
    )


@pytest.mark.parametrize(
    "fault, diagnostic",
    [
        ("unmapped", "unmapped entry"),
        ("overlap", "overlapping selectors"),
        ("stale", "stale selector"),
        ("contract", "unknown contract id"),
        ("issue", "malformed issue reference"),
        ("owner", "owner_issue disagrees"),
    ],
)
def test_link_failures_reach_strict_exit(tree, fault, diagnostic):
    links = _links(tree)
    mapping = links["mappings"][0]
    if fault == "unmapped":
        links["mappings"].pop(0)
    elif fault == "overlap":
        links["mappings"].append(dict(mapping))
    elif fault == "stale":
        mapping["selector"]["function"] = "nonexistent_function"
    elif fault == "contract":
        mapping["contract"] = "C-UNKNOWN"
    elif fault == "issue":
        mapping["issue"] = "#0"
    else:
        next(m for m in links["mappings"] if m["selector"]["ledger"] == "M")[
            "issue"
        ] = 1
    _write_links(tree, links)
    assert checker.main(["--root", str(tree), "--strict"]) == 1
    assert any(diagnostic in e for e in checker.check_tree(tree)["errors"])
    assert checker.main(["--root", str(tree), "--report"]) == 0


@pytest.mark.parametrize(
    "issue", [True, 0, -1, 1.5, "12", "#01", "https://example.org/12", None]
)
def test_issue_syntax_is_strict(issue):
    records = [{"ledger": "example", "id": "entry"}]
    mappings = [
        {
            "selector": {"ledger": "example"},
            "contract": "C-TOOLING",
            "issue": issue,
            "classification": "defect",
        }
    ]
    _, errors = checker.link_records(records, mappings, {"C-TOOLING"})
    assert any("malformed issue reference" in e for e in errors)


@pytest.mark.parametrize(
    "name, source",
    [
        ("test/test_new.py", "@pytest.mark.xfail(strict=True)\ndef test_new(): pass\n"),
        (
            "src/suews_bridge/tests/new.rs",
            '#[test]\n#[ignore = "pending"]\nfn new() {}\n',
        ),
        (
            "src/suews_bridge/tests/new.rs",
            '#[cfg_attr(feature = "slow", ignore)]\nfn new() {}\n',
        ),
    ],
)
def test_unregistered_suppression_fails(tree, name, source):
    (tree / name).write_text(source, encoding="utf-8")
    assert any(
        "unmapped entry" in e and name in e for e in checker.check_tree(tree)["errors"]
    )


@pytest.mark.parametrize(
    "source, scope",
    [
        ("@pytest.mark.xfail\ndef test_new(): pass\n", "test_new"),
        ("@pytest.mark.xfail(strict=True)\ndef test_new(): pass\n", "test_new"),
        ("pytestmark = pytest.mark.xfail\n", "<module>"),
        ("pytestmark = [pytest.mark.xfail]\n", "<module>"),
        ("pytestmark = (pytest.mark.xfail,)\n", "<module>"),
        ("class TestNew:\n    pytestmark = pytest.mark.xfail\n", "TestNew"),
        ("class TestNew:\n    pytestmark = [pytest.mark.xfail]\n", "TestNew"),
        ("class TestNew:\n    pytestmark = (pytest.mark.xfail,)\n", "TestNew"),
        ("@pytest.mark.xfail\nclass TestNew: pass\n", "TestNew"),
        ("cases = [pytest.param(1, marks=pytest.mark.xfail)]\n", "<module>"),
        ("cases = [pytest.param(1, marks=[pytest.mark.xfail])]\n", "<module>"),
        ("cases = [pytest.param(1, marks=(pytest.mark.xfail,))]\n", "<module>"),
        (
            "cases = [pytest.param(1, marks=pytest.mark.xfail(strict=True))]\n",
            "<module>",
        ),
    ],
)
def test_xfail_forms_are_counted_once_and_fail_unmapped(tree, source, scope):
    name = "test/test_new.py"
    (tree / name).write_text(source, encoding="utf-8")
    records, errors = checker.extract_records(tree)
    assert not errors
    assert [r["id"] for r in records if r["source"] == name] == [f"{name}::{scope}::1"]
    assert checker.main(["--root", str(tree), "--strict"]) == 1
    assert f"X:{name}::{scope}::1: unmapped entry" in checker.check_tree(tree)["errors"]


@pytest.mark.parametrize(
    "mutation",
    [
        'ALLOWED[("example.f95", "x")] = "#123"',
        'ALLOWED.update({("example.f95", "x"): "#123"})',
        'ALLOWED |= {("example.f95", "x"): "#123"}',
    ],
)
def test_saved_local_ledger_mutations_fail(tree, mutation):
    path = tree / "scripts/lint/check_saved_fortran_locals.py"
    path.write_text(
        path.read_text(encoding="utf-8") + f"\n{mutation}\n", encoding="utf-8"
    )
    assert checker.main(["--root", str(tree), "--strict"]) == 1
    assert any(
        "unsupported mutation of constant ALLOWED" in e
        for e in checker.check_tree(tree)["errors"]
    )


@pytest.mark.parametrize(
    "mutation", ['_MLM_COMMON.add("new")', '_MLM_DERIVED |= {"new"}']
)
def test_real_ledger_dependency_mutations_reach_strict_exit(tree, mutation):
    path = tree / "test/data_model/test_legacy_table_roundtrip.py"
    path.write_text(
        path.read_text(encoding="utf-8") + f"\n{mutation}\n", encoding="utf-8"
    )
    assert checker.main(["--root", str(tree), "--strict"]) == 1
    assert any(
        "unsupported mutation of constant _MLM_" in e
        for e in checker.check_tree(tree)["errors"]
    )


def test_immutable_dependency_reads_and_unrelated_mutations_are_allowed(tmp_path):
    (tmp_path / "ledger.py").write_text(
        'DEPENDENCY = {"old"}\nLEDGER = DEPENDENCY | {"new"}\n'
        'OTHER = []\nOTHER.append("unrelated")\n',
        encoding="utf-8",
    )
    assert checker._python_values(tmp_path, "ledger.py", ("LEDGER",)) == {
        "LEDGER": {"old", "new"}
    }


@pytest.mark.parametrize(
    "initial, mutation",
    [
        ("{}", 'DEPENDENCY["new"] = 1'),
        ("{}", 'DEPENDENCY.update({"new": 1})'),
        ("{}", 'DEPENDENCY |= {"new": 1}'),
        ("{}", 'DEPENDENCY.setdefault("new", 1)'),
        ('{"old"}', 'DEPENDENCY.add("new")'),
        ("[]", 'DEPENDENCY.append("new")'),
        ("[]", 'DEPENDENCY.extend(["new"])'),
        ("[]", 'DEPENDENCY += ["new"]'),
        ('{"nested": []}', 'DEPENDENCY["nested"].append("new")'),
    ],
)
@pytest.mark.parametrize("before_ledger", [False, True])
def test_python_ledger_dependency_mutations_fail(
    tmp_path, initial, mutation, before_ledger
):
    source = f"DEPENDENCY = {initial}\n"
    ledger = "INTERMEDIATE = DEPENDENCY\nLEDGER = INTERMEDIATE\n"
    source += f"{mutation}\n{ledger}" if before_ledger else f"{ledger}{mutation}\n"
    (tmp_path / "ledger.py").write_text(source, encoding="utf-8")
    with pytest.raises(ValueError, match="unsupported mutation of constant DEPENDENCY"):
        checker._python_values(tmp_path, "ledger.py", ("LEDGER",))


@pytest.mark.parametrize(
    "constant, replacement",
    [
        ("REVIEWED_MULTI_OWNER_SLOTS", '&["new.slot"]'),
        ("KNOWN_BASELINE_NAN_SLOTS", '&["new.slot"]'),
        ("KNOWN_BASELINE_NAN_SLOTS", "computed_slots()"),
    ],
)
def test_empty_rust_ledgers_and_unsupported_syntax_fail(tree, constant, replacement):
    path = tree / checker.JACOBIAN
    path.write_text(
        path.read_text(encoding="utf-8").replace(
            f"const {constant}: &[&str] = &[];",
            f"const {constant}: &[&str] = {replacement};",
        ),
        encoding="utf-8",
    )
    assert checker.check_tree(tree)["errors"]


def test_new_saved_local_cannot_escape_empty_ledger(tree):
    path = tree / "scripts/lint/check_saved_fortran_locals.py"
    path.write_text(
        path.read_text(encoding="utf-8").replace(
            "ALLOWED: dict[tuple[str, str], str] = {}",
            'ALLOWED: dict[tuple[str, str], str] = {("example.f95", "x"): "#123"}',
        ),
        encoding="utf-8",
    )
    assert any(
        "Z:example.f95::x: unmapped" in e for e in checker.check_tree(tree)["errors"]
    )


def test_generated_observer_drift_fails(tree):
    path = tree / checker.MANIFEST
    path.write_text(
        path.read_text(encoding="utf-8").replace(
            'path: "iy_st"',
            'path: "iy_st_unknown"',
        ),
        encoding="utf-8",
    )
    assert any(
        "generated exclusions disagree" in e for e in checker.check_tree(tree)["errors"]
    )


def test_duplicate_mapping_keys_fail(tree):
    path = tree / checker.LINKS
    path.write_text(path.read_text(encoding="utf-8") + "version: 1\n", encoding="utf-8")
    assert any("duplicate key" in e for e in checker.check_tree(tree)["errors"])


def test_missing_ledger_is_not_an_empty_inventory(tree):
    (tree / checker.MUTATIONS).unlink()
    assert checker.main(["--root", str(tree), "--strict"]) == 1


@pytest.mark.parametrize(
    "name, keys, value",
    [
        ("scripts/lint/rust_optional_reads_baseline.json", (), []),
        ("scripts/lint/rust_optional_reads_baseline.json", ("functions",), []),
        (
            "scripts/lint/rust_optional_reads_baseline.json",
            ("functions", "example"),
            2,
        ),
        ("scripts/lint/rust_optional_reads_baseline.json", ("totals",), []),
        (checker.SCHEMA, ("schema_version",), 2),
        (checker.SCHEMA, ("groups",), []),
        (checker.SCHEMA, ("groups", "derived_run_bounds"), []),
        (checker.SCHEMA, ("extra",), {"fields": []}),
        (checker.SCHEMA, ("validator_warning_swallows",), []),
        (checker.SCHEMA, ("doc_required_when_pending_from_yaml",), []),
        (checker.MUTATIONS, ("entries",), {}),
        (checker.MUTATIONS, ("entries", 0), []),
        (checker.MUTATIONS, ("entries", 0, "anchors"), {}),
        (checker.MUTATIONS, ("entries", 0, "mutations"), []),
        ("test/contracts.yml", (), []),
        ("test/contracts.yml", ("contracts",), {}),
        ("test/contracts.yml", ("contracts", 0), []),
        (checker.LINKS, (), []),
        (checker.LINKS, ("mappings",), {}),
        (checker.LINKS, ("mappings", 0), []),
    ],
)
def test_wrong_container_types_are_reported(tree, capsys, name, keys, value):
    path = tree / name
    payload = checker._load(tree, name)
    if keys:
        parent = payload
        for key in keys[:-1]:
            parent = parent[key]
        parent[keys[-1]] = value
    else:
        payload = value
    # JSON is also valid YAML and preserves the deliberately malformed types.
    path.write_text(json.dumps(payload), encoding="utf-8")
    assert checker.main(["--root", str(tree), "--report"]) == 0
    output = capsys.readouterr().out
    assert "## Errors" in output
    diagnostic = output.split("## Errors", 1)[1]
    assert name in diagnostic
    assert (str(keys[0]) if keys else "<root>") in diagnostic
    assert "must be a " in diagnostic
    assert checker.main(["--root", str(tree), "--strict"]) == 1
    # An invalid JSON extractor must not prevent the later Rust extractor.
    assert any(r["ledger"] == "J1" for r in checker.extract_records(tree)[0])


@pytest.mark.parametrize(
    "name, constant",
    [
        (checker.OBSERVER, "REVIEWED_UNBRIDGED_COMPONENTS"),
        (checker.OBSERVER, "REVIEWED_DRIVER_EXCLUDED_COMPONENTS"),
        ("test/data_model/test_legacy_table_roundtrip.py", "_MLM_ALLOWED"),
        ("scripts/lint/check_saved_fortran_locals.py", "ALLOWED"),
    ],
)
def test_wrong_python_constant_types_are_reported(tree, capsys, name, constant):
    path = tree / name
    source = path.read_text(encoding="utf-8")
    module = ast.parse(source)
    node = next(
        item
        for item in module.body
        if isinstance(item, (ast.Assign, ast.AnnAssign))
        and any(
            isinstance(target, ast.Name) and target.id == constant
            for target in (
                item.targets if isinstance(item, ast.Assign) else [item.target]
            )
        )
    )
    lines = source.splitlines(keepends=True)
    lines[node.lineno - 1 : node.end_lineno] = [f"{constant} = []\n"]
    path.write_text("".join(lines), encoding="utf-8")
    assert checker.main(["--root", str(tree), "--report"]) == 0
    diagnostic = capsys.readouterr().out.split("## Errors", 1)[1]
    assert name in diagnostic
    assert constant in diagnostic
    assert "must be a mapping" in diagnostic
    assert checker.main(["--root", str(tree), "--strict"]) == 1


def test_unrelated_repeated_assignments_are_allowed(tmp_path):
    (tmp_path / "ledger.py").write_text(
        'OTHER = []\nDEPENDENCY = {"old"}\n'
        'INTERMEDIATE = DEPENDENCY\nLEDGER = INTERMEDIATE | {"new"}\n'
        'OTHER: list = ["unrelated"]\n',
        encoding="utf-8",
    )
    assert checker._python_values(tmp_path, "ledger.py", ("LEDGER",)) == {
        "LEDGER": {"old", "new"}
    }


@pytest.mark.parametrize("name", ["LEDGER", "INTERMEDIATE", "DEPENDENCY"])
@pytest.mark.parametrize("annotated", [False, True])
def test_repeated_registered_names_and_dependencies_fail(tmp_path, name, annotated):
    target = f"{name}: set" if annotated else name
    (tmp_path / "ledger.py").write_text(
        'DEPENDENCY = {"old"}\nINTERMEDIATE = DEPENDENCY\n'
        f'LEDGER = INTERMEDIATE\n{target} = {{"new"}}\n',
        encoding="utf-8",
    )
    with pytest.raises(ValueError, match=f"repeated constant {name}"):
        checker._python_values(tmp_path, "ledger.py", ("LEDGER",))


def test_suppression_sites_follow_source_order():
    source = (
        "def test_order():\n"
        "    if condition:\n"
        '        pytest.xfail("first")\n'
        '    pytest.xfail("second"); pytest.xfail("third")\n'
        "    if condition:\n"
        "        marker = pytest.mark.xfail\n"
        '    pytest.xfail("last")\n'
    )
    tree = ast.parse(source)
    parents = {
        child: node for node in ast.walk(tree) for child in ast.iter_child_nodes(node)
    }
    sites = checker._suppression_nodes(tree, parents)
    assert [(node.lineno, node.col_offset) for node in sites] == [
        (3, 8),
        (4, 4),
        (4, 28),
        (6, 17),
        (7, 4),
    ]


@pytest.mark.parametrize(
    "source, scope",
    [
        (
            "import unittest\n\n"
            "class TestNew(unittest.TestCase):\n"
            "    @unittest.expectedFailure\n"
            "    def test_new(self):\n"
            "        self.assertEqual(1, 2)\n",
            "TestNew.test_new",
        ),
        (
            "import unittest as ut\n\n"
            "class TestNew(ut.TestCase):\n"
            "    @ut.expectedFailure\n"
            "    def test_new(self):\n"
            "        self.assertEqual(1, 2)\n",
            "TestNew.test_new",
        ),
        (
            "import unittest.case\n\n"
            "class TestNew(unittest.case.TestCase):\n"
            "    @unittest.case.expectedFailure\n"
            "    def test_new(self):\n"
            "        self.assertEqual(1, 2)\n",
            "TestNew.test_new",
        ),
        (
            "import unittest.case as ucase\n\n"
            "class TestNew(ucase.TestCase):\n"
            "    @ucase.expectedFailure\n"
            "    def test_new(self):\n"
            "        self.assertEqual(1, 2)\n",
            "TestNew.test_new",
        ),
        (
            "from unittest import TestCase, case\n\n"
            "class TestNew(TestCase):\n"
            "    @case.expectedFailure\n"
            "    def test_new(self):\n"
            "        self.assertEqual(1, 2)\n",
            "TestNew.test_new",
        ),
        (
            "from unittest import TestCase, case as ucase\n\n"
            "class TestNew(TestCase):\n"
            "    @ucase.expectedFailure\n"
            "    def test_new(self):\n"
            "        self.assertEqual(1, 2)\n",
            "TestNew.test_new",
        ),
        (
            "from unittest import TestCase, expectedFailure\n\n"
            "class TestNew(TestCase):\n"
            "    @expectedFailure\n"
            "    def test_new(self):\n"
            "        self.assertEqual(1, 2)\n",
            "TestNew.test_new",
        ),
        (
            "from unittest import TestCase, expectedFailure as ef\n\n"
            "class TestNew(TestCase):\n"
            "    @ef\n"
            "    def test_new(self):\n"
            "        self.assertEqual(1, 2)\n",
            "TestNew.test_new",
        ),
        (
            "from unittest.case import TestCase, expectedFailure\n\n"
            "class TestNew(TestCase):\n"
            "    @expectedFailure\n"
            "    def test_new(self):\n"
            "        self.assertEqual(1, 2)\n",
            "TestNew.test_new",
        ),
        (
            "from unittest.case import TestCase, expectedFailure as ef\n\n"
            "class TestNew(TestCase):\n"
            "    @ef\n"
            "    def test_new(self):\n"
            "        self.assertEqual(1, 2)\n",
            "TestNew.test_new",
        ),
        (
            "from unittest import *\n\n"
            "class TestNew(TestCase):\n"
            "    @expectedFailure\n"
            "    def test_new(self):\n"
            "        self.assertEqual(1, 2)\n",
            "TestNew.test_new",
        ),
        (
            "import unittest\n\n"
            "@unittest.expectedFailure\n"
            "class TestNew(unittest.TestCase):\n"
            "    pass\n",
            "TestNew",
        ),
        (
            "import unittest\n\n"
            "def _case(self): self.assertEqual(1, 2)\n"
            "wrapped = unittest.expectedFailure(_case)\n",
            "<module>",
        ),
        (
            "from unittest import expectedFailure\n\n"
            "def _case(self): self.assertEqual(1, 2)\n"
            "wrapped = expectedFailure(_case)\n",
            "<module>",
        ),
    ],
)
def test_unittest_expected_failure_forms_are_counted_once_and_fail_unmapped(
    tree, source, scope
):
    name = "test/test_new.py"
    (tree / name).write_text(source, encoding="utf-8")
    records, errors = checker.extract_records(tree)
    assert not errors
    assert [r["id"] for r in records if r["source"] == name] == [f"{name}::{scope}::1"]
    assert checker.main(["--root", str(tree), "--strict"]) == 1
    assert f"X:{name}::{scope}::1: unmapped entry" in checker.check_tree(tree)["errors"]


@pytest.mark.parametrize(
    "source",
    [
        "import unittest\n\nclass TestOk(unittest.TestCase):\n    def test_ok(self):\n        self.assertEqual(1, 1)\n",
        "from unittest import expectedFailure\n\ndef test_ok():\n    assert 1 == 1\n",
        "def expectedFailure(fn): return fn\n\n@expectedFailure\ndef test_ok():\n    assert 1 == 1\n",
        "from custom_module import expectedFailure\n\n@expectedFailure\ndef test_ok():\n    assert 1 == 1\n",
        "def helper():\n    from unittest import expectedFailure as ef\n    return ef\n\ndef test_ok(ef):\n    assert ef(1)\n",
        "def test_ok(unittest):\n    assert unittest.expectedFailure(1)\n",
        "import unittest\n\ndef test_ok(unittest):\n    assert unittest.expectedFailure(1)\n",
        "from unittest import expectedFailure as ef\n\ndef test_ok():\n    ef = lambda x: x\n    assert ef(1)\n",
        "from unittest import expectedFailure\n\ndef test_ok():\n    assert callable(expectedFailure)\n",
        "from __future__ import annotations\nimport unittest\n\ndef helper(x: unittest.expectedFailure(1)):\n    pass\n",
        "import unittest\n\ndef test_ok():\n    unused: unittest.expectedFailure(1)\n    assert True\n",
        "def mark(fn): return fn\n\ndef outer():\n    from unittest import expectedFailure as mark\n    def inner():\n        global mark\n        return mark(lambda: None)\n    return inner\n",
        "from unittest import expectedFailure as ef\nwrappers = [lambda fn: ef(fn) for ef in [lambda fn: fn]]\n",
        "ef = lambda fn: fn\nclass C:\n    from unittest import expectedFailure as ef\n    cases = [ef(fn) for fn in [lambda: None]]\n",
        "from unittest import expectedFailure as ef\nwrap = lambda fn: ((ef := lambda x: x), ef(fn))[1]\n",
    ],
)
def test_unrelated_unittest_and_expected_failure_names_are_not_flagged(tree, source):
    name = "test/test_unrelated.py"
    (tree / name).write_text(source, encoding="utf-8")
    records, errors = checker.extract_records(tree)
    assert not errors
    assert [r for r in records if r["source"] == name] == []


def test_unittest_expected_failure_assignment_aliases_count_each_application(tree):
    name = "test/test_alias_apps.py"
    (tree / name).write_text(
        "import unittest\n"
        "ut = unittest\n"
        "uc = ut.case\n"
        "ef = uc.expectedFailure\n\n"
        "class TestAlias(unittest.TestCase):\n"
        "    @ef\n"
        "    def test_one(self):\n"
        "        self.assertEqual(1, 2)\n\n"
        "    @ef\n"
        "    def test_two(self):\n"
        "        self.assertEqual(1, 2)\n\n"
        "def make_local():\n"
        "    from unittest import expectedFailure as local_ef\n"
        "    @local_ef\n"
        "    def test_inner():\n"
        "        pass\n"
        "    return test_inner\n",
        encoding="utf-8",
    )
    records, errors = checker.extract_records(tree)
    assert not errors
    assert [r["id"] for r in records if r["source"] == name] == [
        f"{name}::TestAlias.test_one::1",
        f"{name}::TestAlias.test_two::1",
        f"{name}::make_local.test_inner::1",
    ]


@pytest.mark.parametrize(
    "source, expected_ids",
    [
        (
            "if True:\n"
            "    from unittest import expectedFailure as ef\n"
            "    def make():\n"
            "        @ef\n"
            "        def test_case():\n"
            "            pass\n"
            "        return test_case\n"
            "    test_case = make()\n",
            ["test/test_branch.py::make.test_case::1"],
        ),
        (
            "def make():\n"
            "    @ef\n"
            "    def test_case():\n"
            "        pass\n"
            "    return test_case\n\n"
            "from unittest import expectedFailure as ef\n",
            ["test/test_branch.py::make.test_case::1"],
        ),
        (
            "import unittest\n"
            "if False:\n"
            "    marker = unittest\n"
            "else:\n"
            "    marker = unittest.expectedFailure\n"
            "@marker\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "import unittest\n"
            "def wrap(fn):\n"
            "    return ef(fn)\n"
            "ef = unittest.expectedFailure\n"
            "test_case = wrap(lambda: None)\n"
            "ef = None\n",
            ["test/test_branch.py::wrap::1"],
        ),
        (
            "import unittest\n"
            "marker = lambda fn: fn\n"
            "test_case = lambda: None\n"
            "for _ in range(2):\n"
            "    test_case = marker(test_case)\n"
            "    marker = unittest.expectedFailure\n",
            ["test/test_branch.py::<module>::1"],
        ),
        (
            "try:\n"
            "    from unittest import expectedFailure as ef\n"
            "    raise RuntimeError\n"
            "except RuntimeError:\n"
            "    @ef\n"
            "    def test_case():\n"
            "        pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "wrap = lambda fn: ef(fn)\n"
            "from unittest import expectedFailure as ef\n"
            "def test_case():\n"
            "    pass\n"
            "wrap(test_case)\n",
            ["test/test_branch.py::<module>::1"],
        ),
        (
            "import unittest\n"
            "(ef := unittest.expectedFailure)\n"
            "@ef\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "import unittest\n"
            "[(ef := unittest.expectedFailure) for _ in [0]]\n"
            "@ef\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "wrap = lambda fn, mark=ef: mark(fn)\n"
            "def wrap_def(fn, mark=ef):\n"
            "    return mark(fn)\n",
            [
                "test/test_branch.py::<module>::1",
                "test/test_branch.py::wrap_def::1",
            ],
        ),
        (
            "from __future__ import annotations\n"
            "import unittest\n"
            "def test_case():\n"
            "    pass\n"
            "unittest.expectedFailure(test_case).flag: int\n",
            ["test/test_branch.py::<module>::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "[(ef := None) for _ in []]\n"
            "@ef\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "def test_case():\n"
            "    pass\n"
            "gen = (ef(fn) for fn in [test_case])\n"
            "from unittest import expectedFailure as ef\n",
            ["test/test_branch.py::<module>::1"],
        ),
        (
            "import unittest\n"
            "class TestCase(unittest.TestCase):\n"
            "    try:\n"
            "        from unittest import expectedFailure as ef\n"
            "        raise RuntimeError\n"
            "    except RuntimeError:\n"
            "        @ef\n"
            "        def test_case(self):\n"
            "            self.fail()\n",
            ["test/test_branch.py::TestCase.test_case::1"],
        ),
        (
            "import unittest\n"
            "list((ef := unittest.expectedFailure) for _ in [0])\n"
            "@ef\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "False and (ef := None)\n"
            "@ef\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "x = 1 if True else (ef := None)\n"
            "@ef\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "import unittest\n"
            "identity = lambda fn: fn\n"
            "test_case = lambda: None\n"
            "try:\n"
            "    marker = unittest.expectedFailure\n"
            "    risky()\n"
            "    marker = identity\n"
            "finally:\n"
            "    marker(test_case)\n",
            ["test/test_branch.py::<module>::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "candidate = lambda: None\n"
            "class C:\n"
            "    ef = lambda fn: fn\n"
            "    test_case = [ef(fn) for fn in [candidate]][0]\n",
            ["test/test_branch.py::C::1"],
        ),
        (
            "import unittest\n"
            "test_case = lambda: None\n"
            "ef: ef(test_case) = unittest.expectedFailure\n",
            ["test/test_branch.py::<module>::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "class Outer:\n"
            "    ef = lambda fn: fn\n"
            "    class Inner:\n"
            "        @ef\n"
            "        def test_case(self):\n"
            "            pass\n",
            ["test/test_branch.py::Outer.Inner.test_case::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "def wrap(fn, mark=ef, unused=(ef := None)):\n"
            "    return mark(fn)\n"
            "def test_case():\n"
            "    pass\n"
            "wrap(test_case)\n",
            ["test/test_branch.py::wrap::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "0 > 1 > (ef := None)\n"
            "@ef\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "assert True, (ef := None)\n"
            "@ef\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "import unittest\n"
            "marker = lambda fn: fn\n"
            "def test_case():\n"
            "    pass\n"
            "[(marker(test_case), (marker := unittest.expectedFailure)) for _ in range(2)]\n",
            ["test/test_branch.py::<module>::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "mark, unused = ef, (ef := None)\n"
            "@mark\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "import unittest\n"
            "marker = lambda fn: fn\n"
            "for stop in [True]:\n"
            "    marker = unittest.expectedFailure\n"
            "    if stop:\n"
            "        break\n"
            "    marker = lambda fn: fn\n"
            "@marker\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "gen = ((ef := marker) for _ in [0])\n"
            "from unittest import expectedFailure as marker\n"
            "list(gen)\n"
            "@ef\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "ef = lambda fn: fn\n"
            "gen = ((ef := marker) for _ in [0])\n"
            "from unittest import expectedFailure as marker\n"
            "list(gen)\n"
            "@ef\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "import unittest\n"
            "def test_one():\n"
            "    pass\n"
            "def test_two():\n"
            "    pass\n"
            "(ef := unittest.expectedFailure)(test_one)\n"
            "ef(test_two)\n",
            [
                "test/test_branch.py::<module>::1",
                "test/test_branch.py::<module>::2",
            ],
        ),
        (
            "import unittest\n"
            "@(ef := unittest.expectedFailure)\n"
            "def test_one():\n"
            "    pass\n"
            "@ef\n"
            "def test_two():\n"
            "    pass\n",
            [
                "test/test_branch.py::test_one::1",
                "test/test_branch.py::test_two::1",
            ],
        ),
        (
            "import unittest\n"
            "marker = lambda fn: fn\n"
            "for _ in [0]:\n"
            "    try:\n"
            "        break\n"
            "    finally:\n"
            "        marker = unittest.expectedFailure\n"
            "    marker = lambda fn: fn\n"
            "@marker\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "class C:\n"
            "    global ef\n"
            "    from unittest import expectedFailure as ef\n"
            "@ef\n"
            "def test_case():\n"
            "    pass\n",
            ["test/test_branch.py::test_case::1"],
        ),
        (
            "def outer():\n"
            "    mark = lambda fn: fn\n"
            "    def middle():\n"
            "        def inner():\n"
            "            nonlocal mark\n"
            "            from unittest import expectedFailure as mark\n"
            "        inner()\n"
            "    middle()\n"
            "    @mark\n"
            "    def test_case():\n"
            "        pass\n"
            "    return test_case\n",
            ["test/test_branch.py::outer.test_case::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "class C:\n"
            "    ef = lambda fn: fn\n"
            "    @ef\n"
            "    def test_ok(self):\n"
            "        pass\n",
            [],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "class C:\n"
            "    @ef\n"
            "    def test_before(self):\n"
            "        pass\n"
            "    ef = lambda fn: fn\n"
            "    @ef\n"
            "    def test_after(self):\n"
            "        pass\n",
            ["test/test_branch.py::C.test_before::1"],
        ),
        (
            "import unittest\n"
            "class C:\n"
            "    ef = unittest.expectedFailure\n"
            "    ef = lambda fn: fn\n"
            "    @ef\n"
            "    def test_monotone(self):\n"
            "        pass\n",
            ["test/test_branch.py::C.test_monotone::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "class C:\n"
            "    def ef(fn):\n"
            "        return fn\n"
            "    @ef\n"
            "    def test_ok(self):\n"
            "        pass\n"
            "    def method(self):\n"
            "        @ef\n"
            "        def helper():\n"
            "            pass\n"
            "        return helper\n",
            ["test/test_branch.py::C.method.helper::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "class C:\n"
            "    if cond:\n"
            "        ef = lambda fn: fn\n"
            "        @ef\n"
            "        def test_in_branch(self):\n"
            "            pass\n"
            "    @ef\n"
            "    def test_after_if(self):\n"
            "        pass\n",
            ["test/test_branch.py::C.test_after_if::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "class C:\n"
            "    x = cond and (ef := lambda fn: fn)\n"
            "    @ef\n"
            "    def test_after_short_circuit(self):\n"
            "        pass\n",
            ["test/test_branch.py::C.test_after_short_circuit::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "class C:\n"
            "    import unittest as ef\n"
            "    @ef\n"
            "    def test_module_obj(self):\n"
            "        pass\n"
            "    @ef.expectedFailure\n"
            "    def test_attr(self):\n"
            "        pass\n",
            ["test/test_branch.py::C.test_attr::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "class C:\n"
            "    ef = lambda fn: fn\n"
            "    del ef\n"
            "    @ef\n"
            "    def test_case(self):\n"
            "        pass\n",
            ["test/test_branch.py::C.test_case::1"],
        ),
        (
            "from unittest import expectedFailure as ef\n"
            "class C:\n"
            "    ef = lambda fn: fn\n"
            "    if cond:\n"
            "        del ef\n"
            "    @ef\n"
            "    def test_case(self):\n"
            "        pass\n",
            ["test/test_branch.py::C.test_case::1"],
        ),
    ],
)
def test_unittest_expected_failure_branch_merge_and_deferred_lookup(
    tree, source, expected_ids
):
    name = "test/test_branch.py"
    (tree / name).write_text(source, encoding="utf-8")
    records, errors = checker.extract_records(tree)
    assert not errors
    assert [r["id"] for r in records if r["source"] == name] == expected_ids


@pytest.mark.parametrize(
    "import_line, decorator",
    [
        ("import unittest", "@unittest.expectedFailure"),
        ("import unittest as ut", "@ut.expectedFailure"),
        ("from unittest import expectedFailure as ef", "@ef"),
    ],
)
def test_unittest_expected_failure_reports_xfail_in_pytest_and_fails_inventory(
    tree, import_line, decorator
):
    name = "test/core/test_unittest_xfail_witness.py"
    path = tree / name
    path.parent.mkdir(parents=True, exist_ok=True)
    base_cls = "ut.TestCase" if "as ut" in import_line else "unittest.TestCase"
    extra_import = (
        ""
        if "unittest" in import_line and "from" not in import_line
        else "import unittest\n"
    )
    path.write_text(
        f"{extra_import}{import_line}\n\n"
        f"class TestWitness({base_cls}):\n"
        f"    {decorator}\n"
        f"    def test_documented_behaviour(self):\n"
        f"        self.assertEqual(1, 2)\n",
        encoding="utf-8",
    )
    run = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            f"--confcutdir={path.parent}",
            "--override-ini=addopts=",
            "-q",
            "-rxX",
            str(path),
        ],
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert run.returncode == 0, run.stdout + run.stderr
    assert "XFAIL" in run.stdout and "1 xfailed" in run.stdout
    assert checker.main(["--root", str(tree), "--strict"]) == 1
    assert (
        f"X:{name}::TestWitness.test_documented_behaviour::1: unmapped entry"
        in checker.check_tree(tree)["errors"]
    )


_FORTRAN_SAVED_LOCAL_SAMPLE = (
    "subroutine ledger_review_sample()\n"
    "  implicit none\n"
    "  integer :: count = 0\n"
    "  count = count + 1\n"
    "end subroutine\n"
)


def _write_fortran_sample(tree: Path) -> None:
    sample = tree / "src/suews/src/ledger_review_sample.f95"
    sample.parent.mkdir(parents=True, exist_ok=True)
    sample.write_text(_FORTRAN_SAVED_LOCAL_SAMPLE, encoding="utf-8")


def _run_saved_locals_linter(tree: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            sys.executable,
            str(tree / "scripts/lint/check_saved_fortran_locals.py"),
            str(tree),
        ],
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )


def test_saved_local_valid_literal_registration_links_cleanly(tree):
    _write_fortran_sample(tree)
    assert _run_saved_locals_linter(tree).returncode == 1
    linter_path = tree / "scripts/lint/check_saved_fortran_locals.py"
    linter_path.write_text(
        linter_path.read_text(encoding="utf-8").replace(
            "ALLOWED: dict[tuple[str, str], str] = {}",
            'ALLOWED: dict[tuple[str, str], str] = {("ledger_review_sample.f95", "count"): "#2087"}',
        ),
        encoding="utf-8",
    )
    linter_run = _run_saved_locals_linter(tree)
    assert linter_run.returncode == 0, linter_run.stdout + linter_run.stderr
    assert "(1 allowlisted)" in linter_run.stdout
    assert any(
        "Z:ledger_review_sample.f95::count: unmapped entry" in e
        for e in checker.check_tree(tree)["errors"]
    )
    links = _links(tree)
    links["mappings"].append({
        "selector": {"ledger": "Z", "id": "ledger_review_sample.f95::count"},
        "contract": "C-RESPONSE",
        "issue": 2087,
        "classification": "defect",
    })
    _write_links(tree, links)
    report = checker.check_tree(tree)
    assert checker.main(["--root", str(tree), "--strict"]) == 0
    assert 2087 in report["issues"]
    assert any(
        r["ledger"] == "Z" and r["id"] == "ledger_review_sample.f95::count"
        for r in report["records"]
    )


@pytest.mark.parametrize(
    "snippet, diagnostic",
    [
        (
            'reviewed_locals = ALLOWED\nreviewed_locals[("ledger_review_sample.f95", "count")] = "#2087"',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            'reviewed_locals = ALLOWED\nreviewed_locals.update({("ledger_review_sample.f95", "count"): "#2087"})',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            'reviewed_locals = ALLOWED\nreviewed_locals |= {("ledger_review_sample.f95", "count"): "#2087"}',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            "def register_reviewed(table, key, issue):\n    table[key] = issue\n"
            'register_reviewed(ALLOWED, ("ledger_review_sample.f95", "count"), "#2087")',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            "def register_reviewed(table, key, issue):\n    table[key] = issue\n"
            'register_reviewed(key=("ledger_review_sample.f95", "count"), issue="#2087", table=ALLOWED)',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            "def register_reviewed(key, issue, table=ALLOWED):\n    table[key] = issue\n"
            'register_reviewed(("ledger_review_sample.f95", "count"), "#2087")',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            'box = [ALLOWED]\nbox[0][("ledger_review_sample.f95", "count")] = "#2087"',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            'box = [ALLOWED] * 1\nbox[0][("ledger_review_sample.f95", "count")] = "#2087"',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            'box = dict(table=ALLOWED)\nbox["table"][("ledger_review_sample.f95", "count")] = "#2087"',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            'alias = next(iter(()), ALLOWED)\nalias[("ledger_review_sample.f95", "count")] = "#2087"',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            'alias = min(iter(()), default=ALLOWED)\nalias[("ledger_review_sample.f95", "count")] = "#2087"',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            "if (alias := ALLOWED) is not None:\n"
            '    alias[("ledger_review_sample.f95", "count")] = "#2087"',
            "unsupported mutation of constant ALLOWED",
        ),
        (
            'updater = ALLOWED.update\nupdater({("ledger_review_sample.f95", "count"): "#2087"})',
            "unsupported escape of constant ALLOWED",
        ),
        (
            "def get_table():\n    return ALLOWED\n"
            'get_table()[("ledger_review_sample.f95", "count")] = "#2087"',
            "unsupported escape of constant ALLOWED",
        ),
        (
            "class Holder:\n    table = ALLOWED\n"
            'Holder.table[("ledger_review_sample.f95", "count")] = "#2087"',
            "unsupported escape of constant ALLOWED",
        ),
        (
            'globals()["ALLOWED"][("ledger_review_sample.f95", "count")] = "#2087"',
            "unsupported dynamic namespace access via globals",
        ),
        (
            'locals()["ALLOWED"][("ledger_review_sample.f95", "count")] = "#2087"',
            "unsupported dynamic namespace access via locals",
        ),
        (
            "import operator\n"
            'operator.setitem(ALLOWED, ("ledger_review_sample.f95", "count"), "#2087")',
            "unsupported escape of constant ALLOWED",
        ),
    ],
)
def test_saved_local_alias_helper_and_escape_writes_fail_closed_while_linter_accepts(
    tree, snippet, diagnostic
):
    _write_fortran_sample(tree)
    assert _run_saved_locals_linter(tree).returncode == 1
    linter_path = tree / "scripts/lint/check_saved_fortran_locals.py"
    intro = "ALLOWED: dict[tuple[str, str], str] = {}"
    source = linter_path.read_text(encoding="utf-8")
    assert source.count(intro) == 1
    linter_path.write_text(
        source.replace(intro, f"{intro}\n{snippet}"), encoding="utf-8"
    )

    # Prove the real linter actually accepts the witness before asserting fail-closed.
    linter_run = _run_saved_locals_linter(tree)
    assert linter_run.returncode == 0, linter_run.stdout + linter_run.stderr
    assert "(1 allowlisted)" in linter_run.stdout

    assert checker.main(["--root", str(tree), "--strict"]) == 1
    assert any(diagnostic in e for e in checker.check_tree(tree)["errors"])


@pytest.mark.parametrize(
    ("snippet", "annotation_eval"),
    [
        (
            'witness: ALLOWED.update({("ledger_review_sample.f95", "count"): "#2087"})',
            "import sys as _ann_sys; _ = _ann_sys.modules[__name__].__annotations__",
        ),
        (
            'def _ann(x: ALLOWED.update({("ledger_review_sample.f95", "count"): "#2087"})):\n    pass',
            "_ = _ann.__annotations__",
        ),
        (
            'def _ret() -> ALLOWED.update({("ledger_review_sample.f95", "count"): "#2087"}):\n    pass',
            "_ = _ret.__annotations__",
        ),
    ],
)
def test_saved_local_evaluated_annotation_writes_fail_closed(
    tree, snippet, annotation_eval
):
    _write_fortran_sample(tree)
    linter_path = tree / "scripts/lint/check_saved_fortran_locals.py"
    source = linter_path.read_text(encoding="utf-8").replace(
        "from __future__ import annotations\n", ""
    )
    intro = "ALLOWED: dict[tuple[str, str], str] = {}"
    assert source.count(intro) == 1
    linter_path.write_text(
        source.replace(intro, f"{intro}\n{snippet}"), encoding="utf-8"
    )
    # Fail closed on the bare annotation on every Python version: SUEWS supports
    # Python 3.12+, where annotations without future-import execute eagerly, and
    # on 3.14+ (PEP 649) they still execute whenever __annotations__ is read.
    assert checker.main(["--root", str(tree), "--strict"]) == 1
    assert any(
        "unsupported mutation of constant ALLOWED" in e
        for e in checker.check_tree(tree)["errors"]
    )
    # Prove the real linter accepts the runtime witness. On Python 3.14+, PEP 649
    # defers annotation evaluation until __annotations__ is accessed.
    if sys.version_info >= (3, 14):
        linter_path.write_text(
            source.replace(intro, f"{intro}\n{snippet}\n{annotation_eval}"),
            encoding="utf-8",
        )
    linter_run = _run_saved_locals_linter(tree)
    assert linter_run.returncode == 0, linter_run.stdout + linter_run.stderr
    assert "(1 allowlisted)" in linter_run.stdout


def test_python_ledger_safe_reads_aliases_helpers_and_unrelated_writes_are_allowed(
    tmp_path,
):
    (tmp_path / "ledger.py").write_text(
        'LEDGER = {("a.f95", "x"): "#123"}\n'
        "ALIAS = LEDGER\n"
        "def count_entries(table):\n"
        "    return len(table)\n"
        "def has_entry(table, key):\n"
        "    return key in table and table.get(key) is not None\n"
        "def read_copy(flag):\n"
        "    eff = LEDGER if flag else {}\n"
        "    merged = {**eff}\n"
        "    copied = LEDGER.copy()\n"
        '    copied[("copy.f95", "c")] = "#0"\n'
        "    return count_entries(eff) + len(merged.keys()) + len(copied)\n"
        "def shadow_outer_alias():\n"
        "    ALIAS = {}\n"
        '    ALIAS[("local.f95", "l")] = "#0"\n'
        "    unused: LEDGER.clear()\n"
        "    return len(ALIAS) + len(sorted(LEDGER, key=len)) + int(type(LEDGER) is dict)\n"
        'LEDGER[("a.f95", "x")]: str\n'
        "ordered = sorted([LEDGER], key=None)\n"
        'BOX = {}\nBOX["x"]: LEDGER\n'
        "OTHER = {}\n"
        'OTHER[("b.f95", "y")] = "#999"\n'
        "def mutate_unrelated(table, key, issue):\n"
        "    table[key] = issue\n"
        'mutate_unrelated(OTHER, ("c.f95", "z"), "#999")\n'
        'found = has_entry(ALIAS, ("a.f95", "x"))\n',
        encoding="utf-8",
    )
    assert checker._python_values(tmp_path, "ledger.py", ("LEDGER",)) == {
        "LEDGER": {("a.f95", "x"): "#123"}
    }


@pytest.mark.parametrize(
    "source, match",
    [
        (
            'LEDGER = {}\ndef len(table):\n    table["k"] = "v"\nlen(LEDGER)\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            'DEPENDENCY = {"nested": []}\nLEDGER = DEPENDENCY\n'
            'sub = DEPENDENCY["nested"]\nsub.append("new")\n',
            "unsupported mutation of constant DEPENDENCY",
        ),
        (
            'DEPENDENCY = {"nested": []}\nLEDGER = DEPENDENCY\n'
            'def add_item(lst):\n    lst.append("new")\n'
            'add_item(DEPENDENCY["nested"])\n',
            "unsupported mutation of constant DEPENDENCY",
        ),
        (
            "LEDGER = {}\n"
            "alias = None\n"
            "def bind():\n    global alias\n    alias = LEDGER\n"
            'def mutate():\n    alias["k"] = "v"\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            "from external_helper import register\n"
            "LEDGER = {}\n"
            'register(LEDGER, "k", "v")\n',
            "unsupported escape of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "def reg(table, k, v):\n    table[k] = v\n"
            'reg(*(LEDGER, "k", "v"))\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            'LEDGER = {}\nbox = 1 * [LEDGER]\nbox[0]["k"] = "v"\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            'LEDGER = {}\nalias = max(LEDGER, {})\nalias["k"] = "v"\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            'LEDGER = {}\nalias = sum((), LEDGER)\nalias["k"] = "v"\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            'LEDGER = {}\nbox = [LEDGER]\nsorted(box, key=lambda d: d.setdefault("k", "v"))\n',
            "unsupported escape of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "alias = LEDGER\n"
            "def outer():\n"
            "    alias = {}\n"
            "    def inner():\n"
            "        global alias\n"
            '        alias["k"] = "v"\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "def outer():\n"
            "    def len(table):\n"
            '        table["k"] = "v"\n'
            "    def inner():\n"
            "        len(LEDGER)\n"
            "    inner()\n"
            "outer()\n",
            "unsupported mutation of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "def outer():\n"
            "    len = lambda d: d.setdefault('k', 'v')\n"
            "    def inner():\n"
            "        sorted([LEDGER], key=len)\n"
            "    inner()\n"
            "outer()\n",
            "unsupported escape of constant LEDGER",
        ),
        (
            'LEDGER = {}\nbox = {}\nbox["x"]: LEDGER.update({"new": 1})\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "def helper(table):\n"
            "    return len(table)\n"
            "def outer():\n"
            '    helper = lambda table: table.update({"new": 1})\n'
            "    def inner():\n"
            "        helper(LEDGER)\n"
            "    inner()\n"
            "outer()\n",
            "unsupported escape of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "def mutate(table):\n"
            '    table["k"] = "v"\n'
            "[len(LEDGER) for len in [mutate]]\n",
            "unsupported escape of constant LEDGER",
        ),
        (
            'LEDGER = {}\n[(alias := LEDGER) for _ in [0]]\nalias["k"] = "v"\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            'Holder = type("Holder", (), {"table": LEDGER})\n'
            'Holder.table["k"] = "v"\n',
            "unsupported escape of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            'def setup(x=(len := lambda table: table.update({"k": "v"}))):\n'
            "    pass\n"
            "len(LEDGER)\n",
            "unsupported escape of constant LEDGER",
        ),
        (
            'LEDGER = {"x": 1}\ndel (LEDGER["x"],)\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "def mutate(table):\n"
            '    table["x"] = 1\n'
            "f = lambda: ((len := mutate), len(LEDGER))\n"
            "f()\n",
            "unsupported escape of constant LEDGER",
        ),
        (
            'LEDGER = {}\nalias: alias.update({"k": "v"}) = LEDGER\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "def mutate(table):\n"
            '    table["k"] = "v"\n'
            "box = {}\n"
            "box[(len := mutate)] = 0\n"
            "len(LEDGER)\n",
            "unsupported escape of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "def mutate(table):\n"
            '    table["x"] = 1\n'
            "def invoke(helper):\n"
            "    helper(LEDGER)\n"
            "    def helper(table):\n"
            "        return len(table)\n"
            "invoke(mutate)\n",
            "unsupported escape of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "alias = LEDGER\n"
            "def outer():\n"
            "    alias = {}\n"
            "    class C:\n"
            "        global alias\n"
            '        alias["k"] = "v"\n'
            "outer()\n",
            "unsupported mutation of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "def outer():\n"
            "    alias = None\n"
            "    def middle():\n"
            "        def inner():\n"
            "            nonlocal alias\n"
            "            alias = LEDGER\n"
            "        inner()\n"
            "    middle()\n"
            '    alias["k"] = "v"\n'
            "outer()\n",
            "unsupported mutation of constant LEDGER",
        ),
        (
            'LEDGER = {"v": []}\nLEDGER.keys().mapping["v"].append(1)\n',
            "unsupported escape of constant LEDGER",
        ),
        (
            'LEDGER = {"v": []}\nks = LEDGER.keys()\nks.mapping["v"].append(1)\n',
            "unsupported escape of constant LEDGER",
        ),
        (
            'LEDGER = {"v": []}\ndef leak():\n    return LEDGER.keys()\n',
            "unsupported escape of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "def outer():\n"
            "    def helper(table):\n"
            '        table["k"] = 1\n'
            "    class C:\n"
            "        nonlocal helper\n"
            "        helper(LEDGER)\n"
            "        def helper(table):\n"
            "            return len(table)\n"
            "outer()\n",
            "unsupported escape of constant LEDGER",
        ),
        (
            "LEDGER = {}\n"
            "def helper(table):\n"
            '    table["k"] = 1\n'
            "class C:\n"
            "    helper(LEDGER)\n"
            "    def helper(table):\n"
            "        return len(table)\n",
            "unsupported escape of constant LEDGER",
        ),
        (
            'LEDGER = {"v": []}\ndict(LEDGER)["v"].append(1)\n',
            "unsupported mutation of constant LEDGER",
        ),
        (
            'LEDGER = {"v": []}\n(LEDGER | {"b": 1})["v"].append(1)\n',
            "unsupported mutation of constant LEDGER",
        ),
    ],
)
def test_python_ledger_advanced_alias_helper_and_escape_cases_fail(
    tmp_path, source, match
):
    (tmp_path / "ledger.py").write_text(source, encoding="utf-8")
    with pytest.raises(ValueError, match=match):
        checker._python_values(tmp_path, "ledger.py", ("LEDGER",))


def test_python_ledger_keys_iteration_allowed_with_mutable_values(tmp_path):
    (tmp_path / "ledger.py").write_text(
        'LEDGER = {"v": []}\n'
        "def list_keys():\n"
        "    out = [k for k in LEDGER]\n"
        "    for k in LEDGER:\n"
        "        out.append(k)\n"
        "    out.extend(k for k in LEDGER.keys())\n"
        '    _ = (len(LEDGER.keys()), set(LEDGER), tuple(LEDGER), LEDGER.keys().isdisjoint({"x"}))\n'
        '    return list(LEDGER) + sorted(LEDGER) + list(LEDGER.keys()) + list(LEDGER.copy()) + list(dict(LEDGER)) + list(LEDGER | {"b": 1}) + out\n',
        encoding="utf-8",
    )
    assert checker._python_values(tmp_path, "ledger.py", ("LEDGER",)) == {
        "LEDGER": {"v": []}
    }


def test_python_ledger_postponed_annotations_are_not_treated_as_mutations(tmp_path):
    (tmp_path / "ledger.py").write_text(
        "from __future__ import annotations\n"
        'LEDGER = {("a.f95", "x"): "#123"}\n'
        "witness: LEDGER.clear()\n"
        "def helper(x: LEDGER.clear()) -> LEDGER.clear():\n"
        "    return len(LEDGER)\n",
        encoding="utf-8",
    )
    assert checker._python_values(tmp_path, "ledger.py", ("LEDGER",)) == {
        "LEDGER": {("a.f95", "x"): "#123"}
    }


def test_saved_local_mixed_type_malformed_key_reports_value_error_not_type_error(tree):
    linter_path = tree / "scripts/lint/check_saved_fortran_locals.py"
    intro = "ALLOWED: dict[tuple[str, str], str] = {}"
    source = linter_path.read_text(encoding="utf-8")
    linter_path.write_text(
        source.replace(
            intro,
            'ALLOWED = {("sample.f95", "count"): "#2087", "bad_key": "#2087"}',
        ),
        encoding="utf-8",
    )
    _, errors = checker.extract_records(tree)
    assert any(
        "scripts/lint/check_saved_fortran_locals.py: unsupported entry key in ALLOWED: 'bad_key'"
        in err
        for err in errors
    )
