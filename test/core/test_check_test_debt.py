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
        "test/data_model/test_unit_metadata.py",
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
    assert observer_ids.count("heat_state.temp_surf_ehc_fast") == 1
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
            'path: "heatState%temp_surf_ehc_fast"',
            'path: "heatState%unknown"',
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
