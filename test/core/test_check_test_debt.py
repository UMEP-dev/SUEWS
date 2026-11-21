"""Test debt governance through the strict CLI and isolated malformed ledgers."""

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
