"""Tests for scripts/lint/check_rust_optional_reads.py.

The lint is a ratchet over legacy optional YAML readers in
``src/suews_bridge/src/yaml_config.rs``: counts above the committed baseline
must fail so new optional fallbacks cannot be added, and counts below the
baseline must fail until the baseline is lowered. The live source tree is
checked too so any drift fails here as well as in CI.
"""

from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import sys

import pytest

pytestmark = pytest.mark.api


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "lint" / "check_rust_optional_reads.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "check_rust_optional_reads", SCRIPT_PATH
)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
check_rust_optional_reads = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = check_rust_optional_reads
SCRIPT_SPEC.loader.exec_module(check_rust_optional_reads)


SOURCE = """\
fn read_numeric(root: &Value, path: &[&str]) -> Option<f64> {
    None
}

fn read_i32(root: &Value, path: &[&str]) -> Option<i32> {
    read_numeric(root, path).map(|v| v as i32)
}

fn require_value<'a>(root: &'a Value, path: &[&str]) -> Result<&'a Value, String> {
    // read_numeric(root, &["in_line_comment"])
    /* outer /* nested read_i32(root, &["in_block"]) */
       read_string(root, &["still_comment"]) */
    let _label = "read_numeric(root, &[\\"in_string\\"])";
    let _raw = r#"read_numeric(root, &["in_raw_string"])"#;
    let _ch = 'x';
    Ok(root)
}

fn apply_soil_overrides(root: &Value) -> Result<(), String> {
    if let Some(_v) = read_numeric(root, &["soildepth"]) {}
    if let Some(_v) = read_numeric(root, &["soilstorecap"]) {}
    let _strict = require_numeric(root, &["other"], "sites[0]")?;
    Ok(())
}

pub fn load_run_config_from_value(root: &Value) -> Result<(), String> {
    let _n = read_i32(root, &["nlayer"]);
    let _s = read_string(root, &["dir"]);
    let _seq = read_numeric_sequence(root, &["dz"], "sites[0]", 5);
    let _sized = read_sized_numeric_sequence(root, &[&["k"]], "sites[0]", 5);
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_only_calls_are_ignored() {
        let _ = read_numeric(&Value::Null, &["a"]);
        let _ = read_i32(&Value::Null, &["b"]);
    }
}
"""


EXPECTED = {
    "apply_soil_overrides": {"read_numeric": 2},
    "load_run_config_from_value": {
        "read_i32": 1,
        "read_numeric_sequence": 1,
        "read_sized_numeric_sequence": 1,
        "read_string": 1,
    },
}


def _make_repo(
    tmp_path: Path,
    source: str,
    baseline: dict | None = None,
    *,
    totals: dict[str, int] | None = None,
) -> Path:
    """Build a throwaway repo root with one yaml_config.rs and a baseline."""
    target = tmp_path / check_rust_optional_reads.TARGET_PATH
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(source, encoding="utf-8")
    baseline_path = tmp_path / check_rust_optional_reads.BASELINE_PATH
    baseline_path.parent.mkdir(parents=True, exist_ok=True)
    if baseline is not None:
        payload = {
            "totals": (
                check_rust_optional_reads._totals(baseline)
                if totals is None
                else totals
            ),
            "functions": baseline,
        }
        baseline_path.write_text(json.dumps(payload), encoding="utf-8")
    return tmp_path


def _run(repo_root: Path, *flags: str) -> int:
    return check_rust_optional_reads.main([
        "check_rust_optional_reads",
        str(repo_root),
        *flags,
    ])


def test_counts_production_calls_and_ignores_helpers_comments_strings_and_tests():
    """Comments, strings, lifetimes, helper bodies and #[cfg(test)] are excluded."""
    counts = check_rust_optional_reads.count_optional_reads(SOURCE)

    assert counts == EXPECTED


def test_single_line_and_impl_calls_are_counted():
    """Single-line functions and calls inside impl blocks cannot bypass the scan."""
    source = (
        'fn inline_reader(v: &Value) { let _ = read_numeric(v, &["a"]); }\n'
        "impl Loader {\n"
        '    fn hidden(v: &Value) { let _ = read_i32(v, &["b"]); }\n'
        "}\n"
    )
    counts = check_rust_optional_reads.count_optional_reads(source)

    assert counts == {
        "<other>": {"read_i32": 1},
        "inline_reader": {"read_numeric": 1},
    }


def test_matching_baseline_passes(tmp_path):
    repo = _make_repo(tmp_path, SOURCE, EXPECTED)

    assert _run(repo) == 0


def test_new_optional_read_fails(tmp_path, capsys):
    """A count above the baseline fails and names the function and reader."""
    baseline = {
        **EXPECTED,
        "apply_soil_overrides": {"read_numeric": 1},
    }
    repo = _make_repo(tmp_path, SOURCE, baseline)

    assert _run(repo) == 1
    report = capsys.readouterr().err
    assert "apply_soil_overrides: read_numeric 1 -> 2" in report


def test_new_function_with_optional_read_fails(tmp_path, capsys):
    """A function absent from the baseline is allowed zero legacy optional reads."""
    extra = SOURCE.replace(
        "#[cfg(test)]",
        "fn apply_new_overrides(v: &Value) {\n"
        '    let _ = read_numeric(v, &["gdd_base"]);\n'
        "}\n\n#[cfg(test)]",
    )
    repo = _make_repo(tmp_path, extra, EXPECTED)

    assert _run(repo) == 1
    assert "apply_new_overrides: read_numeric 0 -> 1" in capsys.readouterr().err


def test_production_function_after_cfg_test_module_is_detected(tmp_path, capsys):
    """Production code placed after `#[cfg(test)] mod tests { ... }` cannot escape."""
    extra = (
        SOURCE
        + "\nfn report_numeric_setting(root: &Value) -> Option<f64> {\n"
        + '    read_numeric(root, &["test_setting"])\n'
        + "}\n"
    )
    repo = _make_repo(tmp_path, extra, EXPECTED)

    assert _run(repo) == 1
    assert "report_numeric_setting: read_numeric 0 -> 1" in capsys.readouterr().err


def test_production_code_before_between_and_after_cfg_test_items():
    """Only `#[cfg(test)]`-annotated items are stripped, never following prod code."""
    source = """\
fn before_tests(root: &Value) {
    let _ = read_numeric(root, &["before"]);
}

#[cfg(test)]
pub(crate) fn fixture_helper(root: &Value) {
    let _ = read_numeric(root, &["ignored_helper"]);
}

fn between_tests(root: &Value) {
    let _ = read_i32(root, &["between"]);
}

#[cfg(test)]
#[allow(dead_code)]
mod tests {
    #[test]
    fn braces_in_strings_and_comments_do_not_break_stripping() {
        let _s = "} fn fake_prod(r: &Value) { read_numeric(r, &[\\"x\\"]); }";
        let _r = r#"} read_i32(r, &["y"])"#;
        let _c = '}';
        // } read_string(r, &["z"])
        /* nested /* } */ } read_numeric(r, &["w"]) */
        let _ = read_numeric(&Value::Null, &["ignored_in_mod"]);
    }
}

fn after_tests(root: &Value) {
    let _ = read_string(root, &["after"]);
}
"""
    counts = check_rust_optional_reads.count_optional_reads(source)

    assert counts == {
        "after_tests": {"read_string": 1},
        "before_tests": {"read_numeric": 1},
        "between_tests": {"read_i32": 1},
    }


def test_cfg_test_semicolon_and_nested_items_preserve_following_production_code():
    """Semicolon-terminated, array-arg, const-fn and impl `#[cfg(test)]` items."""
    source = """\
#[cfg(test)]
mod external_tests;

#[cfg(test)]
use super::{alpha, beta};

#[cfg(test)]
const TEST_SENTINEL: Dummy = Dummy { val: 1 };

#[cfg(test)]
pub(crate) const fn test_const_helper() -> usize {
    42
}

#[cfg(test)]
fn array_arg_helper(_buf: [u8; 4], root: &Value) {
    let _ = read_numeric(root, &["ignored_array_helper"]);
}

fn after_semicolon_and_helpers(root: &Value) {
    let _ = read_numeric(root, &["kept_after_helpers"]);
}

impl Loader {
    #[cfg(test)]
    fn test_only_method(&self, root: &Value) {
        let _ = read_numeric(root, &["ignored_method"]);
    }

    fn prod_method(&self, root: &Value) {
        let _ = read_i32(root, &["kept_method"]);
    }
}
"""
    counts = check_rust_optional_reads.count_optional_reads(source)

    assert counts == {
        "<other>": {"read_i32": 1},
        "after_semicolon_and_helpers": {"read_numeric": 1},
    }


def test_stale_baseline_fails(tmp_path, capsys):
    """A count below the baseline fails until the baseline is lowered."""
    baseline = {
        **EXPECTED,
        "apply_soil_overrides": {"read_numeric": 3},
    }
    repo = _make_repo(tmp_path, SOURCE, baseline)

    assert _run(repo) == 1
    assert "apply_soil_overrides: read_numeric 3 -> 2" in capsys.readouterr().err


def test_mismatched_or_missing_baseline_totals_fails(tmp_path, capsys):
    """Hand-merged totals that disagree with per-function sums must fail."""
    stale_totals = {
        **check_rust_optional_reads._totals(EXPECTED),
        "read_numeric": 999,
    }
    repo = _make_repo(tmp_path, SOURCE, EXPECTED, totals=stale_totals)

    assert _run(repo) == 1
    assert "baseline totals out of sync" in capsys.readouterr().err

    baseline_path = repo / check_rust_optional_reads.BASELINE_PATH
    baseline_path.write_text(json.dumps({"functions": EXPECTED}), encoding="utf-8")
    assert _run(repo) == 1
    assert "baseline totals out of sync" in capsys.readouterr().err


def test_write_baseline_round_trips(tmp_path):
    """--write-baseline records the tree, after which the check passes."""
    repo = _make_repo(tmp_path, SOURCE)

    assert _run(repo, "--write-baseline") == 0
    payload = json.loads(
        (repo / check_rust_optional_reads.BASELINE_PATH).read_text(encoding="utf-8")
    )
    assert payload["functions"] == EXPECTED
    assert _run(repo) == 0


def test_source_tree_matches_baseline():
    """The committed baseline equals the shipped yaml_config.rs counts."""
    assert _run(REPO_ROOT) == 0
