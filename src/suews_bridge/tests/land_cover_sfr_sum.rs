//! The land-cover surface-fraction sum contract is enforced at every native reader entry point.
// contract: C-VALID, C-NATIVE-ERROR, C-PARITY

use serde_yaml::Value;
use std::fs;
use suews_bridge::{load_run_config, load_run_config_from_str, load_run_config_from_value};

const SAMPLE_YAML: &str = include_str!("../../../test/fixtures/bridge/sample_resolved.yml");

fn with_paved_sfr_delta(delta: f64) -> Value {
    let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).expect("parse sample_resolved.yml");
    let paved_sfr = &mut root["sites"][0]["properties"]["land_cover"]["paved"]["sfr"];
    let base = paved_sfr
        .as_f64()
        .or_else(|| paved_sfr.get("value").and_then(Value::as_f64))
        .expect("sample_resolved.yml paved.sfr must be numeric");
    *paved_sfr = Value::from(base + delta);
    root
}

#[test]
fn native_loaders_enforce_land_cover_sfr_sum_contract() {
    let dir = tempfile::tempdir().expect("tempdir");
    let bad_path = dir.path().join("bad_sfr_sum.yml");
    let mut bad_root = with_paved_sfr_delta(0.0002);
    let bad_text = serde_yaml::to_string(&bad_root).expect("serialise bad YAML");
    fs::write(&bad_path, &bad_text).expect("write bad YAML");

    for (label, result) in [
        ("value", load_run_config_from_value(&mut bad_root)),
        ("str", load_run_config_from_str(&bad_text)),
        ("file", load_run_config(&bad_path)),
    ] {
        let err = result.expect_err(&format!("{label}: drift > 1e-4 must fail"));
        assert!(
            err.contains("properties.land_cover.*.sfr") && err.contains("1.0"),
            "{label}: unexpected error: {err}"
        );
    }

    let mut ok_root = with_paved_sfr_delta(5.0e-5);
    load_run_config_from_value(&mut ok_root)
        .expect("drift within 1e-4 tolerance should remain accepted");
}

#[test]
#[cfg(all(feature = "physics", feature = "arrow-output"))]
fn engine_binary_rejects_land_cover_sfr_sum_outside_tolerance() {
    let dir = tempfile::tempdir().expect("tempdir");
    let path = dir.path().join("bad_sfr_sum.yml");
    let bad_root = with_paved_sfr_delta(0.0002);
    fs::write(&path, serde_yaml::to_string(&bad_root).expect("serialise")).expect("write");

    let output = std::process::Command::new(env!("CARGO_BIN_EXE_suews-engine"))
        .arg("run")
        .arg(&path)
        .output()
        .expect("invoke suews-engine");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !output.status.success(),
        "suews-engine accepted land_cover sfr sum outside 1e-4 tolerance"
    );
    assert!(
        stderr.contains("properties.land_cover.*.sfr"),
        "unexpected suews-engine stderr: {stderr}"
    );
}
