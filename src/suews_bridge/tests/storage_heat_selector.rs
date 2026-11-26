//! Resolved `model.physics.storage_heat` is required at every native reader entry point.
// contract: C-STRICT, C-BIND
#![cfg(feature = "physics")]

use serde_yaml::Value;
use std::fs;
use suews_bridge::{load_run_config, load_run_config_from_str, load_run_config_from_value};

const PATH: &str = "model.physics.storage_heat";
const FIXTURES: [&str; 2] = [
    include_str!("../../../test/fixtures/bridge/sample_resolved.yml"),
    include_str!("../../../test/fixtures/bridge/stebbs_resolved.yml"),
];

fn config_with_storage_heat(fixture: &str, key: &str, value_yaml: Option<&str>) -> Value {
    let mut root: Value = serde_yaml::from_str(fixture).unwrap();
    // Both fixtures also pass through the file loader, which requires output.dir.
    root["model"]["control"]["output"]["dir"] = ".".into();
    // Use J11 QF while testing storage selectors independently of STEBBS coupling.
    root["model"]["physics"]["emissions"] = Value::from(2);
    let physics = root["model"]["physics"].as_mapping_mut().unwrap();
    physics.remove("storage_heat");
    physics.remove("storage_heat_method");
    physics.remove("storageheatmethod");
    if let Some(yaml) = value_yaml {
        physics.insert(
            Value::String(key.to_string()),
            serde_yaml::from_str(yaml).unwrap(),
        );
    }
    root
}

fn invalid_values_named_path() -> Vec<(&'static str, &'static str, Option<&'static str>)> {
    vec![
        ("absent", "storage_heat", None),
        ("bare-null", "storage_heat", Some("null")),
        ("wrapped-null", "storage_heat", Some("{value: null}")),
        (
            "wrapped-null-with-ref",
            "storage_heat",
            Some("{value: null, ref: {desc: citation}}"),
        ),
        ("empty-map", "storage_heat", Some("{}")),
        (
            "ref-only-map",
            "storage_heat",
            Some("{ref: {desc: citation}}"),
        ),
        ("bare-nan", "storage_heat", Some(".nan")),
        ("wrapped-nan", "storage_heat", Some("{value: .nan}")),
        ("pos-inf", "storage_heat", Some(".inf")),
        ("neg-inf", "storage_heat", Some("-.inf")),
        ("wrapped-pos-inf", "storage_heat", Some("{value: .inf}")),
        ("wrapped-neg-inf", "storage_heat", Some("{value: -.inf}")),
        ("out-of-i32-high", "storage_heat", Some("2147483648")),
        ("out-of-i32-low", "storage_heat", Some("-2147483649")),
        (
            "wrapped-out-of-i32-high",
            "storage_heat",
            Some("{value: 2147483648}"),
        ),
        ("fractional", "storage_heat", Some("1.5")),
        ("near-int", "storage_heat", Some("1.0000000005")),
        ("neg-fractional", "storage_heat", Some("-0.5")),
        ("wrapped-fractional", "storage_heat", Some("{value: 1.5}")),
        ("bool-true", "storage_heat", Some("true")),
        ("bool-false", "storage_heat", Some("false")),
        ("wrapped-bool", "storage_heat", Some("{value: true}")),
        ("list", "storage_heat", Some("[1]")),
        ("wrapped-list", "storage_heat", Some("{value: [1]}")),
        ("unexpected-map", "storage_heat", Some("{unexpected: 1}")),
        (
            "double-wrapped",
            "storage_heat",
            Some("{value: {value: 1}}"),
        ),
        ("empty-string", "storage_heat", Some("''")),
        ("whitespace-string", "storage_heat", Some("'   '")),
        ("wrapped-empty-string", "storage_heat", Some("{value: ''}")),
        ("legacy-fused-null", "storageheatmethod", Some("null")),
        (
            "legacy-fused-wrapped-null",
            "storageheatmethod",
            Some("{value: null}"),
        ),
        ("compat-alias-nan", "storage_heat_method", Some(".nan")),
        (
            "compat-alias-out-of-i32",
            "storage_heat_method",
            Some("2147483648"),
        ),
    ]
}

#[test]
fn storage_heat_required_by_value_string_and_path_readers() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("resolved.yml");
    for fixture in FIXTURES {
        for (label, key, value_yaml) in invalid_values_named_path() {
            let mut root = config_with_storage_heat(fixture, key, value_yaml);
            let text = serde_yaml::to_string(&root).unwrap();
            fs::write(&path, &text).unwrap();
            for error in [
                load_run_config_from_value(&mut root).unwrap_err(),
                load_run_config_from_str(&text).unwrap_err(),
                load_run_config(&path).unwrap_err(),
            ] {
                assert!(error.contains(PATH), "{label}: {error}");
            }
        }

        // Missing entire model.physics section is also rejected with a model.physics.* path
        // (whichever required physics selector is checked first).
        let mut no_physics: Value = serde_yaml::from_str(fixture).unwrap();
        no_physics["model"]["control"]["output"]["dir"] = ".".into();
        no_physics["model"]
            .as_mapping_mut()
            .unwrap()
            .remove("physics");
        let text = serde_yaml::to_string(&no_physics).unwrap();
        fs::write(&path, &text).unwrap();
        for error in [
            load_run_config_from_value(&mut no_physics).unwrap_err(),
            load_run_config_from_str(&text).unwrap_err(),
            load_run_config(&path).unwrap_err(),
        ] {
            assert!(
                error.contains("model.physics.") && error.contains("missing"),
                "missing physics section: {error}"
            );
        }
    }
}

#[test]
fn storage_heat_unknown_strings_and_duplicate_spellings_are_rejected() {
    for fixture in FIXTURES {
        for raw in ["bogus", "{value: bogus}", "'1'", "{value: '1'}"] {
            let mut root = config_with_storage_heat(fixture, "storage_heat", Some(raw));
            let err = load_run_config_from_value(&mut root).unwrap_err();
            assert!(err.contains("storage_heat"), "{raw}: {err}");
        }

        let mut dup = config_with_storage_heat(fixture, "storage_heat", Some("1"));
        dup["model"]["physics"]
            .as_mapping_mut()
            .unwrap()
            .insert("storageheatmethod".into(), Value::Number(1.into()));
        let err = load_run_config_from_value(&mut dup).unwrap_err();
        assert!(
            err.contains("storage_heat") || err.contains("storageheatmethod"),
            "duplicate spellings: {err}"
        );
    }
}

#[test]
fn storage_heat_legitimate_selectors_aliases_and_names_decode() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("resolved.yml");
    fs::write(dir.path().join("forcing.txt"), "").unwrap();

    let cases: &[(&str, &str, i32)] = &[
        ("storage_heat", "0", 0),
        ("storage_heat", "{value: 0}", 0),
        ("storage_heat", "observed", 0),
        ("storage_heat", "{observed: {value: 0}}", 0),
        ("storage_heat", "1", 1),
        ("storage_heat", "1.0", 1),
        ("storage_heat", "{value: 1}", 1),
        ("storage_heat", "{value: 1.0}", 1),
        ("storage_heat", "ohm", 1),
        ("storage_heat", "ohm_without_qf", 1),
        ("storage_heat", "{value: ohm}", 1),
        ("storage_heat", "{ohm: {value: 1}}", 1),
        ("storage_heat", "3", 3),
        ("storage_heat", "anohm", 3),
        ("storage_heat", "s17", 3),
        ("storage_heat", "{anohm: {value: 3}}", 3),
        ("storage_heat", "5", 5),
        ("storage_heat", "ehc", 5),
        ("storage_heat", "{ehc: {value: 5}}", 5),
        ("storage_heat", "6", 6),
        ("storage_heat", "dyohm", 6),
        ("storage_heat", "l25", 6),
        ("storage_heat", "{dyohm: {value: 6}}", 6),
        ("storage_heat", "7", 7),
        ("storage_heat", "stebbs", 7),
        ("storage_heat", "{stebbs: {value: 7}}", 7),
        ("storage_heat", "8", 8),
        ("storage_heat", "dyohm_building", 8),
        ("storage_heat", "dyohm_buildings", 8),
        ("storage_heat", "dyohm_bldg", 8),
        ("storage_heat", "dyohm_bldgs", 8),
        ("storage_heat", "{dyohm_building: {value: 8}}", 8),
        ("storageheatmethod", "{value: 1}", 1),
        ("storageheatmethod", "6", 6),
        ("storage_heat_method", "{value: 1}", 1),
        ("storage_heat_method", "dyohm", 6),
        // Existing contract does not apply a YAML numeric range filter here:
        // codes 2 and 4 decode as i32 (used by yaml_jacobian and rejected by Fortran error 106).
        ("storage_heat", "2", 2),
        ("storage_heat", "4", 4),
        ("storage_heat", "estm", 4),
        ("storage_heat", "o05", 4),
    ];

    for fixture in FIXTURES {
        for &(key, value_yaml, expected) in cases {
            let mut root = config_with_storage_heat(fixture, key, Some(value_yaml));
            root["model"]["control"]["forcing"]["file"] = "forcing.txt".into();
            let text = serde_yaml::to_string(&root).unwrap();
            fs::write(&path, &text).unwrap();
            for loaded in [
                load_run_config_from_value(&mut root).unwrap(),
                load_run_config_from_str(&text).unwrap(),
                load_run_config(&path).unwrap(),
            ] {
                assert_eq!(
                    loaded.config.storage_heat_method, expected,
                    "{key}={value_yaml}"
                );
            }
        }
    }
}

#[test]
fn storage_heat_nested_ohm_include_qf_forms_decode() {
    for fixture in FIXTURES {
        for (yaml, expected_qf) in [
            ("{ohm: {include_qf: false}}", 0),
            ("{ohm: {include_qf: true}}", 1),
            ("{scheme: ohm, ohm_inc_qf: exclude}", 0),
            ("{scheme: ohm, ohm_inc_qf: include}", 1),
        ] {
            let mut root = config_with_storage_heat(fixture, "storage_heat", Some(yaml));
            let physics = root["model"]["physics"].as_mapping_mut().unwrap();
            physics.remove("ohm_inc_qf");
            physics.remove("ohmincqf");
            let loaded = load_run_config_from_value(&mut root).unwrap();
            assert_eq!(loaded.config.storage_heat_method, 1, "{yaml}");
            assert_eq!(loaded.config.ohm_inc_qf, expected_qf, "{yaml}");
        }
    }
}

#[test]
#[cfg(all(feature = "physics", feature = "arrow-output"))]
fn engine_binary_rejects_invalid_resolved_storage_heat() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("resolved.yml");
    let arrow = dir.path().join("suews_output.arrow");
    for fixture in FIXTURES {
        for (label, key, value_yaml) in invalid_values_named_path() {
            let _ = fs::remove_file(&arrow);
            let mut root = config_with_storage_heat(fixture, key, value_yaml);
            root["model"]["control"]["output"]["dir"] = dir.path().to_str().unwrap().into();
            fs::write(&path, serde_yaml::to_string(&root).unwrap()).unwrap();
            let output = std::process::Command::new(env!("CARGO_BIN_EXE_suews-engine"))
                .arg("run")
                .arg(&path)
                .output()
                .unwrap();
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                !output.status.success(),
                "{label}: engine accepted invalid storage_heat"
            );
            assert!(stderr.contains(PATH), "{label}: {stderr}");
            assert!(!arrow.exists(), "{label}: engine wrote output on error");
        }
    }
}
