//! The resolved forcing clock is required at every native reader entry point.
#![cfg(feature = "physics")]

use serde_yaml::Value;
use std::fs;
use suews_bridge::{load_run_config, load_run_config_from_str, load_run_config_from_value};

const PATH: &str = "model.control.forcing.timestamp_reference";
const FIXTURES: [&str; 2] = [
    include_str!("../../../test/fixtures/bridge/sample_resolved.yml"),
    include_str!("../../../test/fixtures/bridge/stebbs_resolved.yml"),
];

fn config(fixture: &str, clock: Option<&str>) -> Value {
    let mut root: Value = serde_yaml::from_str(fixture).unwrap();
    let forcing = root["model"]["control"]["forcing"].as_mapping_mut().unwrap();
    match clock {
        Some(clock) => {
            forcing.insert("timestamp_reference".into(), serde_yaml::from_str(clock).unwrap());
        }
        None => {
            forcing.remove("timestamp_reference");
        }
    }
    root
}

fn invalid_clocks() -> Vec<Option<&'static str>> {
    vec![
        None,
        Some("null"),
        Some("{value: null}"),
        Some("{}"),
        Some("{ref: citation}"),
        Some("{other: utc}"),
        Some("false"),
        Some("-999"),
        Some(".nan"),
        Some("[]"),
        Some("{value: []}"),
        Some("{value: false}"),
        Some("{value: 1}"),
        Some("{value: {value: utc}}"),
        Some("''"),
        Some("civil_time"),
        Some("{value: civil_time}"),
    ]
}

#[test]
fn forcing_clock_required_by_value_string_and_path_readers() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("resolved.yml");
    for fixture in FIXTURES {
        for clock in invalid_clocks() {
            let mut root = config(fixture, clock);
            let text = serde_yaml::to_string(&root).unwrap();
            fs::write(&path, &text).unwrap();
            for error in [
                load_run_config_from_value(&mut root).unwrap_err(),
                load_run_config_from_str(&text).unwrap_err(),
                load_run_config(&path).unwrap_err(),
            ] {
                assert!(error.contains(PATH), "{clock:?}: {error}");
            }
        }
    }
}

#[test]
fn forcing_clock_bare_and_wrapped_reach_named_config_member() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("resolved.yml");
    fs::write(dir.path().join("forcing.txt"), "").unwrap();
    for fixture in FIXTURES {
        for (clock, expected) in [("local_standard_time", 0), ("utc", 1)] {
            for clock in [clock.to_string(), format!("{{value: {clock}}}")] {
                let mut root = config(fixture, Some(&clock));
                root["model"]["control"]["forcing"]["file"] = "forcing.txt".into();
                root["sites"][0]["properties"]["timezone"] =
                    serde_yaml::from_str("{value: 5.75}").unwrap();
                let text = serde_yaml::to_string(&root).unwrap();
                fs::write(&path, &text).unwrap();
                for loaded in [
                    load_run_config_from_value(&mut root).unwrap(),
                    load_run_config_from_str(&text).unwrap(),
                    load_run_config(&path).unwrap(),
                ] {
                    assert_eq!(loaded.config.forcing_timestamp_reference, expected);
                }
            }
        }
    }
}

#[test]
#[cfg(all(feature = "physics", feature = "arrow-output"))]
fn engine_binary_rejects_invalid_resolved_forcing_clocks() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("resolved.yml");
    for fixture in FIXTURES {
        for clock in invalid_clocks() {
            fs::write(&path, serde_yaml::to_string(&config(fixture, clock)).unwrap()).unwrap();
            let output = std::process::Command::new(env!("CARGO_BIN_EXE_suews-engine"))
                .arg("run")
                .arg(&path)
                .output()
                .unwrap();
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(!output.status.success(), "{clock:?}: engine accepted invalid input");
            assert!(stderr.contains(PATH), "{clock:?}: {stderr}");
        }
    }
}
