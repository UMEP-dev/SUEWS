//! Resolved aerodynamic selectors are required at every native reader entry point.
// contract: C-STRICT, C-BIND
#![cfg(feature = "physics")]

use serde_yaml::Value;
use std::fs;
use suews_bridge::{load_run_config, load_run_config_from_str, load_run_config_from_value};

const FIXTURES: [&str; 2] = [
    include_str!("../../../test/fixtures/bridge/sample_resolved.yml"),
    include_str!("../../../test/fixtures/bridge/stebbs_resolved.yml"),
];

const SELECTORS: [&str; 4] = [
    "roughness_length_momentum",
    "roughness_length_heat",
    "roughness_sublayer",
    "roughness_sublayer_level",
];

fn config_with_selector(fixture: &str, selector: &str, raw_value: Option<&str>) -> Value {
    let mut root: Value = serde_yaml::from_str(fixture).unwrap();
    root["model"]["control"]["output"]["dir"] = ".".into();
    let physics = root["model"]["physics"].as_mapping_mut().unwrap();
    match raw_value {
        Some(raw) => {
            physics.insert(selector.into(), serde_yaml::from_str(raw).unwrap());
        }
        None => {
            physics.remove(selector);
        }
    }
    root
}

fn invalid_numeric_or_missing_values() -> Vec<Option<&'static str>> {
    vec![
        None,
        Some("null"),
        Some("{value: null}"),
        Some("{}"),
        Some("{ref: citation}"),
        Some("{other: 1}"),
        Some("true"),
        Some("false"),
        Some("1.5"),
        Some("1.0000000005"),
        Some("1.0e12"),
        Some(".nan"),
        Some("{value: .nan}"),
        Some(".inf"),
        Some("-.inf"),
        Some("''"),
        Some("'   '"),
        Some("[]"),
        Some("{value: []}"),
        Some("{value: false}"),
        Some("{value: {value: 1}}"),
    ]
}

#[test]
fn aerodynamic_selectors_required_by_value_string_and_path_readers() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("resolved.yml");
    for fixture in FIXTURES {
        for selector in SELECTORS {
            let expected_path = format!("`model.physics.{selector}`");
            for bad in invalid_numeric_or_missing_values() {
                let mut root = config_with_selector(fixture, selector, bad);
                let text = serde_yaml::to_string(&root).unwrap();
                fs::write(&path, &text).unwrap();
                for error in [
                    load_run_config_from_value(&mut root).unwrap_err(),
                    load_run_config_from_str(&text).unwrap_err(),
                    load_run_config(&path).unwrap_err(),
                ] {
                    assert!(
                        error.contains(&expected_path),
                        "{selector} {bad:?}: {error}"
                    );
                }
            }
            // Unknown non-empty strings fail before numeric parsing with the selector name.
            for unknown in ["bogus_method", "{value: bogus_method}"] {
                let mut root = config_with_selector(fixture, selector, Some(unknown));
                let text = serde_yaml::to_string(&root).unwrap();
                fs::write(&path, &text).unwrap();
                for error in [
                    load_run_config_from_value(&mut root).unwrap_err(),
                    load_run_config_from_str(&text).unwrap_err(),
                    load_run_config(&path).unwrap_err(),
                ] {
                    assert!(error.contains(selector), "{selector} {unknown}: {error}");
                }
            }
        }
    }
}

#[test]
fn aerodynamic_selectors_asymmetric_valid_alternatives_reach_proper_receivers() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("resolved.yml");
    fs::write(dir.path().join("forcing.txt"), "").unwrap();

    let cases: [(&str, [&str; 4], [i32; 4]); 5] = [
        (
            "bare_integers_asymmetric",
            ["3", "4", "0", "2"],
            [3, 4, 0, 2],
        ),
        (
            "wrapped_integers_asymmetric",
            ["{value: 4}", "{value: 1}", "{value: 1}", "{value: 0}"],
            [4, 1, 1, 0],
        ),
        (
            "integral_floats_and_most_level_basic",
            ["2.0", "{value: 3.0}", "0.0", "{value: 1.0}"],
            [2, 3, 0, 1],
        ),
        (
            "readable_names_asymmetric",
            ["macdonald", "kanda", "most", "detailed"],
            [3, 4, 0, 2],
        ),
        (
            "wrapped_readable_citation_aliases",
            [
                "{value: go99}",
                "{value: vg00}",
                "{value: t19}",
                "{value: none}",
            ],
            [4, 3, 1, 0],
        ),
    ];

    for fixture in FIXTURES {
        for (label, inputs, [exp_mom, exp_heat, exp_rsl, exp_lvl]) in cases {
            let mut root: Value = serde_yaml::from_str(fixture).unwrap();
            root["model"]["control"]["output"]["dir"] = ".".into();
            root["model"]["control"]["forcing"]["file"] = "forcing.txt".into();
            let physics = root["model"]["physics"].as_mapping_mut().unwrap();
            for (selector, raw) in SELECTORS.iter().zip(inputs) {
                physics.insert((*selector).into(), serde_yaml::from_str(raw).unwrap());
            }
            let text = serde_yaml::to_string(&root).unwrap();
            fs::write(&path, &text).unwrap();

            for loaded in [
                load_run_config_from_value(&mut root).unwrap(),
                load_run_config_from_str(&text).unwrap(),
                load_run_config(&path).unwrap(),
            ] {
                let cfg = &loaded.config;
                assert_eq!(cfg.rough_len_mom_method, exp_mom, "{label}: mom");
                assert_eq!(cfg.rough_len_heat_method, exp_heat, "{label}: heat");
                assert_eq!(cfg.rsl_method, exp_rsl, "{label}: rsl");
                assert_eq!(cfg.rsl_level, exp_lvl, "{label}: rsl_level");
                let flat = cfg.to_flat();
                assert_eq!(flat[3], f64::from(exp_mom), "{label}: flat[3]");
                assert_eq!(flat[2], f64::from(exp_heat), "{label}: flat[2]");
                assert_eq!(flat[0], f64::from(exp_rsl), "{label}: flat[0]");
                assert_eq!(flat[17], f64::from(exp_lvl), "{label}: flat[17]");
            }
        }
    }
}

#[test]
fn aerodynamic_selectors_preserve_existing_legacy_and_intermediate_aliases() {
    let aliases = [
        (
            [
                "roughness_length_momentum_method",
                "roughness_length_heat_method",
                "rsl_method",
                "rsl_level",
            ],
            [3, 4, 0, 2],
        ),
        (
            [
                "roughlenmommethod",
                "roughlenheatmethod",
                "rslmethod",
                "rsllevel",
            ],
            [4, 1, 1, 0],
        ),
    ];

    for (keys, [exp_mom, exp_heat, exp_rsl, exp_lvl]) in aliases {
        let mut root: Value = serde_yaml::from_str(FIXTURES[0]).unwrap();
        let physics = root["model"]["physics"].as_mapping_mut().unwrap();
        for canonical in SELECTORS {
            physics.remove(canonical);
        }
        for (alias, val) in keys.iter().zip([exp_mom, exp_heat, exp_rsl, exp_lvl]) {
            physics.insert((*alias).into(), Value::from(val));
        }
        let loaded = load_run_config_from_value(&mut root).unwrap();
        assert_eq!(loaded.config.rough_len_mom_method, exp_mom);
        assert_eq!(loaded.config.rough_len_heat_method, exp_heat);
        assert_eq!(loaded.config.rsl_method, exp_rsl);
        assert_eq!(loaded.config.rsl_level, exp_lvl);
    }

    // Malformed legacy alias reports the canonical data-model path.
    let mut root: Value = serde_yaml::from_str(FIXTURES[0]).unwrap();
    let physics = root["model"]["physics"].as_mapping_mut().unwrap();
    physics.remove("roughness_length_momentum");
    physics.insert("roughlenmommethod".into(), Value::Null);
    let err = load_run_config_from_value(&mut root).unwrap_err();
    assert!(
        err.contains("`model.physics.roughness_length_momentum`"),
        "expected canonical path, got: {err}"
    );
}

#[test]
#[cfg(all(feature = "physics", feature = "arrow-output"))]
fn engine_binary_rejects_invalid_aerodynamic_selectors_before_output() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("resolved.yml");
    let arrow_out = dir.path().join("suews_output.arrow");

    let representative_invalid = [
        None,
        Some("null"),
        Some("{value: null}"),
        Some("{}"),
        Some("{ref: citation}"),
        Some(".nan"),
        Some("{value: .nan}"),
        Some(".inf"),
        Some("1.5"),
        Some("true"),
        Some("''"),
        Some("[]"),
        Some("{value: {value: 1}}"),
    ];

    for fixture in FIXTURES {
        for selector in SELECTORS {
            let expected_path = format!("`model.physics.{selector}`");
            for bad in representative_invalid {
                let root = config_with_selector(fixture, selector, bad);
                fs::write(&path, serde_yaml::to_string(&root).unwrap()).unwrap();
                let output = std::process::Command::new(env!("CARGO_BIN_EXE_suews-engine"))
                    .arg("run")
                    .arg(&path)
                    .output()
                    .unwrap();
                let stderr = String::from_utf8_lossy(&output.stderr);
                assert!(
                    !output.status.success(),
                    "{selector} {bad:?}: engine accepted invalid input"
                );
                assert!(
                    stderr.contains(&expected_path),
                    "{selector} {bad:?}: {stderr}"
                );
                assert!(
                    stderr.contains("suews run"),
                    "{selector} {bad:?}: missing resolved-YAML hint: {stderr}"
                );
                assert!(
                    !arrow_out.exists(),
                    "{selector} {bad:?}: engine wrote output on invalid input"
                );
            }
        }
    }
}
