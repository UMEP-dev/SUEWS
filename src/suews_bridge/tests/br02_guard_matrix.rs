//! Per-leaf BR-02 invalid-YAML matrix coverage retired from Python guard suites (#2349 W0A-5).
// contract: C-STRICT, C-BIND
#![cfg(feature = "physics")]

use serde_yaml::Value;
use std::fs;
use suews_bridge::{load_run_config, load_run_config_from_str, load_run_config_from_value};

const SAMPLE_YAML: &str = include_str!("../../../test/fixtures/bridge/sample_resolved.yml");
const STEBBS_YAML: &str = include_str!("../../../test/fixtures/bridge/stebbs_resolved.yml");

fn get_path_mut<'a>(root: &'a mut Value, path: &[&str]) -> &'a mut Value {
    let mut cur = root;
    for &segment in path {
        cur = cur
            .as_mapping_mut()
            .unwrap_or_else(|| panic!("expected mapping before `{segment}`"))
            .get_mut(Value::String(segment.to_string()))
            .unwrap_or_else(|| panic!("missing key `{segment}` in fixture"));
    }
    cur
}

fn remove_key(node: &mut Value, key: &str) {
    let map = node.as_mapping_mut().expect("expected YAML mapping");
    map.remove(Value::String(key.to_string()))
        .unwrap_or_else(|| panic!("expected key `{key}` to exist before removal"));
}

fn set_key(node: &mut Value, key: &str, yaml_literal: &str) {
    let map = node.as_mapping_mut().expect("expected YAML mapping");
    map.insert(
        Value::String(key.to_string()),
        serde_yaml::from_str(yaml_literal).unwrap(),
    );
}

fn first_site_mut(root: &mut Value) -> &mut Value {
    root["sites"]
        .as_sequence_mut()
        .expect("fixture has sites sequence")
        .get_mut(0)
        .expect("fixture has first site")
}

#[test]
fn lumps_missing_fields_are_rejected_across_all_public_readers() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("lumps_missing.yml");
    let mut base: Value = serde_yaml::from_str(SAMPLE_YAML).unwrap();
    base["model"]["control"]["output"]["dir"] = Value::String(dir.path().display().to_string());
    first_site_mut(&mut base)["gridiv"] = Value::from(27);

    for field in ["raincover", "rainmaxres", "drainrt", "veg_type"] {
        let mut mutated = base.clone();
        let lumps = get_path_mut(first_site_mut(&mut mutated), &["properties", "lumps"]);
        remove_key(lumps, field);

        let yaml_text = serde_yaml::to_string(&mutated).unwrap();
        fs::write(&path, &yaml_text).unwrap();

        let expected = format!("`sites[gridiv=27].properties.lumps.{field}` is missing");
        for (reader, res) in [
            ("value", load_run_config_from_value(&mut mutated)),
            ("string", load_run_config_from_str(&yaml_text)),
            ("file", load_run_config(&path)),
        ] {
            let err = res.expect_err(reader);
            assert!(
                err.contains(&expected),
                "{reader} missing {field}: expected `{expected}` in `{err}`"
            );
        }
    }
}

#[test]
fn anthro_snow_retired_leaf_cases_are_rejected_with_canonical_paths() {
    // 1. emissions=0 still requires heat.baset_heating.working_day for DailyState.
    {
        let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).unwrap();
        *get_path_mut(&mut root, &["model", "physics", "emissions"]) =
            serde_yaml::from_str("{value: 0}").unwrap();
        let baset = get_path_mut(
            first_site_mut(&mut root),
            &[
                "properties",
                "anthropogenic_emissions",
                "heat",
                "baset_heating",
            ],
        );
        remove_key(baset, "working_day");
        let err = load_run_config_from_value(&mut root)
            .expect_err("emissions=0 must require baset_heating.working_day");
        assert!(
            err.contains(
                "`sites[gridiv=1].properties.anthropogenic_emissions.heat.baset_heating.working_day`"
            ),
            "{err}"
        );
    }

    // 2. emissions=1 requires heat.ah_slope_heating.holiday.
    {
        let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).unwrap();
        *get_path_mut(&mut root, &["model", "physics", "emissions"]) =
            serde_yaml::from_str("{value: 1}").unwrap();
        let slope = get_path_mut(
            first_site_mut(&mut root),
            &[
                "properties",
                "anthropogenic_emissions",
                "heat",
                "ah_slope_heating",
            ],
        );
        remove_key(slope, "holiday");
        let err = load_run_config_from_value(&mut root)
            .expect_err("emissions=1 must require ah_slope_heating.holiday");
        assert!(
            err.contains(
                "`sites[gridiv=1].properties.anthropogenic_emissions.heat.ah_slope_heating.holiday`"
            ),
            "{err}"
        );
    }

    // 3. emissions=5 requires co2.emission_factor_co2_fuel.
    {
        let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).unwrap();
        *get_path_mut(&mut root, &["model", "physics", "emissions"]) =
            serde_yaml::from_str("{value: 5}").unwrap();
        let co2 = get_path_mut(
            first_site_mut(&mut root),
            &["properties", "anthropogenic_emissions", "co2"],
        );
        remove_key(co2, "emission_factor_co2_fuel");
        let err = load_run_config_from_value(&mut root)
            .expect_err("emissions=5 must require co2.emission_factor_co2_fuel");
        assert!(
            err.contains(
                "`sites[gridiv=1].properties.anthropogenic_emissions.co2.emission_factor_co2_fuel`"
            ),
            "{err}"
        );
    }

    // 4. emissions=2 requires heat.ahprof_24hr.working_day, co2.profile_traffic_24hr,
    //    and non-null hour slots in co2.profile_human_activity_24hr.holiday.
    {
        let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).unwrap();
        let ahprof = get_path_mut(
            first_site_mut(&mut root),
            &[
                "properties",
                "anthropogenic_emissions",
                "heat",
                "ahprof_24hr",
            ],
        );
        remove_key(ahprof, "working_day");
        let err = load_run_config_from_value(&mut root)
            .expect_err("emissions=2 must require ahprof_24hr.working_day");
        assert!(
            err.contains(
                "`sites[gridiv=1].properties.anthropogenic_emissions.heat.ahprof_24hr.working_day`"
            ),
            "{err}"
        );
    }
    {
        let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).unwrap();
        let co2 = get_path_mut(
            first_site_mut(&mut root),
            &["properties", "anthropogenic_emissions", "co2"],
        );
        remove_key(co2, "profile_traffic_24hr");
        let err = load_run_config_from_value(&mut root)
            .expect_err("emissions=2 must require co2.profile_traffic_24hr");
        assert!(
            err.contains(
                "`sites[gridiv=1].properties.anthropogenic_emissions.co2.profile_traffic_24hr`"
            ),
            "{err}"
        );
    }
    {
        let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).unwrap();
        let holiday = get_path_mut(
            first_site_mut(&mut root),
            &[
                "properties",
                "anthropogenic_emissions",
                "co2",
                "profile_human_activity_24hr",
                "holiday",
            ],
        );
        set_key(holiday, "12", "null");
        let err = load_run_config_from_value(&mut root)
            .expect_err("null holiday hour slot in active anthro profile must fail");
        assert!(
            err.contains(
                "`sites[gridiv=1].properties.anthropogenic_emissions.co2.profile_human_activity_24hr.holiday.12`"
            ),
            "{err}"
        );
    }
}

#[test]
fn landcover_and_sitescalar_retired_leaf_cases_are_rejected() {
    // Active grass.max_conductance missing.
    {
        let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).unwrap();
        let grass = get_path_mut(
            first_site_mut(&mut root),
            &["properties", "land_cover", "grass"],
        );
        remove_key(grass, "max_conductance");
        let err = load_run_config_from_value(&mut root)
            .expect_err("missing active grass.max_conductance must fail");
        assert!(
            err.contains("`sites[gridiv=1].properties.land_cover.grass.max_conductance`"),
            "{err}"
        );
    }

    // Malformed inactive narp_trans_site is rejected.
    {
        let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).unwrap();
        let props = get_path_mut(first_site_mut(&mut root), &["properties"]);
        set_key(props, "narp_trans_site", "{value: invalid}");
        let err = load_run_config_from_value(&mut root)
            .expect_err("malformed inactive narp_trans_site must fail");
        assert!(
            err.contains("`sites[gridiv=1].properties.narp_trans_site`"),
            "{err}"
        );
    }
}

#[test]
fn irrigation_retired_leaf_matrix_cases_are_rejected() {
    let mut base: Value = serde_yaml::from_str(SAMPLE_YAML).unwrap();
    *get_path_mut(&mut base, &["model", "physics", "water_use"]) =
        serde_yaml::from_str("{value: 0}").unwrap();
    for day in [
        "monday",
        "tuesday",
        "wednesday",
        "thursday",
        "friday",
        "saturday",
        "sunday",
    ] {
        set_key(
            get_path_mut(
                first_site_mut(&mut base),
                &["properties", "irrigation", "daywat"],
            ),
            day,
            "1.0",
        );
        set_key(
            get_path_mut(
                first_site_mut(&mut base),
                &["properties", "irrigation", "daywatper"],
            ),
            day,
            "1.0",
        );
    }

    let targets: &[(&[&str], &str, &str)] = &[
        (
            &["properties", "land_cover", "grass"],
            "ie_m",
            "sites[gridiv=1].properties.land_cover.grass.ie_m",
        ),
        (
            &["properties", "irrigation", "daywat"],
            "monday",
            "sites[gridiv=1].properties.irrigation.daywat.monday",
        ),
        (
            &["properties", "irrigation", "daywatper"],
            "sunday",
            "sites[gridiv=1].properties.irrigation.daywatper.sunday",
        ),
    ];

    for &(parent, leaf, expected) in targets {
        let mut missing = base.clone();
        remove_key(get_path_mut(first_site_mut(&mut missing), parent), leaf);
        let err = load_run_config_from_value(&mut missing)
            .expect_err(&format!("missing {expected} must fail"));
        assert!(err.contains(expected), "missing {expected}: {err}");

        for bad in ["null", "{value: null}", "{value: .nan}", "\"not-a-number\""] {
            let mut mutated = base.clone();
            set_key(
                get_path_mut(first_site_mut(&mut mutated), parent),
                leaf,
                bad,
            );
            let err = load_run_config_from_value(&mut mutated)
                .expect_err(&format!("{expected}={bad} must fail"));
            assert!(err.contains(expected), "{expected}={bad}: {err}");
        }
    }
}

#[test]
fn spartacus_ehc_and_state_retired_leaf_cases_are_rejected() {
    // 1. Fractional n_stream_sw_urban under active SPARTACUS + EHC.
    {
        let mut root: Value = serde_yaml::from_str(STEBBS_YAML).unwrap();
        *get_path_mut(&mut root, &["model", "physics", "storage_heat"]) =
            serde_yaml::from_str("{value: 5}").unwrap();
        let spartacus = get_path_mut(first_site_mut(&mut root), &["properties", "spartacus"]);
        set_key(spartacus, "n_stream_sw_urban", "1.5");
        let err = load_run_config_from_value(&mut root)
            .expect_err("fractional n_stream_sw_urban must fail");
        assert!(
            err.contains("sites[gridiv=1].properties.spartacus.n_stream_sw_urban"),
            "{err}"
        );
    }

    // 2. Missing walls[0].thermal_layers.k under active SPARTACUS + EHC.
    {
        let mut root: Value = serde_yaml::from_str(STEBBS_YAML).unwrap();
        *get_path_mut(&mut root, &["model", "physics", "storage_heat"]) =
            serde_yaml::from_str("{value: 5}").unwrap();
        let walls = get_path_mut(
            first_site_mut(&mut root),
            &["properties", "vertical_layers", "walls"],
        )
        .as_sequence_mut()
        .unwrap();
        remove_key(get_path_mut(&mut walls[0], &["thermal_layers"]), "k");
        let err = load_run_config_from_value(&mut root)
            .expect_err("missing walls[0].thermal_layers.k must fail");
        assert!(
            err.contains("sites[gridiv=1].properties.vertical_layers.walls[0].thermal_layers.k"),
            "{err}"
        );
    }

    // 3. Non-finite inactive initial_states.snowalb under snow_use=0.
    {
        let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).unwrap();
        let init = get_path_mut(first_site_mut(&mut root), &["initial_states"]);
        set_key(init, "snowalb", ".nan");
        let err = load_run_config_from_value(&mut root)
            .expect_err("non-finite initial_states.snowalb must fail");
        assert!(
            err.contains("sites[gridiv=1].initial_states.snowalb"),
            "{err}"
        );
    }
}
