#![cfg(feature = "physics")]
//! Layer 3 native receiver parity tests for sparse YAML shadow-default fields (gh#2099).
//!
//! Verifies the real `EngineYAML -> RunConfig` receiver (`load_run_config_from_value`)
//! directly on sparse configurations with asymmetric supplied values and omissions across
//! every shadow-default group tracked by `test/data_model/known_df_yaml_parity_gaps.json`:
//!
//! 1. Supplied non-default values arrive verbatim on the expected `SuewsSite` members
//!    without dropping, swapping, or falling back to legacy `df_state` shadow defaults.
//! 2. Omitted active/inactive scalar and array fields land on their real observed native
//!    fallback (`0.0` via `Default::default()`), verified in both asymmetry orientations.
//! 3. EHC vectors (`k`/`cp`/`dz` for `surf`/`roof`/`wall` and `soil_storecap_{roof,wall}`)
//!    allocate to observed `0.0` when `nlayer > 0` (even under OHM `storage_heat=1`) and
//!    receive exact asymmetric values vs `0.0` omissions under EHC (`storage_heat=5`), while
//!    staying empty (`len == 0`) when `vertical_layers` is absent (`nlayer == 0`).
//! 4. `soil_density` is verified unread by the native receiver (toggling it yields
//!    bit-identical `SuewsSite` flat payloads), and the three retired `#2077` soil
//!    parameters are rejected when omitted on active surfaces while receiving `0.0`
//!    when omitted on inactive (`sfr == 0`) surfaces.
// contract: C-PARITY, C-STRICT, C-BIND

use serde_yaml::{Mapping, Value};
use suews_bridge::load_run_config_from_value;

const SAMPLE_YAML: &str = include_str!("../../../test/fixtures/bridge/sample_resolved.yml");

fn assert_close(actual: f64, expected: f64, label: &str) {
    assert!(
        actual.is_finite() && (actual - expected).abs() <= 1.0e-12,
        "{label}: expected {expected}, got {actual}"
    );
}

fn assert_slice_close(actual: &[f64], expected: &[f64], label: &str) {
    assert_eq!(
        actual.len(),
        expected.len(),
        "{label}: length mismatch (expected {}, got {})",
        expected.len(),
        actual.len()
    );
    for (idx, (&a, &e)) in actual.iter().zip(expected.iter()).enumerate() {
        assert_close(a, e, &format!("{label}[{idx}]"));
    }
}

fn mapping_at_mut<'a>(root: &'a mut Value, path: &[&str]) -> &'a mut Mapping {
    let mut cur = root;
    for &seg in path {
        cur = match cur {
            Value::Mapping(map) => map
                .get_mut(Value::String(seg.to_string()))
                .unwrap_or_else(|| panic!("missing mapping key {seg:?} in path {path:?}")),
            _ => panic!("expected mapping before {seg:?} in path {path:?}"),
        };
    }
    cur.as_mapping_mut()
        .unwrap_or_else(|| panic!("target at {path:?} is not a mapping"))
}

fn first_site_mut(root: &mut Value) -> &mut Value {
    root.as_mapping_mut()
        .expect("root mapping")
        .get_mut(Value::String("sites".to_string()))
        .expect("sites sequence")
        .as_sequence_mut()
        .expect("sites seq")
        .first_mut()
        .expect("sites[0]")
}

fn set_wrapped_f64(map: &mut Mapping, key: &str, value: f64) {
    let mut wrapped = Mapping::new();
    wrapped.insert(Value::String("value".to_string()), Value::from(value));
    map.insert(Value::String(key.to_string()), Value::Mapping(wrapped));
}

fn set_wrapped_f64_vec(map: &mut Mapping, key: &str, values: &[f64]) {
    let mut wrapped = Mapping::new();
    wrapped.insert(
        Value::String("value".to_string()),
        Value::Sequence(values.iter().copied().map(Value::from).collect()),
    );
    map.insert(Value::String(key.to_string()), Value::Mapping(wrapped));
}

fn remove_key(map: &mut Mapping, key: &str) {
    map.remove(Value::String(key.to_string()));
}

fn strip_veg_shadow_defaults(lc: &mut Mapping, surface: &str) {
    let surf = mapping_at_mut(
        lc.get_mut(Value::String(surface.to_string()))
            .expect(surface),
        &[],
    );
    for key in [
        "alpha_bio_co2",
        "beta_bio_co2",
        "theta_bio_co2",
        "resp_a",
        "resp_b",
        "height_evergreen_tree",
        "fai_evergreen_tree",
        "height_deciduous_tree",
        "fai_deciduous_tree",
    ] {
        remove_key(surf, key);
    }
    if let Some(lai_val) = surf.get_mut(Value::String("lai".to_string())) {
        let lai = lai_val.as_mapping_mut().expect("lai mapping");
        for key in [
            "base_temperature",
            "base_temperature_senescence",
            "gdd_full",
            "sdd_full",
            "lai_max",
            "lai_power",
        ] {
            remove_key(lai, key);
        }
    }
}

fn set_veg_shadow_values(
    lc: &mut Mapping,
    surface: &str,
    height_key: Option<(&str, f64)>,
    fai_key: Option<(&str, f64)>,
    bioco2: [f64; 5],
    lai_scalars: [f64; 5],
    lai_power: [f64; 4],
) {
    let surf = mapping_at_mut(
        lc.get_mut(Value::String(surface.to_string()))
            .expect(surface),
        &[],
    );
    set_wrapped_f64(surf, "alpha_bio_co2", bioco2[0]);
    set_wrapped_f64(surf, "beta_bio_co2", bioco2[1]);
    set_wrapped_f64(surf, "theta_bio_co2", bioco2[2]);
    set_wrapped_f64(surf, "resp_a", bioco2[3]);
    set_wrapped_f64(surf, "resp_b", bioco2[4]);
    if let Some((k, v)) = height_key {
        set_wrapped_f64(surf, k, v);
    }
    if let Some((k, v)) = fai_key {
        set_wrapped_f64(surf, k, v);
    }
    let lai = surf
        .get_mut(Value::String("lai".to_string()))
        .expect("lai")
        .as_mapping_mut()
        .expect("lai mapping");
    set_wrapped_f64(lai, "base_temperature", lai_scalars[0]);
    set_wrapped_f64(lai, "base_temperature_senescence", lai_scalars[1]);
    set_wrapped_f64(lai, "gdd_full", lai_scalars[2]);
    set_wrapped_f64(lai, "sdd_full", lai_scalars[3]);
    set_wrapped_f64(lai, "lai_max", lai_scalars[4]);

    let mut lp = Mapping::new();
    set_wrapped_f64(&mut lp, "growth_lai", lai_power[0]);
    set_wrapped_f64(&mut lp, "growth_gdd", lai_power[1]);
    set_wrapped_f64(&mut lp, "senescence_lai", lai_power[2]);
    set_wrapped_f64(&mut lp, "senescence_sdd", lai_power[3]);
    lai.insert(Value::String("lai_power".to_string()), Value::Mapping(lp));
}

fn build_sparse_ohm_asymmetric_yaml() -> Value {
    let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).expect("parse sample_resolved.yml");
    let site = first_site_mut(&mut root);
    let props = mapping_at_mut(site, &["properties"]);

    // Snow: supply preciplimit asymmetrically, omit snowdensmin & snowdensmax.
    {
        let snow = props
            .get_mut(Value::String("snow".to_string()))
            .expect("snow")
            .as_mapping_mut()
            .expect("snow mapping");
        set_wrapped_f64(snow, "temperature_rain_snow_threshold", 1.75);
        remove_key(snow, "snow_density_min");
        remove_key(snow, "snow_density_max");
    }

    // Vertical layers: omit roof/wall thermal_layers & soil_store_capacity.
    {
        let vl = props
            .get_mut(Value::String("vertical_layers".to_string()))
            .expect("vertical_layers")
            .as_mapping_mut()
            .expect("vertical_layers mapping");
        for key in ["roofs", "walls"] {
            if let Some(Value::Sequence(items)) = vl.get_mut(Value::String(key.to_string())) {
                for item in items.iter_mut() {
                    if let Some(m) = item.as_mapping_mut() {
                        remove_key(m, "thermal_layers");
                        remove_key(m, "soil_store_capacity");
                    }
                }
            }
        }
    }

    // Land cover: supply evetr + bldgs + active paved soil asymmetrically;
    // omit dectr/grass LAI+bioCO2+tree geometry, inactive bsoil soil, and all thermal_layers.
    let lc = props
        .get_mut(Value::String("land_cover".to_string()))
        .expect("land_cover")
        .as_mapping_mut()
        .expect("land_cover mapping");

    for surf_name in [
        "paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water",
    ] {
        if let Some(Value::Mapping(surf)) = lc.get_mut(Value::String(surf_name.to_string())) {
            remove_key(surf, "thermal_layers");
            remove_key(surf, "soil_density");
        }
    }

    // Active paved: distinct soil params + non-zero soil_density (unread by native).
    {
        let paved = lc
            .get_mut(Value::String("paved".to_string()))
            .expect("paved")
            .as_mapping_mut()
            .expect("paved map");
        set_wrapped_f64(paved, "sfr", 0.45);
        set_wrapped_f64(paved, "soil_depth", 420.0);
        set_wrapped_f64(paved, "soil_store_capacity", 185.0);
        set_wrapped_f64(paved, "saturated_hydraulic_conductivity", 0.0025);
        set_wrapped_f64(paved, "soil_density", 1650.0);
    }

    // Active bldgs: distinct bldgh / faibldg.
    {
        let bldgs = lc
            .get_mut(Value::String("bldgs".to_string()))
            .expect("bldgs")
            .as_mapping_mut()
            .expect("bldgs map");
        set_wrapped_f64(bldgs, "sfr", 0.35);
        set_wrapped_f64(bldgs, "bldgh", 22.5);
        set_wrapped_f64(bldgs, "faibldg", 0.41);
    }

    // Active evetr: supply all 6 LAI + 5 bioCO2 + height + FAI asymmetrically.
    {
        let evetr = lc
            .get_mut(Value::String("evetr".to_string()))
            .expect("evetr")
            .as_mapping_mut()
            .expect("evetr map");
        set_wrapped_f64(evetr, "sfr", 0.20);
    }
    set_veg_shadow_values(
        lc,
        "evetr",
        Some(("height_evergreen_tree", 18.5)),
        Some(("fai_evergreen_tree", 0.37)),
        [0.73, 0.52, 1.17, 0.88, 1.24],
        [6.5, 11.5, 240.0, -380.0, 5.4],
        [0.21, 0.22, 0.23, 0.24],
    );

    // Inactive dectr/grass/bsoil/water (sum sfr == 1.0): omit shadow-default groups.
    for surf_name in ["dectr", "grass", "bsoil", "water"] {
        let surf = lc
            .get_mut(Value::String(surf_name.to_string()))
            .expect(surf_name)
            .as_mapping_mut()
            .expect("surf map");
        set_wrapped_f64(surf, "sfr", 0.0);
    }
    strip_veg_shadow_defaults(lc, "dectr");
    strip_veg_shadow_defaults(lc, "grass");
    {
        let bsoil = lc
            .get_mut(Value::String("bsoil".to_string()))
            .expect("bsoil")
            .as_mapping_mut()
            .expect("bsoil map");
        remove_key(bsoil, "soil_depth");
        remove_key(bsoil, "soil_store_capacity");
        remove_key(bsoil, "saturated_hydraulic_conductivity");
    }

    root
}

#[test]
fn test_sparse_asymmetric_ohm_receiver_parity() {
    let mut root_with_density = build_sparse_ohm_asymmetric_yaml();
    let mut root_without_density = root_with_density.clone();
    {
        let site = first_site_mut(&mut root_without_density);
        let paved = mapping_at_mut(site, &["properties", "land_cover", "paved"]);
        remove_key(paved, "soil_density");
    }

    let cfg = load_run_config_from_value(&mut root_with_density)
        .expect("sparse OHM asymmetric YAML must load cleanly");
    let cfg_no_density = load_run_config_from_value(&mut root_without_density)
        .expect("sparse OHM YAML without soil_density must load cleanly");
    let site = &cfg.site;

    // 1. Supplied evetr values arrive verbatim on real SuewsSite receiver.
    assert_close(
        site.lc_evetr.bioco2.alpha_bio_co2,
        0.73,
        "evetr.alpha_bio_co2",
    );
    assert_close(
        site.lc_evetr.bioco2.beta_bio_co2,
        0.52,
        "evetr.beta_bio_co2",
    );
    assert_close(
        site.lc_evetr.bioco2.theta_bio_co2,
        1.17,
        "evetr.theta_bio_co2",
    );
    assert_close(site.lc_evetr.bioco2.resp_a, 0.88, "evetr.resp_a");
    assert_close(site.lc_evetr.bioco2.resp_b, 1.24, "evetr.resp_b");
    assert_close(site.lc_evetr.lai.base_temperature, 6.5, "evetr.baset");
    assert_close(
        site.lc_evetr.lai.base_temperature_senescence,
        11.5,
        "evetr.basete",
    );
    assert_close(site.lc_evetr.lai.gdd_full, 240.0, "evetr.gddfull");
    assert_close(site.lc_evetr.lai.sdd_full, -380.0, "evetr.sddfull");
    assert_close(site.lc_evetr.lai.lai_max, 5.4, "evetr.laimax");
    assert_slice_close(
        &site.lc_evetr.lai.lai_power,
        &[0.21, 0.22, 0.23, 0.24],
        "evetr.laipower",
    );
    assert_close(site.lc_evetr.height_evergreen_tree, 18.5, "evetr.evetreeh");
    assert_close(site.lc_evetr.fai_evergreen_tree, 0.37, "evetr.faievetree");

    // 2. Supplied bldgs, active paved soil, and snow.preciplimit arrive verbatim.
    assert_close(site.lc_bldg.bldgh, 22.5, "bldgs.bldgh");
    assert_close(site.lc_bldg.faibldg, 0.41, "bldgs.faibldg");
    assert_close(site.lc_paved.soil.soil_depth, 420.0, "paved.soil_depth");
    assert_close(
        site.lc_paved.soil.soil_store_capacity,
        185.0,
        "paved.soil_store_capacity",
    );
    assert_close(
        site.lc_paved.soil.saturated_hydraulic_conductivity,
        0.0025,
        "paved.saturated_hydraulic_conductivity",
    );
    assert_close(site.snow.precip_limit, 1.75, "snow.preciplimit");

    // 3. Omitted dectr, grass, snow density, and inactive bsoil soil receive observed 0.0
    //    (NOT the df_state shadow defaults 0.8/0.6/1.2/1.0/1.1/10.0/100.0/0.1/15.0/100/400/150).
    for (label, bioco2, lai) in [
        ("dectr", &site.lc_dectr.bioco2, &site.lc_dectr.lai),
        ("grass", &site.lc_grass.bioco2, &site.lc_grass.lai),
    ] {
        assert_close(bioco2.alpha_bio_co2, 0.0, &format!("{label}.alpha_bio_co2"));
        assert_close(bioco2.beta_bio_co2, 0.0, &format!("{label}.beta_bio_co2"));
        assert_close(bioco2.theta_bio_co2, 0.0, &format!("{label}.theta_bio_co2"));
        assert_close(bioco2.resp_a, 0.0, &format!("{label}.resp_a"));
        assert_close(bioco2.resp_b, 0.0, &format!("{label}.resp_b"));
        assert_close(lai.base_temperature, 0.0, &format!("{label}.baset"));
        assert_close(
            lai.base_temperature_senescence,
            0.0,
            &format!("{label}.basete"),
        );
        assert_close(lai.gdd_full, 0.0, &format!("{label}.gddfull"));
        assert_close(lai.sdd_full, 0.0, &format!("{label}.sddfull"));
        assert_close(lai.lai_max, 0.0, &format!("{label}.laimax"));
        assert_slice_close(
            &lai.lai_power,
            &[0.0, 0.0, 0.0, 0.0],
            &format!("{label}.laipower"),
        );
    }
    assert_close(site.lc_dectr.height_deciduous_tree, 0.0, "dectr.dectreeh");
    assert_close(site.lc_dectr.fai_deciduous_tree, 0.0, "dectr.faidectree");
    assert_close(site.snow.snow_density_min, 0.0, "snow.snowdensmin");
    assert_close(site.snow.snow_density_max, 0.0, "snow.snowdensmax");
    assert_close(site.lc_bsoil.soil.soil_depth, 0.0, "bsoil.soil_depth");
    assert_close(
        site.lc_bsoil.soil.soil_store_capacity,
        0.0,
        "bsoil.soil_store_capacity",
    );
    assert_close(
        site.lc_bsoil.soil.saturated_hydraulic_conductivity,
        0.0,
        "bsoil.saturated_hydraulic_conductivity",
    );

    // 4. EHC vectors when nlayer > 0 (OHM storage_heat=1, omitted thermal_layers/soilstorecap):
    //    real receiver allocates nlayer*ndepth / NSURF*ndepth / nlayer filled with 0.0
    //    (NOT df_state shadow defaults 1.0 / 1000.0 / [0.1..0.5] / 150.0).
    let nlayer = site.ehc.nlayer;
    let ndepth = site.ehc.ndepth;
    assert!(
        nlayer > 0 && ndepth == 5,
        "expected allocated EHC dims when vertical_layers present"
    );
    assert_slice_close(
        &site.ehc.k_surf,
        &vec![0.0; 7 * ndepth],
        "ehc.k_surf omitted (OHM)",
    );
    assert_slice_close(
        &site.ehc.cp_surf,
        &vec![0.0; 7 * ndepth],
        "ehc.cp_surf omitted (OHM)",
    );
    assert_slice_close(
        &site.ehc.dz_surf,
        &vec![0.0; 7 * ndepth],
        "ehc.dz_surf omitted (OHM)",
    );
    assert_slice_close(
        &site.ehc.k_roof,
        &vec![0.0; nlayer * ndepth],
        "ehc.k_roof omitted (OHM)",
    );
    assert_slice_close(
        &site.ehc.cp_roof,
        &vec![0.0; nlayer * ndepth],
        "ehc.cp_roof omitted (OHM)",
    );
    assert_slice_close(
        &site.ehc.dz_roof,
        &vec![0.0; nlayer * ndepth],
        "ehc.dz_roof omitted (OHM)",
    );
    assert_slice_close(
        &site.ehc.k_wall,
        &vec![0.0; nlayer * ndepth],
        "ehc.k_wall omitted (OHM)",
    );
    assert_slice_close(
        &site.ehc.cp_wall,
        &vec![0.0; nlayer * ndepth],
        "ehc.cp_wall omitted (OHM)",
    );
    assert_slice_close(
        &site.ehc.dz_wall,
        &vec![0.0; nlayer * ndepth],
        "ehc.dz_wall omitted (OHM)",
    );
    assert_slice_close(
        &site.ehc.soil_storecap_roof,
        &vec![0.0; nlayer],
        "ehc.soil_storecap_roof omitted (OHM)",
    );
    assert_slice_close(
        &site.ehc.soil_storecap_wall,
        &vec![0.0; nlayer],
        "ehc.soil_storecap_wall omitted (OHM)",
    );

    // 4b. When vertical_layers.nlayer == 0, EHC vectors stay empty (inapplicable).
    let mut root_nlayer0 = build_sparse_ohm_asymmetric_yaml();
    {
        let site_val = first_site_mut(&mut root_nlayer0);
        let vl = mapping_at_mut(site_val, &["properties", "vertical_layers"]);
        set_wrapped_f64(vl, "nlayer", 0.0);
    }
    let cfg_nlayer0 = load_run_config_from_value(&mut root_nlayer0)
        .expect("sparse OHM YAML with nlayer=0 must load cleanly");
    assert_eq!(cfg_nlayer0.site.ehc.nlayer, 0);
    assert!(cfg_nlayer0.site.ehc.k_surf.is_empty());
    assert!(cfg_nlayer0.site.ehc.cp_surf.is_empty());
    assert!(cfg_nlayer0.site.ehc.dz_surf.is_empty());
    assert!(cfg_nlayer0.site.ehc.k_roof.is_empty());
    assert!(cfg_nlayer0.site.ehc.k_wall.is_empty());
    assert!(cfg_nlayer0.site.ehc.soil_storecap_roof.is_empty());
    assert!(cfg_nlayer0.site.ehc.soil_storecap_wall.is_empty());

    // 5. soil_density is legitimately unread by the native receiver.
    assert_eq!(
        cfg.site_scalars, cfg_no_density.site_scalars,
        "soil_density must not alter native site scalars"
    );
    assert_eq!(
        cfg.site, cfg_no_density.site,
        "soil_density must not alter native SuewsSite members"
    );
}

#[test]
fn test_sparse_asymmetric_ehc_flipped_receiver_parity() {
    let mut root: Value = serde_yaml::from_str(SAMPLE_YAML).expect("parse sample_resolved.yml");
    {
        let phys = mapping_at_mut(&mut root, &["model", "physics"]);
        set_wrapped_f64(phys, "storage_heat", 5.0);
        set_wrapped_f64(phys, "net_radiation", 1003.0);
    }

    let site = first_site_mut(&mut root);
    let props = mapping_at_mut(site, &["properties"]);

    // Snow flipped: omit preciplimit, supply snowdensmin & snowdensmax.
    {
        let snow = props
            .get_mut(Value::String("snow".to_string()))
            .expect("snow")
            .as_mapping_mut()
            .expect("snow map");
        remove_key(snow, "temperature_rain_snow_threshold");
        set_wrapped_f64(snow, "snow_density_min", 115.0);
        set_wrapped_f64(snow, "snow_density_max", 385.0);
    }

    // Vertical layers: nlayer=1, supply roofs[0] thermal_layers + soil_store_capacity,
    // omit walls[0] thermal_layers + soil_store_capacity.
    let roof_k = [0.41, 0.42, 0.43, 0.44, 0.45];
    let roof_cp = [1.11e6, 1.12e6, 1.13e6, 1.14e6, 1.15e6];
    let roof_dz = [0.05, 0.06, 0.07, 0.08, 0.09];
    {
        let vl = props
            .get_mut(Value::String("vertical_layers".to_string()))
            .expect("vertical_layers")
            .as_mapping_mut()
            .expect("vl map");
        set_wrapped_f64(vl, "nlayer", 1.0);
        for key in [
            "height",
            "veg_frac",
            "veg_scale",
            "building_frac",
            "building_scale",
        ] {
            if let Some(Value::Mapping(m)) = vl.get_mut(Value::String(key.to_string())) {
                if let Some(Value::Sequence(seq)) = m.get_mut(Value::String("value".to_string())) {
                    let target = if key == "height" { 2 } else { 1 };
                    seq.truncate(target);
                }
            }
        }
        if let Some(Value::Sequence(roofs)) = vl.get_mut(Value::String("roofs".to_string())) {
            roofs.truncate(1);
            let r0 = roofs[0].as_mapping_mut().expect("roofs[0]");
            set_wrapped_f64(r0, "soil_store_capacity", 172.5);
            let mut tl = Mapping::new();
            set_wrapped_f64_vec(&mut tl, "k", &roof_k);
            set_wrapped_f64_vec(&mut tl, "rho_cp", &roof_cp);
            set_wrapped_f64_vec(&mut tl, "dz", &roof_dz);
            r0.insert(
                Value::String("thermal_layers".to_string()),
                Value::Mapping(tl),
            );
        }
        if let Some(Value::Sequence(walls)) = vl.get_mut(Value::String("walls".to_string())) {
            walls.truncate(1);
        }
    }

    // Truncate initial_states.{roofs,walls} to match nlayer=1.
    if let Some(Value::Mapping(init)) = site
        .as_mapping_mut()
        .expect("site")
        .get_mut(Value::String("initial_states".to_string()))
    {
        for key in ["roofs", "walls"] {
            if let Some(Value::Sequence(seq)) = init.get_mut(Value::String(key.to_string())) {
                seq.truncate(1);
            }
        }
    }

    // Land cover flipped: supply dectr LAI+bioCO2+height+FAI and asymmetric paved thermal_layers,
    // omit evetr/grass LAI+bioCO2+height+FAI.
    let paved_k = [0.81, 0.82, 0.83, 0.84, 0.85];
    let paved_cp = [1.51e6, 1.52e6, 1.53e6, 1.54e6, 1.55e6];
    let paved_dz = [0.11, 0.12, 0.13, 0.14, 0.15];
    {
        let lc = mapping_at_mut(site, &["properties", "land_cover"]);
        let paved = lc
            .get_mut(Value::String("paved".to_string()))
            .expect("paved")
            .as_mapping_mut()
            .expect("paved map");
        let mut tl = Mapping::new();
        set_wrapped_f64_vec(&mut tl, "k", &paved_k);
        set_wrapped_f64_vec(&mut tl, "rho_cp", &paved_cp);
        set_wrapped_f64_vec(&mut tl, "dz", &paved_dz);
        paved.insert(
            Value::String("thermal_layers".to_string()),
            Value::Mapping(tl),
        );

        set_veg_shadow_values(
            lc,
            "dectr",
            Some(("height_deciduous_tree", 16.5)),
            Some(("fai_deciduous_tree", 0.29)),
            [0.64, 0.47, 1.31, 0.92, 1.18],
            [7.5, 12.5, 290.0, -410.0, 4.8],
            [0.31, 0.32, 0.33, 0.34],
        );
        strip_veg_shadow_defaults(lc, "evetr");
        // Grass is active in this fixture, and active vegetation must keep a positive
        // lai_max (#2149), so restore the sample's value after stripping; the omitted-LAI
        // receiver path is still exercised on inactive evetr.
        let grass_lai_max = lc["grass"]["lai"]["lai_max"].clone();
        strip_veg_shadow_defaults(lc, "grass");
        mapping_at_mut(
            lc.get_mut(Value::String("grass".to_string()))
                .expect("grass"),
            &["lai"],
        )
        .insert(Value::String("lai_max".to_string()), grass_lai_max);
    }

    // Active facet-resolved EHC (#2002): omitting surface or facet thermal_layers
    // or facet soil_store_capacity must be rejected instead of falling back to 0.0.
    {
        let mut missing_bsoil = root.clone();
        let bsoil = mapping_at_mut(
            first_site_mut(&mut missing_bsoil),
            &["properties", "land_cover", "bsoil"],
        );
        remove_key(bsoil, "thermal_layers");
        let err = load_run_config_from_value(&mut missing_bsoil)
            .expect_err("active EHC omitting bsoil thermal_layers must be rejected");
        assert!(
            err.contains("land_cover.bsoil.thermal_layers.k"),
            "unexpected error for omitted EHC bsoil thermal_layers: {err}"
        );
    }
    {
        let mut missing_wall_tl = root.clone();
        let vl = mapping_at_mut(
            first_site_mut(&mut missing_wall_tl),
            &["properties", "vertical_layers"],
        );
        let w0 = vl
            .get_mut(Value::String("walls".to_string()))
            .and_then(Value::as_sequence_mut)
            .expect("walls")[0]
            .as_mapping_mut()
            .expect("walls[0]");
        remove_key(w0, "thermal_layers");
        let err = load_run_config_from_value(&mut missing_wall_tl)
            .expect_err("facet-resolved EHC omitting walls[0].thermal_layers must be rejected");
        assert!(
            err.contains("vertical_layers.walls[0].thermal_layers.k"),
            "unexpected error for omitted EHC walls[0].thermal_layers: {err}"
        );
    }
    {
        let mut missing_wall_cap = root.clone();
        let vl = mapping_at_mut(
            first_site_mut(&mut missing_wall_cap),
            &["properties", "vertical_layers"],
        );
        let w0 = vl
            .get_mut(Value::String("walls".to_string()))
            .and_then(Value::as_sequence_mut)
            .expect("walls")[0]
            .as_mapping_mut()
            .expect("walls[0]");
        remove_key(w0, "soil_store_capacity");
        let err = load_run_config_from_value(&mut missing_wall_cap)
            .expect_err("facet-resolved EHC omitting walls[0].soil_store_capacity must be rejected");
        assert!(
            err.contains("vertical_layers.walls[0].soil_store_capacity"),
            "unexpected error for omitted EHC walls[0].soil_store_capacity: {err}"
        );
    }

    // Lumped EHC (storage_heat=5, net_radiation=3): inactive facet thermal_layers
    // and soil_store_capacity remain optional and land on observed 0.0.
    let mut lumped_root = root.clone();
    {
        let phys = mapping_at_mut(&mut lumped_root, &["model", "physics"]);
        set_wrapped_f64(phys, "net_radiation", 3.0);
        let vl = mapping_at_mut(
            first_site_mut(&mut lumped_root),
            &["properties", "vertical_layers"],
        );
        let w0 = vl
            .get_mut(Value::String("walls".to_string()))
            .and_then(Value::as_sequence_mut)
            .expect("walls")[0]
            .as_mapping_mut()
            .expect("walls[0]");
        remove_key(w0, "thermal_layers");
        remove_key(w0, "soil_store_capacity");
    }
    let lumped_cfg = load_run_config_from_value(&mut lumped_root)
        .expect("lumped EHC omitting inactive walls[0] parameters must load cleanly");

    let cfg = load_run_config_from_value(&mut root)
        .expect("sparse EHC flipped asymmetric YAML must load cleanly");
    let received = &cfg.site;

    // Flipped dectr supplied values arrive verbatim; evetr omitted values receive 0.0.
    assert_close(
        received.lc_dectr.bioco2.alpha_bio_co2,
        0.64,
        "dectr.alpha_bio_co2",
    );
    assert_close(
        received.lc_dectr.bioco2.beta_bio_co2,
        0.47,
        "dectr.beta_bio_co2",
    );
    assert_close(
        received.lc_dectr.bioco2.theta_bio_co2,
        1.31,
        "dectr.theta_bio_co2",
    );
    assert_close(received.lc_dectr.bioco2.resp_a, 0.92, "dectr.resp_a");
    assert_close(received.lc_dectr.bioco2.resp_b, 1.18, "dectr.resp_b");
    assert_close(received.lc_dectr.lai.base_temperature, 7.5, "dectr.baset");
    assert_close(
        received.lc_dectr.lai.base_temperature_senescence,
        12.5,
        "dectr.basete",
    );
    assert_close(received.lc_dectr.lai.gdd_full, 290.0, "dectr.gddfull");
    assert_close(received.lc_dectr.lai.sdd_full, -410.0, "dectr.sddfull");
    assert_close(received.lc_dectr.lai.lai_max, 4.8, "dectr.laimax");
    assert_slice_close(
        &received.lc_dectr.lai.lai_power,
        &[0.31, 0.32, 0.33, 0.34],
        "dectr.laipower",
    );
    assert_close(
        received.lc_dectr.height_deciduous_tree,
        16.5,
        "dectr.dectreeh",
    );
    assert_close(
        received.lc_dectr.fai_deciduous_tree,
        0.29,
        "dectr.faidectree",
    );

    assert_close(
        received.lc_evetr.bioco2.alpha_bio_co2,
        0.0,
        "evetr.alpha_bio_co2 omitted",
    );
    assert_close(received.lc_evetr.lai.lai_max, 0.0, "evetr.laimax omitted");
    assert_slice_close(
        &received.lc_evetr.lai.lai_power,
        &[0.0; 4],
        "evetr.laipower omitted",
    );
    assert_close(
        received.lc_evetr.height_evergreen_tree,
        0.0,
        "evetr.evetreeh omitted",
    );
    assert_close(
        received.lc_evetr.fai_evergreen_tree,
        0.0,
        "evetr.faievetree omitted",
    );

    // Flipped snow: snowdensmin/max arrive verbatim; omitted preciplimit receives 0.0.
    assert_close(received.snow.precip_limit, 0.0, "snow.preciplimit omitted");
    assert_close(
        received.snow.snow_density_min,
        115.0,
        "snow.snowdensmin supplied",
    );
    assert_close(
        received.snow.snow_density_max,
        385.0,
        "snow.snowdensmax supplied",
    );

    // Active EHC: paved (surface idx 0) & roofs[0] arrive verbatim; under lumped EHC
    // (facet_read == false), omitted walls[0] receives observed 0.0 (not shadow defaults).
    let ndepth = received.ehc.ndepth;
    assert_eq!(ndepth, 5, "expected 5 thermal depth layers under EHC");
    assert_slice_close(
        &received.ehc.k_surf[0..ndepth],
        &paved_k,
        "ehc.k_surf[paved]",
    );
    assert_slice_close(
        &received.ehc.cp_surf[0..ndepth],
        &paved_cp,
        "ehc.cp_surf[paved]",
    );
    assert_slice_close(
        &received.ehc.dz_surf[0..ndepth],
        &paved_dz,
        "ehc.dz_surf[paved]",
    );

    assert_slice_close(&received.ehc.k_roof[0..ndepth], &roof_k, "ehc.k_roof[0]");
    assert_slice_close(&received.ehc.cp_roof[0..ndepth], &roof_cp, "ehc.cp_roof[0]");
    assert_slice_close(&received.ehc.dz_roof[0..ndepth], &roof_dz, "ehc.dz_roof[0]");
    assert_close(
        received.ehc.soil_storecap_roof[0],
        172.5,
        "ehc.soil_storecap_roof[0]",
    );

    let lumped_site = &lumped_cfg.site;
    assert_slice_close(
        &lumped_site.ehc.k_wall[0..ndepth],
        &[0.0; 5],
        "lumped ehc.k_wall[0] omitted",
    );
    assert_slice_close(
        &lumped_site.ehc.cp_wall[0..ndepth],
        &[0.0; 5],
        "lumped ehc.cp_wall[0] omitted",
    );
    assert_slice_close(
        &lumped_site.ehc.dz_wall[0..ndepth],
        &[0.0; 5],
        "lumped ehc.dz_wall[0] omitted",
    );
    assert_close(
        lumped_site.ehc.soil_storecap_wall[0],
        0.0,
        "lumped ehc.soil_storecap_wall[0] omitted",
    );
}

#[test]
fn test_sparse_active_soil_rejected_vs_inactive_zero_on_native_receiver() {
    for missing_field in [
        "soil_depth",
        "soil_store_capacity",
        "saturated_hydraulic_conductivity",
    ] {
        let mut root = build_sparse_ohm_asymmetric_yaml();
        {
            let site = first_site_mut(&mut root);
            let paved = mapping_at_mut(site, &["properties", "land_cover", "paved"]);
            remove_key(paved, missing_field);
        }
        let err = load_run_config_from_value(&mut root).expect_err(&format!(
            "active paved omitting {missing_field} must be rejected by native receiver"
        ));
        assert!(
            err.contains(missing_field) || err.contains("soil"),
            "unexpected native error for missing active {missing_field}: {err}"
        );
    }
}
