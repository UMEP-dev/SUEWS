use crate::building_archetype_prm::{
    building_archetype_prm_from_map, building_archetype_prm_to_map,
};
use crate::config::{suews_config_default_from_fortran, SuewsConfig};
use crate::core::NSURF;
use crate::sim::SiteScalars;
use crate::stebbs_prm::{stebbs_prm_from_map, stebbs_prm_to_map};
use crate::suews_site::{suews_site_default_from_fortran, SuewsSite};
use crate::suews_state::{suews_state_default_from_fortran, SuewsState};
use crate::timer::{suews_timer_default_from_fortran, SuewsTimer};
use serde_yaml::Value;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct RunConfig {
    pub timer: SuewsTimer,
    pub config: SuewsConfig,
    pub site: SuewsSite,
    pub site_scalars: SiteScalars,
    pub state: SuewsState,
    pub forcing_path: PathBuf,
    pub output_dir: PathBuf,
    pub nlayer: i32,
    pub ndepth: i32,
}

const SURFACE_YAML_ORDER: [(&str, usize); 7] = [
    ("paved", 0),
    ("bldgs", 1),
    ("evetr", 2),
    ("dectr", 3),
    ("grass", 4),
    ("bsoil", 5),
    ("water", 6),
];

const VEG_SURFACE_YAML_ORDER: [(&str, usize); 3] = [("evetr", 0), ("dectr", 1), ("grass", 2)];

const OHM_SEASONS: [&str; 4] = ["summer_dry", "summer_wet", "winter_dry", "winter_wet"];

fn get_path<'a>(root: &'a Value, path: &[&str]) -> Option<&'a Value> {
    let mut current = root;
    for key in path {
        match current {
            Value::Mapping(map) => {
                current = map.get(Value::String((*key).to_string()))?;
            }
            _ => return None,
        }
    }
    Some(current)
}

fn read_numeric(root: &Value, path: &[&str]) -> Option<f64> {
    let value = get_path(root, path)?;
    match value {
        Value::Number(n) => n.as_f64(),
        Value::Mapping(_) => get_path(value, &["value"]).and_then(|v| match v {
            Value::Number(n) => n.as_f64(),
            _ => None,
        }),
        _ => None,
    }
}

fn read_i32(root: &Value, path: &[&str]) -> Option<i32> {
    read_numeric(root, path).and_then(|v| {
        if v.is_finite() {
            let rounded = v.round();
            if (rounded - v).abs() < 1.0e-9
                && rounded >= i32::MIN as f64
                && rounded <= i32::MAX as f64
            {
                return Some(rounded as i32);
            }
        }
        None
    })
}

fn read_string(root: &Value, path: &[&str]) -> Option<String> {
    let value = get_path(root, path)?;
    match value {
        Value::String(s) => Some(s.clone()),
        Value::Mapping(_) => get_path(value, &["value"]).and_then(|v| match v {
            Value::String(s) => Some(s.clone()),
            _ => None,
        }),
        _ => None,
    }
}

fn parse_numeric_sequence(value: &Value) -> Option<Vec<f64>> {
    match value {
        Value::Number(n) => n.as_f64().map(|v| vec![v]),
        Value::Sequence(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                let num = match item {
                    Value::Number(n) => n.as_f64()?,
                    _ => return None,
                };
                out.push(num);
            }
            Some(out)
        }
        Value::Mapping(_) => get_path(value, &["value"]).and_then(parse_numeric_sequence),
        _ => None,
    }
}

fn read_numeric_sequence(root: &Value, path: &[&str]) -> Option<Vec<f64>> {
    parse_numeric_sequence(get_path(root, path)?)
}

fn read_sequence<'a>(root: &'a Value, path: &[&str]) -> Option<&'a [Value]> {
    let value = get_path(root, path)?;
    match value {
        Value::Sequence(items) => Some(items.as_slice()),
        Value::Mapping(_) => match get_path(value, &["value"])? {
            Value::Sequence(items) => Some(items.as_slice()),
            _ => None,
        },
        _ => None,
    }
}

fn read_numeric_value(value: &Value) -> Option<f64> {
    match value {
        Value::Number(n) => n.as_f64(),
        Value::Mapping(_) => get_path(value, &["value"]).and_then(read_numeric_value),
        _ => None,
    }
}

fn normalise_field_name(field: &str) -> String {
    let mut out = String::with_capacity(field.len() + 8);
    let mut prev_was_lower_or_digit = false;

    for ch in field.chars() {
        if ch.is_ascii_alphanumeric() {
            if ch.is_ascii_uppercase() {
                if prev_was_lower_or_digit && !out.ends_with('_') {
                    out.push('_');
                }
                out.push(ch.to_ascii_lowercase());
                prev_was_lower_or_digit = false;
            } else {
                out.push(ch.to_ascii_lowercase());
                prev_was_lower_or_digit = true;
            }
        } else if !out.is_empty() && !out.ends_with('_') {
            out.push('_');
            prev_was_lower_or_digit = false;
        }
    }

    while out.ends_with('_') {
        out.pop();
    }
    out
}

fn apply_day_profile_overrides(
    mapped: &mut BTreeMap<String, f64>,
    profile_root: &Value,
    field_prefix: &str,
    n_steps: usize,
) {
    let profile = get_path(profile_root, &["value"]).unwrap_or(profile_root);

    for (day_key, day_type) in [("working_day", 1_usize), ("holiday", 2_usize)] {
        let Some(day_root) = get_path(profile, &[day_key]) else {
            continue;
        };

        let Value::Mapping(day_map) = day_root else {
            continue;
        };

        for (step_key, step_value) in day_map {
            let Value::String(step_label) = step_key else {
                continue;
            };

            let Ok(step_1based) = step_label.parse::<usize>() else {
                continue;
            };

            if step_1based == 0 || step_1based > n_steps {
                continue;
            }

            if let Some(v) = read_numeric_value(step_value) {
                mapped.insert(
                    format!("{field_prefix}.{:03}.{}", step_1based - 1, day_type),
                    v,
                );
            }
        }
    }
}

fn set_mapped_value(mapped: &mut BTreeMap<String, f64>, field_name: &str, value: f64) {
    if mapped.contains_key(field_name) {
        mapped.insert(field_name.to_string(), value);
        return;
    }

    let compact_candidate = field_name.replace('_', "");
    if let Some(resolved_key) = mapped
        .keys()
        .find(|k| k.replace('_', "") == compact_candidate)
        .cloned()
    {
        mapped.insert(resolved_key, value);
    }
}

fn read_normalized_numeric_from_mapping(root: &Value, target_field: &str) -> Option<f64> {
    let Value::Mapping(map) = root else {
        return None;
    };

    let target_compact = target_field.replace('_', "");

    for (field_key, field_value) in map {
        let Value::String(field_name_raw) = field_key else {
            continue;
        };

        let field_name = normalise_field_name(field_name_raw);
        if field_name == target_field || field_name.replace('_', "") == target_compact {
            if let Some(v) = read_numeric_value(field_value) {
                return Some(v);
            }
        }
    }

    None
}

fn ensure_len_with_default(values: &mut Vec<f64>, len: usize, default_value: f64) {
    if values.len() < len {
        values.resize(len, default_value);
    } else if values.len() > len {
        values.truncate(len);
    }
}

fn resize_site_variable_arrays(site: &mut SuewsSite, nlayer: usize) {
    let nspec = if site.spartacus_layer.nspec > 0 {
        site.spartacus_layer.nspec
    } else if site.spartacus.n_stream_sw_urban > 0 {
        site.spartacus.n_stream_sw_urban as usize
    } else {
        2
    };

    ensure_len_with_default(&mut site.spartacus.height, nlayer + 1, 0.0);

    site.spartacus_layer.nlayer = nlayer;
    site.spartacus_layer.nspec = nspec;
    ensure_len_with_default(
        &mut site.spartacus_layer.building_frac,
        nlayer,
        site.lc_bldg.sfr,
    );
    ensure_len_with_default(&mut site.spartacus_layer.building_scale, nlayer, 10.0);
    ensure_len_with_default(&mut site.spartacus_layer.veg_frac, nlayer, 0.0);
    ensure_len_with_default(&mut site.spartacus_layer.veg_scale, nlayer, 10.0);
    ensure_len_with_default(&mut site.spartacus_layer.alb_roof, nlayer, 0.15);
    ensure_len_with_default(&mut site.spartacus_layer.emis_roof, nlayer, site.lc_bldg.emis);
    ensure_len_with_default(&mut site.spartacus_layer.alb_wall, nlayer, 0.15);
    ensure_len_with_default(&mut site.spartacus_layer.emis_wall, nlayer, site.lc_bldg.emis);

    let matrix_len = nlayer * nspec;
    ensure_len_with_default(
        &mut site.spartacus_layer.roof_albedo_dir_mult_fact,
        matrix_len,
        1.0,
    );
    ensure_len_with_default(&mut site.spartacus_layer.wall_specular_frac, matrix_len, 0.0);
}

fn set_spartacus_layer_all_specs(
    values: &mut [f64],
    nlayer: usize,
    nspec: usize,
    layer_idx: usize,
    value: f64,
) {
    if layer_idx >= nlayer || nlayer == 0 || nspec == 0 {
        return;
    }

    for spec_idx in 0..nspec {
        let offset = spec_idx * nlayer + layer_idx;
        if let Some(slot) = values.get_mut(offset) {
            *slot = value;
        }
    }
}

fn assign_prefix(dst: &mut [f64], src: &[f64]) {
    for (dst_value, src_value) in dst.iter_mut().zip(src.iter()) {
        *dst_value = *src_value;
    }
}

fn set_ohm_coef_for_season(target: &mut crate::ohm_coef_lc::OhmCoefLc, season: &str, value: f64) {
    match season {
        "summer_dry" => target.summer_dry = value,
        "summer_wet" => target.summer_wet = value,
        "winter_dry" => target.winter_dry = value,
        "winter_wet" => target.winter_wet = value,
        _ => {}
    }
}

fn apply_ohm_overrides(ohm: &mut crate::ohm_prm::OhmPrm, lc_root: &Value) {
    if let Some(v) =
        read_numeric(lc_root, &["chanohm"]).or_else(|| read_numeric(lc_root, &["ch_anohm"]))
    {
        ohm.chanohm = v;
    }
    if let Some(v) =
        read_numeric(lc_root, &["cpanohm"]).or_else(|| read_numeric(lc_root, &["rho_cp_anohm"]))
    {
        ohm.cpanohm = v;
    }
    if let Some(v) =
        read_numeric(lc_root, &["kkanohm"]).or_else(|| read_numeric(lc_root, &["k_anohm"]))
    {
        ohm.kkanohm = v;
    }
    if let Some(v) = read_numeric(lc_root, &["ohm_threshsw"]) {
        ohm.ohm_threshsw = v;
    }
    if let Some(v) = read_numeric(lc_root, &["ohm_threshwd"]) {
        ohm.ohm_threshwd = v;
    }

    for (coef_index, coef_key) in [(0_usize, "a1"), (1_usize, "a2"), (2_usize, "a3")] {
        for season in OHM_SEASONS {
            if let Some(v) = read_numeric(lc_root, &["ohm_coef", season, coef_key]) {
                set_ohm_coef_for_season(&mut ohm.ohm_coef_lc[coef_index], season, v);
            }
        }
    }
}

fn apply_soil_overrides(soil: &mut crate::soil::SoilPrm, lc_root: &Value) {
    if let Some(v) = read_numeric(lc_root, &["soildepth"]) {
        soil.soildepth = v;
    }
    if let Some(v) = read_numeric(lc_root, &["soilstorecap"]) {
        soil.soilstorecap = v;
    }
    if let Some(v) = read_numeric(lc_root, &["sathydraulicconduct"]) {
        soil.sathydraulicconduct = v;
    }
}

fn apply_waterdist_overrides(waterdist: &mut crate::water_dist::WaterDistPrm, lc_root: &Value) {
    if let Some(v) = read_numeric(lc_root, &["waterdist", "to_paved"]) {
        waterdist.to_paved = v;
    }
    if let Some(v) = read_numeric(lc_root, &["waterdist", "to_bldg"])
        .or_else(|| read_numeric(lc_root, &["waterdist", "to_bldgs"]))
    {
        waterdist.to_bldg = v;
    }
    if let Some(v) = read_numeric(lc_root, &["waterdist", "to_evetr"]) {
        waterdist.to_evetr = v;
    }
    if let Some(v) = read_numeric(lc_root, &["waterdist", "to_dectr"]) {
        waterdist.to_dectr = v;
    }
    if let Some(v) = read_numeric(lc_root, &["waterdist", "to_grass"]) {
        waterdist.to_grass = v;
    }
    if let Some(v) = read_numeric(lc_root, &["waterdist", "to_bsoil"]) {
        waterdist.to_bsoil = v;
    }
    if let Some(v) = read_numeric(lc_root, &["waterdist", "to_water"]) {
        waterdist.to_water = v;
    }
    if let Some(v) = read_numeric(lc_root, &["waterdist", "to_soilstore"])
        .or_else(|| read_numeric(lc_root, &["waterdist", "to_runoff"]))
    {
        waterdist.to_soilstore = v;
    }
}

fn apply_lai_overrides(lai: &mut crate::lai::LaiPrm, lc_root: &Value) {
    if let Some(v) = read_numeric(lc_root, &["lai", "baset"]) {
        lai.baset = v;
    }
    if let Some(v) = read_numeric(lc_root, &["lai", "gddfull"]) {
        lai.gddfull = v;
    }
    if let Some(v) = read_numeric(lc_root, &["lai", "basete"]) {
        lai.basete = v;
    }
    if let Some(v) = read_numeric(lc_root, &["lai", "sddfull"]) {
        lai.sddfull = v;
    }
    if let Some(v) = read_numeric(lc_root, &["lai", "laimin"]) {
        lai.laimin = v;
    }
    if let Some(v) = read_numeric(lc_root, &["lai", "laimax"]) {
        lai.laimax = v;
    }
    if let Some(v) = read_numeric(lc_root, &["lai", "laipower", "growth_lai"]) {
        lai.laipower[0] = v;
    }
    if let Some(v) = read_numeric(lc_root, &["lai", "laipower", "growth_gdd"]) {
        lai.laipower[1] = v;
    }
    if let Some(v) = read_numeric(lc_root, &["lai", "laipower", "senescence_lai"]) {
        lai.laipower[2] = v;
    }
    if let Some(v) = read_numeric(lc_root, &["lai", "laipower", "senescence_sdd"]) {
        lai.laipower[3] = v;
    }
    if let Some(v) = read_i32(lc_root, &["lai", "laitype"]) {
        lai.laitype = v;
    }
}

fn apply_bioco2_overrides(bioco2: &mut crate::bioco2::BioCo2Prm, lc_root: &Value) {
    if let Some(v) = read_numeric(lc_root, &["beta_bioco2"]) {
        bioco2.beta_bioco2 = v;
    }
    if let Some(v) = read_numeric(lc_root, &["beta_enh_bioco2"]) {
        bioco2.beta_enh_bioco2 = v;
    }
    if let Some(v) = read_numeric(lc_root, &["alpha_bioco2"]) {
        bioco2.alpha_bioco2 = v;
    }
    if let Some(v) = read_numeric(lc_root, &["alpha_enh_bioco2"]) {
        bioco2.alpha_enh_bioco2 = v;
    }
    if let Some(v) = read_numeric(lc_root, &["resp_a"]) {
        bioco2.resp_a = v;
    }
    if let Some(v) = read_numeric(lc_root, &["resp_b"]) {
        bioco2.resp_b = v;
    }
    if let Some(v) = read_numeric(lc_root, &["theta_bioco2"]) {
        bioco2.theta_bioco2 = v;
    }
    if let Some(v) = read_numeric(lc_root, &["min_res_bioco2"]) {
        bioco2.min_res_bioco2 = v;
    }
}

fn apply_spartacus_overrides(site: &mut SuewsSite, site_root: &Value) {
    if let Some(v) = read_numeric(site_root, &["properties", "spartacus", "air_ext_lw"]) {
        site.spartacus.air_ext_lw = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "spartacus", "air_ext_sw"]) {
        site.spartacus.air_ext_sw = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "spartacus", "air_ssa_lw"]) {
        site.spartacus.air_ssa_lw = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "spartacus", "air_ssa_sw"]) {
        site.spartacus.air_ssa_sw = v;
    }
    if let Some(v) = read_numeric(
        site_root,
        &["properties", "spartacus", "ground_albedo_dir_mult_fact"],
    ) {
        site.spartacus.ground_albedo_dir_mult_fact = v;
    }
    if let Some(v) = read_i32(site_root, &["properties", "spartacus", "n_stream_lw_urban"]) {
        site.spartacus.n_stream_lw_urban = v;
    }
    if let Some(v) = read_i32(site_root, &["properties", "spartacus", "n_stream_sw_urban"]) {
        site.spartacus.n_stream_sw_urban = v;
    }
    if let Some(v) = read_i32(
        site_root,
        &["properties", "spartacus", "n_vegetation_region_urban"],
    ) {
        site.spartacus.n_vegetation_region_urban = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "spartacus", "sw_dn_direct_frac"]) {
        site.spartacus.sw_dn_direct_frac = v;
    }
    if let Some(v) = read_numeric(
        site_root,
        &["properties", "spartacus", "use_sw_direct_albedo"],
    ) {
        site.spartacus.use_sw_direct_albedo = v;
    }
    if let Some(v) = read_numeric(
        site_root,
        &["properties", "spartacus", "veg_contact_fraction_const"],
    ) {
        site.spartacus.veg_contact_fraction_const = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "spartacus", "veg_fsd_const"]) {
        site.spartacus.veg_fsd_const = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "spartacus", "veg_ssa_lw"]) {
        site.spartacus.veg_ssa_lw = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "spartacus", "veg_ssa_sw"]) {
        site.spartacus.veg_ssa_sw = v;
    }
}

fn apply_vertical_layers_overrides(site: &mut SuewsSite, site_root: &Value) {
    let nlayer = site.spartacus_layer.nlayer;
    let nspec = site.spartacus_layer.nspec;

    if let Some(height_values) =
        read_numeric_sequence(site_root, &["properties", "vertical_layers", "height"])
    {
        if site.spartacus.height.len() == height_values.len() {
            assign_prefix(
                site.spartacus.height.as_mut_slice(),
                height_values.as_slice(),
            );
        } else if !site.spartacus.height.is_empty() {
            assign_prefix(
                site.spartacus.height.as_mut_slice(),
                height_values.as_slice(),
            );
        }
    }

    if let Some(building_frac) = read_numeric_sequence(
        site_root,
        &["properties", "vertical_layers", "building_frac"],
    ) {
        assign_prefix(
            site.spartacus_layer.building_frac.as_mut_slice(),
            building_frac.as_slice(),
        );
    }
    if let Some(veg_frac) =
        read_numeric_sequence(site_root, &["properties", "vertical_layers", "veg_frac"])
    {
        assign_prefix(
            site.spartacus_layer.veg_frac.as_mut_slice(),
            veg_frac.as_slice(),
        );
    }
    if let Some(building_scale) = read_numeric_sequence(
        site_root,
        &["properties", "vertical_layers", "building_scale"],
    ) {
        assign_prefix(
            site.spartacus_layer.building_scale.as_mut_slice(),
            building_scale.as_slice(),
        );
    }
    if let Some(veg_scale) =
        read_numeric_sequence(site_root, &["properties", "vertical_layers", "veg_scale"])
    {
        assign_prefix(
            site.spartacus_layer.veg_scale.as_mut_slice(),
            veg_scale.as_slice(),
        );
    }

    if let Some(roofs) = read_sequence(site_root, &["properties", "vertical_layers", "roofs"]) {
        for (layer_idx, roof_root) in roofs.iter().enumerate() {
            if let Some(v) = read_numeric(roof_root, &["alb"]) {
                if let Some(slot) = site.spartacus_layer.alb_roof.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if let Some(v) = read_numeric(roof_root, &["emis"]) {
                if let Some(slot) = site.spartacus_layer.emis_roof.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if let Some(v) = read_numeric(roof_root, &["roof_albedo_dir_mult_fact"]) {
                set_spartacus_layer_all_specs(
                    site.spartacus_layer
                        .roof_albedo_dir_mult_fact
                        .as_mut_slice(),
                    nlayer,
                    nspec,
                    layer_idx,
                    v,
                );
            }
            if let Some(v) = read_numeric(roof_root, &["wall_specular_frac"]) {
                set_spartacus_layer_all_specs(
                    site.spartacus_layer.wall_specular_frac.as_mut_slice(),
                    nlayer,
                    nspec,
                    layer_idx,
                    v,
                );
            }
        }
    }

    if let Some(walls) = read_sequence(site_root, &["properties", "vertical_layers", "walls"]) {
        for (layer_idx, wall_root) in walls.iter().enumerate() {
            if let Some(v) = read_numeric(wall_root, &["alb"]) {
                if let Some(slot) = site.spartacus_layer.alb_wall.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if let Some(v) = read_numeric(wall_root, &["emis"]) {
                if let Some(slot) = site.spartacus_layer.emis_wall.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if let Some(v) = read_numeric(wall_root, &["roof_albedo_dir_mult_fact"]) {
                set_spartacus_layer_all_specs(
                    site.spartacus_layer
                        .roof_albedo_dir_mult_fact
                        .as_mut_slice(),
                    nlayer,
                    nspec,
                    layer_idx,
                    v,
                );
            }
            if let Some(v) = read_numeric(wall_root, &["wall_specular_frac"]) {
                set_spartacus_layer_all_specs(
                    site.spartacus_layer.wall_specular_frac.as_mut_slice(),
                    nlayer,
                    nspec,
                    layer_idx,
                    v,
                );
            }
        }
    }
}

fn apply_lumps_overrides(site: &mut SuewsSite, site_root: &Value) {
    if let Some(v) = read_numeric(site_root, &["properties", "lumps", "raincover"]) {
        site.lumps.raincover = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "lumps", "rainmaxres"]) {
        site.lumps.rainmaxres = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "lumps", "drainrt"]) {
        site.lumps.drainrt = v;
    }
    if let Some(v) = read_i32(site_root, &["properties", "lumps", "veg_type"]) {
        site.lumps.veg_type = v;
    }
}

fn apply_snow_overrides(site: &mut SuewsSite, site_root: &Value) {
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "crwmax"]) {
        site.snow.crwmax = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "crwmin"]) {
        site.snow.crwmin = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "narp_emis_snow"]) {
        site.snow.narp_emis_snow = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "preciplimit"]) {
        site.snow.preciplimit = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "preciplimitalb"]) {
        site.snow.preciplimitalb = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "snowalbmax"]) {
        site.snow.snowalbmax = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "snowalbmin"]) {
        site.snow.snowalbmin = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "snowdensmax"]) {
        site.snow.snowdensmax = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "snowdensmin"]) {
        site.snow.snowdensmin = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "snowlimbldg"]) {
        site.snow.snowlimbldg = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "snowlimpaved"]) {
        site.snow.snowlimpaved = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "tau_a"]) {
        site.snow.tau_a = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "tau_f"]) {
        site.snow.tau_f = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "tau_r"]) {
        site.snow.tau_r = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "tempmeltfact"]) {
        site.snow.tempmeltfact = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "snow", "radmeltfact"]) {
        site.snow.radmeltfact = v;
    }

    for hour in 1..=24 {
        let key = hour.to_string();
        if let Some(v) = read_numeric(
            site_root,
            &[
                "properties",
                "snow",
                "snowprof_24hr",
                "working_day",
                key.as_str(),
            ],
        ) {
            site.snow.snowprof_24hr_working[hour - 1] = v;
        }
        if let Some(v) = read_numeric(
            site_root,
            &[
                "properties",
                "snow",
                "snowprof_24hr",
                "holiday",
                key.as_str(),
            ],
        ) {
            site.snow.snowprof_24hr_holiday[hour - 1] = v;
        }
    }

    for (surface_name, surface_idx) in SURFACE_YAML_ORDER {
        if let Some(v) = read_numeric(
            site_root,
            &["properties", "land_cover", surface_name, "snowpacklimit"],
        ) {
            site.snow.snowpacklimit[surface_idx] = v;
        }
    }
}

fn apply_land_cover_overrides(site: &mut SuewsSite, site_root: &Value) {
    if let Some(lc_root) = get_path(site_root, &["properties", "land_cover", "paved"]) {
        if let Some(v) = read_numeric(lc_root, &["sfr"]) {
            site.lc_paved.sfr = v;
        }
        if let Some(v) = read_numeric(lc_root, &["emis"]) {
            site.lc_paved.emis = v;
        }
        if let Some(v) = read_numeric(lc_root, &["statelimit"]) {
            site.lc_paved.statelimit = v;
        }
        if let Some(v) = read_numeric(lc_root, &["wetthresh"]) {
            site.lc_paved.wetthresh = v;
        }
        if let Some(v) = read_numeric(lc_root, &["irrfrac"]) {
            site.lc_paved.irrfracpaved = v;
        }
        apply_ohm_overrides(&mut site.lc_paved.ohm, lc_root);
        apply_soil_overrides(&mut site.lc_paved.soil, lc_root);
        apply_waterdist_overrides(&mut site.lc_paved.waterdist, lc_root);
    }

    if let Some(lc_root) = get_path(site_root, &["properties", "land_cover", "bldgs"]) {
        if let Some(v) = read_numeric(lc_root, &["sfr"]) {
            site.lc_bldg.sfr = v;
        }
        if let Some(v) = read_numeric(lc_root, &["emis"]) {
            site.lc_bldg.emis = v;
        }
        if let Some(v) = read_numeric(lc_root, &["faibldg"]) {
            site.lc_bldg.faibldg = v;
        }
        if let Some(v) = read_numeric(lc_root, &["bldgh"]) {
            site.lc_bldg.bldgh = v;
        }
        if let Some(v) = read_numeric(lc_root, &["statelimit"]) {
            site.lc_bldg.statelimit = v;
        }
        if let Some(v) = read_numeric(lc_root, &["wetthresh"]) {
            site.lc_bldg.wetthresh = v;
        }
        if let Some(v) = read_numeric(lc_root, &["irrfrac"]) {
            site.lc_bldg.irrfracbldgs = v;
        }
        apply_ohm_overrides(&mut site.lc_bldg.ohm, lc_root);
        apply_soil_overrides(&mut site.lc_bldg.soil, lc_root);
        apply_waterdist_overrides(&mut site.lc_bldg.waterdist, lc_root);
    }

    if let Some(lc_root) = get_path(site_root, &["properties", "land_cover", "evetr"]) {
        if let Some(v) = read_numeric(lc_root, &["sfr"]) {
            site.lc_evetr.sfr = v;
        }
        if let Some(v) = read_numeric(lc_root, &["emis"]) {
            site.lc_evetr.emis = v;
        }
        if let Some(v) = read_numeric(lc_root, &["faievetree"]) {
            site.lc_evetr.faievetree = v;
        }
        if let Some(v) = read_numeric(lc_root, &["evetreeh"]) {
            site.lc_evetr.evetreeh = v;
        }
        if let Some(v) = read_numeric(lc_root, &["alb_min"]) {
            site.lc_evetr.alb_min = v;
        }
        if let Some(v) = read_numeric(lc_root, &["alb_max"]) {
            site.lc_evetr.alb_max = v;
        }
        if let Some(v) = read_numeric(lc_root, &["statelimit"]) {
            site.lc_evetr.statelimit = v;
        }
        if let Some(v) = read_numeric(lc_root, &["wetthresh"]) {
            site.lc_evetr.wetthresh = v;
        }
        if let Some(v) = read_numeric(lc_root, &["irrfrac"]) {
            site.lc_evetr.irrfracevetr = v;
        }
        if let Some(v) = read_numeric(lc_root, &["maxconductance"]) {
            site.lc_evetr.maxconductance = v;
        }
        apply_ohm_overrides(&mut site.lc_evetr.ohm, lc_root);
        apply_soil_overrides(&mut site.lc_evetr.soil, lc_root);
        apply_bioco2_overrides(&mut site.lc_evetr.bioco2, lc_root);
        apply_lai_overrides(&mut site.lc_evetr.lai, lc_root);
        apply_waterdist_overrides(&mut site.lc_evetr.waterdist, lc_root);
    }

    if let Some(lc_root) = get_path(site_root, &["properties", "land_cover", "dectr"]) {
        if let Some(v) = read_numeric(lc_root, &["sfr"]) {
            site.lc_dectr.sfr = v;
        }
        if let Some(v) = read_numeric(lc_root, &["emis"]) {
            site.lc_dectr.emis = v;
        }
        if let Some(v) = read_numeric(lc_root, &["faidectree"]) {
            site.lc_dectr.faidectree = v;
        }
        if let Some(v) = read_numeric(lc_root, &["dectreeh"]) {
            site.lc_dectr.dectreeh = v;
        }
        if let Some(v) = read_numeric(lc_root, &["pormin_dec"]) {
            site.lc_dectr.pormin_dec = v;
        }
        if let Some(v) = read_numeric(lc_root, &["pormax_dec"]) {
            site.lc_dectr.pormax_dec = v;
        }
        if let Some(v) = read_numeric(lc_root, &["capmax_dec"]) {
            site.lc_dectr.capmax_dec = v;
        }
        if let Some(v) = read_numeric(lc_root, &["capmin_dec"]) {
            site.lc_dectr.capmin_dec = v;
        }
        if let Some(v) = read_numeric(lc_root, &["alb_min"]) {
            site.lc_dectr.alb_min = v;
        }
        if let Some(v) = read_numeric(lc_root, &["alb_max"]) {
            site.lc_dectr.alb_max = v;
        }
        if let Some(v) = read_numeric(lc_root, &["statelimit"]) {
            site.lc_dectr.statelimit = v;
        }
        if let Some(v) = read_numeric(lc_root, &["wetthresh"]) {
            site.lc_dectr.wetthresh = v;
        }
        if let Some(v) = read_numeric(lc_root, &["irrfrac"]) {
            site.lc_dectr.irrfracdectr = v;
        }
        if let Some(v) = read_numeric(lc_root, &["maxconductance"]) {
            site.lc_dectr.maxconductance = v;
        }
        apply_ohm_overrides(&mut site.lc_dectr.ohm, lc_root);
        apply_soil_overrides(&mut site.lc_dectr.soil, lc_root);
        apply_bioco2_overrides(&mut site.lc_dectr.bioco2, lc_root);
        apply_lai_overrides(&mut site.lc_dectr.lai, lc_root);
        apply_waterdist_overrides(&mut site.lc_dectr.waterdist, lc_root);
    }

    if let Some(lc_root) = get_path(site_root, &["properties", "land_cover", "grass"]) {
        if let Some(v) = read_numeric(lc_root, &["sfr"]) {
            site.lc_grass.sfr = v;
        }
        if let Some(v) = read_numeric(lc_root, &["emis"]) {
            site.lc_grass.emis = v;
        }
        if let Some(v) = read_numeric(lc_root, &["alb_min"]) {
            site.lc_grass.alb_min = v;
        }
        if let Some(v) = read_numeric(lc_root, &["alb_max"]) {
            site.lc_grass.alb_max = v;
        }
        if let Some(v) = read_numeric(lc_root, &["statelimit"]) {
            site.lc_grass.statelimit = v;
        }
        if let Some(v) = read_numeric(lc_root, &["wetthresh"]) {
            site.lc_grass.wetthresh = v;
        }
        if let Some(v) = read_numeric(lc_root, &["irrfrac"]) {
            site.lc_grass.irrfracgrass = v;
        }
        if let Some(v) = read_numeric(lc_root, &["maxconductance"]) {
            site.lc_grass.maxconductance = v;
        }
        apply_ohm_overrides(&mut site.lc_grass.ohm, lc_root);
        apply_soil_overrides(&mut site.lc_grass.soil, lc_root);
        apply_bioco2_overrides(&mut site.lc_grass.bioco2, lc_root);
        apply_lai_overrides(&mut site.lc_grass.lai, lc_root);
        apply_waterdist_overrides(&mut site.lc_grass.waterdist, lc_root);
    }

    if let Some(lc_root) = get_path(site_root, &["properties", "land_cover", "bsoil"]) {
        if let Some(v) = read_numeric(lc_root, &["sfr"]) {
            site.lc_bsoil.sfr = v;
        }
        if let Some(v) = read_numeric(lc_root, &["emis"]) {
            site.lc_bsoil.emis = v;
        }
        if let Some(v) = read_numeric(lc_root, &["statelimit"]) {
            site.lc_bsoil.statelimit = v;
        }
        if let Some(v) = read_numeric(lc_root, &["wetthresh"]) {
            site.lc_bsoil.wetthresh = v;
        }
        if let Some(v) = read_numeric(lc_root, &["irrfrac"]) {
            site.lc_bsoil.irrfracbsoil = v;
        }
        apply_ohm_overrides(&mut site.lc_bsoil.ohm, lc_root);
        apply_soil_overrides(&mut site.lc_bsoil.soil, lc_root);
        apply_waterdist_overrides(&mut site.lc_bsoil.waterdist, lc_root);
    }

    if let Some(lc_root) = get_path(site_root, &["properties", "land_cover", "water"]) {
        if let Some(v) = read_numeric(lc_root, &["sfr"]) {
            site.lc_water.sfr = v;
        }
        if let Some(v) = read_numeric(lc_root, &["emis"]) {
            site.lc_water.emis = v;
        }
        if let Some(v) = read_numeric(lc_root, &["statelimit"]) {
            site.lc_water.statelimit = v;
        }
        if let Some(v) = read_numeric(lc_root, &["wetthresh"]) {
            site.lc_water.wetthresh = v;
        }
        if let Some(v) = read_numeric(lc_root, &["irrfrac"]) {
            site.lc_water.irrfracwater = v;
        }
        if let Some(v) = read_numeric(lc_root, &["flowchange"]) {
            site.lc_water.flowchange = v;
        }
        apply_ohm_overrides(&mut site.lc_water.ohm, lc_root);
        apply_soil_overrides(&mut site.lc_water.soil, lc_root);
    }
}

fn apply_conductance_overrides(site: &mut SuewsSite, root: &Value, site_root: &Value) {
    if let Some(v) = read_i32(root, &["model", "physics", "gsmodel"]) {
        site.conductance.gsmodel = v;
    } else if let Some(v) = read_i32(site_root, &["properties", "conductance", "gsmodel"]) {
        site.conductance.gsmodel = v;
    }

    if let Some(v) = read_numeric(site_root, &["properties", "conductance", "g_max"]) {
        site.conductance.g_max = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "conductance", "g_k"]) {
        site.conductance.g_k = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "conductance", "g_q_base"]) {
        site.conductance.g_q_base = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "conductance", "g_q_shape"]) {
        site.conductance.g_q_shape = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "conductance", "g_t"]) {
        site.conductance.g_t = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "conductance", "g_sm"]) {
        site.conductance.g_sm = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "conductance", "kmax"]) {
        site.conductance.kmax = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "conductance", "s1"]) {
        site.conductance.s1 = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "conductance", "s2"]) {
        site.conductance.s2 = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "conductance", "th"]) {
        site.conductance.th = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "conductance", "tl"]) {
        site.conductance.tl = v;
    }
}

fn apply_anthro_emis_overrides(site: &mut SuewsSite, site_root: &Value) {
    let base = ["properties", "anthropogenic_emissions"];

    // --- top-level scalars ---
    if let Some(v) = read_i32(site_root, &[base[0], base[1], "startdls"]) {
        site.anthroemis.startdls = v;
    }
    if let Some(v) = read_i32(site_root, &[base[0], base[1], "enddls"]) {
        site.anthroemis.enddls = v;
    }

    // --- heat sub-section ---
    macro_rules! wd_hol {
        ($yaml_name:expr, $field_wd:expr, $field_hol:expr) => {
            if let Some(v) = read_numeric(
                site_root,
                &[base[0], base[1], "heat", $yaml_name, "working_day"],
            ) {
                $field_wd = v;
            }
            if let Some(v) = read_numeric(
                site_root,
                &[base[0], base[1], "heat", $yaml_name, "holiday"],
            ) {
                $field_hol = v;
            }
        };
    }

    let h = &mut site.anthroemis.anthroheat;
    wd_hol!("qf0_beu", h.qf0_beu_working, h.qf0_beu_holiday);
    wd_hol!("qf_a", h.qf_a_working, h.qf_a_holiday);
    wd_hol!("qf_b", h.qf_b_working, h.qf_b_holiday);
    wd_hol!("qf_c", h.qf_c_working, h.qf_c_holiday);
    wd_hol!(
        "baset_cooling",
        h.baset_cooling_working,
        h.baset_cooling_holiday
    );
    wd_hol!(
        "baset_heating",
        h.baset_heating_working,
        h.baset_heating_holiday
    );
    wd_hol!("ah_min", h.ah_min_working, h.ah_min_holiday);
    wd_hol!(
        "ah_slope_cooling",
        h.ah_slope_cooling_working,
        h.ah_slope_cooling_holiday
    );
    wd_hol!(
        "ah_slope_heating",
        h.ah_slope_heating_working,
        h.ah_slope_heating_holiday
    );
    wd_hol!(
        "popdensdaytime",
        h.popdensdaytime_working,
        h.popdensdaytime_holiday
    );

    if let Some(v) = read_numeric(site_root, &[base[0], base[1], "heat", "popdensnighttime"]) {
        h.popdensnighttime = v;
    }

    // 24-hour profiles under heat
    for hour in 1..=24_usize {
        let key = hour.to_string();
        if let Some(v) = read_numeric(
            site_root,
            &[
                base[0],
                base[1],
                "heat",
                "ahprof_24hr",
                "working_day",
                key.as_str(),
            ],
        ) {
            h.ahprof_24hr_working[hour - 1] = v;
        }
        if let Some(v) = read_numeric(
            site_root,
            &[
                base[0],
                base[1],
                "heat",
                "ahprof_24hr",
                "holiday",
                key.as_str(),
            ],
        ) {
            h.ahprof_24hr_holiday[hour - 1] = v;
        }
        if let Some(v) = read_numeric(
            site_root,
            &[
                base[0],
                base[1],
                "heat",
                "popprof_24hr",
                "working_day",
                key.as_str(),
            ],
        ) {
            h.popprof_24hr_working[hour - 1] = v;
        }
        if let Some(v) = read_numeric(
            site_root,
            &[
                base[0],
                base[1],
                "heat",
                "popprof_24hr",
                "holiday",
                key.as_str(),
            ],
        ) {
            h.popprof_24hr_holiday[hour - 1] = v;
        }
    }

    // --- co2 sub-section ---
    let e = &mut site.anthroemis;
    if let Some(v) = read_numeric(site_root, &[base[0], base[1], "co2", "ef_umolco2perj"]) {
        e.ef_umolco2perj = v;
    }
    if let Some(v) = read_numeric(site_root, &[base[0], base[1], "co2", "enef_v_jkm"]) {
        e.enef_v_jkm = v;
    }
    if let Some(v) = read_numeric(
        site_root,
        &[base[0], base[1], "co2", "fcef_v_kgkm", "working_day"],
    ) {
        e.fcef_v_kgkm[0] = v;
    }
    if let Some(v) = read_numeric(
        site_root,
        &[base[0], base[1], "co2", "fcef_v_kgkm", "holiday"],
    ) {
        e.fcef_v_kgkm[1] = v;
    }
    if let Some(v) = read_numeric(site_root, &[base[0], base[1], "co2", "frfossilfuel_heat"]) {
        e.frfossilfuel_heat = v;
    }
    if let Some(v) = read_numeric(
        site_root,
        &[base[0], base[1], "co2", "frfossilfuel_nonheat"],
    ) {
        e.frfossilfuel_nonheat = v;
    }
    if let Some(v) = read_numeric(site_root, &[base[0], base[1], "co2", "maxfcmetab"]) {
        e.maxfcmetab = v;
    }
    if let Some(v) = read_numeric(site_root, &[base[0], base[1], "co2", "maxqfmetab"]) {
        e.maxqfmetab = v;
    }
    if let Some(v) = read_numeric(site_root, &[base[0], base[1], "co2", "minfcmetab"]) {
        e.minfcmetab = v;
    }
    if let Some(v) = read_numeric(site_root, &[base[0], base[1], "co2", "minqfmetab"]) {
        e.minqfmetab = v;
    }
    if let Some(v) = read_numeric(
        site_root,
        &[base[0], base[1], "co2", "trafficrate", "working_day"],
    ) {
        e.trafficrate_working = v;
    }
    if let Some(v) = read_numeric(
        site_root,
        &[base[0], base[1], "co2", "trafficrate", "holiday"],
    ) {
        e.trafficrate_holiday = v;
    }
    if let Some(v) = read_numeric(site_root, &[base[0], base[1], "co2", "trafficunits"]) {
        e.trafficunits = v;
    }

    for hour in 1..=24_usize {
        let key = hour.to_string();
        if let Some(v) = read_numeric(
            site_root,
            &[
                base[0],
                base[1],
                "co2",
                "traffprof_24hr",
                "working_day",
                key.as_str(),
            ],
        ) {
            e.traffprof_24hr_working[hour - 1] = v;
        }
        if let Some(v) = read_numeric(
            site_root,
            &[
                base[0],
                base[1],
                "co2",
                "traffprof_24hr",
                "holiday",
                key.as_str(),
            ],
        ) {
            e.traffprof_24hr_holiday[hour - 1] = v;
        }
        if let Some(v) = read_numeric(
            site_root,
            &[
                base[0],
                base[1],
                "co2",
                "humactivity_24hr",
                "working_day",
                key.as_str(),
            ],
        ) {
            e.humactivity_24hr_working[hour - 1] = v;
        }
        if let Some(v) = read_numeric(
            site_root,
            &[
                base[0],
                base[1],
                "co2",
                "humactivity_24hr",
                "holiday",
                key.as_str(),
            ],
        ) {
            e.humactivity_24hr_holiday[hour - 1] = v;
        }
    }
}

fn apply_config_overrides(config: &mut SuewsConfig, root: &Value) {
    if let Some(v) = read_i32(root, &["model", "physics", "rslmethod"]) {
        config.rsl_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "emissionsmethod"]) {
        config.emissions_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "roughlenheatmethod"]) {
        config.rough_len_heat_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "roughlenmommethod"]) {
        config.rough_len_mom_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "faimethod"]) {
        config.fai_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "smdmethod"]) {
        config.smd_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "waterusemethod"]) {
        config.water_use_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "netradiationmethod"]) {
        config.net_radiation_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "stabilitymethod"]) {
        config.stability_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "storageheatmethod"]) {
        config.storage_heat_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "control", "diagnose"]) {
        config.diagnose = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "snowuse"]) {
        config.snow_use = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "ohmincqf"]) {
        config.ohm_inc_qf = v;
    }
    // EvapMethod defaults to 2 (Shuttleworth) when not specified.
    // Matches Python DTS behaviour: supy/dts/_populate.py:272-273
    config.evap_method = read_i32(root, &["model", "physics", "evapmethod"]).unwrap_or(2);
    // LAImethod defaults to 1 (GDD model) when not specified.
    // Matches Python DTS behaviour: supy/dts/_populate.py:275-276
    config.lai_method = read_i32(root, &["model", "physics", "laimethod"]).unwrap_or(1);
    if let Some(v) = read_i32(root, &["model", "physics", "rsllevel"]) {
        config.rsl_level = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "stebbsmethod"]) {
        config.stebbs_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "physics", "rcmethod"]) {
        config.rc_method = v;
    }
    if let Some(v) = read_i32(root, &["model", "control", "flag_test"]) {
        config.flag_test = v != 0;
    }
    if let Some(v) = read_i32(root, &["model", "control", "diag_qs"]) {
        config.diag_qs = v;
    }
}

fn apply_ehc_overrides(site: &mut SuewsSite, site_root: &Value, ndepth: usize) {
    // EHC arrays use NSURF (7) as the first dimension to match the physics loop
    // which iterates i_surf=1..nsurf. The Fortran C API driver now uses nsurf
    // (not SPARTACUS nlayer) for EHC buffer sizing and unpacking.
    let nlayer_ehc = NSURF;
    let mat_len = nlayer_ehc * ndepth;

    site.ehc.nlayer = nlayer_ehc;
    site.ehc.ndepth = ndepth;

    // Resize all EHC arrays to correct dimensions (zero-filled by default).
    site.ehc.soil_storecap_roof.resize(nlayer_ehc, 0.0);
    site.ehc.soil_storecap_wall.resize(nlayer_ehc, 0.0);
    site.ehc.state_limit_roof.resize(nlayer_ehc, 0.0);
    site.ehc.state_limit_wall.resize(nlayer_ehc, 0.0);
    site.ehc.wet_thresh_roof.resize(nlayer_ehc, 0.0);
    site.ehc.wet_thresh_wall.resize(nlayer_ehc, 0.0);
    site.ehc.tin_roof.resize(nlayer_ehc, 5.0);
    site.ehc.tin_wall.resize(nlayer_ehc, 5.0);
    site.ehc.tin_surf.resize(nlayer_ehc, 2.0);
    site.ehc.k_roof.resize(mat_len, 0.0);
    site.ehc.k_wall.resize(mat_len, 0.0);
    site.ehc.k_surf.resize(mat_len, 0.0);
    site.ehc.cp_roof.resize(mat_len, 0.0);
    site.ehc.cp_wall.resize(mat_len, 0.0);
    site.ehc.cp_surf.resize(mat_len, 0.0);
    site.ehc.dz_roof.resize(mat_len, 0.0);
    site.ehc.dz_wall.resize(mat_len, 0.0);
    site.ehc.dz_surf.resize(mat_len, 0.0);

    // Populate k_surf, cp_surf, dz_surf from each surface type's thermal_layers.
    // Row-major layout: element [surf_idx * ndepth + depth_idx].
    for &(surface_name, surface_idx) in &SURFACE_YAML_ORDER {
        let tl_path = ["properties", "land_cover", surface_name, "thermal_layers"];
        if let Some(k_vals) = read_numeric_sequence(
            site_root,
            &[tl_path[0], tl_path[1], tl_path[2], tl_path[3], "k"],
        ) {
            for (j, &v) in k_vals.iter().take(ndepth).enumerate() {
                site.ehc.k_surf[surface_idx * ndepth + j] = v;
            }
        }
        if let Some(cp_vals) = read_numeric_sequence(
            site_root,
            &[tl_path[0], tl_path[1], tl_path[2], tl_path[3], "rho_cp"],
        )
        .or_else(|| {
            read_numeric_sequence(
                site_root,
                &[tl_path[0], tl_path[1], tl_path[2], tl_path[3], "cp"],
            )
        }) {
            for (j, &v) in cp_vals.iter().take(ndepth).enumerate() {
                site.ehc.cp_surf[surface_idx * ndepth + j] = v;
            }
        }
        if let Some(dz_vals) = read_numeric_sequence(
            site_root,
            &[tl_path[0], tl_path[1], tl_path[2], tl_path[3], "dz"],
        ) {
            for (j, &v) in dz_vals.iter().take(ndepth).enumerate() {
                site.ehc.dz_surf[surface_idx * ndepth + j] = v;
            }
        }

        // tin_surf from initial_states.<surface>.tin, fallback to temperature[0].
        if let Some(v) =
            read_numeric(site_root, &["initial_states", surface_name, "tin"]).or_else(|| {
                read_numeric_sequence(site_root, &["initial_states", surface_name, "temperature"])
                    .and_then(|vals| vals.first().copied())
            })
        {
            site.ehc.tin_surf[surface_idx] = v;
        }
    }

    if let Some(roofs) = read_sequence(site_root, &["properties", "vertical_layers", "roofs"]) {
        for (layer_idx, roof_root) in roofs.iter().take(nlayer_ehc).enumerate() {
            if let Some(v) = read_numeric(roof_root, &["soilstorecap"]) {
                site.ehc.soil_storecap_roof[layer_idx] = v;
            }
            if let Some(v) = read_numeric(roof_root, &["statelimit"]) {
                site.ehc.state_limit_roof[layer_idx] = v;
            }
            if let Some(v) = read_numeric(roof_root, &["wetthresh"]) {
                site.ehc.wet_thresh_roof[layer_idx] = v;
            }

            if let Some(k_vals) = read_numeric_sequence(roof_root, &["thermal_layers", "k"]) {
                for (j, &v) in k_vals.iter().take(ndepth).enumerate() {
                    site.ehc.k_roof[layer_idx * ndepth + j] = v;
                }
            }
            if let Some(cp_vals) = read_numeric_sequence(roof_root, &["thermal_layers", "rho_cp"])
                .or_else(|| read_numeric_sequence(roof_root, &["thermal_layers", "cp"]))
            {
                for (j, &v) in cp_vals.iter().take(ndepth).enumerate() {
                    site.ehc.cp_roof[layer_idx * ndepth + j] = v;
                }
            }
            if let Some(dz_vals) = read_numeric_sequence(roof_root, &["thermal_layers", "dz"]) {
                for (j, &v) in dz_vals.iter().take(ndepth).enumerate() {
                    site.ehc.dz_roof[layer_idx * ndepth + j] = v;
                }
            }
        }
    }

    if let Some(walls) = read_sequence(site_root, &["properties", "vertical_layers", "walls"]) {
        for (layer_idx, wall_root) in walls.iter().take(nlayer_ehc).enumerate() {
            if let Some(v) = read_numeric(wall_root, &["soilstorecap"]) {
                site.ehc.soil_storecap_wall[layer_idx] = v;
            }
            if let Some(v) = read_numeric(wall_root, &["statelimit"]) {
                site.ehc.state_limit_wall[layer_idx] = v;
            }
            if let Some(v) = read_numeric(wall_root, &["wetthresh"]) {
                site.ehc.wet_thresh_wall[layer_idx] = v;
            }

            if let Some(k_vals) = read_numeric_sequence(wall_root, &["thermal_layers", "k"]) {
                for (j, &v) in k_vals.iter().take(ndepth).enumerate() {
                    site.ehc.k_wall[layer_idx * ndepth + j] = v;
                }
            }
            if let Some(cp_vals) = read_numeric_sequence(wall_root, &["thermal_layers", "rho_cp"])
                .or_else(|| read_numeric_sequence(wall_root, &["thermal_layers", "cp"]))
            {
                for (j, &v) in cp_vals.iter().take(ndepth).enumerate() {
                    site.ehc.cp_wall[layer_idx * ndepth + j] = v;
                }
            }
            if let Some(dz_vals) = read_numeric_sequence(wall_root, &["thermal_layers", "dz"]) {
                for (j, &v) in dz_vals.iter().take(ndepth).enumerate() {
                    site.ehc.dz_wall[layer_idx * ndepth + j] = v;
                }
            }
        }
    }

    if let Some(roofs) = read_sequence(site_root, &["initial_states", "roofs"]) {
        for (layer_idx, roof_root) in roofs.iter().take(nlayer_ehc).enumerate() {
            if let Some(v) = read_numeric(roof_root, &["tin"]).or_else(|| {
                read_numeric_sequence(roof_root, &["temperature"])
                    .and_then(|vals| vals.first().copied())
            }) {
                site.ehc.tin_roof[layer_idx] = v;
            }
        }
    }

    if let Some(walls) = read_sequence(site_root, &["initial_states", "walls"]) {
        for (layer_idx, wall_root) in walls.iter().take(nlayer_ehc).enumerate() {
            if let Some(v) = read_numeric(wall_root, &["tin"]).or_else(|| {
                read_numeric_sequence(wall_root, &["temperature"])
                    .and_then(|vals| vals.first().copied())
            }) {
                site.ehc.tin_wall[layer_idx] = v;
            }
        }
    }
}

fn apply_building_archetype_overrides(site: &mut SuewsSite, site_root: &Value) {
    let Some(archetype_root) = get_path(site_root, &["properties", "building_archetype"]) else {
        return;
    };

    let Value::Mapping(archetype_map) = archetype_root else {
        return;
    };

    let mut mapped = building_archetype_prm_to_map(&site.building_archtype);

    for (field_key, field_value) in archetype_map {
        let Value::String(field_name_raw) = field_key else {
            continue;
        };

        let field_name = normalise_field_name(field_name_raw);

        if field_name == "metabolism_profile" {
            apply_day_profile_overrides(&mut mapped, field_value, "metabolismprofile", 144);
            continue;
        }

        if field_name == "appliance_profile" {
            apply_day_profile_overrides(&mut mapped, field_value, "applianceprofile", 144);
            continue;
        }

        if let Some(v) = read_numeric_value(field_value) {
            set_mapped_value(&mut mapped, &field_name, v);
        }
    }

    if let Ok(updated) = building_archetype_prm_from_map(&mapped) {
        site.building_archtype = updated;
    }
}

fn apply_stebbs_overrides(site: &mut SuewsSite, site_root: &Value) {
    let Some(stebbs_root) = get_path(site_root, &["properties", "stebbs"]) else {
        return;
    };

    let Value::Mapping(stebbs_map) = stebbs_root else {
        return;
    };

    let mut mapped = stebbs_prm_to_map(&site.stebbs);
    let mut archetype_mapped = building_archetype_prm_to_map(&site.building_archtype);

    for (field_key, field_value) in stebbs_map {
        let Value::String(field_name_raw) = field_key else {
            continue;
        };

        let field_name = normalise_field_name(field_name_raw);

        if field_name == "hot_water_flow_profile" {
            apply_day_profile_overrides(&mut mapped, field_value, "hot_water_flow_profile", 144);
            continue;
        }

        if field_name == "appliance_profile" {
            // STEBBS YAML stores appliance demand profile under `stebbs`, but
            // the physics consumes it from building archetype parameters.
            apply_day_profile_overrides(
                &mut archetype_mapped,
                field_value,
                "applianceprofile",
                144,
            );
            continue;
        }

        if let Some(v) = read_numeric_value(field_value) {
            set_mapped_value(&mut mapped, &field_name, v);
        }
    }

    if let Ok(updated) = stebbs_prm_from_map(&mapped) {
        site.stebbs = updated;
    }
    if let Ok(updated) = building_archetype_prm_from_map(&archetype_mapped) {
        site.building_archtype = updated;
    }
}

fn apply_site_overrides(
    site: &mut SuewsSite,
    root: &Value,
    site_root: &Value,
    nlayer: usize,
    ndepth: usize,
) {
    apply_conductance_overrides(site, root, site_root);
    apply_lumps_overrides(site, site_root);
    apply_spartacus_overrides(site, site_root);
    resize_site_variable_arrays(site, nlayer);
    apply_land_cover_overrides(site, site_root);
    apply_snow_overrides(site, site_root);
    apply_vertical_layers_overrides(site, site_root);
    apply_anthro_emis_overrides(site, site_root);
    apply_building_archetype_overrides(site, site_root);
    apply_stebbs_overrides(site, site_root);
    apply_ehc_overrides(site, site_root, ndepth);
}

fn apply_state_overrides(state: &mut SuewsState, site_root: &Value) {
    // STEBBS initial temperatures are provided under properties.stebbs in YAML
    // but consumed from stebbs_state by the physics core.
    if let Some(stebbs_root) = get_path(site_root, &["properties", "stebbs"]) {
        let init_out =
            read_normalized_numeric_from_mapping(stebbs_root, "initial_outdoor_temperature");
        let init_in =
            read_normalized_numeric_from_mapping(stebbs_root, "initial_indoor_temperature");
        let deep_soil =
            read_normalized_numeric_from_mapping(stebbs_root, "deep_soil_temperature");
        let mains_water =
            read_normalized_numeric_from_mapping(stebbs_root, "mains_water_temperature");
        let hot_water_setpoint = read_normalized_numeric_from_mapping(
            stebbs_root,
            "hot_water_heating_setpoint_temperature",
        );

        eprintln!(
            "DEBUG_STATE_MAP init_out={:?} init_in={:?} deep={:?} mains={:?} hwt_set={:?}",
            init_out, init_in, deep_soil, mains_water, hot_water_setpoint
        );

        if let Some(v) = init_out {
            state.stebbs_state.outdoor_air_start_temperature = v;
            state.stebbs_state.wall_outdoor_surface_temperature = v;
            state.stebbs_state.roof_outdoor_surface_temperature = v;
            state.stebbs_state.window_outdoor_surface_temperature = v;
            state.stebbs_state.ground_floor_outdoor_surface_temperature = v;
        }
        if let Some(v) = init_in {
            state.stebbs_state.indoor_air_start_temperature = v;
            state.stebbs_state.indoor_mass_start_temperature = v;
            state.stebbs_state.wall_indoor_surface_temperature = v;
            state.stebbs_state.roof_indoor_surface_temperature = v;
            state.stebbs_state.window_indoor_surface_temperature = v;
            state.stebbs_state.ground_floor_indoor_surface_temperature = v;
        }
        if let Some(v) = deep_soil {
            state.stebbs_state.deep_soil_temperature = v;
        }
        if let Some(v) = mains_water {
            state.stebbs_state.mains_water_temperature = v;
        }
        if let Some(v) = hot_water_setpoint {
            state.stebbs_state.water_tank_temperature = v;
            state.stebbs_state.internal_wall_water_tank_temperature = v;
            state.stebbs_state.external_wall_water_tank_temperature = v;
            let dhw_init = mains_water.unwrap_or(v);
            state.stebbs_state.domestic_hot_water_temperature_in_use_in_building = dhw_init;
            state.stebbs_state.internal_wall_dhw_vessel_temperature = dhw_init;
            state.stebbs_state.external_wall_dhw_vessel_temperature = dhw_init;
        }
    }

    let Some(initial_states_root) = get_path(site_root, &["initial_states"]) else {
        return;
    };

    for (surface_name, surface_idx) in SURFACE_YAML_ORDER {
        if let Some(v) = read_numeric(initial_states_root, &[surface_name, "soilstore"]) {
            state.hydro_state.soilstore_surf[surface_idx] = v;
        }
        if let Some(v) = read_numeric(initial_states_root, &[surface_name, "state"]) {
            state.hydro_state.state_surf[surface_idx] = v;
        }

        if let Some(v) = read_numeric(initial_states_root, &[surface_name, "snowfrac"]) {
            state.snow_state.snowfrac[surface_idx] = v;
        }
        if let Some(v) = read_numeric(initial_states_root, &[surface_name, "snowpack"]) {
            state.snow_state.snowpack[surface_idx] = v;
        }
        if let Some(v) = read_numeric(initial_states_root, &[surface_name, "icefrac"]) {
            state.snow_state.icefrac[surface_idx] = v;
        }
        if let Some(v) = read_numeric(initial_states_root, &[surface_name, "snowwater"]) {
            state.snow_state.snowwater[surface_idx] = v;
        }
        if let Some(v) = read_numeric(initial_states_root, &[surface_name, "snowdens"]) {
            state.snow_state.snowdens[surface_idx] = v;
        }

        if let Some(v) = read_numeric(initial_states_root, &[surface_name, "alb_id"])
            .or_else(|| read_numeric(initial_states_root, &[surface_name, "alb"]))
            .or_else(|| {
                read_numeric(
                    site_root,
                    &["properties", "land_cover", surface_name, "alb"],
                )
            })
        {
            state.phenology_state.alb[surface_idx] = v;
        }

        // Read storedrainprm from land_cover for this surface.
        // Row mapping matches Fortran storedrainprm(6, nsurf):
        //   [0] store_min, [1] drain_eq, [2] drain_coef_1,
        //   [3] drain_coef_2, [4] store_cap, [5] store_max
        if let Some(sdp_root) = get_path(
            site_root,
            &["properties", "land_cover", surface_name, "storedrainprm"],
        ) {
            if let Some(v) = read_numeric(sdp_root, &["store_min"]) {
                state.phenology_state.store_drain_prm[surface_idx][0] = v;
            }
            if let Some(v) = read_numeric(sdp_root, &["drain_eq"]) {
                state.phenology_state.store_drain_prm[surface_idx][1] = v;
            }
            if let Some(v) = read_numeric(sdp_root, &["drain_coef_1"]) {
                state.phenology_state.store_drain_prm[surface_idx][2] = v;
            }
            if let Some(v) = read_numeric(sdp_root, &["drain_coef_2"]) {
                state.phenology_state.store_drain_prm[surface_idx][3] = v;
            }
            if let Some(v) = read_numeric(sdp_root, &["store_cap"]) {
                state.phenology_state.store_drain_prm[surface_idx][4] = v;
            }
            if let Some(v) = read_numeric(sdp_root, &["store_max"]) {
                state.phenology_state.store_drain_prm[surface_idx][5] = v;
            }
        }
        // Fallback: use statelimit for store_min if not yet populated
        if state.phenology_state.store_drain_prm[surface_idx][0] == 0.0 {
            if let Some(v) = read_numeric(
                site_root,
                &["properties", "land_cover", surface_name, "statelimit"],
            ) {
                state.phenology_state.store_drain_prm[surface_idx][0] = v;
            }
        }

        let ndepth = state.heat_state.ndepth;
        if ndepth > 0 {
            if let Some(temps) =
                read_numeric_sequence(initial_states_root, &[surface_name, "temperature"])
            {
                let start = surface_idx * ndepth;
                if start + ndepth <= state.heat_state.temp_surf.len() {
                    for depth_idx in 0..temps.len().min(ndepth) {
                        state.heat_state.temp_surf[start + depth_idx] = temps[depth_idx];
                    }
                }
            }
        }
        if let Some(v) = read_numeric(initial_states_root, &[surface_name, "tsfc"]) {
            if let Some(slot) = state.heat_state.tsfc_surf.get_mut(surface_idx) {
                *slot = v;
            }
        }
    }

    if let Some(v) = read_numeric(initial_states_root, &["snowalb"]) {
        state.snow_state.snowalb = v;
    }
    if let Some(v) = read_numeric(initial_states_root, &["snowfallcum"]) {
        state.snow_state.snowfall_cum = v;
    }

    for (veg_name, veg_idx) in VEG_SURFACE_YAML_ORDER {
        if let Some(v) = read_numeric(initial_states_root, &[veg_name, "lai_id"]) {
            state.phenology_state.lai_id[veg_idx] = v;
        }
        if let Some(v) = read_numeric(initial_states_root, &[veg_name, "gdd_id"]) {
            state.phenology_state.gdd_id[veg_idx] = v;
        }
        if let Some(v) = read_numeric(initial_states_root, &[veg_name, "sdd_id"]) {
            state.phenology_state.sdd_id[veg_idx] = v;
        }
    }

    if let Some(v) = read_numeric(initial_states_root, &["dectr", "porosity_id"]) {
        state.phenology_state.porosity_id = v;
    }
    if let Some(v) = read_numeric(initial_states_root, &["dectr", "decidcap_id"]) {
        state.phenology_state.decidcap_id = v;
    }
    if let Some(v) = read_numeric(initial_states_root, &["dectr", "alb_id"]) {
        state.phenology_state.alb_dectr_id = v;
    }
    if let Some(v) = read_numeric(initial_states_root, &["evetr", "alb_id"]) {
        state.phenology_state.alb_evetr_id = v;
    }
    if let Some(v) = read_numeric(initial_states_root, &["grass", "alb_id"]) {
        state.phenology_state.alb_grass_id = v;
    }
    if let Some(v) = read_numeric(initial_states_root, &["tmin_id"]) {
        state.phenology_state.tmin_id = v;
    }
    if let Some(v) = read_numeric(initial_states_root, &["tmax_id"]) {
        state.phenology_state.tmax_id = v;
    }
    if let Some(v) = read_numeric(initial_states_root, &["lenday_id"]) {
        state.phenology_state.len_day_id = v;
    }

    if let Some(values) = read_numeric_sequence(initial_states_root, &["hdd_id"]) {
        assign_prefix(&mut state.anthroemis_state.hdd_id, values.as_slice());
    }
    if let Some(v) = read_numeric(initial_states_root, &["qn_av"]) {
        state.ohm_state.qn_av = v;
    }
    if let Some(v) = read_numeric(initial_states_root, &["qn_s_av"]) {
        state.ohm_state.qn_s_av = v;
    }
    if let Some(v) = read_numeric(initial_states_root, &["dqndt"]) {
        state.ohm_state.dqndt = v;
    }
    if let Some(v) = read_numeric(initial_states_root, &["dqnsdt"]) {
        state.ohm_state.dqnsdt = v;
    }

    if let Some(roofs) = read_sequence(initial_states_root, &["roofs"]) {
        let ndepth = state.heat_state.ndepth;
        for (layer_idx, roof_root) in roofs.iter().enumerate() {
            if let Some(v) = read_numeric(roof_root, &["soilstore"]) {
                if let Some(slot) = state.hydro_state.soilstore_roof.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if let Some(v) = read_numeric(roof_root, &["state"]) {
                if let Some(slot) = state.hydro_state.state_roof.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if let Some(v) = read_numeric(roof_root, &["tsfc"]) {
                if let Some(slot) = state.heat_state.tsfc_roof.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if ndepth > 0 {
                if let Some(temps) = read_numeric_sequence(roof_root, &["temperature"]) {
                    let start = layer_idx * ndepth;
                    if start + ndepth <= state.heat_state.temp_roof.len() {
                        for depth_idx in 0..temps.len().min(ndepth) {
                            state.heat_state.temp_roof[start + depth_idx] = temps[depth_idx];
                        }
                    }
                }
            }
        }
    }

    if let Some(walls) = read_sequence(initial_states_root, &["walls"]) {
        let ndepth = state.heat_state.ndepth;
        for (layer_idx, wall_root) in walls.iter().enumerate() {
            if let Some(v) = read_numeric(wall_root, &["soilstore"]) {
                if let Some(slot) = state.hydro_state.soilstore_wall.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if let Some(v) = read_numeric(wall_root, &["state"]) {
                if let Some(slot) = state.hydro_state.state_wall.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if let Some(v) = read_numeric(wall_root, &["tsfc"]) {
                if let Some(slot) = state.heat_state.tsfc_wall.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if ndepth > 0 {
                if let Some(temps) = read_numeric_sequence(wall_root, &["temperature"]) {
                    let start = layer_idx * ndepth;
                    if start + ndepth <= state.heat_state.temp_wall.len() {
                        for depth_idx in 0..temps.len().min(ndepth) {
                            state.heat_state.temp_wall[start + depth_idx] = temps[depth_idx];
                        }
                    }
                }
            }
        }
    }
}

fn apply_site_scalar_overrides(site_scalars: &mut SiteScalars, site_root: &Value) {
    if let Some(v) = read_numeric(site_root, &["properties", "lat"]) {
        site_scalars.lat = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "lng"])
        .or_else(|| read_numeric(site_root, &["properties", "lon"]))
    {
        site_scalars.lon = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "alt"]) {
        site_scalars.alt = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "timezone"]) {
        site_scalars.timezone = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "surfacearea"]) {
        site_scalars.surfacearea = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "z"]) {
        site_scalars.z = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "z0m_in"]) {
        site_scalars.z0m_in = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "zdm_in"]) {
        site_scalars.zdm_in = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "pipecapacity"]) {
        site_scalars.pipecapacity = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "runofftowater"]) {
        site_scalars.runofftowater = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "narp_trans_site"]) {
        site_scalars.narp_trans_site = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "CO2PointSource"]) {
        site_scalars.co2_point_source = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "flowchange"]) {
        site_scalars.flowchange = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "n_buildings"]) {
        site_scalars.n_buildings = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "h_std"]) {
        site_scalars.h_std = v;
    }
    if let Some(v) = read_numeric(site_root, &["properties", "lambda_c"]) {
        site_scalars.lambda_c = v;
    }
    if let Some(v) = read_i32(site_root, &["gridiv"]) {
        site_scalars.gridiv = v;
    }

    for (surface_name, idx) in SURFACE_YAML_ORDER {
        if let Some(v) = read_numeric(
            site_root,
            &["properties", "land_cover", surface_name, "sfr"],
        ) {
            site_scalars.sfr_surf[idx] = v;
        }
    }
}

fn read_sites_indexed<'a>(root: &'a Value, idx: usize) -> Option<&'a Value> {
    let sites = get_path(root, &["sites"])?;
    match sites {
        Value::Sequence(items) => items.get(idx),
        _ => None,
    }
}

fn read_forcing_rel(root: &Value) -> Option<String> {
    read_string(root, &["model", "control", "forcing_file"])
        .or_else(|| read_string(root, &["model", "control", "forcing_file", "value"]))
}

pub fn load_run_config_from_value(root: &Value) -> Result<RunConfig, String> {
    let mut timer = suews_timer_default_from_fortran().map_err(|e| e.to_string())?;
    let mut config = suews_config_default_from_fortran().map_err(|e| e.to_string())?;
    let mut site = suews_site_default_from_fortran().map_err(|e| e.to_string())?;
    let mut site_scalars = SiteScalars::default();
    let mut state = suews_state_default_from_fortran().map_err(|e| e.to_string())?;

    let site_root = read_sites_indexed(&root, 0)
        .ok_or_else(|| "config must contain at least one site under `sites`".to_string())?;

    apply_config_overrides(&mut config, &root);

    let nlayer = read_i32(site_root, &["properties", "vertical_layers", "nlayer"]).unwrap_or(5);
    let ndepth = read_i32(site_root, &["properties", "vertical_layers", "ndepth"]).unwrap_or(5);

    apply_site_overrides(
        &mut site,
        &root,
        site_root,
        nlayer as usize,
        ndepth as usize,
    );
    apply_site_scalar_overrides(&mut site_scalars, site_root);

    if let Some(tstep) = read_i32(&root, &["model", "control", "tstep"]) {
        timer.tstep = tstep;
        timer.tstep_prev = tstep;
    }

    // Resize variable-length arrays BEFORE applying state overrides so that
    // vectors like tsfc_surf have the correct length when values are assigned.
    resize_state_variable_arrays(&mut state, nlayer as usize, ndepth as usize);

    apply_state_overrides(&mut state, site_root);

    let output_dir = read_string(root, &["model", "control", "output_file", "path"])
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("./output"));

    Ok(RunConfig {
        timer,
        config,
        site,
        site_scalars,
        state,
        forcing_path: read_forcing_rel(root)
            .map(PathBuf::from)
            .unwrap_or_default(),
        output_dir,
        nlayer,
        ndepth,
    })
}

pub fn load_run_config(path: &Path) -> Result<RunConfig, String> {
    let yaml_text = fs::read_to_string(path)
        .map_err(|e| format!("failed to read config {}: {e}", path.display()))?;
    let root: Value = serde_yaml::from_str(&yaml_text).map_err(|e| format!("invalid YAML: {e}"))?;
    let mut run_cfg = load_run_config_from_value(&root)?;

    let forcing_rel = read_forcing_rel(&root)
        .ok_or_else(|| "`model.control.forcing_file` is missing".to_string())?;
    let base_dir = path.parent().unwrap_or_else(|| Path::new("."));
    run_cfg.forcing_path = base_dir.join(forcing_rel);
    // Resolve output_dir relative to config file location
    if run_cfg.output_dir.is_relative() {
        run_cfg.output_dir = base_dir.join(&run_cfg.output_dir);
    }

    if !run_cfg.forcing_path.exists() {
        return Err(format!(
            "forcing file does not exist: {}",
            run_cfg.forcing_path.display()
        ));
    }

    Ok(run_cfg)
}

pub fn load_run_config_from_str(yaml_str: &str) -> Result<RunConfig, String> {
    let root: Value = serde_yaml::from_str(yaml_str).map_err(|e| format!("invalid YAML: {e}"))?;
    load_run_config_from_value(&root)
}

/// Resize variable-length arrays in HydroState and HeatState to match nlayer/ndepth.
/// The Fortran unpack routines expect these arrays to be present in the flat buffer.
fn resize_state_variable_arrays(state: &mut SuewsState, nlayer: usize, ndepth: usize) {
    fn ensure_len(v: &mut Vec<f64>, n: usize) {
        v.resize(n, 0.0);
    }

    // HydroState: 6 arrays of length nlayer
    ensure_len(&mut state.hydro_state.soilstore_roof, nlayer);
    ensure_len(&mut state.hydro_state.state_roof, nlayer);
    ensure_len(&mut state.hydro_state.soilstore_wall, nlayer);
    ensure_len(&mut state.hydro_state.state_wall, nlayer);
    ensure_len(&mut state.hydro_state.ev_roof, nlayer);
    ensure_len(&mut state.hydro_state.ev_wall, nlayer);

    // HeatState: record dimensions and resize all variable-length arrays
    state.heat_state.nlayer = nlayer;
    state.heat_state.ndepth = ndepth;

    // 2 arrays of nlayer*ndepth
    ensure_len(&mut state.heat_state.temp_roof, nlayer * ndepth);
    ensure_len(&mut state.heat_state.temp_wall, nlayer * ndepth);
    // 2 arrays of nsurf*ndepth
    ensure_len(&mut state.heat_state.temp_surf, NSURF * ndepth);
    ensure_len(&mut state.heat_state.temp_surf_dyohm, NSURF * ndepth);
    // 14 arrays of nlayer
    ensure_len(&mut state.heat_state.tsfc_roof, nlayer);
    ensure_len(&mut state.heat_state.tsfc_wall, nlayer);
    ensure_len(&mut state.heat_state.tsfc_roof_stepstart, nlayer);
    ensure_len(&mut state.heat_state.tsfc_wall_stepstart, nlayer);
    ensure_len(&mut state.heat_state.qs_roof, nlayer);
    ensure_len(&mut state.heat_state.qn_roof, nlayer);
    ensure_len(&mut state.heat_state.qe_roof, nlayer);
    ensure_len(&mut state.heat_state.qh_roof, nlayer);
    ensure_len(&mut state.heat_state.qh_resist_roof, nlayer);
    ensure_len(&mut state.heat_state.qs_wall, nlayer);
    ensure_len(&mut state.heat_state.qn_wall, nlayer);
    ensure_len(&mut state.heat_state.qe_wall, nlayer);
    ensure_len(&mut state.heat_state.qh_wall, nlayer);
    ensure_len(&mut state.heat_state.qh_resist_wall, nlayer);
    // 3 arrays of nsurf
    ensure_len(&mut state.heat_state.tsfc_surf, NSURF);
    ensure_len(&mut state.heat_state.tsfc_surf_dyohm, NSURF);
    ensure_len(&mut state.heat_state.tsfc_surf_stepstart, NSURF);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_stebbs_and_building_archetype_sections() {
        let yaml_str =
            include_str!("../../../test/fixtures/data_test/stebbs_test/sample_config.yml");
        let root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let run_cfg = load_run_config_from_value(&root).expect("run config should parse");

        assert!(run_cfg.site.building_archtype.wallthickness > 0.0);
        assert!(run_cfg.site.building_archtype.wallextthickness > 0.0);
        assert!(run_cfg.site.building_archtype.roofthickness > 0.0);

        assert!(run_cfg.site.stebbs.wall_internal_convection_coefficient > 0.0);
        assert!(run_cfg.site.stebbs.water_tank_surface_area > 0.0);
        assert!(run_cfg.site.stebbs.hot_water_flow_profile[0][43] >= 0.0);
    }

    #[test]
    fn parses_ohm_legacy_surface_field_names() {
        let yaml_str =
            include_str!("../../../test/fixtures/data_test/stebbs_test/sample_config.yml");
        let root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let run_cfg = load_run_config_from_value(&root).expect("run config should parse");

        // Legacy yaml uses ch_anohm/rho_cp_anohm/k_anohm in land_cover.
        assert!(run_cfg.site.lc_paved.ohm.chanohm > 0.0);
        assert!(run_cfg.site.lc_paved.ohm.cpanohm > 0.0);
        assert!(run_cfg.site.lc_paved.ohm.kkanohm > 0.0);
    }

    #[test]
    fn maps_vertical_layer_thermal_parameters_into_ehc() {
        let yaml_str =
            include_str!("../../../test/fixtures/data_test/stebbs_test/sample_config.yml");
        let root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let run_cfg = load_run_config_from_value(&root).expect("run config should parse");

        let ndepth = run_cfg.ndepth as usize;
        assert!(ndepth > 0);

        assert!((run_cfg.site.ehc.dz_roof[0] - 0.03).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.k_roof[0] - 0.106).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.cp_roof[0] - 997_274.2).abs() < 1.0e-6);
        assert!((run_cfg.site.ehc.dz_wall[0] - 0.231).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.k_wall[0] - 0.755).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.cp_wall[0] - 1_724_700.0).abs() < 1.0e-6);

        let second_layer_first_depth = ndepth;
        assert!((run_cfg.site.ehc.dz_wall[second_layer_first_depth] - 0.231).abs() < 1.0e-12);

        assert!((run_cfg.site.ehc.soil_storecap_roof[0] - 120.0).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.state_limit_roof[0] - 5.0).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.wet_thresh_roof[0] - 5.0).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.soil_storecap_wall[0] - 120.0).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.state_limit_wall[0] - 5.0).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.wet_thresh_wall[0] - 5.0).abs() < 1.0e-12);

        assert!((run_cfg.site.ehc.tin_surf[0] - 17.540002822875977).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.tin_roof[0] - 17.540002822875977).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.tin_wall[0] - 17.540002822875977).abs() < 1.0e-12);
    }
}
