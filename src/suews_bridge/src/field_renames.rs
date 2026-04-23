//! YAML key-name preprocessor bridging Python snake_case and legacy fused
//! spellings that the hand-written Rust parser in `yaml_config.rs` expects.
//!
//! # Why this exists
//!
//! The Python data model (#1308) renamed 59 YAML keys from their legacy
//! fused spellings (`netradiationmethod`, `soildepth`, `baset`, ...) to
//! `snake_case` (`net_radiation_method`, `soil_depth`, `base_temperature`,
//! ...). On the Python side, a `@model_validator(mode='before')` shim
//! accepts both spellings transparently. The Rust CLI (`suews run
//! config.yml`) bypasses that shim and its parser indexes every path with
//! the old fused name as a hardcoded string literal. A new-style YAML
//! submitted directly to the CLI silently falls through to
//! `.unwrap_or(default)` for every renamed field — wrong physics, no
//! error.
//!
//! # What this does
//!
//! `normalize_field_names` walks the parsed `serde_yaml::Value` tree and
//! renames every matching new-style key back to its legacy spelling, in
//! place, before the rest of the parser runs. The parser itself is
//! unchanged — this module is the only seam that knows about the rename.
//!
//! # Source of truth
//!
//! [`FIELD_RENAMES`] below mirrors `ALL_FIELD_RENAMES` in
//! `src/supy/data_model/core/field_renames.py`. Drift is enforced by
//! `scripts/lint/check_rust_yaml_aliases.py`.

use serde_yaml::Value;

/// Ordered list of `(new_name, old_name)` pairs.
///
/// Grouped to match the Python registry sections in
/// `src/supy/data_model/core/field_renames.py`. Each pair maps the
/// current final name to its legacy fused spelling (what the Fortran
/// state is keyed by).
/// Total: ~100 pairs (expanded for gh#1334 STEBBS + Snow snake_case sweep).
pub const FIELD_RENAMES: &[(&str, &str)] = &[
    // ModelPhysics (17) — fused -> final (Cat 2+3, gh#1321) + flags
    ("net_radiation", "netradiationmethod"),
    ("emissions", "emissionsmethod"),
    ("storage_heat", "storageheatmethod"),
    ("ohm_inc_qf", "ohmincqf"),
    ("roughness_length_momentum", "roughlenmommethod"),
    ("roughness_length_heat", "roughlenheatmethod"),
    ("stability", "stabilitymethod"),
    ("soil_moisture_deficit", "smdmethod"),
    ("water_use", "waterusemethod"),
    ("roughness_sublayer", "rslmethod"),
    ("frontal_area_index", "faimethod"),
    ("roughness_sublayer_level", "rsllevel"),
    ("surface_conductance", "gsmodel"),
    ("snow_use", "snowuse"),
    ("stebbs", "stebbsmethod"),
    ("outer_cap_fraction", "rcmethod"),
    ("setpoint", "setpointmethod"),
    // SurfaceProperties (11)
    ("soil_depth", "soildepth"),
    ("soil_store_capacity", "soilstorecap"),
    ("state_limit", "statelimit"),
    ("wet_threshold", "wetthresh"),
    ("saturated_hydraulic_conductivity", "sathydraulicconduct"),
    ("soil_density", "soildensity"),
    ("storage_drain_params", "storedrainprm"),
    ("snowpack_limit", "snowpacklimit"),
    ("irrigation_fraction", "irrfrac"),
    ("ohm_threshold_summer_winter", "ohm_threshsw"),
    ("ohm_threshold_wet_dry", "ohm_threshwd"),
    // LAIParams (8)
    ("base_temperature", "baset"),
    ("gdd_full", "gddfull"),
    ("base_temperature_senescence", "basete"),
    ("sdd_full", "sddfull"),
    ("lai_min", "laimin"),
    ("lai_max", "laimax"),
    ("lai_power", "laipower"),
    ("lai_type", "laitype"),
    // VegetatedSurfaceProperties (4)
    ("max_conductance", "maxconductance"),
    ("beta_bio_co2", "beta_bioco2"),
    ("alpha_bio_co2", "alpha_bioco2"),
    ("theta_bio_co2", "theta_bioco2"),
    // EvetrProperties (2)
    ("fai_evergreen_tree", "faievetree"),
    ("height_evergreen_tree", "evetreeh"),
    // DectrProperties (6)
    ("fai_deciduous_tree", "faidectree"),
    ("height_deciduous_tree", "dectreeh"),
    ("porosity_min_deciduous", "pormin_dec"),
    ("porosity_max_deciduous", "pormax_dec"),
    ("capacity_max_deciduous", "capmax_dec"),
    ("capacity_min_deciduous", "capmin_dec"),
    // ArchetypeProperties — gh#1334 expansion renames where the compact
    // strip-underscores match against the Rust struct field fails. The new
    // snake_case name must be rewritten to the legacy Rust field name
    // before the yaml_config parser runs. Fields whose new name compact-
    // matches directly (e.g. `wall_thickness` -> `wallthickness`) are not
    // listed — `set_mapped_value` resolves them automatically.
    //
    // External (Wallext / Roofext) cluster from gh#1327 — the new name
    // jumped straight to snake_case under gh#1334 (skipping the Cat 5
    // PascalCase intermediate); the compat aliases section below keeps
    // the PascalCase intermediate loading for users on Schema 2026.5.dev1.
    ("wall_external_thickness", "wallextthickness"),
    ("wall_external_effective_conductivity", "wallexteffectiveconductivity"),
    ("wall_external_density", "wallextdensity"),
    ("wall_external_specific_heat_capacity", "wallextcp"),
    ("roof_external_thickness", "roofextthickness"),
    ("roof_external_effective_conductivity", "roofexteffectiveconductivity"),
    ("roof_external_density", "roofextdensity"),
    ("roof_external_specific_heat_capacity", "roofextcp"),
    // Archetype abbreviation expansions (Cp -> specific_heat_capacity,
    // Cap -> heat_capacity, Frac -> fraction, WWR / Ratio / stebbs_Height /
    // FloorThickness semantic renames, Absorbtivity spelling fix)
    ("wall_specific_heat_capacity", "wallcp"),
    ("roof_specific_heat_capacity", "roofcp"),
    ("window_specific_heat_capacity", "windowcp"),
    ("internal_mass_specific_heat_capacity", "internalmasscp"),
    ("ground_floor_specific_heat_capacity", "groundfloorcp"),
    ("wall_outer_heat_capacity_fraction", "walloutercapfrac"),
    ("roof_outer_heat_capacity_fraction", "roofoutercapfrac"),
    ("window_to_wall_ratio", "wwr"),
    ("internal_volume_ratio", "ratiointernalvolume"),
    ("building_height", "stebbs_height"),
    ("ground_floor_thickness", "floorthickness"),
    ("wall_absorptivity", "wallabsorbtivity"),
    ("roof_absorptivity", "roofabsorbtivity"),
    ("window_absorptivity", "windowabsorbtivity"),
    // SnowParams — legacy fused -> gh#1334 final snake_case.
    //
    // Six entries retarget from the 2026.5 intermediate to the #1334
    // final (preciplimit -> temperature_rain_snow_threshold etc.); five
    // new entries are for fields whose 2026.5 shape had no fused
    // predecessor (tau_a, narp_emis_snow, etc.) — the "legacy" name is
    // what the Fortran bridge still reads.
    ("water_holding_capacity_max", "crwmax"),
    ("water_holding_capacity_min", "crwmin"),
    ("temperature_rain_snow_threshold", "preciplimit"),
    ("precipitation_threshold_albedo_reset", "preciplimitalb"),
    ("snow_albedo_max", "snowalbmax"),
    ("snow_albedo_min", "snowalbmin"),
    ("snow_density_min", "snowdensmin"),
    ("snow_density_max", "snowdensmax"),
    ("snow_depth_limit_building", "snowlimbldg"),
    ("snow_depth_limit_paved", "snowlimpaved"),
    ("temperature_melt_factor", "tempmeltfact"),
    ("radiation_melt_factor", "radmeltfact"),
    ("tau_cold_snow", "tau_a"),
    ("tau_melting_snow", "tau_f"),
    ("tau_refreezing_snow", "tau_r"),
    ("snow_profile_24hr", "snowprof_24hr"),
    ("narp_emissivity_snow", "narp_emis_snow"),
];

/// Additional compatibility aliases for the short-lived Schema 2026.5
/// ModelPhysics shape (`net_radiation_method`, `gs_model`, ...).
///
/// These are intentionally kept separate from `FIELD_RENAMES` so the
/// Rust/Python parity lint can continue to compare the one-to-one
/// final-name registry against Python `ALL_FIELD_RENAMES`.
pub const FIELD_COMPAT_ALIASES: &[(&str, &str)] = &[
    // Schema 2026.5 ModelPhysics intermediate (gh#1321)
    ("net_radiation_method", "netradiationmethod"),
    ("emissions_method", "emissionsmethod"),
    ("storage_heat_method", "storageheatmethod"),
    ("roughness_length_momentum_method", "roughlenmommethod"),
    ("roughness_length_heat_method", "roughlenheatmethod"),
    ("stability_method", "stabilitymethod"),
    ("water_use_method", "waterusemethod"),
    ("stebbs_method", "stebbsmethod"),
    ("smd_method", "smdmethod"),
    ("rsl_method", "rslmethod"),
    ("rsl_level", "rsllevel"),
    ("fai_method", "faimethod"),
    ("rc_method", "rcmethod"),
    ("gs_model", "gsmodel"),
    // Schema 2026.5.dev1 STEBBS ArchetypeProperties Cat 5 intermediate
    // (gh#1327). After gh#1334 the final names are snake_case; the
    // PascalCase intermediate stays accepted for back-compat.
    ("WallExternalThickness", "wallextthickness"),
    ("WallExternalEffectiveConductivity", "wallexteffectiveconductivity"),
    ("WallExternalDensity", "wallextdensity"),
    ("WallExternalCp", "wallextcp"),
    ("RoofExternalThickness", "roofextthickness"),
    ("RoofExternalEffectiveConductivity", "roofexteffectiveconductivity"),
    ("RoofExternalDensity", "roofextdensity"),
    ("RoofExternalCp", "roofextcp"),
    // Schema 2026.5.dev2 SnowParams intermediate (gh#1334)
    ("precip_limit", "preciplimit"),
    ("precip_limit_albedo", "preciplimitalb"),
    ("snow_limit_building", "snowlimbldg"),
    ("snow_limit_paved", "snowlimpaved"),
    ("temp_melt_factor", "tempmeltfact"),
    ("rad_melt_factor", "radmeltfact"),
];

fn legacy_name_for(key: &str) -> Option<(&'static str, &'static str)> {
    FIELD_RENAMES
        .iter()
        .chain(FIELD_COMPAT_ALIASES.iter())
        .find(|(new_name, _)| *new_name == key)
        .map(|(new_name, old_name)| (*new_name, *old_name))
}

/// Recursively rewrite new snake_case keys to their legacy fused spellings
/// so the downstream parser in `yaml_config.rs` keeps seeing the keys it
/// has always read.
///
/// Behaviour
/// - Mappings are walked entry by entry. When a key matches a `new_name`
///   in `FIELD_RENAMES` and the corresponding `old_name` is not already
///   present in the same mapping, the entry is reinserted under
///   `old_name`.
/// - If both spellings are present in the same mapping, the walk stops
///   with an error instead of silently choosing one. This matches the
///   Python rename helpers, which reject the same ambiguous user input.
/// - Sequences are walked element by element.
/// - Scalars are left untouched.
///
/// The walk is in place, so callers that keep the tree around see the
/// renamed keys on subsequent reads. Idempotent — running twice on the
/// same tree is a no-op.
pub fn normalize_field_names(root: &mut Value) -> Result<(), String> {
    normalize_field_names_at(root, "<root>")
}

fn normalize_field_names_at(root: &mut Value, path: &str) -> Result<(), String> {
    match root {
        Value::Mapping(map) => {
            let rename_candidates: Vec<(Value, &'static str, &'static str)> = map
                .iter()
                .filter_map(|(key, _)| match key {
                    Value::String(k) => {
                        legacy_name_for(k.as_str()).map(|(new_name, old_name)| {
                            (key.clone(), new_name, old_name)
                        })
                    }
                    _ => None,
                })
                .collect();

            for (new_key, new_name, old_name) in rename_candidates {
                let old_key = Value::String(old_name.to_string());
                if map.contains_key(&old_key) {
                    return Err(format!(
                        "Both '{old_name}' (deprecated) and '{new_name}' are present at {path}. Use only '{new_name}'."
                    ));
                }
                if let Some(value) = map.remove(&new_key) {
                    map.insert(old_key, value);
                }
            }

            for (key, value) in map.iter_mut() {
                let child_path = match key {
                    Value::String(k) => {
                        if path == "<root>" {
                            k.clone()
                        } else {
                            format!("{path}.{k}")
                        }
                    }
                    _ => path.to_string(),
                };
                normalize_field_names_at(value, &child_path)?;
            }
        }
        Value::Sequence(seq) => {
            for (idx, item) in seq.iter_mut().enumerate() {
                let child_path = if path == "<root>" {
                    format!("[{idx}]")
                } else {
                    format!("{path}[{idx}]")
                };
                normalize_field_names_at(item, &child_path)?;
            }
        }
        _ => {}
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_yaml::from_str;

    #[test]
    fn field_renames_registry_has_expected_size() {
        // Matches the Python ALL_FIELD_RENAMES total (see field_renames.py).
        // Bump when ModelPhysics / SurfaceProperties / ... dicts change.
        assert_eq!(FIELD_RENAMES.len(), 68);
    }

    #[test]
    fn field_renames_entries_are_unique() {
        use std::collections::HashSet;
        let new_names: HashSet<&str> = FIELD_RENAMES.iter().map(|(n, _)| *n).collect();
        let old_names: HashSet<&str> = FIELD_RENAMES.iter().map(|(_, o)| *o).collect();
        assert_eq!(new_names.len(), FIELD_RENAMES.len(), "duplicate new names");
        assert_eq!(old_names.len(), FIELD_RENAMES.len(), "duplicate old names");
    }

    #[test]
    fn field_compat_aliases_entries_are_unique() {
        use std::collections::HashSet;
        let new_names: HashSet<&str> = FIELD_COMPAT_ALIASES.iter().map(|(n, _)| *n).collect();
        assert_eq!(
            new_names.len(),
            FIELD_COMPAT_ALIASES.len(),
            "duplicate compat alias names"
        );
    }

    #[test]
    fn renames_top_level_new_key_to_legacy() {
        let mut root: Value =
            from_str("model:\n  physics:\n    net_radiation: {value: 3}\n").unwrap();
        normalize_field_names(&mut root).unwrap();
        let physics = &root["model"]["physics"];
        assert!(physics.get("netradiationmethod").is_some());
        assert!(physics.get("net_radiation").is_none());
    }

    #[test]
    fn leaves_legacy_keys_unchanged() {
        let yaml = "model:\n  physics:\n    netradiationmethod: {value: 3}\n    storageheatmethod: {value: 1}\n";
        let mut root: Value = from_str(yaml).unwrap();
        let before = root.clone();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(root, before, "idempotent on already-legacy YAML");
    }

    #[test]
    fn renames_mixed_input_per_key() {
        let yaml = "model:\n  physics:\n    net_radiation: {value: 3}\n    storageheatmethod: {value: 1}\n";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let physics = &root["model"]["physics"];
        assert!(physics.get("netradiationmethod").is_some());
        assert!(physics.get("storageheatmethod").is_some());
        assert!(physics.get("net_radiation").is_none());
    }

    #[test]
    fn renames_intermediate_physics_key_to_legacy() {
        let mut root: Value =
            from_str("model:\n  physics:\n    net_radiation_method: {value: 3}\n").unwrap();
        normalize_field_names(&mut root).unwrap();
        let physics = &root["model"]["physics"];
        assert!(physics.get("netradiationmethod").is_some());
        assert!(physics.get("net_radiation_method").is_none());
    }

    #[test]
    fn rejects_when_both_spellings_present() {
        let yaml = "sites:\n  - properties:\n      land_cover:\n        paved:\n          soildepth: {value: 0.5}\n          soil_depth: {value: 0.2}\n";
        let mut root: Value = from_str(yaml).unwrap();
        let err = normalize_field_names(&mut root).expect_err("duplicate spellings must fail");
        assert_eq!(
            err,
            "Both 'soildepth' (deprecated) and 'soil_depth' are present at sites[0].properties.land_cover.paved. Use only 'soil_depth'."
        );
    }

    #[test]
    fn recurses_into_sequences_of_mappings() {
        // Renames inside a list entry are still applied.
        let yaml = "sites:\n  - properties:\n      land_cover:\n        paved:\n          soil_depth: {value: 0.35}\n";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let paved = &root["sites"][0]["properties"]["land_cover"]["paved"];
        assert!(paved.get("soildepth").is_some());
        assert!(paved.get("soil_depth").is_none());
    }

    #[test]
    fn handles_missing_keys_without_panic() {
        let mut root: Value = from_str("model:\n  control:\n    tstep: 300\n").unwrap();
        let before = root.clone();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(
            root, before,
            "mapping without renamed keys must be untouched"
        );
    }

    #[test]
    fn normalises_nested_lai_and_snow_params() {
        let yaml = "sites:\n  - properties:\n      land_cover:\n        grass:\n          lai:\n            base_temperature: {value: 5.0}\n            lai_max: {value: 6.0}\n      snow:\n        snow_albedo_max: {value: 0.85}\n        temp_melt_factor: {value: 0.12}\n";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let lai = &root["sites"][0]["properties"]["land_cover"]["grass"]["lai"];
        assert!(lai.get("baset").is_some());
        assert!(lai.get("laimax").is_some());
        assert!(lai.get("base_temperature").is_none());

        let snow = &root["sites"][0]["properties"]["snow"];
        assert!(snow.get("snowalbmax").is_some());
        assert!(snow.get("tempmeltfact").is_some());
    }

    #[test]
    fn idempotent_on_second_pass() {
        let yaml =
            "model:\n  physics:\n    net_radiation_method: {value: 3}\n    snow_use: {value: 0}\n";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let after_first = root.clone();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(root, after_first);
    }

    #[test]
    fn rejects_when_old_and_intermediate_spellings_present() {
        let yaml =
            "model:\n  physics:\n    netradiationmethod: {value: 3}\n    net_radiation_method: {value: 2}\n";
        let mut root: Value = from_str(yaml).unwrap();
        let err = normalize_field_names(&mut root).expect_err("duplicate spellings must fail");
        assert_eq!(
            err,
            "Both 'netradiationmethod' (deprecated) and 'net_radiation_method' are present at model.physics. Use only 'net_radiation_method'."
        );
    }
}
