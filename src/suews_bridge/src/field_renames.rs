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
/// Mirrors `ALL_FIELD_RENAMES` in
/// `src/supy/data_model/core/field_renames.py` one-to-one, by section.
/// Every pair maps the current final snake_case name to the legacy
/// spelling the Pydantic shim accepts (case-preserving — what users
/// wrote in legacy YAMLs). The preprocessor's downstream
/// `normalise_field_name` then lowercases/fuses before the hand-written
/// parser reads the Fortran-indexed key.
/// Total: 177 pairs (gh#1334 full STEBBS + Snow snake_case sweep).
pub const FIELD_RENAMES: &[(&str, &str)] = &[
    // ModelPhysics (17) — fused -> final (Cat 2+3, gh#1321) + flags
    ("net_radiation", "netradiationmethod"),
    ("emissions", "emissionsmethod"),
    ("storage_heat", "storageheatmethod"),
    ("roughness_length_momentum", "roughlenmommethod"),
    ("roughness_length_heat", "roughlenheatmethod"),
    ("stability", "stabilitymethod"),
    ("soil_moisture_deficit", "smdmethod"),
    ("water_use", "waterusemethod"),
    ("roughness_sublayer", "rslmethod"),
    ("frontal_area_index", "faimethod"),
    ("roughness_sublayer_level", "rsllevel"),
    ("surface_conductance", "gsmodel"),
    ("stebbs", "stebbsmethod"),
    ("outer_cap_fraction", "rcmethod"),
    ("setpoint", "setpointmethod"),
    ("ohm_inc_qf", "ohmincqf"),
    ("snow_use", "snowuse"),
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
    // ArchetypeProperties (62) — current dev7 snake_case targets (gh#1390
    // Rule-2 reorder applied) mapped back to the user's legacy YAML
    // spelling. Pre-gh#1327 fused Wallext/Roofext cluster is preserved
    // here; the gh#1329 PascalCase intermediate (WallExternalThickness
    // etc.) lives in FIELD_COMPAT_ALIASES below.
    ("thickness_wall_outer", "WallextThickness"),
    (
        "conductivity_wall_outer",
        "WallextEffectiveConductivity",
    ),
    ("density_wall_outer", "WallextDensity"),
    ("specific_heat_capacity_wall_outer", "WallextCp"),
    ("thickness_roof_outer", "RoofextThickness"),
    (
        "conductivity_roof_outer",
        "RoofextEffectiveConductivity",
    ),
    ("density_roof_outer", "RoofextDensity"),
    ("specific_heat_capacity_roof_outer", "RoofextCp"),
    ("building_type", "BuildingType"),
    ("building_name", "BuildingName"),
    ("building_count", "BuildingCount"),
    ("occupants", "Occupants"),
    ("building_height", "stebbs_Height"),
    ("footprint_area", "FootprintArea"),
    ("wall_external_area", "WallExternalArea"),
    ("internal_volume_ratio", "RatioInternalVolume"),
    ("internal_mass_area", "InternalMassArea"),
    ("window_to_wall_ratio", "WWR"),
    ("thickness_wall", "WallThickness"),
    ("conductivity_wall", "WallEffectiveConductivity"),
    ("density_wall", "WallDensity"),
    ("specific_heat_capacity_wall", "WallCp"),
    ("fraction_wall_heat_capacity_outer", "WallOuterCapFrac"),
    ("emissivity_wall_external", "WallExternalEmissivity"),
    ("emissivity_wall_internal", "WallInternalEmissivity"),
    ("transmissivity_wall_external", "WallTransmissivity"),
    ("absorptivity_wall_external", "WallAbsorbtivity"),
    ("reflectivity_wall_external", "WallReflectivity"),
    ("thickness_roof", "RoofThickness"),
    ("conductivity_roof", "RoofEffectiveConductivity"),
    ("density_roof", "RoofDensity"),
    ("specific_heat_capacity_roof", "RoofCp"),
    ("fraction_roof_heat_capacity_outer", "RoofOuterCapFrac"),
    ("emissivity_roof_external", "RoofExternalEmissivity"),
    ("emissivity_roof_internal", "RoofInternalEmissivity"),
    ("transmissivity_roof_external", "RoofTransmissivity"),
    ("absorptivity_roof_external", "RoofAbsorbtivity"),
    ("reflectivity_roof_external", "RoofReflectivity"),
    ("thickness_ground_floor", "FloorThickness"),
    (
        "conductivity_ground_floor",
        "GroundFloorEffectiveConductivity",
    ),
    ("density_ground_floor", "GroundFloorDensity"),
    ("specific_heat_capacity_ground_floor", "GroundFloorCp"),
    ("thickness_window", "WindowThickness"),
    (
        "conductivity_window",
        "WindowEffectiveConductivity",
    ),
    ("density_window", "WindowDensity"),
    ("specific_heat_capacity_window", "WindowCp"),
    ("emissivity_window_external", "WindowExternalEmissivity"),
    ("emissivity_window_internal", "WindowInternalEmissivity"),
    ("transmissivity_window_external", "WindowTransmissivity"),
    ("absorptivity_window_external", "WindowAbsorbtivity"),
    ("reflectivity_window_external", "WindowReflectivity"),
    ("density_internal_mass", "InternalMassDensity"),
    ("specific_heat_capacity_internal_mass", "InternalMassCp"),
    ("emissivity_internal_mass", "InternalMassEmissivity"),
    ("max_heating_power", "MaxHeatingPower"),
    ("hot_water_tank_volume", "WaterTankWaterVolume"),
    (
        "maximum_hot_water_heating_power",
        "MaximumHotWaterHeatingPower",
    ),
    ("heating_setpoint_temperature", "HeatingSetpointTemperature"),
    ("cooling_setpoint_temperature", "CoolingSetpointTemperature"),
    (
        "heating_setpoint_temperature_profile",
        "HeatingSetpointTemperatureProfile",
    ),
    (
        "cooling_setpoint_temperature_profile",
        "CoolingSetpointTemperatureProfile",
    ),
    ("metabolism_profile", "MetabolismProfile"),
    // StebbsProperties (50) — PascalCase was the sole legacy form for the
    // full STEBBS surface pre-gh#1334.
    (
        "wall_internal_convection_coefficient",
        "WallInternalConvectionCoefficient",
    ),
    (
        "roof_internal_convection_coefficient",
        "RoofInternalConvectionCoefficient",
    ),
    (
        "internal_mass_convection_coefficient",
        "InternalMassConvectionCoefficient",
    ),
    (
        "floor_internal_convection_coefficient",
        "FloorInternalConvectionCoefficient",
    ),
    (
        "window_internal_convection_coefficient",
        "WindowInternalConvectionCoefficient",
    ),
    (
        "wall_external_convection_coefficient",
        "WallExternalConvectionCoefficient",
    ),
    (
        "roof_external_convection_coefficient",
        "RoofExternalConvectionCoefficient",
    ),
    (
        "window_external_convection_coefficient",
        "WindowExternalConvectionCoefficient",
    ),
    ("ground_depth", "GroundDepth"),
    ("external_ground_conductivity", "ExternalGroundConductivity"),
    (
        "month_mean_air_temperature_diffmax",
        "MonthMeanAirTemperature_diffmax",
    ),
    ("annual_mean_air_temperature", "AnnualMeanAirTemperature"),
    ("metabolism_threshold", "MetabolismThreshold"),
    ("latent_sensible_ratio", "LatentSensibleRatio"),
    ("daylight_control", "DaylightControl"),
    (
        "lighting_illuminance_threshold",
        "LightingIlluminanceThreshold",
    ),
    ("heating_system_efficiency", "HeatingSystemEfficiency"),
    ("max_cooling_power", "MaxCoolingPower"),
    ("cooling_system_cop", "CoolingSystemCOP"),
    ("ventilation_rate", "VentilationRate"),
    ("initial_outdoor_temperature", "InitialOutdoorTemperature"),
    ("initial_indoor_temperature", "InitialIndoorTemperature"),
    ("hot_water_tank_wall_thickness", "WaterTankWallThickness"),
    ("mains_water_temperature", "MainsWaterTemperature"),
    ("hot_water_tank_surface_area", "WaterTankSurfaceArea"),
    (
        "hot_water_heating_setpoint_temperature",
        "HotWaterHeatingSetpointTemperature",
    ),
    (
        "hot_water_tank_wall_emissivity",
        "HotWaterTankWallEmissivity",
    ),
    ("hot_water_vessel_wall_thickness", "DHWVesselWallThickness"),
    ("hot_water_volume", "DHWWaterVolume"),
    ("hot_water_surface_area", "DHWSurfaceArea"),
    ("hot_water_flow_rate", "HotWaterFlowRate"),
    ("hot_water_flow_profile", "HotWaterFlowProfile"),
    (
        "hot_water_specific_heat_capacity",
        "DHWSpecificHeatCapacity",
    ),
    (
        "hot_water_tank_specific_heat_capacity",
        "HotWaterTankSpecificHeatCapacity",
    ),
    (
        "hot_water_vessel_specific_heat_capacity",
        "DHWVesselSpecificHeatCapacity",
    ),
    ("hot_water_density", "DHWDensity"),
    ("hot_water_tank_wall_density", "HotWaterTankWallDensity"),
    ("hot_water_vessel_density", "DHWVesselDensity"),
    (
        "hot_water_tank_building_wall_view_factor",
        "HotWaterTankBuildingWallViewFactor",
    ),
    (
        "hot_water_tank_internal_mass_view_factor",
        "HotWaterTankInternalMassViewFactor",
    ),
    (
        "hot_water_tank_wall_conductivity",
        "HotWaterTankWallConductivity",
    ),
    (
        "hot_water_tank_internal_wall_convection_coefficient",
        "HotWaterTankInternalWallConvectionCoefficient",
    ),
    (
        "hot_water_tank_external_wall_convection_coefficient",
        "HotWaterTankExternalWallConvectionCoefficient",
    ),
    (
        "hot_water_vessel_wall_conductivity",
        "DHWVesselWallConductivity",
    ),
    (
        "hot_water_vessel_internal_wall_convection_coefficient",
        "DHWVesselInternalWallConvectionCoefficient",
    ),
    (
        "hot_water_vessel_external_wall_convection_coefficient",
        "DHWVesselExternalWallConvectionCoefficient",
    ),
    (
        "hot_water_vessel_wall_emissivity",
        "DHWVesselWallEmissivity",
    ),
    ("hot_water_heating_efficiency", "HotWaterHeatingEfficiency"),
    ("appliance_profile", "ApplianceProfile"),
    ("lighting_power_density", "LightingPowerDensity"),
    // SnowParams (17) — six entries retarget from the 2026.5 intermediate
    // (preciplimit -> temperature_rain_snow_threshold etc.); five new
    // entries are for fields whose 2026.5 shape had no fused predecessor
    // (tau_a, narp_emis_snow, etc.) — the "legacy" name is what the
    // Fortran bridge still reads.
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

/// Additional compatibility aliases for short-lived schema-intermediate
/// spellings (`net_radiation_method`, `WallExternalThickness`, dev6
/// ArchetypeProperties names, ...).
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
    (
        "WallExternalEffectiveConductivity",
        "wallexteffectiveconductivity",
    ),
    ("WallExternalDensity", "wallextdensity"),
    ("WallExternalCp", "wallextcp"),
    ("RoofExternalThickness", "roofextthickness"),
    (
        "RoofExternalEffectiveConductivity",
        "roofexteffectiveconductivity",
    ),
    ("RoofExternalDensity", "roofextdensity"),
    ("RoofExternalCp", "roofextcp"),
    // Schema 2026.5.dev6 ArchetypeProperties names, before the dev7
    // naming-convention Rule-2 reorder. These need the same Rust-side
    // compatibility path as Python's ARCHETYPEPROPERTIES_DEV6_RENAMES
    // because the CLI parser still reads the legacy fused bridge names.
    ("wall_thickness", "wallthickness"),
    ("wall_effective_conductivity", "walleffectiveconductivity"),
    ("wall_density", "walldensity"),
    ("wall_specific_heat_capacity", "wallcp"),
    ("wall_external_thickness", "wallextthickness"),
    (
        "wall_external_effective_conductivity",
        "wallexteffectiveconductivity",
    ),
    ("wall_external_density", "wallextdensity"),
    ("wall_external_specific_heat_capacity", "wallextcp"),
    ("wall_outer_heat_capacity_fraction", "walloutercapfrac"),
    ("wall_external_emissivity", "wallexternalemissivity"),
    ("wall_internal_emissivity", "wallinternalemissivity"),
    ("wall_transmissivity", "walltransmissivity"),
    ("wall_absorptivity", "wallabsorbtivity"),
    ("wall_reflectivity", "wallreflectivity"),
    ("roof_thickness", "roofthickness"),
    ("roof_effective_conductivity", "roofeffectiveconductivity"),
    ("roof_density", "roofdensity"),
    ("roof_specific_heat_capacity", "roofcp"),
    ("roof_external_thickness", "roofextthickness"),
    (
        "roof_external_effective_conductivity",
        "roofexteffectiveconductivity",
    ),
    ("roof_external_density", "roofextdensity"),
    ("roof_external_specific_heat_capacity", "roofextcp"),
    ("roof_outer_heat_capacity_fraction", "roofoutercapfrac"),
    ("roof_external_emissivity", "roofexternalemissivity"),
    ("roof_internal_emissivity", "roofinternalemissivity"),
    ("roof_transmissivity", "rooftransmissivity"),
    ("roof_absorptivity", "roofabsorbtivity"),
    ("roof_reflectivity", "roofreflectivity"),
    ("window_thickness", "windowthickness"),
    (
        "window_effective_conductivity",
        "windoweffectiveconductivity",
    ),
    ("window_density", "windowdensity"),
    ("window_specific_heat_capacity", "windowcp"),
    ("window_external_emissivity", "windowexternalemissivity"),
    ("window_internal_emissivity", "windowinternalemissivity"),
    ("window_transmissivity", "windowtransmissivity"),
    ("window_absorptivity", "windowabsorbtivity"),
    ("window_reflectivity", "windowreflectivity"),
    ("ground_floor_thickness", "floorthickness"),
    (
        "ground_floor_effective_conductivity",
        "groundflooreffectiveconductivity",
    ),
    ("ground_floor_density", "groundfloordensity"),
    ("ground_floor_specific_heat_capacity", "groundfloorcp"),
    ("internal_mass_density", "internalmassdensity"),
    ("internal_mass_specific_heat_capacity", "internalmasscp"),
    ("internal_mass_emissivity", "internalmassemissivity"),
    // Schema 2026.5.dev2 SnowParams intermediate (gh#1334)
    ("precip_limit", "preciplimit"),
    ("precip_limit_albedo", "preciplimitalb"),
    ("snow_limit_building", "snowlimbldg"),
    ("snow_limit_paved", "snowlimpaved"),
    ("temp_melt_factor", "tempmeltfact"),
    ("rad_melt_factor", "radmeltfact"),
];

/// Family registry for nested `model.physics` sub-options (gh#972).
///
/// Mirrors `PHYSICS_FAMILIES` in
/// `src/supy/data_model/core/physics_families.py` 1:1 — same snake_case
/// field keys, same family tags, same numeric codes. Drift is caught by
/// `scripts/lint/check_rust_yaml_aliases.py`.
///
/// `emissions` is trimmed to codes 0-5 to match the current
/// `EmissionsMethod` enum — biogen variants (11-45) live in the Python
/// docstring but are not yet enum members.
///
/// Keyed by the canonical snake_case field name because
/// `collapse_nested_physics` runs BEFORE the legacy-name rewrite
/// (`normalize_field_names_at`). That ordering matters: some family
/// tags (notably `stebbs` under `storage_heat`) collide with
/// ModelPhysics field names that the recursive rename walker would
/// otherwise rewrite (`stebbs` -> `stebbsmethod`). Collapsing first
/// consumes the family tag before the walker can see it.
type FamilyCodes = &'static [i64];

pub const PHYSICS_FAMILIES_RS: &[(&str, &[(&str, FamilyCodes)])] = &[
    (
        "net_radiation",
        &[
            ("forcing", &[0]),
            ("narp", &[1, 2, 3, 11, 12, 13, 100, 200, 300]),
            ("spartacus", &[1001, 1002, 1003]),
        ],
    ),
    (
        "storage_heat",
        &[
            ("observed", &[0]),
            ("ohm", &[1]),
            ("anohm", &[3]),
            ("estm", &[4]),
            ("ehc", &[5]),
            ("dyohm", &[6]),
            ("stebbs", &[7]),
        ],
    ),
    (
        "emissions",
        &[("observed", &[0]), ("simple", &[1, 2, 3, 4, 5])],
    ),
];

const NET_RADIATION_OUTER_KEYS: &[&str] = &[
    "net_radiation",
    "net_radiation_method",
    "netradiationmethod",
];
const STORAGE_HEAT_OUTER_KEYS: &[&str] =
    &["storage_heat", "storage_heat_method", "storageheatmethod"];
const EMISSIONS_OUTER_KEYS: &[&str] = &["emissions", "emissions_method", "emissionsmethod"];

fn physics_outer_keys(field_name: &str) -> &'static [&'static str] {
    match field_name {
        "net_radiation" => NET_RADIATION_OUTER_KEYS,
        "storage_heat" => STORAGE_HEAT_OUTER_KEYS,
        "emissions" => EMISSIONS_OUTER_KEYS,
        _ => &[],
    }
}

/// Collapse family-tagged nested physics input to the flat `{value: N}`
/// shape underneath `model.physics.<field>`. Called from
/// `normalize_field_names` BEFORE the recursive rename walk, so it must
/// recognise the canonical field key plus the accepted legacy/intermediate
/// aliases (`*_method`, fused spelling).
///
/// Returns `Err(String)` when the family tag is unknown, multiple family
/// keys are present, the inner mapping lacks `value`, or the numeric code
/// does not belong to the declared family. Matches the Python-side error
/// surface in `physics_families.coerce_nested_to_flat`.
fn collapse_nested_physics(root: &mut Value) -> Result<(), String> {
    let physics = match root
        .get_mut("model")
        .and_then(|m| m.as_mapping_mut())
        .and_then(|m| m.get_mut(Value::String("physics".into())))
        .and_then(|p| p.as_mapping_mut())
    {
        Some(p) => p,
        None => return Ok(()),
    };

    for (field_name, families) in PHYSICS_FAMILIES_RS.iter() {
        for outer_key_name in physics_outer_keys(field_name) {
            let outer_key = Value::String((*outer_key_name).to_string());
            let entry = match physics.get_mut(&outer_key) {
                Some(v) => v,
                None => continue,
            };
            let map = match entry.as_mapping_mut() {
                Some(m) => m,
                None => continue,
            };

            let matched: Vec<&str> = families
                .iter()
                .filter_map(|(fam, _)| {
                    let fam_key = Value::String((*fam).to_string());
                    if map.contains_key(&fam_key) {
                        Some(*fam)
                    } else {
                        None
                    }
                })
                .collect();

            if matched.is_empty() {
                continue;
            }
            if matched.len() > 1 {
                return Err(format!(
                    "'{field_name}' received multiple family tags ({matched:?}); supply exactly one."
                ));
            }

            let family = matched[0];
            let fam_codes = families
                .iter()
                .find(|(f, _)| *f == family)
                .map(|(_, codes)| *codes)
                .expect("family just matched");

            let foreign: Vec<String> = map
                .iter()
                .filter_map(|(k, _)| {
                    k.as_str()
                        .filter(|s| *s != family && *s != "ref")
                        .map(|s| s.to_string())
                })
                .collect();
            if !foreign.is_empty() {
                return Err(format!(
                    "'{field_name}.{family}' cannot be combined with sibling keys {foreign:?}."
                ));
            }

            let fam_key = Value::String(family.to_string());
            let inner = map.remove(&fam_key).expect("family key present");
            let inner_map = inner.as_mapping().ok_or_else(|| {
                format!("'{field_name}.{family}' must be a mapping with a 'value' key")
            })?;
            let code_value = inner_map
                .get(Value::String("value".into()))
                .ok_or_else(|| {
                    format!("'{field_name}.{family}' must be a mapping with a 'value' key")
                })?;

            let code = match code_value {
                Value::Number(n) => n.as_i64().ok_or_else(|| {
                    format!("'{field_name}.{family}.value' must be an integer code")
                })?,
                _ => {
                    return Err(format!(
                        "'{field_name}.{family}.value' must be a scalar integer code"
                    ));
                }
            };

            if !fam_codes.contains(&code) {
                return Err(format!(
                    "'{field_name}.{family}' expects one of {fam_codes:?}, got {code}."
                ));
            }

            // Preserve inner `ref` (if any) when rewriting to the flat shape.
            let carried_ref = inner_map.get(Value::String("ref".into())).cloned();
            let mut flat = serde_yaml::Mapping::new();
            flat.insert(Value::String("value".into()), Value::Number(code.into()));
            if let Some(r) = carried_ref {
                flat.insert(Value::String("ref".into()), r);
            }
            // Drop any remaining non-family keys on the outer map (e.g. the
            // outer `ref`) — family-gated shape intentionally narrows to
            // `{value: N, ref?: ...}`.
            map.clear();
            for (k, v) in flat {
                map.insert(k, v);
            }
        }
    }

    Ok(())
}

fn legacy_name_for(key: &str) -> Option<(&'static str, &'static str)> {
    FIELD_RENAMES
        .iter()
        .chain(FIELD_COMPAT_ALIASES.iter())
        .find(|(new_name, _)| *new_name == key)
        .map(|(new_name, old_name)| (*new_name, *old_name))
}

fn should_rename_key_at_path(new_name: &str, path: &str) -> bool {
    // `stebbs` is both a ModelPhysics option and a site-properties section.
    // Only the physics option should be folded to `stebbsmethod`; the
    // `sites[].properties.stebbs` section must keep its name so the STEBBS
    // parser can read the parameter and initial-state overrides.
    new_name != "stebbs" || path == "model.physics"
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
    // Nested family form must be collapsed BEFORE the recursive rename
    // walker runs — some family tags (e.g. `stebbs` under `storage_heat`)
    // collide with ModelPhysics field names that the walker would
    // otherwise rewrite (`stebbs` -> `stebbsmethod`). See
    // `PHYSICS_FAMILIES_RS` for the full rationale (gh#972).
    collapse_nested_physics(root)?;
    normalize_field_names_at(root, "<root>")?;
    Ok(())
}

fn normalize_field_names_at(root: &mut Value, path: &str) -> Result<(), String> {
    match root {
        Value::Mapping(map) => {
            let rename_candidates: Vec<(Value, &'static str, &'static str)> = map
                .iter()
                .filter_map(|(key, _)| match key {
                    Value::String(k) => legacy_name_for(k.as_str())
                        .filter(|(new_name, _)| should_rename_key_at_path(new_name, path))
                        .map(|(new_name, old_name)| (key.clone(), new_name, old_name)),
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
        assert_eq!(FIELD_RENAMES.len(), 177);
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
    fn preserves_site_stebbs_section_name() {
        let yaml = "\
model:
  physics:
    stebbs: {value: 1}
sites:
  - properties:
      stebbs:
        annual_mean_air_temperature: {value: 10.0}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();

        let physics = &root["model"]["physics"];
        assert!(physics.get("stebbsmethod").is_some());
        assert!(physics.get("stebbs").is_none());

        let properties = &root["sites"][0]["properties"];
        assert!(properties.get("stebbs").is_some());
        assert!(properties.get("stebbsmethod").is_none());
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
    fn renames_archetype_dev6_keys_to_legacy() {
        let yaml = "\
sites:
  - properties:
      building_archetype:
        wall_external_thickness: {value: 0.25}
        wall_thickness: {value: 0.30}
        window_absorptivity: {value: 0.01}
        internal_mass_density: {value: 1000.0}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();

        let arch = &root["sites"][0]["properties"]["building_archetype"];
        assert!(arch.get("wallextthickness").is_some());
        assert!(arch.get("wallthickness").is_some());
        assert!(arch.get("windowabsorbtivity").is_some());
        assert!(arch.get("internalmassdensity").is_some());
        assert!(arch.get("wall_external_thickness").is_none());
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

    #[test]
    fn nested_family_collapses_to_flat() {
        let yaml = "\
model:
  physics:
    net_radiation:
      spartacus:
        value: 1001
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let physics = &root["model"]["physics"];
        let netrad = physics
            .get(Value::String("netradiationmethod".into()))
            .expect("renamed to fused spelling");
        let v = netrad
            .get(Value::String("value".into()))
            .expect("flat value key");
        assert_eq!(v.as_i64(), Some(1001));
        assert!(
            netrad.get(Value::String("spartacus".into())).is_none(),
            "family tag should be discarded"
        );
    }

    #[test]
    fn nested_storage_heat_ehc_collapses() {
        let yaml = "model:\n  physics:\n    storage_heat:\n      ehc:\n        value: 5\n";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let v = root["model"]["physics"]["storageheatmethod"]
            .get(Value::String("value".into()))
            .unwrap();
        assert_eq!(v.as_i64(), Some(5));
    }

    #[test]
    fn nested_emissions_simple_collapses() {
        let yaml = "model:\n  physics:\n    emissions:\n      simple:\n        value: 2\n";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let v = root["model"]["physics"]["emissionsmethod"]
            .get(Value::String("value".into()))
            .unwrap();
        assert_eq!(v.as_i64(), Some(2));
    }

    #[test]
    fn wrong_family_rejected() {
        let yaml = "model:\n  physics:\n    net_radiation:\n      narp:\n        value: 1001\n";
        let mut root: Value = from_str(yaml).unwrap();
        let err = normalize_field_names(&mut root).expect_err("wrong family must fail");
        assert!(err.contains("expects one of"), "error was: {err}");
    }

    #[test]
    fn flat_value_form_untouched_by_collapse() {
        let yaml = "model:\n  physics:\n    net_radiation:\n      value: 3\n";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let v = root["model"]["physics"]["netradiationmethod"]
            .get(Value::String("value".into()))
            .unwrap();
        assert_eq!(v.as_i64(), Some(3));
    }

    #[test]
    fn multiple_family_tags_rejected() {
        let yaml = "model:\n  physics:\n    net_radiation:\n      narp: {value: 3}\n      spartacus: {value: 1001}\n";
        let mut root: Value = from_str(yaml).unwrap();
        let err = normalize_field_names(&mut root).expect_err("multi-tag must fail");
        assert!(err.contains("multiple family tags"), "error was: {err}");
    }

    #[test]
    fn nested_family_under_intermediate_outer_key_collapses() {
        let yaml =
            "model:\n  physics:\n    net_radiation_method:\n      spartacus:\n        value: 1001\n";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let v = root["model"]["physics"]["netradiationmethod"]
            .get(Value::String("value".into()))
            .unwrap();
        assert_eq!(v.as_i64(), Some(1001));
    }

    #[test]
    fn nested_family_under_legacy_outer_key_collapses() {
        let yaml = "model:\n  physics:\n    storageheatmethod:\n      stebbs:\n        value: 7\n";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let storage = &root["model"]["physics"]["storageheatmethod"];
        let v = storage.get(Value::String("value".into())).unwrap();
        assert_eq!(v.as_i64(), Some(7));
        assert!(
            storage.get(Value::String("stebbsmethod".into())).is_none(),
            "family tag should collapse before recursive rename touches it"
        );
    }
}
