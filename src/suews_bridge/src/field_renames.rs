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
/// Total: 174 pairs (gh#1334 full STEBBS + Snow snake_case sweep; building_type
/// and the two hot_water_tank view-factor params dropped per the Reading STEBBS
/// team review, gh#1392).
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
    // Schema 2026.5.dev12 (Reading Column D): former `outer_cap_fraction`
    // (former fused `rcmethod`) is now `capacitance`. Pure key rename — still
    // a RCMethod enum, bridge column stays `rcmethod`. The dev11 spelling
    // `outer_cap_fraction` is a back-compat alias in FIELD_COMPAT_ALIASES.
    ("capacitance", "rcmethod"),
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
    // ArchetypeProperties (61) — current dev7 snake_case targets (gh#1390
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
    ("archetype_name", "BuildingName"),
    ("archetype_building_count", "BuildingCount"),
    ("occupants", "Occupants"),
    ("archetype_height", "stebbs_Height"),
    ("area_footprint", "FootprintArea"),
    ("area_wall_external", "WallExternalArea"),
    ("ratio_internal_mass_volume", "RatioInternalVolume"),
    ("area_internal_mass", "InternalMassArea"),
    ("ratio_window_to_wall", "WWR"),
    ("thickness_wall", "WallThickness"),
    ("conductivity_wall", "WallEffectiveConductivity"),
    ("density_wall", "WallDensity"),
    ("specific_heat_capacity_wall", "WallCp"),
    ("fraction_heat_capacity_wall_external", "WallOuterCapFrac"),
    ("emissivity_wall_external", "WallExternalEmissivity"),
    ("emissivity_wall_internal", "WallInternalEmissivity"),
    ("transmissivity_wall_external", "WallTransmissivity"),
    ("absorptivity_wall_external", "WallAbsorbtivity"),
    ("reflectivity_wall_external", "WallReflectivity"),
    ("thickness_roof", "RoofThickness"),
    ("conductivity_roof", "RoofEffectiveConductivity"),
    ("density_roof", "RoofDensity"),
    ("specific_heat_capacity_roof", "RoofCp"),
    ("fraction_heat_capacity_roof_external", "RoofOuterCapFrac"),
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
    ("power_air_heating_max", "MaxHeatingPower"),
    ("volume_hot_water_tank", "WaterTankWaterVolume"),
    (
        "power_water_heating_max",
        "MaximumHotWaterHeatingPower",
    ),
    ("temperature_air_heating_setpoint", "HeatingSetpointTemperature"),
    ("temperature_air_cooling_setpoint", "CoolingSetpointTemperature"),
    (
        "profile_temperature_air_heating_setpoint",
        "HeatingSetpointTemperatureProfile",
    ),
    (
        "profile_temperature_air_cooling_setpoint",
        "CoolingSetpointTemperatureProfile",
    ),
    ("profile_metabolism", "MetabolismProfile"),
    // StebbsProperties (48) — PascalCase was the sole legacy form for the
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
/// ArchetypeProperties names, the pre-Tier-1 ArchetypeProperties names, and
/// the StebbsProperties Rule-2 final names) that must fold directly to the
/// bridge-era keys. The Rust CLI bypasses the Pydantic rename shim, so any
/// name that is not the canonical `FIELD_RENAMES` final needs a direct alias
/// here.
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
    // Schema 2026.5.dev11 spelling of the field now named `capacitance`
    // (gh#1392 Column D). The CLI bypasses the Pydantic shim, so the dev11
    // spelling needs a direct alias to the bridge column `rcmethod`.
    ("outer_cap_fraction", "rcmethod"),
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
    // Pre-Tier-1 ArchetypeProperties names (gh#1392): the master-current
    // spellings before the archetype_* / area_* / ratio_* / power_* /
    // temperature_air_* / volume_hot_water_tank / profile_* reorder. FIELD_RENAMES
    // now carries the Tier-1 finals; these older names stay accepted via the
    // same fused-lowercase bridge keys (lowercased PascalCase ancestry).
    ("building_name", "buildingname"),
    ("building_count", "buildingcount"),
    ("building_height", "stebbs_height"),
    ("footprint_area", "footprintarea"),
    ("wall_external_area", "wallexternalarea"),
    ("internal_mass_area", "internalmassarea"),
    ("internal_volume_ratio", "ratiointernalvolume"),
    ("window_to_wall_ratio", "wwr"),
    ("max_heating_power", "maxheatingpower"),
    (
        "maximum_hot_water_heating_power",
        "maximumhotwaterheatingpower",
    ),
    ("hot_water_tank_volume", "watertankwatervolume"),
    (
        "heating_setpoint_temperature",
        "heatingsetpointtemperature",
    ),
    (
        "cooling_setpoint_temperature",
        "coolingsetpointtemperature",
    ),
    (
        "heating_setpoint_temperature_profile",
        "heatingsetpointtemperatureprofile",
    ),
    (
        "cooling_setpoint_temperature_profile",
        "coolingsetpointtemperatureprofile",
    ),
    ("metabolism_profile", "metabolismprofile"),
    // gh#1390 final spellings for the wall/roof heat-capacity fraction, before
    // the outer -> external move (D. Hertwig col D). Kept accepted by the CLI.
    ("fraction_wall_heat_capacity_outer", "walloutercapfrac"),
    ("fraction_roof_heat_capacity_outer", "roofoutercapfrac"),
    // Schema 2026.5.dev2 SnowParams intermediate (gh#1334)
    ("precip_limit", "preciplimit"),
    ("precip_limit_albedo", "preciplimitalb"),
    ("snow_limit_building", "snowlimbldg"),
    ("snow_limit_paved", "snowlimpaved"),
    ("temp_melt_factor", "tempmeltfact"),
    ("rad_melt_factor", "radmeltfact"),
    // Schema 2026.5.dev9 StebbsProperties Rule-2 final names. Python
    // reaches these through the Pydantic rename chain; the Rust CLI bypasses
    // that shim, so current YAML needs a direct alias to the bridge keys.
    ("convection_coefficient_wall_internal", "wallinternalconvectioncoefficient"),
    ("convection_coefficient_roof_internal", "roofinternalconvectioncoefficient"),
    ("convection_coefficient_internal_mass", "internalmassconvectioncoefficient"),
    ("convection_coefficient_ground_floor_internal", "floorinternalconvectioncoefficient"),
    ("convection_coefficient_window_internal", "windowinternalconvectioncoefficient"),
    ("convection_coefficient_wall_external", "wallexternalconvectioncoefficient"),
    ("convection_coefficient_roof_external", "roofexternalconvectioncoefficient"),
    ("convection_coefficient_window_external", "windowexternalconvectioncoefficient"),
    ("thermal_conductivity_ground", "externalgroundconductivity"),
    ("threshold_metabolism", "metabolismthreshold"),
    ("ratio_latent_sensible", "latentsensibleratio"),
    ("control_daylight", "daylightcontrol"),
    ("threshold_lighting_illuminance", "lightingilluminancethreshold"),
    ("efficiency_heating_system_air", "heatingsystemefficiency"),
    ("power_air_cooling_max", "maxcoolingpower"),
    ("efficiency_cooling_system_air", "coolingsystemcop"),
    ("temperature_air_outdoor_initial", "initialoutdoortemperature"),
    ("temperature_air_indoor_initial", "initialindoortemperature"),
    ("temperature_air_annual_mean", "annualmeanairtemperature"),
    ("thickness_hot_water_tank_wall", "watertankwallthickness"),
    ("temperature_water_mains", "mainswatertemperature"),
    ("area_hot_water_tank_surface", "watertanksurfacearea"),
    ("temperature_water_heating_setpoint", "hotwaterheatingsetpointtemperature"),
    ("emissivity_hot_water_tank_wall", "hotwatertankwallemissivity"),
    ("conductivity_hot_water_tank_wall", "hotwatertankwallconductivity"),
    ("density_hot_water_tank_wall", "hotwatertankwalldensity"),
    ("specific_heat_capacity_hot_water_tank_wall", "hotwatertankspecificheatcapacity"),
    (
        "convection_coefficient_hot_water_tank_wall_internal",
        "hotwatertankinternalwallconvectioncoefficient",
    ),
    (
        "convection_coefficient_hot_water_tank_wall_external",
        "hotwatertankexternalwallconvectioncoefficient",
    ),
    ("thickness_hot_water_vessel_wall", "dhwvesselwallthickness"),
    ("conductivity_hot_water_vessel_wall", "dhwvesselwallconductivity"),
    ("density_hot_water_vessel_wall", "dhwvesseldensity"),
    ("specific_heat_capacity_hot_water_vessel_wall", "dhwvesselspecificheatcapacity"),
    (
        "convection_coefficient_hot_water_vessel_wall_internal",
        "dhwvesselinternalwallconvectioncoefficient",
    ),
    (
        "convection_coefficient_hot_water_vessel_wall_external",
        "dhwvesselexternalwallconvectioncoefficient",
    ),
    ("emissivity_hot_water_vessel_wall", "dhwvesselwallemissivity"),
    ("volume_hot_water", "dhwwatervolume"),
    ("area_hot_water_surface", "dhwsurfacearea"),
    ("rate_hot_water_flow", "hotwaterflowrate"),
    ("density_hot_water", "dhwdensity"),
    ("specific_heat_capacity_hot_water", "dhwspecificheatcapacity"),
    ("efficiency_heating_system_water", "hotwaterheatingefficiency"),
    ("profile_hot_water_flow", "hotwaterflowprofile"),
    ("profile_appliance", "applianceprofile"),
    // Schema 2026.5.dev12 STEBBS straggler reorder (gh#1392 follow-up). The
    // current YAML carries the quantity-first names; the Rust CLI bypasses the
    // Pydantic rename chain, so each needs a direct alias to its PascalCase
    // legacy ancestry. PascalCase (not fused-lowercase) is deliberate:
    // `apply_stebbs_overrides` matches `lighting_power_density` by EXACT
    // equality on the de-camelCased key, so `LightingPowerDensity` ->
    // `lighting_power_density` keeps that check firing.
    ("depth_ground", "GroundDepth"),
    ("rate_ventilation", "VentilationRate"),
    ("power_density_lighting", "LightingPowerDensity"),
    ("temperature_air_month_mean_diffmax", "MonthMeanAirTemperature_diffmax"),
    // Schema 2026.5.dev12 Column D alignment (gh#1392 follow-up): sixteen more
    // STEBBS / Archetype fields adopt the Reading STEBBS team's "Column D"
    // names (D. Hertwig / S. Rognone, 2026-05). The current YAML carries the
    // new names; the Rust CLI bypasses the Pydantic rename chain, so each needs
    // a direct alias to its PascalCase legacy ancestry. PascalCase (not
    // fused-lowercase) is deliberate: scalar `set_mapped_value` matches on the
    // underscore-stripped form (so either spelling resolves), but the profile
    // handlers in `apply_stebbs_overrides` compare the de-camelCased key by
    // EXACT equality (e.g. `HotWaterFlowProfile` -> `hot_water_flow_profile`),
    // so the PascalCase ancestry is required for the profile fields.
    // ArchetypeProperties (6)
    ("max_power_heating_system_air", "MaxHeatingPower"),
    ("max_power_heating_system_water", "MaximumHotWaterHeatingPower"),
    ("setpoint_temperature_heating_air", "HeatingSetpointTemperature"),
    ("setpoint_temperature_cooling_air", "CoolingSetpointTemperature"),
    (
        "profile_setpoint_temperature_heating_air",
        "HeatingSetpointTemperatureProfile",
    ),
    (
        "profile_setpoint_temperature_cooling_air",
        "CoolingSetpointTemperatureProfile",
    ),
    // StebbsProperties (10)
    ("max_power_cooling_system_air", "MaxCoolingPower"),
    ("setpoint_temperature_heating_water", "HotWaterHeatingSetpointTemperature"),
    ("temperature_mains_water", "MainsWaterTemperature"),
    ("surface_area_hot_water_tank", "WaterTankSurfaceArea"),
    ("surface_area_hot_water", "DHWSurfaceArea"),
    ("rate_flow_hot_water", "HotWaterFlowRate"),
    ("profile_flow_hot_water", "HotWaterFlowProfile"),
    ("daylight_control", "DaylightControl"),
    (
        "convection_coefficient_hot_water_tank_vessel_internal",
        "DHWVesselInternalWallConvectionCoefficient",
    ),
    (
        "convection_coefficient_hot_water_tank_vessel_external",
        "DHWVesselExternalWallConvectionCoefficient",
    ),
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

/// Nested `model.physics.stebbs` leaf keys recognised by the flatten
/// pre-pass. Mirrors `_STEBBS_NESTED_KEYS` in
/// `src/supy/data_model/core/field_renames.py` (gh#1456). A `stebbs` mapping
/// carrying any of these is the new nested object; a `{value: N}` /
/// scalar `stebbs` is the legacy master toggle and is left untouched.
///
/// gh#1456: `ref` is deliberately NOT a discriminating key. A legacy flat
/// master toggle carrying provenance is a RefValue scalar -- `{value: 1, ref:
/// {...}}` -- whose only keys are `value`/`ref`; treating `ref` as a nested
/// marker would misclassify that scalar as the new nested object, silently
/// reading the absent `enabled` as `false` and flipping `stebbsmethod` to 0.
/// A genuine nested block always carries `enabled`/`parameters`/a leaf name
/// alongside any optional `ref`, so `ref` is never the sole discriminator.
/// See `is_stebbs_refvalue_scalar`. This keeps the Rust and Python
/// nested-detection predicates identical.
const STEBBS_NESTED_KEYS: &[&str] = &[
    "enabled",
    "parameters",
    "capacitance",
    "setpoint",
    "same_albedo_wall",
    "same_albedo_roof",
    "same_emissivity_wall",
    "same_emissivity_roof",
];

/// Return true for a `{value: ...}` / `{value: ..., ref: ...}` scalar. Such a
/// mapping is a RefValue-wrapped legacy master toggle, never the new nested
/// `StebbsPhysics` object (gh#1456). Mirrors `_is_stebbs_refvalue_scalar` in
/// the Python fold.
fn is_stebbs_refvalue_scalar(map: &serde_yaml::Mapping) -> bool {
    let has_value = map.contains_key(Value::String("value".into()));
    if !has_value {
        return false;
    }
    map.keys().all(|k| {
        matches!(k, Value::String(s) if s == "value" || s == "ref")
    })
}

/// `(nested_leaf, fused_flat_key)` for the five non-master STEBBS switches.
/// `capacitance` / `setpoint` fold to the fused DataFrame columns the parser
/// reads (`rcmethod`, `setpointmethod`); the four `same_*` switches keep their
/// names (no rename — the DataFrame columns are unchanged, gh#1456). Flattening
/// straight to the fused keys keeps this pre-pass self-contained and
/// independent of the downstream `FIELD_RENAMES` rename-walker ordering.
const STEBBS_LEAF_TO_FUSED: &[(&str, &str)] = &[
    ("capacitance", "rcmethod"),
    ("setpoint", "setpointmethod"),
    ("same_albedo_wall", "same_albedo_wall"),
    ("same_albedo_roof", "same_albedo_roof"),
    ("same_emissivity_wall", "same_emissivity_wall"),
    ("same_emissivity_roof", "same_emissivity_roof"),
];

/// Unwrap a RefValue-style `{value: X}` mapping to its inner scalar, else
/// return the value as-is. Mirrors `_unwrap_scalar` in the Python fold.
fn unwrap_ref_scalar(entry: &Value) -> &Value {
    if let Value::Mapping(map) = entry {
        if let Some(inner) = map.get(Value::String("value".into())) {
            return inner;
        }
    }
    entry
}

/// Interpret a `stebbs.enabled` scalar as a boolean. Accepts native bools,
/// the integers `0`/`1`, and the strings `true`/`false` (case-insensitive).
fn read_enabled_flag(entry: &Value) -> bool {
    match unwrap_ref_scalar(entry) {
        Value::Bool(b) => *b,
        Value::Number(n) => n.as_f64().map(|v| v != 0.0).unwrap_or(false),
        Value::String(s) => matches!(s.trim().to_ascii_lowercase().as_str(), "true" | "1"),
        _ => false,
    }
}

/// Interpret a `stebbs.parameters` scalar as the non-zero `StebbsMethod`
/// integer code. Accepts the integers `1`/`2` and the `StebbsParameterSource`
/// enum names `default`/`provided` (case-insensitive). Mirrors the Python
/// `int(parameters)` composition. Unknown values fall back to `2` (PROVIDED),
/// the most permissive non-zero reading (matches `_decompose_stebbs_master`).
fn read_parameters_code(entry: &Value) -> i64 {
    match unwrap_ref_scalar(entry) {
        Value::Number(n) => n.as_i64().unwrap_or(1),
        Value::String(s) => match s.trim().to_ascii_lowercase().as_str() {
            "default" => 1,
            "provided" => 2,
            other => other.parse::<i64>().unwrap_or(2),
        },
        _ => 1,
    }
}

/// Flatten a nested `model.physics.stebbs` object back to the fused flat keys
/// the hand-written parser indexes (`stebbsmethod`, `rcmethod`,
/// `setpointmethod`, `same_*`). MUST run before `collapse_nested_physics` and
/// the recursive rename walker so the nested object is consumed before either
/// can misread it (the walker would otherwise rename the whole `stebbs`
/// mapping to `stebbsmethod`; see `should_rename_key_at_path`).
///
/// Composition mirrors `StebbsPhysics.to_df_state` / `fold_stebbs_physics` in
/// `src/supy/data_model/core/model.py` + `field_renames.py` (gh#1456):
/// `stebbsmethod = 0 if not enabled else int(parameters)`. `enabled` defaults
/// to `false` (STEBBS off) and `parameters` to `1` (DEFAULT) when omitted,
/// matching the Python `StebbsPhysics` field defaults.
///
/// The legacy flat form (`stebbs: {value: N}` master toggle, plus flat
/// `outer_cap_fraction` / `setpoint` / `same_*` siblings) is untouched here and
/// continues through the existing rename walker. Idempotent: running twice is a
/// no-op because the nested object is removed on the first pass and the fused
/// keys it writes are not themselves nested-leaf keys.
fn flatten_stebbs_physics(root: &mut Value) -> Result<(), String> {
    let physics = match root
        .get_mut("model")
        .and_then(|m| m.as_mapping_mut())
        .and_then(|m| m.get_mut(Value::String("physics".into())))
        .and_then(|p| p.as_mapping_mut())
    {
        Some(p) => p,
        None => return Ok(()),
    };

    let stebbs_key = Value::String("stebbs".into());
    let is_nested = match physics.get(&stebbs_key) {
        // A `{value: N}` / `{value: N, ref: {...}}` RefValue scalar is the
        // legacy master toggle, NOT the nested object (gh#1456).
        Some(Value::Mapping(map)) if is_stebbs_refvalue_scalar(map) => false,
        Some(Value::Mapping(map)) => STEBBS_NESTED_KEYS
            .iter()
            .any(|leaf| map.contains_key(Value::String((*leaf).to_string()))),
        // A scalar / `{value: N}` master toggle, or absent: legacy flat path.
        _ => false,
    };
    if !is_nested {
        return Ok(());
    }

    // Take ownership of the nested object so we can move its leaves out.
    let stebbs_block = match physics.remove(&stebbs_key) {
        Some(Value::Mapping(map)) => map,
        _ => return Ok(()),
    };

    // Guard against a flat sibling colliding with a key the fold produces
    // (e.g. a flat `outer_cap_fraction`/`rcmethod` alongside the nested
    // `stebbs.capacitance`). The legacy and nested forms are mutually
    // exclusive on input; reject rather than silently pick one. Mirrors the
    // recursive walker's both-spellings-present error.
    for (_leaf, fused) in STEBBS_LEAF_TO_FUSED {
        let fused_key = Value::String((*fused).to_string());
        if physics.contains_key(&fused_key) {
            return Err(format!(
                "Both a flat '{fused}' and a nested 'model.physics.stebbs' object are \
                 present. Use only the nested 'stebbs' form."
            ));
        }
    }
    let stebbsmethod_key = Value::String("stebbsmethod".into());
    if physics.contains_key(&stebbsmethod_key) {
        return Err(
            "Both a flat 'stebbsmethod' and a nested 'model.physics.stebbs' object are \
             present. Use only the nested 'stebbs' form."
                .to_string(),
        );
    }

    // Compose the master toggle: 0 if disabled, else int(parameters).
    let enabled = stebbs_block
        .get(Value::String("enabled".into()))
        .map(read_enabled_flag)
        .unwrap_or(false);
    let stebbsmethod_value = if !enabled {
        0
    } else {
        stebbs_block
            .get(Value::String("parameters".into()))
            .map(read_parameters_code)
            .unwrap_or(1)
    };
    let mut stebbsmethod_flat = serde_yaml::Mapping::new();
    stebbsmethod_flat.insert(
        Value::String("value".into()),
        Value::Number(stebbsmethod_value.into()),
    );
    physics.insert(stebbsmethod_key, Value::Mapping(stebbsmethod_flat));

    // Move the five remaining switches to their fused flat keys verbatim
    // (the `{value: N}` wrapper, if any, is preserved as-is).
    for (leaf, fused) in STEBBS_LEAF_TO_FUSED {
        if let Some(v) = stebbs_block.get(Value::String((*leaf).to_string())) {
            physics.insert(Value::String((*fused).to_string()), v.clone());
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
    // gh#1456: the nested `model.physics.stebbs` object must be flattened to
    // the fused flat keys (`stebbsmethod`, `rcmethod`, `setpointmethod`,
    // `same_*`) BEFORE anything else. If the recursive rename walker saw the
    // nested `stebbs` key first it would rename the whole object to
    // `stebbsmethod` (see `should_rename_key_at_path`), and
    // `collapse_nested_physics` could misread a `stebbs` leaf as a family tag.
    // Flattening first consumes the nested object so the legacy flat path
    // (master-toggle scalar + flat `outer_cap_fraction`/`setpoint`/`same_*`)
    // still flows through the walker unchanged.
    flatten_stebbs_physics(root)?;
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
        assert_eq!(FIELD_RENAMES.len(), 174);
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
    fn renames_current_archetype_keys_to_bridge_names() {
        let yaml = "\
sites:
  - properties:
      building_archetype:
        thickness_wall: {value: 0.3}
        thickness_wall_outer: {value: 0.2}
        conductivity_wall_outer: {value: 1.1}
        emissivity_wall_external: {value: 0.85}
        fraction_heat_capacity_roof_external: {value: 0.55}
        ratio_window_to_wall: {value: 0.4}
        temperature_air_heating_setpoint: {value: 21.0}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let archetype = &root["sites"][0]["properties"]["building_archetype"];

        assert!(archetype.get("WallThickness").is_some());
        assert!(archetype.get("WallextThickness").is_some());
        assert!(archetype.get("WallextEffectiveConductivity").is_some());
        assert!(archetype.get("WallExternalEmissivity").is_some());
        assert!(archetype.get("RoofOuterCapFrac").is_some());
        assert!(archetype.get("WWR").is_some());
        assert!(archetype.get("HeatingSetpointTemperature").is_some());
        assert!(archetype.get("thickness_wall").is_none());
        assert!(archetype.get("thickness_wall_outer").is_none());
        assert!(archetype.get("ratio_window_to_wall").is_none());
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

    // -- gh#1456: nested `model.physics.stebbs` flatten pre-pass --------------

    fn physics_i64(root: &Value, key: &str) -> Option<i64> {
        root["model"]["physics"]
            .get(Value::String(key.into()))
            .and_then(|v| v.get(Value::String("value".into())))
            .and_then(|v| v.as_i64())
    }

    #[test]
    fn nested_stebbs_disabled_composes_method_zero() {
        // enabled=false -> stebbsmethod 0 (parameters ignored).
        let yaml = "\
model:
  physics:
    stebbs:
      enabled: {value: false}
      parameters: {value: 2}
      capacitance: {value: 1}
      setpoint: {value: 3}
      same_albedo_wall: {value: 1}
      same_emissivity_roof: {value: 1}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let physics = &root["model"]["physics"];
        assert!(
            physics.get(Value::String("stebbs".into())).is_none(),
            "nested stebbs object must be consumed"
        );
        assert_eq!(physics_i64(&root, "stebbsmethod"), Some(0));
        assert_eq!(physics_i64(&root, "rcmethod"), Some(1));
        assert_eq!(physics_i64(&root, "setpointmethod"), Some(3));
        assert_eq!(physics_i64(&root, "same_albedo_wall"), Some(1));
        assert_eq!(physics_i64(&root, "same_emissivity_roof"), Some(1));
    }

    #[test]
    fn nested_stebbs_enabled_default_composes_method_one() {
        // enabled=true, parameters=1 (DEFAULT) -> stebbsmethod 1.
        let yaml = "\
model:
  physics:
    stebbs:
      enabled: {value: true}
      parameters: {value: 1}
      capacitance: {value: 0}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(physics_i64(&root, "stebbsmethod"), Some(1));
        assert_eq!(physics_i64(&root, "rcmethod"), Some(0));
    }

    #[test]
    fn nested_stebbs_enabled_provided_composes_method_two() {
        // enabled=true, parameters=2 (PROVIDED) -> stebbsmethod 2.
        let yaml = "\
model:
  physics:
    stebbs:
      enabled: {value: true}
      parameters: {value: 2}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(physics_i64(&root, "stebbsmethod"), Some(2));
    }

    #[test]
    fn nested_stebbs_enum_name_parameters_resolves() {
        // parameters may be the StebbsParameterSource enum name.
        let yaml = "\
model:
  physics:
    stebbs:
      enabled: {value: true}
      parameters: {value: provided}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(physics_i64(&root, "stebbsmethod"), Some(2));
    }

    #[test]
    fn nested_stebbs_omitted_parameters_defaults_when_enabled() {
        // enabled with no `parameters` key -> DEFAULT (1), matching the
        // StebbsPhysics field default.
        let yaml = "model:\n  physics:\n    stebbs:\n      enabled: {value: true}\n";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(physics_i64(&root, "stebbsmethod"), Some(1));
    }

    #[test]
    fn legacy_flat_stebbs_master_toggle_still_parses() {
        // The legacy flat form (scalar master toggle + flat siblings) must
        // continue to flow through the existing rename walker unchanged.
        let yaml = "\
model:
  physics:
    stebbs: {value: 2}
    outer_cap_fraction: {value: 1}
    setpoint: {value: 3}
    same_albedo_wall: {value: 1}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(physics_i64(&root, "stebbsmethod"), Some(2));
        assert_eq!(physics_i64(&root, "rcmethod"), Some(1));
        assert_eq!(physics_i64(&root, "setpointmethod"), Some(3));
        assert_eq!(physics_i64(&root, "same_albedo_wall"), Some(1));
    }

    #[test]
    fn legacy_fused_stebbs_keys_untouched() {
        // Already-fused legacy keys must remain idempotent.
        let yaml = "\
model:
  physics:
    stebbsmethod: {value: 1}
    rcmethod: {value: 2}
    setpointmethod: {value: 0}
";
        let mut root: Value = from_str(yaml).unwrap();
        let before = root.clone();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(root, before, "fused legacy STEBBS keys must be untouched");
    }

    #[test]
    fn nested_stebbs_flatten_is_idempotent() {
        let yaml = "\
model:
  physics:
    stebbs:
      enabled: {value: true}
      parameters: {value: 2}
      capacitance: {value: 1}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let after_first = root.clone();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(root, after_first, "second pass must be a no-op");
    }

    #[test]
    fn nested_and_flat_stebbs_both_present_rejected() {
        // A flat `outer_cap_fraction` alongside the nested object is ambiguous.
        // (The flat key renames to `rcmethod` only AFTER flatten runs, so the
        // collision is detected against the fused `rcmethod` the fold writes.)
        let yaml = "\
model:
  physics:
    rcmethod: {value: 0}
    stebbs:
      enabled: {value: true}
      capacitance: {value: 1}
";
        let mut root: Value = from_str(yaml).unwrap();
        let err = normalize_field_names(&mut root).expect_err("ambiguous input must fail");
        assert!(err.contains("nested 'model.physics.stebbs'"), "error was: {err}");
    }

    #[test]
    fn legacy_stebbs_master_toggle_with_ref_stays_legacy() {
        // gh#1456 regression: a legacy master toggle carrying provenance --
        // `stebbs: {value: 1, ref: {...}}` -- is a RefValue scalar, NOT the
        // nested object. Its keys are exactly {value, ref}; treating `ref` as
        // a nested marker would read the absent `enabled` as false and flip
        // stebbsmethod to 0. It must compose to 1 (enabled), not 0.
        let yaml = "\
model:
  physics:
    stebbs:
      value: 1
      ref: {desc: 'STEBBS on', DOI: '10.0/x'}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(physics_i64(&root, "stebbsmethod"), Some(1));
    }

    #[test]
    fn legacy_stebbs_master_toggle_with_ref_disabled() {
        // The same RefValue-scalar path with value 0 -> disabled (stebbsmethod 0).
        let yaml = "\
model:
  physics:
    stebbs:
      value: 0
      ref: {desc: 'STEBBS off'}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(physics_i64(&root, "stebbsmethod"), Some(0));
    }

    #[test]
    fn site_properties_stebbs_section_untouched_by_flatten() {
        // Only `model.physics.stebbs` is flattened; the site-level
        // `properties.stebbs` parameter section must be left intact.
        let yaml = "\
model:
  physics:
    stebbs:
      enabled: {value: true}
      parameters: {value: 1}
sites:
  - properties:
      stebbs:
        annual_mean_air_temperature: {value: 10.0}
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        assert_eq!(physics_i64(&root, "stebbsmethod"), Some(1));
        let props = &root["sites"][0]["properties"];
        assert!(
            props.get(Value::String("stebbs".into())).is_some(),
            "site-level stebbs section must survive"
        );
    }
}
