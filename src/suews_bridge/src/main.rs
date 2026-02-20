use clap::{Parser, Subcommand};
use paste::paste;
use serde_json::json;
#[cfg(all(feature = "physics", feature = "arrow-output"))]
use std::fs;
#[cfg(all(feature = "physics", feature = "arrow-output"))]
use suews_bridge::write_output_arrow;
use suews_bridge::{
    anthro_emis_prm_default_from_fortran, anthro_emis_prm_schema, anthro_emis_prm_schema_info,
    anthro_emis_prm_schema_version, anthro_emis_prm_schema_version_runtime, anthro_emis_prm_to_map,
    anthro_emis_prm_to_values_payload, anthro_heat_prm_default_from_fortran,
    anthro_heat_prm_schema, anthro_heat_prm_schema_info, anthro_heat_prm_schema_version,
    anthro_heat_prm_schema_version_runtime, anthro_heat_prm_to_map,
    anthro_heat_prm_to_values_payload, anthroemis_state_default_from_fortran,
    anthroemis_state_schema, anthroemis_state_schema_info, anthroemis_state_schema_version,
    anthroemis_state_schema_version_runtime, anthroemis_state_to_map,
    anthroemis_state_to_values_payload, atm_state_default_from_fortran, atm_state_schema,
    atm_state_schema_info, atm_state_schema_version, atm_state_schema_version_runtime,
    atm_state_to_map, atm_state_to_values_payload, bioco2_prm_default_from_fortran,
    bioco2_prm_schema, bioco2_prm_schema_info, bioco2_prm_schema_version,
    bioco2_prm_schema_version_runtime, bioco2_prm_to_map, bioco2_prm_to_values_payload,
    building_archetype_prm_default_from_fortran, building_archetype_prm_schema,
    building_archetype_prm_schema_info, building_archetype_prm_schema_version,
    building_archetype_prm_schema_version_runtime, building_archetype_prm_to_map,
    building_archetype_prm_to_values_payload, conductance_prm_default_from_fortran,
    conductance_prm_schema, conductance_prm_schema_info, conductance_prm_schema_version,
    conductance_prm_schema_version_runtime, conductance_prm_to_map,
    conductance_prm_to_values_payload, ehc_prm_default_from_fortran, ehc_prm_schema,
    ehc_prm_schema_info, ehc_prm_schema_version, ehc_prm_schema_version_runtime, ehc_prm_to_map,
    ehc_prm_to_values_payload, error_entry_default_from_fortran, error_entry_field_names,
    error_entry_schema, error_entry_schema_info, error_entry_schema_version,
    error_entry_schema_version_runtime, error_entry_to_values_payload,
    error_state_default_from_fortran, error_state_field_names, error_state_schema,
    error_state_schema_info, error_state_schema_version, error_state_schema_version_runtime,
    error_state_to_values_payload, flag_state_default_from_fortran, flag_state_schema,
    flag_state_schema_info, flag_state_schema_version, flag_state_schema_version_runtime,
    flag_state_to_map, flag_state_to_values_payload, heat_state_default_from_fortran,
    heat_state_schema, heat_state_schema_info, heat_state_schema_version,
    heat_state_schema_version_runtime, heat_state_to_map, heat_state_to_values_payload,
    hydro_state_default_from_fortran, hydro_state_schema, hydro_state_schema_info,
    hydro_state_schema_version, hydro_state_schema_version_runtime, hydro_state_to_map,
    hydro_state_to_values_payload, irrig_daywater_default_from_fortran, irrig_daywater_schema,
    irrig_daywater_schema_info, irrig_daywater_schema_version,
    irrig_daywater_schema_version_runtime, irrig_daywater_to_map, irrig_daywater_to_values_payload,
    irrigation_prm_default_from_fortran, irrigation_prm_schema, irrigation_prm_schema_info,
    irrigation_prm_schema_version, irrigation_prm_schema_version_runtime, irrigation_prm_to_map,
    irrigation_prm_to_values_payload, lai_prm_default_from_fortran, lai_prm_schema,
    lai_prm_schema_info, lai_prm_schema_version, lai_prm_schema_version_runtime, lai_prm_to_map,
    lai_prm_to_values_payload, lc_bldg_prm_default_from_fortran, lc_bldg_prm_schema,
    lc_bldg_prm_schema_info, lc_bldg_prm_schema_version, lc_bldg_prm_schema_version_runtime,
    lc_bldg_prm_to_map, lc_bldg_prm_to_values_payload, lc_bsoil_prm_default_from_fortran,
    lc_bsoil_prm_schema, lc_bsoil_prm_schema_info, lc_bsoil_prm_schema_version,
    lc_bsoil_prm_schema_version_runtime, lc_bsoil_prm_to_map, lc_bsoil_prm_to_values_payload,
    lc_dectr_prm_default_from_fortran, lc_dectr_prm_schema, lc_dectr_prm_schema_info,
    lc_dectr_prm_schema_version, lc_dectr_prm_schema_version_runtime, lc_dectr_prm_to_map,
    lc_dectr_prm_to_values_payload, lc_evetr_prm_default_from_fortran, lc_evetr_prm_schema,
    lc_evetr_prm_schema_info, lc_evetr_prm_schema_version, lc_evetr_prm_schema_version_runtime,
    lc_evetr_prm_to_map, lc_evetr_prm_to_values_payload, lc_grass_prm_default_from_fortran,
    lc_grass_prm_schema, lc_grass_prm_schema_info, lc_grass_prm_schema_version,
    lc_grass_prm_schema_version_runtime, lc_grass_prm_to_map, lc_grass_prm_to_values_payload,
    lc_paved_prm_default_from_fortran, lc_paved_prm_schema, lc_paved_prm_schema_info,
    lc_paved_prm_schema_version, lc_paved_prm_schema_version_runtime, lc_paved_prm_to_map,
    lc_paved_prm_to_values_payload, lc_water_prm_default_from_fortran, lc_water_prm_schema,
    lc_water_prm_schema_info, lc_water_prm_schema_version, lc_water_prm_schema_version_runtime,
    lc_water_prm_to_map, lc_water_prm_to_values_payload, lumps_prm_default_from_fortran,
    lumps_prm_schema, lumps_prm_schema_info, lumps_prm_schema_version,
    lumps_prm_schema_version_runtime, lumps_prm_to_map, lumps_prm_to_values_payload,
    nhood_state_default_from_fortran, nhood_state_schema, nhood_state_schema_info,
    nhood_state_schema_version, nhood_state_schema_version_runtime, nhood_state_to_map,
    nhood_state_to_values_payload, ohm_coef_lc_default_from_fortran, ohm_coef_lc_schema,
    ohm_coef_lc_schema_info, ohm_coef_lc_schema_version, ohm_coef_lc_schema_version_runtime,
    ohm_coef_lc_to_map, ohm_coef_lc_to_values_payload, ohm_prm_default_from_fortran,
    ohm_prm_schema, ohm_prm_schema_info, ohm_prm_schema_version, ohm_prm_schema_version_runtime,
    ohm_prm_to_map, ohm_prm_to_values_payload, ohm_state_default_from_fortran,
    ohm_state_field_names, ohm_state_schema, ohm_state_schema_info, ohm_state_schema_version,
    ohm_state_schema_version_runtime, ohm_state_to_map, ohm_state_to_values_payload,
    output_block_default_from_fortran, output_block_field_names, output_block_schema,
    output_block_schema_info, output_block_schema_version, output_block_schema_version_runtime,
    output_block_to_rows_map, output_block_to_values_payload, output_line_default_from_fortran,
    output_line_schema, output_line_schema_info, output_line_schema_version,
    output_line_schema_version_runtime, output_line_to_map, output_line_to_values_payload,
    phenology_state_default_from_fortran, phenology_state_schema, phenology_state_schema_info,
    phenology_state_schema_version, phenology_state_schema_version_runtime, phenology_state_to_map,
    phenology_state_to_values_payload, roughness_state_default_from_fortran,
    roughness_state_schema, roughness_state_schema_info, roughness_state_schema_version,
    roughness_state_schema_version_runtime, roughness_state_to_map,
    roughness_state_to_values_payload, snow_prm_default_from_fortran, snow_prm_schema,
    snow_prm_schema_info, snow_prm_schema_version, snow_prm_schema_version_runtime,
    snow_prm_to_map, snow_prm_to_values_payload, snow_state_default_from_fortran,
    snow_state_schema, snow_state_schema_info, snow_state_schema_version,
    snow_state_schema_version_runtime, snow_state_to_map, snow_state_to_values_payload,
    soil_prm_default_from_fortran, soil_prm_schema, soil_prm_schema_info, soil_prm_schema_version,
    soil_prm_schema_version_runtime, soil_prm_to_map, soil_prm_to_values_payload,
    solar_state_default_from_fortran, solar_state_schema, solar_state_schema_info,
    solar_state_schema_version, solar_state_schema_version_runtime, solar_state_to_map,
    solar_state_to_values_payload, spartacus_layer_prm_default_from_fortran,
    spartacus_layer_prm_schema, spartacus_layer_prm_schema_info,
    spartacus_layer_prm_schema_version, spartacus_layer_prm_schema_version_runtime,
    spartacus_layer_prm_to_map, spartacus_layer_prm_to_values_payload,
    spartacus_prm_default_from_fortran, spartacus_prm_schema, spartacus_prm_schema_info,
    spartacus_prm_schema_version, spartacus_prm_schema_version_runtime, spartacus_prm_to_map,
    spartacus_prm_to_values_payload, stebbs_prm_default_from_fortran, stebbs_prm_schema,
    stebbs_prm_schema_info, stebbs_prm_schema_version, stebbs_prm_schema_version_runtime,
    stebbs_prm_to_map, stebbs_prm_to_values_payload, suews_config_default_from_fortran,
    suews_config_schema, suews_config_schema_info, suews_config_schema_version,
    suews_config_schema_version_runtime, suews_config_to_map, suews_config_to_values_payload,
    suews_forcing_default_from_fortran, suews_forcing_schema, suews_forcing_schema_info,
    suews_forcing_schema_version, suews_forcing_schema_version_runtime, suews_forcing_to_map,
    suews_forcing_to_values_payload, suews_site_default_from_fortran, suews_site_field_names,
    suews_site_member_names, suews_site_schema_info, suews_site_schema_version,
    suews_site_schema_version_runtime, suews_site_to_map, suews_site_to_nested_payload,
    suews_site_to_values_payload, suews_timer_default_from_fortran, suews_timer_schema,
    suews_timer_schema_info, suews_timer_schema_version, suews_timer_schema_version_runtime,
    suews_timer_to_map, suews_timer_to_values_payload, surf_store_prm_default_from_fortran,
    surf_store_prm_schema, surf_store_prm_schema_info, surf_store_prm_schema_version,
    surf_store_prm_schema_version_runtime, surf_store_prm_to_map, surf_store_prm_to_values_payload,
    water_dist_prm_default_from_fortran, water_dist_prm_schema, water_dist_prm_schema_info,
    water_dist_prm_schema_version, water_dist_prm_schema_version_runtime, water_dist_prm_to_map,
    water_dist_prm_to_values_payload,
};
#[cfg(all(feature = "physics", feature = "arrow-output"))]
use suews_bridge::{
    interpolate_forcing, load_run_config, read_forcing_block, run_from_config_str_and_forcing,
};

#[derive(Debug, Parser)]
#[command(name = "suews", version = env!("SUEWS_VERSION"), about = "SUEWS — Surface Urban Energy and Water Balance Scheme")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    #[cfg(all(feature = "physics", feature = "arrow-output"))]
    /// Run SUEWS batch simulation from YAML config.
    Run {
        /// Path to config YAML.
        config: String,
    },
    /// Inspect bridge type schemas and samples.
    Schema {
        #[command(subcommand)]
        command: SchemaType,
    },
}

#[derive(Debug, Subcommand, Clone)]
enum StandardSchemaAction {
    /// Print JSON schema.
    SchemaJson,
    /// Print sample as JSON map payload.
    SampleJson,
    /// Print sample as JSON ordered values payload.
    SampleValuesJson,
}

#[derive(Debug, Subcommand, Clone)]
enum SuewsSiteSchemaAction {
    /// Print JSON schema.
    SchemaJson,
    /// Print sample as JSON map payload.
    SampleJson,
    /// Print sample as JSON ordered values payload.
    SampleValuesJson,
    /// Print sample as nested composite payload.
    SampleNestedJson,
}

#[derive(Debug, Subcommand, Clone)]
enum OhmStateAction {
    /// Print JSON schema.
    SchemaJson,
    /// Print sample as JSON map payload.
    SampleJson,
    /// Print sample as JSON ordered values payload.
    SampleValuesJson,
    /// Print flat schema with index and field name.
    FlatSchema,
}

#[derive(Debug, Subcommand, Clone)]
enum SchemaType {
    /// Configuration types (suews-config, suews-timer, suews-forcing, suews-site).
    Config {
        #[command(subcommand)]
        command: SchemaConfigType,
    },
    /// Model state types (ohm, hydro, heat, flag, ...).
    State {
        #[command(subcommand)]
        command: SchemaStateType,
    },
    /// Physical parameters (prm types, coefficients, irrigation).
    Param {
        #[command(subcommand)]
        command: SchemaParamType,
    },
    /// Output and error types.
    Output {
        #[command(subcommand)]
        command: SchemaOutputType,
    },
}

#[derive(Debug, Subcommand, Clone)]
enum SchemaConfigType {
    /// SUEWS_CONFIG.
    SuewsConfig {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SUEWS_TIMER.
    SuewsTimer {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SUEWS_FORCING.
    SuewsForcing {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SUEWS_SITE.
    SuewsSite {
        #[command(subcommand)]
        action: SuewsSiteSchemaAction,
    },
}

#[derive(Debug, Subcommand, Clone)]
enum SchemaStateType {
    /// OHM_STATE (schema, samples, and OHM physics).
    Ohm {
        #[command(subcommand)]
        action: OhmStateAction,
    },
    /// HYDRO_STATE.
    Hydro {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// HEAT_STATE.
    Heat {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// FLAG_STATE.
    Flag {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// anthroEmis_STATE.
    Anthroemis {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// ATM_STATE.
    Atm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// PHENOLOGY_STATE.
    Phenology {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SNOW_STATE.
    Snow {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SOLAR_STATE.
    Solar {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// ROUGHNESS_STATE.
    Roughness {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// NHOOD_STATE.
    Nhood {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
}

#[derive(Debug, Subcommand, Clone)]
enum SchemaParamType {
    /// anthroHEAT_PRM.
    AnthroHeat {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// anthroEMIS_PRM.
    AnthroEmis {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// BUILDING_ARCHETYPE_PRM.
    BuildingArchetype {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// STEBBS_PRM.
    Stebbs {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// CONDUCTANCE_PRM.
    Conductance {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// EHC_PRM.
    Ehc {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SPARTACUS_PRM.
    Spartacus {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SPARTACUS_LAYER_PRM.
    SpartacusLayer {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// bioCO2_PRM.
    Bioco2 {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LAI_PRM.
    Lai {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SNOW_PRM.
    Snow {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SOIL_PRM.
    Soil {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LUMPS_PRM.
    Lumps {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// OHM_COEF_LC.
    OhmCoefLc {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// OHM_PRM.
    Ohm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_PAVED_PRM.
    LcPaved {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_BLDG_PRM.
    LcBldg {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_BSOIL_PRM.
    LcBsoil {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_WATER_PRM.
    LcWater {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_DECTR_PRM.
    LcDectr {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_EVETR_PRM.
    LcEvetr {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_GRASS_PRM.
    LcGrass {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SURF_STORE_PRM.
    SurfStore {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// WATER_DIST_PRM.
    WaterDist {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// IRRIG_daywater.
    IrrigDaywater {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// IRRIGATION_PRM.
    Irrigation {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
}

#[derive(Debug, Subcommand, Clone)]
enum SchemaOutputType {
    /// output_line.
    Line {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// output_block.
    Block {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// error_entry.
    ErrorEntry {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// error_state.
    ErrorState {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
}

#[derive(Debug, Subcommand)]
enum FlatCommand {
    #[cfg(all(feature = "physics", feature = "arrow-output"))]
    /// Run SUEWS batch simulation from YAML config.
    Run {
        /// Path to config YAML.
        config: String,
    },
    /// Print OHM_STATE flat schema with index and field name.
    StateSchema,
    /// Print OHM_STATE schema as JSON for programmatic tooling.
    StateSchemaJson,
    /// Print sample OHM_STATE as JSON map payload.
    StateSampleJson,
    /// Print sample OHM_STATE as JSON ordered values payload.
    StateSampleValuesJson,
    /// Print SUEWS_CONFIG schema as JSON for programmatic tooling.
    SuewsConfigSchemaJson,
    /// Print sample SUEWS_CONFIG as JSON map payload.
    SuewsConfigSampleJson,
    /// Print sample SUEWS_CONFIG as JSON ordered values payload.
    SuewsConfigSampleValuesJson,
    /// Print SUEWS_FORCING schema as JSON for programmatic tooling.
    SuewsForcingSchemaJson,
    /// Print sample SUEWS_FORCING as JSON map payload.
    SuewsForcingSampleJson,
    /// Print sample SUEWS_FORCING as JSON ordered values payload.
    SuewsForcingSampleValuesJson,
    /// Print HYDRO_STATE schema as JSON for programmatic tooling.
    HydroStateSchemaJson,
    /// Print sample HYDRO_STATE as JSON map payload.
    HydroStateSampleJson,
    /// Print sample HYDRO_STATE as JSON ordered values payload.
    HydroStateSampleValuesJson,
    /// Print HEAT_STATE schema as JSON for programmatic tooling.
    HeatStateSchemaJson,
    /// Print sample HEAT_STATE as JSON map payload.
    HeatStateSampleJson,
    /// Print sample HEAT_STATE as JSON ordered values payload.
    HeatStateSampleValuesJson,
    /// Print SUEWS_TIMER schema as JSON for programmatic tooling.
    SuewsTimerSchemaJson,
    /// Print sample SUEWS_TIMER as JSON map payload.
    SuewsTimerSampleJson,
    /// Print sample SUEWS_TIMER as JSON ordered values payload.
    SuewsTimerSampleValuesJson,
    /// Print flag_STATE schema as JSON for programmatic tooling.
    FlagStateSchemaJson,
    /// Print sample flag_STATE as JSON map payload.
    FlagStateSampleJson,
    /// Print sample flag_STATE as JSON ordered values payload.
    FlagStateSampleValuesJson,
    /// Print anthroEmis_STATE schema as JSON for programmatic tooling.
    AnthroemisStateSchemaJson,
    /// Print sample anthroEmis_STATE as JSON map payload.
    AnthroemisStateSampleJson,
    /// Print sample anthroEmis_STATE as JSON ordered values payload.
    AnthroemisStateSampleValuesJson,
    /// Print anthroHEAT_PRM schema as JSON for programmatic tooling.
    AnthroHeatPrmSchemaJson,
    /// Print sample anthroHEAT_PRM as JSON map payload.
    AnthroHeatPrmSampleJson,
    /// Print sample anthroHEAT_PRM as JSON ordered values payload.
    AnthroHeatPrmSampleValuesJson,
    /// Print anthroEMIS_PRM schema as JSON for programmatic tooling.
    AnthroEmisPrmSchemaJson,
    /// Print sample anthroEMIS_PRM as JSON map payload.
    AnthroEmisPrmSampleJson,
    /// Print sample anthroEMIS_PRM as JSON ordered values payload.
    AnthroEmisPrmSampleValuesJson,
    /// Print atm_state schema as JSON for programmatic tooling.
    AtmStateSchemaJson,
    /// Print sample atm_state as JSON map payload.
    AtmStateSampleJson,
    /// Print sample atm_state as JSON ordered values payload.
    AtmStateSampleValuesJson,
    /// Print BUILDING_ARCHETYPE_PRM schema as JSON for programmatic tooling.
    BuildingArchetypePrmSchemaJson,
    /// Print sample BUILDING_ARCHETYPE_PRM as JSON map payload.
    BuildingArchetypePrmSampleJson,
    /// Print sample BUILDING_ARCHETYPE_PRM as JSON ordered values payload.
    BuildingArchetypePrmSampleValuesJson,
    /// Print STEBBS_PRM schema as JSON for programmatic tooling.
    StebbsPrmSchemaJson,
    /// Print sample STEBBS_PRM as JSON map payload.
    StebbsPrmSampleJson,
    /// Print sample STEBBS_PRM as JSON ordered values payload.
    StebbsPrmSampleValuesJson,
    /// Print output_line schema as JSON for programmatic tooling.
    OutputLineSchemaJson,
    /// Print sample output_line as JSON map payload.
    OutputLineSampleJson,
    /// Print sample output_line as JSON ordered values payload.
    OutputLineSampleValuesJson,
    /// Print output_block schema as JSON for programmatic tooling.
    OutputBlockSchemaJson,
    /// Print sample output_block as JSON state payload.
    OutputBlockSampleJson,
    /// Print sample output_block as JSON ordered values payload with dims.
    OutputBlockSampleValuesJson,
    /// Print error_entry schema as JSON for programmatic tooling.
    ErrorEntrySchemaJson,
    /// Print sample error_entry as JSON state payload.
    ErrorEntrySampleJson,
    /// Print sample error_entry as JSON structured values payload.
    ErrorEntrySampleValuesJson,
    /// Print error_state schema as JSON for programmatic tooling.
    ErrorStateSchemaJson,
    /// Print sample error_state as JSON state payload.
    ErrorStateSampleJson,
    /// Print sample error_state as JSON structured values payload.
    ErrorStateSampleValuesJson,
    /// Print CONDUCTANCE_PRM schema as JSON for programmatic tooling.
    ConductancePrmSchemaJson,
    /// Print sample CONDUCTANCE_PRM as JSON map payload.
    ConductancePrmSampleJson,
    /// Print sample CONDUCTANCE_PRM as JSON ordered values payload.
    ConductancePrmSampleValuesJson,
    /// Print EHC_PRM schema as JSON for programmatic tooling.
    EhcPrmSchemaJson,
    /// Print sample EHC_PRM as JSON map payload.
    EhcPrmSampleJson,
    /// Print SPARTACUS_PRM schema as JSON for programmatic tooling.
    SpartacusPrmSchemaJson,
    /// Print sample SPARTACUS_PRM as JSON map payload.
    SpartacusPrmSampleJson,
    /// Print sample SPARTACUS_PRM as JSON ordered values payload.
    SpartacusPrmSampleValuesJson,
    /// Print SPARTACUS_LAYER_PRM schema as JSON for programmatic tooling.
    SpartacusLayerPrmSchemaJson,
    /// Print sample SPARTACUS_LAYER_PRM as JSON map payload.
    SpartacusLayerPrmSampleJson,
    /// Print sample SPARTACUS_LAYER_PRM as JSON ordered values payload.
    SpartacusLayerPrmSampleValuesJson,
    /// Print SUEWS_SITE schema as JSON for programmatic tooling.
    SuewsSiteSchemaJson,
    /// Print sample SUEWS_SITE as flattened member.field JSON map payload.
    SuewsSiteSampleJson,
    /// Print sample SUEWS_SITE as structured JSON values payload.
    SuewsSiteSampleValuesJson,
    /// Print sample SUEWS_SITE as nested composite payload JSON.
    SuewsSiteSampleNestedJson,
    /// Print sample EHC_PRM as JSON ordered values payload.
    EhcPrmSampleValuesJson,
    /// Print bioCO2_PRM schema as JSON for programmatic tooling.
    Bioco2PrmSchemaJson,
    /// Print sample bioCO2_PRM as JSON map payload.
    Bioco2PrmSampleJson,
    /// Print sample bioCO2_PRM as JSON ordered values payload.
    Bioco2PrmSampleValuesJson,
    /// Print LAI_PRM schema as JSON for programmatic tooling.
    LaiPrmSchemaJson,
    /// Print sample LAI_PRM as JSON map payload.
    LaiPrmSampleJson,
    /// Print sample LAI_PRM as JSON ordered values payload.
    LaiPrmSampleValuesJson,
    /// Print PHENOLOGY_STATE schema as JSON for programmatic tooling.
    PhenologyStateSchemaJson,
    /// Print sample PHENOLOGY_STATE as JSON map payload.
    PhenologyStateSampleJson,
    /// Print sample PHENOLOGY_STATE as JSON ordered values payload.
    PhenologyStateSampleValuesJson,
    /// Print SNOW_STATE schema as JSON for programmatic tooling.
    SnowStateSchemaJson,
    /// Print sample SNOW_STATE as JSON map payload.
    SnowStateSampleJson,
    /// Print sample SNOW_STATE as JSON ordered values payload.
    SnowStateSampleValuesJson,
    /// Print SNOW_PRM schema as JSON for programmatic tooling.
    SnowPrmSchemaJson,
    /// Print sample SNOW_PRM as JSON map payload.
    SnowPrmSampleJson,
    /// Print sample SNOW_PRM as JSON ordered values payload.
    SnowPrmSampleValuesJson,
    /// Print SOIL_PRM schema as JSON for programmatic tooling.
    SoilPrmSchemaJson,
    /// Print sample SOIL_PRM as JSON map payload.
    SoilPrmSampleJson,
    /// Print sample SOIL_PRM as JSON ordered values payload.
    SoilPrmSampleValuesJson,
    /// Print LC_PAVED_PRM schema as JSON for programmatic tooling.
    LcPavedPrmSchemaJson,
    /// Print sample LC_PAVED_PRM as JSON map payload.
    LcPavedPrmSampleJson,
    /// Print sample LC_PAVED_PRM as JSON ordered values payload.
    LcPavedPrmSampleValuesJson,
    /// Print LC_BLDG_PRM schema as JSON for programmatic tooling.
    LcBldgPrmSchemaJson,
    /// Print sample LC_BLDG_PRM as JSON map payload.
    LcBldgPrmSampleJson,
    /// Print sample LC_BLDG_PRM as JSON ordered values payload.
    LcBldgPrmSampleValuesJson,
    /// Print LC_BSOIL_PRM schema as JSON for programmatic tooling.
    LcBsoilPrmSchemaJson,
    /// Print sample LC_BSOIL_PRM as JSON map payload.
    LcBsoilPrmSampleJson,
    /// Print sample LC_BSOIL_PRM as JSON ordered values payload.
    LcBsoilPrmSampleValuesJson,
    /// Print LC_WATER_PRM schema as JSON for programmatic tooling.
    LcWaterPrmSchemaJson,
    /// Print sample LC_WATER_PRM as JSON map payload.
    LcWaterPrmSampleJson,
    /// Print sample LC_WATER_PRM as JSON ordered values payload.
    LcWaterPrmSampleValuesJson,
    /// Print LC_DECTR_PRM schema as JSON for programmatic tooling.
    LcDectrPrmSchemaJson,
    /// Print sample LC_DECTR_PRM as JSON map payload.
    LcDectrPrmSampleJson,
    /// Print sample LC_DECTR_PRM as JSON ordered values payload.
    LcDectrPrmSampleValuesJson,
    /// Print LC_EVETR_PRM schema as JSON for programmatic tooling.
    LcEvetrPrmSchemaJson,
    /// Print sample LC_EVETR_PRM as JSON map payload.
    LcEvetrPrmSampleJson,
    /// Print sample LC_EVETR_PRM as JSON ordered values payload.
    LcEvetrPrmSampleValuesJson,
    /// Print LC_GRASS_PRM schema as JSON for programmatic tooling.
    LcGrassPrmSchemaJson,
    /// Print sample LC_GRASS_PRM as JSON map payload.
    LcGrassPrmSampleJson,
    /// Print sample LC_GRASS_PRM as JSON ordered values payload.
    LcGrassPrmSampleValuesJson,
    /// Print SURF_STORE_PRM schema as JSON for programmatic tooling.
    SurfStorePrmSchemaJson,
    /// Print sample SURF_STORE_PRM as JSON map payload.
    SurfStorePrmSampleJson,
    /// Print sample SURF_STORE_PRM as JSON ordered values payload.
    SurfStorePrmSampleValuesJson,
    /// Print WATER_DIST_PRM schema as JSON for programmatic tooling.
    WaterDistPrmSchemaJson,
    /// Print sample WATER_DIST_PRM as JSON map payload.
    WaterDistPrmSampleJson,
    /// Print sample WATER_DIST_PRM as JSON ordered values payload.
    WaterDistPrmSampleValuesJson,
    /// Print IRRIG_daywater schema as JSON for programmatic tooling.
    IrrigDaywaterSchemaJson,
    /// Print sample IRRIG_daywater as JSON map payload.
    IrrigDaywaterSampleJson,
    /// Print sample IRRIG_daywater as JSON ordered values payload.
    IrrigDaywaterSampleValuesJson,
    /// Print IRRIGATION_PRM schema as JSON for programmatic tooling.
    IrrigationPrmSchemaJson,
    /// Print sample IRRIGATION_PRM as JSON map payload.
    IrrigationPrmSampleJson,
    /// Print sample IRRIGATION_PRM as JSON ordered values payload.
    IrrigationPrmSampleValuesJson,
    /// Print LUMPS_PRM schema as JSON for programmatic tooling.
    LumpsPrmSchemaJson,
    /// Print sample LUMPS_PRM as JSON map payload.
    LumpsPrmSampleJson,
    /// Print sample LUMPS_PRM as JSON ordered values payload.
    LumpsPrmSampleValuesJson,
    /// Print OHM_COEF_LC schema as JSON for programmatic tooling.
    OhmCoefLcSchemaJson,
    /// Print sample OHM_COEF_LC as JSON map payload.
    OhmCoefLcSampleJson,
    /// Print sample OHM_COEF_LC as JSON ordered values payload.
    OhmCoefLcSampleValuesJson,
    /// Print OHM_PRM schema as JSON for programmatic tooling.
    OhmPrmSchemaJson,
    /// Print sample OHM_PRM as JSON map payload.
    OhmPrmSampleJson,
    /// Print sample OHM_PRM as JSON ordered values payload.
    OhmPrmSampleValuesJson,
    /// Print solar_State schema as JSON for programmatic tooling.
    SolarStateSchemaJson,
    /// Print sample solar_State as JSON map payload.
    SolarStateSampleJson,
    /// Print sample solar_State as JSON ordered values payload.
    SolarStateSampleValuesJson,
    /// Print ROUGHNESS_STATE schema as JSON for programmatic tooling.
    RoughnessStateSchemaJson,
    /// Print sample ROUGHNESS_STATE as JSON map payload.
    RoughnessStateSampleJson,
    /// Print sample ROUGHNESS_STATE as JSON ordered values payload.
    RoughnessStateSampleValuesJson,
    /// Print NHOOD_STATE schema as JSON for programmatic tooling.
    NhoodStateSchemaJson,
    /// Print sample NHOOD_STATE as JSON map payload.
    NhoodStateSampleJson,
    /// Print sample NHOOD_STATE as JSON ordered values payload.
    NhoodStateSampleValuesJson,
}

fn main() {
    // No subcommand: show prominent error + banner + help, then exit
    if std::env::args().len() == 1 {
        eprintln!("\x1b[1;31merror:\x1b[0m no subcommand provided. Run \x1b[1msuews run <config>\x1b[0m or \x1b[1msuews --help\x1b[0m\n");
        print_banner();
        Cli::parse_from(["suews", "--help"]);
    }

    let cli = Cli::parse();

    let code = match run(cli) {
        Ok(()) => 0,
        Err(err) => {
            eprintln!("error: {err}");
            1
        }
    };

    std::process::exit(code);
}

#[cfg(all(feature = "physics", feature = "arrow-output"))]
fn run_physics_command(config_path: &str) -> Result<(), String> {
    let config_path = std::path::PathBuf::from(config_path);
    let config_yaml = fs::read_to_string(&config_path)
        .map_err(|e| format!("failed to read config {}: {e}", config_path.display()))?;
    let run_cfg = load_run_config(&config_path)?;
    let raw_forcing = read_forcing_block(&run_cfg.forcing_path)?;
    let forcing = interpolate_forcing(&raw_forcing, run_cfg.timer.tstep)?;

    let (output_block, _state, len_sim) =
        run_from_config_str_and_forcing(&config_yaml, forcing.block, forcing.len_sim)
            .map_err(|e| e.to_string())?;

    let output_path = write_output_arrow(&run_cfg.output_dir, &output_block, len_sim)?;

    println!("simulation complete");
    println!("timesteps_processed={}", len_sim);
    println!("output_file={}", output_path.display());

    Ok(())
}

macro_rules! handle_simple_schema {
    ($action:expr, $prefix:ident) => {{
        paste! {
            match $action {
                StandardSchemaAction::SchemaJson => {
                    run_flat(FlatCommand::[<$prefix SchemaJson>])?;
                }
                StandardSchemaAction::SampleJson => {
                    run_flat(FlatCommand::[<$prefix SampleJson>])?;
                }
                StandardSchemaAction::SampleValuesJson => {
                    run_flat(FlatCommand::[<$prefix SampleValuesJson>])?;
                }
            }
        }
    }};
}

fn run_ohm_state_action(action: OhmStateAction) -> Result<(), String> {
    match action {
        OhmStateAction::SchemaJson => run_flat(FlatCommand::StateSchemaJson)?,
        OhmStateAction::SampleJson => run_flat(FlatCommand::StateSampleJson)?,
        OhmStateAction::SampleValuesJson => run_flat(FlatCommand::StateSampleValuesJson)?,
        OhmStateAction::FlatSchema => run_flat(FlatCommand::StateSchema)?,
    }

    Ok(())
}

fn run_suews_site_action(action: SuewsSiteSchemaAction) -> Result<(), String> {
    match action {
        SuewsSiteSchemaAction::SchemaJson => run_flat(FlatCommand::SuewsSiteSchemaJson)?,
        SuewsSiteSchemaAction::SampleJson => run_flat(FlatCommand::SuewsSiteSampleJson)?,
        SuewsSiteSchemaAction::SampleValuesJson => {
            run_flat(FlatCommand::SuewsSiteSampleValuesJson)?
        }
        SuewsSiteSchemaAction::SampleNestedJson => {
            run_flat(FlatCommand::SuewsSiteSampleNestedJson)?
        }
    }

    Ok(())
}

fn run_schema(schema_type: SchemaType) -> Result<(), String> {
    match schema_type {
        SchemaType::Config { command } => run_schema_config(command),
        SchemaType::State { command } => run_schema_state(command),
        SchemaType::Param { command } => run_schema_param(command),
        SchemaType::Output { command } => run_schema_output(command),
    }
}

fn run_schema_config(command: SchemaConfigType) -> Result<(), String> {
    match command {
        SchemaConfigType::SuewsConfig { action } => handle_simple_schema!(action, SuewsConfig),
        SchemaConfigType::SuewsTimer { action } => handle_simple_schema!(action, SuewsTimer),
        SchemaConfigType::SuewsForcing { action } => handle_simple_schema!(action, SuewsForcing),
        SchemaConfigType::SuewsSite { action } => run_suews_site_action(action)?,
    }

    Ok(())
}

fn run_schema_state(command: SchemaStateType) -> Result<(), String> {
    match command {
        SchemaStateType::Ohm { action } => run_ohm_state_action(action)?,
        SchemaStateType::Hydro { action } => handle_simple_schema!(action, HydroState),
        SchemaStateType::Heat { action } => handle_simple_schema!(action, HeatState),
        SchemaStateType::Flag { action } => handle_simple_schema!(action, FlagState),
        SchemaStateType::Anthroemis { action } => handle_simple_schema!(action, AnthroemisState),
        SchemaStateType::Atm { action } => handle_simple_schema!(action, AtmState),
        SchemaStateType::Phenology { action } => handle_simple_schema!(action, PhenologyState),
        SchemaStateType::Snow { action } => handle_simple_schema!(action, SnowState),
        SchemaStateType::Solar { action } => handle_simple_schema!(action, SolarState),
        SchemaStateType::Roughness { action } => handle_simple_schema!(action, RoughnessState),
        SchemaStateType::Nhood { action } => handle_simple_schema!(action, NhoodState),
    }

    Ok(())
}

fn run_schema_param(command: SchemaParamType) -> Result<(), String> {
    match command {
        SchemaParamType::AnthroHeat { action } => handle_simple_schema!(action, AnthroHeatPrm),
        SchemaParamType::AnthroEmis { action } => handle_simple_schema!(action, AnthroEmisPrm),
        SchemaParamType::BuildingArchetype { action } => {
            handle_simple_schema!(action, BuildingArchetypePrm)
        }
        SchemaParamType::Stebbs { action } => handle_simple_schema!(action, StebbsPrm),
        SchemaParamType::Conductance { action } => handle_simple_schema!(action, ConductancePrm),
        SchemaParamType::Ehc { action } => handle_simple_schema!(action, EhcPrm),
        SchemaParamType::Spartacus { action } => handle_simple_schema!(action, SpartacusPrm),
        SchemaParamType::SpartacusLayer { action } => {
            handle_simple_schema!(action, SpartacusLayerPrm)
        }
        SchemaParamType::Bioco2 { action } => handle_simple_schema!(action, Bioco2Prm),
        SchemaParamType::Lai { action } => handle_simple_schema!(action, LaiPrm),
        SchemaParamType::Snow { action } => handle_simple_schema!(action, SnowPrm),
        SchemaParamType::Soil { action } => handle_simple_schema!(action, SoilPrm),
        SchemaParamType::Lumps { action } => handle_simple_schema!(action, LumpsPrm),
        SchemaParamType::OhmCoefLc { action } => handle_simple_schema!(action, OhmCoefLc),
        SchemaParamType::Ohm { action } => handle_simple_schema!(action, OhmPrm),
        SchemaParamType::LcPaved { action } => handle_simple_schema!(action, LcPavedPrm),
        SchemaParamType::LcBldg { action } => handle_simple_schema!(action, LcBldgPrm),
        SchemaParamType::LcBsoil { action } => handle_simple_schema!(action, LcBsoilPrm),
        SchemaParamType::LcWater { action } => handle_simple_schema!(action, LcWaterPrm),
        SchemaParamType::LcDectr { action } => handle_simple_schema!(action, LcDectrPrm),
        SchemaParamType::LcEvetr { action } => handle_simple_schema!(action, LcEvetrPrm),
        SchemaParamType::LcGrass { action } => handle_simple_schema!(action, LcGrassPrm),
        SchemaParamType::SurfStore { action } => handle_simple_schema!(action, SurfStorePrm),
        SchemaParamType::WaterDist { action } => handle_simple_schema!(action, WaterDistPrm),
        SchemaParamType::IrrigDaywater { action } => handle_simple_schema!(action, IrrigDaywater),
        SchemaParamType::Irrigation { action } => handle_simple_schema!(action, IrrigationPrm),
    }

    Ok(())
}

fn run_schema_output(command: SchemaOutputType) -> Result<(), String> {
    match command {
        SchemaOutputType::Line { action } => handle_simple_schema!(action, OutputLine),
        SchemaOutputType::Block { action } => handle_simple_schema!(action, OutputBlock),
        SchemaOutputType::ErrorEntry { action } => handle_simple_schema!(action, ErrorEntry),
        SchemaOutputType::ErrorState { action } => handle_simple_schema!(action, ErrorState),
    }

    Ok(())
}

fn print_banner() {
    eprintln!("===========================================");
    eprintln!(
        "SUEWS version: {} ({})",
        env!("SUEWS_VERSION"),
        env!("SUEWS_COMMIT")
    );
    eprintln!();
    eprintln!("Website: https://suews.io/");
    eprintln!("===========================================");
    eprintln!();
}

fn run(cli: Cli) -> Result<(), String> {
    match cli.command {
        #[cfg(all(feature = "physics", feature = "arrow-output"))]
        Commands::Run { config } => {
            print_banner();
            run_physics_command(&config)?;
        }
        Commands::Schema { command } => run_schema(command)?,
    }

    Ok(())
}

fn run_flat(command: FlatCommand) -> Result<(), String> {
    macro_rules! render_simple_schema_json {
        ($fn_prefix:ident) => {{
            let schema = paste!([<$fn_prefix _schema_info>]()).map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": paste!([<$fn_prefix _schema_version_runtime>]()).map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }};
    }

    macro_rules! render_simple_default_json {
        ($fn_prefix:ident) => {{
            let flat_len = paste!([<$fn_prefix _schema>]()).map_err(|e| e.to_string())?;
            let state =
                paste!([<$fn_prefix _default_from_fortran>]()).map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": paste!([<$fn_prefix _schema_version>]()),
                "schema_version_runtime": paste!([<$fn_prefix _schema_version_runtime>]()).map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": paste!([<$fn_prefix _to_map>](&state)),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }};
    }

    macro_rules! render_simple_default_values_json {
        ($fn_prefix:ident) => {{
            let state =
                paste!([<$fn_prefix _default_from_fortran>]()).map_err(|e| e.to_string())?;
            let payload = paste!([<$fn_prefix _to_values_payload>](&state));
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": paste!([<$fn_prefix _schema_version_runtime>]()).map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }};
    }

    match command {
        #[cfg(all(feature = "physics", feature = "arrow-output"))]
        FlatCommand::Run { config } => {
            run_physics_command(&config)?;
        }
        FlatCommand::StateSchema => {
            let (flat_len, nsurf) = ohm_state_schema().map_err(|e| e.to_string())?;
            let fields = ohm_state_field_names();

            println!("schema_flat_len={flat_len}");
            println!("schema_nsurf={nsurf}");
            for (idx, name) in fields.iter().enumerate() {
                println!("{idx:02} {name}");
            }
        }
        FlatCommand::StateSchemaJson => {
            let schema = ohm_state_schema_info().map_err(|e| e.to_string())?;

            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": ohm_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "nsurf": schema.nsurf,
                "surface_names": schema.surface_names,
                "fields": schema.field_names,
            });

            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::StateSampleJson => {
            let state = ohm_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": ohm_state_schema_version(),
                "schema_version_runtime": ohm_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "state": ohm_state_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::StateSampleValuesJson => {
            let state = ohm_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = ohm_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": ohm_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SuewsConfigSchemaJson => {
            let schema = suews_config_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": suews_config_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SuewsConfigSampleJson => {
            let flat_len = suews_config_schema().map_err(|e| e.to_string())?;
            let state = suews_config_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": suews_config_schema_version(),
                "schema_version_runtime": suews_config_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": suews_config_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SuewsConfigSampleValuesJson => {
            let state = suews_config_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = suews_config_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": suews_config_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SuewsForcingSchemaJson => {
            let schema = suews_forcing_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": suews_forcing_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "base_flat_len": schema.base_flat_len,
                "ts5mindata_ir_len": schema.ts5mindata_ir_len,
                "allocatable_dims": schema.allocatable_dims,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SuewsForcingSampleJson => {
            let (flat_len, ts5mindata_ir_len) =
                suews_forcing_schema().map_err(|e| e.to_string())?;
            let state = suews_forcing_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": suews_forcing_schema_version(),
                "schema_version_runtime": suews_forcing_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "ts5mindata_ir_len": ts5mindata_ir_len,
                "state": suews_forcing_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SuewsForcingSampleValuesJson => {
            let state = suews_forcing_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = suews_forcing_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": suews_forcing_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
                "dims": payload.dims,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::HydroStateSchemaJson => {
            let schema = hydro_state_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": hydro_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "base_flat_len": schema.base_flat_len,
                "allocatable_dims": schema.allocatable_dims,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::HydroStateSampleJson => {
            let (flat_len, alloc_lens) = hydro_state_schema().map_err(|e| e.to_string())?;
            let state = hydro_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": hydro_state_schema_version(),
                "schema_version_runtime": hydro_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "alloc_lens": alloc_lens,
                "state": hydro_state_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::HydroStateSampleValuesJson => {
            let state = hydro_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = hydro_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": hydro_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
                "dims": payload.dims,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::HeatStateSchemaJson => {
            let schema = heat_state_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": heat_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "base_flat_len": schema.base_flat_len,
                "nlayer": schema.nlayer,
                "ndepth": schema.ndepth,
                "allocatable_dims": schema.allocatable_dims,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::HeatStateSampleJson => {
            let (flat_len, nlayer, ndepth) = heat_state_schema().map_err(|e| e.to_string())?;
            let state = heat_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": heat_state_schema_version(),
                "schema_version_runtime": heat_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "nlayer": nlayer,
                "ndepth": ndepth,
                "state": heat_state_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::HeatStateSampleValuesJson => {
            let state = heat_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = heat_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": heat_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
                "dims": payload.dims,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SuewsTimerSchemaJson => render_simple_schema_json!(suews_timer),
        FlatCommand::SuewsTimerSampleJson => render_simple_default_json!(suews_timer),
        FlatCommand::SuewsTimerSampleValuesJson => render_simple_default_values_json!(suews_timer),
        FlatCommand::FlagStateSchemaJson => render_simple_schema_json!(flag_state),
        FlatCommand::FlagStateSampleJson => render_simple_default_json!(flag_state),
        FlatCommand::FlagStateSampleValuesJson => render_simple_default_values_json!(flag_state),
        FlatCommand::AnthroemisStateSchemaJson => render_simple_schema_json!(anthroemis_state),
        FlatCommand::AnthroemisStateSampleJson => render_simple_default_json!(anthroemis_state),
        FlatCommand::AnthroemisStateSampleValuesJson => render_simple_default_values_json!(anthroemis_state),
        FlatCommand::AnthroHeatPrmSchemaJson => render_simple_schema_json!(anthro_heat_prm),
        FlatCommand::AnthroHeatPrmSampleJson => render_simple_default_json!(anthro_heat_prm),
        FlatCommand::AnthroHeatPrmSampleValuesJson => render_simple_default_values_json!(anthro_heat_prm),
        FlatCommand::AnthroEmisPrmSchemaJson => render_simple_schema_json!(anthro_emis_prm),
        FlatCommand::AnthroEmisPrmSampleJson => render_simple_default_json!(anthro_emis_prm),
        FlatCommand::AnthroEmisPrmSampleValuesJson => render_simple_default_values_json!(anthro_emis_prm),
        FlatCommand::AtmStateSchemaJson => render_simple_schema_json!(atm_state),
        FlatCommand::AtmStateSampleJson => render_simple_default_json!(atm_state),
        FlatCommand::AtmStateSampleValuesJson => render_simple_default_values_json!(atm_state),
        FlatCommand::BuildingArchetypePrmSchemaJson => {
            let schema = building_archetype_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": building_archetype_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::BuildingArchetypePrmSampleJson => {
            let flat_len = building_archetype_prm_schema().map_err(|e| e.to_string())?;
            let state = building_archetype_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": building_archetype_prm_schema_version(),
                "schema_version_runtime": building_archetype_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": building_archetype_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::BuildingArchetypePrmSampleValuesJson => {
            let state = building_archetype_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = building_archetype_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": building_archetype_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::StebbsPrmSchemaJson => render_simple_schema_json!(stebbs_prm),
        FlatCommand::StebbsPrmSampleJson => render_simple_default_json!(stebbs_prm),
        FlatCommand::StebbsPrmSampleValuesJson => render_simple_default_values_json!(stebbs_prm),
        FlatCommand::OutputLineSchemaJson => {
            let schema = output_line_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": output_line_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::OutputLineSampleJson => {
            let flat_len = output_line_schema().map_err(|e| e.to_string())?;
            let state = output_line_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": output_line_schema_version(),
                "schema_version_runtime": output_line_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": output_line_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::OutputLineSampleValuesJson => {
            let state = output_line_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = output_line_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": output_line_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::OutputBlockSchemaJson => {
            let schema = output_block_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": output_block_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
                "allocatable_dims": schema.allocatable_dims,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::OutputBlockSampleJson => {
            let flat_len = output_block_schema().map_err(|e| e.to_string())?;
            let state = output_block_default_from_fortran().map_err(|e| e.to_string())?;
            let payload_values = output_block_to_values_payload(&state);
            let rows_map = output_block_to_rows_map(&state).map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": output_block_schema_version(),
                "schema_version_runtime": output_block_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "fields": output_block_field_names(),
                "state": rows_map,
                "dims": payload_values.dims,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::OutputBlockSampleValuesJson => {
            let state = output_block_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = output_block_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": output_block_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
                "dims": payload.dims,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::ErrorEntrySchemaJson => {
            let schema = error_entry_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": error_entry_schema_version_runtime().map_err(|e| e.to_string())?,
                "timer_flat_len": schema.timer_flat_len,
                "message_len": schema.message_len,
                "location_len": schema.location_len,
                "fields": schema.field_names,
                "timer_fields": schema.timer_field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::ErrorEntrySampleJson => {
            let (timer_flat_len, message_len, location_len) =
                error_entry_schema().map_err(|e| e.to_string())?;
            let state = error_entry_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": error_entry_schema_version(),
                "schema_version_runtime": error_entry_schema_version_runtime().map_err(|e| e.to_string())?,
                "timer_flat_len": timer_flat_len,
                "message_len": message_len,
                "location_len": location_len,
                "fields": error_entry_field_names(),
                "state": {
                    "timer": suews_timer_to_map(&state.timer),
                    "message": state.message,
                    "location": state.location,
                    "is_fatal": state.is_fatal,
                },
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::ErrorEntrySampleValuesJson => {
            let state = error_entry_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = error_entry_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": error_entry_schema_version_runtime().map_err(|e| e.to_string())?,
                "timer_values": payload.timer_values,
                "message": payload.message,
                "location": payload.location,
                "is_fatal": payload.is_fatal,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::ErrorStateSchemaJson => {
            let schema = error_state_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": error_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "message_len": schema.message_len,
                "fields": schema.field_names,
                "allocatable_dims": schema.allocatable_dims,
                "entry_schema_version": schema.entry_schema.schema_version,
                "entry_timer_flat_len": schema.entry_schema.timer_flat_len,
                "entry_message_len": schema.entry_schema.message_len,
                "entry_location_len": schema.entry_schema.location_len,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::ErrorStateSampleJson => {
            let message_len = error_state_schema().map_err(|e| e.to_string())?;
            let state = error_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload_values = error_state_to_values_payload(&state);
            let log = state
                .log
                .iter()
                .map(|entry| {
                    json!({
                        "timer": suews_timer_to_map(&entry.timer),
                        "message": entry.message,
                        "location": entry.location,
                        "is_fatal": entry.is_fatal,
                    })
                })
                .collect::<Vec<_>>();

            let payload = json!({
                "schema_version": error_state_schema_version(),
                "schema_version_runtime": error_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "message_len": message_len,
                "fields": error_state_field_names(),
                "state": {
                    "flag": state.flag,
                    "code": state.code,
                    "message": state.message,
                    "has_fatal": state.has_fatal,
                    "count": state.count,
                    "log": log,
                },
                "dims": payload_values.dims,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::ErrorStateSampleValuesJson => {
            let state = error_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = error_state_to_values_payload(&state);
            let log = payload
                .log
                .iter()
                .map(|entry| {
                    json!({
                        "schema_version": entry.schema_version,
                        "timer_values": entry.timer_values,
                        "message": entry.message,
                        "location": entry.location,
                        "is_fatal": entry.is_fatal,
                    })
                })
                .collect::<Vec<_>>();

            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": error_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flag": payload.flag,
                "code": payload.code,
                "message": payload.message,
                "has_fatal": payload.has_fatal,
                "count": payload.count,
                "log": log,
                "dims": payload.dims,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::ConductancePrmSchemaJson => render_simple_schema_json!(conductance_prm),
        FlatCommand::ConductancePrmSampleJson => render_simple_default_json!(conductance_prm),
        FlatCommand::ConductancePrmSampleValuesJson => render_simple_default_values_json!(conductance_prm),
        FlatCommand::EhcPrmSchemaJson => {
            let schema = ehc_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": ehc_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "nlayer": schema.nlayer,
                "ndepth": schema.ndepth,
                "allocatable_dims": schema.allocatable_dims,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::EhcPrmSampleJson => {
            let (flat_len, nlayer, ndepth) = ehc_prm_schema().map_err(|e| e.to_string())?;
            let state = ehc_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": ehc_prm_schema_version(),
                "schema_version_runtime": ehc_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "nlayer": nlayer,
                "ndepth": ndepth,
                "state": ehc_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SpartacusPrmSchemaJson => {
            let schema = spartacus_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": spartacus_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "base_flat_len": schema.base_flat_len,
                "height_len": schema.height_len,
                "nlayer": schema.nlayer,
                "allocatable_dims": schema.allocatable_dims,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SpartacusPrmSampleJson => {
            let (flat_len, height_len, nlayer) =
                spartacus_prm_schema().map_err(|e| e.to_string())?;
            let state = spartacus_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": spartacus_prm_schema_version(),
                "schema_version_runtime": spartacus_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "height_len": height_len,
                "nlayer": nlayer,
                "state": spartacus_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SpartacusPrmSampleValuesJson => {
            let state = spartacus_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = spartacus_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": spartacus_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
                "dims": payload.dims,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SpartacusLayerPrmSchemaJson => {
            let schema = spartacus_layer_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": spartacus_layer_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "nlayer": schema.nlayer,
                "nspec": schema.nspec,
                "allocatable_dims": schema.allocatable_dims,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SpartacusLayerPrmSampleJson => {
            let (flat_len, nlayer, nspec) =
                spartacus_layer_prm_schema().map_err(|e| e.to_string())?;
            let state = spartacus_layer_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": spartacus_layer_prm_schema_version(),
                "schema_version_runtime": spartacus_layer_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "nlayer": nlayer,
                "nspec": nspec,
                "state": spartacus_layer_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SpartacusLayerPrmSampleValuesJson => {
            let state = spartacus_layer_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = spartacus_layer_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": spartacus_layer_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
                "dims": payload.dims,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SuewsSiteSchemaJson => {
            let schema = suews_site_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": suews_site_schema_version_runtime().map_err(|e| e.to_string())?,
                "members": suews_site_member_names(),
                "field_count": schema.field_names.len(),
                "fields": schema.field_names,
                "member_field_counts": schema.member_field_counts,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SuewsSiteSampleJson => {
            let state = suews_site_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": suews_site_schema_version(),
                "schema_version_runtime": suews_site_schema_version_runtime().map_err(|e| e.to_string())?,
                "fields": suews_site_field_names().map_err(|e| e.to_string())?,
                "state": suews_site_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SuewsSiteSampleValuesJson => {
            let state = suews_site_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = suews_site_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": suews_site_schema_version_runtime().map_err(|e| e.to_string())?,
                "members": payload.members,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::SuewsSiteSampleNestedJson => {
            let state = suews_site_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = suews_site_to_nested_payload(&state);
            let out = json!({
                "schema_version": suews_site_schema_version(),
                "schema_version_runtime": suews_site_schema_version_runtime().map_err(|e| e.to_string())?,
                "nested_payload": payload,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default nested json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::EhcPrmSampleValuesJson => {
            let state = ehc_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = ehc_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": ehc_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
                "dims": payload.dims,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::Bioco2PrmSchemaJson => render_simple_schema_json!(bioco2_prm),
        FlatCommand::Bioco2PrmSampleJson => render_simple_default_json!(bioco2_prm),
        FlatCommand::Bioco2PrmSampleValuesJson => render_simple_default_values_json!(bioco2_prm),
        FlatCommand::LaiPrmSchemaJson => render_simple_schema_json!(lai_prm),
        FlatCommand::LaiPrmSampleJson => render_simple_default_json!(lai_prm),
        FlatCommand::LaiPrmSampleValuesJson => render_simple_default_values_json!(lai_prm),
        FlatCommand::PhenologyStateSchemaJson => render_simple_schema_json!(phenology_state),
        FlatCommand::PhenologyStateSampleJson => render_simple_default_json!(phenology_state),
        FlatCommand::PhenologyStateSampleValuesJson => render_simple_default_values_json!(phenology_state),
        FlatCommand::SnowStateSchemaJson => render_simple_schema_json!(snow_state),
        FlatCommand::SnowStateSampleJson => render_simple_default_json!(snow_state),
        FlatCommand::SnowStateSampleValuesJson => render_simple_default_values_json!(snow_state),
        FlatCommand::SnowPrmSchemaJson => render_simple_schema_json!(snow_prm),
        FlatCommand::SnowPrmSampleJson => render_simple_default_json!(snow_prm),
        FlatCommand::SnowPrmSampleValuesJson => render_simple_default_values_json!(snow_prm),
        FlatCommand::SoilPrmSchemaJson => render_simple_schema_json!(soil_prm),
        FlatCommand::SoilPrmSampleJson => render_simple_default_json!(soil_prm),
        FlatCommand::SoilPrmSampleValuesJson => render_simple_default_values_json!(soil_prm),
        FlatCommand::LcPavedPrmSchemaJson => render_simple_schema_json!(lc_paved_prm),
        FlatCommand::LcPavedPrmSampleJson => render_simple_default_json!(lc_paved_prm),
        FlatCommand::LcPavedPrmSampleValuesJson => render_simple_default_values_json!(lc_paved_prm),
        FlatCommand::LcBldgPrmSchemaJson => render_simple_schema_json!(lc_bldg_prm),
        FlatCommand::LcBldgPrmSampleJson => render_simple_default_json!(lc_bldg_prm),
        FlatCommand::LcBldgPrmSampleValuesJson => render_simple_default_values_json!(lc_bldg_prm),
        FlatCommand::LcBsoilPrmSchemaJson => render_simple_schema_json!(lc_bsoil_prm),
        FlatCommand::LcBsoilPrmSampleJson => render_simple_default_json!(lc_bsoil_prm),
        FlatCommand::LcBsoilPrmSampleValuesJson => render_simple_default_values_json!(lc_bsoil_prm),
        FlatCommand::LcWaterPrmSchemaJson => render_simple_schema_json!(lc_water_prm),
        FlatCommand::LcWaterPrmSampleJson => render_simple_default_json!(lc_water_prm),
        FlatCommand::LcWaterPrmSampleValuesJson => render_simple_default_values_json!(lc_water_prm),
        FlatCommand::LcDectrPrmSchemaJson => render_simple_schema_json!(lc_dectr_prm),
        FlatCommand::LcDectrPrmSampleJson => render_simple_default_json!(lc_dectr_prm),
        FlatCommand::LcDectrPrmSampleValuesJson => render_simple_default_values_json!(lc_dectr_prm),
        FlatCommand::LcEvetrPrmSchemaJson => render_simple_schema_json!(lc_evetr_prm),
        FlatCommand::LcEvetrPrmSampleJson => render_simple_default_json!(lc_evetr_prm),
        FlatCommand::LcEvetrPrmSampleValuesJson => render_simple_default_values_json!(lc_evetr_prm),
        FlatCommand::LcGrassPrmSchemaJson => render_simple_schema_json!(lc_grass_prm),
        FlatCommand::LcGrassPrmSampleJson => render_simple_default_json!(lc_grass_prm),
        FlatCommand::LcGrassPrmSampleValuesJson => render_simple_default_values_json!(lc_grass_prm),
        FlatCommand::SurfStorePrmSchemaJson => render_simple_schema_json!(surf_store_prm),
        FlatCommand::SurfStorePrmSampleJson => render_simple_default_json!(surf_store_prm),
        FlatCommand::SurfStorePrmSampleValuesJson => render_simple_default_values_json!(surf_store_prm),
        FlatCommand::WaterDistPrmSchemaJson => render_simple_schema_json!(water_dist_prm),
        FlatCommand::WaterDistPrmSampleJson => render_simple_default_json!(water_dist_prm),
        FlatCommand::WaterDistPrmSampleValuesJson => render_simple_default_values_json!(water_dist_prm),
        FlatCommand::IrrigDaywaterSchemaJson => render_simple_schema_json!(irrig_daywater),
        FlatCommand::IrrigDaywaterSampleJson => render_simple_default_json!(irrig_daywater),
        FlatCommand::IrrigDaywaterSampleValuesJson => render_simple_default_values_json!(irrig_daywater),
        FlatCommand::IrrigationPrmSchemaJson => render_simple_schema_json!(irrigation_prm),
        FlatCommand::IrrigationPrmSampleJson => render_simple_default_json!(irrigation_prm),
        FlatCommand::IrrigationPrmSampleValuesJson => render_simple_default_values_json!(irrigation_prm),
        FlatCommand::LumpsPrmSchemaJson => render_simple_schema_json!(lumps_prm),
        FlatCommand::LumpsPrmSampleJson => render_simple_default_json!(lumps_prm),
        FlatCommand::LumpsPrmSampleValuesJson => render_simple_default_values_json!(lumps_prm),
        FlatCommand::OhmCoefLcSchemaJson => render_simple_schema_json!(ohm_coef_lc),
        FlatCommand::OhmCoefLcSampleJson => render_simple_default_json!(ohm_coef_lc),
        FlatCommand::OhmCoefLcSampleValuesJson => render_simple_default_values_json!(ohm_coef_lc),
        FlatCommand::OhmPrmSchemaJson => render_simple_schema_json!(ohm_prm),
        FlatCommand::OhmPrmSampleJson => render_simple_default_json!(ohm_prm),
        FlatCommand::OhmPrmSampleValuesJson => render_simple_default_values_json!(ohm_prm),
        FlatCommand::SolarStateSchemaJson => render_simple_schema_json!(solar_state),
        FlatCommand::SolarStateSampleJson => render_simple_default_json!(solar_state),
        FlatCommand::SolarStateSampleValuesJson => render_simple_default_values_json!(solar_state),
        FlatCommand::RoughnessStateSchemaJson => render_simple_schema_json!(roughness_state),
        FlatCommand::RoughnessStateSampleJson => render_simple_default_json!(roughness_state),
        FlatCommand::RoughnessStateSampleValuesJson => render_simple_default_values_json!(roughness_state),
        FlatCommand::NhoodStateSchemaJson => render_simple_schema_json!(nhood_state),
        FlatCommand::NhoodStateSampleJson => render_simple_default_json!(nhood_state),
        FlatCommand::NhoodStateSampleValuesJson => render_simple_default_values_json!(nhood_state),
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use paste::paste;

    /// Helper macro for tests that go through the two-level hierarchy:
    /// SchemaType::$group { command: $group_enum::$variant { action: $action_enum::$action } }
    macro_rules! test_schema {
        ($name:ident, $group:ident, $group_enum:ident, $variant:ident, $action_enum:ident, $action:ident) => {
            #[test]
            fn $name() {
                let cli = Cli {
                    command: Commands::Schema {
                        command: SchemaType::$group {
                            command: $group_enum::$variant {
                                action: $action_enum::$action,
                            },
                        },
                    },
                };
                run(cli).expect(concat!(stringify!($name), " should succeed"));
            }
        };
    }

    macro_rules! test_simple_schema_all {
        ($prefix:ident, $group:ident, $group_enum:ident, $variant:ident) => {
            paste! {
                test_schema!(
                    [<run_ $prefix _schema_json_succeeds>],
                    $group,
                    $group_enum,
                    $variant,
                    StandardSchemaAction,
                    SchemaJson
                );
                test_schema!(
                    [<run_ $prefix _sample_json_succeeds>],
                    $group,
                    $group_enum,
                    $variant,
                    StandardSchemaAction,
                    SampleJson
                );
                test_schema!(
                    [<run_ $prefix _sample_values_json_succeeds>],
                    $group,
                    $group_enum,
                    $variant,
                    StandardSchemaAction,
                    SampleValuesJson
                );
            }
        };
    }

    // --- OhmState (custom action enum) ---
    test_schema!(
        run_ohm_state_schema_json_succeeds,
        State,
        SchemaStateType,
        Ohm,
        OhmStateAction,
        SchemaJson
    );
    test_schema!(
        run_ohm_state_sample_json_succeeds,
        State,
        SchemaStateType,
        Ohm,
        OhmStateAction,
        SampleJson
    );
    test_schema!(
        run_ohm_state_sample_values_json_succeeds,
        State,
        SchemaStateType,
        Ohm,
        OhmStateAction,
        SampleValuesJson
    );

    #[test]
    fn run_ohm_state_flat_schema_succeeds() {
        let cli = Cli {
            command: Commands::Schema {
                command: SchemaType::State {
                    command: SchemaStateType::Ohm {
                        action: OhmStateAction::FlatSchema,
                    },
                },
            },
        };
        run(cli).expect("ohm-state flat-schema should succeed");
    }

    // --- SuewsSite (custom action enum) ---
    test_schema!(
        run_suews_site_schema_json_succeeds,
        Config,
        SchemaConfigType,
        SuewsSite,
        SuewsSiteSchemaAction,
        SchemaJson
    );
    test_schema!(
        run_suews_site_sample_json_succeeds,
        Config,
        SchemaConfigType,
        SuewsSite,
        SuewsSiteSchemaAction,
        SampleJson
    );
    test_schema!(
        run_suews_site_sample_values_json_succeeds,
        Config,
        SchemaConfigType,
        SuewsSite,
        SuewsSiteSchemaAction,
        SampleValuesJson
    );
    test_schema!(
        run_suews_site_sample_nested_json_succeeds,
        Config,
        SchemaConfigType,
        SuewsSite,
        SuewsSiteSchemaAction,
        SampleNestedJson
    );

    // --- Config group ---
    test_simple_schema_all!(suews_config, Config, SchemaConfigType, SuewsConfig);
    test_simple_schema_all!(suews_forcing, Config, SchemaConfigType, SuewsForcing);
    test_simple_schema_all!(suews_timer, Config, SchemaConfigType, SuewsTimer);

    // --- State group ---
    test_simple_schema_all!(hydro_state, State, SchemaStateType, Hydro);
    test_simple_schema_all!(heat_state, State, SchemaStateType, Heat);
    test_simple_schema_all!(flag_state, State, SchemaStateType, Flag);
    test_simple_schema_all!(anthroemis_state, State, SchemaStateType, Anthroemis);
    test_simple_schema_all!(atm_state, State, SchemaStateType, Atm);
    test_simple_schema_all!(phenology_state, State, SchemaStateType, Phenology);
    test_simple_schema_all!(snow_state, State, SchemaStateType, Snow);
    test_simple_schema_all!(solar_state, State, SchemaStateType, Solar);
    test_simple_schema_all!(roughness_state, State, SchemaStateType, Roughness);
    test_simple_schema_all!(nhood_state, State, SchemaStateType, Nhood);

    // --- Param group ---
    test_simple_schema_all!(anthro_heat_prm, Param, SchemaParamType, AnthroHeat);
    test_simple_schema_all!(anthro_emis_prm, Param, SchemaParamType, AnthroEmis);
    test_simple_schema_all!(building_archetype_prm, Param, SchemaParamType, BuildingArchetype);
    test_simple_schema_all!(stebbs_prm, Param, SchemaParamType, Stebbs);
    test_simple_schema_all!(conductance_prm, Param, SchemaParamType, Conductance);
    test_simple_schema_all!(ehc_prm, Param, SchemaParamType, Ehc);
    test_simple_schema_all!(spartacus_prm, Param, SchemaParamType, Spartacus);
    test_simple_schema_all!(spartacus_layer_prm, Param, SchemaParamType, SpartacusLayer);
    test_simple_schema_all!(bioco2_prm, Param, SchemaParamType, Bioco2);
    test_simple_schema_all!(lai_prm, Param, SchemaParamType, Lai);
    test_simple_schema_all!(snow_prm, Param, SchemaParamType, Snow);
    test_simple_schema_all!(soil_prm, Param, SchemaParamType, Soil);
    test_simple_schema_all!(lumps_prm, Param, SchemaParamType, Lumps);
    test_simple_schema_all!(ohm_coef_lc, Param, SchemaParamType, OhmCoefLc);
    test_simple_schema_all!(ohm_prm, Param, SchemaParamType, Ohm);
    test_simple_schema_all!(lc_paved_prm, Param, SchemaParamType, LcPaved);
    test_simple_schema_all!(lc_bldg_prm, Param, SchemaParamType, LcBldg);
    test_simple_schema_all!(lc_bsoil_prm, Param, SchemaParamType, LcBsoil);
    test_simple_schema_all!(lc_water_prm, Param, SchemaParamType, LcWater);
    test_simple_schema_all!(lc_dectr_prm, Param, SchemaParamType, LcDectr);
    test_simple_schema_all!(lc_evetr_prm, Param, SchemaParamType, LcEvetr);
    test_simple_schema_all!(lc_grass_prm, Param, SchemaParamType, LcGrass);
    test_simple_schema_all!(surf_store_prm, Param, SchemaParamType, SurfStore);
    test_simple_schema_all!(water_dist_prm, Param, SchemaParamType, WaterDist);
    test_simple_schema_all!(irrig_daywater, Param, SchemaParamType, IrrigDaywater);
    test_simple_schema_all!(irrigation_prm, Param, SchemaParamType, Irrigation);

    // --- Output group ---
    test_simple_schema_all!(output_line, Output, SchemaOutputType, Line);
    test_simple_schema_all!(output_block, Output, SchemaOutputType, Block);
    test_simple_schema_all!(error_entry, Output, SchemaOutputType, ErrorEntry);
    test_simple_schema_all!(error_state, Output, SchemaOutputType, ErrorState);
}
