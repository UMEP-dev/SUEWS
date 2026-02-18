use clap::{Parser, Subcommand};
use paste::paste;
use serde_json::json;
use serde_json::Value;
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
    ohm_state_field_names, ohm_state_from_map, ohm_state_from_values_payload, ohm_state_schema,
    ohm_state_schema_info, ohm_state_schema_version, ohm_state_schema_version_runtime,
    ohm_state_step, ohm_state_to_map, ohm_state_to_values_payload, ohm_step,
    output_block_default_from_fortran, output_block_field_names, output_block_schema,
    output_block_schema_info, output_block_schema_version, output_block_schema_version_runtime,
    output_block_to_rows_map, output_block_to_values_payload, output_line_default_from_fortran,
    output_line_schema, output_line_schema_info, output_line_schema_version,
    output_line_schema_version_runtime, output_line_to_map, output_line_to_values_payload,
    phenology_state_default_from_fortran, phenology_state_schema, phenology_state_schema_info,
    phenology_state_schema_version, phenology_state_schema_version_runtime, phenology_state_to_map,
    phenology_state_to_values_payload, qs_calc, roughness_state_default_from_fortran,
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
    water_dist_prm_to_values_payload, OhmModel, OhmStateValuesPayload, OHM_STATE_FLAT_LEN,
};
#[cfg(feature = "physics")]
use suews_bridge::{
    interpolate_forcing, load_run_config, read_forcing_block, run_from_config_str_and_forcing,
    write_output_csv,
};

fn parse_state_map_json(text: &str) -> Result<std::collections::BTreeMap<String, f64>, String> {
    fn parse_field_object(value: Value) -> Result<std::collections::BTreeMap<String, f64>, String> {
        let obj = value
            .as_object()
            .ok_or_else(|| "state payload must be a JSON object".to_string())?;

        let mut out = std::collections::BTreeMap::new();
        for (key, val) in obj {
            let num = val
                .as_f64()
                .ok_or_else(|| format!("state field `{key}` must be numeric"))?;
            out.insert(key.clone(), num);
        }
        Ok(out)
    }

    let mut root: serde_json::Map<String, Value> = serde_json::from_str(text)
        .map_err(|e| format!("failed to parse JSON state payload: {e}"))?;

    if let Some(state_value) = root.remove("state") {
        if let Some(version_value) = root.remove("schema_version") {
            let version = version_value.as_u64().ok_or_else(|| {
                "`schema_version` must be an unsigned integer when provided".to_string()
            })?;
            if version as u32 != ohm_state_schema_version() {
                return Err(format!(
                    "schema_version mismatch: got {version}, expected {}",
                    ohm_state_schema_version()
                ));
            }
        }

        parse_field_object(state_value)
    } else {
        parse_field_object(Value::Object(root))
    }
}

fn parse_state_values_json(text: &str) -> Result<OhmStateValuesPayload, String> {
    fn validate_values_len(values: &[f64]) -> Result<(), String> {
        if values.len() != OHM_STATE_FLAT_LEN {
            return Err(format!(
                "values length mismatch: got {}, expected {}",
                values.len(),
                OHM_STATE_FLAT_LEN
            ));
        }
        Ok(())
    }

    fn parse_values_array(value: Value) -> Result<Vec<f64>, String> {
        let items = value
            .as_array()
            .ok_or_else(|| "values payload must be a JSON array".to_string())?;

        let mut out = Vec::with_capacity(items.len());
        for (idx, item) in items.iter().enumerate() {
            let num = item
                .as_f64()
                .ok_or_else(|| format!("values[{idx}] must be numeric"))?;
            out.push(num);
        }
        Ok(out)
    }

    let value: Value = serde_json::from_str(text)
        .map_err(|e| format!("failed to parse JSON values payload: {e}"))?;

    match value {
        Value::Array(_) => {
            let values = parse_values_array(value)?;
            validate_values_len(&values)?;
            Ok(OhmStateValuesPayload {
                schema_version: ohm_state_schema_version(),
                values,
            })
        }
        Value::Object(mut obj) => {
            let values_value = obj
                .remove("values")
                .ok_or_else(|| "values payload object must contain `values`".to_string())?;
            let values = parse_values_array(values_value)?;
            validate_values_len(&values)?;

            let schema_version = if let Some(v) = obj.remove("schema_version") {
                v.as_u64().ok_or_else(|| {
                    "`schema_version` must be an unsigned integer when provided".to_string()
                })? as u32
            } else {
                ohm_state_schema_version()
            };

            if schema_version != ohm_state_schema_version() {
                return Err(format!(
                    "schema_version mismatch: got {schema_version}, expected {}",
                    ohm_state_schema_version()
                ));
            }

            Ok(OhmStateValuesPayload {
                schema_version,
                values,
            })
        }
        _ => Err("values payload must be a JSON array or object".to_string()),
    }
}

#[derive(Debug, Parser)]
#[command(name = "suews", version, about = "SUEWS Rust bridge CLI")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    #[cfg(feature = "physics")]
    /// Run SUEWS batch simulation from YAML config.
    Run {
        /// Path to config YAML.
        config: String,
        /// Output directory for simulation results.
        #[arg(long, default_value = "./output")]
        output_dir: String,
        /// Output format: csv or arrow (requires arrow-output feature).
        #[arg(long, default_value = "csv")]
        format: String,
    },
    /// Inspect bridge type schemas and defaults.
    Schema {
        #[command(subcommand)]
        command: SchemaType,
    },
}

#[derive(Debug, Subcommand, Clone)]
enum StandardSchemaAction {
    /// Print JSON schema.
    SchemaJson,
    /// Print default as JSON map payload.
    DefaultJson,
    /// Print default as JSON ordered values payload.
    DefaultValuesJson,
}

#[derive(Debug, Subcommand, Clone)]
enum SuewsSiteSchemaAction {
    /// Print JSON schema.
    SchemaJson,
    /// Print default as JSON map payload.
    DefaultJson,
    /// Print default as JSON ordered values payload.
    DefaultValuesJson,
    /// Print default as nested composite payload.
    DefaultNestedJson,
}

#[derive(Debug, Subcommand, Clone)]
enum OhmStateAction {
    /// Print JSON schema.
    SchemaJson,
    /// Print default as JSON map payload.
    DefaultJson,
    /// Print default as JSON ordered values payload.
    DefaultValuesJson,
    /// Print flat schema with index and field name.
    FlatSchema,
    /// Step using JSON I/O.
    StepJson {
        #[arg(long)]
        dt: i32,
        #[arg(long)]
        dt_since_start: i32,
        #[arg(long)]
        qn1: f64,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
        /// Optional JSON file with field-value object for initial state.
        #[arg(long)]
        state_json: Option<String>,
    },
    /// Step using ordered values JSON I/O.
    StepValuesJson {
        #[arg(long)]
        dt: i32,
        #[arg(long)]
        dt_since_start: i32,
        #[arg(long)]
        qn1: f64,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
        /// Optional JSON file with values array or {schema_version, values}.
        #[arg(long)]
        state_values_json: Option<String>,
    },
    /// Calculate QS for one step.
    Qs {
        #[arg(long)]
        qn1: f64,
        #[arg(long)]
        dqndt: f64,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
    },
    /// Compute dqndt and QS for one OHM timestep.
    Step {
        #[arg(long)]
        dt: i32,
        #[arg(long)]
        dt_since_start: i32,
        #[arg(long)]
        qn1_av_prev: f64,
        #[arg(long)]
        dqndt_prev: f64,
        #[arg(long)]
        qn1: f64,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
    },
    /// Run a sequence of qn1 values through a stateful model.
    Series {
        #[arg(long)]
        dt: i32,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
        #[arg(long, value_delimiter = ',')]
        qn: Vec<f64>,
    },
    /// Step the bridge payload once (raw output).
    StateStep {
        #[arg(long)]
        dt: i32,
        #[arg(long)]
        dt_since_start: i32,
        #[arg(long)]
        qn1: f64,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
    },
}

#[derive(Debug, Subcommand, Clone)]
enum SchemaType {
    /// OHM_STATE (schema, defaults, and OHM physics).
    OhmState {
        #[command(subcommand)]
        action: OhmStateAction,
    },
    /// SUEWS_CONFIG.
    SuewsConfig {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SUEWS_FORCING.
    SuewsForcing {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// HYDRO_STATE.
    HydroState {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// HEAT_STATE.
    HeatState {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SUEWS_TIMER.
    SuewsTimer {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// FLAG_STATE.
    FlagState {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// anthroEmis_STATE.
    AnthroemisState {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// ATM_STATE.
    AtmState {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// PHENOLOGY_STATE.
    PhenologyState {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SNOW_STATE.
    SnowState {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SOLAR_STATE.
    SolarState {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// ROUGHNESS_STATE.
    RoughnessState {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// NHOOD_STATE.
    NhoodState {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SUEWS_SITE.
    SuewsSite {
        #[command(subcommand)]
        action: SuewsSiteSchemaAction,
    },
    /// anthroHEAT_PRM.
    AnthroHeatPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// anthroEMIS_PRM.
    AnthroEmisPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// BUILDING_ARCHETYPE_PRM.
    BuildingArchetypePrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// STEBBS_PRM.
    StebbsPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// CONDUCTANCE_PRM.
    ConductancePrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// EHC_PRM.
    EhcPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SPARTACUS_PRM.
    SpartacusPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SPARTACUS_LAYER_PRM.
    SpartacusLayerPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// bioCO2_PRM.
    Bioco2Prm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LAI_PRM.
    LaiPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SNOW_PRM.
    SnowPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SOIL_PRM.
    SoilPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LUMPS_PRM.
    LumpsPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// OHM_COEF_LC.
    OhmCoefLc {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// OHM_PRM.
    OhmPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_PAVED_PRM.
    LcPavedPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_BLDG_PRM.
    LcBldgPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_BSOIL_PRM.
    LcBsoilPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_WATER_PRM.
    LcWaterPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_DECTR_PRM.
    LcDectrPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_EVETR_PRM.
    LcEvetrPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// LC_GRASS_PRM.
    LcGrassPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// SURF_STORE_PRM.
    SurfStorePrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// WATER_DIST_PRM.
    WaterDistPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// IRRIG_daywater.
    IrrigDaywater {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// IRRIGATION_PRM.
    IrrigationPrm {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// output_line.
    OutputLine {
        #[command(subcommand)]
        action: StandardSchemaAction,
    },
    /// output_block.
    OutputBlock {
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
    /// Calculate QS for one step from qn1, dqndt and OHM coefficients.
    Qs {
        #[arg(long)]
        qn1: f64,
        #[arg(long)]
        dqndt: f64,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
    },
    /// Compute dqndt and QS for one OHM timestep.
    Step {
        #[arg(long)]
        dt: i32,
        #[arg(long)]
        dt_since_start: i32,
        #[arg(long)]
        qn1_av_prev: f64,
        #[arg(long)]
        dqndt_prev: f64,
        #[arg(long)]
        qn1: f64,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
    },
    /// Run a short sequence of qn1 values through a stateful model.
    Series {
        #[arg(long)]
        dt: i32,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
        #[arg(long, value_delimiter = ',')]
        qn: Vec<f64>,
    },
    #[cfg(feature = "physics")]
    /// Run SUEWS batch simulation from YAML config.
    Run {
        /// Path to config YAML.
        config: String,
        /// Output directory for simulation results.
        #[arg(long, default_value = "./output")]
        output_dir: String,
        /// Output format: csv or arrow (requires arrow-output feature).
        #[arg(long, default_value = "csv")]
        format: String,
    },
    /// Step the Fortran OHM_STATE bridge payload once.
    StateStep {
        #[arg(long)]
        dt: i32,
        #[arg(long)]
        dt_since_start: i32,
        #[arg(long)]
        qn1: f64,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
    },
    /// Print OHM_STATE flat schema with index and field name.
    StateSchema,
    /// Print OHM_STATE schema as JSON for programmatic tooling.
    StateSchemaJson,
    /// Print default OHM_STATE as JSON map payload.
    StateDefaultJson,
    /// Print default OHM_STATE as JSON ordered values payload.
    StateDefaultValuesJson,
    /// Print SUEWS_CONFIG schema as JSON for programmatic tooling.
    SuewsConfigSchemaJson,
    /// Print default SUEWS_CONFIG as JSON map payload.
    SuewsConfigDefaultJson,
    /// Print default SUEWS_CONFIG as JSON ordered values payload.
    SuewsConfigDefaultValuesJson,
    /// Print SUEWS_FORCING schema as JSON for programmatic tooling.
    SuewsForcingSchemaJson,
    /// Print default SUEWS_FORCING as JSON map payload.
    SuewsForcingDefaultJson,
    /// Print default SUEWS_FORCING as JSON ordered values payload.
    SuewsForcingDefaultValuesJson,
    /// Print HYDRO_STATE schema as JSON for programmatic tooling.
    HydroStateSchemaJson,
    /// Print default HYDRO_STATE as JSON map payload.
    HydroStateDefaultJson,
    /// Print default HYDRO_STATE as JSON ordered values payload.
    HydroStateDefaultValuesJson,
    /// Print HEAT_STATE schema as JSON for programmatic tooling.
    HeatStateSchemaJson,
    /// Print default HEAT_STATE as JSON map payload.
    HeatStateDefaultJson,
    /// Print default HEAT_STATE as JSON ordered values payload.
    HeatStateDefaultValuesJson,
    /// Print SUEWS_TIMER schema as JSON for programmatic tooling.
    SuewsTimerSchemaJson,
    /// Print default SUEWS_TIMER as JSON map payload.
    SuewsTimerDefaultJson,
    /// Print default SUEWS_TIMER as JSON ordered values payload.
    SuewsTimerDefaultValuesJson,
    /// Step OHM_STATE using JSON input/output.
    StateStepJson {
        #[arg(long)]
        dt: i32,
        #[arg(long)]
        dt_since_start: i32,
        #[arg(long)]
        qn1: f64,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
        /// Optional JSON file with field-value object for initial state.
        #[arg(long)]
        state_json: Option<String>,
    },
    /// Step OHM_STATE using ordered values JSON input/output.
    StateStepValuesJson {
        #[arg(long)]
        dt: i32,
        #[arg(long)]
        dt_since_start: i32,
        #[arg(long)]
        qn1: f64,
        #[arg(long)]
        a1: f64,
        #[arg(long)]
        a2: f64,
        #[arg(long)]
        a3: f64,
        /// Optional JSON file with values array or {schema_version, values}.
        #[arg(long)]
        state_values_json: Option<String>,
    },
    /// Print flag_STATE schema as JSON for programmatic tooling.
    FlagStateSchemaJson,
    /// Print default flag_STATE as JSON map payload.
    FlagStateDefaultJson,
    /// Print default flag_STATE as JSON ordered values payload.
    FlagStateDefaultValuesJson,
    /// Print anthroEmis_STATE schema as JSON for programmatic tooling.
    AnthroemisStateSchemaJson,
    /// Print default anthroEmis_STATE as JSON map payload.
    AnthroemisStateDefaultJson,
    /// Print default anthroEmis_STATE as JSON ordered values payload.
    AnthroemisStateDefaultValuesJson,
    /// Print anthroHEAT_PRM schema as JSON for programmatic tooling.
    AnthroHeatPrmSchemaJson,
    /// Print default anthroHEAT_PRM as JSON map payload.
    AnthroHeatPrmDefaultJson,
    /// Print default anthroHEAT_PRM as JSON ordered values payload.
    AnthroHeatPrmDefaultValuesJson,
    /// Print anthroEMIS_PRM schema as JSON for programmatic tooling.
    AnthroEmisPrmSchemaJson,
    /// Print default anthroEMIS_PRM as JSON map payload.
    AnthroEmisPrmDefaultJson,
    /// Print default anthroEMIS_PRM as JSON ordered values payload.
    AnthroEmisPrmDefaultValuesJson,
    /// Print atm_state schema as JSON for programmatic tooling.
    AtmStateSchemaJson,
    /// Print default atm_state as JSON map payload.
    AtmStateDefaultJson,
    /// Print default atm_state as JSON ordered values payload.
    AtmStateDefaultValuesJson,
    /// Print BUILDING_ARCHETYPE_PRM schema as JSON for programmatic tooling.
    BuildingArchetypePrmSchemaJson,
    /// Print default BUILDING_ARCHETYPE_PRM as JSON map payload.
    BuildingArchetypePrmDefaultJson,
    /// Print default BUILDING_ARCHETYPE_PRM as JSON ordered values payload.
    BuildingArchetypePrmDefaultValuesJson,
    /// Print STEBBS_PRM schema as JSON for programmatic tooling.
    StebbsPrmSchemaJson,
    /// Print default STEBBS_PRM as JSON map payload.
    StebbsPrmDefaultJson,
    /// Print default STEBBS_PRM as JSON ordered values payload.
    StebbsPrmDefaultValuesJson,
    /// Print output_line schema as JSON for programmatic tooling.
    OutputLineSchemaJson,
    /// Print default output_line as JSON map payload.
    OutputLineDefaultJson,
    /// Print default output_line as JSON ordered values payload.
    OutputLineDefaultValuesJson,
    /// Print output_block schema as JSON for programmatic tooling.
    OutputBlockSchemaJson,
    /// Print default output_block as JSON state payload.
    OutputBlockDefaultJson,
    /// Print default output_block as JSON ordered values payload with dims.
    OutputBlockDefaultValuesJson,
    /// Print error_entry schema as JSON for programmatic tooling.
    ErrorEntrySchemaJson,
    /// Print default error_entry as JSON state payload.
    ErrorEntryDefaultJson,
    /// Print default error_entry as JSON structured values payload.
    ErrorEntryDefaultValuesJson,
    /// Print error_state schema as JSON for programmatic tooling.
    ErrorStateSchemaJson,
    /// Print default error_state as JSON state payload.
    ErrorStateDefaultJson,
    /// Print default error_state as JSON structured values payload.
    ErrorStateDefaultValuesJson,
    /// Print CONDUCTANCE_PRM schema as JSON for programmatic tooling.
    ConductancePrmSchemaJson,
    /// Print default CONDUCTANCE_PRM as JSON map payload.
    ConductancePrmDefaultJson,
    /// Print default CONDUCTANCE_PRM as JSON ordered values payload.
    ConductancePrmDefaultValuesJson,
    /// Print EHC_PRM schema as JSON for programmatic tooling.
    EhcPrmSchemaJson,
    /// Print default EHC_PRM as JSON map payload.
    EhcPrmDefaultJson,
    /// Print SPARTACUS_PRM schema as JSON for programmatic tooling.
    SpartacusPrmSchemaJson,
    /// Print default SPARTACUS_PRM as JSON map payload.
    SpartacusPrmDefaultJson,
    /// Print default SPARTACUS_PRM as JSON ordered values payload.
    SpartacusPrmDefaultValuesJson,
    /// Print SPARTACUS_LAYER_PRM schema as JSON for programmatic tooling.
    SpartacusLayerPrmSchemaJson,
    /// Print default SPARTACUS_LAYER_PRM as JSON map payload.
    SpartacusLayerPrmDefaultJson,
    /// Print default SPARTACUS_LAYER_PRM as JSON ordered values payload.
    SpartacusLayerPrmDefaultValuesJson,
    /// Print SUEWS_SITE schema as JSON for programmatic tooling.
    SuewsSiteSchemaJson,
    /// Print default SUEWS_SITE as flattened member.field JSON map payload.
    SuewsSiteDefaultJson,
    /// Print default SUEWS_SITE as structured JSON values payload.
    SuewsSiteDefaultValuesJson,
    /// Print default SUEWS_SITE as nested composite payload JSON.
    SuewsSiteDefaultNestedJson,
    /// Print default EHC_PRM as JSON ordered values payload.
    EhcPrmDefaultValuesJson,
    /// Print bioCO2_PRM schema as JSON for programmatic tooling.
    Bioco2PrmSchemaJson,
    /// Print default bioCO2_PRM as JSON map payload.
    Bioco2PrmDefaultJson,
    /// Print default bioCO2_PRM as JSON ordered values payload.
    Bioco2PrmDefaultValuesJson,
    /// Print LAI_PRM schema as JSON for programmatic tooling.
    LaiPrmSchemaJson,
    /// Print default LAI_PRM as JSON map payload.
    LaiPrmDefaultJson,
    /// Print default LAI_PRM as JSON ordered values payload.
    LaiPrmDefaultValuesJson,
    /// Print PHENOLOGY_STATE schema as JSON for programmatic tooling.
    PhenologyStateSchemaJson,
    /// Print default PHENOLOGY_STATE as JSON map payload.
    PhenologyStateDefaultJson,
    /// Print default PHENOLOGY_STATE as JSON ordered values payload.
    PhenologyStateDefaultValuesJson,
    /// Print SNOW_STATE schema as JSON for programmatic tooling.
    SnowStateSchemaJson,
    /// Print default SNOW_STATE as JSON map payload.
    SnowStateDefaultJson,
    /// Print default SNOW_STATE as JSON ordered values payload.
    SnowStateDefaultValuesJson,
    /// Print SNOW_PRM schema as JSON for programmatic tooling.
    SnowPrmSchemaJson,
    /// Print default SNOW_PRM as JSON map payload.
    SnowPrmDefaultJson,
    /// Print default SNOW_PRM as JSON ordered values payload.
    SnowPrmDefaultValuesJson,
    /// Print SOIL_PRM schema as JSON for programmatic tooling.
    SoilPrmSchemaJson,
    /// Print default SOIL_PRM as JSON map payload.
    SoilPrmDefaultJson,
    /// Print default SOIL_PRM as JSON ordered values payload.
    SoilPrmDefaultValuesJson,
    /// Print LC_PAVED_PRM schema as JSON for programmatic tooling.
    LcPavedPrmSchemaJson,
    /// Print default LC_PAVED_PRM as JSON map payload.
    LcPavedPrmDefaultJson,
    /// Print default LC_PAVED_PRM as JSON ordered values payload.
    LcPavedPrmDefaultValuesJson,
    /// Print LC_BLDG_PRM schema as JSON for programmatic tooling.
    LcBldgPrmSchemaJson,
    /// Print default LC_BLDG_PRM as JSON map payload.
    LcBldgPrmDefaultJson,
    /// Print default LC_BLDG_PRM as JSON ordered values payload.
    LcBldgPrmDefaultValuesJson,
    /// Print LC_BSOIL_PRM schema as JSON for programmatic tooling.
    LcBsoilPrmSchemaJson,
    /// Print default LC_BSOIL_PRM as JSON map payload.
    LcBsoilPrmDefaultJson,
    /// Print default LC_BSOIL_PRM as JSON ordered values payload.
    LcBsoilPrmDefaultValuesJson,
    /// Print LC_WATER_PRM schema as JSON for programmatic tooling.
    LcWaterPrmSchemaJson,
    /// Print default LC_WATER_PRM as JSON map payload.
    LcWaterPrmDefaultJson,
    /// Print default LC_WATER_PRM as JSON ordered values payload.
    LcWaterPrmDefaultValuesJson,
    /// Print LC_DECTR_PRM schema as JSON for programmatic tooling.
    LcDectrPrmSchemaJson,
    /// Print default LC_DECTR_PRM as JSON map payload.
    LcDectrPrmDefaultJson,
    /// Print default LC_DECTR_PRM as JSON ordered values payload.
    LcDectrPrmDefaultValuesJson,
    /// Print LC_EVETR_PRM schema as JSON for programmatic tooling.
    LcEvetrPrmSchemaJson,
    /// Print default LC_EVETR_PRM as JSON map payload.
    LcEvetrPrmDefaultJson,
    /// Print default LC_EVETR_PRM as JSON ordered values payload.
    LcEvetrPrmDefaultValuesJson,
    /// Print LC_GRASS_PRM schema as JSON for programmatic tooling.
    LcGrassPrmSchemaJson,
    /// Print default LC_GRASS_PRM as JSON map payload.
    LcGrassPrmDefaultJson,
    /// Print default LC_GRASS_PRM as JSON ordered values payload.
    LcGrassPrmDefaultValuesJson,
    /// Print SURF_STORE_PRM schema as JSON for programmatic tooling.
    SurfStorePrmSchemaJson,
    /// Print default SURF_STORE_PRM as JSON map payload.
    SurfStorePrmDefaultJson,
    /// Print default SURF_STORE_PRM as JSON ordered values payload.
    SurfStorePrmDefaultValuesJson,
    /// Print WATER_DIST_PRM schema as JSON for programmatic tooling.
    WaterDistPrmSchemaJson,
    /// Print default WATER_DIST_PRM as JSON map payload.
    WaterDistPrmDefaultJson,
    /// Print default WATER_DIST_PRM as JSON ordered values payload.
    WaterDistPrmDefaultValuesJson,
    /// Print IRRIG_daywater schema as JSON for programmatic tooling.
    IrrigDaywaterSchemaJson,
    /// Print default IRRIG_daywater as JSON map payload.
    IrrigDaywaterDefaultJson,
    /// Print default IRRIG_daywater as JSON ordered values payload.
    IrrigDaywaterDefaultValuesJson,
    /// Print IRRIGATION_PRM schema as JSON for programmatic tooling.
    IrrigationPrmSchemaJson,
    /// Print default IRRIGATION_PRM as JSON map payload.
    IrrigationPrmDefaultJson,
    /// Print default IRRIGATION_PRM as JSON ordered values payload.
    IrrigationPrmDefaultValuesJson,
    /// Print LUMPS_PRM schema as JSON for programmatic tooling.
    LumpsPrmSchemaJson,
    /// Print default LUMPS_PRM as JSON map payload.
    LumpsPrmDefaultJson,
    /// Print default LUMPS_PRM as JSON ordered values payload.
    LumpsPrmDefaultValuesJson,
    /// Print OHM_COEF_LC schema as JSON for programmatic tooling.
    OhmCoefLcSchemaJson,
    /// Print default OHM_COEF_LC as JSON map payload.
    OhmCoefLcDefaultJson,
    /// Print default OHM_COEF_LC as JSON ordered values payload.
    OhmCoefLcDefaultValuesJson,
    /// Print OHM_PRM schema as JSON for programmatic tooling.
    OhmPrmSchemaJson,
    /// Print default OHM_PRM as JSON map payload.
    OhmPrmDefaultJson,
    /// Print default OHM_PRM as JSON ordered values payload.
    OhmPrmDefaultValuesJson,
    /// Print solar_State schema as JSON for programmatic tooling.
    SolarStateSchemaJson,
    /// Print default solar_State as JSON map payload.
    SolarStateDefaultJson,
    /// Print default solar_State as JSON ordered values payload.
    SolarStateDefaultValuesJson,
    /// Print ROUGHNESS_STATE schema as JSON for programmatic tooling.
    RoughnessStateSchemaJson,
    /// Print default ROUGHNESS_STATE as JSON map payload.
    RoughnessStateDefaultJson,
    /// Print default ROUGHNESS_STATE as JSON ordered values payload.
    RoughnessStateDefaultValuesJson,
    /// Print NHOOD_STATE schema as JSON for programmatic tooling.
    NhoodStateSchemaJson,
    /// Print default NHOOD_STATE as JSON map payload.
    NhoodStateDefaultJson,
    /// Print default NHOOD_STATE as JSON ordered values payload.
    NhoodStateDefaultValuesJson,
}

fn main() {
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

#[cfg(feature = "physics")]
fn run_physics_command(config_path: &str, output_dir: &str, format: &str) -> Result<(), String> {
    let supported = if cfg!(feature = "arrow-output") {
        vec!["csv", "arrow"]
    } else {
        vec!["csv"]
    };
    if !supported.iter().any(|s| format.eq_ignore_ascii_case(s)) {
        return Err(format!(
            "unsupported output format `{format}`; supported: {}",
            supported.join(", ")
        ));
    }

    let config_path = std::path::PathBuf::from(config_path);
    let config_yaml = fs::read_to_string(&config_path)
        .map_err(|e| format!("failed to read config {}: {e}", config_path.display()))?;
    let run_cfg = load_run_config(&config_path)?;
    let raw_forcing = read_forcing_block(&run_cfg.forcing_path)?;
    let forcing = interpolate_forcing(&raw_forcing, run_cfg.timer.tstep)?;

    let (output_block, _state, len_sim) =
        run_from_config_str_and_forcing(&config_yaml, forcing.block, forcing.len_sim)
            .map_err(|e| e.to_string())?;

    let output_path = if format.eq_ignore_ascii_case("csv") {
        write_output_csv(std::path::Path::new(output_dir), &output_block, len_sim)?
    } else {
        #[cfg(feature = "arrow-output")]
        {
            write_output_arrow(std::path::Path::new(output_dir), &output_block, len_sim)?
        }
        #[cfg(not(feature = "arrow-output"))]
        {
            return Err(format!(
                "output format `{format}` requires the `arrow-output` feature"
            ));
        }
    };

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
                StandardSchemaAction::DefaultJson => {
                    run_flat(FlatCommand::[<$prefix DefaultJson>])?;
                }
                StandardSchemaAction::DefaultValuesJson => {
                    run_flat(FlatCommand::[<$prefix DefaultValuesJson>])?;
                }
            }
        }
    }};
}

fn run_ohm_state_action(action: OhmStateAction) -> Result<(), String> {
    match action {
        OhmStateAction::SchemaJson => run_flat(FlatCommand::StateSchemaJson)?,
        OhmStateAction::DefaultJson => run_flat(FlatCommand::StateDefaultJson)?,
        OhmStateAction::DefaultValuesJson => run_flat(FlatCommand::StateDefaultValuesJson)?,
        OhmStateAction::FlatSchema => run_flat(FlatCommand::StateSchema)?,
        OhmStateAction::StepJson {
            dt,
            dt_since_start,
            qn1,
            a1,
            a2,
            a3,
            state_json,
        } => run_flat(FlatCommand::StateStepJson {
            dt,
            dt_since_start,
            qn1,
            a1,
            a2,
            a3,
            state_json,
        })?,
        OhmStateAction::StepValuesJson {
            dt,
            dt_since_start,
            qn1,
            a1,
            a2,
            a3,
            state_values_json,
        } => run_flat(FlatCommand::StateStepValuesJson {
            dt,
            dt_since_start,
            qn1,
            a1,
            a2,
            a3,
            state_values_json,
        })?,
        OhmStateAction::Qs {
            qn1,
            dqndt,
            a1,
            a2,
            a3,
        } => run_flat(FlatCommand::Qs {
            qn1,
            dqndt,
            a1,
            a2,
            a3,
        })?,
        OhmStateAction::Step {
            dt,
            dt_since_start,
            qn1_av_prev,
            dqndt_prev,
            qn1,
            a1,
            a2,
            a3,
        } => run_flat(FlatCommand::Step {
            dt,
            dt_since_start,
            qn1_av_prev,
            dqndt_prev,
            qn1,
            a1,
            a2,
            a3,
        })?,
        OhmStateAction::Series { dt, a1, a2, a3, qn } => {
            run_flat(FlatCommand::Series { dt, a1, a2, a3, qn })?
        }
        OhmStateAction::StateStep {
            dt,
            dt_since_start,
            qn1,
            a1,
            a2,
            a3,
        } => run_flat(FlatCommand::StateStep {
            dt,
            dt_since_start,
            qn1,
            a1,
            a2,
            a3,
        })?,
    }

    Ok(())
}

fn run_suews_site_action(action: SuewsSiteSchemaAction) -> Result<(), String> {
    match action {
        SuewsSiteSchemaAction::SchemaJson => run_flat(FlatCommand::SuewsSiteSchemaJson)?,
        SuewsSiteSchemaAction::DefaultJson => run_flat(FlatCommand::SuewsSiteDefaultJson)?,
        SuewsSiteSchemaAction::DefaultValuesJson => {
            run_flat(FlatCommand::SuewsSiteDefaultValuesJson)?
        }
        SuewsSiteSchemaAction::DefaultNestedJson => {
            run_flat(FlatCommand::SuewsSiteDefaultNestedJson)?
        }
    }

    Ok(())
}

fn run_schema(schema_type: SchemaType) -> Result<(), String> {
    match schema_type {
        SchemaType::OhmState { action } => run_ohm_state_action(action)?,
        SchemaType::SuewsConfig { action } => handle_simple_schema!(action, SuewsConfig),
        SchemaType::SuewsForcing { action } => handle_simple_schema!(action, SuewsForcing),
        SchemaType::HydroState { action } => handle_simple_schema!(action, HydroState),
        SchemaType::HeatState { action } => handle_simple_schema!(action, HeatState),
        SchemaType::SuewsTimer { action } => handle_simple_schema!(action, SuewsTimer),
        SchemaType::FlagState { action } => handle_simple_schema!(action, FlagState),
        SchemaType::AnthroemisState { action } => handle_simple_schema!(action, AnthroemisState),
        SchemaType::AtmState { action } => handle_simple_schema!(action, AtmState),
        SchemaType::PhenologyState { action } => handle_simple_schema!(action, PhenologyState),
        SchemaType::SnowState { action } => handle_simple_schema!(action, SnowState),
        SchemaType::SolarState { action } => handle_simple_schema!(action, SolarState),
        SchemaType::RoughnessState { action } => handle_simple_schema!(action, RoughnessState),
        SchemaType::NhoodState { action } => handle_simple_schema!(action, NhoodState),
        SchemaType::SuewsSite { action } => run_suews_site_action(action)?,
        SchemaType::AnthroHeatPrm { action } => handle_simple_schema!(action, AnthroHeatPrm),
        SchemaType::AnthroEmisPrm { action } => handle_simple_schema!(action, AnthroEmisPrm),
        SchemaType::BuildingArchetypePrm { action } => {
            handle_simple_schema!(action, BuildingArchetypePrm)
        }
        SchemaType::StebbsPrm { action } => handle_simple_schema!(action, StebbsPrm),
        SchemaType::ConductancePrm { action } => handle_simple_schema!(action, ConductancePrm),
        SchemaType::EhcPrm { action } => handle_simple_schema!(action, EhcPrm),
        SchemaType::SpartacusPrm { action } => handle_simple_schema!(action, SpartacusPrm),
        SchemaType::SpartacusLayerPrm { action } => {
            handle_simple_schema!(action, SpartacusLayerPrm)
        }
        SchemaType::Bioco2Prm { action } => handle_simple_schema!(action, Bioco2Prm),
        SchemaType::LaiPrm { action } => handle_simple_schema!(action, LaiPrm),
        SchemaType::SnowPrm { action } => handle_simple_schema!(action, SnowPrm),
        SchemaType::SoilPrm { action } => handle_simple_schema!(action, SoilPrm),
        SchemaType::LumpsPrm { action } => handle_simple_schema!(action, LumpsPrm),
        SchemaType::OhmCoefLc { action } => handle_simple_schema!(action, OhmCoefLc),
        SchemaType::OhmPrm { action } => handle_simple_schema!(action, OhmPrm),
        SchemaType::LcPavedPrm { action } => handle_simple_schema!(action, LcPavedPrm),
        SchemaType::LcBldgPrm { action } => handle_simple_schema!(action, LcBldgPrm),
        SchemaType::LcBsoilPrm { action } => handle_simple_schema!(action, LcBsoilPrm),
        SchemaType::LcWaterPrm { action } => handle_simple_schema!(action, LcWaterPrm),
        SchemaType::LcDectrPrm { action } => handle_simple_schema!(action, LcDectrPrm),
        SchemaType::LcEvetrPrm { action } => handle_simple_schema!(action, LcEvetrPrm),
        SchemaType::LcGrassPrm { action } => handle_simple_schema!(action, LcGrassPrm),
        SchemaType::SurfStorePrm { action } => handle_simple_schema!(action, SurfStorePrm),
        SchemaType::WaterDistPrm { action } => handle_simple_schema!(action, WaterDistPrm),
        SchemaType::IrrigDaywater { action } => handle_simple_schema!(action, IrrigDaywater),
        SchemaType::IrrigationPrm { action } => handle_simple_schema!(action, IrrigationPrm),
        SchemaType::OutputLine { action } => handle_simple_schema!(action, OutputLine),
        SchemaType::OutputBlock { action } => handle_simple_schema!(action, OutputBlock),
        SchemaType::ErrorEntry { action } => handle_simple_schema!(action, ErrorEntry),
        SchemaType::ErrorState { action } => handle_simple_schema!(action, ErrorState),
    }

    Ok(())
}

fn run(cli: Cli) -> Result<(), String> {
    match cli.command {
        #[cfg(feature = "physics")]
        Commands::Run {
            config,
            output_dir,
            format,
        } => {
            run_physics_command(&config, &output_dir, &format)?;
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
        FlatCommand::Qs {
            qn1,
            dqndt,
            a1,
            a2,
            a3,
        } => {
            let qs = qs_calc(qn1, dqndt, a1, a2, a3).map_err(|e| e.to_string())?;
            println!("qs={qs:.10}");
        }
        FlatCommand::Step {
            dt,
            dt_since_start,
            qn1_av_prev,
            dqndt_prev,
            qn1,
            a1,
            a2,
            a3,
        } => {
            let out = ohm_step(dt, dt_since_start, qn1_av_prev, dqndt_prev, qn1, a1, a2, a3)
                .map_err(|e| e.to_string())?;
            println!("qn1_av_next={:.10}", out.qn1_av_next);
            println!("dqndt_next={:.10}", out.dqndt_next);
            println!("qs={:.10}", out.qs);
        }
        FlatCommand::Series { dt, a1, a2, a3, qn } => {
            if qn.is_empty() {
                return Err("`--qn` requires at least one value".to_string());
            }

            let mut model = OhmModel::new(a1, a2, a3, dt, 0.0, 0.0, 0);
            for (idx, qn1) in qn.into_iter().enumerate() {
                let qs = model.step(qn1).map_err(|e| e.to_string())?;
                let st = model.state();
                println!(
                    "step={} qn1={:.6} qn1_av={:.6} dqndt={:.6} qs={:.6}",
                    idx + 1,
                    qn1,
                    st.qn1_av,
                    st.dqndt,
                    qs
                );
            }
        }
        #[cfg(feature = "physics")]
        FlatCommand::Run {
            config,
            output_dir,
            format,
        } => {
            run_physics_command(&config, &output_dir, &format)?;
        }
        FlatCommand::StateStep {
            dt,
            dt_since_start,
            qn1,
            a1,
            a2,
            a3,
        } => {
            let (flat_len, nsurf) = ohm_state_schema().map_err(|e| e.to_string())?;
            let mut state = ohm_state_default_from_fortran().map_err(|e| e.to_string())?;
            let qs = ohm_state_step(&mut state, dt, dt_since_start, qn1, a1, a2, a3)
                .map_err(|e| e.to_string())?;

            println!("schema_flat_len={flat_len}");
            println!("schema_nsurf={nsurf}");
            println!("qn1_av_next={:.10}", state.qn_av);
            println!("dqndt_next={:.10}", state.dqndt);
            println!("iter_safe={}", state.iter_safe);
            println!("qs={:.10}", qs);
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
        FlatCommand::StateDefaultJson => {
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
        FlatCommand::StateDefaultValuesJson => {
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
        FlatCommand::SuewsConfigDefaultJson => {
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
        FlatCommand::SuewsConfigDefaultValuesJson => {
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
        FlatCommand::SuewsForcingDefaultJson => {
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
        FlatCommand::SuewsForcingDefaultValuesJson => {
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
        FlatCommand::HydroStateDefaultJson => {
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
        FlatCommand::HydroStateDefaultValuesJson => {
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
        FlatCommand::HeatStateDefaultJson => {
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
        FlatCommand::HeatStateDefaultValuesJson => {
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
        FlatCommand::SuewsTimerDefaultJson => render_simple_default_json!(suews_timer),
        FlatCommand::SuewsTimerDefaultValuesJson => render_simple_default_values_json!(suews_timer),
        FlatCommand::StateStepJson {
            dt,
            dt_since_start,
            qn1,
            a1,
            a2,
            a3,
            state_json,
        } => {
            let mut state = if let Some(path) = state_json {
                let text = fs::read_to_string(&path)
                    .map_err(|e| format!("failed to read state_json file {path}: {e}"))?;
                let values = parse_state_map_json(&text)
                    .map_err(|e| format!("failed to parse state_json file {path}: {e}"))?;
                ohm_state_from_map(&values).map_err(|e| e.to_string())?
            } else {
                ohm_state_default_from_fortran().map_err(|e| e.to_string())?
            };

            let qs = ohm_state_step(&mut state, dt, dt_since_start, qn1, a1, a2, a3)
                .map_err(|e| e.to_string())?;

            let payload = json!({
                "schema_version": ohm_state_schema_version(),
                "schema_version_runtime": ohm_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "qs": qs,
                "state": ohm_state_to_map(&state),
            });

            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render step json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::StateStepValuesJson {
            dt,
            dt_since_start,
            qn1,
            a1,
            a2,
            a3,
            state_values_json,
        } => {
            let mut state = if let Some(path) = state_values_json {
                let text = fs::read_to_string(&path)
                    .map_err(|e| format!("failed to read state_values_json file {path}: {e}"))?;
                let payload = parse_state_values_json(&text)
                    .map_err(|e| format!("failed to parse state_values_json file {path}: {e}"))?;
                ohm_state_from_values_payload(&payload).map_err(|e| e.to_string())?
            } else {
                ohm_state_default_from_fortran().map_err(|e| e.to_string())?
            };

            let qs = ohm_state_step(&mut state, dt, dt_since_start, qn1, a1, a2, a3)
                .map_err(|e| e.to_string())?;

            let payload = ohm_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": ohm_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "qs": qs,
                "values": payload.values,
            });

            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render values json: {e}"))?;
            println!("{text}");
        }
        FlatCommand::FlagStateSchemaJson => render_simple_schema_json!(flag_state),
        FlatCommand::FlagStateDefaultJson => render_simple_default_json!(flag_state),
        FlatCommand::FlagStateDefaultValuesJson => render_simple_default_values_json!(flag_state),
        FlatCommand::AnthroemisStateSchemaJson => render_simple_schema_json!(anthroemis_state),
        FlatCommand::AnthroemisStateDefaultJson => render_simple_default_json!(anthroemis_state),
        FlatCommand::AnthroemisStateDefaultValuesJson => render_simple_default_values_json!(anthroemis_state),
        FlatCommand::AnthroHeatPrmSchemaJson => render_simple_schema_json!(anthro_heat_prm),
        FlatCommand::AnthroHeatPrmDefaultJson => render_simple_default_json!(anthro_heat_prm),
        FlatCommand::AnthroHeatPrmDefaultValuesJson => render_simple_default_values_json!(anthro_heat_prm),
        FlatCommand::AnthroEmisPrmSchemaJson => render_simple_schema_json!(anthro_emis_prm),
        FlatCommand::AnthroEmisPrmDefaultJson => render_simple_default_json!(anthro_emis_prm),
        FlatCommand::AnthroEmisPrmDefaultValuesJson => render_simple_default_values_json!(anthro_emis_prm),
        FlatCommand::AtmStateSchemaJson => render_simple_schema_json!(atm_state),
        FlatCommand::AtmStateDefaultJson => render_simple_default_json!(atm_state),
        FlatCommand::AtmStateDefaultValuesJson => render_simple_default_values_json!(atm_state),
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
        FlatCommand::BuildingArchetypePrmDefaultJson => {
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
        FlatCommand::BuildingArchetypePrmDefaultValuesJson => {
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
        FlatCommand::StebbsPrmDefaultJson => render_simple_default_json!(stebbs_prm),
        FlatCommand::StebbsPrmDefaultValuesJson => render_simple_default_values_json!(stebbs_prm),
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
        FlatCommand::OutputLineDefaultJson => {
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
        FlatCommand::OutputLineDefaultValuesJson => {
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
        FlatCommand::OutputBlockDefaultJson => {
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
        FlatCommand::OutputBlockDefaultValuesJson => {
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
        FlatCommand::ErrorEntryDefaultJson => {
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
        FlatCommand::ErrorEntryDefaultValuesJson => {
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
        FlatCommand::ErrorStateDefaultJson => {
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
        FlatCommand::ErrorStateDefaultValuesJson => {
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
        FlatCommand::ConductancePrmDefaultJson => render_simple_default_json!(conductance_prm),
        FlatCommand::ConductancePrmDefaultValuesJson => render_simple_default_values_json!(conductance_prm),
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
        FlatCommand::EhcPrmDefaultJson => {
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
        FlatCommand::SpartacusPrmDefaultJson => {
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
        FlatCommand::SpartacusPrmDefaultValuesJson => {
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
        FlatCommand::SpartacusLayerPrmDefaultJson => {
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
        FlatCommand::SpartacusLayerPrmDefaultValuesJson => {
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
        FlatCommand::SuewsSiteDefaultJson => {
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
        FlatCommand::SuewsSiteDefaultValuesJson => {
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
        FlatCommand::SuewsSiteDefaultNestedJson => {
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
        FlatCommand::EhcPrmDefaultValuesJson => {
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
        FlatCommand::Bioco2PrmDefaultJson => render_simple_default_json!(bioco2_prm),
        FlatCommand::Bioco2PrmDefaultValuesJson => render_simple_default_values_json!(bioco2_prm),
        FlatCommand::LaiPrmSchemaJson => render_simple_schema_json!(lai_prm),
        FlatCommand::LaiPrmDefaultJson => render_simple_default_json!(lai_prm),
        FlatCommand::LaiPrmDefaultValuesJson => render_simple_default_values_json!(lai_prm),
        FlatCommand::PhenologyStateSchemaJson => render_simple_schema_json!(phenology_state),
        FlatCommand::PhenologyStateDefaultJson => render_simple_default_json!(phenology_state),
        FlatCommand::PhenologyStateDefaultValuesJson => render_simple_default_values_json!(phenology_state),
        FlatCommand::SnowStateSchemaJson => render_simple_schema_json!(snow_state),
        FlatCommand::SnowStateDefaultJson => render_simple_default_json!(snow_state),
        FlatCommand::SnowStateDefaultValuesJson => render_simple_default_values_json!(snow_state),
        FlatCommand::SnowPrmSchemaJson => render_simple_schema_json!(snow_prm),
        FlatCommand::SnowPrmDefaultJson => render_simple_default_json!(snow_prm),
        FlatCommand::SnowPrmDefaultValuesJson => render_simple_default_values_json!(snow_prm),
        FlatCommand::SoilPrmSchemaJson => render_simple_schema_json!(soil_prm),
        FlatCommand::SoilPrmDefaultJson => render_simple_default_json!(soil_prm),
        FlatCommand::SoilPrmDefaultValuesJson => render_simple_default_values_json!(soil_prm),
        FlatCommand::LcPavedPrmSchemaJson => render_simple_schema_json!(lc_paved_prm),
        FlatCommand::LcPavedPrmDefaultJson => render_simple_default_json!(lc_paved_prm),
        FlatCommand::LcPavedPrmDefaultValuesJson => render_simple_default_values_json!(lc_paved_prm),
        FlatCommand::LcBldgPrmSchemaJson => render_simple_schema_json!(lc_bldg_prm),
        FlatCommand::LcBldgPrmDefaultJson => render_simple_default_json!(lc_bldg_prm),
        FlatCommand::LcBldgPrmDefaultValuesJson => render_simple_default_values_json!(lc_bldg_prm),
        FlatCommand::LcBsoilPrmSchemaJson => render_simple_schema_json!(lc_bsoil_prm),
        FlatCommand::LcBsoilPrmDefaultJson => render_simple_default_json!(lc_bsoil_prm),
        FlatCommand::LcBsoilPrmDefaultValuesJson => render_simple_default_values_json!(lc_bsoil_prm),
        FlatCommand::LcWaterPrmSchemaJson => render_simple_schema_json!(lc_water_prm),
        FlatCommand::LcWaterPrmDefaultJson => render_simple_default_json!(lc_water_prm),
        FlatCommand::LcWaterPrmDefaultValuesJson => render_simple_default_values_json!(lc_water_prm),
        FlatCommand::LcDectrPrmSchemaJson => render_simple_schema_json!(lc_dectr_prm),
        FlatCommand::LcDectrPrmDefaultJson => render_simple_default_json!(lc_dectr_prm),
        FlatCommand::LcDectrPrmDefaultValuesJson => render_simple_default_values_json!(lc_dectr_prm),
        FlatCommand::LcEvetrPrmSchemaJson => render_simple_schema_json!(lc_evetr_prm),
        FlatCommand::LcEvetrPrmDefaultJson => render_simple_default_json!(lc_evetr_prm),
        FlatCommand::LcEvetrPrmDefaultValuesJson => render_simple_default_values_json!(lc_evetr_prm),
        FlatCommand::LcGrassPrmSchemaJson => render_simple_schema_json!(lc_grass_prm),
        FlatCommand::LcGrassPrmDefaultJson => render_simple_default_json!(lc_grass_prm),
        FlatCommand::LcGrassPrmDefaultValuesJson => render_simple_default_values_json!(lc_grass_prm),
        FlatCommand::SurfStorePrmSchemaJson => render_simple_schema_json!(surf_store_prm),
        FlatCommand::SurfStorePrmDefaultJson => render_simple_default_json!(surf_store_prm),
        FlatCommand::SurfStorePrmDefaultValuesJson => render_simple_default_values_json!(surf_store_prm),
        FlatCommand::WaterDistPrmSchemaJson => render_simple_schema_json!(water_dist_prm),
        FlatCommand::WaterDistPrmDefaultJson => render_simple_default_json!(water_dist_prm),
        FlatCommand::WaterDistPrmDefaultValuesJson => render_simple_default_values_json!(water_dist_prm),
        FlatCommand::IrrigDaywaterSchemaJson => render_simple_schema_json!(irrig_daywater),
        FlatCommand::IrrigDaywaterDefaultJson => render_simple_default_json!(irrig_daywater),
        FlatCommand::IrrigDaywaterDefaultValuesJson => render_simple_default_values_json!(irrig_daywater),
        FlatCommand::IrrigationPrmSchemaJson => render_simple_schema_json!(irrigation_prm),
        FlatCommand::IrrigationPrmDefaultJson => render_simple_default_json!(irrigation_prm),
        FlatCommand::IrrigationPrmDefaultValuesJson => render_simple_default_values_json!(irrigation_prm),
        FlatCommand::LumpsPrmSchemaJson => render_simple_schema_json!(lumps_prm),
        FlatCommand::LumpsPrmDefaultJson => render_simple_default_json!(lumps_prm),
        FlatCommand::LumpsPrmDefaultValuesJson => render_simple_default_values_json!(lumps_prm),
        FlatCommand::OhmCoefLcSchemaJson => render_simple_schema_json!(ohm_coef_lc),
        FlatCommand::OhmCoefLcDefaultJson => render_simple_default_json!(ohm_coef_lc),
        FlatCommand::OhmCoefLcDefaultValuesJson => render_simple_default_values_json!(ohm_coef_lc),
        FlatCommand::OhmPrmSchemaJson => render_simple_schema_json!(ohm_prm),
        FlatCommand::OhmPrmDefaultJson => render_simple_default_json!(ohm_prm),
        FlatCommand::OhmPrmDefaultValuesJson => render_simple_default_values_json!(ohm_prm),
        FlatCommand::SolarStateSchemaJson => render_simple_schema_json!(solar_state),
        FlatCommand::SolarStateDefaultJson => render_simple_default_json!(solar_state),
        FlatCommand::SolarStateDefaultValuesJson => render_simple_default_values_json!(solar_state),
        FlatCommand::RoughnessStateSchemaJson => render_simple_schema_json!(roughness_state),
        FlatCommand::RoughnessStateDefaultJson => render_simple_default_json!(roughness_state),
        FlatCommand::RoughnessStateDefaultValuesJson => render_simple_default_values_json!(roughness_state),
        FlatCommand::NhoodStateSchemaJson => render_simple_schema_json!(nhood_state),
        FlatCommand::NhoodStateDefaultJson => render_simple_default_json!(nhood_state),
        FlatCommand::NhoodStateDefaultValuesJson => render_simple_default_values_json!(nhood_state),
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use paste::paste;

    macro_rules! test_schema {
        ($name:ident, $variant:ident, $action_enum:ident, $action:ident) => {
            #[test]
            fn $name() {
                let cli = Cli {
                    command: Commands::Schema {
                        command: SchemaType::$variant {
                            action: $action_enum::$action,
                        },
                    },
                };
                run(cli).expect(concat!(stringify!($name), " should succeed"));
            }
        };
    }

    macro_rules! test_simple_schema_all {
        ($prefix:ident, $variant:ident) => {
            paste! {
                test_schema!(
                    [<run_ $prefix _schema_json_succeeds>],
                    $variant,
                    StandardSchemaAction,
                    SchemaJson
                );
                test_schema!(
                    [<run_ $prefix _default_json_succeeds>],
                    $variant,
                    StandardSchemaAction,
                    DefaultJson
                );
                test_schema!(
                    [<run_ $prefix _default_values_json_succeeds>],
                    $variant,
                    StandardSchemaAction,
                    DefaultValuesJson
                );
            }
        };
    }

    #[test]
    fn parse_state_map_accepts_wrapped_form() {
        let text = r#"{
            "schema_version": 1,
            "state": {
                "qn_surfs.paved": 12.5,
                "qn_rav.bldg": 3.0
            }
        }"#;
        let out = parse_state_map_json(text).expect("wrapped map should parse");
        assert_eq!(out.get("qn_surfs.paved"), Some(&12.5));
        assert_eq!(out.get("qn_rav.bldg"), Some(&3.0));
    }

    #[test]
    fn parse_state_map_rejects_schema_mismatch() {
        let bad_version = ohm_state_schema_version() + 1;
        let text =
            format!("{{\"schema_version\":{bad_version},\"state\":{{\"qn_surfs.paved\":1.0}}}}");
        let err = parse_state_map_json(&text).expect_err("schema mismatch should fail");
        assert!(err.contains("schema_version mismatch"));
    }

    #[test]
    fn parse_state_values_accepts_array_form() {
        let values = vec![0.0_f64; OHM_STATE_FLAT_LEN];
        let text = serde_json::to_string(&values).expect("array json should render");
        let payload = parse_state_values_json(&text).expect("array values should parse");
        assert_eq!(payload.schema_version, ohm_state_schema_version());
        assert_eq!(payload.values.len(), OHM_STATE_FLAT_LEN);
    }

    #[test]
    fn parse_state_values_accepts_wrapped_form() {
        let text = serde_json::to_string(&json!({
            "schema_version": ohm_state_schema_version(),
            "values": vec![0.0_f64; OHM_STATE_FLAT_LEN],
        }))
        .expect("wrapped values json should render");
        let payload = parse_state_values_json(&text).expect("wrapped values should parse");
        assert_eq!(payload.schema_version, ohm_state_schema_version());
        assert_eq!(payload.values.len(), OHM_STATE_FLAT_LEN);
    }

    #[test]
    fn parse_state_values_rejects_non_numeric_values() {
        let text = r#"{"values":[0,1,"x"]}"#;
        let err = parse_state_values_json(text).expect_err("non-numeric values should fail");
        assert!(err.contains("must be numeric"));
    }

    #[test]
    fn parse_state_values_rejects_length_mismatch() {
        let text = serde_json::to_string(&json!({
            "values": vec![0.0_f64; OHM_STATE_FLAT_LEN - 1],
        }))
        .expect("bad values json should render");
        let err = parse_state_values_json(&text).expect_err("length mismatch should fail");
        assert!(err.contains("values length mismatch"));
    }

    #[test]
    fn parse_state_values_rejects_schema_mismatch() {
        let bad_version = ohm_state_schema_version() + 1;
        let text = serde_json::to_string(&json!({
            "schema_version": bad_version,
            "values": vec![0.0_f64; OHM_STATE_FLAT_LEN],
        }))
        .expect("bad schema json should render");
        let err = parse_state_values_json(&text).expect_err("schema mismatch should fail");
        assert!(err.contains("schema_version mismatch"));
    }

    test_schema!(
        run_ohm_state_schema_json_succeeds,
        OhmState,
        OhmStateAction,
        SchemaJson
    );
    test_schema!(
        run_ohm_state_default_json_succeeds,
        OhmState,
        OhmStateAction,
        DefaultJson
    );
    test_schema!(
        run_ohm_state_default_values_json_succeeds,
        OhmState,
        OhmStateAction,
        DefaultValuesJson
    );

    #[test]
    fn run_ohm_state_flat_schema_succeeds() {
        let cli = Cli {
            command: Commands::Schema {
                command: SchemaType::OhmState {
                    action: OhmStateAction::FlatSchema,
                },
            },
        };
        run(cli).expect("ohm-state flat-schema should succeed");
    }

    #[test]
    fn run_ohm_state_qs_succeeds() {
        let cli = Cli {
            command: Commands::Schema {
                command: SchemaType::OhmState {
                    action: OhmStateAction::Qs {
                        qn1: 100.0,
                        dqndt: 0.0,
                        a1: 0.5,
                        a2: 0.3,
                        a3: -30.0,
                    },
                },
            },
        };
        run(cli).expect("ohm-state qs should succeed");
    }

    #[test]
    fn run_ohm_state_step_succeeds() {
        let cli = Cli {
            command: Commands::Schema {
                command: SchemaType::OhmState {
                    action: OhmStateAction::Step {
                        dt: 3600,
                        dt_since_start: 3600,
                        qn1_av_prev: 0.0,
                        dqndt_prev: 0.0,
                        qn1: 100.0,
                        a1: 0.5,
                        a2: 0.3,
                        a3: -30.0,
                    },
                },
            },
        };
        run(cli).expect("ohm-state step should succeed");
    }

    #[test]
    fn run_ohm_state_series_succeeds() {
        let cli = Cli {
            command: Commands::Schema {
                command: SchemaType::OhmState {
                    action: OhmStateAction::Series {
                        dt: 3600,
                        a1: 0.5,
                        a2: 0.3,
                        a3: -30.0,
                        qn: vec![100.0, 110.0, 90.0],
                    },
                },
            },
        };
        run(cli).expect("ohm-state series should succeed");
    }

    #[test]
    fn run_ohm_state_state_step_succeeds() {
        let cli = Cli {
            command: Commands::Schema {
                command: SchemaType::OhmState {
                    action: OhmStateAction::StateStep {
                        dt: 3600,
                        dt_since_start: 3600,
                        qn1: 100.0,
                        a1: 0.5,
                        a2: 0.3,
                        a3: -30.0,
                    },
                },
            },
        };
        run(cli).expect("ohm-state state-step should succeed");
    }

    #[test]
    fn run_ohm_state_step_json_succeeds() {
        let cli = Cli {
            command: Commands::Schema {
                command: SchemaType::OhmState {
                    action: OhmStateAction::StepJson {
                        dt: 3600,
                        dt_since_start: 3600,
                        qn1: 100.0,
                        a1: 0.5,
                        a2: 0.3,
                        a3: -30.0,
                        state_json: None,
                    },
                },
            },
        };
        run(cli).expect("ohm-state step-json should succeed");
    }

    #[test]
    fn run_ohm_state_step_values_json_succeeds() {
        let cli = Cli {
            command: Commands::Schema {
                command: SchemaType::OhmState {
                    action: OhmStateAction::StepValuesJson {
                        dt: 3600,
                        dt_since_start: 3600,
                        qn1: 100.0,
                        a1: 0.5,
                        a2: 0.3,
                        a3: -30.0,
                        state_values_json: None,
                    },
                },
            },
        };
        run(cli).expect("ohm-state step-values-json should succeed");
    }

    test_schema!(
        run_suews_site_schema_json_succeeds,
        SuewsSite,
        SuewsSiteSchemaAction,
        SchemaJson
    );
    test_schema!(
        run_suews_site_default_json_succeeds,
        SuewsSite,
        SuewsSiteSchemaAction,
        DefaultJson
    );
    test_schema!(
        run_suews_site_default_values_json_succeeds,
        SuewsSite,
        SuewsSiteSchemaAction,
        DefaultValuesJson
    );
    test_schema!(
        run_suews_site_default_nested_json_succeeds,
        SuewsSite,
        SuewsSiteSchemaAction,
        DefaultNestedJson
    );

    test_simple_schema_all!(suews_config, SuewsConfig);
    test_simple_schema_all!(suews_forcing, SuewsForcing);
    test_simple_schema_all!(hydro_state, HydroState);
    test_simple_schema_all!(heat_state, HeatState);
    test_simple_schema_all!(suews_timer, SuewsTimer);
    test_simple_schema_all!(flag_state, FlagState);
    test_simple_schema_all!(anthroemis_state, AnthroemisState);
    test_simple_schema_all!(atm_state, AtmState);
    test_simple_schema_all!(phenology_state, PhenologyState);
    test_simple_schema_all!(snow_state, SnowState);
    test_simple_schema_all!(solar_state, SolarState);
    test_simple_schema_all!(roughness_state, RoughnessState);
    test_simple_schema_all!(nhood_state, NhoodState);
    test_simple_schema_all!(anthro_heat_prm, AnthroHeatPrm);
    test_simple_schema_all!(anthro_emis_prm, AnthroEmisPrm);
    test_simple_schema_all!(building_archetype_prm, BuildingArchetypePrm);
    test_simple_schema_all!(stebbs_prm, StebbsPrm);
    test_simple_schema_all!(conductance_prm, ConductancePrm);
    test_simple_schema_all!(ehc_prm, EhcPrm);
    test_simple_schema_all!(spartacus_prm, SpartacusPrm);
    test_simple_schema_all!(spartacus_layer_prm, SpartacusLayerPrm);
    test_simple_schema_all!(bioco2_prm, Bioco2Prm);
    test_simple_schema_all!(lai_prm, LaiPrm);
    test_simple_schema_all!(snow_prm, SnowPrm);
    test_simple_schema_all!(soil_prm, SoilPrm);
    test_simple_schema_all!(lumps_prm, LumpsPrm);
    test_simple_schema_all!(ohm_coef_lc, OhmCoefLc);
    test_simple_schema_all!(ohm_prm, OhmPrm);
    test_simple_schema_all!(lc_paved_prm, LcPavedPrm);
    test_simple_schema_all!(lc_bldg_prm, LcBldgPrm);
    test_simple_schema_all!(lc_bsoil_prm, LcBsoilPrm);
    test_simple_schema_all!(lc_water_prm, LcWaterPrm);
    test_simple_schema_all!(lc_dectr_prm, LcDectrPrm);
    test_simple_schema_all!(lc_evetr_prm, LcEvetrPrm);
    test_simple_schema_all!(lc_grass_prm, LcGrassPrm);
    test_simple_schema_all!(surf_store_prm, SurfStorePrm);
    test_simple_schema_all!(water_dist_prm, WaterDistPrm);
    test_simple_schema_all!(irrig_daywater, IrrigDaywater);
    test_simple_schema_all!(irrigation_prm, IrrigationPrm);
    test_simple_schema_all!(output_line, OutputLine);
    test_simple_schema_all!(output_block, OutputBlock);
    test_simple_schema_all!(error_entry, ErrorEntry);
    test_simple_schema_all!(error_state, ErrorState);
}
