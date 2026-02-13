use clap::{Parser, Subcommand};
use serde_json::json;
use serde_json::Value;
use std::fs;
use suews_bridge::{
    anthroemis_state_default_from_fortran, anthroemis_state_schema, anthroemis_state_schema_info,
    anthroemis_state_schema_version, anthroemis_state_schema_version_runtime,
    anthroemis_state_to_map, anthroemis_state_to_values_payload, atm_state_default_from_fortran,
    atm_state_schema, atm_state_schema_info, atm_state_schema_version,
    atm_state_schema_version_runtime, atm_state_to_map, atm_state_to_values_payload,
    bioco2_prm_default_from_fortran, bioco2_prm_schema, bioco2_prm_schema_info,
    bioco2_prm_schema_version, bioco2_prm_schema_version_runtime, bioco2_prm_to_map,
    bioco2_prm_to_values_payload, conductance_prm_default_from_fortran, conductance_prm_schema,
    conductance_prm_schema_info, conductance_prm_schema_version,
    conductance_prm_schema_version_runtime, conductance_prm_to_map,
    conductance_prm_to_values_payload, flag_state_default_from_fortran, flag_state_schema,
    flag_state_schema_info, flag_state_schema_version, flag_state_schema_version_runtime,
    flag_state_to_map, flag_state_to_values_payload, irrig_daywater_default_from_fortran,
    irrig_daywater_schema, irrig_daywater_schema_info, irrig_daywater_schema_version,
    irrig_daywater_schema_version_runtime, irrig_daywater_to_map, irrig_daywater_to_values_payload,
    irrigation_prm_default_from_fortran, irrigation_prm_schema, irrigation_prm_schema_info,
    irrigation_prm_schema_version, irrigation_prm_schema_version_runtime, irrigation_prm_to_map,
    irrigation_prm_to_values_payload, lai_prm_default_from_fortran, lai_prm_schema,
    lai_prm_schema_info, lai_prm_schema_version, lai_prm_schema_version_runtime, lai_prm_to_map,
    lai_prm_to_values_payload, lumps_prm_default_from_fortran, lumps_prm_schema,
    lumps_prm_schema_info, lumps_prm_schema_version, lumps_prm_schema_version_runtime,
    lumps_prm_to_map, lumps_prm_to_values_payload, nhood_state_default_from_fortran,
    nhood_state_schema, nhood_state_schema_info, nhood_state_schema_version,
    nhood_state_schema_version_runtime, nhood_state_to_map, nhood_state_to_values_payload,
    ohm_coef_lc_default_from_fortran, ohm_coef_lc_schema, ohm_coef_lc_schema_info,
    ohm_coef_lc_schema_version, ohm_coef_lc_schema_version_runtime, ohm_coef_lc_to_map,
    ohm_coef_lc_to_values_payload, ohm_prm_default_from_fortran, ohm_prm_schema,
    ohm_prm_schema_info, ohm_prm_schema_version, ohm_prm_schema_version_runtime, ohm_prm_to_map,
    ohm_prm_to_values_payload, ohm_state_default_from_fortran, ohm_state_field_names,
    ohm_state_from_map, ohm_state_from_values_payload, ohm_state_schema, ohm_state_schema_info,
    ohm_state_schema_version, ohm_state_schema_version_runtime, ohm_state_step, ohm_state_to_map,
    ohm_state_to_values_payload, ohm_step, phenology_state_default_from_fortran,
    phenology_state_schema, phenology_state_schema_info, phenology_state_schema_version,
    phenology_state_schema_version_runtime, phenology_state_to_map,
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
    solar_state_to_values_payload, suews_config_default_from_fortran, suews_config_schema,
    suews_config_schema_info, suews_config_schema_version, suews_config_schema_version_runtime,
    suews_config_to_map, suews_config_to_values_payload, suews_timer_default_from_fortran,
    suews_timer_schema, suews_timer_schema_info, suews_timer_schema_version,
    suews_timer_schema_version_runtime, suews_timer_to_map, suews_timer_to_values_payload,
    surf_store_prm_default_from_fortran, surf_store_prm_schema, surf_store_prm_schema_info,
    surf_store_prm_schema_version, surf_store_prm_schema_version_runtime, surf_store_prm_to_map,
    surf_store_prm_to_values_payload, water_dist_prm_default_from_fortran, water_dist_prm_schema,
    water_dist_prm_schema_info, water_dist_prm_schema_version,
    water_dist_prm_schema_version_runtime, water_dist_prm_to_map, water_dist_prm_to_values_payload,
    OhmModel, OhmStateValuesPayload, OHM_STATE_FLAT_LEN,
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
    /// Print atm_state schema as JSON for programmatic tooling.
    AtmStateSchemaJson,
    /// Print default atm_state as JSON map payload.
    AtmStateDefaultJson,
    /// Print default atm_state as JSON ordered values payload.
    AtmStateDefaultValuesJson,
    /// Print CONDUCTANCE_PRM schema as JSON for programmatic tooling.
    ConductancePrmSchemaJson,
    /// Print default CONDUCTANCE_PRM as JSON map payload.
    ConductancePrmDefaultJson,
    /// Print default CONDUCTANCE_PRM as JSON ordered values payload.
    ConductancePrmDefaultValuesJson,
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

fn run(cli: Cli) -> Result<(), String> {
    match cli.command {
        Commands::Qs {
            qn1,
            dqndt,
            a1,
            a2,
            a3,
        } => {
            let qs = qs_calc(qn1, dqndt, a1, a2, a3).map_err(|e| e.to_string())?;
            println!("qs={qs:.10}");
        }
        Commands::Step {
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
        Commands::Series { dt, a1, a2, a3, qn } => {
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
        Commands::StateStep {
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
        Commands::StateSchema => {
            let (flat_len, nsurf) = ohm_state_schema().map_err(|e| e.to_string())?;
            let fields = ohm_state_field_names();

            println!("schema_flat_len={flat_len}");
            println!("schema_nsurf={nsurf}");
            for (idx, name) in fields.iter().enumerate() {
                println!("{idx:02} {name}");
            }
        }
        Commands::StateSchemaJson => {
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
        Commands::StateDefaultJson => {
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
        Commands::StateDefaultValuesJson => {
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
        Commands::SuewsConfigSchemaJson => {
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
        Commands::SuewsConfigDefaultJson => {
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
        Commands::SuewsConfigDefaultValuesJson => {
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
        Commands::SuewsTimerSchemaJson => {
            let schema = suews_timer_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": suews_timer_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::SuewsTimerDefaultJson => {
            let flat_len = suews_timer_schema().map_err(|e| e.to_string())?;
            let state = suews_timer_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": suews_timer_schema_version(),
                "schema_version_runtime": suews_timer_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": suews_timer_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::SuewsTimerDefaultValuesJson => {
            let state = suews_timer_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = suews_timer_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": suews_timer_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::StateStepJson {
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
        Commands::StateStepValuesJson {
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
        Commands::FlagStateSchemaJson => {
            let schema = flag_state_schema_info().map_err(|e| e.to_string())?;

            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": flag_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });

            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::FlagStateDefaultJson => {
            let flat_len = flag_state_schema().map_err(|e| e.to_string())?;
            let state = flag_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": flag_state_schema_version(),
                "schema_version_runtime": flag_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": flag_state_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::FlagStateDefaultValuesJson => {
            let state = flag_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = flag_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": flag_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::AnthroemisStateSchemaJson => {
            let schema = anthroemis_state_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": anthroemis_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::AnthroemisStateDefaultJson => {
            let flat_len = anthroemis_state_schema().map_err(|e| e.to_string())?;
            let state = anthroemis_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": anthroemis_state_schema_version(),
                "schema_version_runtime": anthroemis_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": anthroemis_state_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::AnthroemisStateDefaultValuesJson => {
            let state = anthroemis_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = anthroemis_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": anthroemis_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::AtmStateSchemaJson => {
            let schema = atm_state_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": atm_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::AtmStateDefaultJson => {
            let flat_len = atm_state_schema().map_err(|e| e.to_string())?;
            let state = atm_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": atm_state_schema_version(),
                "schema_version_runtime": atm_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": atm_state_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::AtmStateDefaultValuesJson => {
            let state = atm_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = atm_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": atm_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::ConductancePrmSchemaJson => {
            let schema = conductance_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": conductance_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::ConductancePrmDefaultJson => {
            let flat_len = conductance_prm_schema().map_err(|e| e.to_string())?;
            let state = conductance_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": conductance_prm_schema_version(),
                "schema_version_runtime": conductance_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": conductance_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::ConductancePrmDefaultValuesJson => {
            let state = conductance_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = conductance_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": conductance_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::Bioco2PrmSchemaJson => {
            let schema = bioco2_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": bioco2_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::Bioco2PrmDefaultJson => {
            let flat_len = bioco2_prm_schema().map_err(|e| e.to_string())?;
            let state = bioco2_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": bioco2_prm_schema_version(),
                "schema_version_runtime": bioco2_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": bioco2_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::Bioco2PrmDefaultValuesJson => {
            let state = bioco2_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = bioco2_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": bioco2_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::LaiPrmSchemaJson => {
            let schema = lai_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": lai_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::LaiPrmDefaultJson => {
            let flat_len = lai_prm_schema().map_err(|e| e.to_string())?;
            let state = lai_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": lai_prm_schema_version(),
                "schema_version_runtime": lai_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": lai_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::LaiPrmDefaultValuesJson => {
            let state = lai_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = lai_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": lai_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::PhenologyStateSchemaJson => {
            let schema = phenology_state_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": phenology_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::PhenologyStateDefaultJson => {
            let flat_len = phenology_state_schema().map_err(|e| e.to_string())?;
            let state = phenology_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": phenology_state_schema_version(),
                "schema_version_runtime": phenology_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": phenology_state_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::PhenologyStateDefaultValuesJson => {
            let state = phenology_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = phenology_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": phenology_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::SnowStateSchemaJson => {
            let schema = snow_state_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": snow_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::SnowStateDefaultJson => {
            let flat_len = snow_state_schema().map_err(|e| e.to_string())?;
            let state = snow_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": snow_state_schema_version(),
                "schema_version_runtime": snow_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": snow_state_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::SnowStateDefaultValuesJson => {
            let state = snow_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = snow_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": snow_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::SnowPrmSchemaJson => {
            let schema = snow_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": snow_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::SnowPrmDefaultJson => {
            let flat_len = snow_prm_schema().map_err(|e| e.to_string())?;
            let state = snow_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": snow_prm_schema_version(),
                "schema_version_runtime": snow_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": snow_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::SnowPrmDefaultValuesJson => {
            let state = snow_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = snow_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": snow_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::SoilPrmSchemaJson => {
            let schema = soil_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": soil_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::SoilPrmDefaultJson => {
            let flat_len = soil_prm_schema().map_err(|e| e.to_string())?;
            let state = soil_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": soil_prm_schema_version(),
                "schema_version_runtime": soil_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": soil_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::SoilPrmDefaultValuesJson => {
            let state = soil_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = soil_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": soil_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::SurfStorePrmSchemaJson => {
            let schema = surf_store_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": surf_store_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::SurfStorePrmDefaultJson => {
            let flat_len = surf_store_prm_schema().map_err(|e| e.to_string())?;
            let state = surf_store_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": surf_store_prm_schema_version(),
                "schema_version_runtime": surf_store_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": surf_store_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::SurfStorePrmDefaultValuesJson => {
            let state = surf_store_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = surf_store_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": surf_store_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::WaterDistPrmSchemaJson => {
            let schema = water_dist_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": water_dist_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::WaterDistPrmDefaultJson => {
            let flat_len = water_dist_prm_schema().map_err(|e| e.to_string())?;
            let state = water_dist_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": water_dist_prm_schema_version(),
                "schema_version_runtime": water_dist_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": water_dist_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::WaterDistPrmDefaultValuesJson => {
            let state = water_dist_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = water_dist_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": water_dist_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::IrrigDaywaterSchemaJson => {
            let schema = irrig_daywater_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": irrig_daywater_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::IrrigDaywaterDefaultJson => {
            let flat_len = irrig_daywater_schema().map_err(|e| e.to_string())?;
            let state = irrig_daywater_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": irrig_daywater_schema_version(),
                "schema_version_runtime": irrig_daywater_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": irrig_daywater_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::IrrigDaywaterDefaultValuesJson => {
            let state = irrig_daywater_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = irrig_daywater_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": irrig_daywater_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::IrrigationPrmSchemaJson => {
            let schema = irrigation_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": irrigation_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::IrrigationPrmDefaultJson => {
            let flat_len = irrigation_prm_schema().map_err(|e| e.to_string())?;
            let state = irrigation_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": irrigation_prm_schema_version(),
                "schema_version_runtime": irrigation_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": irrigation_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::IrrigationPrmDefaultValuesJson => {
            let state = irrigation_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = irrigation_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": irrigation_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::LumpsPrmSchemaJson => {
            let schema = lumps_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": lumps_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::LumpsPrmDefaultJson => {
            let flat_len = lumps_prm_schema().map_err(|e| e.to_string())?;
            let state = lumps_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": lumps_prm_schema_version(),
                "schema_version_runtime": lumps_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": lumps_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::LumpsPrmDefaultValuesJson => {
            let state = lumps_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = lumps_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": lumps_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::OhmCoefLcSchemaJson => {
            let schema = ohm_coef_lc_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": ohm_coef_lc_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::OhmCoefLcDefaultJson => {
            let flat_len = ohm_coef_lc_schema().map_err(|e| e.to_string())?;
            let state = ohm_coef_lc_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": ohm_coef_lc_schema_version(),
                "schema_version_runtime": ohm_coef_lc_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": ohm_coef_lc_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::OhmCoefLcDefaultValuesJson => {
            let state = ohm_coef_lc_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = ohm_coef_lc_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": ohm_coef_lc_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::OhmPrmSchemaJson => {
            let schema = ohm_prm_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": ohm_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::OhmPrmDefaultJson => {
            let flat_len = ohm_prm_schema().map_err(|e| e.to_string())?;
            let state = ohm_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": ohm_prm_schema_version(),
                "schema_version_runtime": ohm_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": ohm_prm_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::OhmPrmDefaultValuesJson => {
            let state = ohm_prm_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = ohm_prm_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": ohm_prm_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::SolarStateSchemaJson => {
            let schema = solar_state_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": solar_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::SolarStateDefaultJson => {
            let flat_len = solar_state_schema().map_err(|e| e.to_string())?;
            let state = solar_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": solar_state_schema_version(),
                "schema_version_runtime": solar_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": solar_state_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::SolarStateDefaultValuesJson => {
            let state = solar_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = solar_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": solar_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::RoughnessStateSchemaJson => {
            let schema = roughness_state_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": roughness_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::RoughnessStateDefaultJson => {
            let flat_len = roughness_state_schema().map_err(|e| e.to_string())?;
            let state = roughness_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": roughness_state_schema_version(),
                "schema_version_runtime": roughness_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": roughness_state_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::RoughnessStateDefaultValuesJson => {
            let state = roughness_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = roughness_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": roughness_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
        Commands::NhoodStateSchemaJson => {
            let schema = nhood_state_schema_info().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": schema.schema_version,
                "schema_version_runtime": nhood_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": schema.flat_len,
                "fields": schema.field_names,
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
            println!("{text}");
        }
        Commands::NhoodStateDefaultJson => {
            let flat_len = nhood_state_schema().map_err(|e| e.to_string())?;
            let state = nhood_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = json!({
                "schema_version": nhood_state_schema_version(),
                "schema_version_runtime": nhood_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "flat_len": flat_len,
                "state": nhood_state_to_map(&state),
            });
            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render default state json: {e}"))?;
            println!("{text}");
        }
        Commands::NhoodStateDefaultValuesJson => {
            let state = nhood_state_default_from_fortran().map_err(|e| e.to_string())?;
            let payload = nhood_state_to_values_payload(&state);
            let out = json!({
                "schema_version": payload.schema_version,
                "schema_version_runtime": nhood_state_schema_version_runtime().map_err(|e| e.to_string())?,
                "values": payload.values,
            });
            let text = serde_json::to_string_pretty(&out)
                .map_err(|e| format!("failed to render default values json: {e}"))?;
            println!("{text}");
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn run_state_default_json_succeeds() {
        let cli = Cli {
            command: Commands::StateDefaultJson,
        };
        run(cli).expect("state-default-json should succeed");
    }

    #[test]
    fn run_state_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::StateDefaultValuesJson,
        };
        run(cli).expect("state-default-values-json should succeed");
    }

    #[test]
    fn run_suews_config_default_json_succeeds() {
        let cli = Cli {
            command: Commands::SuewsConfigDefaultJson,
        };
        run(cli).expect("suews-config-default-json should succeed");
    }

    #[test]
    fn run_suews_config_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::SuewsConfigDefaultValuesJson,
        };
        run(cli).expect("suews-config-default-values-json should succeed");
    }

    #[test]
    fn run_suews_timer_default_json_succeeds() {
        let cli = Cli {
            command: Commands::SuewsTimerDefaultJson,
        };
        run(cli).expect("suews-timer-default-json should succeed");
    }

    #[test]
    fn run_suews_timer_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::SuewsTimerDefaultValuesJson,
        };
        run(cli).expect("suews-timer-default-values-json should succeed");
    }

    #[test]
    fn run_flag_state_default_json_succeeds() {
        let cli = Cli {
            command: Commands::FlagStateDefaultJson,
        };
        run(cli).expect("flag-state-default-json should succeed");
    }

    #[test]
    fn run_flag_state_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::FlagStateDefaultValuesJson,
        };
        run(cli).expect("flag-state-default-values-json should succeed");
    }

    #[test]
    fn run_anthroemis_state_default_json_succeeds() {
        let cli = Cli {
            command: Commands::AnthroemisStateDefaultJson,
        };
        run(cli).expect("anthroemis-state-default-json should succeed");
    }

    #[test]
    fn run_anthroemis_state_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::AnthroemisStateDefaultValuesJson,
        };
        run(cli).expect("anthroemis-state-default-values-json should succeed");
    }

    #[test]
    fn run_atm_state_default_json_succeeds() {
        let cli = Cli {
            command: Commands::AtmStateDefaultJson,
        };
        run(cli).expect("atm-state-default-json should succeed");
    }

    #[test]
    fn run_atm_state_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::AtmStateDefaultValuesJson,
        };
        run(cli).expect("atm-state-default-values-json should succeed");
    }

    #[test]
    fn run_conductance_prm_default_json_succeeds() {
        let cli = Cli {
            command: Commands::ConductancePrmDefaultJson,
        };
        run(cli).expect("conductance-prm-default-json should succeed");
    }

    #[test]
    fn run_conductance_prm_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::ConductancePrmDefaultValuesJson,
        };
        run(cli).expect("conductance-prm-default-values-json should succeed");
    }

    #[test]
    fn run_bioco2_prm_default_json_succeeds() {
        let cli = Cli {
            command: Commands::Bioco2PrmDefaultJson,
        };
        run(cli).expect("bioco2-prm-default-json should succeed");
    }

    #[test]
    fn run_bioco2_prm_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::Bioco2PrmDefaultValuesJson,
        };
        run(cli).expect("bioco2-prm-default-values-json should succeed");
    }

    #[test]
    fn run_lai_prm_default_json_succeeds() {
        let cli = Cli {
            command: Commands::LaiPrmDefaultJson,
        };
        run(cli).expect("lai-prm-default-json should succeed");
    }

    #[test]
    fn run_lai_prm_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::LaiPrmDefaultValuesJson,
        };
        run(cli).expect("lai-prm-default-values-json should succeed");
    }

    #[test]
    fn run_phenology_state_default_json_succeeds() {
        let cli = Cli {
            command: Commands::PhenologyStateDefaultJson,
        };
        run(cli).expect("phenology-state-default-json should succeed");
    }

    #[test]
    fn run_phenology_state_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::PhenologyStateDefaultValuesJson,
        };
        run(cli).expect("phenology-state-default-values-json should succeed");
    }

    #[test]
    fn run_snow_state_default_json_succeeds() {
        let cli = Cli {
            command: Commands::SnowStateDefaultJson,
        };
        run(cli).expect("snow-state-default-json should succeed");
    }

    #[test]
    fn run_snow_state_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::SnowStateDefaultValuesJson,
        };
        run(cli).expect("snow-state-default-values-json should succeed");
    }

    #[test]
    fn run_snow_prm_default_json_succeeds() {
        let cli = Cli {
            command: Commands::SnowPrmDefaultJson,
        };
        run(cli).expect("snow-prm-default-json should succeed");
    }

    #[test]
    fn run_snow_prm_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::SnowPrmDefaultValuesJson,
        };
        run(cli).expect("snow-prm-default-values-json should succeed");
    }

    #[test]
    fn run_soil_prm_default_json_succeeds() {
        let cli = Cli {
            command: Commands::SoilPrmDefaultJson,
        };
        run(cli).expect("soil-prm-default-json should succeed");
    }

    #[test]
    fn run_soil_prm_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::SoilPrmDefaultValuesJson,
        };
        run(cli).expect("soil-prm-default-values-json should succeed");
    }

    #[test]
    fn run_surf_store_prm_default_json_succeeds() {
        let cli = Cli {
            command: Commands::SurfStorePrmDefaultJson,
        };
        run(cli).expect("surf-store-prm-default-json should succeed");
    }

    #[test]
    fn run_surf_store_prm_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::SurfStorePrmDefaultValuesJson,
        };
        run(cli).expect("surf-store-prm-default-values-json should succeed");
    }

    #[test]
    fn run_water_dist_prm_default_json_succeeds() {
        let cli = Cli {
            command: Commands::WaterDistPrmDefaultJson,
        };
        run(cli).expect("water-dist-prm-default-json should succeed");
    }

    #[test]
    fn run_water_dist_prm_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::WaterDistPrmDefaultValuesJson,
        };
        run(cli).expect("water-dist-prm-default-values-json should succeed");
    }

    #[test]
    fn run_irrig_daywater_default_json_succeeds() {
        let cli = Cli {
            command: Commands::IrrigDaywaterDefaultJson,
        };
        run(cli).expect("irrig-daywater-default-json should succeed");
    }

    #[test]
    fn run_irrig_daywater_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::IrrigDaywaterDefaultValuesJson,
        };
        run(cli).expect("irrig-daywater-default-values-json should succeed");
    }

    #[test]
    fn run_irrigation_prm_default_json_succeeds() {
        let cli = Cli {
            command: Commands::IrrigationPrmDefaultJson,
        };
        run(cli).expect("irrigation-prm-default-json should succeed");
    }

    #[test]
    fn run_irrigation_prm_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::IrrigationPrmDefaultValuesJson,
        };
        run(cli).expect("irrigation-prm-default-values-json should succeed");
    }

    #[test]
    fn run_lumps_prm_default_json_succeeds() {
        let cli = Cli {
            command: Commands::LumpsPrmDefaultJson,
        };
        run(cli).expect("lumps-prm-default-json should succeed");
    }

    #[test]
    fn run_lumps_prm_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::LumpsPrmDefaultValuesJson,
        };
        run(cli).expect("lumps-prm-default-values-json should succeed");
    }

    #[test]
    fn run_ohm_coef_lc_default_json_succeeds() {
        let cli = Cli {
            command: Commands::OhmCoefLcDefaultJson,
        };
        run(cli).expect("ohm-coef-lc-default-json should succeed");
    }

    #[test]
    fn run_ohm_coef_lc_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::OhmCoefLcDefaultValuesJson,
        };
        run(cli).expect("ohm-coef-lc-default-values-json should succeed");
    }

    #[test]
    fn run_ohm_prm_default_json_succeeds() {
        let cli = Cli {
            command: Commands::OhmPrmDefaultJson,
        };
        run(cli).expect("ohm-prm-default-json should succeed");
    }

    #[test]
    fn run_ohm_prm_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::OhmPrmDefaultValuesJson,
        };
        run(cli).expect("ohm-prm-default-values-json should succeed");
    }

    #[test]
    fn run_solar_state_default_json_succeeds() {
        let cli = Cli {
            command: Commands::SolarStateDefaultJson,
        };
        run(cli).expect("solar-state-default-json should succeed");
    }

    #[test]
    fn run_solar_state_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::SolarStateDefaultValuesJson,
        };
        run(cli).expect("solar-state-default-values-json should succeed");
    }

    #[test]
    fn run_roughness_state_default_json_succeeds() {
        let cli = Cli {
            command: Commands::RoughnessStateDefaultJson,
        };
        run(cli).expect("roughness-state-default-json should succeed");
    }

    #[test]
    fn run_roughness_state_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::RoughnessStateDefaultValuesJson,
        };
        run(cli).expect("roughness-state-default-values-json should succeed");
    }

    #[test]
    fn run_nhood_state_default_json_succeeds() {
        let cli = Cli {
            command: Commands::NhoodStateDefaultJson,
        };
        run(cli).expect("nhood-state-default-json should succeed");
    }

    #[test]
    fn run_nhood_state_default_values_json_succeeds() {
        let cli = Cli {
            command: Commands::NhoodStateDefaultValuesJson,
        };
        run(cli).expect("nhood-state-default-values-json should succeed");
    }
}
