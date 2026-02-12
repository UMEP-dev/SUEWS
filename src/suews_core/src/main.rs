use clap::{Parser, Subcommand};
use serde_json::json;
use std::collections::BTreeMap;
use std::fs;
use suews_core::{
    ohm_state_default_from_fortran, ohm_state_field_names, ohm_state_schema, ohm_state_step,
    ohm_step, ohm_surface_names, qs_calc, OhmModel, OhmState,
};

#[derive(Debug, Parser)]
#[command(
    name = "suews",
    version,
    about = "SUEWS Rust bridge MVP CLI (OHM C API)"
)]
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
}

fn state_to_map(state: &OhmState) -> BTreeMap<String, f64> {
    let names = ohm_state_field_names();
    let values = state.to_flat();
    names.into_iter().zip(values).collect()
}

fn state_from_map(values: &BTreeMap<String, f64>) -> Result<OhmState, String> {
    let names = ohm_state_field_names();
    let mut state = ohm_state_default_from_fortran().map_err(|e| e.to_string())?;
    let mut flat = state.to_flat();

    for (name, value) in values {
        let idx = names
            .iter()
            .position(|n| n == name)
            .ok_or_else(|| format!("unknown OHM_STATE field name: {name}"))?;
        flat[idx] = *value;
    }

    state = OhmState::from_flat(&flat).map_err(|e| e.to_string())?;
    Ok(state)
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
            let (flat_len, nsurf) = ohm_state_schema().map_err(|e| e.to_string())?;
            let fields = ohm_state_field_names();
            let surfaces = ohm_surface_names();

            let payload = json!({
                "flat_len": flat_len,
                "nsurf": nsurf,
                "surface_names": surfaces,
                "fields": fields,
            });

            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render schema json: {e}"))?;
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
                let values: BTreeMap<String, f64> = serde_json::from_str(&text)
                    .map_err(|e| format!("failed to parse state_json file {path}: {e}"))?;
                state_from_map(&values)?
            } else {
                ohm_state_default_from_fortran().map_err(|e| e.to_string())?
            };

            let qs = ohm_state_step(&mut state, dt, dt_since_start, qn1, a1, a2, a3)
                .map_err(|e| e.to_string())?;

            let payload = json!({
                "qs": qs,
                "state": state_to_map(&state),
            });

            let text = serde_json::to_string_pretty(&payload)
                .map_err(|e| format!("failed to render step json: {e}"))?;
            println!("{text}");
        }
    }

    Ok(())
}
