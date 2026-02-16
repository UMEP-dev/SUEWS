use crate::sim::OUTPUT_SUEWS_COLS;
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

#[cfg(feature = "arrow-output")]
use arrow::array::Float64Array;
#[cfg(feature = "arrow-output")]
use arrow::datatypes::{DataType, Field, Schema};
#[cfg(feature = "arrow-output")]
use arrow::ipc::writer::FileWriter;
#[cfg(feature = "arrow-output")]
use arrow::record_batch::RecordBatch;

/// 118 SUEWS output column names, positionally mapped 1:1 to the Fortran
/// output block `dataOutBlockSUEWS(ir, 1:118)`.
///
/// Structure: 5 time columns + 113 physics columns.
/// The time columns are prepended by the Fortran driver via `datetimeLine`.
/// Physics columns sourced from: src/supy/data_model/output/suews_vars.py
const SUEWS_OUTPUT_NAMES: [&str; 118] = [
    // Time columns (0-4) — from Fortran datetimeLine
    "iy", "id", "it", "imin", "dectime",
    // Radiation (5-9)
    "Kdown", "Kup", "Ldown", "Lup", "Tsurf",
    // Energy balance (10-18)
    "QN", "QF", "QS", "QH", "QE",
    "QHlumps", "QElumps", "QHinit", "QHresis",
    // Water balance - core (19-28)
    "Rain", "Irr", "Evap", "RO", "TotCh", "SurfCh",
    "State", "NWtrState", "Drainage", "SMD",
    // Water balance - extended (29-35)
    "FlowCh", "AddWater", "ROSoil", "ROPipe", "ROImp", "ROVeg", "ROWater",
    // Water use by vegetation (36-39)
    "WUInt", "WUEveTr", "WUDecTr", "WUGrass",
    // SMD by surface (40-45)
    "SMDPaved", "SMDBldgs", "SMDEveTr", "SMDDecTr", "SMDGrass", "SMDBSoil",
    // Surface wetness state by surface (46-52)
    "StPaved", "StBldgs", "StEveTr", "StDecTr", "StGrass", "StBSoil", "StWater",
    // Solar geometry and surface properties (53-57)
    "Zenith", "Azimuth", "AlbBulk", "Fcld", "LAI",
    // Turbulence and surface characteristics (58-65)
    "z0m", "zdm", "zL", "UStar", "TStar", "Lob", "RA", "RS",
    // Carbon fluxes (66-72)
    "Fc", "FcPhoto", "FcRespi", "FcMetab", "FcTraff", "FcBuild", "FcPoint",
    // Snow-related (73-84)
    "QNSnowFr", "QNSnow", "AlbSnow", "QM", "QMFreeze", "QMRain",
    "SWE", "MeltWater", "MeltWStore", "SnowCh", "SnowRPaved", "SnowRBldgs",
    // Meteorological at standard heights (85-89)
    "Ts", "T2", "Q2", "U10", "RH2",
    // Surface temperature by type (90-96)
    "Ts_Paved", "Ts_Bldgs", "Ts_EveTr", "Ts_DecTr", "Ts_Grass", "Ts_BSoil", "Ts_Water",
    // Surface temperature dynamic OHM (97-103)
    "Ts_Paved_dyohm", "Ts_Bldgs_dyohm", "Ts_EveTr_dyohm", "Ts_DecTr_dyohm",
    "Ts_Grass_dyohm", "Ts_BSoil_dyohm", "Ts_Water_dyohm",
    // Net all-wave radiation by surface (104-110)
    "QN_Paved", "QN_Bldgs", "QN_EveTr", "QN_DecTr", "QN_Grass", "QN_BSoil", "QN_Water",
    // Storage heat flux by surface (111-117)
    "QS_Paved", "QS_Bldgs", "QS_EveTr", "QS_DecTr", "QS_Grass", "QS_BSoil", "QS_Water",
];

fn suews_output_headers() -> Vec<String> {
    SUEWS_OUTPUT_NAMES.iter().map(|s| s.to_string()).collect()
}

pub fn write_output_csv(
    output_dir: &Path,
    output_block: &[f64],
    len_sim: usize,
) -> Result<PathBuf, String> {
    let expected_len = len_sim
        .checked_mul(OUTPUT_SUEWS_COLS)
        .ok_or_else(|| "output length overflow".to_string())?;

    if output_block.len() != expected_len {
        return Err(format!(
            "output block length mismatch: got {}, expected {}",
            output_block.len(),
            expected_len
        ));
    }

    fs::create_dir_all(output_dir).map_err(|e| {
        format!(
            "failed to create output directory {}: {e}",
            output_dir.display()
        )
    })?;

    let out_path = output_dir.join("suews_output.csv");
    let file = File::create(&out_path)
        .map_err(|e| format!("failed to create {}: {e}", out_path.display()))?;
    let mut writer = BufWriter::new(file);

    let headers = suews_output_headers();
    if headers.len() != OUTPUT_SUEWS_COLS {
        return Err(format!(
            "unexpected SUEWS output header count: got {}, expected {}",
            headers.len(),
            OUTPUT_SUEWS_COLS
        ));
    }

    writeln!(writer, "{}", headers.join(","))
        .map_err(|e| format!("failed to write CSV header: {e}"))?;

    for row_idx in 0..len_sim {
        let base = row_idx * OUTPUT_SUEWS_COLS;
        let mut row = String::new();
        for col_idx in 0..OUTPUT_SUEWS_COLS {
            if col_idx > 0 {
                row.push(',');
            }
            row.push_str(&format!("{:.10}", output_block[base + col_idx]));
        }
        writeln!(writer, "{row}")
            .map_err(|e| format!("failed to write CSV row {}: {e}", row_idx + 1))?;
    }

    writer
        .flush()
        .map_err(|e| format!("failed to flush CSV output: {e}"))?;

    Ok(out_path)
}

#[cfg(feature = "arrow-output")]
pub fn write_output_arrow(
    output_dir: &Path,
    output_block: &[f64],
    len_sim: usize,
) -> Result<PathBuf, String> {
    let expected_len = len_sim
        .checked_mul(OUTPUT_SUEWS_COLS)
        .ok_or_else(|| "output length overflow".to_string())?;

    if output_block.len() != expected_len {
        return Err(format!(
            "output block length mismatch: got {}, expected {}",
            output_block.len(),
            expected_len
        ));
    }

    fs::create_dir_all(output_dir).map_err(|e| {
        format!(
            "failed to create output directory {}: {e}",
            output_dir.display()
        )
    })?;

    // Transpose row-major output_block into column vectors.
    let mut columns: Vec<Vec<f64>> = vec![Vec::with_capacity(len_sim); OUTPUT_SUEWS_COLS];
    for row_idx in 0..len_sim {
        let base = row_idx * OUTPUT_SUEWS_COLS;
        for col_idx in 0..OUTPUT_SUEWS_COLS {
            columns[col_idx].push(output_block[base + col_idx]);
        }
    }

    // Build Arrow schema from column names.
    let fields: Vec<Field> = SUEWS_OUTPUT_NAMES
        .iter()
        .map(|name| Field::new(*name, DataType::Float64, false))
        .collect();
    let schema = Schema::new(fields);

    // Build Arrow arrays (zero-copy from Vec<f64>).
    let arrays: Vec<std::sync::Arc<dyn arrow::array::Array>> = columns
        .into_iter()
        .map(|col| std::sync::Arc::new(Float64Array::from(col)) as _)
        .collect();

    let batch = RecordBatch::try_new(std::sync::Arc::new(schema), arrays)
        .map_err(|e| format!("failed to create Arrow RecordBatch: {e}"))?;

    let out_path = output_dir.join("suews_output.arrow");
    let file = File::create(&out_path)
        .map_err(|e| format!("failed to create {}: {e}", out_path.display()))?;

    let mut writer = FileWriter::try_new(file, &batch.schema())
        .map_err(|e| format!("failed to create Arrow IPC writer: {e}"))?;
    writer
        .write(&batch)
        .map_err(|e| format!("failed to write Arrow IPC batch: {e}"))?;
    writer
        .finish()
        .map_err(|e| format!("failed to finish Arrow IPC file: {e}"))?;

    Ok(out_path)
}
