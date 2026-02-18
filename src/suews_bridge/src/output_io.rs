use crate::sim::{OUTPUT_ALL_COLS, OUTPUT_GROUP_LAYOUT, OUTPUT_SUEWS_COLS};
use std::collections::HashMap;
use std::fs::{self, File};
use std::path::{Path, PathBuf};

use arrow::array::Float64Array;
use arrow::datatypes::{DataType, Field, Schema};
use arrow::ipc::writer::FileWriter;
use arrow::record_batch::RecordBatch;

/// 118 SUEWS output column names, positionally mapped 1:1 to the Fortran
/// output block `dataOutBlockSUEWS(ir, 1:118)`.
///
/// Structure: 5 time columns + 113 physics columns.
const SUEWS_OUTPUT_NAMES: [&str; 118] = [
    // Time columns (0-4)
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

/// Build column names for the full 1134-column flat buffer.
///
/// The SUEWS group uses proper variable names from [`SUEWS_OUTPUT_NAMES`].
/// Other groups use `{group}_{idx}` where `idx` is zero-based.
fn build_all_column_names() -> Vec<String> {
    let mut names = Vec::with_capacity(OUTPUT_ALL_COLS);
    let mut is_suews = true;
    for &(group, ncols) in OUTPUT_GROUP_LAYOUT {
        if is_suews {
            // SUEWS group: use proper variable names
            debug_assert_eq!(ncols, OUTPUT_SUEWS_COLS);
            for name in &SUEWS_OUTPUT_NAMES {
                names.push(name.to_string());
            }
            is_suews = false;
        } else {
            // Other groups: {group}_{idx}
            for idx in 0..ncols {
                names.push(format!("{group}_{idx}"));
            }
        }
    }
    names
}

/// Build Arrow schema metadata encoding the group layout.
///
/// Stored as `group_layout` key with value
/// `"SUEWS:118,snow:103,BEERS:34,..."` so downstream consumers
/// can split the flat column space into groups.
fn group_layout_metadata() -> HashMap<String, String> {
    let layout_str: String = OUTPUT_GROUP_LAYOUT
        .iter()
        .map(|&(name, ncols)| format!("{name}:{ncols}"))
        .collect::<Vec<_>>()
        .join(",");
    let mut meta = HashMap::new();
    meta.insert("group_layout".to_string(), layout_str);
    meta
}

/// Write all 11 output groups to a single Arrow IPC file.
///
/// The flat buffer has `len_sim` rows of `OUTPUT_ALL_COLS` columns.
/// SUEWS columns use proper variable names; other groups use
/// `{group}_{idx}` naming.  The group layout is stored as Arrow
/// schema metadata.
pub fn write_output_arrow(
    output_dir: &Path,
    output_block: &[f64],
    len_sim: usize,
) -> Result<PathBuf, String> {
    let expected_len = len_sim
        .checked_mul(OUTPUT_ALL_COLS)
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

    // Transpose row-major buffer into column vectors.
    let mut columns: Vec<Vec<f64>> = vec![Vec::with_capacity(len_sim); OUTPUT_ALL_COLS];
    for row_idx in 0..len_sim {
        let base = row_idx * OUTPUT_ALL_COLS;
        for col_idx in 0..OUTPUT_ALL_COLS {
            columns[col_idx].push(output_block[base + col_idx]);
        }
    }

    // Build schema with column names and layout metadata.
    let col_names = build_all_column_names();
    let fields: Vec<Field> = col_names
        .iter()
        .map(|name| Field::new(name, DataType::Float64, false))
        .collect();
    let schema = Schema::new_with_metadata(fields, group_layout_metadata());

    // Build Arrow arrays.
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
