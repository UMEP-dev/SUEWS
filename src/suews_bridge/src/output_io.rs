//! Optional Arrow file output for the standalone engine, adjacent to the bridge
//! transport core. Group widths come from generated metadata; the SUEWS names
//! below are still hand-maintained and other groups use indexed names. Width
//! agreement alone does not establish parity with Python's output registry.
//! This writer serialises existing results and must not reinterpret the science.

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

/// Build column names for the full combined output buffer.
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
    let mut columns: Vec<Vec<f64>> = (0..OUTPUT_ALL_COLS)
        .map(|_| Vec::with_capacity(len_sim))
        .collect();
    for row in output_block.as_chunks::<OUTPUT_ALL_COLS>().0 {
        for (col, &val) in columns.iter_mut().zip(row) {
            col.push(val);
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

#[cfg(test)]
mod tests {
    use super::*;
    use arrow::array::Array;
    use arrow::ipc::reader::FileReader;
    use std::collections::HashSet;

    #[test]
    fn build_all_column_names_matches_suews_names_and_group_layout() {
        let names = build_all_column_names();
        assert_eq!(names.len(), OUTPUT_ALL_COLS);
        assert_eq!(OUTPUT_SUEWS_COLS, SUEWS_OUTPUT_NAMES.len());

        // First group must be SUEWS with exact positional variable names iy..QS_Water.
        assert_eq!(OUTPUT_GROUP_LAYOUT[0], ("SUEWS", OUTPUT_SUEWS_COLS));
        assert_eq!(names[0], "iy");
        assert_eq!(names[OUTPUT_SUEWS_COLS - 1], "QS_Water");
        for (actual, &expected) in names[..OUTPUT_SUEWS_COLS]
            .iter()
            .zip(SUEWS_OUTPUT_NAMES.iter())
        {
            assert_eq!(actual, expected);
        }

        // Subsequent groups must follow `{group}_{idx}` matching OUTPUT_GROUP_LAYOUT.
        let mut offset = OUTPUT_SUEWS_COLS;
        for &(group, ncols) in &OUTPUT_GROUP_LAYOUT[1..] {
            for (idx, actual) in names[offset..offset + ncols].iter().enumerate() {
                assert_eq!(actual, &format!("{group}_{idx}"));
            }
            offset += ncols;
        }
        assert_eq!(offset, OUTPUT_ALL_COLS);

        // Column names must be globally unique across all output groups.
        let unique: HashSet<&str> = names.iter().map(String::as_str).collect();
        assert_eq!(unique.len(), names.len());
    }

    #[test]
    fn group_layout_metadata_matches_output_group_layout_and_total_width() {
        let meta = group_layout_metadata();
        assert_eq!(meta.len(), 1);
        let encoded = meta
            .get("group_layout")
            .expect("group_layout metadata key must be present");

        let parsed: Vec<(&str, usize)> = encoded
            .split(',')
            .map(|entry| {
                let (name, width) = entry
                    .split_once(':')
                    .expect("each group_layout entry must have 'name:width' format");
                let ncols: usize = width
                    .parse()
                    .expect("group width must be a valid unsigned integer");
                (name, ncols)
            })
            .collect();

        assert_eq!(parsed.as_slice(), OUTPUT_GROUP_LAYOUT);
        let total_cols: usize = parsed.iter().map(|&(_, ncols)| ncols).sum();
        assert_eq!(total_cols, OUTPUT_ALL_COLS);
    }

    #[test]
    fn write_output_arrow_round_trips_values_schema_metadata_and_sentinels() {
        let dir = tempfile::tempdir().expect("create tempdir");
        let output_dir = dir.path().join("nested_output");
        let len_sim = 3;

        // Distinct row-major values so any transpose/stride bug is caught immediately.
        let mut output_block = Vec::with_capacity(len_sim * OUTPUT_ALL_COLS);
        for row_idx in 0..len_sim {
            for col_idx in 0..OUTPUT_ALL_COLS {
                output_block.push(((row_idx + 1) * 10_000 + col_idx) as f64 + 0.25);
            }
        }

        // Inject -999.0 sentinels and NaN values across SUEWS and non-SUEWS groups.
        output_block[10] = -999.0; // row 0, QN
        output_block[OUTPUT_ALL_COLS] = -999.0; // row 1, iy
        output_block[2 * OUTPUT_ALL_COLS + (OUTPUT_ALL_COLS - 1)] = -999.0; // row 2, last col
        output_block[5] = f64::NAN; // row 0, Kdown
        output_block[OUTPUT_ALL_COLS + OUTPUT_SUEWS_COLS] = f64::NAN; // row 1, snow_0
        output_block[2 * OUTPUT_ALL_COLS + (OUTPUT_SUEWS_COLS - 1)] = f64::NAN; // row 2, QS_Water
        output_block[OUTPUT_ALL_COLS + (OUTPUT_SUEWS_COLS - 1)] = -123.456; // row 1, QS_Water

        let out_path = write_output_arrow(&output_dir, &output_block, len_sim)
            .expect("write_output_arrow should succeed");
        assert_eq!(out_path, output_dir.join("suews_output.arrow"));
        assert!(out_path.exists());

        let file = File::open(&out_path).expect("open written Arrow IPC file");
        let reader = FileReader::try_new(file, None).expect("parse Arrow IPC file");

        let expected_names = build_all_column_names();
        let schema = reader.schema();
        assert_eq!(schema.fields().len(), OUTPUT_ALL_COLS);
        for (field, expected_name) in schema.fields().iter().zip(expected_names.iter()) {
            assert_eq!(field.name(), expected_name);
            assert_eq!(field.data_type(), &DataType::Float64);
            assert!(!field.is_nullable());
        }
        assert_eq!(schema.metadata(), &group_layout_metadata());

        let batches: Vec<RecordBatch> = reader
            .collect::<Result<_, _>>()
            .expect("read all Arrow record batches");
        assert_eq!(batches.len(), 1);
        let batch = &batches[0];
        assert_eq!(batch.num_rows(), len_sim);
        assert_eq!(batch.num_columns(), OUTPUT_ALL_COLS);

        for (col_idx, col) in batch.columns().iter().enumerate() {
            let arr = col
                .as_any()
                .downcast_ref::<Float64Array>()
                .expect("Arrow column should be Float64Array");
            // Non-nullable float columns preserve -999.0 and NaN as literal values, not nulls.
            assert_eq!(arr.null_count(), 0);
            for (row_idx, row) in output_block
                .as_chunks::<OUTPUT_ALL_COLS>()
                .0
                .iter()
                .enumerate()
            {
                let expected = row[col_idx];
                let actual = arr.value(row_idx);
                if expected.is_nan() {
                    assert!(
                        actual.is_nan(),
                        "expected NaN at row {row_idx}, col {col_idx}, got {actual}"
                    );
                } else {
                    assert_eq!(
                        actual.to_bits(),
                        expected.to_bits(),
                        "value mismatch at row {row_idx}, col {col_idx}: expected {expected}, got {actual}"
                    );
                }
            }
        }
    }

    #[test]
    fn write_output_arrow_supports_zero_rows() {
        let dir = tempfile::tempdir().expect("create tempdir");
        let out_path = write_output_arrow(dir.path(), &[], 0)
            .expect("write_output_arrow should support zero rows");

        let file = File::open(&out_path).expect("open zero-row Arrow IPC file");
        let reader = FileReader::try_new(file, None).expect("parse zero-row Arrow IPC file");
        assert_eq!(reader.schema().fields().len(), OUTPUT_ALL_COLS);
        assert_eq!(reader.schema().metadata(), &group_layout_metadata());

        let batches: Vec<RecordBatch> = reader
            .collect::<Result<_, _>>()
            .expect("read zero-row Arrow batches");
        assert_eq!(batches.len(), 1);
        assert_eq!(batches[0].num_rows(), 0);
        assert_eq!(batches[0].num_columns(), OUTPUT_ALL_COLS);
    }

    #[test]
    fn write_output_arrow_rejects_mismatched_buffer_length_and_overflow() {
        let dir = tempfile::tempdir().expect("create tempdir");
        let missing_dir = dir.path().join("should_not_be_created");

        let short_block = vec![0.0; OUTPUT_ALL_COLS - 1];
        let err = write_output_arrow(&missing_dir, &short_block, 1)
            .expect_err("too-short buffer must fail");
        assert!(
            err.contains("output block length mismatch"),
            "unexpected error for short buffer: {err}"
        );
        assert!(!missing_dir.exists());

        let long_block = vec![0.0; OUTPUT_ALL_COLS + 1];
        let err = write_output_arrow(&missing_dir, &long_block, 1)
            .expect_err("too-long buffer must fail");
        assert!(
            err.contains("output block length mismatch"),
            "unexpected error for long buffer: {err}"
        );
        assert!(!missing_dir.exists());

        let extra_for_zero = vec![0.0; OUTPUT_ALL_COLS];
        let err = write_output_arrow(&missing_dir, &extra_for_zero, 0)
            .expect_err("non-empty buffer for len_sim=0 must fail");
        assert!(
            err.contains("output block length mismatch"),
            "unexpected error for zero-row mismatch: {err}"
        );

        let err = write_output_arrow(&missing_dir, &[], usize::MAX)
            .expect_err("len_sim overflow must fail");
        assert!(
            err.contains("output length overflow"),
            "unexpected error for overflow: {err}"
        );
        assert!(!missing_dir.exists());
    }
}
