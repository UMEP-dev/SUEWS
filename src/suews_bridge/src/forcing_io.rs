use std::collections::HashMap;
use std::fs;
use std::path::Path;

use crate::forcing::{
    BASELINE_FORCING_COLUMNS, // TODO: migrate to field descriptors
    PER_LANDCOVER_FORCING_VARS,
    SUEWS_FORCING_BASE_FLAT_LEN,
    PerLandcoverVar,
    SuewsField,
    FieldDescriptor,
    InterpKind,
    FIELD_DESCRIPTORS,
    field_by_name,
};
use crate::forcing_time::{
    TIME_COLUMNS,
    TIME_COLUMN_COUNT,
    to_seconds,
    from_seconds,
};

const FORCING_OPTIONAL_FILL: f64 = -999.0;

// pub const MET_FORCING_COLS: usize = 30;
pub const FORCING_BLOCK_STRIDE: usize = 
    TIME_COLUMN_COUNT + SUEWS_FORCING_BASE_FLAT_LEN;

pub const DATETIME_COLUMNS: &[&str] = &[
    "iy", "id", "it", "imin",
];

#[derive(Debug, Clone)]
pub struct ForcingData {
    pub block: Vec<f64>,
    pub len_sim: usize,
    pub stride: usize,
    pub extras: HashMap<String, Vec<f64>>,
}

fn parse_f64(token: &str, line_no: usize, column: &str) -> Result<f64, String> {
    token
        .parse::<f64>()
        .map_err(|e| format!("failed to parse `{column}` at line {line_no}: {e}"))
}

fn find_col(cols: &HashMap<String, usize>, name: &str) -> Result<usize, String> {
    cols.get(name)
        .copied()
        .ok_or_else(|| format!("forcing header is missing required column `{name}`"))
}

fn is_per_landcover(name: &str) -> Option<String> {
    let lowered = name.to_ascii_lowercase();

    for var in PER_LANDCOVER_FORCING_VARS {
        if let Some(suffix) = lowered.strip_prefix(&format!("{}_", var.prefix)) {
            if var.allowed_suffixes.contains(&suffix) {
                return Some(lowered);
            }
        }
    }

    None
}

fn per_landcover_fields(prefix: &str) -> Vec<SuewsField> {
    FIELD_DESCRIPTORS
        .iter()
        .filter(|d| d.field.name().starts_with(prefix))
        .map(|d| d.field)
        .collect()
}

struct InterpGroups {
    inst: Vec<usize>,
    avg: Vec<usize>,
    sum: Vec<usize>,
}

impl InterpGroups {
    fn from_schema() -> Self {
        let mut inst = Vec::new();
        let mut avg = Vec::new();
        let mut sum = Vec::new();

        for d in FIELD_DESCRIPTORS.iter() {
            let idx = d.field.index();

            match d.interp {
                InterpKind::Instantaneous => inst.push(idx),
                InterpKind::Average => avg.push(idx),
                InterpKind::Sum => sum.push(idx),
            }
        }

        Self { inst, avg, sum }
    }
}

pub fn read_forcing_block(path: &Path) -> Result<ForcingData, String> {
    let text = fs::read_to_string(path)
        .map_err(|e| format!("failed to read forcing file {}: {e}", path.display()))?;

    let mut lines = text.lines().filter(|line| !line.trim().is_empty());
    let header_line = lines
        .next()
        .ok_or_else(|| format!("forcing file {} is empty", path.display()))?;

    let headers: Vec<String> = header_line
        .split_whitespace()
        .map(|s| s.trim().to_string())
        .collect();

    if headers.is_empty() {
        return Err(format!(
            "forcing header in {} does not contain any columns",
            path.display()
        ));
    }

    // Strip a leading '%' so legacy UMEP/SUEWS headers like "%iy" match
    // the canonical "iy" (mirrors the same normalisation applied by
    // src/supy/_load.py::_apply_named_column_matching).
    let mut col_idx = HashMap::new();
    for (idx, col) in headers.iter().enumerate() {
        col_idx.insert(col.trim_start_matches('%').to_ascii_lowercase(), idx);
    }

    // Baseline-required columns must all be present (case-insensitive).
    for d in FIELD_DESCRIPTORS.iter().filter(|d| d.required) {
        let name = d.field.name();
        find_col(&col_idx, name)?;
    }

    // Accepted canonical forcing columns that the current 23-column kernel
    // block does not consume.
    let unused_canonical = ["kdiff", "kdir", "wdir"];

    let iy_col = find_col(&col_idx, "iy")?;
    let id_col = find_col(&col_idx, "id")?;
    let it_col = find_col(&col_idx, "it")?;
    let imin_col = find_col(&col_idx, "imin")?;

    // Identify per-landcover columns up-front so we can pre-allocate Vec<f64>.
    let is_canonical = |name: &str| {
        FIELD_DESCRIPTORS
            .iter()
            .any(|d| d.field.name().eq_ignore_ascii_case(name))
    };

    let mut extras_targets: Vec<(String, usize)> = Vec::new();
    for (idx, header) in headers.iter().enumerate() {
        let lowered = header.to_ascii_lowercase();
        if is_canonical(&lowered) {
            continue;
        }
        match is_per_landcover(&lowered) {
            Some(canon) => extras_targets.push((canon, idx)),
            None => {
                eprintln!("warning: forcing column '{header}' is not canonical; ignored (gh#1372)");
            }
        }
    }
    let mut extras: HashMap<String, Vec<f64>> = HashMap::new();
    for (name, _) in &extras_targets {
        extras.insert(name.clone(), Vec::new());
    }

    let mut block = Vec::new();
    let mut len_sim = 0_usize;

    for (line_idx, raw_line) in lines.enumerate() {
        let line_no = line_idx + 2;
        let parts: Vec<&str> = raw_line.split_whitespace().collect();
        if parts.is_empty() {
            continue;
        }

        if parts.len() < headers.len() {
            return Err(format!(
                "forcing row at line {line_no} has {} columns, expected at least {}",
                parts.len(),
                headers.len()
            ));
        }

        let mut row = vec![FORCING_OPTIONAL_FILL; FORCING_BLOCK_STRIDE];
        let t = TIME_COLUMNS;

        row[t.iy] = parse_f64(parts[iy_col], line_no, "iy")?;
        row[t.id] = parse_f64(parts[id_col], line_no, "id")?;
        row[t.it] = parse_f64(parts[it_col], line_no, "it")?;
        row[t.imin] = parse_f64(parts[imin_col], line_no, "imin")?;

        for d in FIELD_DESCRIPTORS.iter() {
            apply_field(
                &mut row,
                d.field,
                &col_idx,
                &parts,
                line_no,
                None,
            )?;
        }
        for field in per_landcover_fields("lai") {
            apply_field(
                &mut row,
                field,
                &col_idx,
                &parts,
                line_no,
                Some("lai"),
            )?;
        }

        for field in per_landcover_fields("wuh") {
            apply_field(
                &mut row,
                field,
                &col_idx,
                &parts,
                line_no,
                Some("wuh"),
            )?;
        }

        let bulk_lai_col = col_idx.get("lai").copied();
        for (target_idx, lai_col_name) in &lai_kernel_map {
            if let Some(source_idx) = col_idx.get(*lai_col_name).copied().or(bulk_lai_col) {
                row[*target_idx] = parse_f64(parts[source_idx], line_no, *lai_col_name)?;
            }
            
        }

        // Convert pressure from kPa (SUEWS input convention) to hPa (Fortran internal).
        // Matches supy/_load.py: df_forcing_met["pres"] *= 10.
        // pres is in BASELINE_FORCING_COLUMNS so it is always read; defend against
        // the sentinel anyway in case a future change relaxes that requirement.
        if row[SuewsField::pres.index()] != FORCING_OPTIONAL_FILL {
            row[SuewsField::pres.index()] *= 10.0;
        }

        block.extend_from_slice(&row);

        for (canon_name, source_idx) in &extras_targets {
            let v = parse_f64(parts[*source_idx], line_no, canon_name.as_str())?;
            extras.get_mut(canon_name).unwrap().push(v);
        }

        len_sim += 1;
    }

    if len_sim == 0 {
        return Err(format!(
            "forcing file {} contains no timestep rows",
            path.display()
        ));
    }

    Ok(ForcingData {
        block,
        len_sim,
        extras,
        stride: FORCING_BLOCK_STRIDE
    })
}


fn row_val(forcing: &ForcingData, row: usize, col: usize) -> f64 {
    forcing.block[row * forcing.stride + col]
}

/// Detect forcing input resolution (seconds) from the first two rows.
fn detect_tstep_in(forcing: &ForcingData) -> Result<i32, String> {
    if forcing.len_sim < 2 {
        return Err("need at least 2 forcing rows to detect resolution".into());
    }
    let t = TIME_COLUMNS;
    let base_iy = row_val(forcing, 0, 0) as i32;
    let t0 = to_seconds(
        row_val(forcing, 0, t.iy) as i32,
        row_val(forcing, 0, t.id) as i32,
        row_val(forcing, 0, t.it) as i32,
        row_val(forcing, 0, t.imin) as i32,
        base_iy,
    );
    let t1 = to_seconds(
        row_val(forcing, 1, t.iy) as i32,
        row_val(forcing, 1, t.id) as i32,
        row_val(forcing, 1, t.it) as i32,
        row_val(forcing, 1, t.imin) as i32,
        base_iy,
    );

    let diff = t1 - t0;
    if diff <= 0 {
        return Err(format!(
            "forcing timestamps not strictly increasing: t0={t0}, t1={t1}"
        ));
    }
    Ok(diff as i32)
}

fn apply_field(
    row: &mut [f64],
    field: SuewsField,
    col_idx: &HashMap<String, usize>,
    parts: &[&str],
    line_no: usize,
    fallback: Option<&str>,
) -> Result<(), String> {
    let name = field.name().to_ascii_lowercase();

    let source_idx = col_idx
        .get(&name)
        .copied()
        .or_else(|| {
            fallback
                .and_then(|fb| col_idx.get(&fb.to_ascii_lowercase()).copied())
        });

    if let Some(idx) = source_idx {
        row[field.index()] = parse_f64(parts[idx], line_no, &name)?;
    } else if FIELD_DESCRIPTORS.iter().any(|d| d.field == field && d.required) {
        return Err(format!("missing required column `{name}`"));
    }

    Ok(())
}

/// Interpolate forcing data from input resolution to model timestep.
///
/// Applies three interpolation schemes depending on variable type:
/// - Instantaneous (U, RH, Tair, pres, snow, fcld, xsmd, LAI): linear interpolation
/// - Average (qn, qh, qe, qs, qf, kdown, ldown): shift by -tstep_in/2, then linear
/// - Sum (rain, wuh): proportional redistribution (step function)
///
/// Time columns (iy, id, it, imin) are regenerated from output timestamps.
pub fn interpolate_forcing(forcing: &ForcingData, tstep_mod: i32) -> Result<ForcingData, String> {
    let n_in = forcing.len_sim;
    if n_in < 2 {
        return Ok(forcing.clone());
    }

    let groups = InterpGroups::from_schema();

    let tstep_in = detect_tstep_in(forcing)?;
    if tstep_in == tstep_mod {
        return Ok(forcing.clone());
    }
    if tstep_in < tstep_mod {
        return Err(format!(
            "forcing resolution ({tstep_in}s) is finer than model timestep ({tstep_mod}s)"
        ));
    }
    if tstep_in % tstep_mod != 0 {
        return Err(format!(
            "forcing resolution ({tstep_in}s) not divisible by model timestep ({tstep_mod}s)"
        ));
    }

    let ratio = (tstep_in / tstep_mod) as usize;
    let n_out = n_in * ratio;
    let tstep_in_i64 = tstep_in as i64;
    let tstep_mod_i64 = tstep_mod as i64;
    let tstep_in_f64 = tstep_in as f64;

    // Convert input times to seconds from base year
    let base_iy = row_val(forcing, 0, 0) as i32;
    let t = TIME_COLUMNS;
    let t_in: Vec<i64> = (0..n_in)
        .map(|i| {
            to_seconds(
                row_val(forcing, i, t.iy) as i32,
                row_val(forcing, i, t.id) as i32,
                row_val(forcing, i, t.it) as i32,
                row_val(forcing, i, t.imin) as i32,
                base_iy,
            )
        })
        .collect();

    // Output times: t_in[0] - tstep_in + tstep_mod  ..  t_in[N-1]
    let t_out_start = t_in[0] - tstep_in_i64 + tstep_mod_i64;

    // Verify alignment
    let expected_last = t_out_start + (n_out as i64 - 1) * tstep_mod_i64;
    if expected_last != t_in[n_in - 1] {
        return Err(format!(
            "output time alignment error: last output {expected_last} != last input {}",
            t_in[n_in - 1]
        ));
    }

    let mut block = vec![0.0_f64; n_out * forcing.stride];

    // --- Time columns (0=iy, 1=id, 2=it, 3=imin) ---
    for j in 0..n_out {
        let t = t_out_start + j as i64 * tstep_mod_i64;
        let (iy, id, it, imin) = from_seconds(t, base_iy);
        let base = j * forcing.stride;
        let t = TIME_COLUMNS;
        block[base + t.iy] = iy as f64;
        block[base + t.id] = id as f64;
        block[base + t.it] = it as f64;
        block[base + t.imin] = imin as f64;
    }

    // --- Instantaneous: linear interpolation between input points ---
    // Output times before t_in[0] are backfilled from the first value.
    // The last input row is treated as a boundary marker (not a valid data
    // point): output times after t_in[N-2] are forward-filled from the
    // second-to-last value.  This matches the Python resample_linear_inst
    // behaviour which sets t_end to NaN before interpolation.
    let t_in_0 = t_in[0];
    let t_in_penult = t_in[n_in - 2];
    for &col in &groups.inst {
        for j in 0..n_out {
            let t = t_out_start + j as i64 * tstep_mod_i64;
            let v = if t <= t_in_0 {
                row_val(forcing, 0, col)
            } else if t > t_in_penult {
                row_val(forcing, n_in - 2, col)
            } else {
                let offset = (t - t_in_0) as f64;
                let i_float = offset / tstep_in_f64;
                let i = (i_float as usize).min(n_in - 2);
                let alpha = i_float - i as f64;
                let v0 = row_val(forcing, i, col);
                let v1 = row_val(forcing, i + 1, col);
                v0 + alpha * (v1 - v0)
            };
            block[j * forcing.stride + col] = v;
        }
    }

    // --- Average: shift input by -tstep_in/2, then linear interpolation ---
    // Shifted times represent period midpoints. Backfill before first, forward-fill after last.
    let half_in = tstep_in_i64 / 2;
    let t_shifted_0 = t_in_0 - half_in;
    let t_shifted_last = t_in[n_in - 1] - half_in;
    for &col in &groups.avg {
        for j in 0..n_out {
            let t = t_out_start + j as i64 * tstep_mod_i64;
            let v = if t <= t_shifted_0 {
                row_val(forcing, 0, col)
            } else if t >= t_shifted_last {
                row_val(forcing, n_in - 1, col)
            } else {
                let offset = (t - t_shifted_0) as f64;
                let i_float = offset / tstep_in_f64;
                let i = (i_float as usize).min(n_in - 2);
                let alpha = i_float - i as f64;
                let v0 = row_val(forcing, i, col);
                let v1 = row_val(forcing, i + 1, col);
                v0 + alpha * (v1 - v0)
            };
            block[j * forcing.stride + col] = v;
        }
    }

    // --- Sum: proportional redistribution (step function) ---
    // Each input period is evenly split across `ratio` output steps.
    //
    // gh#1372 review: preserve FORCING_OPTIONAL_FILL through interpolation.
    // The SUEWS missing-data sentinel is exactly representable; if a sum
    // column (rain or wuh) carries -999 it must stay -999 in the
    // downscaled output, otherwise scaling by tstep_mod/tstep_in produces
    // a non-sentinel negative value (e.g. hourly -999 -> -83.25 at 5 min)
    // that the Fortran observed-water-use path would treat as a real
    // negative water flux instead of "missing".
    let scale = tstep_mod as f64 / tstep_in_f64;
    for &col in &groups.sum {
        for j in 0..n_out {
            let i = j / ratio;
            let v_in = row_val(forcing, i, col);
            block[j * forcing.stride + col] = if v_in == FORCING_OPTIONAL_FILL {
                FORCING_OPTIONAL_FILL
            } else {
                v_in * scale
            };
        }
    }

    Ok(ForcingData {
        block,
        len_sim: n_out,
        extras: HashMap::new(),
        stride: FORCING_BLOCK_STRIDE,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn parse_fixture_header_and_rows() {
        let path = Path::new("../../test/fixtures/benchmark1/forcing/Kc1_2011_data_5_short.txt");
        let forcing = read_forcing_block(path).expect("fixture forcing should parse");
        assert!(forcing.len_sim > 10);
        assert_eq!(forcing.block.len(), forcing.len_sim * FORCING_BLOCK_STRIDE);
        assert_eq!(forcing.block[0], 2011.0);
        assert_eq!(forcing.block[1], 1.0);
        assert_eq!(forcing.block[2], 0.0);
        assert_eq!(forcing.block[3], 5.0);
    }

    #[test]
    fn time_round_trip() {
        // 2012 is a leap year
        let base = 2012;
        let cases = [
            (2012, 1, 0, 5),
            (2012, 1, 1, 0),
            (2012, 366, 23, 0),
            (2013, 1, 0, 0),
        ];
        for (iy, id, it, imin) in cases {
            let sec = to_seconds(iy, id, it, imin, base);
            let (iy2, id2, it2, imin2) = from_seconds(sec, base);
            assert_eq!(
                (iy, id, it, imin),
                (iy2, id2, it2, imin2),
                "round-trip failed for ({iy},{id},{it},{imin})"
            );
        }
    }

    #[test]
    fn detect_resolution_hourly() {
        // Two rows one hour apart: (2012,1,1,0) and (2012,1,2,0)
        let mut block = vec![0.0_f64; 2 * FORCING_BLOCK_STRIDE];
        block[0] = 2012.0;
        block[1] = 1.0;
        block[2] = 1.0;
        block[3] = 0.0;
        let r1 = FORCING_BLOCK_STRIDE;
        block[r1] = 2012.0;
        block[r1 + 1] = 1.0;
        block[r1 + 2] = 2.0;
        block[r1 + 3] = 0.0;
        let forcing = ForcingData {
            block,
            len_sim: 2,
            extras: HashMap::new(),
            stride: FORCING_BLOCK_STRIDE,
        };
        assert_eq!(detect_tstep_in(&forcing).unwrap(), 3600);
    }

    #[test]
    fn interpolate_noop_when_same_resolution() {
        let mut block = vec![0.0_f64; 2 * FORCING_BLOCK_STRIDE];
        block[0] = 2012.0;
        block[1] = 1.0;
        block[2] = 0.0;
        block[3] = 5.0;
        block[SuewsField::u.index()] = 5.0;
        let r1 = FORCING_BLOCK_STRIDE;
        block[r1] = 2012.0;
        block[r1 + 1] = 1.0;
        block[r1 + 2] = 0.0;
        block[r1 + 3] = 10.0;
        block[r1 + SuewsField::u.index()] = 10.0;
        let forcing = ForcingData {
            block: block.clone(),
            len_sim: 2,
            extras: HashMap::new(),
            stride: FORCING_BLOCK_STRIDE,
        };
        let result = interpolate_forcing(&forcing, 300).unwrap();
        assert_eq!(result.len_sim, 2);
        assert_eq!(result.block, block);
    }

    #[test]
    fn interpolate_hourly_to_5min() {
        // 3 hourly rows to 36 five-minute rows
        let mut block = vec![0.0_f64; 3 * FORCING_BLOCK_STRIDE];
        // Row 0: (2012, 1, 1, 0), U=10
        block[0] = 2012.0;
        block[1] = 1.0;
        block[2] = 1.0;
        block[3] = 0.0;
        block[SuewsField::u.index()] = 10.0; // U (inst)
        block[SuewsField::kdown.index()] = 100.0; // kdown (avg)
        block[SuewsField::rain.index()] = 12.0; // rain (sum)

        // Row 1: (2012, 1, 2, 0), U=20
        let r1 = FORCING_BLOCK_STRIDE;
        block[r1] = 2012.0;
        block[r1 + 1] = 1.0;
        block[r1 + 2] = 2.0;
        block[r1 + 3] = 0.0;
        block[r1 + SuewsField::u.index()] = 20.0;
        block[r1 + SuewsField::kdown.index()] = 200.0;
        block[r1 + SuewsField::rain.index()] = 24.0;
        // Row 2: (2012, 1, 3, 0), U=30
        let r2 = 2 * FORCING_BLOCK_STRIDE;
        block[r2] = 2012.0;
        block[r2 + 1] = 1.0;
        block[r2 + 2] = 3.0;
        block[r2 + 3] = 0.0;
        block[r2 + SuewsField::u.index()] = 30.0;
        block[r2 + SuewsField::kdown.index()] = 300.0;
        block[r2 + SuewsField::rain.index()] = 0.0;

        let forcing = ForcingData {
            block,
            len_sim: 3,
            extras: HashMap::new(),
            stride: FORCING_BLOCK_STRIDE,
        };
        let result = interpolate_forcing(&forcing, 300).unwrap();
        assert_eq!(result.len_sim, 36); // 3 * 12

        // First output time: (2012, 1, 0, 5)
        assert_eq!(row_val(&result, 0, 0), 2012.0);
        assert_eq!(row_val(&result, 0, 1), 1.0);
        assert_eq!(row_val(&result, 0, 2), 0.0);
        assert_eq!(row_val(&result, 0, 3), 5.0);

        // Last output time matches last input: (2012, 1, 3, 0)
        assert_eq!(row_val(&result, 35, 2), 3.0);
        assert_eq!(row_val(&result, 35, 3), 0.0);

        // Instantaneous: first 11 rows are before t_in[0], should be backfilled to 10.0
        assert_eq!(row_val(&result, 0, SuewsField::u.index()), 10.0);
        // Row 11 = t_in[0] = (2012,1,1,0): U=10
        assert_eq!(row_val(&result, 11, SuewsField::temp_c.index()), 10.0);
        // Row 23 = t_in[1] = (2012,1,2,0): U=20
        assert_eq!(row_val(&result, 23, SuewsField::u.index()), 20.0);
        // Row 17 = midpoint between t_in[0] and t_in[1]: U=15
        assert!((row_val(&result, 17, SuewsField::u.index()) - 15.0).abs() < 0.01);

        // Sum: rain is redistributed. Row 0's input rain=12.0, scale=1/12=1.0
        assert!((row_val(&result, 0, SuewsField::rain.index()) - 1.0).abs() < 1e-9);
        assert!((row_val(&result, 11, SuewsField::rain.index()) - 1.0).abs() < 1e-9);
        // Row 12 maps to input row 1: rain=24.0 → 2.0
        assert!((row_val(&result, 12, SuewsField::rain.index()) - 2.0).abs() < 1e-9);
    }

    #[test]
    fn missing_baseline_column_errors() {
        use std::io::Write;
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("no_tair.txt");
        let mut f = std::fs::File::create(&path).unwrap();
        // Header without 'Tair'
        writeln!(f, "iy id it imin RH U pres rain kdown").unwrap();
        writeln!(f, "2011 1 0 5 80 2 100 0 0").unwrap();
        let err = read_forcing_block(&path).expect_err("must error on missing Tair");
        assert!(err.contains("tair") || err.contains("Tair"), "error: {err}");
    }

    #[test]
    fn shuffled_header_yields_identical_block() {
        let canonical =
            Path::new("../../test/fixtures/benchmark1/forcing/Kc1_2011_data_5_tiny.txt");
        let shuffled = Path::new("../../test/fixtures/forcing/kc_shuffled.txt");
        let a = read_forcing_block(canonical).expect("canonical");
        let b = read_forcing_block(shuffled).expect("shuffled");
        assert_eq!(a.len_sim, b.len_sim);
        assert_eq!(a.block, b.block);
    }

    #[test]
    fn per_landcover_columns_loaded_into_extras() {
        let path = Path::new("../../test/fixtures/forcing/kc_per_landcover.txt");
        let forcing = read_forcing_block(path).expect("per-landcover fixture");
        assert!(forcing.extras.contains_key("lai_evetr"));
        assert!(forcing.extras.contains_key("wuh_paved"));
        assert_eq!(forcing.extras["lai_evetr"].len(), forcing.len_sim);
        assert_eq!(row_val(&forcing, 0, SuewsField::lai_evetr.index()), forcing.extras["lai_evetr"][0]);
        assert_eq!(row_val(&forcing, 0, SuewsField::lai_dectr.index()), forcing.extras["lai_dectr"][0]);
        assert_eq!(row_val(&forcing, 0, SuewsField::lai_grass.index()), forcing.extras["lai_grass"][0]);
        assert_ne!(row_val(&forcing, 0, SuewsField::wu_m3.index()), forcing.extras["wuh_paved"][0]);
        // Canonical block unchanged in shape.
        assert_eq!(forcing.block.len(), forcing.len_sim * FORCING_BLOCK_STRIDE);
    }

    #[test]
    fn bulk_lai_broadcasts_to_kernel_lai_columns() {
        use std::io::Write;
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("bulk_lai.txt");
        let mut f = std::fs::File::create(&path).unwrap();
        writeln!(f, "iy id it imin Tair RH U pres kdown rain lai").unwrap();
        writeln!(f, "2011 1 0 5 18.0 80.0 2.0 100.0 0.0 0.0 2.75").unwrap();
        let forcing = read_forcing_block(&path).expect("bulk-lai fixture");
        assert_eq!(row_val(&forcing, 0, SuewsField::lai_evetr.index()), 2.75);
        assert_eq!(row_val(&forcing, 0, SuewsField::lai_dectr.index()), 2.75);
        assert_eq!(row_val(&forcing, 0, SuewsField::lai_grass.index()), 2.75);
    }

    #[test]
    fn bulk_lai_broadcasts_to_kernel_lai_columns() {
        use std::io::Write;
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("bulk_lai.txt");
        let mut f = std::fs::File::create(&path).unwrap();
        writeln!(f, "iy id it imin Tair RH U pres kdown rain lai").unwrap();
        writeln!(f, "2011 1 0 5 18.0 80.0 2.0 100.0 0.0 0.0 2.75").unwrap();
        let forcing = read_forcing_block(&path).expect("bulk-lai fixture");
        assert_eq!(row_val(&forcing, 0, 20), 2.75);
        assert_eq!(row_val(&forcing, 0, 21), 2.75);
        assert_eq!(row_val(&forcing, 0, 22), 2.75);
        
    }

    #[test]
    fn missing_optional_columns_filled_with_sentinel() {
        // gh#1372 review fix: the Rust bridge unifies with Python `_load.py`
        // by writing FORCING_OPTIONAL_FILL (-999) for any optional canonical
        // column that the user omits, instead of leaving 0.0 from the row
        // initialiser. Pin the contract so a future regression surfaces here
        // (a kernel that summed silently-zero columns would shift if -999
        // were ever lost).
        use std::io::Write;
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("baseline_only.txt");
        let mut f = std::fs::File::create(&path).unwrap();
        // Header has only the 10 baseline-required columns (no qn/qh/qe/qs/
        // qf/snow/ldown/fcld/wuh/xsmd/lai).
        writeln!(f, "iy id it imin Tair RH U pres kdown rain").unwrap();
        writeln!(f, "2011 1 0 5 18.0 80.0 2.0 100.0 0.0 0.0").unwrap();
        writeln!(f, "2011 1 0 10 18.5 79.0 2.1 100.0 0.0 0.0").unwrap();
        let forcing = read_forcing_block(&path).expect("baseline-only fixture");
        assert_eq!(forcing.len_sim, 2);
        assert_eq!(forcing.block.len(), forcing.len_sim * FORCING_BLOCK_STRIDE);
        // Optional canonical columns: qn(4), qh(5), qe(6), qs(7), qf(8),
        // snow(15), ldown(16), fcld(17), wuh(18), xsmd(19), and the three
        // LAI kernel columns (20..22). All must hold -999 sentinel for both rows.
        for row in 0..forcing.len_sim {
            for &optional_idx in &[4, 5, 6, 7, 8, 15, 16, 17, 18, 19, 20, 21, 22] {
                let v = row_val(&forcing, row, optional_idx);
                assert!(
                    (v - FORCING_OPTIONAL_FILL).abs() < 1e-9,
                    "row {row} col {optional_idx} expected {} (FORCING_OPTIONAL_FILL), got {v}",
                    FORCING_OPTIONAL_FILL,
                );
            }
        }
    }
}
