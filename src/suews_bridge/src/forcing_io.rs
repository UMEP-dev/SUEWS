use std::collections::HashMap;
use std::fs;
use std::path::Path;

pub const MET_FORCING_COLS: usize = 21;

// gh#1372 — Baseline-required columns; the loader errors if any is missing.
// Names are lower-cased; lookups are case-insensitive.
const BASELINE_FORCING_COLUMNS: &[&str] = &[
    "iy", "id", "it", "imin", "tair", "rh", "u", "pres", "kdown", "rain",
];

// gh#1372 — Per-landcover whitelist: <var>_<surface>. `wuh` covers
// per-surface external water use — irrigation and impervious-surface
// washing on land surfaces, fountains and ornamental water features
// on the open-water surface — and is therefore accepted on every
// surface. `lai` is leaf-area index and is meaningful only for the
// three vegetated surfaces. The bulk site-level columns `Wuh` /
// `xsmd` remain in the canonical block — `xsmd` is intentionally NOT
// per-landcover (it is fed in as a single bulk soil-moisture-deficit
// value).
const PER_LANDCOVER_FORCING_VARS: &[&str] = &["lai", "wuh"];
const LANDCOVER_SUFFIXES: &[&str] = &[
    "paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water",
];
const LAI_LANDCOVER_SUFFIXES: &[&str] = &["evetr", "dectr", "grass"];
const WUH_LANDCOVER_SUFFIXES: &[&str] = &[
    "paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water",
];

const FORCING_OPTIONAL_FILL: f64 = -999.0;

#[derive(Debug, Clone)]
pub struct ForcingData {
    pub block: Vec<f64>,
    pub len_sim: usize,
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
        let prefix = format!("{var}_");
        if let Some(suffix) = lowered.strip_prefix(&prefix) {
            let allowed: &[&str] = match *var {
                "lai" => LAI_LANDCOVER_SUFFIXES,
                "wuh" => WUH_LANDCOVER_SUFFIXES,
                _ => LANDCOVER_SUFFIXES,
            };
            if allowed.contains(&suffix) {
                return Some(lowered);
            }
        }
    }
    None
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

    let mut col_idx = HashMap::new();
    for (idx, col) in headers.iter().enumerate() {
        col_idx.insert(col.to_ascii_lowercase(), idx);
    }

    // Baseline-required columns must all be present (case-insensitive).
    for name in BASELINE_FORCING_COLUMNS {
        find_col(&col_idx, name)?;
    }

    // Mapping of canonical optional columns -> flat-block index.
    let optional_map: [(usize, &str); 15] = [
        (4, "qn"),
        (5, "qh"),
        (6, "qe"),
        (7, "qs"),
        (8, "qf"),
        (9, "u"),
        (10, "rh"),
        (11, "tair"),
        (12, "pres"),
        (13, "rain"),
        (14, "kdown"),
        (15, "snow"),
        (16, "ldown"),
        (17, "fcld"),
        (18, "wuh"),
    ];
    // xsmd (19) and lai (20) are also canonical and read identically.
    let extra_canonical: [(usize, &str); 2] = [(19, "xsmd"), (20, "lai")];

    let iy_col = find_col(&col_idx, "iy")?;
    let id_col = find_col(&col_idx, "id")?;
    let it_col = find_col(&col_idx, "it")?;
    let imin_col = find_col(&col_idx, "imin")?;

    // Identify per-landcover columns up-front so we can pre-allocate Vec<f64>.
    let canonical_lower: Vec<&str> = {
        let mut v: Vec<&str> = BASELINE_FORCING_COLUMNS.to_vec();
        for (_, name) in optional_map.iter().chain(extra_canonical.iter()) {
            v.push(name);
        }
        v
    };
    let mut extras_targets: Vec<(String, usize)> = Vec::new();
    for (idx, header) in headers.iter().enumerate() {
        let lowered = header.to_ascii_lowercase();
        if canonical_lower.contains(&lowered.as_str()) {
            continue;
        }
        match is_per_landcover(&lowered) {
            Some(canon) => extras_targets.push((canon, idx)),
            None => {
                eprintln!(
                    "warning: forcing column '{header}' is not canonical; ignored (gh#1372)"
                );
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

        let mut row = vec![FORCING_OPTIONAL_FILL; MET_FORCING_COLS];
        row[0] = parse_f64(parts[iy_col], line_no, "iy")?;
        row[1] = parse_f64(parts[id_col], line_no, "id")?;
        row[2] = parse_f64(parts[it_col], line_no, "it")?;
        row[3] = parse_f64(parts[imin_col], line_no, "imin")?;

        for (target_idx, col_name) in optional_map.iter().chain(extra_canonical.iter()) {
            if let Some(source_idx) = col_idx.get(*col_name) {
                row[*target_idx] = parse_f64(parts[*source_idx], line_no, col_name)?;
            }
        }

        // Convert pressure from kPa (SUEWS input convention) to hPa (Fortran internal).
        // Matches supy/_load.py: df_forcing_met["pres"] *= 10.
        // pres is in BASELINE_FORCING_COLUMNS so it is always read; defend against
        // the sentinel anyway in case a future change relaxes that requirement.
        if row[12] != FORCING_OPTIONAL_FILL {
            row[12] *= 10.0;
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
    })
}

// ---------------------------------------------------------------------------
// Forcing time interpolation
// ---------------------------------------------------------------------------

/// Forcing columns interpolated as instantaneous point values (linear).
const INST_COLS: [usize; 8] = [
    9,  // U
    10, // RH
    11, // Tair
    12, // pres
    15, // snow
    17, // fcld
    19, // xsmd
    20, // lai
];

/// Forcing columns interpolated as period averages (shift by -tstep_in/2, then linear).
const AVG_COLS: [usize; 7] = [
    4,  // qn
    5,  // qh
    6,  // qe
    7,  // qs
    8,  // qf
    14, // kdown
    16, // ldown
];

/// Forcing columns interpolated as period sums (proportional redistribution).
const SUM_COLS: [usize; 2] = [
    13, // rain
    18, // wuh
];

fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

fn days_in_year(year: i32) -> i64 {
    if is_leap_year(year) {
        366
    } else {
        365
    }
}

/// Convert (year, day-of-year, hour, minute) to seconds since Jan 1 00:00 of `base_year`.
fn to_seconds(iy: i32, id: i32, it: i32, imin: i32, base_year: i32) -> i64 {
    let mut days: i64 = 0;
    for y in base_year..iy {
        days += days_in_year(y);
    }
    days += (id - 1) as i64; // id is 1-based
    days * 86400 + it as i64 * 3600 + imin as i64 * 60
}

/// Convert seconds since Jan 1 00:00 of `base_year` back to (year, doy, hour, minute).
fn from_seconds(total_sec: i64, base_year: i32) -> (i32, i32, i32, i32) {
    let mut remaining = total_sec;
    let mut year = base_year;
    loop {
        let year_sec = days_in_year(year) * 86400;
        if remaining < year_sec {
            break;
        }
        remaining -= year_sec;
        year += 1;
    }
    let day_of_year = (remaining / 86400) as i32 + 1; // 1-based
    remaining %= 86400;
    let hour = (remaining / 3600) as i32;
    remaining %= 3600;
    let minute = (remaining / 60) as i32;
    (year, day_of_year, hour, minute)
}

fn row_val(forcing: &ForcingData, row: usize, col: usize) -> f64 {
    forcing.block[row * MET_FORCING_COLS + col]
}

/// Detect forcing input resolution (seconds) from the first two rows.
fn detect_tstep_in(forcing: &ForcingData) -> Result<i32, String> {
    if forcing.len_sim < 2 {
        return Err("need at least 2 forcing rows to detect resolution".into());
    }
    let base_iy = row_val(forcing, 0, 0) as i32;
    let t0 = to_seconds(
        row_val(forcing, 0, 0) as i32,
        row_val(forcing, 0, 1) as i32,
        row_val(forcing, 0, 2) as i32,
        row_val(forcing, 0, 3) as i32,
        base_iy,
    );
    let t1 = to_seconds(
        row_val(forcing, 1, 0) as i32,
        row_val(forcing, 1, 1) as i32,
        row_val(forcing, 1, 2) as i32,
        row_val(forcing, 1, 3) as i32,
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

/// Interpolate forcing data from input resolution to model timestep.
///
/// Applies three interpolation schemes depending on variable type:
/// - Instantaneous (U, RH, Tair, pres, snow, fcld, xsmd, lai): linear interpolation
/// - Average (qn, qh, qe, qs, qf, kdown, ldown): shift by -tstep_in/2, then linear
/// - Sum (rain, wuh): proportional redistribution (step function)
///
/// Time columns (iy, id, it, imin) are regenerated from output timestamps.
pub fn interpolate_forcing(
    forcing: &ForcingData,
    tstep_mod: i32,
) -> Result<ForcingData, String> {
    let n_in = forcing.len_sim;
    if n_in < 2 {
        return Ok(forcing.clone());
    }

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
    let t_in: Vec<i64> = (0..n_in)
        .map(|i| {
            to_seconds(
                row_val(forcing, i, 0) as i32,
                row_val(forcing, i, 1) as i32,
                row_val(forcing, i, 2) as i32,
                row_val(forcing, i, 3) as i32,
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

    let mut block = vec![0.0_f64; n_out * MET_FORCING_COLS];

    // --- Time columns (0=iy, 1=id, 2=it, 3=imin) ---
    for j in 0..n_out {
        let t = t_out_start + j as i64 * tstep_mod_i64;
        let (iy, id, it, imin) = from_seconds(t, base_iy);
        let base = j * MET_FORCING_COLS;
        block[base] = iy as f64;
        block[base + 1] = id as f64;
        block[base + 2] = it as f64;
        block[base + 3] = imin as f64;
    }

    // --- Instantaneous: linear interpolation between input points ---
    // Output times before t_in[0] are backfilled from the first value.
    // The last input row is treated as a boundary marker (not a valid data
    // point): output times after t_in[N-2] are forward-filled from the
    // second-to-last value.  This matches the Python resample_linear_inst
    // behaviour which sets t_end to NaN before interpolation.
    let t_in_0 = t_in[0];
    let t_in_penult = t_in[n_in - 2];
    for &col in &INST_COLS {
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
            block[j * MET_FORCING_COLS + col] = v;
        }
    }

    // --- Average: shift input by -tstep_in/2, then linear interpolation ---
    // Shifted times represent period midpoints. Backfill before first, forward-fill after last.
    let half_in = tstep_in_i64 / 2;
    let t_shifted_0 = t_in_0 - half_in;
    let t_shifted_last = t_in[n_in - 1] - half_in;
    for &col in &AVG_COLS {
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
            block[j * MET_FORCING_COLS + col] = v;
        }
    }

    // --- Sum: proportional redistribution (step function) ---
    // Each input period is evenly split across `ratio` output steps.
    let scale = tstep_mod as f64 / tstep_in_f64;
    for &col in &SUM_COLS {
        for j in 0..n_out {
            let i = j / ratio;
            block[j * MET_FORCING_COLS + col] = row_val(forcing, i, col) * scale;
        }
    }

    Ok(ForcingData {
        block,
        len_sim: n_out,
        extras: HashMap::new(),
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
        assert_eq!(forcing.block.len(), forcing.len_sim * MET_FORCING_COLS);
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
        let block = vec![
            2012.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, //
            2012.0, 1.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        ];
        let forcing = ForcingData {
            block,
            len_sim: 2,
            extras: HashMap::new(),
        };
        assert_eq!(detect_tstep_in(&forcing).unwrap(), 3600);
    }

    #[test]
    fn interpolate_noop_when_same_resolution() {
        let block = vec![
            2012.0, 1.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, //
            2012.0, 1.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        ];
        let forcing = ForcingData {
            block: block.clone(),
            len_sim: 2,
            extras: HashMap::new(),
        };
        let result = interpolate_forcing(&forcing, 300).unwrap();
        assert_eq!(result.len_sim, 2);
        assert_eq!(result.block, block);
    }

    #[test]
    fn interpolate_hourly_to_5min() {
        // 3 hourly rows → 36 five-minute rows
        let mut block = vec![0.0_f64; 3 * MET_FORCING_COLS];
        // Row 0: (2012, 1, 1, 0), U=10
        block[0] = 2012.0;
        block[1] = 1.0;
        block[2] = 1.0;
        block[3] = 0.0;
        block[9] = 10.0; // U (inst)
        block[14] = 100.0; // kdown (avg)
        block[13] = 12.0; // rain (sum)
        // Row 1: (2012, 1, 2, 0), U=20
        let r1 = MET_FORCING_COLS;
        block[r1] = 2012.0;
        block[r1 + 1] = 1.0;
        block[r1 + 2] = 2.0;
        block[r1 + 3] = 0.0;
        block[r1 + 9] = 20.0;
        block[r1 + 14] = 200.0;
        block[r1 + 13] = 24.0;
        // Row 2: (2012, 1, 3, 0), U=30
        let r2 = 2 * MET_FORCING_COLS;
        block[r2] = 2012.0;
        block[r2 + 1] = 1.0;
        block[r2 + 2] = 3.0;
        block[r2 + 3] = 0.0;
        block[r2 + 9] = 30.0;
        block[r2 + 14] = 300.0;
        block[r2 + 13] = 0.0;

        let forcing = ForcingData {
            block,
            len_sim: 3,
            extras: HashMap::new(),
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
        assert_eq!(row_val(&result, 0, 9), 10.0);
        // Row 11 = t_in[0] = (2012,1,1,0): U=10
        assert_eq!(row_val(&result, 11, 9), 10.0);
        // Row 23 = t_in[1] = (2012,1,2,0): U=20
        assert_eq!(row_val(&result, 23, 9), 20.0);
        // Row 17 = midpoint between t_in[0] and t_in[1]: U=15
        assert!((row_val(&result, 17, 9) - 15.0).abs() < 0.01);

        // Sum: rain is redistributed. Row 0's input rain=12.0, scale=1/12=1.0
        assert!((row_val(&result, 0, 13) - 1.0).abs() < 1e-9);
        assert!((row_val(&result, 11, 13) - 1.0).abs() < 1e-9);
        // Row 12 maps to input row 1: rain=24.0 → 2.0
        assert!((row_val(&result, 12, 13) - 2.0).abs() < 1e-9);
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
        let canonical = Path::new("../../test/fixtures/benchmark1/forcing/Kc1_2011_data_5_tiny.txt");
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
        // Canonical block unchanged in shape.
        assert_eq!(forcing.block.len(), forcing.len_sim * MET_FORCING_COLS);
    }
}
