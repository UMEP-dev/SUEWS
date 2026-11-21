//! Standalone-engine forcing file adapter and interpolation, adjacent to the
//! transport core. Column order and interpolation kinds come from `forcing`;
//! timestamps come from `forcing_time`. Existing unit conversion (including
//! file pressure to kernel pressure) belongs to this I/O contract, not codecs.
//! Preserve cumulative/average/instantaneous semantics and optional-value
//! markers; a new scientific default or unit policy needs its owning layer.

use std::collections::HashMap;
use std::fs;
use std::path::Path;

use crate::config::SuewsConfig;
use crate::forcing::{
    PER_LANDCOVER_FORCING_VARS,
    SUEWS_FORCING_BASE_FLAT_LEN,
    SuewsField,
    InterpKind,
    FIELD_DESCRIPTORS,
    descriptor_by_index,
};
use crate::forcing_time::{
    TIME_COLUMNS,
    TIME_COLUMN_COUNT,
    to_seconds,
    from_seconds,
};

const FORCING_OPTIONAL_FILL: f64 = -999.0;
/// Defensive missing-value threshold matching Python (`SUEWS_MISSING_THRESHOLD`)
/// and the Fortran runtime convention (`<= -900D0`).
const FORCING_MISSING_THRESHOLD: f64 = -900.0;

pub(crate) fn is_missing_forcing_value(value: f64) -> bool {
    !value.is_finite() || value <= FORCING_MISSING_THRESHOLD
}

fn sanitize_forcing_value(value: f64) -> f64 {
    if is_missing_forcing_value(value) {
        FORCING_OPTIONAL_FILL
    } else {
        value
    }
}

fn interpolate_pair(v0: f64, v1: f64, alpha: f64) -> f64 {
    if is_missing_forcing_value(v0) || is_missing_forcing_value(v1) {
        FORCING_OPTIONAL_FILL
    } else {
        v0 + alpha * (v1 - v0)
    }
}

/// Mirror `src/supy/data_model/forcing/variables.py` observed-`qf` requirement:
/// when `emissions_method == 0`, every forcing row must carry a finite,
/// non-sentinel `qf` value (`> -900.0`).
pub fn validate_forcing_block_for_emissions(
    forcing_block: &[f64],
    len_sim: usize,
    stride: usize,
    emissions_method: i32,
) -> Result<(), String> {
    if emissions_method != 0 {
        return Ok(());
    }
    let qf_col = SuewsField::qf_obs.index();
    let expected_len = len_sim
        .checked_mul(stride)
        .ok_or_else(|| "forcing block length overflow".to_string())?;
    if stride <= qf_col || forcing_block.len() < expected_len {
        return Err(format!(
            "forcing block shape mismatch: len={}, len_sim={len_sim}, stride={stride}",
            forcing_block.len()
        ));
    }

    let t = TIME_COLUMNS;
    let mut missing_count = 0_usize;
    let mut first_missing: Option<(i32, i32, i32, i32)> = None;

    for row in 0..len_sim {
        let base = row * stride;
        let qf = forcing_block[base + qf_col];
        if is_missing_forcing_value(qf) {
            missing_count += 1;
            if first_missing.is_none() {
                first_missing = Some((
                    forcing_block[base + t.iy] as i32,
                    forcing_block[base + t.id] as i32,
                    forcing_block[base + t.it] as i32,
                    forcing_block[base + t.imin] as i32,
                ));
            }
        }
    }

    if missing_count == 0 {
        return Ok(());
    }

    if missing_count == len_sim {
        return Err(
            "physics/forcing mismatch: forcing column 'qf' must contain valid data when \
             emissions=0 (every row is missing; provide observed qf in W m-2, or set values \
             to 0 to exclude QF from the energy balance)"
                .to_string(),
        );
    }

    let (iy, id, it, imin) = first_missing.unwrap_or((0, 0, 0, 0));
    Err(format!(
        "physics/forcing mismatch: forcing column 'qf' must contain valid data when \
         emissions=0 ({missing_count} of {len_sim} row(s) missing, first at \
         {iy:04}-{id:03} {it:02}:{imin:02}; provide observed qf in W m-2, or set values \
         to 0 to exclude QF from the energy balance)"
    ))
}

/// Validate a loaded or interpolated forcing table against physics switches
/// enforced by the standalone engine.
pub fn validate_forcing_for_config(
    forcing: &ForcingData,
    config: &SuewsConfig,
) -> Result<(), String> {
    validate_forcing_block_for_emissions(
        &forcing.block,
        forcing.len_sim,
        forcing.stride,
        config.emissions_method,
    )
}

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
        for suffix in var.allowed_suffixes {
            let canonical = format!("{}_{}", var.prefix, suffix);
            let accepted = FIELD_DESCRIPTORS.iter().any(|descriptor| {
                descriptor
                    .csv_names
                    .iter()
                    .any(|name| name.eq_ignore_ascii_case(&lowered))
                    && descriptor
                        .csv_names
                        .iter()
                        .any(|name| name.eq_ignore_ascii_case(&canonical))
            });
            if accepted {
                return Some(canonical);
            }
        }
    }

    None
}

fn per_landcover_fields(prefix: &str) -> Vec<SuewsField> {
    let canonical_prefix = format!("{prefix}_");
    FIELD_DESCRIPTORS
        .iter()
        .filter(|descriptor| {
            descriptor
                .csv_names
                .iter()
                .any(|name| name.starts_with(&canonical_prefix))
        })
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

    // An entirely missing legacy `xsmd` placeholder is read as missing `sm`
    // for modelled runs (deprecated; refused from forcing contract 3.0.0).
    // Any legacy column carrying values still needs the converter.
    let legacy: Vec<String> = ["xsmd", "smd"]
        .iter()
        .filter(|name| col_idx.contains_key(**name))
        .map(|name| name.to_string())
        .collect();
    if !legacy.is_empty() {
        if col_idx.contains_key("sm") || legacy.len() > 1 {
            return Err("Both legacy and canonical soil-moisture inputs were supplied; select one contract before conversion.".into());
        }
        let idx = col_idx[&legacy[0]];
        let all_missing = lines.clone().all(|line| {
            line.split_whitespace()
                .nth(idx)
                .and_then(|token| token.parse::<f64>().ok())
                .is_none_or(|value| value.is_nan() || value == -999.0)
        });
        if !all_missing {
            return Err("Legacy soil-moisture input requires explicit migration; xsmd is not canonical sm".into());
        }
        eprintln!(
            "warning: forcing column '{}' is deprecated; an entirely missing placeholder is read as missing 'sm' for modelled runs only and will be refused from forcing contract 3.0.0",
            legacy[0]
        );
        col_idx.remove(&legacy[0]);
        col_idx.insert("sm".to_string(), idx);
    }

    // Baseline-required columns must all be present (case-insensitive).
    for d in FIELD_DESCRIPTORS.iter().filter(|d| d.required) {
        let found = d.csv_names.iter().any(|name| {
            col_idx.contains_key(&name.to_ascii_lowercase())
        });

        if !found {
            return Err(format!(
                "forcing header missing required column for `{}` (accepted names: {})",
                d.field.name(),
                d.csv_names.join(", ")
            ));
        }
    }

    // Accepted canonical forcing columns that the kernel block does not consume.
    let unused_canonical = ["wdir"];

    let iy_col = find_col(&col_idx, "iy")?;
    let id_col = find_col(&col_idx, "id")?;
    let it_col = find_col(&col_idx, "it")?;
    let imin_col = find_col(&col_idx, "imin")?;

    // Identify per-landcover columns up-front so we can pre-allocate Vec<f64>.
    let is_canonical = |name: &str| {
        let lowered = name.to_ascii_lowercase();

        // time columns
        if DATETIME_COLUMNS
            .iter()
            .any(|c| c.eq_ignore_ascii_case(name))
        {
            return true;
        }

        // explicitly ignored / legacy-but-accepted canonical columns
        if unused_canonical
            .iter()
            .any(|c| c.eq_ignore_ascii_case(name))
        {
            return true;
        }

        // per-landcover bulk variables (lai, wuh, etc.)
        if PER_LANDCOVER_FORCING_VARS.iter().any(|var| {
            lowered == var.prefix
                || lowered.starts_with(&format!("{}_", var.prefix))
        }) {
            return true;
        }

        // normal canonical fields (with aliases)
        FIELD_DESCRIPTORS.iter().any(|d| {
            d.csv_names.iter().any(|n| {
                n.eq_ignore_ascii_case(name)
            })
        })
    };

    let mut extras_targets: Vec<(String, usize)> = Vec::new();
    for (idx, header) in headers.iter().enumerate() {
        let lowered = header.trim_start_matches('%').to_ascii_lowercase();
        if let Some(canonical) = is_per_landcover(&lowered) {
            extras_targets.push((canonical, idx));
            continue;
        }
        if is_canonical(&lowered) || legacy.contains(&lowered) {
            continue;
        }
        eprintln!("warning: forcing column '{header}' is not canonical; ignored (gh#1372)");
    }
    let mut extras: HashMap<String, Vec<f64>> = HashMap::new();
    for (name, _) in &extras_targets {
        extras.insert(name.clone(), Vec::new());
    }

    // A bulk water-use column fills any surface without its own column,
    // whichever accepted spelling (`wuh` or `wu_mm`) the file uses.
    let wuh_bulk_names =
        descriptor_by_index(SuewsField::wu_mm.index()).map_or(&[][..], |d| d.csv_names);

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
                &[],
            )?;
        }
        for field in per_landcover_fields("lai") {
            apply_field(
                &mut row,
                field,
                &col_idx,
                &parts,
                line_no,
                &["lai"],
            )?;
        }

        for field in per_landcover_fields("wuh") {
            apply_field(
                &mut row,
                field,
                &col_idx,
                &parts,
                line_no,
                wuh_bulk_names,
            )?;
        }

        // This direct reader has no site geometry and cannot derive a deficit.
        // Refuse raw content rather than placing it into the kernel xsmd slot.
        let content = row[SuewsField::xsmd.index()];
        if content != FORCING_OPTIONAL_FILL && !content.is_nan() {
            return Err("Raw sm execution requires Python soil-content preparation with site metadata".into());
        }
        row[SuewsField::xsmd.index()] = FORCING_OPTIONAL_FILL;
        validate_wuh_fields(&row, line_no)?;

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
    fallback: &[&str],
) -> Result<(), String> {
    let desc = FIELD_DESCRIPTORS
        .iter()
        .find(|d| d.field == field)
        .ok_or_else(|| format!("unknown field {:?}", field))?;

    let source = desc.csv_names.iter()
        .find_map(|name| {
            col_idx
                .get(&name.to_ascii_lowercase())
                .copied()
                .map(|idx| (*name, idx))
        })
        .or_else(|| {
            fallback.iter().find_map(|fb| {
                col_idx
                    .get(&fb.to_ascii_lowercase())
                    .copied()
                    .map(|idx| (*fb, idx))
            })
        });

    if let Some((name, idx)) = source {
        row[field.index()] = parse_f64(parts[idx], line_no, name)?;
    } else if desc.required {
        return Err(format!(
            "missing required column for `{}`",
            desc.field.name()
        ));
    }

    Ok(())
}

fn validate_wuh_fields(row: &[f64], line_no: usize) -> Result<(), String> {
    let fields = std::iter::once(SuewsField::wu_mm).chain(per_landcover_fields("wuh"));
    for field in fields {
        let value = row[field.index()];
        if value != FORCING_OPTIONAL_FILL && (!value.is_finite() || value < 0.0) {
            return Err(format!(
                "forcing value for `{}` at line {line_no} must be finite and non-negative or exact {FORCING_OPTIONAL_FILL}",
                field.name()
            ));
        }
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
    // point): output times at or after t_in[N-2] take the second-to-last
    // value.  This matches the Python resample_linear_inst behaviour which
    // sets t_end to NaN before interpolation.
    // Missing sentinels (<= -900 or non-finite) act as barriers: never blend
    // a sentinel into an adjacent valid observation, and preserve exact valid
    // knots without poisoning them from a missing neighbour. Non-knot
    // intervals keep the exact `offset / tstep_in - i` floating-point
    // arithmetic so valid series stay bit-for-bit identical.
    let t_in_0 = t_in[0];
    let t_in_penult = t_in[n_in - 2];
    for &col in &groups.inst {
        for j in 0..n_out {
            let t = t_out_start + j as i64 * tstep_mod_i64;
            let v = if t <= t_in_0 {
                sanitize_forcing_value(row_val(forcing, 0, col))
            } else if t >= t_in_penult {
                sanitize_forcing_value(row_val(forcing, n_in - 2, col))
            } else {
                let offset_i64 = t - t_in_0;
                if offset_i64 % tstep_in_i64 == 0 {
                    let i = ((offset_i64 / tstep_in_i64) as usize).min(n_in - 2);
                    sanitize_forcing_value(row_val(forcing, i, col))
                } else {
                    let offset = offset_i64 as f64;
                    let i_float = offset / tstep_in_f64;
                    let i = (i_float as usize).min(n_in - 2);
                    let alpha = i_float - i as f64;
                    let v0 = row_val(forcing, i, col);
                    let v1 = row_val(forcing, i + 1, col);
                    interpolate_pair(v0, v1, alpha)
                }
            };
            block[j * forcing.stride + col] = v;
        }
    }

    // --- Average: shift input by -tstep_in/2, then linear interpolation ---
    // Shifted times represent period midpoints. Backfill before first,
    // forward-fill after last, and preserve missing sentinels as barriers.
    // Work in doubled seconds so half-step midpoint checks stay exact even for
    // odd `tstep_in`, while keeping `offset / tstep_in - i` floating-point
    // arithmetic on non-knot intervals (for even `tstep_in`, doubling both
    // numerator and denominator alters only the IEEE-754 exponent, leaving
    // `i_float` bit-identical to `(t - t_shifted_0) as f64 / tstep_in_f64`).
    let t2_shifted_0 = 2 * t_in_0 - tstep_in_i64;
    let t2_shifted_last = 2 * t_in[n_in - 1] - tstep_in_i64;
    let step2 = 2 * tstep_in_i64;
    let step2_f64 = step2 as f64;
    for &col in &groups.avg {
        for j in 0..n_out {
            let t2 = 2 * (t_out_start + j as i64 * tstep_mod_i64);
            let v = if t2 <= t2_shifted_0 {
                sanitize_forcing_value(row_val(forcing, 0, col))
            } else if t2 >= t2_shifted_last {
                sanitize_forcing_value(row_val(forcing, n_in - 1, col))
            } else {
                let offset2 = t2 - t2_shifted_0;
                if offset2 % step2 == 0 {
                    let i = ((offset2 / step2) as usize).min(n_in - 2);
                    sanitize_forcing_value(row_val(forcing, i, col))
                } else {
                    let i_float = offset2 as f64 / step2_f64;
                    let i = (i_float as usize).min(n_in - 2);
                    let alpha = i_float - i as f64;
                    let v0 = row_val(forcing, i, col);
                    let v1 = row_val(forcing, i + 1, col);
                    interpolate_pair(v0, v1, alpha)
                }
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
            block[j * forcing.stride + col] = if is_missing_forcing_value(v_in) {
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
        let path = Path::new("../../test/fixtures/soil_observation/modelled_content_short.txt");
        let forcing = read_forcing_block(path).expect("fixture forcing should parse");
        assert!(forcing.len_sim > 10);
        assert_eq!(forcing.block.len(), forcing.len_sim * FORCING_BLOCK_STRIDE);
        assert_eq!(forcing.block[0], 2011.0);
        assert_eq!(forcing.block[1], 1.0);
        assert_eq!(forcing.block[2], 0.0);
        assert_eq!(forcing.block[3], 5.0);
    }

    #[test]
    fn soil_content_cannot_enter_deficit_slot() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("soil.txt");
        for (header, value, expected) in [
            ("sm", "50", "Raw sm execution"),
            ("sm", "inf", "Raw sm execution"),
            ("xsmd", "5", "explicit migration"),
        ] {
            fs::write(&path, format!(
                "iy id it imin Tair RH U pres kdown rain {header}\n2012 1 1 0 10 50 2 101.3 100 0 {value}\n"
            )).unwrap();
            assert!(read_forcing_block(&path).unwrap_err().contains(expected));
        }
    }

    #[test]
    fn missing_legacy_placeholder_reads_as_missing_sm() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("legacy.txt");
        fs::write(&path,
            "iy id it imin Tair RH U pres kdown rain xsmd\n2012 1 1 0 10 50 2 101.3 100 0 -999\n2012 1 1 5 10 50 2 101.3 100 0 -999\n"
        ).unwrap();
        let forcing = read_forcing_block(&path).expect("missing xsmd placeholder should parse");
        assert_eq!(forcing.len_sim, 2);

        fs::write(&path,
            "iy id it imin Tair RH U pres kdown rain xsmd sm\n2012 1 1 0 10 50 2 101.3 100 0 -999 -999\n"
        ).unwrap();
        assert!(read_forcing_block(&path).unwrap_err().contains("Both legacy and canonical"));
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
        block[SuewsField::wu_mm.index()] = 12.0; // bulk water use (sum)
        block[SuewsField::wu_mm_paved.index()] = -999.0; // exact missing sentinel

        // Row 1: (2012, 1, 2, 0), U=20
        let r1 = FORCING_BLOCK_STRIDE;
        block[r1] = 2012.0;
        block[r1 + 1] = 1.0;
        block[r1 + 2] = 2.0;
        block[r1 + 3] = 0.0;
        block[r1 + SuewsField::u.index()] = 20.0;
        block[r1 + SuewsField::kdown.index()] = 200.0;
        block[r1 + SuewsField::rain.index()] = 24.0;
        block[r1 + SuewsField::wu_mm.index()] = 24.0;
        block[r1 + SuewsField::wu_mm_paved.index()] = -999.0;
        // Row 2: (2012, 1, 3, 0), U=30
        let r2 = 2 * FORCING_BLOCK_STRIDE;
        block[r2] = 2012.0;
        block[r2 + 1] = 1.0;
        block[r2 + 2] = 3.0;
        block[r2 + 3] = 0.0;
        block[r2 + SuewsField::u.index()] = 30.0;
        block[r2 + SuewsField::kdown.index()] = 300.0;
        block[r2 + SuewsField::rain.index()] = 0.0;
        block[r2 + SuewsField::wu_mm.index()] = -999.0;
        block[r2 + SuewsField::wu_mm_paved.index()] = -999.0;

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
        assert_eq!(row_val(&result, 11, SuewsField::u.index()), 10.0);
        // Row 23 = t_in[1] = (2012,1,2,0): U=20
        assert_eq!(row_val(&result, 23, SuewsField::u.index()), 20.0);
        // Row 17 = midpoint between t_in[0] and t_in[1]: U=15
        assert!((row_val(&result, 17, SuewsField::u.index()) - 15.0).abs() < 0.01);

        // Sum: rain is redistributed. Row 0's input rain=12.0, scale=1/12=1.0
        assert!((row_val(&result, 0, SuewsField::rain.index()) - 1.0).abs() < 1e-9);
        assert!((row_val(&result, 11, SuewsField::rain.index()) - 1.0).abs() < 1e-9);
        // Row 12 maps to input row 1: rain=24.0 → 2.0
        assert!((row_val(&result, 12, SuewsField::rain.index()) - 2.0).abs() < 1e-9);
        // Bulk Wuh uses the same sum redistribution as rain.
        assert!((row_val(&result, 0, SuewsField::wu_mm.index()) - 1.0).abs() < 1e-9);
        assert!((row_val(&result, 11, SuewsField::wu_mm.index()) - 1.0).abs() < 1e-9);
        assert!((row_val(&result, 12, SuewsField::wu_mm.index()) - 2.0).abs() < 1e-9);
        // Exact bulk and surface missing values stay missing instead of
        // becoming a flux.
        assert_eq!(row_val(&result, 24, SuewsField::wu_mm.index()), -999.0);
        assert_eq!(row_val(&result, 0, SuewsField::wu_mm_paved.index()), -999.0);
        assert_eq!(row_val(&result, 12, SuewsField::wu_mm_paved.index()), -999.0);
    }

    #[test]
    fn interpolate_midnight_borrows_previous_year() {
        for (year, previous_days) in [(2012, 365), (2013, 366), (2001, 366), (1901, 365)] {
            let mut block = vec![0.0; 2 * FORCING_BLOCK_STRIDE];
            for row in 0..2 {
                let start = row * FORCING_BLOCK_STRIDE;
                block[start..start + 4].copy_from_slice(&[year as f64, 1.0, row as f64, 0.0]);
            }
            let forcing = ForcingData {
                block,
                len_sim: 2,
                extras: HashMap::new(),
                stride: FORCING_BLOCK_STRIDE,
            };
            let result = interpolate_forcing(&forcing, 300).unwrap();
            assert_eq!(
                &result.block[..4],
                &[(year - 1) as f64, previous_days as f64, 23.0, 5.0]
            );
            let midnight = 11 * FORCING_BLOCK_STRIDE;
            assert_eq!(
                &result.block[midnight..midnight + 4],
                &[year as f64, 1.0, 0.0, 0.0]
            );
        }
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
            Path::new("../../test/fixtures/soil_observation/modelled_content.txt");
        let shuffled = Path::new("../../test/fixtures/soil_observation/content_shuffled.txt");
        let a = read_forcing_block(canonical).expect("canonical");
        let b = read_forcing_block(shuffled).expect("shuffled");
        assert_eq!(a.len_sim, b.len_sim);
        assert_eq!(a.block, b.block);
    }

    #[test]
    fn file_aliases_yield_identical_blocks_and_interpolation() {
        use std::io::Write;

        let dir = tempfile::tempdir().unwrap();
        let canonical_path = dir.path().join("canonical.txt");
        let alias_path = dir.path().join("aliases.txt");
        let mut canonical = std::fs::File::create(&canonical_path).unwrap();
        let mut aliases = std::fs::File::create(&alias_path).unwrap();

        writeln!(
            canonical,
            "iy id it imin qn qs qf U RH Tair pres rain kdown snow Wuh wuh_paved"
        )
        .unwrap();
        writeln!(
            aliases,
            "iy id it imin qn1_obs qs_obs qf_obs U RH temp_c pres rain kdown snowfrac wu_mm wu_mm_paved"
        )
        .unwrap();
        for row in [
            "2011 1 1 0 100 10 5 2 70 18 101.3 12 200 0 6 1",
            "2011 1 2 0 120 12 6 4 72 20 101.2 24 240 0 8 2",
        ] {
            writeln!(canonical, "{row}").unwrap();
            writeln!(aliases, "{row}").unwrap();
        }

        let canonical = read_forcing_block(&canonical_path).expect("canonical");
        let aliases = read_forcing_block(&alias_path).expect("aliases");
        assert_eq!(aliases.block, canonical.block);
        assert_eq!(row_val(&aliases, 0, SuewsField::pres.index()), 1013.0);
        assert_eq!(aliases.extras, canonical.extras);
        assert_eq!(aliases.extras["wuh_paved"], vec![1.0, 2.0]);

        let canonical_interp = interpolate_forcing(&canonical, 300).unwrap();
        let alias_interp = interpolate_forcing(&aliases, 300).unwrap();
        assert_eq!(alias_interp.block, canonical_interp.block);
    }

    #[test]
    fn accessor_alias_does_not_satisfy_baseline_file_requirement() {
        use std::io::Write;

        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("accessor-alias.txt");
        let mut file = std::fs::File::create(&path).unwrap();
        writeln!(file, "iy id it imin U RH temperature pres rain kdown").unwrap();
        writeln!(file, "2011 1 0 5 2 70 18 101.3 0 200").unwrap();

        let error = read_forcing_block(&path).expect_err("temperature is not a file alias");
        assert!(
            error.contains("tair") || error.contains("Tair"),
            "error: {error}"
        );
    }

    #[test]
    fn per_landcover_columns_loaded_into_extras() {
        let path = Path::new("../../test/fixtures/soil_observation/content_per_landcover.txt");
        let forcing = read_forcing_block(path).expect("per-landcover fixture");
        assert!(forcing.extras.contains_key("lai_evetr"));
        assert!(forcing.extras.contains_key("wuh_paved"));
        assert_eq!(forcing.extras["lai_evetr"].len(), forcing.len_sim);
        assert_eq!(row_val(&forcing, 0, SuewsField::lai_evetr.index()), forcing.extras["lai_evetr"][0]);
        assert_eq!(row_val(&forcing, 0, SuewsField::lai_dectr.index()), forcing.extras["lai_dectr"][0]);
        assert_eq!(row_val(&forcing, 0, SuewsField::lai_grass.index()), forcing.extras["lai_grass"][0]);
        assert_eq!(row_val(&forcing, 0, SuewsField::wu_mm_paved.index()), forcing.extras["wuh_paved"][0]);
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
    fn bulk_wuh_fills_only_missing_surface_columns() {
        use std::io::Write;

        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("bulk-wuh.txt");
        let mut file = std::fs::File::create(&path).unwrap();
        writeln!(
            file,
            "iy id it imin Tair RH U pres kdown rain Wuh wuh_paved wuh_grass"
        )
        .unwrap();
        writeln!(file, "2011 1 0 5 18 80 2 100 0 0 2.5 1.0 -999").unwrap();

        let forcing = read_forcing_block(&path).expect("bulk Wuh fixture");

        assert_eq!(row_val(&forcing, 0, SuewsField::wu_mm_paved.index()), 1.0);
        assert_eq!(row_val(&forcing, 0, SuewsField::wu_mm_bldgs.index()), 2.5);
        assert_eq!(row_val(&forcing, 0, SuewsField::wu_mm_evetr.index()), 2.5);
        assert_eq!(row_val(&forcing, 0, SuewsField::wu_mm_dectr.index()), 2.5);
        assert_eq!(row_val(&forcing, 0, SuewsField::wu_mm_grass.index()), -999.0);
        assert_eq!(row_val(&forcing, 0, SuewsField::wu_mm_bsoil.index()), 2.5);
        assert_eq!(row_val(&forcing, 0, SuewsField::wu_mm_water.index()), 2.5);
    }

    #[test]
    fn invalid_wuh_values_are_rejected_at_reader_boundary() {
        use std::io::Write;

        for invalid in ["-950", "NaN", "inf"] {
            let dir = tempfile::tempdir().unwrap();

            let bulk_path = dir.path().join("invalid-bulk-wuh.txt");
            let mut bulk = std::fs::File::create(&bulk_path).unwrap();
            writeln!(bulk, "iy id it imin Tair RH U pres kdown rain Wuh").unwrap();
            writeln!(bulk, "2011 1 0 5 18 80 2 100 0 0 {invalid}").unwrap();
            let error = read_forcing_block(&bulk_path).expect_err("invalid bulk Wuh");
            assert!(error.contains("wu_mm"), "error: {error}");

            let surface_path = dir.path().join("invalid-surface-wuh.txt");
            let mut surface = std::fs::File::create(&surface_path).unwrap();
            writeln!(
                surface,
                "iy id it imin Tair RH U pres kdown rain Wuh wuh_paved"
            )
            .unwrap();
            writeln!(surface, "2011 1 0 5 18 80 2 100 0 0 2 {invalid}").unwrap();
            let error =
                read_forcing_block(&surface_path).expect_err("invalid surface Wuh");
            assert!(error.contains("wu_mm_paved"), "error: {error}");
        }
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

    fn make_hourly_forcing(n_hours: usize) -> ForcingData {
        let mut block = vec![FORCING_OPTIONAL_FILL; n_hours * FORCING_BLOCK_STRIDE];
        for row in 0..n_hours {
            let base = row * FORCING_BLOCK_STRIDE;
            block[base] = 2012.0;
            block[base + 1] = 1.0;
            block[base + 2] = (row + 1) as f64;
            block[base + 3] = 0.0;
        }
        ForcingData {
            block,
            len_sim: n_hours,
            extras: HashMap::new(),
            stride: FORCING_BLOCK_STRIDE,
        }
    }

    fn set_col(forcing: &mut ForcingData, field: SuewsField, values: &[f64]) {
        assert_eq!(values.len(), forcing.len_sim);
        let col = field.index();
        for (row, &value) in values.iter().enumerate() {
            forcing.block[row * forcing.stride + col] = value;
        }
    }

    fn col_values(forcing: &ForcingData, field: SuewsField) -> Vec<f64> {
        let col = field.index();
        (0..forcing.len_sim)
            .map(|row| row_val(forcing, row, col))
            .collect()
    }

    #[test]
    fn interpolate_inst_never_blends_missing_sentinels() {
        // 5 hourly rows (1:00..5:00) -> 60 five-minute rows (0:05..5:00).
        // Input knots used by inst are rows 0..=3 (1:00..4:00); row 4 (5:00) is
        // the boundary marker.
        // Indices on 5-min grid: 1:00=11, 2:00=23, 3:00=35, 4:00=47, 5:00=59.
        let mut forcing = make_hourly_forcing(5);
        // Interior -999 at row 1 (2:00): [10, -999, 30, 40, 50]
        set_col(
            &mut forcing,
            SuewsField::u,
            &[10.0, -999.0, 30.0, 40.0, 50.0],
        );
        // Leading -999 at row 0 (1:00): [-999, 20, 30, 40, 50]
        set_col(
            &mut forcing,
            SuewsField::rh,
            &[-999.0, 20.0, 30.0, 40.0, 50.0],
        );
        // Trailing penultimate -999 at row 3 (4:00): [10, 20, 30, -999, 50]
        set_col(
            &mut forcing,
            SuewsField::temp_c,
            &[10.0, 20.0, 30.0, -999.0, 50.0],
        );
        // Boundary marker row 4 is -999, while active knots 0..=3 are valid.
        set_col(
            &mut forcing,
            SuewsField::fcld,
            &[0.1, 0.2, 0.3, 0.4, -999.0],
        );
        // Defensive threshold (-950) and NaN / inf behave identically to -999.
        set_col(
            &mut forcing,
            SuewsField::lai_evetr,
            &[1.0, -950.0, 3.0, f64::NAN, 5.0],
        );
        // All-missing column stays exact -999.
        set_col(
            &mut forcing,
            SuewsField::lai_dectr,
            &[-999.0, -999.0, -999.0, -999.0, -999.0],
        );

        let result = interpolate_forcing(&forcing, 300).unwrap();
        assert_eq!(result.len_sim, 60);

        // Interior -999 on U:
        // 0:05..=1:00 (j=0..=11) backfills valid 10.0.
        // (1:00, 3:00) (j=12..=34) touches row 1 (-999) -> exact -999.0, never blended.
        // 3:00 (j=35) is exact valid knot 30.0.
        // (3:00, 4:00) (j=36..=46) interpolates linearly 30 -> 40.
        // >= 4:00 (j=47..=59) forward-fills 40.0.
        let u = col_values(&result, SuewsField::u);
        for &v in &u[0..=11] {
            assert_eq!(v, 10.0);
        }
        for &v in &u[12..=34] {
            assert_eq!(v, FORCING_OPTIONAL_FILL, "must not blend -999 into U");
        }
        assert_eq!(u[35], 30.0);
        assert!((u[41] - 35.0).abs() < 1e-12);
        for &v in &u[47..=59] {
            assert_eq!(v, 40.0);
        }

        // Leading -999 on RH:
        // 0:05..1:55 (j=0..=22) -> -999.0; 2:00 (j=23) -> 20.0; 2:30 (j=29) -> 25.0.
        let rh = col_values(&result, SuewsField::rh);
        for &v in &rh[0..=22] {
            assert_eq!(v, FORCING_OPTIONAL_FILL);
        }
        assert_eq!(rh[23], 20.0);
        assert!((rh[29] - 25.0).abs() < 1e-12);
        assert_eq!(rh[35], 30.0);
        assert!((rh[41] - 35.0).abs() < 1e-12);
        for &v in &rh[47..=59] {
            assert_eq!(v, 40.0);
        }

        // Trailing penultimate -999 on Tair:
        // 0:05..=3:00 (j=0..=35) valid (10..30), >3:00 (j=36..=59) -> -999.0.
        let tair = col_values(&result, SuewsField::temp_c);
        assert_eq!(tair[11], 10.0);
        assert!((tair[17] - 15.0).abs() < 1e-12);
        assert_eq!(tair[23], 20.0);
        assert!((tair[29] - 25.0).abs() < 1e-12);
        assert_eq!(tair[35], 30.0);
        for &v in &tair[36..=59] {
            assert_eq!(v, FORCING_OPTIONAL_FILL);
        }

        // Boundary marker row 4 (-999) on fcld must NOT poison active knots 0..=3:
        let fcld = col_values(&result, SuewsField::fcld);
        assert_eq!(fcld[11], 0.1);
        assert!((fcld[17] - 0.15).abs() < 1e-12);
        assert_eq!(fcld[35], 0.3);
        assert!((fcld[41] - 0.35).abs() < 1e-12);
        for &v in &fcld[47..=59] {
            assert_eq!(v, 0.4);
        }

        // Defensive threshold (-950) and NaN on lai_evetr sanitize to -999.0:
        let lai = col_values(&result, SuewsField::lai_evetr);
        for &v in &lai[0..=11] {
            assert_eq!(v, 1.0);
        }
        for &v in &lai[12..=34] {
            assert_eq!(v, FORCING_OPTIONAL_FILL);
        }
        assert_eq!(lai[35], 3.0);
        for &v in &lai[36..=59] {
            assert_eq!(v, FORCING_OPTIONAL_FILL);
        }

        // All-missing stays exact -999.0:
        let lai_dectr = col_values(&result, SuewsField::lai_dectr);
        assert!(lai_dectr.iter().all(|&v| v == FORCING_OPTIONAL_FILL));
    }

    #[test]
    fn interpolate_avg_never_blends_missing_sentinels() {
        // 4 hourly rows (1:00..4:00) -> 48 five-minute rows (0:05..4:00).
        // Shifted avg knots are 0:30 (j=5), 1:30 (j=17), 2:30 (j=29), 3:30 (j=41).
        let mut forcing = make_hourly_forcing(4);
        // Interior -999 at row 1 (1:30): [100, -999, 300, 400]
        set_col(
            &mut forcing,
            SuewsField::qf_obs,
            &[100.0, -999.0, 300.0, 400.0],
        );
        // Leading -999 at row 0 (0:30): [-999, 200, 300, 400]
        set_col(
            &mut forcing,
            SuewsField::ldown,
            &[-999.0, 200.0, 300.0, 400.0],
        );
        // Trailing -999 at row 3 (3:30): [100, 200, 300, -999]
        set_col(
            &mut forcing,
            SuewsField::qn1_obs,
            &[100.0, 200.0, 300.0, -999.0],
        );
        // Defensive threshold (-950) and +inf: [100, -950, 300, inf]
        set_col(
            &mut forcing,
            SuewsField::qs_obs,
            &[100.0, -950.0, 300.0, f64::INFINITY],
        );
        // Valid parity reference: [100, 200, 300, 400]
        set_col(
            &mut forcing,
            SuewsField::kdown,
            &[100.0, 200.0, 300.0, 400.0],
        );
        // Sum columns also sanitise defensive threshold (-950) and non-finite inputs.
        set_col(
            &mut forcing,
            SuewsField::rain,
            &[12.0, -950.0, f64::NAN, -999.0],
        );

        let result = interpolate_forcing(&forcing, 300).unwrap();
        assert_eq!(result.len_sim, 48);

        // Interior -999 on qf:
        // 0:05..=0:30 (j=0..=5) -> 100.0
        // (0:30, 2:30) (j=6..=28) -> -999.0 (never blended)
        // 2:30 (j=29) -> 300.0 (exact valid shifted knot)
        // (2:30, 3:30) (j=30..=40) -> linear 300..400 (midpoint 3:00 j=35 -> 350.0)
        // >= 3:30 (j=41..=47) -> 400.0
        let qf = col_values(&result, SuewsField::qf_obs);
        for &v in &qf[0..=5] {
            assert_eq!(v, 100.0);
        }
        for &v in &qf[6..=28] {
            assert_eq!(v, FORCING_OPTIONAL_FILL, "must not blend -999 into qf");
        }
        assert_eq!(qf[29], 300.0);
        assert!((qf[35] - 350.0).abs() < 1e-12);
        for &v in &qf[41..=47] {
            assert_eq!(v, 400.0);
        }

        // Leading -999 on ldown:
        // 0:05..1:25 (j=0..=16) -> -999.0; 1:30 (j=17) -> 200.0; 2:00 (j=23) -> 250.0.
        let ldown = col_values(&result, SuewsField::ldown);
        for &v in &ldown[0..=16] {
            assert_eq!(v, FORCING_OPTIONAL_FILL);
        }
        assert_eq!(ldown[17], 200.0);
        assert!((ldown[23] - 250.0).abs() < 1e-12);
        assert_eq!(ldown[29], 300.0);
        assert!((ldown[35] - 350.0).abs() < 1e-12);
        for &v in &ldown[41..=47] {
            assert_eq!(v, 400.0);
        }

        // Trailing -999 on qn:
        // 0:05..=2:30 (j=0..=29) valid; >2:30 (j=30..=47) -> -999.0.
        let qn = col_values(&result, SuewsField::qn1_obs);
        for &v in &qn[0..=5] {
            assert_eq!(v, 100.0);
        }
        assert!((qn[11] - 150.0).abs() < 1e-12);
        assert_eq!(qn[17], 200.0);
        assert!((qn[23] - 250.0).abs() < 1e-12);
        assert_eq!(qn[29], 300.0);
        for &v in &qn[30..=47] {
            assert_eq!(v, FORCING_OPTIONAL_FILL);
        }

        // Defensive threshold (-950) and +inf on qs:
        let qs = col_values(&result, SuewsField::qs_obs);
        for &v in &qs[0..=5] {
            assert_eq!(v, 100.0);
        }
        for &v in &qs[6..=28] {
            assert_eq!(v, FORCING_OPTIONAL_FILL);
        }
        assert_eq!(qs[29], 300.0);
        for &v in &qs[30..=47] {
            assert_eq!(v, FORCING_OPTIONAL_FILL);
        }

        // Valid parity on kdown:
        let kdown = col_values(&result, SuewsField::kdown);
        for &v in &kdown[0..=5] {
            assert_eq!(v, 100.0);
        }
        for step in 1..12 {
            let expected = 100.0 + 100.0 * (step as f64 / 12.0);
            assert!((kdown[5 + step] - expected).abs() < 1e-12);
        }
        assert_eq!(kdown[17], 200.0);
        assert!((kdown[23] - 250.0).abs() < 1e-12);
        assert_eq!(kdown[29], 300.0);
        assert!((kdown[35] - 350.0).abs() < 1e-12);
        for &v in &kdown[41..=47] {
            assert_eq!(v, 400.0);
        }

        // Sum column defensive threshold (-950) and NaN stay exact -999.0 (never scaled).
        let rain = col_values(&result, SuewsField::rain);
        for &v in &rain[0..12] {
            assert!((v - 1.0).abs() < 1e-12);
        }
        for &v in &rain[12..48] {
            assert_eq!(v, FORCING_OPTIONAL_FILL);
        }
    }

    #[test]
    fn interpolate_avg_odd_ratio_midpoints_stay_exact() {
        // 15-min (900s) input -> 5-min (300s) model (ratio = 3, midpoint at +450s
        // falls strictly between 5-min output timestamps).
        let mut block = vec![FORCING_OPTIONAL_FILL; 3 * FORCING_BLOCK_STRIDE];
        for (row, imin) in [(0, 15.0), (1, 30.0), (2, 45.0)] {
            let base = row * FORCING_BLOCK_STRIDE;
            block[base] = 2012.0;
            block[base + 1] = 1.0;
            block[base + 2] = 0.0;
            block[base + 3] = imin;
            block[base + SuewsField::kdown.index()] = (row as f64 + 1.0) * 90.0;
        }
        let forcing = ForcingData {
            block,
            len_sim: 3,
            extras: HashMap::new(),
            stride: FORCING_BLOCK_STRIDE,
        };
        let result = interpolate_forcing(&forcing, 300).unwrap();
        assert_eq!(result.len_sim, 9);
        // Output times: 0:05, 0:10, 0:15, 0:20, 0:25, 0:30, 0:35, 0:40, 0:45.
        // Shifted knots: 0:07:30 (90), 0:22:30 (180), 0:37:30 (270).
        let kdown = col_values(&result, SuewsField::kdown);
        let expected = [90.0, 105.0, 135.0, 165.0, 195.0, 225.0, 255.0, 270.0, 270.0];
        for (idx, (&actual, &exp)) in kdown.iter().zip(expected.iter()).enumerate() {
            assert!(
                (actual - exp).abs() < 1e-12,
                "idx {idx}: expected {exp}, got {actual}"
            );
        }
    }

    #[test]
    fn interpolate_valid_48h_series_is_bit_exact_with_master_formula() {
        // 49 hourly rows (48h + boundary) -> 588 five-minute rows.
        // Non-trivial decimals over >24h expose 1-ULP drift if non-knot alpha
        // switches from `(offset / tstep) - i` to `(offset % tstep) / tstep`.
        let n_in = 49usize;
        let tstep_in = 3600i64;
        let tstep_in_f64 = tstep_in as f64;
        let tstep_mod = 300i64;
        let mut block = vec![FORCING_OPTIONAL_FILL; n_in * FORCING_BLOCK_STRIDE];
        let mut tair_in = Vec::with_capacity(n_in);
        let mut kdown_in = Vec::with_capacity(n_in);
        for row in 0..n_in {
            let total_hours = row + 1;
            let day = 1 + (total_hours / 24);
            let hour = total_hours % 24;
            let base = row * FORCING_BLOCK_STRIDE;
            block[base] = 2012.0;
            block[base + 1] = day as f64;
            block[base + 2] = hour as f64;
            block[base + 3] = 0.0;
            let tair = 11.77 + 0.13 * (row as f64) - 0.007 * ((row * row) as f64);
            let kdown = 14.75 + 7.19 * (row as f64) + 0.031 * ((row * row) as f64);
            block[base + SuewsField::temp_c.index()] = tair;
            block[base + SuewsField::kdown.index()] = kdown;
            tair_in.push(tair);
            kdown_in.push(kdown);
        }
        let forcing = ForcingData {
            block,
            len_sim: n_in,
            extras: HashMap::new(),
            stride: FORCING_BLOCK_STRIDE,
        };
        let result = interpolate_forcing(&forcing, tstep_mod as i32).unwrap();
        let n_out = n_in * 12;
        assert_eq!(result.len_sim, n_out);

        let tair_out = col_values(&result, SuewsField::temp_c);
        let kdown_out = col_values(&result, SuewsField::kdown);

        // Input times relative to t_in[0] = 3600: t_out[j] = (j + 1) * 300.
        let t_in_0 = tstep_in;
        let t_in_penult = t_in_0 + (n_in as i64 - 2) * tstep_in;
        let t_shifted_0 = t_in_0 - tstep_in / 2;
        let t_shifted_last = t_in_0 + (n_in as i64 - 1) * tstep_in - tstep_in / 2;

        for j in 0..n_out {
            let t = (j as i64 + 1) * tstep_mod;
            let expected_inst = if t <= t_in_0 {
                tair_in[0]
            } else if t > t_in_penult {
                tair_in[n_in - 2]
            } else {
                let offset = (t - t_in_0) as f64;
                let i_float = offset / tstep_in_f64;
                let i = (i_float as usize).min(n_in - 2);
                let alpha = i_float - i as f64;
                tair_in[i] + alpha * (tair_in[i + 1] - tair_in[i])
            };
            assert_eq!(
                tair_out[j].to_bits(),
                expected_inst.to_bits(),
                "inst bit drift at j={j}: got {}, expected {}",
                tair_out[j],
                expected_inst
            );

            let expected_avg = if t <= t_shifted_0 {
                kdown_in[0]
            } else if t >= t_shifted_last {
                kdown_in[n_in - 1]
            } else {
                let offset = (t - t_shifted_0) as f64;
                let i_float = offset / tstep_in_f64;
                let i = (i_float as usize).min(n_in - 2);
                let alpha = i_float - i as f64;
                kdown_in[i] + alpha * (kdown_in[i + 1] - kdown_in[i])
            };
            assert_eq!(
                kdown_out[j].to_bits(),
                expected_avg.to_bits(),
                "avg bit drift at j={j}: got {}, expected {}",
                kdown_out[j],
                expected_avg
            );
        }
    }

    #[test]
    fn validate_forcing_for_emissions_zero_rejects_missing_qf_and_accepts_valid() {
        let mut cfg = SuewsConfig {
            emissions_method: 0,
            ..Default::default()
        };

        // 1. All-missing qf (default FORCING_OPTIONAL_FILL = -999) is rejected.
        let missing_all = make_hourly_forcing(3);
        let err = validate_forcing_for_config(&missing_all, &cfg)
            .expect_err("all-missing qf must fail for emissions=0");
        assert!(err.contains("forcing column 'qf'"), "err: {err}");
        assert!(err.contains("emissions=0"), "err: {err}");
        assert!(err.contains("every row is missing"), "err: {err}");

        // 2. Partial missing qf (interior -999) fails both raw and after interpolation.
        let mut partial = make_hourly_forcing(3);
        set_col(&mut partial, SuewsField::qf_obs, &[20.0, -999.0, 30.0]);
        let err_raw = validate_forcing_for_config(&partial, &cfg)
            .expect_err("partial missing qf must fail before interpolation");
        assert!(err_raw.contains("1 of 3 row(s) missing"), "err: {err_raw}");
        assert!(err_raw.contains("2012-001 02:00"), "err: {err_raw}");

        let interp = interpolate_forcing(&partial, 300).unwrap();
        let err_interp = validate_forcing_for_config(&interp, &cfg)
            .expect_err("partial missing qf must remain rejected after interpolation");
        assert!(
            err_interp.contains("forcing column 'qf'"),
            "err: {err_interp}"
        );
        assert!(err_interp.contains("emissions=0"), "err: {err_interp}");

        // 3. Defensive threshold (-950) and NaN are rejected.
        for bad in [-950.0, f64::NAN, f64::INFINITY] {
            let mut bad_forcing = make_hourly_forcing(2);
            set_col(&mut bad_forcing, SuewsField::qf_obs, &[15.0, bad]);
            assert!(
                validate_forcing_for_config(&bad_forcing, &cfg).is_err(),
                "qf={bad} must fail for emissions=0"
            );
        }

        // 4. Valid observed qf (including 0.0 and physically valid negative flux) passes.
        let mut valid = make_hourly_forcing(3);
        set_col(&mut valid, SuewsField::qf_obs, &[0.0, 25.0, -5.0]);
        validate_forcing_for_config(&valid, &cfg)
            .expect("valid observed qf must pass for emissions=0");
        let valid_interp = interpolate_forcing(&valid, 300).unwrap();
        validate_forcing_for_config(&valid_interp, &cfg)
            .expect("interpolated valid observed qf must pass for emissions=0");

        // 5. Non-zero emissions methods do not require qf.
        for method in [2, 12] {
            cfg.emissions_method = method;
            validate_forcing_for_config(&missing_all, &cfg)
                .expect("modelled emissions should allow missing qf");
        }
    }
}
