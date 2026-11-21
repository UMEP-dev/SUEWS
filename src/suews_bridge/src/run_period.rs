//! Requested simulation period: parsing, row selection and forcing coverage
//! checks for resolved standalone file runs (`suews-engine run`).
//!
//! Mirrors `src/supy/_run_period.py` and `docs/source/inputs/timestamp-convention.rst`:
//!
//! - Forcing rows are stamped at the *end* of each interval: a row stamped `t`
//!   covers `(t - dt, t]`, so a calendar day `D` is `(D 00:00, D+1 00:00]`
//!   (rows `D 00:05` .. `D+1 00:00` at a 5-minute timestep).
//! - A **date-only** bound (`YYYY-MM-DD`) names a calendar day: `start_time`
//!   selects rows strictly after `D1 00:00`, and `end_time` selects rows up to
//!   and including `D2+1 00:00`.
//! - An **explicit timestamp** bound (`YYYY-MM-DD HH:MM[:SS[.fff]]` or `T`
//!   separator) names an instant on the forcing clock and is inclusive at both
//!   ends (`>= start` and `<= end`).
//! - Omitted or `null` bounds leave that side unbounded.
//! - Bounds and forcing timestamps stay on the configured forcing clock
//!   (`model.control.forcing.timestamp_reference`, either `local_standard_time`
//!   or `utc`); timezone suffixes (`Z`, `+HH:MM`, `-HH:MM`) are rejected.
//!
//! Direct-buffer callers (`run_from_config_str_and_forcing*`, `run_suews*`) may
//! pass chunks or pre-selected forcing intervals and do not invoke this module.

use std::fmt;

use crate::forcing_time::{doy_to_ymd, from_seconds, to_seconds, ymd_to_doy};

#[cfg(feature = "physics")]
use std::collections::HashMap;

#[cfg(feature = "physics")]
use crate::forcing_io::ForcingData;

#[cfg(feature = "physics")]
use crate::forcing_time::{TIME_COLUMNS, TIME_COLUMN_COUNT};

const EPOCH_YEAR: i32 = 2000;
const NANOS_PER_SEC: i128 = 1_000_000_000;
const SECS_PER_DAY: i128 = 86_400;

/// Naive timestamp on the forcing clock, represented as exact nanoseconds since
/// `2000-01-01 00:00:00`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PeriodInstant {
    nanos: i128,
}

impl PeriodInstant {
    /// Construct an instant from whole seconds relative to `2000-01-01 00:00:00`.
    pub const fn from_seconds(secs: i64) -> Self {
        Self {
            nanos: (secs as i128) * NANOS_PER_SEC,
        }
    }

    /// Construct an instant from whole nanoseconds relative to `2000-01-01 00:00:00`.
    pub const fn from_nanos(nanos: i128) -> Self {
        Self { nanos }
    }

    /// Return signed nanoseconds relative to `2000-01-01 00:00:00`.
    pub const fn as_nanos(self) -> i128 {
        self.nanos
    }

    /// Add `secs` seconds to this instant.
    pub const fn add_seconds(self, secs: i64) -> Self {
        Self {
            nanos: self.nanos + (secs as i128) * NANOS_PER_SEC,
        }
    }

    /// Add `delta_nanos` nanoseconds to this instant.
    pub const fn add_nanos(self, delta_nanos: i128) -> Self {
        Self {
            nanos: self.nanos + delta_nanos,
        }
    }

    /// Construct an instant from Gregorian calendar fields on the forcing clock.
    pub fn from_ymdhms_nanos(
        year: i32,
        month: u32,
        day: u32,
        hour: u32,
        minute: u32,
        second: u32,
        nanos: u32,
    ) -> Option<Self> {
        if hour > 23 || minute > 59 || second > 59 || nanos >= NANOS_PER_SEC as u32 {
            return None;
        }
        let doy = ymd_to_doy(year, month, day)?;
        let secs =
            to_seconds(year, doy, hour as i32, minute as i32, EPOCH_YEAR) + i64::from(second);
        Some(Self {
            nanos: i128::from(secs) * NANOS_PER_SEC + i128::from(nanos),
        })
    }

    /// Construct an instant from SUEWS forcing calendar columns `(iy, id, it, imin)`.
    pub fn from_forcing_ydhm(iy: i32, id: i32, it: i32, imin: i32) -> Result<Self, String> {
        if !(1..=9999).contains(&iy) {
            return Err(format!("forcing timestamp has unsupported year {iy}"));
        }
        let secs = to_seconds(iy, id, it, imin, EPOCH_YEAR);
        Ok(Self::from_seconds(secs))
    }
}

impl fmt::Display for PeriodInstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let secs = self.nanos.div_euclid(NANOS_PER_SEC) as i64;
        let sub_nanos = self.nanos.rem_euclid(NANOS_PER_SEC) as u32;
        let second = secs.rem_euclid(60) as u32;
        let (year, doy, hour, minute) = from_seconds(secs, EPOCH_YEAR);
        let (month, day) = doy_to_ymd(year, doy).unwrap_or((1, 1));
        write!(
            f,
            "{year:04}-{month:02}-{day:02} {hour:02}:{minute:02}:{second:02}"
        )?;
        if sub_nanos != 0 {
            let frac = format!("{sub_nanos:09}");
            write!(f, ".{}", frac.trim_end_matches('0'))?;
        }
        Ok(())
    }
}

/// A resolved request for a file-run simulation period on the forcing clock.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RunPeriod {
    /// Start bound on the forcing clock (`None` when unbounded on the left).
    pub start: Option<PeriodInstant>,
    /// End bound on the forcing clock (`None` when unbounded on the right).
    /// For a date-only `end_time` (`D2`), this is `D2+1 00:00:00`.
    pub end: Option<PeriodInstant>,
    /// `true` when `start_time` was supplied as a calendar date (`YYYY-MM-DD`).
    pub start_is_date: bool,
    /// `true` when `end_time` was supplied as a calendar date (`YYYY-MM-DD`).
    pub end_is_date: bool,
    /// Original `model.control.start_time` string, for diagnostics.
    pub start_raw: Option<String>,
    /// Original `model.control.end_time` string, for diagnostics.
    pub end_raw: Option<String>,
}

impl RunPeriod {
    /// Return an unbounded period (selects the entire forcing series).
    pub const fn unbounded() -> Self {
        Self {
            start: None,
            end: None,
            start_is_date: false,
            end_is_date: false,
            start_raw: None,
            end_raw: None,
        }
    }

    /// Return `true` when neither `start_time` nor `end_time` is constrained.
    pub const fn is_unbounded(&self) -> bool {
        self.start.is_none() && self.end.is_none()
    }

    /// Human-readable description of the requested period, matching Python's
    /// `RunPeriod.describe()`.
    pub fn describe(&self) -> String {
        let left = match &self.start_raw {
            Some(s) => s.as_str(),
            None => "forcing start",
        };
        let right = match &self.end_raw {
            Some(s) => s.as_str(),
            None => "forcing end",
        };
        format!("{left} to {right}")
    }

    #[cfg(feature = "physics")]
    fn bound_fields_label(&self) -> &'static str {
        match (self.start.is_some(), self.end.is_some()) {
            (true, true) => "`model.control.start_time` and `model.control.end_time`",
            (true, false) => "`model.control.start_time`",
            (false, true) => "`model.control.end_time`",
            (false, false) => "`model.control.start_time` / `model.control.end_time`",
        }
    }
}

fn parse_ascii_digits(s: &str, min_len: usize, max_len: usize) -> Option<u32> {
    if s.len() < min_len || s.len() > max_len || !s.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    s.parse::<u32>().ok()
}

fn parse_date_part(date_str: &str, raw: &str, field: &str) -> Result<(i32, u32, u32), String> {
    let mut parts = date_str.split('-');
    let Some(y_str) = parts.next() else {
        return Err(format!(
            "`{field}` has invalid date/timestamp {raw:?}; expected YYYY-MM-DD or YYYY-MM-DD HH:MM[:SS]"
        ));
    };
    let Some(m_str) = parts.next() else {
        return Err(format!(
            "`{field}` has invalid date/timestamp {raw:?}; expected YYYY-MM-DD or YYYY-MM-DD HH:MM[:SS]"
        ));
    };
    let Some(d_str) = parts.next() else {
        return Err(format!(
            "`{field}` has invalid date/timestamp {raw:?}; expected YYYY-MM-DD or YYYY-MM-DD HH:MM[:SS]"
        ));
    };
    if parts.next().is_some() {
        return Err(format!(
            "`{field}` has invalid date/timestamp {raw:?}; expected YYYY-MM-DD or YYYY-MM-DD HH:MM[:SS]"
        ));
    }

    let year = parse_ascii_digits(y_str, 4, 4).ok_or_else(|| {
        format!(
            "`{field}` has invalid date/timestamp {raw:?}; expected a 4-digit year in YYYY-MM-DD"
        )
    })? as i32;
    if year == 0 {
        return Err(format!(
            "`{field}` has invalid calendar date in {raw:?}; year must be >= 0001"
        ));
    }
    let month = parse_ascii_digits(m_str, 1, 2).ok_or_else(|| {
        format!(
            "`{field}` has invalid date/timestamp {raw:?}; expected YYYY-MM-DD or YYYY-MM-DD HH:MM[:SS]"
        )
    })?;
    let day = parse_ascii_digits(d_str, 1, 2).ok_or_else(|| {
        format!(
            "`{field}` has invalid date/timestamp {raw:?}; expected YYYY-MM-DD or YYYY-MM-DD HH:MM[:SS]"
        )
    })?;
    if ymd_to_doy(year, month, day).is_none() {
        return Err(format!("`{field}` has invalid calendar date in {raw:?}"));
    }
    Ok((year, month, day))
}

fn parse_time_part(time_str: &str, raw: &str, field: &str) -> Result<(u32, u32, u32, u32), String> {
    if time_str.is_empty() {
        return Err(format!(
            "`{field}` has invalid timestamp {raw:?}; missing time component after date"
        ));
    }
    if time_str.ends_with('Z')
        || time_str.ends_with('z')
        || time_str.contains('+')
        || time_str.contains('-')
    {
        return Err(format!(
            "`{field}` ({raw:?}) must not include a timezone offset; bounds are interpreted \
             on the configured forcing clock (`model.control.forcing.timestamp_reference`)"
        ));
    }

    let mut parts = time_str.split(':');
    let Some(h_str) = parts.next() else {
        return Err(format!(
            "`{field}` has invalid timestamp {raw:?}; expected HH:MM or HH:MM:SS"
        ));
    };
    let Some(min_str) = parts.next() else {
        return Err(format!(
            "`{field}` has invalid timestamp {raw:?}; expected HH:MM or HH:MM:SS"
        ));
    };
    let sec_part = parts.next();
    if parts.next().is_some() {
        return Err(format!(
            "`{field}` has invalid timestamp {raw:?}; expected HH:MM or HH:MM:SS"
        ));
    }

    let hour = parse_ascii_digits(h_str, 1, 2)
        .ok_or_else(|| format!("`{field}` has invalid hour in {raw:?}; expected 00..=23"))?;
    let minute = parse_ascii_digits(min_str, 2, 2).ok_or_else(|| {
        format!("`{field}` has invalid minute in {raw:?}; expected two-digit 00..=59")
    })?;
    if hour > 23 {
        return Err(format!(
            "`{field}` has out-of-range hour in {raw:?}; expected 00..=23"
        ));
    }
    if minute > 59 {
        return Err(format!(
            "`{field}` has out-of-range minute in {raw:?}; expected 00..=59"
        ));
    }

    let (second, nanos) = match sec_part {
        None => (0, 0),
        Some(sec_str) => {
            let (whole, frac) = match sec_str.split_once('.') {
                Some((w, f)) => (w, Some(f)),
                None => (sec_str, None),
            };
            let second = parse_ascii_digits(whole, 2, 2).ok_or_else(|| {
                format!("`{field}` has invalid second in {raw:?}; expected two-digit 00..=59")
            })?;
            if second > 59 {
                return Err(format!(
                    "`{field}` has out-of-range second in {raw:?}; expected 00..=59"
                ));
            }
            let nanos = match frac {
                None => 0,
                Some(f) => {
                    if f.is_empty() || f.len() > 9 || !f.bytes().all(|b| b.is_ascii_digit()) {
                        return Err(format!(
                            "`{field}` has invalid fractional seconds in {raw:?}; expected 1 to 9 digits"
                        ));
                    }
                    let mut scaled = f.parse::<u32>().map_err(|_| {
                        format!("`{field}` has invalid fractional seconds in {raw:?}")
                    })?;
                    for _ in f.len()..9 {
                        scaled *= 10;
                    }
                    scaled
                }
            };
            (second, nanos)
        }
    };

    Ok((hour, minute, second, nanos))
}

fn parse_bound_string(raw: &str, field: &str) -> Result<(PeriodInstant, bool), String> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return Err(format!(
            "`{field}` is empty; expected a date (YYYY-MM-DD), timestamp (YYYY-MM-DD HH:MM[:SS]) or null"
        ));
    }

    let sep_pos = trimmed
        .bytes()
        .position(|b| matches!(b, b' ' | b'T' | b't'));

    match sep_pos {
        None => {
            if trimmed.ends_with('Z') || trimmed.ends_with('z') || trimmed.contains('+') {
                return Err(format!(
                    "`{field}` ({raw:?}) must not include a timezone offset; bounds are interpreted \
                     on the configured forcing clock (`model.control.forcing.timestamp_reference`)"
                ));
            }
            let (year, month, day) = parse_date_part(trimmed, raw, field)?;
            let instant = PeriodInstant::from_ymdhms_nanos(year, month, day, 0, 0, 0, 0)
                .ok_or_else(|| format!("`{field}` has invalid calendar date in {raw:?}"))?;
            Ok((instant, true))
        }
        Some(pos) => {
            let date_str = &trimmed[..pos];
            let sep = trimmed.as_bytes()[pos];
            let rest = &trimmed[pos + 1..];
            let time_str = if sep == b' ' {
                rest.trim_start_matches(' ')
            } else {
                rest
            };
            let (year, month, day) = parse_date_part(date_str, raw, field)?;
            let (hour, minute, second, nanos) = parse_time_part(time_str, raw, field)?;
            let instant =
                PeriodInstant::from_ymdhms_nanos(year, month, day, hour, minute, second, nanos)
                    .ok_or_else(|| format!("`{field}` has invalid timestamp {raw:?}"))?;
            Ok((instant, false))
        }
    }
}

/// Parse and validate optional `model.control.start_time` and
/// `model.control.end_time` strings on the forcing clock.
///
/// Mirrors `supy._run_period.resolve_run_period`:
/// - date-only `start_time` (`D1`) resolves to `D1 00:00:00` (`start_is_date = true`),
/// - date-only `end_time` (`D2`) resolves to `D2+1 00:00:00` (`end_is_date = true`),
/// - explicit timestamps are inclusive at both ends,
/// - reversed or empty requests (`end < start` or `end == start && start_is_date`)
///   fail immediately with a field-specific diagnostic naming both bounds.
pub fn parse_run_period(
    start_raw: Option<&str>,
    end_raw: Option<&str>,
) -> Result<RunPeriod, String> {
    let (start, start_is_date) = match start_raw {
        Some(raw) => {
            let (inst, is_date) = parse_bound_string(raw, "model.control.start_time")?;
            (Some(inst), is_date)
        }
        None => (None, false),
    };

    let (end, end_is_date) = match end_raw {
        Some(raw) => {
            let (inst, is_date) = parse_bound_string(raw, "model.control.end_time")?;
            let resolved = if is_date {
                inst.add_seconds(SECS_PER_DAY as i64)
            } else {
                inst
            };
            (Some(resolved), is_date)
        }
        None => (None, false),
    };

    if let (Some(s), Some(e)) = (start, end) {
        if e < s || (e == s && start_is_date) {
            return Err(format!(
                "`model.control.end_time` ({:?}) is before `model.control.start_time` ({:?}); \
                 the requested simulation period is empty",
                end_raw.unwrap_or(""),
                start_raw.unwrap_or("")
            ));
        }
    }

    Ok(RunPeriod {
        start,
        end,
        start_is_date,
        end_is_date,
        start_raw: start_raw.map(ToOwned::to_owned),
        end_raw: end_raw.map(ToOwned::to_owned),
    })
}

/// Infer the forcing row spacing in nanoseconds: constant observed positive gap
/// across consecutive rows wins over `tstep`; otherwise falls back to `tstep`
/// (in seconds) when positive; otherwise `None`.
pub fn forcing_step_nanos(timestamps: &[PeriodInstant], tstep: Option<i32>) -> Option<i128> {
    if timestamps.len() >= 2 {
        let first_gap = timestamps[1].as_nanos() - timestamps[0].as_nanos();
        if first_gap > 0
            && timestamps
                .windows(2)
                .all(|w| w[1].as_nanos() - w[0].as_nanos() == first_gap)
        {
            return Some(first_gap);
        }
    }
    match tstep {
        Some(s) if s > 0 => Some(i128::from(s) * NANOS_PER_SEC),
        _ => None,
    }
}

fn snap_to_grid(
    instant: PeriodInstant,
    origin: PeriodInstant,
    step_nanos: i128,
    up: bool,
) -> PeriodInstant {
    let diff = instant.as_nanos() - origin.as_nanos();
    let q = diff.div_euclid(step_nanos);
    let r = diff.rem_euclid(step_nanos);
    let n = if up && r != 0 { q + 1 } else { q };
    PeriodInstant::from_nanos(origin.as_nanos() + n * step_nanos)
}

/// Return the `(first, last)` forcing timestamps required to cover `period`,
/// matching `supy._run_period.required_rows`.
///
/// `None` on a side means that side is unbounded. A date-only `start_time`
/// requires a known row spacing to identify its first interval-end row
/// (`D1 00:00 + step`).
pub fn required_rows(
    period: &RunPeriod,
    timestamps: &[PeriodInstant],
    tstep: Option<i32>,
) -> Result<(Option<PeriodInstant>, Option<PeriodInstant>), String> {
    if timestamps.is_empty() {
        return Err("forcing data is empty".to_string());
    }
    let step = forcing_step_nanos(timestamps, tstep);
    let origin = timestamps[0];

    let first = match period.start {
        None => None,
        Some(start) => {
            if period.start_is_date {
                let Some(step_nanos) = step else {
                    return Err(format!(
                        "Cannot check that the forcing covers the date-only \
                         `model.control.start_time` ({:?}): the forcing rows have \
                         no regular spacing and no model timestep was given. \
                         Supply regularly spaced forcing, or give `model.control.start_time` \
                         as an explicit timestamp.",
                        period.start_raw.as_deref().unwrap_or("")
                    ));
                };
                Some(start.add_nanos(step_nanos))
            } else if let Some(step_nanos) = step {
                Some(snap_to_grid(start, origin, step_nanos, true))
            } else {
                Some(start)
            }
        }
    };

    let last = match period.end {
        None => None,
        Some(end) => {
            if period.end_is_date {
                Some(end)
            } else if let Some(step_nanos) = step {
                Some(snap_to_grid(end, origin, step_nanos, false))
            } else {
                Some(end)
            }
        }
    };

    Ok((first, last))
}

/// Return indices of `timestamps` that fall inside `period`.
pub fn select_row_indices(timestamps: &[PeriodInstant], period: &RunPeriod) -> Vec<usize> {
    timestamps
        .iter()
        .enumerate()
        .filter_map(|(idx, &ts)| {
            let after_start = match period.start {
                None => true,
                Some(start) if period.start_is_date => ts > start,
                Some(start) => ts >= start,
            };
            let before_end = match period.end {
                None => true,
                Some(end) => ts <= end,
            };
            (after_start && before_end).then_some(idx)
        })
        .collect()
}

/// Describe why `timestamps` does not cover `period`, or return `Ok(None)` when
/// the forcing covers the entire request.
pub fn coverage_problem(
    period: &RunPeriod,
    timestamps: &[PeriodInstant],
    tstep: Option<i32>,
) -> Result<Option<String>, String> {
    let (first, last) = required_rows(period, timestamps, tstep)?;
    let available_start = timestamps[0];
    let available_end = timestamps[timestamps.len() - 1];
    let start_uncovered = first.is_some_and(|f| available_start > f);
    let end_uncovered = last.is_some_and(|l| available_end < l);
    if !start_uncovered && !end_uncovered {
        return Ok(None);
    }

    let first_row = first.unwrap_or(available_start);
    let last_row = last.unwrap_or(available_end);
    let step = forcing_step_nanos(timestamps, tstep);
    let span = match step {
        Some(step_nanos) => format!(
            "the period ({}, {}], i.e. forcing rows ",
            first_row.add_nanos(-step_nanos),
            last_row
        ),
        None => "forcing rows ".to_string(),
    };
    let offending = match (start_uncovered, end_uncovered) {
        (true, false) => "`model.control.start_time`",
        (false, true) => "`model.control.end_time`",
        _ => "`model.control.start_time` and `model.control.end_time`",
    };
    Ok(Some(format!(
        "Forcing does not cover the requested simulation period ({} needs {}{} to {}; \
         forcing available {} to {}; see 'Timestamp convention' in the SUEWS documentation). \
         Supply forcing covering the period or adjust {} to the covered range.",
        period.describe(),
        span,
        first_row,
        last_row,
        available_start,
        available_end,
        offending,
    )))
}

#[cfg(feature = "physics")]
fn forcing_f64_to_i32(value: f64, row: usize, col_name: &str) -> Result<i32, String> {
    if !value.is_finite()
        || value.fract() != 0.0
        || value < i32::MIN as f64
        || value > i32::MAX as f64
    {
        return Err(format!(
            "forcing row {row} has invalid `{col_name}` timestamp value {value}"
        ));
    }
    Ok(value as i32)
}

#[cfg(feature = "physics")]
fn extract_forcing_timestamps(forcing: &ForcingData) -> Result<Vec<PeriodInstant>, String> {
    if forcing.len_sim == 0 {
        return Err("forcing data is empty".to_string());
    }
    let expected_len = forcing
        .len_sim
        .checked_mul(forcing.stride)
        .ok_or_else(|| "forcing block length overflow".to_string())?;
    if forcing.stride < TIME_COLUMN_COUNT || forcing.block.len() < expected_len {
        return Err(format!(
            "forcing block shape mismatch: len={}, len_sim={}, stride={}",
            forcing.block.len(),
            forcing.len_sim,
            forcing.stride
        ));
    }

    let t = TIME_COLUMNS;
    let mut timestamps = Vec::with_capacity(forcing.len_sim);
    for row in 0..forcing.len_sim {
        let base = row * forcing.stride;
        let iy = forcing_f64_to_i32(forcing.block[base + t.iy], row, "iy")?;
        let id = forcing_f64_to_i32(forcing.block[base + t.id], row, "id")?;
        let it = forcing_f64_to_i32(forcing.block[base + t.it], row, "it")?;
        let imin = forcing_f64_to_i32(forcing.block[base + t.imin], row, "imin")?;
        timestamps.push(PeriodInstant::from_forcing_ydhm(iy, id, it, imin)?);
    }
    Ok(timestamps)
}

/// Slice an interpolated (or native-timestep) forcing block to `period` on the
/// forcing clock and verify full forcing coverage before kernel evolution.
///
/// Mirrors `supy._run_period.slice_forcing_to_period(..., clip_to_forcing=False)`:
/// - An unbounded period returns a clone of `forcing` after basic shape checks.
/// - Non-overlapping requests fail with `"does not overlap"` naming the
///   configured `model.control.start_time` / `model.control.end_time` field(s).
/// - Partially covered requests fail with `"does not cover"` naming the
///   offending bound field(s), required left-open period `(first - dt, last]`,
///   and available forcing range.
#[cfg(feature = "physics")]
pub fn slice_forcing_to_period(
    forcing: &ForcingData,
    period: &RunPeriod,
    tstep: i32,
) -> Result<ForcingData, String> {
    let timestamps = extract_forcing_timestamps(forcing)?;
    if period.is_unbounded() {
        return Ok(forcing.clone());
    }

    let selected = select_row_indices(&timestamps, period);
    if selected.is_empty() {
        return Err(format!(
            "Requested simulation period ({}) does not overlap the loaded forcing \
             (available {} to {}). Supply forcing for the requested period or adjust {}.",
            period.describe(),
            timestamps[0],
            timestamps[timestamps.len() - 1],
            period.bound_fields_label(),
        ));
    }

    let fallback_tstep = (tstep > 0).then_some(tstep);
    if let Some(detail) = coverage_problem(period, &timestamps, fallback_tstep)? {
        return Err(detail);
    }

    let mut block = Vec::with_capacity(selected.len() * forcing.stride);
    for &row in &selected {
        let start = row * forcing.stride;
        block.extend_from_slice(&forcing.block[start..start + forcing.stride]);
    }

    let mut extras = HashMap::with_capacity(forcing.extras.len());
    for (key, values) in &forcing.extras {
        if values.len() == forcing.len_sim {
            let sliced: Vec<f64> = selected.iter().map(|&row| values[row]).collect();
            extras.insert(key.clone(), sliced);
        }
    }

    Ok(ForcingData {
        block,
        len_sim: selected.len(),
        stride: forcing.stride,
        extras,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn regular_5min(start: PeriodInstant, count: usize) -> Vec<PeriodInstant> {
        (0..count)
            .map(|i| start.add_seconds((i as i64) * 300))
            .collect()
    }

    #[test]
    fn parses_date_only_and_explicit_timestamps_including_leap_day() {
        let p = parse_run_period(Some("2012-02-28"), Some("2012-02-29")).unwrap();
        assert!(p.start_is_date);
        assert!(p.end_is_date);
        assert_eq!(p.start.unwrap().to_string(), "2012-02-28 00:00:00");
        assert_eq!(p.end.unwrap().to_string(), "2012-03-01 00:00:00");

        let p2 = parse_run_period(Some(" 2012-1-2 "), Some("2012-01-02T23:59:59.5")).unwrap();
        assert!(p2.start_is_date);
        assert!(!p2.end_is_date);
        assert_eq!(p2.start.unwrap().to_string(), "2012-01-02 00:00:00");
        assert_eq!(p2.end.unwrap().to_string(), "2012-01-02 23:59:59.5");

        let p3 = parse_run_period(Some("2012-01-01 6:00"), Some("2012-01-01 06:00:00")).unwrap();
        assert!(!p3.start_is_date);
        assert!(!p3.end_is_date);
        assert_eq!(p3.start, p3.end);
    }

    #[test]
    fn rejects_invalid_syntax_calendar_tz_offsets_and_empty_periods() {
        for bad in [
            "",
            "   ",
            "NaT",
            "spring",
            "2012/01/01",
            "2013-02-29",
            "1900-02-29",
            "2012-04-31",
            "2012-13-01",
            "2012-01-01 24:00",
            "2012-01-01 12:60",
            "2012-01-01 12:00:60",
            "2012-01-01 12:00:00.1234567890",
            "2012-01-01T00:05:00Z",
            "2012-01-01T00:05:00+00:00",
            "2012-01-01 00:05:00-05:00",
            "2012-01-01Z",
        ] {
            let err_start = parse_run_period(Some(bad), None).unwrap_err();
            assert!(
                err_start.contains("model.control.start_time"),
                "{bad}: {err_start}"
            );
            let err_end = parse_run_period(None, Some(bad)).unwrap_err();
            assert!(
                err_end.contains("model.control.end_time"),
                "{bad}: {err_end}"
            );
        }

        for (start, end) in [
            ("2012-01-02", "2012-01-01"),
            ("2012-01-02", "2012-01-02 00:00:00"),
            ("2012-01-02 00:05", "2012-01-02 00:00"),
            ("2012-01-02 00:05", "2012-01-01"),
        ] {
            let err = parse_run_period(Some(start), Some(end)).unwrap_err();
            assert!(err.contains("model.control.start_time"), "{err}");
            assert!(err.contains("model.control.end_time"), "{err}");
            assert!(err.contains("empty"), "{err}");
        }
    }

    #[test]
    fn date_only_and_explicit_bounds_match_python_grid_semantics() {
        let start = PeriodInstant::from_ymdhms_nanos(2012, 1, 1, 0, 5, 0, 0).unwrap();
        let ts = regular_5min(start, 288 * 2); // 2012-01-01 00:05 .. 2012-01-03 00:00

        // Inclusive single calendar day: 288 rows ending at D+1 00:00.
        let p_day1 = parse_run_period(Some("2012-01-01"), Some("2012-01-01")).unwrap();
        let idx = select_row_indices(&ts, &p_day1);
        assert_eq!(idx.len(), 288);
        assert_eq!(ts[idx[0]].to_string(), "2012-01-01 00:05:00");
        assert_eq!(ts[*idx.last().unwrap()].to_string(), "2012-01-02 00:00:00");
        assert_eq!(coverage_problem(&p_day1, &ts, Some(300)).unwrap(), None);

        // Date-only start excludes midnight row belonging to the previous day.
        let ts_with_midnight = regular_5min(
            PeriodInstant::from_ymdhms_nanos(2012, 1, 1, 0, 0, 0, 0).unwrap(),
            289,
        );
        let p_from_day1 = parse_run_period(Some("2012-01-01"), None).unwrap();
        let idx_m = select_row_indices(&ts_with_midnight, &p_from_day1);
        assert_eq!(idx_m.len(), 288);
        assert_eq!(
            ts_with_midnight[idx_m[0]].to_string(),
            "2012-01-01 00:05:00"
        );

        // Between-row explicit bounds snap up on start and down on end.
        let p_between =
            parse_run_period(Some("2012-01-01 00:03"), Some("2012-01-01 23:59:59")).unwrap();
        let (first, last) = required_rows(&p_between, &ts, Some(300)).unwrap();
        assert_eq!(first.unwrap().to_string(), "2012-01-01 00:05:00");
        assert_eq!(last.unwrap().to_string(), "2012-01-01 23:55:00");
        assert_eq!(coverage_problem(&p_between, &ts, Some(300)).unwrap(), None);

        // Partial overlap reports offending bound and left-open period.
        let p_late = parse_run_period(Some("2012-01-01"), Some("2012-01-10")).unwrap();
        let err = coverage_problem(&p_late, &ts, Some(300)).unwrap().unwrap();
        assert!(err.contains("does not cover"), "{err}");
        assert!(
            err.contains("(2012-01-01 00:00:00, 2012-01-11 00:00:00]"),
            "{err}"
        );
        assert!(err.contains("Timestamp convention"), "{err}");
        assert!(err.contains("model.control.end_time"), "{err}");

        let p_early = parse_run_period(Some("2011-12-31"), Some("2012-01-01")).unwrap();
        let err = coverage_problem(&p_early, &ts, Some(300)).unwrap().unwrap();
        assert!(err.contains("does not cover"), "{err}");
        assert!(err.contains("model.control.start_time"), "{err}");
    }

    #[test]
    fn observed_spacing_wins_over_tstep_and_unknown_spacing_fails_closed() {
        let hourly = vec![
            PeriodInstant::from_ymdhms_nanos(2012, 1, 1, 1, 0, 0, 0).unwrap(),
            PeriodInstant::from_ymdhms_nanos(2012, 1, 1, 2, 0, 0, 0).unwrap(),
        ];
        let p = parse_run_period(Some("2012-01-01"), None).unwrap();
        let (first, _) = required_rows(&p, &hourly, Some(300)).unwrap();
        assert_eq!(first.unwrap().to_string(), "2012-01-01 01:00:00");

        let single = vec![PeriodInstant::from_ymdhms_nanos(2012, 1, 1, 12, 0, 0, 0).unwrap()];
        let err = required_rows(&p, &single, None).unwrap_err();
        assert!(err.contains("no regular spacing"), "{err}");
        assert!(err.contains("model.control.start_time"), "{err}");
        let (first_tstep, _) = required_rows(&p, &single, Some(300)).unwrap();
        assert_eq!(first_tstep.unwrap().to_string(), "2012-01-01 00:05:00");
    }
}
