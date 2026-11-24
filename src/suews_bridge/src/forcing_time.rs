//! Gregorian calendar and column-index utilities for standalone forcing I/O.
//!
//! Provides the four leading timestamp column indices (`iy`, `id`, `it`, `imin`)
//! and signed-second conversions relative to an arbitrary `base_year`, used by
//! `forcing_io` to detect timesteps and align interpolated forcing rows across
//! year and leap-year boundaries.

/// Zero-based column indices of the four timestamp fields in SUEWS forcing data.
#[derive(Debug, Clone, Copy)]
pub struct TimeColumns {
    /// Year column index (`0`).
    pub iy: usize,
    /// 1-based day-of-year column index (`1`).
    pub id: usize,
    /// Hour-of-day column index (`2`).
    pub it: usize,
    /// Minute-of-hour column index (`3`).
    pub imin: usize,
}

impl Default for TimeColumns {
    fn default() -> Self {
        Self::new()
    }
}

impl TimeColumns {
    /// Number of leading calendar columns in a forcing row (`4`).
    pub const COUNT: usize = 4;

    /// Construct the canonical `(0, 1, 2, 3)` timestamp column index mapping.
    pub const fn new() -> Self {
        Self {
            iy: 0,
            id: 1,
            it: 2,
            imin: 3,
        }
    }
}

/// Canonical timestamp column indices for SUEWS forcing tables.
pub const TIME_COLUMNS: TimeColumns = TimeColumns::new();
/// Number of leading calendar columns (`iy`, `id`, `it`, `imin`).
pub const TIME_COLUMN_COUNT: usize = TimeColumns::COUNT;

/// Return `true` if `year` is a Gregorian leap year.
pub fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

/// Return the number of calendar days in `year` (`366` in a leap year, else `365`).
pub fn days_in_year(year: i32) -> i64 {
    if is_leap_year(year) {
        366
    } else {
        365
    }
}

/// Return the number of calendar days in `(year, month)` for 1-based `month`,
/// or `None` when `month` is outside `1..=12`.
pub fn days_in_month(year: i32, month: u32) -> Option<u32> {
    match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => Some(31),
        4 | 6 | 9 | 11 => Some(30),
        2 => Some(if is_leap_year(year) { 29 } else { 28 }),
        _ => None,
    }
}

/// Convert Gregorian `(year, month, day)` (1-based) to 1-based day-of-year,
/// returning `None` for invalid calendar dates.
pub fn ymd_to_doy(year: i32, month: u32, day: u32) -> Option<i32> {
    let max_day = days_in_month(year, month)?;
    if day == 0 || day > max_day {
        return None;
    }
    let mut doy: u32 = 0;
    for m in 1..month {
        doy += days_in_month(year, m)?;
    }
    Some((doy + day) as i32)
}

/// Convert `(year, doy)` (1-based day-of-year) to 1-based `(month, day)`,
/// returning `None` when `doy` is outside the calendar year.
pub fn doy_to_ymd(year: i32, doy: i32) -> Option<(u32, u32)> {
    if doy < 1 || i64::from(doy) > days_in_year(year) {
        return None;
    }
    let mut remaining = doy as u32;
    for month in 1..=12 {
        let dim = days_in_month(year, month)?;
        if remaining <= dim {
            return Some((month, remaining));
        }
        remaining -= dim;
    }
    None
}

/// Convert (year, day-of-year, hour, minute) to seconds since Jan 1 00:00 of `base_year`.
pub fn to_seconds(iy: i32, id: i32, it: i32, imin: i32, base_year: i32) -> i64 {
    let mut days: i64 = 0;
    for y in base_year..iy {
        days += days_in_year(y);
    }
    for y in iy..base_year {
        days -= days_in_year(y);
    }
    days += (id - 1) as i64; // id is 1-based
    days * 86400 + it as i64 * 3600 + imin as i64 * 60
}

/// Convert seconds since Jan 1 00:00 of `base_year` back to (year, doy, hour, minute).
pub fn from_seconds(total_sec: i64, base_year: i32) -> (i32, i32, i32, i32) {
    let mut remaining = total_sec;
    let mut year = base_year;
    while remaining < 0 {
        year -= 1;
        remaining += days_in_year(year) * 86400;
    }
    loop {
        let year_sec = days_in_year(year) * 86400;
        if remaining < year_sec {
            break;
        }
        remaining -= year_sec;
        year += 1;
    }
    let day_of_year = remaining.div_euclid(86400) as i32 + 1; // 1-based
    remaining = remaining.rem_euclid(86400);
    let hour = (remaining / 3600) as i32;
    remaining %= 3600;
    let minute = (remaining / 60) as i32;
    (year, day_of_year, hour, minute)
}

// contract: C-CLOCK
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn signed_seconds_cross_year_and_leap_boundaries() {
        for (seconds, base, expected) in [
            (-1, 2012, (2011, 365, 23, 59)),
            (-60, 2012, (2011, 365, 23, 59)),
            (-3900, 2012, (2011, 365, 22, 55)),
            (-86400, 2013, (2012, 366, 0, 0)),
            (-366 * 86400, 2013, (2012, 1, 0, 0)),
            (59 * 86400, 2012, (2012, 60, 0, 0)),
            (366 * 86400, 2012, (2013, 1, 0, 0)),
            (0, 2012, (2012, 1, 0, 0)),
            (3900, 2012, (2012, 1, 1, 5)),
        ] {
            assert_eq!(from_seconds(seconds, base), expected);
            let (iy, id, it, imin) = expected;
            assert_eq!(
                to_seconds(iy, id, it, imin, base),
                seconds.div_euclid(60) * 60
            );
        }
    }

    #[test]
    fn dates_round_trip_on_both_sides_of_base_year() {
        for base in [1900, 2000, 2012, 2100] {
            for year in (base - 4)..=(base + 4) {
                for day in [1, 59, 60, 61, days_in_year(year) as i32] {
                    let date = (year, day, 23, 55);
                    assert_eq!(
                        from_seconds(to_seconds(year, day, 23, 55, base), base),
                        date
                    );
                }
            }
        }
    }

    #[test]
    fn ymd_and_doy_conversions_respect_gregorian_leap_years() {
        assert_eq!(days_in_month(2012, 2), Some(29));
        assert_eq!(days_in_month(2013, 2), Some(28));
        assert_eq!(days_in_month(2000, 2), Some(29));
        assert_eq!(days_in_month(1900, 2), Some(28));
        assert_eq!(days_in_month(2012, 0), None);
        assert_eq!(days_in_month(2012, 13), None);

        assert_eq!(ymd_to_doy(2012, 2, 29), Some(60));
        assert_eq!(ymd_to_doy(2012, 3, 1), Some(61));
        assert_eq!(ymd_to_doy(2012, 12, 31), Some(366));
        assert_eq!(ymd_to_doy(2013, 2, 29), None);
        assert_eq!(ymd_to_doy(2013, 3, 1), Some(60));
        assert_eq!(ymd_to_doy(2013, 12, 31), Some(365));
        assert_eq!(ymd_to_doy(1900, 2, 29), None);
        assert_eq!(ymd_to_doy(2000, 2, 29), Some(60));
        assert_eq!(ymd_to_doy(2012, 4, 31), None);
        assert_eq!(ymd_to_doy(2012, 1, 0), None);

        for year in [1900, 2000, 2012, 2013, 2100] {
            let max_doy = days_in_year(year) as i32;
            for doy in 1..=max_doy {
                let (m, d) = doy_to_ymd(year, doy).expect("valid doy");
                assert_eq!(ymd_to_doy(year, m, d), Some(doy));
            }
            assert_eq!(doy_to_ymd(year, 0), None);
            assert_eq!(doy_to_ymd(year, max_doy + 1), None);
        }
    }
}
