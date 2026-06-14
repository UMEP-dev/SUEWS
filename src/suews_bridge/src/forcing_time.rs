#[derive(Debug, Clone, Copy)]
pub struct TimeColumns {
    pub iy: usize,
    pub id: usize,
    pub it: usize,
    pub imin: usize,
}

impl Default for TimeColumns {
    fn default() -> Self {
        Self::new()
    }
}

impl TimeColumns {
    pub const COUNT: usize = 4;

    pub const fn new() -> Self {
        Self {
            iy: 0,
            id: 1,
            it: 2,
            imin: 3,
        }
    }
}

pub const TIME_COLUMNS: TimeColumns = TimeColumns::new();
pub const TIME_COLUMN_COUNT: usize = TimeColumns::COUNT;

pub fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

pub fn days_in_year(year: i32) -> i64 {
    if is_leap_year(year) {
        366
    } else {
        365
    }
}

/// Convert (year, day-of-year, hour, minute) to seconds since Jan 1 00:00 of `base_year`.
pub fn to_seconds(iy: i32, id: i32, it: i32, imin: i32, base_year: i32) -> i64 {
    let mut days: i64 = 0;
    for y in base_year..iy {
        days += days_in_year(y);
    }
    days += (id - 1) as i64; // id is 1-based
    days * 86400 + it as i64 * 3600 + imin as i64 * 60
}

/// Convert seconds since Jan 1 00:00 of `base_year` back to (year, doy, hour, minute).
pub fn from_seconds(total_sec: i64, base_year: i32) -> (i32, i32, i32, i32) {
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