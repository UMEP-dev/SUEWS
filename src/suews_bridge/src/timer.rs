use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const SUEWS_TIMER_FLAT_LEN: usize = 18;
pub const SUEWS_TIMER_SCHEMA_VERSION: u32 = 1;

pub type SuewsTimerSchema = crate::codec::SimpleSchema;

pub type SuewsTimerValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct SuewsTimer {
    pub id: i32,
    pub imin: i32,
    pub isec: i32,
    pub it: i32,
    pub iy: i32,
    pub tstep: i32,
    pub tstep_prev: i32,
    pub dt_since_start: i32,
    pub dt_since_start_prev: i32,
    pub nsh: i32,
    pub nsh_real: f64,
    pub tstep_real: f64,
    pub dectime: f64,
    pub dayofweek_id: [i32; 3],
    pub dls: i32,
    pub new_day: i32,
}

impl Default for SuewsTimer {
    fn default() -> Self {
        Self {
            id: 0,
            imin: 0,
            isec: 0,
            it: 0,
            iy: 0,
            tstep: 0,
            tstep_prev: 0,
            dt_since_start: 0,
            dt_since_start_prev: 0,
            nsh: 0,
            nsh_real: 0.0,
            tstep_real: 0.0,
            dectime: 0.0,
            dayofweek_id: [0; 3],
            dls: 0,
            new_day: 0,
        }
    }
}

fn decode_int(value: f64) -> Result<i32, BridgeError> {
    if !value.is_finite() {
        return Err(BridgeError::BadState);
    }

    let rounded = value.round();
    if (value - rounded).abs() > 1.0e-9 {
        return Err(BridgeError::BadState);
    }

    if rounded < i32::MIN as f64 || rounded > i32::MAX as f64 {
        return Err(BridgeError::BadState);
    }

    Ok(rounded as i32)
}

impl SuewsTimer {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, SUEWS_TIMER_FLAT_LEN)?;

        Ok(Self {
            id: decode_int(flat[0])?,
            imin: decode_int(flat[1])?,
            isec: decode_int(flat[2])?,
            it: decode_int(flat[3])?,
            iy: decode_int(flat[4])?,
            tstep: decode_int(flat[5])?,
            tstep_prev: decode_int(flat[6])?,
            dt_since_start: decode_int(flat[7])?,
            dt_since_start_prev: decode_int(flat[8])?,
            nsh: decode_int(flat[9])?,
            nsh_real: flat[10],
            tstep_real: flat[11],
            dectime: flat[12],
            dayofweek_id: [
                decode_int(flat[13])?,
                decode_int(flat[14])?,
                decode_int(flat[15])?,
            ],
            dls: decode_int(flat[16])?,
            new_day: decode_int(flat[17])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.id as f64,
            self.imin as f64,
            self.isec as f64,
            self.it as f64,
            self.iy as f64,
            self.tstep as f64,
            self.tstep_prev as f64,
            self.dt_since_start as f64,
            self.dt_since_start_prev as f64,
            self.nsh as f64,
            self.nsh_real,
            self.tstep_real,
            self.dectime,
            self.dayofweek_id[0] as f64,
            self.dayofweek_id[1] as f64,
            self.dayofweek_id[2] as f64,
            self.dls as f64,
            self.new_day as f64,
        ]
    }
}

impl StateCodec for SuewsTimer {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "SUEWS_TIMER".to_string(),
            schema_version: SUEWS_TIMER_SCHEMA_VERSION,
            flat_len: SUEWS_TIMER_FLAT_LEN,
            field_names: suews_timer_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        SuewsTimer::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        SuewsTimer::to_flat(self)
    }
}

pub fn suews_timer_field_names() -> Vec<String> {
    vec![
        "id".to_string(),
        "imin".to_string(),
        "isec".to_string(),
        "it".to_string(),
        "iy".to_string(),
        "tstep".to_string(),
        "tstep_prev".to_string(),
        "dt_since_start".to_string(),
        "dt_since_start_prev".to_string(),
        "nsh".to_string(),
        "nsh_real".to_string(),
        "tstep_real".to_string(),
        "dectime".to_string(),
        "dayofweek_id_1".to_string(),
        "dayofweek_id_2".to_string(),
        "dayofweek_id_3".to_string(),
        "dls".to_string(),
        "new_day".to_string(),
    ]
}

crate::codec::impl_state_module_fns! {
    prefix = suews_timer,
    state_type = SuewsTimer,
    schema_type = SuewsTimerSchema,
    payload_type = SuewsTimerValuesPayload,
    flat_len_const = SUEWS_TIMER_FLAT_LEN,
    schema_version_const = SUEWS_TIMER_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_timer_len,
    ffi_schema_version_fn = ffi::suews_timer_schema_version,
    ffi_default_fn = ffi::suews_timer_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = suews_timer_schema().expect("schema call should succeed");
        assert_eq!(n_flat, SUEWS_TIMER_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = suews_timer_default_from_fortran().expect("default state should be available");
        assert_eq!(state, SuewsTimer::default());

        let state2 =
            SuewsTimer::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = suews_timer_default_from_fortran().expect("default state should be available");
        let mut mapped = suews_timer_to_map(&state);
        mapped.insert("tstep".to_string(), 300.0);
        mapped.insert("dectime".to_string(), 12.5);
        mapped.insert("dayofweek_id_2".to_string(), 7.0);

        let updated = suews_timer_from_map(&mapped).expect("map to state should succeed");
        assert_eq!(updated.tstep, 300);
        assert!((updated.dectime - 12.5).abs() < 1.0e-12);
        assert_eq!(updated.dayofweek_id[1], 7);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = suews_timer_default_from_fortran().expect("default state should be available");
        let payload = suews_timer_to_values_payload(&state);
        let recovered =
            suews_timer_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = SuewsTimerValuesPayload {
            schema_version: SUEWS_TIMER_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = suews_timer_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn from_flat_rejects_non_integer_fields() {
        let mut flat = vec![0.0_f64; SUEWS_TIMER_FLAT_LEN];
        flat[5] = 123.4;
        let err = SuewsTimer::from_flat(&flat).expect_err("fractional integer should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
