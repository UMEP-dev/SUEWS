use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const SOLAR_STATE_FLAT_LEN: usize = 3;
pub const SOLAR_STATE_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SolarStateSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type SolarStateValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct SolarState {
    pub azimuth_deg: f64,
    pub zenith_deg: f64,
    pub iter_safe: bool,
}

impl Default for SolarState {
    fn default() -> Self {
        Self {
            azimuth_deg: 0.0,
            zenith_deg: 0.0,
            iter_safe: true,
        }
    }
}

impl SolarState {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, SOLAR_STATE_FLAT_LEN)?;
        Ok(Self {
            azimuth_deg: flat[0],
            zenith_deg: flat[1],
            iter_safe: flat[2] >= 0.5,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.azimuth_deg,
            self.zenith_deg,
            if self.iter_safe { 1.0 } else { 0.0 },
        ]
    }
}

impl StateCodec for SolarState {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "solar_State".to_string(),
            schema_version: SOLAR_STATE_SCHEMA_VERSION,
            flat_len: SOLAR_STATE_FLAT_LEN,
            field_names: solar_state_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        SolarState::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        SolarState::to_flat(self)
    }
}

pub fn solar_state_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_solar_state_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn solar_state_schema_info() -> Result<SolarStateSchema, BridgeError> {
    let flat_len = solar_state_schema()?;
    let schema_version_runtime = solar_state_schema_version_runtime()?;
    let field_names = solar_state_field_names();

    if schema_version_runtime != SOLAR_STATE_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(SolarStateSchema {
        schema_version: SOLAR_STATE_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn solar_state_field_names() -> Vec<String> {
    vec![
        "azimuth_deg".to_string(),
        "zenith_deg".to_string(),
        "iter_safe".to_string(),
    ]
}

pub fn solar_state_schema_version() -> u32 {
    SOLAR_STATE_SCHEMA_VERSION
}

pub fn solar_state_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_solar_state_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn solar_state_field_index(name: &str) -> Option<usize> {
    let names = solar_state_field_names();
    field_index(&names, name)
}

pub fn solar_state_to_map(state: &SolarState) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn solar_state_to_ordered_values(state: &SolarState) -> Vec<f64> {
    state.to_flat()
}

pub fn solar_state_from_ordered_values(values: &[f64]) -> Result<SolarState, BridgeError> {
    SolarState::from_flat(values)
}

pub fn solar_state_to_values_payload(state: &SolarState) -> SolarStateValuesPayload {
    to_values_payload(state)
}

pub fn solar_state_from_values_payload(
    payload: &SolarStateValuesPayload,
) -> Result<SolarState, BridgeError> {
    from_values_payload(payload)
}

pub fn solar_state_from_map(values: &BTreeMap<String, f64>) -> Result<SolarState, BridgeError> {
    let default_state = solar_state_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn solar_state_default_from_fortran() -> Result<SolarState, BridgeError> {
    let n_flat = solar_state_schema()?;
    if n_flat != SOLAR_STATE_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_solar_state_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    SolarState::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = solar_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, SOLAR_STATE_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = solar_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, SolarState::default());
        let state2 =
            SolarState::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = solar_state_default_from_fortran().expect("default state should be available");
        let mut mapped = solar_state_to_map(&state);
        mapped.insert("azimuth_deg".to_string(), 23.5);
        mapped.insert("iter_safe".to_string(), 0.0);

        let updated = solar_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.azimuth_deg - 23.5).abs() < 1.0e-12);
        assert!(!updated.iter_safe);
    }

    #[test]
    fn state_map_rejects_unknown_keys() {
        let mut mapped = BTreeMap::new();
        mapped.insert("bad_field".to_string(), 1.0);
        let err = solar_state_from_map(&mapped).expect_err("unknown field should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = solar_state_default_from_fortran().expect("default state should be available");
        let payload = solar_state_to_values_payload(&state);
        let recovered =
            solar_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = SolarStateValuesPayload {
            schema_version: SOLAR_STATE_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = solar_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
