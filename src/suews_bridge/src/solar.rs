use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const SOLAR_STATE_FLAT_LEN: usize = 3;
pub const SOLAR_STATE_SCHEMA_VERSION: u32 = 1;

pub type SolarStateSchema = crate::codec::SimpleSchema;

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

pub fn solar_state_field_names() -> Vec<String> {
    vec![
        "azimuth_deg".to_string(),
        "zenith_deg".to_string(),
        "iter_safe".to_string(),
    ]
}

crate::codec::impl_state_module_fns! {
    prefix = solar_state,
    state_type = SolarState,
    schema_type = SolarStateSchema,
    payload_type = SolarStateValuesPayload,
    flat_len_const = SOLAR_STATE_FLAT_LEN,
    schema_version_const = SOLAR_STATE_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_solar_state_len,
    ffi_schema_version_fn = ffi::suews_solar_state_schema_version,
    ffi_default_fn = ffi::suews_solar_state_default,
}
#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

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
