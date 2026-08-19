use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const NHOOD_STATE_FLAT_LEN: usize = 5;
pub const NHOOD_STATE_SCHEMA_VERSION: u32 = 1;

pub type NhoodStateSchema = crate::codec::SimpleSchema;

pub type NhoodStateValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct NhoodState {
    pub u_hbh_1dravg: f64,
    pub qn_1dravg: f64,
    pub tair_mn_prev: f64,
    pub iter_count: f64,
    pub iter_safe: bool,
}

impl Default for NhoodState {
    fn default() -> Self {
        Self {
            u_hbh_1dravg: 0.0,
            qn_1dravg: 0.0,
            tair_mn_prev: 0.0,
            iter_count: 0.0,
            iter_safe: false,
        }
    }
}

impl NhoodState {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, NHOOD_STATE_FLAT_LEN)?;
        Ok(Self {
            u_hbh_1dravg: flat[0],
            qn_1dravg: flat[1],
            tair_mn_prev: flat[2],
            iter_count: flat[3],
            iter_safe: flat[4] >= 0.5,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.u_hbh_1dravg,
            self.qn_1dravg,
            self.tair_mn_prev,
            self.iter_count,
            if self.iter_safe { 1.0 } else { 0.0 },
        ]
    }
}

impl StateCodec for NhoodState {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "NHOOD_STATE".to_string(),
            schema_version: NHOOD_STATE_SCHEMA_VERSION,
            flat_len: NHOOD_STATE_FLAT_LEN,
            field_names: nhood_state_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        NhoodState::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        NhoodState::to_flat(self)
    }
}

pub fn nhood_state_field_names() -> Vec<String> {
    vec![
        "u_hbh_1dravg".to_string(),
        "qn_1dravg".to_string(),
        "tair_mn_prev".to_string(),
        "iter_count".to_string(),
        "iter_safe".to_string(),
    ]
}

crate::codec::impl_state_module_fns! {
    prefix = nhood_state,
    state_type = NhoodState,
    schema_type = NhoodStateSchema,
    payload_type = NhoodStateValuesPayload,
    flat_len_const = NHOOD_STATE_FLAT_LEN,
    schema_version_const = NHOOD_STATE_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_nhood_state_len,
    ffi_schema_version_fn = ffi::suews_nhood_state_schema_version,
    ffi_default_fn = ffi::suews_nhood_state_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = nhood_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, NHOOD_STATE_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = nhood_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, NhoodState::default());
        let state2 =
            NhoodState::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = nhood_state_default_from_fortran().expect("default state should be available");
        let mut mapped = nhood_state_to_map(&state);
        mapped.insert("iter_count".to_string(), 2.0);
        mapped.insert("iter_safe".to_string(), 1.0);

        let updated = nhood_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.iter_count - 2.0).abs() < 1.0e-12);
        assert!(updated.iter_safe);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = nhood_state_default_from_fortran().expect("default state should be available");
        let payload = nhood_state_to_values_payload(&state);
        let recovered =
            nhood_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = NhoodStateValuesPayload {
            schema_version: NHOOD_STATE_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = nhood_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
