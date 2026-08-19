use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const SURF_STORE_PRM_FLAT_LEN: usize = 6;
pub const SURF_STORE_PRM_SCHEMA_VERSION: u32 = 1;

pub type SurfStorePrmSchema = crate::codec::SimpleSchema;

pub type SurfStorePrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct SurfStorePrm {
    pub store_min: f64,
    pub store_max: f64,
    pub store_cap: f64,
    pub drain_eq: i32,
    pub drain_coef_1: f64,
    pub drain_coef_2: f64,
}

impl Default for SurfStorePrm {
    fn default() -> Self {
        Self {
            store_min: 0.0,
            store_max: 0.0,
            store_cap: 0.0,
            drain_eq: 0,
            drain_coef_1: 0.0,
            drain_coef_2: 0.0,
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

impl SurfStorePrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, SURF_STORE_PRM_FLAT_LEN)?;
        Ok(Self {
            store_min: flat[0],
            store_max: flat[1],
            store_cap: flat[2],
            drain_eq: decode_int(flat[3])?,
            drain_coef_1: flat[4],
            drain_coef_2: flat[5],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.store_min,
            self.store_max,
            self.store_cap,
            self.drain_eq as f64,
            self.drain_coef_1,
            self.drain_coef_2,
        ]
    }
}

impl StateCodec for SurfStorePrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "SURF_STORE_PRM".to_string(),
            schema_version: SURF_STORE_PRM_SCHEMA_VERSION,
            flat_len: SURF_STORE_PRM_FLAT_LEN,
            field_names: surf_store_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        SurfStorePrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        SurfStorePrm::to_flat(self)
    }
}

pub fn surf_store_prm_field_names() -> Vec<String> {
    vec![
        "store_min".to_string(),
        "store_max".to_string(),
        "store_cap".to_string(),
        "drain_eq".to_string(),
        "drain_coef_1".to_string(),
        "drain_coef_2".to_string(),
    ]
}

crate::codec::impl_state_module_fns! {
    prefix = surf_store_prm,
    state_type = SurfStorePrm,
    schema_type = SurfStorePrmSchema,
    payload_type = SurfStorePrmValuesPayload,
    flat_len_const = SURF_STORE_PRM_FLAT_LEN,
    schema_version_const = SURF_STORE_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_surf_store_prm_len,
    ffi_schema_version_fn = ffi::suews_surf_store_prm_schema_version,
    ffi_default_fn = ffi::suews_surf_store_prm_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = surf_store_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, SURF_STORE_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            surf_store_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, SurfStorePrm::default());

        let state2 =
            SurfStorePrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            surf_store_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = surf_store_prm_to_map(&state);
        mapped.insert("store_cap".to_string(), 2.5);
        mapped.insert("drain_eq".to_string(), 3.0);

        let updated = surf_store_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.store_cap - 2.5).abs() < 1.0e-12);
        assert_eq!(updated.drain_eq, 3);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            surf_store_prm_default_from_fortran().expect("default state should be available");
        let payload = surf_store_prm_to_values_payload(&state);
        let recovered =
            surf_store_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = SurfStorePrmValuesPayload {
            schema_version: SURF_STORE_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = surf_store_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn from_flat_rejects_non_integer_drain_eq() {
        let mut flat = vec![0.0_f64; SURF_STORE_PRM_FLAT_LEN];
        flat[3] = 1.5;
        let err = SurfStorePrm::from_flat(&flat).expect_err("fractional integer should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
