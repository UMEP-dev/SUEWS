use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const ROUGHNESS_STATE_FLAT_LEN: usize = 11;
pub const ROUGHNESS_STATE_SCHEMA_VERSION: u32 = 1;

pub type RoughnessStateSchema = crate::codec::SimpleSchema;

pub type RoughnessStateValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct RoughnessState {
    pub faibldg_use: f64,
    pub faievetree_use: f64,
    pub faidectree_use: f64,
    pub fai: f64,
    pub pai: f64,
    pub zh: f64,
    pub z0m: f64,
    pub z0v: f64,
    pub zdm: f64,
    pub zzd: f64,
    pub iter_safe: bool,
}

impl Default for RoughnessState {
    fn default() -> Self {
        Self {
            faibldg_use: 0.0,
            faievetree_use: 0.0,
            faidectree_use: 0.0,
            fai: 0.0,
            pai: 0.0,
            zh: 0.0,
            z0m: 0.0,
            z0v: 0.0,
            zdm: 0.0,
            zzd: 0.0,
            iter_safe: true,
        }
    }
}

impl RoughnessState {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, ROUGHNESS_STATE_FLAT_LEN)?;
        Ok(Self {
            faibldg_use: flat[0],
            faievetree_use: flat[1],
            faidectree_use: flat[2],
            fai: flat[3],
            pai: flat[4],
            zh: flat[5],
            z0m: flat[6],
            z0v: flat[7],
            zdm: flat[8],
            zzd: flat[9],
            iter_safe: flat[10] >= 0.5,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.faibldg_use,
            self.faievetree_use,
            self.faidectree_use,
            self.fai,
            self.pai,
            self.zh,
            self.z0m,
            self.z0v,
            self.zdm,
            self.zzd,
            if self.iter_safe { 1.0 } else { 0.0 },
        ]
    }
}

impl StateCodec for RoughnessState {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "ROUGHNESS_STATE".to_string(),
            schema_version: ROUGHNESS_STATE_SCHEMA_VERSION,
            flat_len: ROUGHNESS_STATE_FLAT_LEN,
            field_names: roughness_state_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        RoughnessState::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        RoughnessState::to_flat(self)
    }
}

pub fn roughness_state_field_names() -> Vec<String> {
    vec![
        "faibldg_use".to_string(),
        "faievetree_use".to_string(),
        "faidectree_use".to_string(),
        "fai".to_string(),
        "pai".to_string(),
        "zh".to_string(),
        "z0m".to_string(),
        "z0v".to_string(),
        "zdm".to_string(),
        "zzd".to_string(),
        "iter_safe".to_string(),
    ]
}

crate::codec::impl_state_module_fns! {
    prefix = roughness_state,
    state_type = RoughnessState,
    schema_type = RoughnessStateSchema,
    payload_type = RoughnessStateValuesPayload,
    flat_len_const = ROUGHNESS_STATE_FLAT_LEN,
    schema_version_const = ROUGHNESS_STATE_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_roughness_state_len,
    ffi_schema_version_fn = ffi::suews_roughness_state_schema_version,
    ffi_default_fn = ffi::suews_roughness_state_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = roughness_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, ROUGHNESS_STATE_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            roughness_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, RoughnessState::default());
        let state2 =
            RoughnessState::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            roughness_state_default_from_fortran().expect("default state should be available");
        let mut mapped = roughness_state_to_map(&state);
        mapped.insert("z0m".to_string(), 0.42);
        mapped.insert("iter_safe".to_string(), 0.0);

        let updated = roughness_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.z0m - 0.42).abs() < 1.0e-12);
        assert!(!updated.iter_safe);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            roughness_state_default_from_fortran().expect("default state should be available");
        let payload = roughness_state_to_values_payload(&state);
        let recovered =
            roughness_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = RoughnessStateValuesPayload {
            schema_version: ROUGHNESS_STATE_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = roughness_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
