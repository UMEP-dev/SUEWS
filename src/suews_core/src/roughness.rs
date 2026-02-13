use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const ROUGHNESS_STATE_FLAT_LEN: usize = 11;
pub const ROUGHNESS_STATE_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RoughnessStateSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

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

pub fn roughness_state_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_roughness_state_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn roughness_state_schema_info() -> Result<RoughnessStateSchema, BridgeError> {
    let flat_len = roughness_state_schema()?;
    let schema_version_runtime = roughness_state_schema_version_runtime()?;
    let field_names = roughness_state_field_names();

    if schema_version_runtime != ROUGHNESS_STATE_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(RoughnessStateSchema {
        schema_version: ROUGHNESS_STATE_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
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

pub fn roughness_state_schema_version() -> u32 {
    ROUGHNESS_STATE_SCHEMA_VERSION
}

pub fn roughness_state_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_roughness_state_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn roughness_state_field_index(name: &str) -> Option<usize> {
    let names = roughness_state_field_names();
    field_index(&names, name)
}

pub fn roughness_state_to_map(state: &RoughnessState) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn roughness_state_to_ordered_values(state: &RoughnessState) -> Vec<f64> {
    state.to_flat()
}

pub fn roughness_state_from_ordered_values(values: &[f64]) -> Result<RoughnessState, BridgeError> {
    RoughnessState::from_flat(values)
}

pub fn roughness_state_to_values_payload(state: &RoughnessState) -> RoughnessStateValuesPayload {
    to_values_payload(state)
}

pub fn roughness_state_from_values_payload(
    payload: &RoughnessStateValuesPayload,
) -> Result<RoughnessState, BridgeError> {
    from_values_payload(payload)
}

pub fn roughness_state_from_map(
    values: &BTreeMap<String, f64>,
) -> Result<RoughnessState, BridgeError> {
    let default_state = roughness_state_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn roughness_state_default_from_fortran() -> Result<RoughnessState, BridgeError> {
    let n_flat = roughness_state_schema()?;
    if n_flat != ROUGHNESS_STATE_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_roughness_state_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    RoughnessState::from_flat(&flat)
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
