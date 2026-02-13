use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const BIOCO2_PRM_FLAT_LEN: usize = 8;
pub const BIOCO2_PRM_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BioCo2PrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type BioCo2PrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct BioCo2Prm {
    pub beta_bioco2: f64,
    pub beta_enh_bioco2: f64,
    pub alpha_bioco2: f64,
    pub alpha_enh_bioco2: f64,
    pub resp_a: f64,
    pub resp_b: f64,
    pub theta_bioco2: f64,
    pub min_res_bioco2: f64,
}

impl Default for BioCo2Prm {
    fn default() -> Self {
        Self {
            beta_bioco2: 0.0,
            beta_enh_bioco2: 0.0,
            alpha_bioco2: 0.0,
            alpha_enh_bioco2: 0.0,
            resp_a: 0.0,
            resp_b: 0.0,
            theta_bioco2: 0.0,
            min_res_bioco2: 0.0,
        }
    }
}

impl BioCo2Prm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, BIOCO2_PRM_FLAT_LEN)?;
        Ok(Self {
            beta_bioco2: flat[0],
            beta_enh_bioco2: flat[1],
            alpha_bioco2: flat[2],
            alpha_enh_bioco2: flat[3],
            resp_a: flat[4],
            resp_b: flat[5],
            theta_bioco2: flat[6],
            min_res_bioco2: flat[7],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.beta_bioco2,
            self.beta_enh_bioco2,
            self.alpha_bioco2,
            self.alpha_enh_bioco2,
            self.resp_a,
            self.resp_b,
            self.theta_bioco2,
            self.min_res_bioco2,
        ]
    }
}

impl StateCodec for BioCo2Prm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "bioCO2_PRM".to_string(),
            schema_version: BIOCO2_PRM_SCHEMA_VERSION,
            flat_len: BIOCO2_PRM_FLAT_LEN,
            field_names: bioco2_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        BioCo2Prm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        BioCo2Prm::to_flat(self)
    }
}

pub fn bioco2_prm_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_bioco2_prm_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn bioco2_prm_schema_info() -> Result<BioCo2PrmSchema, BridgeError> {
    let flat_len = bioco2_prm_schema()?;
    let schema_version_runtime = bioco2_prm_schema_version_runtime()?;
    let field_names = bioco2_prm_field_names();

    if schema_version_runtime != BIOCO2_PRM_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(BioCo2PrmSchema {
        schema_version: BIOCO2_PRM_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn bioco2_prm_field_names() -> Vec<String> {
    vec![
        "beta_bioco2".to_string(),
        "beta_enh_bioco2".to_string(),
        "alpha_bioco2".to_string(),
        "alpha_enh_bioco2".to_string(),
        "resp_a".to_string(),
        "resp_b".to_string(),
        "theta_bioco2".to_string(),
        "min_res_bioco2".to_string(),
    ]
}

pub fn bioco2_prm_schema_version() -> u32 {
    BIOCO2_PRM_SCHEMA_VERSION
}

pub fn bioco2_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_bioco2_prm_schema_version(&mut schema_version as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn bioco2_prm_field_index(name: &str) -> Option<usize> {
    let names = bioco2_prm_field_names();
    field_index(&names, name)
}

pub fn bioco2_prm_to_map(state: &BioCo2Prm) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn bioco2_prm_to_ordered_values(state: &BioCo2Prm) -> Vec<f64> {
    state.to_flat()
}

pub fn bioco2_prm_from_ordered_values(values: &[f64]) -> Result<BioCo2Prm, BridgeError> {
    BioCo2Prm::from_flat(values)
}

pub fn bioco2_prm_to_values_payload(state: &BioCo2Prm) -> BioCo2PrmValuesPayload {
    to_values_payload(state)
}

pub fn bioco2_prm_from_values_payload(
    payload: &BioCo2PrmValuesPayload,
) -> Result<BioCo2Prm, BridgeError> {
    from_values_payload(payload)
}

pub fn bioco2_prm_from_map(values: &BTreeMap<String, f64>) -> Result<BioCo2Prm, BridgeError> {
    let default_state = bioco2_prm_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn bioco2_prm_default_from_fortran() -> Result<BioCo2Prm, BridgeError> {
    let n_flat = bioco2_prm_schema()?;
    if n_flat != BIOCO2_PRM_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_bioco2_prm_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    BioCo2Prm::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = bioco2_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, BIOCO2_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = bioco2_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, BioCo2Prm::default());

        let state2 = BioCo2Prm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = bioco2_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = bioco2_prm_to_map(&state);
        mapped.insert("beta_bioco2".to_string(), 0.8);
        mapped.insert("resp_b".to_string(), 0.02);

        let updated = bioco2_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.beta_bioco2 - 0.8).abs() < 1.0e-12);
        assert!((updated.resp_b - 0.02).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = bioco2_prm_default_from_fortran().expect("default state should be available");
        let payload = bioco2_prm_to_values_payload(&state);
        let recovered =
            bioco2_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = BioCo2PrmValuesPayload {
            schema_version: BIOCO2_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = bioco2_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
