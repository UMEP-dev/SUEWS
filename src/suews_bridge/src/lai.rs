use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const LAI_PRM_FLAT_LEN: usize = 11;
pub const LAI_PRM_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LaiPrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type LaiPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct LaiPrm {
    pub baset: f64,
    pub gddfull: f64,
    pub basete: f64,
    pub sddfull: f64,
    pub laimin: f64,
    pub laimax: f64,
    pub laipower: [f64; 4],
    pub laitype: i32,
}

impl Default for LaiPrm {
    fn default() -> Self {
        Self {
            baset: 0.0,
            gddfull: 0.0,
            basete: 0.0,
            sddfull: 0.0,
            laimin: 0.0,
            laimax: 0.0,
            laipower: [0.0; 4],
            laitype: 0,
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

impl LaiPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, LAI_PRM_FLAT_LEN)?;
        Ok(Self {
            baset: flat[0],
            gddfull: flat[1],
            basete: flat[2],
            sddfull: flat[3],
            laimin: flat[4],
            laimax: flat[5],
            laipower: [flat[6], flat[7], flat[8], flat[9]],
            laitype: decode_int(flat[10])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.baset,
            self.gddfull,
            self.basete,
            self.sddfull,
            self.laimin,
            self.laimax,
            self.laipower[0],
            self.laipower[1],
            self.laipower[2],
            self.laipower[3],
            self.laitype as f64,
        ]
    }
}

impl StateCodec for LaiPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "LAI_PRM".to_string(),
            schema_version: LAI_PRM_SCHEMA_VERSION,
            flat_len: LAI_PRM_FLAT_LEN,
            field_names: lai_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        LaiPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        LaiPrm::to_flat(self)
    }
}

pub fn lai_prm_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_lai_prm_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn lai_prm_schema_info() -> Result<LaiPrmSchema, BridgeError> {
    let flat_len = lai_prm_schema()?;
    let schema_version_runtime = lai_prm_schema_version_runtime()?;
    let field_names = lai_prm_field_names();

    if schema_version_runtime != LAI_PRM_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(LaiPrmSchema {
        schema_version: LAI_PRM_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn lai_prm_field_names() -> Vec<String> {
    vec![
        "baset".to_string(),
        "gddfull".to_string(),
        "basete".to_string(),
        "sddfull".to_string(),
        "laimin".to_string(),
        "laimax".to_string(),
        "laipower_1".to_string(),
        "laipower_2".to_string(),
        "laipower_3".to_string(),
        "laipower_4".to_string(),
        "laitype".to_string(),
    ]
}

pub fn lai_prm_schema_version() -> u32 {
    LAI_PRM_SCHEMA_VERSION
}

pub fn lai_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_lai_prm_schema_version(&mut schema_version as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn lai_prm_field_index(name: &str) -> Option<usize> {
    let names = lai_prm_field_names();
    field_index(&names, name)
}

pub fn lai_prm_to_map(state: &LaiPrm) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn lai_prm_to_ordered_values(state: &LaiPrm) -> Vec<f64> {
    state.to_flat()
}

pub fn lai_prm_from_ordered_values(values: &[f64]) -> Result<LaiPrm, BridgeError> {
    LaiPrm::from_flat(values)
}

pub fn lai_prm_to_values_payload(state: &LaiPrm) -> LaiPrmValuesPayload {
    to_values_payload(state)
}

pub fn lai_prm_from_values_payload(payload: &LaiPrmValuesPayload) -> Result<LaiPrm, BridgeError> {
    from_values_payload(payload)
}

pub fn lai_prm_from_map(values: &BTreeMap<String, f64>) -> Result<LaiPrm, BridgeError> {
    let default_state = lai_prm_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn lai_prm_default_from_fortran() -> Result<LaiPrm, BridgeError> {
    let n_flat = lai_prm_schema()?;
    if n_flat != LAI_PRM_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_lai_prm_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    LaiPrm::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = lai_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, LAI_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = lai_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, LaiPrm::default());

        let state2 = LaiPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = lai_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = lai_prm_to_map(&state);
        mapped.insert("laimax".to_string(), 5.0);
        mapped.insert("laipower_3".to_string(), 0.6);
        mapped.insert("laitype".to_string(), 1.0);

        let updated = lai_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.laimax - 5.0).abs() < 1.0e-12);
        assert!((updated.laipower[2] - 0.6).abs() < 1.0e-12);
        assert_eq!(updated.laitype, 1);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = lai_prm_default_from_fortran().expect("default state should be available");
        let payload = lai_prm_to_values_payload(&state);
        let recovered = lai_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = LaiPrmValuesPayload {
            schema_version: LAI_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = lai_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn from_flat_rejects_non_integer_laitype() {
        let mut flat = vec![0.0_f64; LAI_PRM_FLAT_LEN];
        flat[10] = 1.5;
        let err = LaiPrm::from_flat(&flat).expect_err("fractional integer should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
