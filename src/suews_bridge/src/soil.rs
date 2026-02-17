use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const SOIL_PRM_FLAT_LEN: usize = 3;
pub const SOIL_PRM_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SoilPrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type SoilPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct SoilPrm {
    pub soildepth: f64,
    pub soilstorecap: f64,
    pub sathydraulicconduct: f64,
}

impl Default for SoilPrm {
    fn default() -> Self {
        Self {
            soildepth: 0.0,
            soilstorecap: 0.0,
            sathydraulicconduct: 0.0,
        }
    }
}

impl SoilPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, SOIL_PRM_FLAT_LEN)?;
        Ok(Self {
            soildepth: flat[0],
            soilstorecap: flat[1],
            sathydraulicconduct: flat[2],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![self.soildepth, self.soilstorecap, self.sathydraulicconduct]
    }
}

impl StateCodec for SoilPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "SOIL_PRM".to_string(),
            schema_version: SOIL_PRM_SCHEMA_VERSION,
            flat_len: SOIL_PRM_FLAT_LEN,
            field_names: soil_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        SoilPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        SoilPrm::to_flat(self)
    }
}

pub fn soil_prm_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_soil_prm_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn soil_prm_schema_info() -> Result<SoilPrmSchema, BridgeError> {
    let flat_len = soil_prm_schema()?;
    let schema_version_runtime = soil_prm_schema_version_runtime()?;
    let field_names = soil_prm_field_names();

    if schema_version_runtime != SOIL_PRM_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(SoilPrmSchema {
        schema_version: SOIL_PRM_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn soil_prm_field_names() -> Vec<String> {
    vec![
        "soildepth".to_string(),
        "soilstorecap".to_string(),
        "sathydraulicconduct".to_string(),
    ]
}

pub fn soil_prm_schema_version() -> u32 {
    SOIL_PRM_SCHEMA_VERSION
}

pub fn soil_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_soil_prm_schema_version(&mut schema_version as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn soil_prm_field_index(name: &str) -> Option<usize> {
    let names = soil_prm_field_names();
    field_index(&names, name)
}

pub fn soil_prm_to_map(state: &SoilPrm) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn soil_prm_to_ordered_values(state: &SoilPrm) -> Vec<f64> {
    state.to_flat()
}

pub fn soil_prm_from_ordered_values(values: &[f64]) -> Result<SoilPrm, BridgeError> {
    SoilPrm::from_flat(values)
}

pub fn soil_prm_to_values_payload(state: &SoilPrm) -> SoilPrmValuesPayload {
    to_values_payload(state)
}

pub fn soil_prm_from_values_payload(
    payload: &SoilPrmValuesPayload,
) -> Result<SoilPrm, BridgeError> {
    from_values_payload(payload)
}

pub fn soil_prm_from_map(values: &BTreeMap<String, f64>) -> Result<SoilPrm, BridgeError> {
    let default_state = soil_prm_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn soil_prm_default_from_fortran() -> Result<SoilPrm, BridgeError> {
    let n_flat = soil_prm_schema()?;
    if n_flat != SOIL_PRM_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_soil_prm_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    SoilPrm::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = soil_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, SOIL_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = soil_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, SoilPrm::default());
        let state2 = SoilPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = soil_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = soil_prm_to_map(&state);
        mapped.insert("soildepth".to_string(), 450.0);
        mapped.insert("sathydraulicconduct".to_string(), 0.003);

        let updated = soil_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.soildepth - 450.0).abs() < 1.0e-12);
        assert!((updated.sathydraulicconduct - 0.003).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = soil_prm_default_from_fortran().expect("default state should be available");
        let payload = soil_prm_to_values_payload(&state);
        let recovered = soil_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = SoilPrmValuesPayload {
            schema_version: SOIL_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = soil_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
