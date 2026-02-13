use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use crate::ohm_coef_lc::OhmCoefLc;
use std::collections::BTreeMap;

pub const OHM_PRM_FLAT_LEN: usize = 17;
pub const OHM_PRM_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OhmPrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type OhmPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct OhmPrm {
    pub chanohm: f64,
    pub cpanohm: f64,
    pub kkanohm: f64,
    pub ohm_threshsw: f64,
    pub ohm_threshwd: f64,
    pub ohm_coef_lc: [OhmCoefLc; 3],
}

impl Default for OhmPrm {
    fn default() -> Self {
        Self {
            chanohm: 0.0,
            cpanohm: 0.0,
            kkanohm: 0.0,
            ohm_threshsw: 0.0,
            ohm_threshwd: 0.0,
            ohm_coef_lc: [
                OhmCoefLc::default(),
                OhmCoefLc::default(),
                OhmCoefLc::default(),
            ],
        }
    }
}

impl OhmPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, OHM_PRM_FLAT_LEN)?;

        Ok(Self {
            chanohm: flat[0],
            cpanohm: flat[1],
            kkanohm: flat[2],
            ohm_threshsw: flat[3],
            ohm_threshwd: flat[4],
            ohm_coef_lc: [
                OhmCoefLc::from_flat(&flat[5..9])?,
                OhmCoefLc::from_flat(&flat[9..13])?,
                OhmCoefLc::from_flat(&flat[13..17])?,
            ],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = vec![
            self.chanohm,
            self.cpanohm,
            self.kkanohm,
            self.ohm_threshsw,
            self.ohm_threshwd,
        ];
        out.extend(self.ohm_coef_lc[0].to_flat());
        out.extend(self.ohm_coef_lc[1].to_flat());
        out.extend(self.ohm_coef_lc[2].to_flat());
        out
    }
}

impl StateCodec for OhmPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "OHM_PRM".to_string(),
            schema_version: OHM_PRM_SCHEMA_VERSION,
            flat_len: OHM_PRM_FLAT_LEN,
            field_names: ohm_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        OhmPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        OhmPrm::to_flat(self)
    }
}

pub fn ohm_prm_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_prm_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn ohm_prm_schema_info() -> Result<OhmPrmSchema, BridgeError> {
    let flat_len = ohm_prm_schema()?;
    let schema_version_runtime = ohm_prm_schema_version_runtime()?;
    let field_names = ohm_prm_field_names();

    if schema_version_runtime != OHM_PRM_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(OhmPrmSchema {
        schema_version: OHM_PRM_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn ohm_prm_field_names() -> Vec<String> {
    vec![
        "chanohm".to_string(),
        "cpanohm".to_string(),
        "kkanohm".to_string(),
        "ohm_threshsw".to_string(),
        "ohm_threshwd".to_string(),
        "ohm_coef_lc_1.summer_dry".to_string(),
        "ohm_coef_lc_1.summer_wet".to_string(),
        "ohm_coef_lc_1.winter_dry".to_string(),
        "ohm_coef_lc_1.winter_wet".to_string(),
        "ohm_coef_lc_2.summer_dry".to_string(),
        "ohm_coef_lc_2.summer_wet".to_string(),
        "ohm_coef_lc_2.winter_dry".to_string(),
        "ohm_coef_lc_2.winter_wet".to_string(),
        "ohm_coef_lc_3.summer_dry".to_string(),
        "ohm_coef_lc_3.summer_wet".to_string(),
        "ohm_coef_lc_3.winter_dry".to_string(),
        "ohm_coef_lc_3.winter_wet".to_string(),
    ]
}

pub fn ohm_prm_schema_version() -> u32 {
    OHM_PRM_SCHEMA_VERSION
}

pub fn ohm_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_prm_schema_version(&mut schema_version as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn ohm_prm_field_index(name: &str) -> Option<usize> {
    let names = ohm_prm_field_names();
    field_index(&names, name)
}

pub fn ohm_prm_to_map(state: &OhmPrm) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn ohm_prm_to_ordered_values(state: &OhmPrm) -> Vec<f64> {
    state.to_flat()
}

pub fn ohm_prm_from_ordered_values(values: &[f64]) -> Result<OhmPrm, BridgeError> {
    OhmPrm::from_flat(values)
}

pub fn ohm_prm_to_values_payload(state: &OhmPrm) -> OhmPrmValuesPayload {
    to_values_payload(state)
}

pub fn ohm_prm_from_values_payload(payload: &OhmPrmValuesPayload) -> Result<OhmPrm, BridgeError> {
    from_values_payload(payload)
}

pub fn ohm_prm_from_map(values: &BTreeMap<String, f64>) -> Result<OhmPrm, BridgeError> {
    let default_state = ohm_prm_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn ohm_prm_default_from_fortran() -> Result<OhmPrm, BridgeError> {
    let n_flat = ohm_prm_schema()?;
    if n_flat != OHM_PRM_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_prm_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    OhmPrm::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = ohm_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, OHM_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = ohm_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, OhmPrm::default());

        let state2 = OhmPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = ohm_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = ohm_prm_to_map(&state);
        mapped.insert("ohm_threshsw".to_string(), 15.0);
        mapped.insert("ohm_coef_lc_2.winter_wet".to_string(), 0.25);

        let updated = ohm_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.ohm_threshsw - 15.0).abs() < 1.0e-12);
        assert!((updated.ohm_coef_lc[1].winter_wet - 0.25).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = ohm_prm_default_from_fortran().expect("default state should be available");
        let payload = ohm_prm_to_values_payload(&state);
        let recovered = ohm_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = OhmPrmValuesPayload {
            schema_version: OHM_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = ohm_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
