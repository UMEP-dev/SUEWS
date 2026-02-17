use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const OHM_COEF_LC_FLAT_LEN: usize = 4;
pub const OHM_COEF_LC_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OhmCoefLcSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type OhmCoefLcValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct OhmCoefLc {
    pub summer_dry: f64,
    pub summer_wet: f64,
    pub winter_dry: f64,
    pub winter_wet: f64,
}

impl Default for OhmCoefLc {
    fn default() -> Self {
        Self {
            summer_dry: 0.0,
            summer_wet: 0.0,
            winter_dry: 0.0,
            winter_wet: 0.0,
        }
    }
}

impl OhmCoefLc {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, OHM_COEF_LC_FLAT_LEN)?;
        Ok(Self {
            summer_dry: flat[0],
            summer_wet: flat[1],
            winter_dry: flat[2],
            winter_wet: flat[3],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.summer_dry,
            self.summer_wet,
            self.winter_dry,
            self.winter_wet,
        ]
    }
}

impl StateCodec for OhmCoefLc {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "OHM_COEF_LC".to_string(),
            schema_version: OHM_COEF_LC_SCHEMA_VERSION,
            flat_len: OHM_COEF_LC_FLAT_LEN,
            field_names: ohm_coef_lc_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        OhmCoefLc::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        OhmCoefLc::to_flat(self)
    }
}

pub fn ohm_coef_lc_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_coef_lc_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn ohm_coef_lc_schema_info() -> Result<OhmCoefLcSchema, BridgeError> {
    let flat_len = ohm_coef_lc_schema()?;
    let schema_version_runtime = ohm_coef_lc_schema_version_runtime()?;
    let field_names = ohm_coef_lc_field_names();

    if schema_version_runtime != OHM_COEF_LC_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(OhmCoefLcSchema {
        schema_version: OHM_COEF_LC_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn ohm_coef_lc_field_names() -> Vec<String> {
    vec![
        "summer_dry".to_string(),
        "summer_wet".to_string(),
        "winter_dry".to_string(),
        "winter_wet".to_string(),
    ]
}

pub fn ohm_coef_lc_schema_version() -> u32 {
    OHM_COEF_LC_SCHEMA_VERSION
}

pub fn ohm_coef_lc_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_coef_lc_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn ohm_coef_lc_field_index(name: &str) -> Option<usize> {
    let names = ohm_coef_lc_field_names();
    field_index(&names, name)
}

pub fn ohm_coef_lc_to_map(state: &OhmCoefLc) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn ohm_coef_lc_to_ordered_values(state: &OhmCoefLc) -> Vec<f64> {
    state.to_flat()
}

pub fn ohm_coef_lc_from_ordered_values(values: &[f64]) -> Result<OhmCoefLc, BridgeError> {
    OhmCoefLc::from_flat(values)
}

pub fn ohm_coef_lc_to_values_payload(state: &OhmCoefLc) -> OhmCoefLcValuesPayload {
    to_values_payload(state)
}

pub fn ohm_coef_lc_from_values_payload(
    payload: &OhmCoefLcValuesPayload,
) -> Result<OhmCoefLc, BridgeError> {
    from_values_payload(payload)
}

pub fn ohm_coef_lc_from_map(values: &BTreeMap<String, f64>) -> Result<OhmCoefLc, BridgeError> {
    let default_state = ohm_coef_lc_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn ohm_coef_lc_default_from_fortran() -> Result<OhmCoefLc, BridgeError> {
    let n_flat = ohm_coef_lc_schema()?;
    if n_flat != OHM_COEF_LC_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_coef_lc_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    OhmCoefLc::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = ohm_coef_lc_schema().expect("schema call should succeed");
        assert_eq!(n_flat, OHM_COEF_LC_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = ohm_coef_lc_default_from_fortran().expect("default state should be available");
        assert_eq!(state, OhmCoefLc::default());

        let state2 = OhmCoefLc::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = ohm_coef_lc_default_from_fortran().expect("default state should be available");
        let mut mapped = ohm_coef_lc_to_map(&state);
        mapped.insert("summer_dry".to_string(), 0.11);
        mapped.insert("winter_wet".to_string(), 0.44);

        let updated = ohm_coef_lc_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.summer_dry - 0.11).abs() < 1.0e-12);
        assert!((updated.winter_wet - 0.44).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = ohm_coef_lc_default_from_fortran().expect("default state should be available");
        let payload = ohm_coef_lc_to_values_payload(&state);
        let recovered =
            ohm_coef_lc_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = OhmCoefLcValuesPayload {
            schema_version: OHM_COEF_LC_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = ohm_coef_lc_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
