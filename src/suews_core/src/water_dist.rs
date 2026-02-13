use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const WATER_DIST_PRM_FLAT_LEN: usize = 8;
pub const WATER_DIST_PRM_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WaterDistPrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type WaterDistPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct WaterDistPrm {
    pub to_paved: f64,
    pub to_bldg: f64,
    pub to_evetr: f64,
    pub to_dectr: f64,
    pub to_grass: f64,
    pub to_bsoil: f64,
    pub to_water: f64,
    pub to_soilstore: f64,
}

impl Default for WaterDistPrm {
    fn default() -> Self {
        Self {
            to_paved: 0.0,
            to_bldg: 0.0,
            to_evetr: 0.0,
            to_dectr: 0.0,
            to_grass: 0.0,
            to_bsoil: 0.0,
            to_water: 0.0,
            to_soilstore: 0.0,
        }
    }
}

impl WaterDistPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, WATER_DIST_PRM_FLAT_LEN)?;
        Ok(Self {
            to_paved: flat[0],
            to_bldg: flat[1],
            to_evetr: flat[2],
            to_dectr: flat[3],
            to_grass: flat[4],
            to_bsoil: flat[5],
            to_water: flat[6],
            to_soilstore: flat[7],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.to_paved,
            self.to_bldg,
            self.to_evetr,
            self.to_dectr,
            self.to_grass,
            self.to_bsoil,
            self.to_water,
            self.to_soilstore,
        ]
    }
}

impl StateCodec for WaterDistPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "WATER_DIST_PRM".to_string(),
            schema_version: WATER_DIST_PRM_SCHEMA_VERSION,
            flat_len: WATER_DIST_PRM_FLAT_LEN,
            field_names: water_dist_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        WaterDistPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        WaterDistPrm::to_flat(self)
    }
}

pub fn water_dist_prm_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_water_dist_prm_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn water_dist_prm_schema_info() -> Result<WaterDistPrmSchema, BridgeError> {
    let flat_len = water_dist_prm_schema()?;
    let schema_version_runtime = water_dist_prm_schema_version_runtime()?;
    let field_names = water_dist_prm_field_names();

    if schema_version_runtime != WATER_DIST_PRM_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(WaterDistPrmSchema {
        schema_version: WATER_DIST_PRM_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn water_dist_prm_field_names() -> Vec<String> {
    vec![
        "to_paved".to_string(),
        "to_bldg".to_string(),
        "to_evetr".to_string(),
        "to_dectr".to_string(),
        "to_grass".to_string(),
        "to_bsoil".to_string(),
        "to_water".to_string(),
        "to_soilstore".to_string(),
    ]
}

pub fn water_dist_prm_schema_version() -> u32 {
    WATER_DIST_PRM_SCHEMA_VERSION
}

pub fn water_dist_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_water_dist_prm_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn water_dist_prm_field_index(name: &str) -> Option<usize> {
    let names = water_dist_prm_field_names();
    field_index(&names, name)
}

pub fn water_dist_prm_to_map(state: &WaterDistPrm) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn water_dist_prm_to_ordered_values(state: &WaterDistPrm) -> Vec<f64> {
    state.to_flat()
}

pub fn water_dist_prm_from_ordered_values(values: &[f64]) -> Result<WaterDistPrm, BridgeError> {
    WaterDistPrm::from_flat(values)
}

pub fn water_dist_prm_to_values_payload(state: &WaterDistPrm) -> WaterDistPrmValuesPayload {
    to_values_payload(state)
}

pub fn water_dist_prm_from_values_payload(
    payload: &WaterDistPrmValuesPayload,
) -> Result<WaterDistPrm, BridgeError> {
    from_values_payload(payload)
}

pub fn water_dist_prm_from_map(
    values: &BTreeMap<String, f64>,
) -> Result<WaterDistPrm, BridgeError> {
    let default_state = water_dist_prm_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn water_dist_prm_default_from_fortran() -> Result<WaterDistPrm, BridgeError> {
    let n_flat = water_dist_prm_schema()?;
    if n_flat != WATER_DIST_PRM_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_water_dist_prm_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    WaterDistPrm::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = water_dist_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, WATER_DIST_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            water_dist_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, WaterDistPrm::default());

        let state2 =
            WaterDistPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            water_dist_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = water_dist_prm_to_map(&state);
        mapped.insert("to_paved".to_string(), 0.3);
        mapped.insert("to_soilstore".to_string(), 0.1);

        let updated = water_dist_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.to_paved - 0.3).abs() < 1.0e-12);
        assert!((updated.to_soilstore - 0.1).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            water_dist_prm_default_from_fortran().expect("default state should be available");
        let payload = water_dist_prm_to_values_payload(&state);
        let recovered =
            water_dist_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = WaterDistPrmValuesPayload {
            schema_version: WATER_DIST_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = water_dist_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
