use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use crate::ohm_prm::{ohm_prm_field_names, OhmPrm};
use crate::soil::{soil_prm_field_names, SoilPrm};
use crate::water_dist::{water_dist_prm_field_names, WaterDistPrm};
use std::collections::BTreeMap;

pub const LC_BLDG_PRM_FLAT_LEN: usize = 36;
pub const LC_BLDG_PRM_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LcBldgPrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type LcBldgPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct LcBldgPrm {
    pub sfr: f64,
    pub faibldg: f64,
    pub bldgh: f64,
    pub emis: f64,
    pub ohm: OhmPrm,
    pub soil: SoilPrm,
    pub state: f64,
    pub statelimit: f64,
    pub irrfracbldgs: f64,
    pub wetthresh: f64,
    pub waterdist: WaterDistPrm,
}

impl Default for LcBldgPrm {
    fn default() -> Self {
        Self {
            sfr: 0.0,
            faibldg: 0.0,
            bldgh: 0.0,
            emis: 0.0,
            ohm: OhmPrm::default(),
            soil: SoilPrm::default(),
            state: 0.0,
            statelimit: 0.0,
            irrfracbldgs: 0.0,
            wetthresh: 0.0,
            waterdist: WaterDistPrm::default(),
        }
    }
}

impl LcBldgPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, LC_BLDG_PRM_FLAT_LEN)?;

        Ok(Self {
            sfr: flat[0],
            faibldg: flat[1],
            bldgh: flat[2],
            emis: flat[3],
            ohm: OhmPrm::from_flat(&flat[4..21])?,
            soil: SoilPrm::from_flat(&flat[21..24])?,
            state: flat[24],
            statelimit: flat[25],
            irrfracbldgs: flat[26],
            wetthresh: flat[27],
            waterdist: WaterDistPrm::from_flat(&flat[28..36])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = vec![self.sfr, self.faibldg, self.bldgh, self.emis];
        out.extend(self.ohm.to_flat());
        out.extend(self.soil.to_flat());
        out.push(self.state);
        out.push(self.statelimit);
        out.push(self.irrfracbldgs);
        out.push(self.wetthresh);
        out.extend(self.waterdist.to_flat());
        out
    }
}

impl StateCodec for LcBldgPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "LC_BLDG_PRM".to_string(),
            schema_version: LC_BLDG_PRM_SCHEMA_VERSION,
            flat_len: LC_BLDG_PRM_FLAT_LEN,
            field_names: lc_bldg_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        LcBldgPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        LcBldgPrm::to_flat(self)
    }
}

pub fn lc_bldg_prm_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_lc_bldg_prm_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn lc_bldg_prm_schema_info() -> Result<LcBldgPrmSchema, BridgeError> {
    let flat_len = lc_bldg_prm_schema()?;
    let schema_version_runtime = lc_bldg_prm_schema_version_runtime()?;
    let field_names = lc_bldg_prm_field_names();

    if schema_version_runtime != LC_BLDG_PRM_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(LcBldgPrmSchema {
        schema_version: LC_BLDG_PRM_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn lc_bldg_prm_field_names() -> Vec<String> {
    let mut names = vec![
        "sfr".to_string(),
        "faibldg".to_string(),
        "bldgh".to_string(),
        "emis".to_string(),
    ];

    for field in ohm_prm_field_names() {
        names.push(format!("ohm.{field}"));
    }
    for field in soil_prm_field_names() {
        names.push(format!("soil.{field}"));
    }

    names.push("state".to_string());
    names.push("statelimit".to_string());
    names.push("irrfracbldgs".to_string());
    names.push("wetthresh".to_string());

    for field in water_dist_prm_field_names() {
        names.push(format!("waterdist.{field}"));
    }

    names
}

pub fn lc_bldg_prm_schema_version() -> u32 {
    LC_BLDG_PRM_SCHEMA_VERSION
}

pub fn lc_bldg_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_lc_bldg_prm_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn lc_bldg_prm_field_index(name: &str) -> Option<usize> {
    let names = lc_bldg_prm_field_names();
    field_index(&names, name)
}

pub fn lc_bldg_prm_to_map(state: &LcBldgPrm) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn lc_bldg_prm_to_ordered_values(state: &LcBldgPrm) -> Vec<f64> {
    state.to_flat()
}

pub fn lc_bldg_prm_from_ordered_values(values: &[f64]) -> Result<LcBldgPrm, BridgeError> {
    LcBldgPrm::from_flat(values)
}

pub fn lc_bldg_prm_to_values_payload(state: &LcBldgPrm) -> LcBldgPrmValuesPayload {
    to_values_payload(state)
}

pub fn lc_bldg_prm_from_values_payload(
    payload: &LcBldgPrmValuesPayload,
) -> Result<LcBldgPrm, BridgeError> {
    from_values_payload(payload)
}

pub fn lc_bldg_prm_from_map(values: &BTreeMap<String, f64>) -> Result<LcBldgPrm, BridgeError> {
    let default_state = lc_bldg_prm_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn lc_bldg_prm_default_from_fortran() -> Result<LcBldgPrm, BridgeError> {
    let n_flat = lc_bldg_prm_schema()?;
    if n_flat != LC_BLDG_PRM_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_lc_bldg_prm_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    LcBldgPrm::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = lc_bldg_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, LC_BLDG_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = lc_bldg_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, LcBldgPrm::default());

        let state2 = LcBldgPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = lc_bldg_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = lc_bldg_prm_to_map(&state);
        mapped.insert("faibldg".to_string(), 1.8);
        mapped.insert("ohm.chanohm".to_string(), 0.5);
        mapped.insert("waterdist.to_paved".to_string(), 0.2);

        let updated = lc_bldg_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.faibldg - 1.8).abs() < 1.0e-12);
        assert!((updated.ohm.chanohm - 0.5).abs() < 1.0e-12);
        assert!((updated.waterdist.to_paved - 0.2).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = lc_bldg_prm_default_from_fortran().expect("default state should be available");
        let payload = lc_bldg_prm_to_values_payload(&state);
        let recovered =
            lc_bldg_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = LcBldgPrmValuesPayload {
            schema_version: LC_BLDG_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = lc_bldg_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
