use crate::bioco2::{bioco2_prm_field_names, BioCo2Prm};
use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use crate::lai::{lai_prm_field_names, LaiPrm};
use crate::ohm_prm::{ohm_prm_field_names, OhmPrm};
use crate::soil::{soil_prm_field_names, SoilPrm};
use crate::water_dist::{water_dist_prm_field_names, WaterDistPrm};
use std::collections::BTreeMap;

pub const LC_EVETR_PRM_FLAT_LEN: usize = 57;
pub const LC_EVETR_PRM_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LcEvetrPrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type LcEvetrPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct LcEvetrPrm {
    pub sfr: f64,
    pub emis: f64,
    pub faievetree: f64,
    pub evetreeh: f64,
    pub alb_min: f64,
    pub alb_max: f64,
    pub ohm: OhmPrm,
    pub soil: SoilPrm,
    pub statelimit: f64,
    pub irrfracevetr: f64,
    pub wetthresh: f64,
    pub bioco2: BioCo2Prm,
    pub maxconductance: f64,
    pub lai: LaiPrm,
    pub waterdist: WaterDistPrm,
}

impl Default for LcEvetrPrm {
    fn default() -> Self {
        Self {
            sfr: 0.0,
            emis: 0.0,
            faievetree: 0.0,
            evetreeh: 0.0,
            alb_min: 0.0,
            alb_max: 0.0,
            ohm: OhmPrm::default(),
            soil: SoilPrm::default(),
            statelimit: 0.0,
            irrfracevetr: 0.0,
            wetthresh: 0.0,
            bioco2: BioCo2Prm::default(),
            maxconductance: 0.0,
            lai: LaiPrm::default(),
            waterdist: WaterDistPrm::default(),
        }
    }
}

impl LcEvetrPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, LC_EVETR_PRM_FLAT_LEN)?;

        Ok(Self {
            sfr: flat[0],
            emis: flat[1],
            faievetree: flat[2],
            evetreeh: flat[3],
            alb_min: flat[4],
            alb_max: flat[5],
            ohm: OhmPrm::from_flat(&flat[6..23])?,
            soil: SoilPrm::from_flat(&flat[23..26])?,
            statelimit: flat[26],
            irrfracevetr: flat[27],
            wetthresh: flat[28],
            bioco2: BioCo2Prm::from_flat(&flat[29..37])?,
            maxconductance: flat[37],
            lai: LaiPrm::from_flat(&flat[38..49])?,
            waterdist: WaterDistPrm::from_flat(&flat[49..57])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = vec![
            self.sfr,
            self.emis,
            self.faievetree,
            self.evetreeh,
            self.alb_min,
            self.alb_max,
        ];
        out.extend(self.ohm.to_flat());
        out.extend(self.soil.to_flat());
        out.push(self.statelimit);
        out.push(self.irrfracevetr);
        out.push(self.wetthresh);
        out.extend(self.bioco2.to_flat());
        out.push(self.maxconductance);
        out.extend(self.lai.to_flat());
        out.extend(self.waterdist.to_flat());
        out
    }
}

impl StateCodec for LcEvetrPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "LC_EVETR_PRM".to_string(),
            schema_version: LC_EVETR_PRM_SCHEMA_VERSION,
            flat_len: LC_EVETR_PRM_FLAT_LEN,
            field_names: lc_evetr_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        LcEvetrPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        LcEvetrPrm::to_flat(self)
    }
}

pub fn lc_evetr_prm_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_lc_evetr_prm_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn lc_evetr_prm_schema_info() -> Result<LcEvetrPrmSchema, BridgeError> {
    let flat_len = lc_evetr_prm_schema()?;
    let schema_version_runtime = lc_evetr_prm_schema_version_runtime()?;
    let field_names = lc_evetr_prm_field_names();

    if schema_version_runtime != LC_EVETR_PRM_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(LcEvetrPrmSchema {
        schema_version: LC_EVETR_PRM_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn lc_evetr_prm_field_names() -> Vec<String> {
    let mut names = vec![
        "sfr".to_string(),
        "emis".to_string(),
        "faievetree".to_string(),
        "evetreeh".to_string(),
        "alb_min".to_string(),
        "alb_max".to_string(),
    ];

    for field in ohm_prm_field_names() {
        names.push(format!("ohm.{field}"));
    }
    for field in soil_prm_field_names() {
        names.push(format!("soil.{field}"));
    }

    names.push("statelimit".to_string());
    names.push("irrfracevetr".to_string());
    names.push("wetthresh".to_string());

    for field in bioco2_prm_field_names() {
        names.push(format!("bioco2.{field}"));
    }

    names.push("maxconductance".to_string());

    for field in lai_prm_field_names() {
        names.push(format!("lai.{field}"));
    }
    for field in water_dist_prm_field_names() {
        names.push(format!("waterdist.{field}"));
    }

    names
}

pub fn lc_evetr_prm_schema_version() -> u32 {
    LC_EVETR_PRM_SCHEMA_VERSION
}

pub fn lc_evetr_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_lc_evetr_prm_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn lc_evetr_prm_field_index(name: &str) -> Option<usize> {
    let names = lc_evetr_prm_field_names();
    field_index(&names, name)
}

pub fn lc_evetr_prm_to_map(state: &LcEvetrPrm) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn lc_evetr_prm_to_ordered_values(state: &LcEvetrPrm) -> Vec<f64> {
    state.to_flat()
}

pub fn lc_evetr_prm_from_ordered_values(values: &[f64]) -> Result<LcEvetrPrm, BridgeError> {
    LcEvetrPrm::from_flat(values)
}

pub fn lc_evetr_prm_to_values_payload(state: &LcEvetrPrm) -> LcEvetrPrmValuesPayload {
    to_values_payload(state)
}

pub fn lc_evetr_prm_from_values_payload(
    payload: &LcEvetrPrmValuesPayload,
) -> Result<LcEvetrPrm, BridgeError> {
    from_values_payload(payload)
}

pub fn lc_evetr_prm_from_map(values: &BTreeMap<String, f64>) -> Result<LcEvetrPrm, BridgeError> {
    let default_state = lc_evetr_prm_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn lc_evetr_prm_default_from_fortran() -> Result<LcEvetrPrm, BridgeError> {
    let n_flat = lc_evetr_prm_schema()?;
    if n_flat != LC_EVETR_PRM_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_lc_evetr_prm_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    LcEvetrPrm::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = lc_evetr_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, LC_EVETR_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = lc_evetr_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, LcEvetrPrm::default());

        let state2 =
            LcEvetrPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = lc_evetr_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = lc_evetr_prm_to_map(&state);
        mapped.insert("irrfracevetr".to_string(), 0.3);
        mapped.insert("bioco2.resp_b".to_string(), 0.02);
        mapped.insert("lai.laipower_2".to_string(), 0.7);
        mapped.insert("waterdist.to_evetr".to_string(), 0.5);

        let updated = lc_evetr_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.irrfracevetr - 0.3).abs() < 1.0e-12);
        assert!((updated.bioco2.resp_b - 0.02).abs() < 1.0e-12);
        assert!((updated.lai.laipower[1] - 0.7).abs() < 1.0e-12);
        assert!((updated.waterdist.to_evetr - 0.5).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = lc_evetr_prm_default_from_fortran().expect("default state should be available");
        let payload = lc_evetr_prm_to_values_payload(&state);
        let recovered =
            lc_evetr_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = LcEvetrPrmValuesPayload {
            schema_version: LC_EVETR_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = lc_evetr_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
