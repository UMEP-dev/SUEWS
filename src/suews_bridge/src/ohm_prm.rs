use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;
use crate::ohm_coef_lc::OhmCoefLc;

pub const OHM_PRM_FLAT_LEN: usize = 17;
pub const OHM_PRM_SCHEMA_VERSION: u32 = 1;

pub type OhmPrmSchema = crate::codec::SimpleSchema;

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

crate::codec::impl_state_module_fns! {
    prefix = ohm_prm,
    state_type = OhmPrm,
    schema_type = OhmPrmSchema,
    payload_type = OhmPrmValuesPayload,
    flat_len_const = OHM_PRM_FLAT_LEN,
    schema_version_const = OHM_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_ohm_prm_len,
    ffi_schema_version_fn = ffi::suews_ohm_prm_schema_version,
    ffi_default_fn = ffi::suews_ohm_prm_default,
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
