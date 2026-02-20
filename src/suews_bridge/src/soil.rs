use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const SOIL_PRM_FLAT_LEN: usize = 3;
pub const SOIL_PRM_SCHEMA_VERSION: u32 = 1;

pub type SoilPrmSchema = crate::codec::SimpleSchema;

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

pub fn soil_prm_field_names() -> Vec<String> {
    vec![
        "soildepth".to_string(),
        "soilstorecap".to_string(),
        "sathydraulicconduct".to_string(),
    ]
}

crate::codec::impl_state_module_fns! {
    prefix = soil_prm,
    state_type = SoilPrm,
    schema_type = SoilPrmSchema,
    payload_type = SoilPrmValuesPayload,
    flat_len_const = SOIL_PRM_FLAT_LEN,
    schema_version_const = SOIL_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_soil_prm_len,
    ffi_schema_version_fn = ffi::suews_soil_prm_schema_version,
    ffi_default_fn = ffi::suews_soil_prm_default,
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
