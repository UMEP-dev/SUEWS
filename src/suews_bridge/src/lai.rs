use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const LAI_PRM_FLAT_LEN: usize = 11;
pub const LAI_PRM_SCHEMA_VERSION: u32 = 1;

pub type LaiPrmSchema = crate::codec::SimpleSchema;

pub type LaiPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct LaiPrm {
    pub base_temperature: f64,
    pub gdd_full: f64,
    pub base_temperature_senescence: f64,
    pub sdd_full: f64,
    pub lai_min: f64,
    pub lai_max: f64,
    pub lai_power: [f64; 4],
    pub lai_type: i32,
}

impl Default for LaiPrm {
    fn default() -> Self {
        Self {
            base_temperature: 0.0,
            gdd_full: 0.0,
            base_temperature_senescence: 0.0,
            sdd_full: 0.0,
            lai_min: 0.0,
            lai_max: 0.0,
            lai_power: [0.0; 4],
            lai_type: 0,
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
            base_temperature: flat[0],
            gdd_full: flat[1],
            base_temperature_senescence: flat[2],
            sdd_full: flat[3],
            lai_min: flat[4],
            lai_max: flat[5],
            lai_power: [flat[6], flat[7], flat[8], flat[9]],
            lai_type: decode_int(flat[10])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.base_temperature,
            self.gdd_full,
            self.base_temperature_senescence,
            self.sdd_full,
            self.lai_min,
            self.lai_max,
            self.lai_power[0],
            self.lai_power[1],
            self.lai_power[2],
            self.lai_power[3],
            self.lai_type as f64,
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

crate::codec::impl_state_module_fns! {
    prefix = lai_prm,
    state_type = LaiPrm,
    schema_type = LaiPrmSchema,
    payload_type = LaiPrmValuesPayload,
    flat_len_const = LAI_PRM_FLAT_LEN,
    schema_version_const = LAI_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_lai_prm_len,
    ffi_schema_version_fn = ffi::suews_lai_prm_schema_version,
    ffi_default_fn = ffi::suews_lai_prm_default,
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
        assert!((updated.lai_max - 5.0).abs() < 1.0e-12);
        assert!((updated.lai_power[2] - 0.6).abs() < 1.0e-12);
        assert_eq!(updated.lai_type, 1);
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
