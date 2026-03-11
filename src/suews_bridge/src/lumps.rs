use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const LUMPS_PRM_FLAT_LEN: usize = 4;
pub const LUMPS_PRM_SCHEMA_VERSION: u32 = 1;

pub type LumpsPrmSchema = crate::codec::SimpleSchema;

pub type LumpsPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct LumpsPrm {
    pub raincover: f64,
    pub rainmaxres: f64,
    pub drainrt: f64,
    pub veg_type: i32,
}

impl Default for LumpsPrm {
    fn default() -> Self {
        Self {
            raincover: 0.0,
            rainmaxres: 0.0,
            drainrt: 0.0,
            veg_type: 0,
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

impl LumpsPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, LUMPS_PRM_FLAT_LEN)?;
        Ok(Self {
            raincover: flat[0],
            rainmaxres: flat[1],
            drainrt: flat[2],
            veg_type: decode_int(flat[3])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.raincover,
            self.rainmaxres,
            self.drainrt,
            self.veg_type as f64,
        ]
    }
}

impl StateCodec for LumpsPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "LUMPS_PRM".to_string(),
            schema_version: LUMPS_PRM_SCHEMA_VERSION,
            flat_len: LUMPS_PRM_FLAT_LEN,
            field_names: lumps_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        LumpsPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        LumpsPrm::to_flat(self)
    }
}

pub fn lumps_prm_field_names() -> Vec<String> {
    vec![
        "raincover".to_string(),
        "rainmaxres".to_string(),
        "drainrt".to_string(),
        "veg_type".to_string(),
    ]
}

crate::codec::impl_state_module_fns! {
    prefix = lumps_prm,
    state_type = LumpsPrm,
    schema_type = LumpsPrmSchema,
    payload_type = LumpsPrmValuesPayload,
    flat_len_const = LUMPS_PRM_FLAT_LEN,
    schema_version_const = LUMPS_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_lumps_prm_len,
    ffi_schema_version_fn = ffi::suews_lumps_prm_schema_version,
    ffi_default_fn = ffi::suews_lumps_prm_default,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = lumps_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, LUMPS_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = lumps_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, LumpsPrm::default());

        let state2 = LumpsPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = lumps_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = lumps_prm_to_map(&state);
        mapped.insert("raincover".to_string(), 0.25);
        mapped.insert("veg_type".to_string(), 2.0);

        let updated = lumps_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.raincover - 0.25).abs() < 1.0e-12);
        assert_eq!(updated.veg_type, 2);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = lumps_prm_default_from_fortran().expect("default state should be available");
        let payload = lumps_prm_to_values_payload(&state);
        let recovered =
            lumps_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = LumpsPrmValuesPayload {
            schema_version: LUMPS_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = lumps_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn from_flat_rejects_non_integer_veg_type() {
        let mut flat = vec![0.0_f64; LUMPS_PRM_FLAT_LEN];
        flat[3] = 1.5;
        let err = LumpsPrm::from_flat(&flat).expect_err("fractional integer should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
