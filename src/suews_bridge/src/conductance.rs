use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const CONDUCTANCE_PRM_FLAT_LEN: usize = 12;
pub const CONDUCTANCE_PRM_SCHEMA_VERSION: u32 = 1;

pub type ConductancePrmSchema = crate::codec::SimpleSchema;

pub type ConductancePrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct ConductancePrm {
    pub g_max: f64,
    pub g_k: f64,
    pub g_q_base: f64,
    pub g_q_shape: f64,
    pub g_t: f64,
    pub g_sm: f64,
    pub kmax: f64,
    pub gsmodel: i32,
    pub s1: f64,
    pub s2: f64,
    pub th: f64,
    pub tl: f64,
}

impl Default for ConductancePrm {
    fn default() -> Self {
        Self {
            g_max: 0.0,
            g_k: 0.0,
            g_q_base: 0.0,
            g_q_shape: 0.0,
            g_t: 0.0,
            g_sm: 0.0,
            kmax: 0.0,
            gsmodel: 0,
            s1: 0.0,
            s2: 0.0,
            th: 0.0,
            tl: 0.0,
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

impl ConductancePrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, CONDUCTANCE_PRM_FLAT_LEN)?;
        Ok(Self {
            g_max: flat[0],
            g_k: flat[1],
            g_q_base: flat[2],
            g_q_shape: flat[3],
            g_t: flat[4],
            g_sm: flat[5],
            kmax: flat[6],
            gsmodel: decode_int(flat[7])?,
            s1: flat[8],
            s2: flat[9],
            th: flat[10],
            tl: flat[11],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.g_max,
            self.g_k,
            self.g_q_base,
            self.g_q_shape,
            self.g_t,
            self.g_sm,
            self.kmax,
            self.gsmodel as f64,
            self.s1,
            self.s2,
            self.th,
            self.tl,
        ]
    }
}

impl StateCodec for ConductancePrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "CONDUCTANCE_PRM".to_string(),
            schema_version: CONDUCTANCE_PRM_SCHEMA_VERSION,
            flat_len: CONDUCTANCE_PRM_FLAT_LEN,
            field_names: conductance_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        ConductancePrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        ConductancePrm::to_flat(self)
    }
}

pub fn conductance_prm_field_names() -> Vec<String> {
    vec![
        "g_max".to_string(),
        "g_k".to_string(),
        "g_q_base".to_string(),
        "g_q_shape".to_string(),
        "g_t".to_string(),
        "g_sm".to_string(),
        "kmax".to_string(),
        "gsmodel".to_string(),
        "s1".to_string(),
        "s2".to_string(),
        "th".to_string(),
        "tl".to_string(),
    ]
}

crate::codec::impl_state_module_fns! {
    prefix = conductance_prm,
    state_type = ConductancePrm,
    schema_type = ConductancePrmSchema,
    payload_type = ConductancePrmValuesPayload,
    flat_len_const = CONDUCTANCE_PRM_FLAT_LEN,
    schema_version_const = CONDUCTANCE_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_conductance_prm_len,
    ffi_schema_version_fn = ffi::suews_conductance_prm_schema_version,
    ffi_default_fn = ffi::suews_conductance_prm_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = conductance_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, CONDUCTANCE_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            conductance_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, ConductancePrm::default());

        let state2 =
            ConductancePrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            conductance_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = conductance_prm_to_map(&state);
        mapped.insert("g_max".to_string(), 7.5);
        mapped.insert("gsmodel".to_string(), 2.0);

        let updated = conductance_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.g_max - 7.5).abs() < 1.0e-12);
        assert_eq!(updated.gsmodel, 2);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            conductance_prm_default_from_fortran().expect("default state should be available");
        let payload = conductance_prm_to_values_payload(&state);
        let recovered =
            conductance_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = ConductancePrmValuesPayload {
            schema_version: CONDUCTANCE_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = conductance_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn from_flat_rejects_non_integer_gsmodel() {
        let mut flat = vec![0.0_f64; CONDUCTANCE_PRM_FLAT_LEN];
        flat[7] = 1.5;
        let err = ConductancePrm::from_flat(&flat).expect_err("fractional integer should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
