use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const LAI_PRM_FLAT_LEN: usize = 17;
pub const LAI_PRM_SCHEMA_VERSION: u32 = 2;

pub type LaiPrmSchema = crate::codec::SimpleSchema;

pub type LaiPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct LaiPrm {
    pub baset: f64,
    pub gddfull: f64,
    pub basete: f64,
    pub sddfull: f64,
    pub laimin: f64,
    pub laimax: f64,
    pub laipower: [f64; 4],
    pub laitype: i32,
    // GH-1292 moisture-aware phenology parameters (laitype=2); thresholds operate on
    // dimensionless relative soil water w = 1 - smd/smdcap, except tau_w which is days.
    pub w_wilt: f64,
    pub w_opt: f64,
    pub f_shape: f64,
    pub w_on: f64,
    pub w_off: f64,
    pub tau_w: f64,
}

impl Default for LaiPrm {
    fn default() -> Self {
        Self {
            baset: 0.0,
            gddfull: 0.0,
            basete: 0.0,
            sddfull: 0.0,
            laimin: 0.0,
            laimax: 0.0,
            laipower: [0.0; 4],
            laitype: 0,
            w_wilt: 0.15,
            w_opt: 0.40,
            f_shape: 1.0,
            w_on: 0.35,
            w_off: 0.20,
            tau_w: 15.0,
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
            baset: flat[0],
            gddfull: flat[1],
            basete: flat[2],
            sddfull: flat[3],
            laimin: flat[4],
            laimax: flat[5],
            laipower: [flat[6], flat[7], flat[8], flat[9]],
            laitype: decode_int(flat[10])?,
            w_wilt: flat[11],
            w_opt: flat[12],
            f_shape: flat[13],
            w_on: flat[14],
            w_off: flat[15],
            tau_w: flat[16],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.baset,
            self.gddfull,
            self.basete,
            self.sddfull,
            self.laimin,
            self.laimax,
            self.laipower[0],
            self.laipower[1],
            self.laipower[2],
            self.laipower[3],
            self.laitype as f64,
            self.w_wilt,
            self.w_opt,
            self.f_shape,
            self.w_on,
            self.w_off,
            self.tau_w,
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
        "w_wilt".to_string(),
        "w_opt".to_string(),
        "f_shape".to_string(),
        "w_on".to_string(),
        "w_off".to_string(),
        "tau_w".to_string(),
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
        assert!((updated.laimax - 5.0).abs() < 1.0e-12);
        assert!((updated.laipower[2] - 0.6).abs() < 1.0e-12);
        assert_eq!(updated.laitype, 1);
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

    #[test]
    fn moisture_fields_roundtrip() {
        // GH-1292 PR1: verify the six new LAI_PRM moisture slots serialise through to_flat/from_flat.
        let original = LaiPrm {
            w_wilt: 0.12,
            w_opt: 0.48,
            f_shape: 1.3,
            w_on: 0.38,
            w_off: 0.18,
            tau_w: 20.0,
            ..LaiPrm::default()
        };
        let flat = original.to_flat();
        assert_eq!(flat.len(), LAI_PRM_FLAT_LEN);
        let decoded = LaiPrm::from_flat(&flat).expect("round-trip should succeed");
        assert_eq!(original, decoded);
    }

    #[test]
    fn schema_version_is_two() {
        // GH-1292 PR1: schema bumped because LAI_PRM layout changed.
        assert_eq!(LAI_PRM_SCHEMA_VERSION, 2);
        assert_eq!(LAI_PRM_FLAT_LEN, 17);
    }
}
