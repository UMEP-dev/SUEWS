use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const FLAG_STATE_FLAT_LEN: usize = 5;
pub const FLAG_STATE_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlagStateSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type FlagStateValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlagState {
    pub flag_converge: bool,
    pub i_iter: i32,
    pub stebbs_bldg_init: i32,
    pub snow_warning_shown: bool,
    pub iter_safe: bool,
}

impl Default for FlagState {
    fn default() -> Self {
        Self {
            flag_converge: false,
            i_iter: 0,
            stebbs_bldg_init: 0,
            snow_warning_shown: false,
            iter_safe: true,
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

impl FlagState {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, FLAG_STATE_FLAT_LEN)?;

        Ok(Self {
            flag_converge: flat[0] >= 0.5,
            i_iter: decode_int(flat[1])?,
            stebbs_bldg_init: decode_int(flat[2])?,
            snow_warning_shown: flat[3] >= 0.5,
            iter_safe: flat[4] >= 0.5,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            if self.flag_converge { 1.0 } else { 0.0 },
            self.i_iter as f64,
            self.stebbs_bldg_init as f64,
            if self.snow_warning_shown { 1.0 } else { 0.0 },
            if self.iter_safe { 1.0 } else { 0.0 },
        ]
    }
}

impl StateCodec for FlagState {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "flag_STATE".to_string(),
            schema_version: FLAG_STATE_SCHEMA_VERSION,
            flat_len: FLAG_STATE_FLAT_LEN,
            field_names: flag_state_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        FlagState::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        FlagState::to_flat(self)
    }
}

pub fn flag_state_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_flag_state_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn flag_state_schema_info() -> Result<FlagStateSchema, BridgeError> {
    let flat_len = flag_state_schema()?;
    let schema_version_runtime = flag_state_schema_version_runtime()?;
    let field_names = flag_state_field_names();

    if schema_version_runtime != FLAG_STATE_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(FlagStateSchema {
        schema_version: FLAG_STATE_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn flag_state_field_names() -> Vec<String> {
    vec![
        "flag_converge".to_string(),
        "i_iter".to_string(),
        "stebbs_bldg_init".to_string(),
        "snow_warning_shown".to_string(),
        "iter_safe".to_string(),
    ]
}

pub fn flag_state_schema_version() -> u32 {
    FLAG_STATE_SCHEMA_VERSION
}

pub fn flag_state_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_flag_state_schema_version(&mut schema_version as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn flag_state_field_index(name: &str) -> Option<usize> {
    let names = flag_state_field_names();
    field_index(&names, name)
}

pub fn flag_state_to_map(state: &FlagState) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn flag_state_to_ordered_values(state: &FlagState) -> Vec<f64> {
    state.to_flat()
}

pub fn flag_state_from_ordered_values(values: &[f64]) -> Result<FlagState, BridgeError> {
    FlagState::from_flat(values)
}

pub fn flag_state_to_values_payload(state: &FlagState) -> FlagStateValuesPayload {
    to_values_payload(state)
}

pub fn flag_state_from_values_payload(
    payload: &FlagStateValuesPayload,
) -> Result<FlagState, BridgeError> {
    from_values_payload(payload)
}

pub fn flag_state_from_map(values: &BTreeMap<String, f64>) -> Result<FlagState, BridgeError> {
    let default_state = flag_state_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn flag_state_default_from_fortran() -> Result<FlagState, BridgeError> {
    let n_flat = flag_state_schema()?;
    if n_flat != FLAG_STATE_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_flag_state_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    FlagState::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = flag_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, FLAG_STATE_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = flag_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, FlagState::default());

        let flat = state.to_flat();
        let state2 = FlagState::from_flat(&flat).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = flag_state_default_from_fortran().expect("default state should be available");
        let mut mapped = flag_state_to_map(&state);
        mapped.insert("i_iter".to_string(), 3.0);
        mapped.insert("iter_safe".to_string(), 0.0);

        let updated = flag_state_from_map(&mapped).expect("map to state should succeed");
        assert_eq!(updated.i_iter, 3);
        assert!(!updated.iter_safe);
    }

    #[test]
    fn state_map_rejects_unknown_keys() {
        let mut mapped = BTreeMap::new();
        mapped.insert("not_a_real_field".to_string(), 1.0);

        let err = flag_state_from_map(&mapped).expect_err("unknown field should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn state_schema_info_is_consistent() {
        let schema = flag_state_schema_info().expect("schema info should be available");
        assert_eq!(schema.schema_version, FLAG_STATE_SCHEMA_VERSION);
        assert_eq!(schema.flat_len, FLAG_STATE_FLAT_LEN);
        assert_eq!(schema.field_names.len(), FLAG_STATE_FLAT_LEN);
    }

    #[test]
    fn runtime_schema_version_matches_static() {
        let runtime =
            flag_state_schema_version_runtime().expect("runtime schema version should work");
        assert_eq!(runtime, FLAG_STATE_SCHEMA_VERSION);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = flag_state_default_from_fortran().expect("default state should be available");
        let payload = flag_state_to_values_payload(&state);
        let recovered =
            flag_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = FlagStateValuesPayload {
            schema_version: FLAG_STATE_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = flag_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn from_flat_requires_exact_length() {
        let good = vec![0.0_f64; FLAG_STATE_FLAT_LEN];
        assert!(FlagState::from_flat(&good).is_ok());

        let short = vec![0.0_f64; FLAG_STATE_FLAT_LEN - 1];
        let err = FlagState::from_flat(&short).expect_err("short payload should fail");
        assert_eq!(err, BridgeError::BadBuffer);

        let long = vec![0.0_f64; FLAG_STATE_FLAT_LEN + 1];
        let err = FlagState::from_flat(&long).expect_err("long payload should fail");
        assert_eq!(err, BridgeError::BadBuffer);
    }

    #[test]
    fn from_flat_rejects_non_integer_int_fields() {
        let mut flat = vec![0.0_f64; FLAG_STATE_FLAT_LEN];
        flat[1] = 1.5;
        let err = FlagState::from_flat(&flat).expect_err("fractional integer should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
