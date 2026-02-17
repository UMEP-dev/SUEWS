use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const ANTHROEMIS_STATE_HDD_LEN: usize = 12;
pub const ANTHROEMIS_STATE_FLAT_LEN: usize = 22;
pub const ANTHROEMIS_STATE_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnthroEmisStateSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type AnthroEmisStateValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct AnthroEmisState {
    pub hdd_id: [f64; ANTHROEMIS_STATE_HDD_LEN],
    pub fc: f64,
    pub fc_anthro: f64,
    pub fc_biogen: f64,
    pub fc_build: f64,
    pub fc_metab: f64,
    pub fc_photo: f64,
    pub fc_point: f64,
    pub fc_respi: f64,
    pub fc_traff: f64,
    pub iter_safe: bool,
}

impl Default for AnthroEmisState {
    fn default() -> Self {
        Self {
            hdd_id: [0.0; ANTHROEMIS_STATE_HDD_LEN],
            fc: 0.0,
            fc_anthro: 0.0,
            fc_biogen: 0.0,
            fc_build: 0.0,
            fc_metab: 0.0,
            fc_photo: 0.0,
            fc_point: 0.0,
            fc_respi: 0.0,
            fc_traff: 0.0,
            iter_safe: false,
        }
    }
}

impl AnthroEmisState {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, ANTHROEMIS_STATE_FLAT_LEN)?;
        let mut state = Self::default();

        let mut idx = 0_usize;
        let mut next = || {
            let value = flat[idx];
            idx += 1;
            value
        };

        for i in 0..ANTHROEMIS_STATE_HDD_LEN {
            state.hdd_id[i] = next();
        }
        state.fc = next();
        state.fc_anthro = next();
        state.fc_biogen = next();
        state.fc_build = next();
        state.fc_metab = next();
        state.fc_photo = next();
        state.fc_point = next();
        state.fc_respi = next();
        state.fc_traff = next();
        state.iter_safe = next() >= 0.5;

        Ok(state)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(ANTHROEMIS_STATE_FLAT_LEN);
        flat.extend_from_slice(&self.hdd_id);
        flat.push(self.fc);
        flat.push(self.fc_anthro);
        flat.push(self.fc_biogen);
        flat.push(self.fc_build);
        flat.push(self.fc_metab);
        flat.push(self.fc_photo);
        flat.push(self.fc_point);
        flat.push(self.fc_respi);
        flat.push(self.fc_traff);
        flat.push(if self.iter_safe { 1.0 } else { 0.0 });
        flat
    }
}

impl StateCodec for AnthroEmisState {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "anthroEmis_STATE".to_string(),
            schema_version: ANTHROEMIS_STATE_SCHEMA_VERSION,
            flat_len: ANTHROEMIS_STATE_FLAT_LEN,
            field_names: anthroemis_state_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        AnthroEmisState::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        AnthroEmisState::to_flat(self)
    }
}

pub fn anthroemis_state_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_anthroemis_state_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn anthroemis_state_schema_info() -> Result<AnthroEmisStateSchema, BridgeError> {
    let flat_len = anthroemis_state_schema()?;
    let schema_version_runtime = anthroemis_state_schema_version_runtime()?;
    let field_names = anthroemis_state_field_names();

    if schema_version_runtime != ANTHROEMIS_STATE_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(AnthroEmisStateSchema {
        schema_version: ANTHROEMIS_STATE_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn anthroemis_state_field_names() -> Vec<String> {
    let mut names = Vec::with_capacity(ANTHROEMIS_STATE_FLAT_LEN);
    for i in 1..=ANTHROEMIS_STATE_HDD_LEN {
        names.push(format!("hdd_id.{i}"));
    }
    names.push("fc".to_string());
    names.push("fc_anthro".to_string());
    names.push("fc_biogen".to_string());
    names.push("fc_build".to_string());
    names.push("fc_metab".to_string());
    names.push("fc_photo".to_string());
    names.push("fc_point".to_string());
    names.push("fc_respi".to_string());
    names.push("fc_traff".to_string());
    names.push("iter_safe".to_string());
    names
}

pub fn anthroemis_state_schema_version() -> u32 {
    ANTHROEMIS_STATE_SCHEMA_VERSION
}

pub fn anthroemis_state_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_anthroemis_state_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn anthroemis_state_field_index(name: &str) -> Option<usize> {
    let names = anthroemis_state_field_names();
    field_index(&names, name)
}

pub fn anthroemis_state_to_map(state: &AnthroEmisState) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn anthroemis_state_to_ordered_values(state: &AnthroEmisState) -> Vec<f64> {
    state.to_flat()
}

pub fn anthroemis_state_from_ordered_values(
    values: &[f64],
) -> Result<AnthroEmisState, BridgeError> {
    AnthroEmisState::from_flat(values)
}

pub fn anthroemis_state_to_values_payload(state: &AnthroEmisState) -> AnthroEmisStateValuesPayload {
    to_values_payload(state)
}

pub fn anthroemis_state_from_values_payload(
    payload: &AnthroEmisStateValuesPayload,
) -> Result<AnthroEmisState, BridgeError> {
    from_values_payload(payload)
}

pub fn anthroemis_state_from_map(
    values: &BTreeMap<String, f64>,
) -> Result<AnthroEmisState, BridgeError> {
    let default_state = anthroemis_state_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn anthroemis_state_default_from_fortran() -> Result<AnthroEmisState, BridgeError> {
    let n_flat = anthroemis_state_schema()?;
    if n_flat != ANTHROEMIS_STATE_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_anthroemis_state_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    AnthroEmisState::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = anthroemis_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, ANTHROEMIS_STATE_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            anthroemis_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, AnthroEmisState::default());
        let state2 =
            AnthroEmisState::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            anthroemis_state_default_from_fortran().expect("default state should be available");
        let mut mapped = anthroemis_state_to_map(&state);
        mapped.insert("hdd_id.1".to_string(), 1.2);
        mapped.insert("fc_traff".to_string(), 3.4);

        let updated = anthroemis_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.hdd_id[0] - 1.2).abs() < 1.0e-12);
        assert!((updated.fc_traff - 3.4).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            anthroemis_state_default_from_fortran().expect("default state should be available");
        let payload = anthroemis_state_to_values_payload(&state);
        let recovered =
            anthroemis_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = AnthroEmisStateValuesPayload {
            schema_version: ANTHROEMIS_STATE_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = anthroemis_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
