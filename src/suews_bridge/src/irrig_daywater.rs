use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const IRRIG_DAYWATER_FLAT_LEN: usize = 14;
pub const IRRIG_DAYWATER_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrrigDaywaterSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type IrrigDaywaterValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct IrrigDaywater {
    pub monday_flag: f64,
    pub monday_percent: f64,
    pub tuesday_flag: f64,
    pub tuesday_percent: f64,
    pub wednesday_flag: f64,
    pub wednesday_percent: f64,
    pub thursday_flag: f64,
    pub thursday_percent: f64,
    pub friday_flag: f64,
    pub friday_percent: f64,
    pub saturday_flag: f64,
    pub saturday_percent: f64,
    pub sunday_flag: f64,
    pub sunday_percent: f64,
}

impl Default for IrrigDaywater {
    fn default() -> Self {
        Self {
            monday_flag: 0.0,
            monday_percent: 0.0,
            tuesday_flag: 0.0,
            tuesday_percent: 0.0,
            wednesday_flag: 0.0,
            wednesday_percent: 0.0,
            thursday_flag: 0.0,
            thursday_percent: 0.0,
            friday_flag: 0.0,
            friday_percent: 0.0,
            saturday_flag: 0.0,
            saturday_percent: 0.0,
            sunday_flag: 0.0,
            sunday_percent: 0.0,
        }
    }
}

impl IrrigDaywater {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, IRRIG_DAYWATER_FLAT_LEN)?;
        Ok(Self {
            monday_flag: flat[0],
            monday_percent: flat[1],
            tuesday_flag: flat[2],
            tuesday_percent: flat[3],
            wednesday_flag: flat[4],
            wednesday_percent: flat[5],
            thursday_flag: flat[6],
            thursday_percent: flat[7],
            friday_flag: flat[8],
            friday_percent: flat[9],
            saturday_flag: flat[10],
            saturday_percent: flat[11],
            sunday_flag: flat[12],
            sunday_percent: flat[13],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.monday_flag,
            self.monday_percent,
            self.tuesday_flag,
            self.tuesday_percent,
            self.wednesday_flag,
            self.wednesday_percent,
            self.thursday_flag,
            self.thursday_percent,
            self.friday_flag,
            self.friday_percent,
            self.saturday_flag,
            self.saturday_percent,
            self.sunday_flag,
            self.sunday_percent,
        ]
    }
}

impl StateCodec for IrrigDaywater {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "IRRIG_daywater".to_string(),
            schema_version: IRRIG_DAYWATER_SCHEMA_VERSION,
            flat_len: IRRIG_DAYWATER_FLAT_LEN,
            field_names: irrig_daywater_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        IrrigDaywater::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        IrrigDaywater::to_flat(self)
    }
}

pub fn irrig_daywater_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_irrig_daywater_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn irrig_daywater_schema_info() -> Result<IrrigDaywaterSchema, BridgeError> {
    let flat_len = irrig_daywater_schema()?;
    let schema_version_runtime = irrig_daywater_schema_version_runtime()?;
    let field_names = irrig_daywater_field_names();

    if schema_version_runtime != IRRIG_DAYWATER_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(IrrigDaywaterSchema {
        schema_version: IRRIG_DAYWATER_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn irrig_daywater_field_names() -> Vec<String> {
    vec![
        "monday_flag".to_string(),
        "monday_percent".to_string(),
        "tuesday_flag".to_string(),
        "tuesday_percent".to_string(),
        "wednesday_flag".to_string(),
        "wednesday_percent".to_string(),
        "thursday_flag".to_string(),
        "thursday_percent".to_string(),
        "friday_flag".to_string(),
        "friday_percent".to_string(),
        "saturday_flag".to_string(),
        "saturday_percent".to_string(),
        "sunday_flag".to_string(),
        "sunday_percent".to_string(),
    ]
}

pub fn irrig_daywater_schema_version() -> u32 {
    IRRIG_DAYWATER_SCHEMA_VERSION
}

pub fn irrig_daywater_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_irrig_daywater_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn irrig_daywater_field_index(name: &str) -> Option<usize> {
    let names = irrig_daywater_field_names();
    field_index(&names, name)
}

pub fn irrig_daywater_to_map(state: &IrrigDaywater) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn irrig_daywater_to_ordered_values(state: &IrrigDaywater) -> Vec<f64> {
    state.to_flat()
}

pub fn irrig_daywater_from_ordered_values(values: &[f64]) -> Result<IrrigDaywater, BridgeError> {
    IrrigDaywater::from_flat(values)
}

pub fn irrig_daywater_to_values_payload(state: &IrrigDaywater) -> IrrigDaywaterValuesPayload {
    to_values_payload(state)
}

pub fn irrig_daywater_from_values_payload(
    payload: &IrrigDaywaterValuesPayload,
) -> Result<IrrigDaywater, BridgeError> {
    from_values_payload(payload)
}

pub fn irrig_daywater_from_map(
    values: &BTreeMap<String, f64>,
) -> Result<IrrigDaywater, BridgeError> {
    let default_state = irrig_daywater_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn irrig_daywater_default_from_fortran() -> Result<IrrigDaywater, BridgeError> {
    let n_flat = irrig_daywater_schema()?;
    if n_flat != IRRIG_DAYWATER_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_irrig_daywater_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    IrrigDaywater::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = irrig_daywater_schema().expect("schema call should succeed");
        assert_eq!(n_flat, IRRIG_DAYWATER_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            irrig_daywater_default_from_fortran().expect("default state should be available");
        assert_eq!(state, IrrigDaywater::default());

        let state2 =
            IrrigDaywater::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            irrig_daywater_default_from_fortran().expect("default state should be available");
        let mut mapped = irrig_daywater_to_map(&state);
        mapped.insert("monday_flag".to_string(), 1.0);
        mapped.insert("sunday_percent".to_string(), 0.25);

        let updated = irrig_daywater_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.monday_flag - 1.0).abs() < 1.0e-12);
        assert!((updated.sunday_percent - 0.25).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            irrig_daywater_default_from_fortran().expect("default state should be available");
        let payload = irrig_daywater_to_values_payload(&state);
        let recovered =
            irrig_daywater_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = IrrigDaywaterValuesPayload {
            schema_version: IRRIG_DAYWATER_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = irrig_daywater_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
