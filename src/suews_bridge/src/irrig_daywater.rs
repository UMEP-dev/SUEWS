use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const IRRIG_DAYWATER_FLAT_LEN: usize = 14;
pub const IRRIG_DAYWATER_SCHEMA_VERSION: u32 = 1;

pub type IrrigDaywaterSchema = crate::codec::SimpleSchema;

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

crate::codec::impl_state_module_fns! {
    prefix = irrig_daywater,
    state_type = IrrigDaywater,
    schema_type = IrrigDaywaterSchema,
    payload_type = IrrigDaywaterValuesPayload,
    flat_len_const = IRRIG_DAYWATER_FLAT_LEN,
    schema_version_const = IRRIG_DAYWATER_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_irrig_daywater_len,
    ffi_schema_version_fn = ffi::suews_irrig_daywater_schema_version,
    ffi_default_fn = ffi::suews_irrig_daywater_default,
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
