use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;
use crate::irrig_daywater::IrrigDaywater;

pub const IRRIGATION_PRM_FLAT_LEN: usize = 121;
pub const IRRIGATION_PRM_SCHEMA_VERSION: u32 = 1;

const HOURS_PER_DAY: usize = 24;

pub type IrrigationPrmSchema = crate::codec::SimpleSchema;

pub type IrrigationPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct IrrigationPrm {
    pub h_maintain: f64,
    pub faut: f64,
    pub ie_a: [f64; 3],
    pub ie_m: [f64; 3],
    pub ie_start: i32,
    pub ie_end: i32,
    pub internalwateruse_h: f64,
    pub irr_daywater: IrrigDaywater,
    pub wuprofa_24hr_working: [f64; HOURS_PER_DAY],
    pub wuprofa_24hr_holiday: [f64; HOURS_PER_DAY],
    pub wuprofm_24hr_working: [f64; HOURS_PER_DAY],
    pub wuprofm_24hr_holiday: [f64; HOURS_PER_DAY],
}

impl Default for IrrigationPrm {
    fn default() -> Self {
        Self {
            h_maintain: 0.0,
            faut: 0.0,
            ie_a: [0.0; 3],
            ie_m: [0.0; 3],
            ie_start: 0,
            ie_end: 0,
            internalwateruse_h: 0.0,
            irr_daywater: IrrigDaywater::default(),
            wuprofa_24hr_working: [0.0; HOURS_PER_DAY],
            wuprofa_24hr_holiday: [0.0; HOURS_PER_DAY],
            wuprofm_24hr_working: [0.0; HOURS_PER_DAY],
            wuprofm_24hr_holiday: [0.0; HOURS_PER_DAY],
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

fn copy_24(src: &[f64]) -> Result<[f64; HOURS_PER_DAY], BridgeError> {
    if src.len() != HOURS_PER_DAY {
        return Err(BridgeError::BadState);
    }

    let mut out = [0.0_f64; HOURS_PER_DAY];
    out.copy_from_slice(src);
    Ok(out)
}

impl IrrigationPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, IRRIGATION_PRM_FLAT_LEN)?;

        Ok(Self {
            h_maintain: flat[0],
            faut: flat[1],
            ie_a: [flat[2], flat[3], flat[4]],
            ie_m: [flat[5], flat[6], flat[7]],
            ie_start: decode_int(flat[8])?,
            ie_end: decode_int(flat[9])?,
            internalwateruse_h: flat[10],
            irr_daywater: IrrigDaywater::from_flat(&flat[11..25])?,
            wuprofa_24hr_working: copy_24(&flat[25..49])?,
            wuprofa_24hr_holiday: copy_24(&flat[49..73])?,
            wuprofm_24hr_working: copy_24(&flat[73..97])?,
            wuprofm_24hr_holiday: copy_24(&flat[97..121])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = Vec::with_capacity(IRRIGATION_PRM_FLAT_LEN);

        out.push(self.h_maintain);
        out.push(self.faut);
        out.extend_from_slice(&self.ie_a);
        out.extend_from_slice(&self.ie_m);
        out.push(self.ie_start as f64);
        out.push(self.ie_end as f64);
        out.push(self.internalwateruse_h);
        out.extend(self.irr_daywater.to_flat());
        out.extend_from_slice(&self.wuprofa_24hr_working);
        out.extend_from_slice(&self.wuprofa_24hr_holiday);
        out.extend_from_slice(&self.wuprofm_24hr_working);
        out.extend_from_slice(&self.wuprofm_24hr_holiday);

        out
    }
}

impl StateCodec for IrrigationPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "IRRIGATION_PRM".to_string(),
            schema_version: IRRIGATION_PRM_SCHEMA_VERSION,
            flat_len: IRRIGATION_PRM_FLAT_LEN,
            field_names: irrigation_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        IrrigationPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        IrrigationPrm::to_flat(self)
    }
}

pub fn irrigation_prm_field_names() -> Vec<String> {
    let mut names = vec![
        "h_maintain".to_string(),
        "faut".to_string(),
        "ie_a_1".to_string(),
        "ie_a_2".to_string(),
        "ie_a_3".to_string(),
        "ie_m_1".to_string(),
        "ie_m_2".to_string(),
        "ie_m_3".to_string(),
        "ie_start".to_string(),
        "ie_end".to_string(),
        "internalwateruse_h".to_string(),
    ];

    for field in [
        "monday_flag",
        "monday_percent",
        "tuesday_flag",
        "tuesday_percent",
        "wednesday_flag",
        "wednesday_percent",
        "thursday_flag",
        "thursday_percent",
        "friday_flag",
        "friday_percent",
        "saturday_flag",
        "saturday_percent",
        "sunday_flag",
        "sunday_percent",
    ] {
        names.push(format!("irr_daywater.{field}"));
    }

    for i in 0..HOURS_PER_DAY {
        names.push(format!("wuprofa_24hr_working_{i:02}"));
    }
    for i in 0..HOURS_PER_DAY {
        names.push(format!("wuprofa_24hr_holiday_{i:02}"));
    }
    for i in 0..HOURS_PER_DAY {
        names.push(format!("wuprofm_24hr_working_{i:02}"));
    }
    for i in 0..HOURS_PER_DAY {
        names.push(format!("wuprofm_24hr_holiday_{i:02}"));
    }

    names
}

crate::codec::impl_state_module_fns! {
    prefix = irrigation_prm,
    state_type = IrrigationPrm,
    schema_type = IrrigationPrmSchema,
    payload_type = IrrigationPrmValuesPayload,
    flat_len_const = IRRIGATION_PRM_FLAT_LEN,
    schema_version_const = IRRIGATION_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_irrigation_prm_len,
    ffi_schema_version_fn = ffi::suews_irrigation_prm_schema_version,
    ffi_default_fn = ffi::suews_irrigation_prm_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = irrigation_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, IRRIGATION_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            irrigation_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, IrrigationPrm::default());

        let state2 =
            IrrigationPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            irrigation_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = irrigation_prm_to_map(&state);
        mapped.insert("ie_start".to_string(), 120.0);
        mapped.insert("irr_daywater.monday_flag".to_string(), 1.0);
        mapped.insert("wuprofa_24hr_working_00".to_string(), 0.2);
        mapped.insert("wuprofm_24hr_holiday_23".to_string(), 0.4);

        let updated = irrigation_prm_from_map(&mapped).expect("map to state should succeed");
        assert_eq!(updated.ie_start, 120);
        assert!((updated.irr_daywater.monday_flag - 1.0).abs() < 1.0e-12);
        assert!((updated.wuprofa_24hr_working[0] - 0.2).abs() < 1.0e-12);
        assert!((updated.wuprofm_24hr_holiday[23] - 0.4).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            irrigation_prm_default_from_fortran().expect("default state should be available");
        let payload = irrigation_prm_to_values_payload(&state);
        let recovered =
            irrigation_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = IrrigationPrmValuesPayload {
            schema_version: IRRIGATION_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = irrigation_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn from_flat_rejects_non_integer_ie_start() {
        let mut flat = vec![0.0_f64; IRRIGATION_PRM_FLAT_LEN];
        flat[8] = 12.5;
        let err = IrrigationPrm::from_flat(&flat).expect_err("fractional integer should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
