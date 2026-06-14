use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const ANTHRO_HEAT_PRM_FLAT_LEN: usize = 117;
pub const ANTHRO_HEAT_PRM_SCHEMA_VERSION: u32 = 1;

const HOURS_PER_DAY: usize = 24;

pub type AnthroHeatPrmSchema = crate::codec::SimpleSchema;

pub type AnthroHeatPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct AnthroHeatPrm {
    pub qf0_beu_working: f64,
    pub qf0_beu_holiday: f64,
    pub qf_a_working: f64,
    pub qf_a_holiday: f64,
    pub qf_b_working: f64,
    pub qf_b_holiday: f64,
    pub qf_c_working: f64,
    pub qf_c_holiday: f64,
    pub baset_cooling_working: f64,
    pub baset_cooling_holiday: f64,
    pub baset_heating_working: f64,
    pub baset_heating_holiday: f64,
    pub ah_min_working: f64,
    pub ah_min_holiday: f64,
    pub ah_slope_cooling_working: f64,
    pub ah_slope_cooling_holiday: f64,
    pub ah_slope_heating_working: f64,
    pub ah_slope_heating_holiday: f64,
    pub ahprof_24hr_working: [f64; HOURS_PER_DAY],
    pub ahprof_24hr_holiday: [f64; HOURS_PER_DAY],
    pub popdensdaytime_working: f64,
    pub popdensdaytime_holiday: f64,
    pub popdensnighttime: f64,
    pub popprof_24hr_working: [f64; HOURS_PER_DAY],
    pub popprof_24hr_holiday: [f64; HOURS_PER_DAY],
}

impl Default for AnthroHeatPrm {
    fn default() -> Self {
        Self {
            qf0_beu_working: 0.0,
            qf0_beu_holiday: 0.0,
            qf_a_working: 0.0,
            qf_a_holiday: 0.0,
            qf_b_working: 0.0,
            qf_b_holiday: 0.0,
            qf_c_working: 0.0,
            qf_c_holiday: 0.0,
            baset_cooling_working: 0.0,
            baset_cooling_holiday: 0.0,
            baset_heating_working: 0.0,
            baset_heating_holiday: 0.0,
            ah_min_working: 0.0,
            ah_min_holiday: 0.0,
            ah_slope_cooling_working: 0.0,
            ah_slope_cooling_holiday: 0.0,
            ah_slope_heating_working: 0.0,
            ah_slope_heating_holiday: 0.0,
            ahprof_24hr_working: [0.0; HOURS_PER_DAY],
            ahprof_24hr_holiday: [0.0; HOURS_PER_DAY],
            popdensdaytime_working: 0.0,
            popdensdaytime_holiday: 0.0,
            popdensnighttime: 0.0,
            popprof_24hr_working: [0.0; HOURS_PER_DAY],
            popprof_24hr_holiday: [0.0; HOURS_PER_DAY],
        }
    }
}

fn copy_24(src: &[f64]) -> Result<[f64; HOURS_PER_DAY], BridgeError> {
    if src.len() != HOURS_PER_DAY {
        return Err(BridgeError::BadState);
    }

    let mut out = [0.0_f64; HOURS_PER_DAY];
    out.copy_from_slice(src);
    Ok(out)
}

impl AnthroHeatPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, ANTHRO_HEAT_PRM_FLAT_LEN)?;

        Ok(Self {
            qf0_beu_working: flat[0],
            qf0_beu_holiday: flat[1],
            qf_a_working: flat[2],
            qf_a_holiday: flat[3],
            qf_b_working: flat[4],
            qf_b_holiday: flat[5],
            qf_c_working: flat[6],
            qf_c_holiday: flat[7],
            baset_cooling_working: flat[8],
            baset_cooling_holiday: flat[9],
            baset_heating_working: flat[10],
            baset_heating_holiday: flat[11],
            ah_min_working: flat[12],
            ah_min_holiday: flat[13],
            ah_slope_cooling_working: flat[14],
            ah_slope_cooling_holiday: flat[15],
            ah_slope_heating_working: flat[16],
            ah_slope_heating_holiday: flat[17],
            ahprof_24hr_working: copy_24(&flat[18..42])?,
            ahprof_24hr_holiday: copy_24(&flat[42..66])?,
            popdensdaytime_working: flat[66],
            popdensdaytime_holiday: flat[67],
            popdensnighttime: flat[68],
            popprof_24hr_working: copy_24(&flat[69..93])?,
            popprof_24hr_holiday: copy_24(&flat[93..117])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = Vec::with_capacity(ANTHRO_HEAT_PRM_FLAT_LEN);

        out.push(self.qf0_beu_working);
        out.push(self.qf0_beu_holiday);
        out.push(self.qf_a_working);
        out.push(self.qf_a_holiday);
        out.push(self.qf_b_working);
        out.push(self.qf_b_holiday);
        out.push(self.qf_c_working);
        out.push(self.qf_c_holiday);
        out.push(self.baset_cooling_working);
        out.push(self.baset_cooling_holiday);
        out.push(self.baset_heating_working);
        out.push(self.baset_heating_holiday);
        out.push(self.ah_min_working);
        out.push(self.ah_min_holiday);
        out.push(self.ah_slope_cooling_working);
        out.push(self.ah_slope_cooling_holiday);
        out.push(self.ah_slope_heating_working);
        out.push(self.ah_slope_heating_holiday);
        out.extend_from_slice(&self.ahprof_24hr_working);
        out.extend_from_slice(&self.ahprof_24hr_holiday);
        out.push(self.popdensdaytime_working);
        out.push(self.popdensdaytime_holiday);
        out.push(self.popdensnighttime);
        out.extend_from_slice(&self.popprof_24hr_working);
        out.extend_from_slice(&self.popprof_24hr_holiday);

        out
    }
}

impl StateCodec for AnthroHeatPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "anthroHEAT_PRM".to_string(),
            schema_version: ANTHRO_HEAT_PRM_SCHEMA_VERSION,
            flat_len: ANTHRO_HEAT_PRM_FLAT_LEN,
            field_names: anthro_heat_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        AnthroHeatPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        AnthroHeatPrm::to_flat(self)
    }
}

pub fn anthro_heat_prm_field_names() -> Vec<String> {
    let mut names = vec![
        "qf0_beu_working".to_string(),
        "qf0_beu_holiday".to_string(),
        "qf_a_working".to_string(),
        "qf_a_holiday".to_string(),
        "qf_b_working".to_string(),
        "qf_b_holiday".to_string(),
        "qf_c_working".to_string(),
        "qf_c_holiday".to_string(),
        "baset_cooling_working".to_string(),
        "baset_cooling_holiday".to_string(),
        "baset_heating_working".to_string(),
        "baset_heating_holiday".to_string(),
        "ah_min_working".to_string(),
        "ah_min_holiday".to_string(),
        "ah_slope_cooling_working".to_string(),
        "ah_slope_cooling_holiday".to_string(),
        "ah_slope_heating_working".to_string(),
        "ah_slope_heating_holiday".to_string(),
    ];

    for i in 0..HOURS_PER_DAY {
        names.push(format!("ahprof_24hr_working_{i:02}"));
    }

    for i in 0..HOURS_PER_DAY {
        names.push(format!("ahprof_24hr_holiday_{i:02}"));
    }

    names.push("popdensdaytime_working".to_string());
    names.push("popdensdaytime_holiday".to_string());
    names.push("popdensnighttime".to_string());

    for i in 0..HOURS_PER_DAY {
        names.push(format!("popprof_24hr_working_{i:02}"));
    }

    for i in 0..HOURS_PER_DAY {
        names.push(format!("popprof_24hr_holiday_{i:02}"));
    }

    names
}

crate::codec::impl_state_module_fns! {
    prefix = anthro_heat_prm,
    state_type = AnthroHeatPrm,
    schema_type = AnthroHeatPrmSchema,
    payload_type = AnthroHeatPrmValuesPayload,
    flat_len_const = ANTHRO_HEAT_PRM_FLAT_LEN,
    schema_version_const = ANTHRO_HEAT_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_anthro_heat_prm_len,
    ffi_schema_version_fn = ffi::suews_anthro_heat_prm_schema_version,
    ffi_default_fn = ffi::suews_anthro_heat_prm_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = anthro_heat_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, ANTHRO_HEAT_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            anthro_heat_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, AnthroHeatPrm::default());

        let state2 =
            AnthroHeatPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            anthro_heat_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = anthro_heat_prm_to_map(&state);
        mapped.insert("qf_a_working".to_string(), 25.0);
        mapped.insert("ahprof_24hr_working_07".to_string(), 0.5);
        mapped.insert("popprof_24hr_holiday_23".to_string(), 0.8);

        let updated = anthro_heat_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.qf_a_working - 25.0).abs() < 1.0e-12);
        assert!((updated.ahprof_24hr_working[7] - 0.5).abs() < 1.0e-12);
        assert!((updated.popprof_24hr_holiday[23] - 0.8).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            anthro_heat_prm_default_from_fortran().expect("default state should be available");
        let payload = anthro_heat_prm_to_values_payload(&state);
        let recovered =
            anthro_heat_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = AnthroHeatPrmValuesPayload {
            schema_version: ANTHRO_HEAT_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = anthro_heat_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
