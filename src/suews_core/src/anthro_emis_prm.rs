use crate::anthro_heat_prm::{anthro_heat_prm_field_names, AnthroHeatPrm};
use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const ANTHRO_EMIS_PRM_FLAT_LEN: usize = 228;
pub const ANTHRO_EMIS_PRM_SCHEMA_VERSION: u32 = 1;

const HOURS_PER_DAY: usize = 24;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnthroEmisPrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type AnthroEmisPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct AnthroEmisPrm {
    pub startdls: i32,
    pub enddls: i32,
    pub anthroheat: AnthroHeatPrm,
    pub ef_umolco2perj: f64,
    pub enef_v_jkm: f64,
    pub frfossilfuel_heat: f64,
    pub frfossilfuel_nonheat: f64,
    pub fcef_v_kgkm: [f64; 2],
    pub humactivity_24hr_working: [f64; HOURS_PER_DAY],
    pub humactivity_24hr_holiday: [f64; HOURS_PER_DAY],
    pub maxfcmetab: f64,
    pub maxqfmetab: f64,
    pub minfcmetab: f64,
    pub minqfmetab: f64,
    pub trafficrate_working: f64,
    pub trafficrate_holiday: f64,
    pub trafficunits: f64,
    pub traffprof_24hr_working: [f64; HOURS_PER_DAY],
    pub traffprof_24hr_holiday: [f64; HOURS_PER_DAY],
}

impl Default for AnthroEmisPrm {
    fn default() -> Self {
        Self {
            startdls: 0,
            enddls: 0,
            anthroheat: AnthroHeatPrm::default(),
            ef_umolco2perj: 0.0,
            enef_v_jkm: 0.0,
            frfossilfuel_heat: 0.0,
            frfossilfuel_nonheat: 0.0,
            fcef_v_kgkm: [0.0; 2],
            humactivity_24hr_working: [0.0; HOURS_PER_DAY],
            humactivity_24hr_holiday: [0.0; HOURS_PER_DAY],
            maxfcmetab: 0.0,
            maxqfmetab: 0.0,
            minfcmetab: 0.0,
            minqfmetab: 0.0,
            trafficrate_working: 0.0,
            trafficrate_holiday: 0.0,
            trafficunits: 0.0,
            traffprof_24hr_working: [0.0; HOURS_PER_DAY],
            traffprof_24hr_holiday: [0.0; HOURS_PER_DAY],
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

fn copy_fixed<const N: usize>(src: &[f64]) -> Result<[f64; N], BridgeError> {
    if src.len() != N {
        return Err(BridgeError::BadState);
    }

    let mut out = [0.0_f64; N];
    out.copy_from_slice(src);
    Ok(out)
}

impl AnthroEmisPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, ANTHRO_EMIS_PRM_FLAT_LEN)?;

        Ok(Self {
            startdls: decode_int(flat[0])?,
            enddls: decode_int(flat[1])?,
            anthroheat: AnthroHeatPrm::from_flat(&flat[2..119])?,
            ef_umolco2perj: flat[119],
            enef_v_jkm: flat[120],
            frfossilfuel_heat: flat[121],
            frfossilfuel_nonheat: flat[122],
            fcef_v_kgkm: copy_fixed(&flat[123..125])?,
            humactivity_24hr_working: copy_fixed(&flat[125..149])?,
            humactivity_24hr_holiday: copy_fixed(&flat[149..173])?,
            maxfcmetab: flat[173],
            maxqfmetab: flat[174],
            minfcmetab: flat[175],
            minqfmetab: flat[176],
            trafficrate_working: flat[177],
            trafficrate_holiday: flat[178],
            trafficunits: flat[179],
            traffprof_24hr_working: copy_fixed(&flat[180..204])?,
            traffprof_24hr_holiday: copy_fixed(&flat[204..228])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = Vec::with_capacity(ANTHRO_EMIS_PRM_FLAT_LEN);

        out.push(self.startdls as f64);
        out.push(self.enddls as f64);
        out.extend(self.anthroheat.to_flat());
        out.push(self.ef_umolco2perj);
        out.push(self.enef_v_jkm);
        out.push(self.frfossilfuel_heat);
        out.push(self.frfossilfuel_nonheat);
        out.extend_from_slice(&self.fcef_v_kgkm);
        out.extend_from_slice(&self.humactivity_24hr_working);
        out.extend_from_slice(&self.humactivity_24hr_holiday);
        out.push(self.maxfcmetab);
        out.push(self.maxqfmetab);
        out.push(self.minfcmetab);
        out.push(self.minqfmetab);
        out.push(self.trafficrate_working);
        out.push(self.trafficrate_holiday);
        out.push(self.trafficunits);
        out.extend_from_slice(&self.traffprof_24hr_working);
        out.extend_from_slice(&self.traffprof_24hr_holiday);

        out
    }
}

impl StateCodec for AnthroEmisPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "anthroEMIS_PRM".to_string(),
            schema_version: ANTHRO_EMIS_PRM_SCHEMA_VERSION,
            flat_len: ANTHRO_EMIS_PRM_FLAT_LEN,
            field_names: anthro_emis_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        AnthroEmisPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        AnthroEmisPrm::to_flat(self)
    }
}

pub fn anthro_emis_prm_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_anthro_emis_prm_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn anthro_emis_prm_schema_info() -> Result<AnthroEmisPrmSchema, BridgeError> {
    let flat_len = anthro_emis_prm_schema()?;
    let schema_version_runtime = anthro_emis_prm_schema_version_runtime()?;
    let field_names = anthro_emis_prm_field_names();

    if schema_version_runtime != ANTHRO_EMIS_PRM_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(AnthroEmisPrmSchema {
        schema_version: ANTHRO_EMIS_PRM_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn anthro_emis_prm_field_names() -> Vec<String> {
    let mut names = vec!["startdls".to_string(), "enddls".to_string()];

    for field in anthro_heat_prm_field_names() {
        names.push(format!("anthroheat.{field}"));
    }

    names.push("ef_umolco2perj".to_string());
    names.push("enef_v_jkm".to_string());
    names.push("frfossilfuel_heat".to_string());
    names.push("frfossilfuel_nonheat".to_string());
    names.push("fcef_v_kgkm_1".to_string());
    names.push("fcef_v_kgkm_2".to_string());

    for i in 0..HOURS_PER_DAY {
        names.push(format!("humactivity_24hr_working_{i:02}"));
    }

    for i in 0..HOURS_PER_DAY {
        names.push(format!("humactivity_24hr_holiday_{i:02}"));
    }

    names.push("maxfcmetab".to_string());
    names.push("maxqfmetab".to_string());
    names.push("minfcmetab".to_string());
    names.push("minqfmetab".to_string());
    names.push("trafficrate_working".to_string());
    names.push("trafficrate_holiday".to_string());
    names.push("trafficunits".to_string());

    for i in 0..HOURS_PER_DAY {
        names.push(format!("traffprof_24hr_working_{i:02}"));
    }

    for i in 0..HOURS_PER_DAY {
        names.push(format!("traffprof_24hr_holiday_{i:02}"));
    }

    names
}

pub fn anthro_emis_prm_schema_version() -> u32 {
    ANTHRO_EMIS_PRM_SCHEMA_VERSION
}

pub fn anthro_emis_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_anthro_emis_prm_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn anthro_emis_prm_field_index(name: &str) -> Option<usize> {
    let names = anthro_emis_prm_field_names();
    field_index(&names, name)
}

pub fn anthro_emis_prm_to_map(state: &AnthroEmisPrm) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn anthro_emis_prm_to_ordered_values(state: &AnthroEmisPrm) -> Vec<f64> {
    state.to_flat()
}

pub fn anthro_emis_prm_from_ordered_values(values: &[f64]) -> Result<AnthroEmisPrm, BridgeError> {
    AnthroEmisPrm::from_flat(values)
}

pub fn anthro_emis_prm_to_values_payload(state: &AnthroEmisPrm) -> AnthroEmisPrmValuesPayload {
    to_values_payload(state)
}

pub fn anthro_emis_prm_from_values_payload(
    payload: &AnthroEmisPrmValuesPayload,
) -> Result<AnthroEmisPrm, BridgeError> {
    from_values_payload(payload)
}

pub fn anthro_emis_prm_from_map(
    values: &BTreeMap<String, f64>,
) -> Result<AnthroEmisPrm, BridgeError> {
    let default_state = anthro_emis_prm_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn anthro_emis_prm_default_from_fortran() -> Result<AnthroEmisPrm, BridgeError> {
    let n_flat = anthro_emis_prm_schema()?;
    if n_flat != ANTHRO_EMIS_PRM_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_anthro_emis_prm_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    AnthroEmisPrm::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = anthro_emis_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, ANTHRO_EMIS_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            anthro_emis_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, AnthroEmisPrm::default());

        let state2 =
            AnthroEmisPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            anthro_emis_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = anthro_emis_prm_to_map(&state);
        mapped.insert("startdls".to_string(), 90.0);
        mapped.insert("anthroheat.qf_a_working".to_string(), 12.5);
        mapped.insert("fcef_v_kgkm_2".to_string(), 1.2);
        mapped.insert("humactivity_24hr_holiday_12".to_string(), 0.3);

        let updated = anthro_emis_prm_from_map(&mapped).expect("map to state should succeed");
        assert_eq!(updated.startdls, 90);
        assert!((updated.anthroheat.qf_a_working - 12.5).abs() < 1.0e-12);
        assert!((updated.fcef_v_kgkm[1] - 1.2).abs() < 1.0e-12);
        assert!((updated.humactivity_24hr_holiday[12] - 0.3).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            anthro_emis_prm_default_from_fortran().expect("default state should be available");
        let payload = anthro_emis_prm_to_values_payload(&state);
        let recovered =
            anthro_emis_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = AnthroEmisPrmValuesPayload {
            schema_version: ANTHRO_EMIS_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = anthro_emis_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn from_flat_rejects_non_integer_startdls() {
        let mut flat = vec![0.0_f64; ANTHRO_EMIS_PRM_FLAT_LEN];
        flat[0] = 12.25;
        let err =
            AnthroEmisPrm::from_flat(&flat).expect_err("non-integer startdls should be rejected");
        assert_eq!(err, BridgeError::BadState);
    }
}
