use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const STEBBS_STATE_RSL_LEN: usize = 30;
pub const STEBBS_STATE_FLAT_LEN: usize = 154;
pub const STEBBS_STATE_SCHEMA_VERSION: u32 = 1;

pub type StebbsStateSchema = crate::codec::SimpleSchema;

pub type StebbsStateValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct StebbsState {
    pub kdown2d: f64,
    pub kup2d: f64,
    pub kwest: f64,
    pub ksouth: f64,
    pub knorth: f64,
    pub keast: f64,
    pub ldown2d: f64,
    pub lup2d: f64,
    pub lwest: f64,
    pub lsouth: f64,
    pub lnorth: f64,
    pub least: f64,
    pub zarray: [f64; STEBBS_STATE_RSL_LEN],
    pub dataout_line_ursl: [f64; STEBBS_STATE_RSL_LEN],
    pub dataout_line_trsl: [f64; STEBBS_STATE_RSL_LEN],
    pub dataout_line_qrsl: [f64; STEBBS_STATE_RSL_LEN],
    pub deep_soil_temperature: f64,
    pub outdoor_air_start_temperature: f64,
    pub indoor_air_start_temperature: f64,
    pub indoor_mass_start_temperature: f64,
    pub wall_indoor_surface_temperature: f64,
    pub wall_outdoor_surface_temperature: f64,
    pub roof_indoor_surface_temperature: f64,
    pub roof_outdoor_surface_temperature: f64,
    pub window_indoor_surface_temperature: f64,
    pub window_outdoor_surface_temperature: f64,
    pub ground_floor_indoor_surface_temperature: f64,
    pub ground_floor_outdoor_surface_temperature: f64,
    pub water_tank_temperature: f64,
    pub internal_wall_water_tank_temperature: f64,
    pub external_wall_water_tank_temperature: f64,
    pub mains_water_temperature: f64,
    pub domestic_hot_water_temperature_in_use_in_building: f64,
    pub internal_wall_dhw_vessel_temperature: f64,
    pub external_wall_dhw_vessel_temperature: f64,
    pub qs_stebbs: f64,
    pub buildings_count: usize,
    pub iter_safe: bool,
}

impl Default for StebbsState {
    fn default() -> Self {
        Self {
            kdown2d: 0.0,
            kup2d: 0.0,
            kwest: 0.0,
            ksouth: 0.0,
            knorth: 0.0,
            keast: 0.0,
            ldown2d: 0.0,
            lup2d: 0.0,
            lwest: 0.0,
            lsouth: 0.0,
            lnorth: 0.0,
            least: 0.0,
            zarray: [-999.0; STEBBS_STATE_RSL_LEN],
            dataout_line_ursl: [-999.0; STEBBS_STATE_RSL_LEN],
            dataout_line_trsl: [-999.0; STEBBS_STATE_RSL_LEN],
            dataout_line_qrsl: [-999.0; STEBBS_STATE_RSL_LEN],
            deep_soil_temperature: 0.0,
            outdoor_air_start_temperature: 0.0,
            indoor_air_start_temperature: 0.0,
            indoor_mass_start_temperature: 0.0,
            wall_indoor_surface_temperature: 0.0,
            wall_outdoor_surface_temperature: 0.0,
            roof_indoor_surface_temperature: 0.0,
            roof_outdoor_surface_temperature: 0.0,
            window_indoor_surface_temperature: 0.0,
            window_outdoor_surface_temperature: 0.0,
            ground_floor_indoor_surface_temperature: 0.0,
            ground_floor_outdoor_surface_temperature: 0.0,
            water_tank_temperature: 0.0,
            internal_wall_water_tank_temperature: 0.0,
            external_wall_water_tank_temperature: 0.0,
            mains_water_temperature: 0.0,
            domestic_hot_water_temperature_in_use_in_building: 0.0,
            internal_wall_dhw_vessel_temperature: 0.0,
            external_wall_dhw_vessel_temperature: 0.0,
            qs_stebbs: 0.0,
            buildings_count: 0,
            iter_safe: false,
        }
    }
}

fn copy_fixed<const N: usize>(slice: &[f64]) -> Result<[f64; N], BridgeError> {
    if slice.len() != N {
        return Err(BridgeError::BadState);
    }
    let mut out = [0.0_f64; N];
    out.copy_from_slice(slice);
    Ok(out)
}

fn parse_buildings_count(raw: f64) -> Result<usize, BridgeError> {
    if !raw.is_finite() || raw < 0.0 {
        return Err(BridgeError::BadState);
    }

    let rounded = raw.round();
    if (raw - rounded).abs() > 1.0e-9 {
        return Err(BridgeError::BadState);
    }
    if rounded > usize::MAX as f64 {
        return Err(BridgeError::BadState);
    }

    Ok(rounded as usize)
}

impl StebbsState {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, STEBBS_STATE_FLAT_LEN)?;

        let mut idx = 0_usize;

        let kdown2d = flat[idx];
        idx += 1;
        let kup2d = flat[idx];
        idx += 1;
        let kwest = flat[idx];
        idx += 1;
        let ksouth = flat[idx];
        idx += 1;
        let knorth = flat[idx];
        idx += 1;
        let keast = flat[idx];
        idx += 1;
        let ldown2d = flat[idx];
        idx += 1;
        let lup2d = flat[idx];
        idx += 1;
        let lwest = flat[idx];
        idx += 1;
        let lsouth = flat[idx];
        idx += 1;
        let lnorth = flat[idx];
        idx += 1;
        let least = flat[idx];
        idx += 1;

        let zarray = copy_fixed::<STEBBS_STATE_RSL_LEN>(&flat[idx..idx + STEBBS_STATE_RSL_LEN])?;
        idx += STEBBS_STATE_RSL_LEN;

        let dataout_line_ursl =
            copy_fixed::<STEBBS_STATE_RSL_LEN>(&flat[idx..idx + STEBBS_STATE_RSL_LEN])?;
        idx += STEBBS_STATE_RSL_LEN;

        let dataout_line_trsl =
            copy_fixed::<STEBBS_STATE_RSL_LEN>(&flat[idx..idx + STEBBS_STATE_RSL_LEN])?;
        idx += STEBBS_STATE_RSL_LEN;

        let dataout_line_qrsl =
            copy_fixed::<STEBBS_STATE_RSL_LEN>(&flat[idx..idx + STEBBS_STATE_RSL_LEN])?;
        idx += STEBBS_STATE_RSL_LEN;

        let deep_soil_temperature = flat[idx];
        idx += 1;
        let outdoor_air_start_temperature = flat[idx];
        idx += 1;
        let indoor_air_start_temperature = flat[idx];
        idx += 1;
        let indoor_mass_start_temperature = flat[idx];
        idx += 1;
        let wall_indoor_surface_temperature = flat[idx];
        idx += 1;
        let wall_outdoor_surface_temperature = flat[idx];
        idx += 1;
        let roof_indoor_surface_temperature = flat[idx];
        idx += 1;
        let roof_outdoor_surface_temperature = flat[idx];
        idx += 1;
        let window_indoor_surface_temperature = flat[idx];
        idx += 1;
        let window_outdoor_surface_temperature = flat[idx];
        idx += 1;
        let ground_floor_indoor_surface_temperature = flat[idx];
        idx += 1;
        let ground_floor_outdoor_surface_temperature = flat[idx];
        idx += 1;
        let water_tank_temperature = flat[idx];
        idx += 1;
        let internal_wall_water_tank_temperature = flat[idx];
        idx += 1;
        let external_wall_water_tank_temperature = flat[idx];
        idx += 1;
        let mains_water_temperature = flat[idx];
        idx += 1;
        let domestic_hot_water_temperature_in_use_in_building = flat[idx];
        idx += 1;
        let internal_wall_dhw_vessel_temperature = flat[idx];
        idx += 1;
        let external_wall_dhw_vessel_temperature = flat[idx];
        idx += 1;
        let qs_stebbs = flat[idx];
        idx += 1;

        let buildings_count = parse_buildings_count(flat[idx])?;
        idx += 1;
        let iter_safe = flat[idx] >= 0.5;

        Ok(Self {
            kdown2d,
            kup2d,
            kwest,
            ksouth,
            knorth,
            keast,
            ldown2d,
            lup2d,
            lwest,
            lsouth,
            lnorth,
            least,
            zarray,
            dataout_line_ursl,
            dataout_line_trsl,
            dataout_line_qrsl,
            deep_soil_temperature,
            outdoor_air_start_temperature,
            indoor_air_start_temperature,
            indoor_mass_start_temperature,
            wall_indoor_surface_temperature,
            wall_outdoor_surface_temperature,
            roof_indoor_surface_temperature,
            roof_outdoor_surface_temperature,
            window_indoor_surface_temperature,
            window_outdoor_surface_temperature,
            ground_floor_indoor_surface_temperature,
            ground_floor_outdoor_surface_temperature,
            water_tank_temperature,
            internal_wall_water_tank_temperature,
            external_wall_water_tank_temperature,
            mains_water_temperature,
            domestic_hot_water_temperature_in_use_in_building,
            internal_wall_dhw_vessel_temperature,
            external_wall_dhw_vessel_temperature,
            qs_stebbs,
            buildings_count,
            iter_safe,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(STEBBS_STATE_FLAT_LEN);

        flat.push(self.kdown2d);
        flat.push(self.kup2d);
        flat.push(self.kwest);
        flat.push(self.ksouth);
        flat.push(self.knorth);
        flat.push(self.keast);
        flat.push(self.ldown2d);
        flat.push(self.lup2d);
        flat.push(self.lwest);
        flat.push(self.lsouth);
        flat.push(self.lnorth);
        flat.push(self.least);

        flat.extend_from_slice(&self.zarray);
        flat.extend_from_slice(&self.dataout_line_ursl);
        flat.extend_from_slice(&self.dataout_line_trsl);
        flat.extend_from_slice(&self.dataout_line_qrsl);

        flat.push(self.deep_soil_temperature);
        flat.push(self.outdoor_air_start_temperature);
        flat.push(self.indoor_air_start_temperature);
        flat.push(self.indoor_mass_start_temperature);
        flat.push(self.wall_indoor_surface_temperature);
        flat.push(self.wall_outdoor_surface_temperature);
        flat.push(self.roof_indoor_surface_temperature);
        flat.push(self.roof_outdoor_surface_temperature);
        flat.push(self.window_indoor_surface_temperature);
        flat.push(self.window_outdoor_surface_temperature);
        flat.push(self.ground_floor_indoor_surface_temperature);
        flat.push(self.ground_floor_outdoor_surface_temperature);
        flat.push(self.water_tank_temperature);
        flat.push(self.internal_wall_water_tank_temperature);
        flat.push(self.external_wall_water_tank_temperature);
        flat.push(self.mains_water_temperature);
        flat.push(self.domestic_hot_water_temperature_in_use_in_building);
        flat.push(self.internal_wall_dhw_vessel_temperature);
        flat.push(self.external_wall_dhw_vessel_temperature);
        flat.push(self.qs_stebbs);
        flat.push(self.buildings_count as f64);
        flat.push(if self.iter_safe { 1.0 } else { 0.0 });

        flat
    }
}

impl StateCodec for StebbsState {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "STEBBS_STATE".to_string(),
            schema_version: STEBBS_STATE_SCHEMA_VERSION,
            flat_len: STEBBS_STATE_FLAT_LEN,
            field_names: stebbs_state_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        StebbsState::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        StebbsState::to_flat(self)
    }
}

pub fn stebbs_state_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_stebbs_state_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn stebbs_state_schema_info() -> Result<StebbsStateSchema, BridgeError> {
    let flat_len = stebbs_state_schema()?;
    let schema_version_runtime = stebbs_state_schema_version_runtime()?;
    let field_names = stebbs_state_field_names();

    if schema_version_runtime != STEBBS_STATE_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(StebbsStateSchema {
        schema_version: STEBBS_STATE_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

fn append_indexed_names(names: &mut Vec<String>, prefix: &str, len: usize) {
    for idx in 1..=len {
        names.push(format!("{prefix}.{idx}"));
    }
}

pub fn stebbs_state_field_names() -> Vec<String> {
    let mut names = Vec::with_capacity(STEBBS_STATE_FLAT_LEN);

    names.push("kdown2d".to_string());
    names.push("kup2d".to_string());
    names.push("kwest".to_string());
    names.push("ksouth".to_string());
    names.push("knorth".to_string());
    names.push("keast".to_string());
    names.push("ldown2d".to_string());
    names.push("lup2d".to_string());
    names.push("lwest".to_string());
    names.push("lsouth".to_string());
    names.push("lnorth".to_string());
    names.push("least".to_string());

    append_indexed_names(&mut names, "zarray", STEBBS_STATE_RSL_LEN);
    append_indexed_names(&mut names, "dataout_line_ursl", STEBBS_STATE_RSL_LEN);
    append_indexed_names(&mut names, "dataout_line_trsl", STEBBS_STATE_RSL_LEN);
    append_indexed_names(&mut names, "dataout_line_qrsl", STEBBS_STATE_RSL_LEN);

    names.push("deep_soil_temperature".to_string());
    names.push("outdoor_air_start_temperature".to_string());
    names.push("indoor_air_start_temperature".to_string());
    names.push("indoor_mass_start_temperature".to_string());
    names.push("wall_indoor_surface_temperature".to_string());
    names.push("wall_outdoor_surface_temperature".to_string());
    names.push("roof_indoor_surface_temperature".to_string());
    names.push("roof_outdoor_surface_temperature".to_string());
    names.push("window_indoor_surface_temperature".to_string());
    names.push("window_outdoor_surface_temperature".to_string());
    names.push("ground_floor_indoor_surface_temperature".to_string());
    names.push("ground_floor_outdoor_surface_temperature".to_string());
    names.push("water_tank_temperature".to_string());
    names.push("internal_wall_water_tank_temperature".to_string());
    names.push("external_wall_water_tank_temperature".to_string());
    names.push("mains_water_temperature".to_string());
    names.push("domestic_hot_water_temperature_in_use_in_building".to_string());
    names.push("internal_wall_dhw_vessel_temperature".to_string());
    names.push("external_wall_dhw_vessel_temperature".to_string());
    names.push("qs_stebbs".to_string());
    names.push("buildings_count".to_string());
    names.push("iter_safe".to_string());

    names
}

pub fn stebbs_state_schema_version() -> u32 {
    STEBBS_STATE_SCHEMA_VERSION
}

pub fn stebbs_state_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_stebbs_state_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn stebbs_state_field_index(name: &str) -> Option<usize> {
    let names = stebbs_state_field_names();
    field_index(&names, name)
}

pub fn stebbs_state_to_map(state: &StebbsState) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn stebbs_state_to_ordered_values(state: &StebbsState) -> Vec<f64> {
    state.to_flat()
}

pub fn stebbs_state_from_ordered_values(values: &[f64]) -> Result<StebbsState, BridgeError> {
    StebbsState::from_flat(values)
}

pub fn stebbs_state_to_values_payload(state: &StebbsState) -> StebbsStateValuesPayload {
    to_values_payload(state)
}

pub fn stebbs_state_from_values_payload(
    payload: &StebbsStateValuesPayload,
) -> Result<StebbsState, BridgeError> {
    from_values_payload(payload)
}

pub fn stebbs_state_from_map(values: &BTreeMap<String, f64>) -> Result<StebbsState, BridgeError> {
    let default_state = stebbs_state_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn stebbs_state_default_from_fortran() -> Result<StebbsState, BridgeError> {
    let n_flat = stebbs_state_schema()?;
    if n_flat != STEBBS_STATE_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_stebbs_state_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    StebbsState::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = stebbs_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, STEBBS_STATE_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = stebbs_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, StebbsState::default());
        let state2 = StebbsState::from_flat(&state.to_flat()).expect("flat roundtrip should work");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = stebbs_state_default_from_fortran().expect("default state should be available");
        let mut mapped = stebbs_state_to_map(&state);
        mapped.insert("kdown2d".to_string(), 410.0);
        mapped.insert("zarray.1".to_string(), 12.0);
        mapped.insert("buildings_count".to_string(), 3.0);
        mapped.insert("iter_safe".to_string(), 1.0);

        let updated = stebbs_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.kdown2d - 410.0).abs() < 1.0e-12);
        assert!((updated.zarray[0] - 12.0).abs() < 1.0e-12);
        assert_eq!(updated.buildings_count, 3);
        assert!(updated.iter_safe);
    }

    #[test]
    fn buildings_count_must_be_non_negative_integer() {
        let mut payload = stebbs_state_to_values_payload(&StebbsState::default());
        payload.values[STEBBS_STATE_FLAT_LEN - 2] = 1.5;

        let err = stebbs_state_from_values_payload(&payload)
            .expect_err("non-integer buildings_count should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = stebbs_state_default_from_fortran().expect("default state should be available");
        let payload = stebbs_state_to_values_payload(&state);
        let recovered =
            stebbs_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = StebbsStateValuesPayload {
            schema_version: STEBBS_STATE_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = stebbs_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
