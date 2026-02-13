use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use crate::NSURF;
use std::collections::BTreeMap;

pub const PHENOLOGY_STATE_NVEGSURF: usize = 3;
pub const PHENOLOGY_STATE_STORE_DRAIN_ROWS: usize = 6;
pub const PHENOLOGY_STATE_FLAT_LEN: usize = 76;
pub const PHENOLOGY_STATE_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhenologyStateSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type PhenologyStateValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct PhenologyState {
    pub alb: [f64; NSURF],
    pub lai_id: [f64; PHENOLOGY_STATE_NVEGSURF],
    pub gdd_id: [f64; PHENOLOGY_STATE_NVEGSURF],
    pub sdd_id: [f64; PHENOLOGY_STATE_NVEGSURF],
    pub veg_phen_lumps: f64,
    pub porosity_id: f64,
    pub decidcap_id: f64,
    pub alb_dectr_id: f64,
    pub alb_evetr_id: f64,
    pub alb_grass_id: f64,
    pub tmin_id: f64,
    pub tmax_id: f64,
    pub len_day_id: f64,
    pub temp_veg: f64,
    pub store_drain_prm: [[f64; PHENOLOGY_STATE_STORE_DRAIN_ROWS]; NSURF],
    pub gfunc: f64,
    pub gsc: f64,
    pub g_kdown: f64,
    pub g_dq: f64,
    pub g_ta: f64,
    pub g_smd: f64,
    pub g_lai: f64,
    pub iter_safe: bool,
}

impl Default for PhenologyState {
    fn default() -> Self {
        Self {
            alb: [0.0; NSURF],
            lai_id: [0.0; PHENOLOGY_STATE_NVEGSURF],
            gdd_id: [0.0; PHENOLOGY_STATE_NVEGSURF],
            sdd_id: [0.0; PHENOLOGY_STATE_NVEGSURF],
            veg_phen_lumps: 0.0,
            porosity_id: 0.0,
            decidcap_id: 0.0,
            alb_dectr_id: 0.0,
            alb_evetr_id: 0.0,
            alb_grass_id: 0.0,
            tmin_id: 0.0,
            tmax_id: 0.0,
            len_day_id: 0.0,
            temp_veg: 0.0,
            store_drain_prm: [[0.0; PHENOLOGY_STATE_STORE_DRAIN_ROWS]; NSURF],
            gfunc: 0.0,
            gsc: 0.0,
            g_kdown: 0.0,
            g_dq: 0.0,
            g_ta: 0.0,
            g_smd: 0.0,
            g_lai: 0.0,
            iter_safe: false,
        }
    }
}

impl PhenologyState {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, PHENOLOGY_STATE_FLAT_LEN)?;

        let mut idx = 0_usize;
        let mut next = || {
            let value = flat[idx];
            idx += 1;
            value
        };

        let mut state = Self::default();

        for i in 0..NSURF {
            state.alb[i] = next();
        }

        for i in 0..PHENOLOGY_STATE_NVEGSURF {
            state.lai_id[i] = next();
        }

        for i in 0..PHENOLOGY_STATE_NVEGSURF {
            state.gdd_id[i] = next();
        }

        for i in 0..PHENOLOGY_STATE_NVEGSURF {
            state.sdd_id[i] = next();
        }

        state.veg_phen_lumps = next();
        state.porosity_id = next();
        state.decidcap_id = next();
        state.alb_dectr_id = next();
        state.alb_evetr_id = next();
        state.alb_grass_id = next();
        state.tmin_id = next();
        state.tmax_id = next();
        state.len_day_id = next();
        state.temp_veg = next();

        for surf in 0..NSURF {
            for row in 0..PHENOLOGY_STATE_STORE_DRAIN_ROWS {
                state.store_drain_prm[surf][row] = next();
            }
        }

        state.gfunc = next();
        state.gsc = next();
        state.g_kdown = next();
        state.g_dq = next();
        state.g_ta = next();
        state.g_smd = next();
        state.g_lai = next();
        state.iter_safe = next() >= 0.5;

        Ok(state)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(PHENOLOGY_STATE_FLAT_LEN);

        flat.extend_from_slice(&self.alb);
        flat.extend_from_slice(&self.lai_id);
        flat.extend_from_slice(&self.gdd_id);
        flat.extend_from_slice(&self.sdd_id);

        flat.push(self.veg_phen_lumps);
        flat.push(self.porosity_id);
        flat.push(self.decidcap_id);
        flat.push(self.alb_dectr_id);
        flat.push(self.alb_evetr_id);
        flat.push(self.alb_grass_id);
        flat.push(self.tmin_id);
        flat.push(self.tmax_id);
        flat.push(self.len_day_id);
        flat.push(self.temp_veg);

        for surf in 0..NSURF {
            flat.extend_from_slice(&self.store_drain_prm[surf]);
        }

        flat.push(self.gfunc);
        flat.push(self.gsc);
        flat.push(self.g_kdown);
        flat.push(self.g_dq);
        flat.push(self.g_ta);
        flat.push(self.g_smd);
        flat.push(self.g_lai);
        flat.push(if self.iter_safe { 1.0 } else { 0.0 });

        flat
    }
}

impl StateCodec for PhenologyState {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "PHENOLOGY_STATE".to_string(),
            schema_version: PHENOLOGY_STATE_SCHEMA_VERSION,
            flat_len: PHENOLOGY_STATE_FLAT_LEN,
            field_names: phenology_state_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        PhenologyState::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        PhenologyState::to_flat(self)
    }
}

pub fn phenology_state_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_phenology_state_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn phenology_state_schema_info() -> Result<PhenologyStateSchema, BridgeError> {
    let flat_len = phenology_state_schema()?;
    let schema_version_runtime = phenology_state_schema_version_runtime()?;
    let field_names = phenology_state_field_names();

    if schema_version_runtime != PHENOLOGY_STATE_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(PhenologyStateSchema {
        schema_version: PHENOLOGY_STATE_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn phenology_state_field_names() -> Vec<String> {
    fn surface_names() -> [&'static str; NSURF] {
        ["paved", "bldg", "evetr", "dectr", "grass", "bsoil", "water"]
    }

    fn veg_surface_names() -> [&'static str; PHENOLOGY_STATE_NVEGSURF] {
        ["evetr", "dectr", "grass"]
    }

    let mut names = Vec::with_capacity(PHENOLOGY_STATE_FLAT_LEN);

    for surface in surface_names() {
        names.push(format!("alb.{surface}"));
    }

    for veg_surface in veg_surface_names() {
        names.push(format!("lai_id.{veg_surface}"));
    }

    for veg_surface in veg_surface_names() {
        names.push(format!("gdd_id.{veg_surface}"));
    }

    for veg_surface in veg_surface_names() {
        names.push(format!("sdd_id.{veg_surface}"));
    }

    names.push("veg_phen_lumps".to_string());
    names.push("porosity_id".to_string());
    names.push("decidcap_id".to_string());
    names.push("alb_dectr_id".to_string());
    names.push("alb_evetr_id".to_string());
    names.push("alb_grass_id".to_string());
    names.push("tmin_id".to_string());
    names.push("tmax_id".to_string());
    names.push("len_day_id".to_string());
    names.push("temp_veg".to_string());

    for surface in surface_names() {
        for row in 1..=PHENOLOGY_STATE_STORE_DRAIN_ROWS {
            names.push(format!("store_drain_prm.{surface}.{row}"));
        }
    }

    names.push("gfunc".to_string());
    names.push("gsc".to_string());
    names.push("g_kdown".to_string());
    names.push("g_dq".to_string());
    names.push("g_ta".to_string());
    names.push("g_smd".to_string());
    names.push("g_lai".to_string());
    names.push("iter_safe".to_string());

    names
}

pub fn phenology_state_schema_version() -> u32 {
    PHENOLOGY_STATE_SCHEMA_VERSION
}

pub fn phenology_state_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_phenology_state_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn phenology_state_field_index(name: &str) -> Option<usize> {
    let names = phenology_state_field_names();
    field_index(&names, name)
}

pub fn phenology_state_to_map(state: &PhenologyState) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn phenology_state_to_ordered_values(state: &PhenologyState) -> Vec<f64> {
    state.to_flat()
}

pub fn phenology_state_from_ordered_values(values: &[f64]) -> Result<PhenologyState, BridgeError> {
    PhenologyState::from_flat(values)
}

pub fn phenology_state_to_values_payload(state: &PhenologyState) -> PhenologyStateValuesPayload {
    to_values_payload(state)
}

pub fn phenology_state_from_values_payload(
    payload: &PhenologyStateValuesPayload,
) -> Result<PhenologyState, BridgeError> {
    from_values_payload(payload)
}

pub fn phenology_state_from_map(
    values: &BTreeMap<String, f64>,
) -> Result<PhenologyState, BridgeError> {
    let default_state = phenology_state_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn phenology_state_default_from_fortran() -> Result<PhenologyState, BridgeError> {
    let n_flat = phenology_state_schema()?;
    if n_flat != PHENOLOGY_STATE_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_phenology_state_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    PhenologyState::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = phenology_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, PHENOLOGY_STATE_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            phenology_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, PhenologyState::default());
        let state2 =
            PhenologyState::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            phenology_state_default_from_fortran().expect("default state should be available");
        let mut mapped = phenology_state_to_map(&state);
        mapped.insert("alb.paved".to_string(), 0.17);
        mapped.insert("lai_id.evetr".to_string(), 1.9);
        mapped.insert("store_drain_prm.grass.4".to_string(), 0.22);
        mapped.insert("iter_safe".to_string(), 1.0);

        let updated = phenology_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.alb[0] - 0.17).abs() < 1.0e-12);
        assert!((updated.lai_id[0] - 1.9).abs() < 1.0e-12);
        assert!((updated.store_drain_prm[4][3] - 0.22).abs() < 1.0e-12);
        assert!(updated.iter_safe);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            phenology_state_default_from_fortran().expect("default state should be available");
        let payload = phenology_state_to_values_payload(&state);
        let recovered =
            phenology_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = PhenologyStateValuesPayload {
            schema_version: PHENOLOGY_STATE_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = phenology_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
