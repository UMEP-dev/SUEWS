use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const NSURF: usize = 7;
pub const ANOHM_MAX_SAMPLES: usize = 48;
pub const OHM_STATE_V1_FLAT_LEN: usize = 53;
pub const OHM_STATE_FLAT_LEN: usize =
    OHM_STATE_V1_FLAT_LEN + 5 + 14 * ANOHM_MAX_SAMPLES + 3 * NSURF;
pub const OHM_STATE_SCHEMA_VERSION: u32 = 2;
pub const SURFACE_NAMES: [&str; NSURF] =
    ["paved", "bldg", "evetr", "dectr", "grass", "bsoil", "water"];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OhmStateSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub nsurf: usize,
    pub surface_names: Vec<String>,
    pub field_names: Vec<String>,
}

pub type OhmStateValuesPayload = ValuesPayload;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OhmStepResult {
    pub qn1_av_next: f64,
    pub dqndt_next: f64,
    pub qs: f64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OhmState {
    pub qn_av: f64,
    pub dqndt: f64,
    pub qn_surfs: [f64; NSURF],
    pub dqndt_surf: [f64; NSURF],
    pub qn_s_av: f64,
    pub dqnsdt: f64,
    pub a1: f64,
    pub a2: f64,
    pub a3: f64,
    pub t2_prev: f64,
    pub ws_rav: f64,
    pub tair_prev: f64,
    pub qn_rav: [f64; NSURF],
    // Surface order: paved, bldg, evetr, dectr, grass, bsoil, water.
    pub dyn_a1: [f64; NSURF],
    pub dyn_a2: [f64; NSURF],
    pub dyn_a3: [f64; NSURF],
    pub iter_safe: bool,
    pub anohm_working_day: i32,
    pub anohm_working_count: i32,
    pub anohm_working_t_hr: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_working_sd: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_working_ta: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_working_rh: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_working_pres: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_working_ws: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_working_ah: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_coeff_day: i32,
    pub anohm_coeff_count: i32,
    pub anohm_coeff_ready: bool,
    pub anohm_coeff_t_hr: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_coeff_sd: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_coeff_ta: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_coeff_rh: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_coeff_pres: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_coeff_ws: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_coeff_ah: [f64; ANOHM_MAX_SAMPLES],
    pub anohm_a1_surf: [f64; NSURF],
    pub anohm_a2_surf: [f64; NSURF],
    pub anohm_a3_surf: [f64; NSURF],
}

impl Default for OhmState {
    fn default() -> Self {
        Self {
            qn_av: 0.0,
            dqndt: 0.0,
            qn_surfs: [0.0; NSURF],
            dqndt_surf: [0.0; NSURF],
            qn_s_av: 0.0,
            dqnsdt: 0.0,
            a1: 0.0,
            a2: 0.0,
            a3: 0.0,
            t2_prev: 0.0,
            ws_rav: 0.0,
            tair_prev: 0.0,
            qn_rav: [0.0; NSURF],
            dyn_a1: [0.0; NSURF],
            dyn_a2: [0.0; NSURF],
            dyn_a3: [0.0; NSURF],
            iter_safe: true,
            anohm_working_day: -999,
            anohm_working_count: 0,
            anohm_working_t_hr: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_working_sd: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_working_ta: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_working_rh: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_working_pres: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_working_ws: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_working_ah: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_coeff_day: -999,
            anohm_coeff_count: 0,
            anohm_coeff_ready: false,
            anohm_coeff_t_hr: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_coeff_sd: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_coeff_ta: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_coeff_rh: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_coeff_pres: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_coeff_ws: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_coeff_ah: [-999.0; ANOHM_MAX_SAMPLES],
            anohm_a1_surf: [-999.0; NSURF],
            anohm_a2_surf: [-999.0; NSURF],
            anohm_a3_surf: [-999.0; NSURF],
        }
    }
}

fn decode_int(value: f64) -> Result<i32, BridgeError> {
    if !value.is_finite() {
        return Err(BridgeError::BadState);
    }

    let rounded = value.round();
    if (value - rounded).abs() > 1.0e-9 || rounded < i32::MIN as f64 || rounded > i32::MAX as f64 {
        return Err(BridgeError::BadState);
    }

    Ok(rounded as i32)
}

fn decode_sample_count(value: f64) -> Result<i32, BridgeError> {
    let count = decode_int(value)?;
    if !(0..=ANOHM_MAX_SAMPLES as i32).contains(&count) {
        return Err(BridgeError::BadState);
    }
    Ok(count)
}

fn take_array<const N: usize>(flat: &[f64], idx: &mut usize) -> [f64; N] {
    let mut values = [0.0; N];
    values.copy_from_slice(&flat[*idx..*idx + N]);
    *idx += N;
    values
}

impl OhmState {
    fn from_v1_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, OHM_STATE_V1_FLAT_LEN)?;
        let mut idx = 0_usize;
        let mut next = || {
            let value = flat[idx];
            idx += 1;
            value
        };

        let mut state = Self {
            qn_av: next(),
            dqndt: next(),
            ..Self::default()
        };

        for i in 0..NSURF {
            state.qn_surfs[i] = next();
        }
        for i in 0..NSURF {
            state.dqndt_surf[i] = next();
        }

        state.qn_s_av = next();
        state.dqnsdt = next();
        state.a1 = next();
        state.a2 = next();
        state.a3 = next();
        state.t2_prev = next();
        state.ws_rav = next();
        state.tair_prev = next();

        for i in 0..NSURF {
            state.qn_rav[i] = next();
        }
        for i in 0..NSURF {
            state.dyn_a1[i] = next();
        }
        for i in 0..NSURF {
            state.dyn_a2[i] = next();
        }
        for i in 0..NSURF {
            state.dyn_a3[i] = next();
        }

        state.iter_safe = next() >= 0.5;

        Ok(state)
    }

    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, OHM_STATE_FLAT_LEN)?;

        let mut state = Self::from_v1_flat(&flat[..OHM_STATE_V1_FLAT_LEN])?;
        let mut idx = OHM_STATE_V1_FLAT_LEN;

        state.anohm_working_day = decode_int(flat[idx])?;
        idx += 1;
        state.anohm_working_count = decode_sample_count(flat[idx])?;
        idx += 1;
        state.anohm_working_t_hr = take_array(flat, &mut idx);
        state.anohm_working_sd = take_array(flat, &mut idx);
        state.anohm_working_ta = take_array(flat, &mut idx);
        state.anohm_working_rh = take_array(flat, &mut idx);
        state.anohm_working_pres = take_array(flat, &mut idx);
        state.anohm_working_ws = take_array(flat, &mut idx);
        state.anohm_working_ah = take_array(flat, &mut idx);

        state.anohm_coeff_day = decode_int(flat[idx])?;
        idx += 1;
        state.anohm_coeff_count = decode_sample_count(flat[idx])?;
        idx += 1;
        state.anohm_coeff_ready = flat[idx] >= 0.5;
        idx += 1;
        state.anohm_coeff_t_hr = take_array(flat, &mut idx);
        state.anohm_coeff_sd = take_array(flat, &mut idx);
        state.anohm_coeff_ta = take_array(flat, &mut idx);
        state.anohm_coeff_rh = take_array(flat, &mut idx);
        state.anohm_coeff_pres = take_array(flat, &mut idx);
        state.anohm_coeff_ws = take_array(flat, &mut idx);
        state.anohm_coeff_ah = take_array(flat, &mut idx);
        state.anohm_a1_surf = take_array(flat, &mut idx);
        state.anohm_a2_surf = take_array(flat, &mut idx);
        state.anohm_a3_surf = take_array(flat, &mut idx);

        debug_assert_eq!(idx, OHM_STATE_FLAT_LEN);
        Ok(state)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(OHM_STATE_FLAT_LEN);

        flat.push(self.qn_av);
        flat.push(self.dqndt);
        flat.extend_from_slice(&self.qn_surfs);
        flat.extend_from_slice(&self.dqndt_surf);

        flat.push(self.qn_s_av);
        flat.push(self.dqnsdt);
        flat.push(self.a1);
        flat.push(self.a2);
        flat.push(self.a3);
        flat.push(self.t2_prev);
        flat.push(self.ws_rav);
        flat.push(self.tair_prev);

        flat.extend_from_slice(&self.qn_rav);
        flat.extend_from_slice(&self.dyn_a1);
        flat.extend_from_slice(&self.dyn_a2);
        flat.extend_from_slice(&self.dyn_a3);
        flat.push(if self.iter_safe { 1.0 } else { 0.0 });

        flat.push(self.anohm_working_day as f64);
        flat.push(self.anohm_working_count as f64);
        flat.extend_from_slice(&self.anohm_working_t_hr);
        flat.extend_from_slice(&self.anohm_working_sd);
        flat.extend_from_slice(&self.anohm_working_ta);
        flat.extend_from_slice(&self.anohm_working_rh);
        flat.extend_from_slice(&self.anohm_working_pres);
        flat.extend_from_slice(&self.anohm_working_ws);
        flat.extend_from_slice(&self.anohm_working_ah);

        flat.push(self.anohm_coeff_day as f64);
        flat.push(self.anohm_coeff_count as f64);
        flat.push(if self.anohm_coeff_ready { 1.0 } else { 0.0 });
        flat.extend_from_slice(&self.anohm_coeff_t_hr);
        flat.extend_from_slice(&self.anohm_coeff_sd);
        flat.extend_from_slice(&self.anohm_coeff_ta);
        flat.extend_from_slice(&self.anohm_coeff_rh);
        flat.extend_from_slice(&self.anohm_coeff_pres);
        flat.extend_from_slice(&self.anohm_coeff_ws);
        flat.extend_from_slice(&self.anohm_coeff_ah);
        flat.extend_from_slice(&self.anohm_a1_surf);
        flat.extend_from_slice(&self.anohm_a2_surf);
        flat.extend_from_slice(&self.anohm_a3_surf);

        flat
    }
}

impl StateCodec for OhmState {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "OHM_STATE".to_string(),
            schema_version: OHM_STATE_SCHEMA_VERSION,
            flat_len: OHM_STATE_FLAT_LEN,
            field_names: ohm_state_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        OhmState::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        OhmState::to_flat(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OhmModelState {
    pub dt_seconds: i32,
    pub dt_since_start: i32,
    pub qn1_av: f64,
    pub dqndt: f64,
    pub a1: f64,
    pub a2: f64,
    pub a3: f64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OhmModel {
    state: OhmModelState,
}

impl OhmModel {
    pub fn new(
        a1: f64,
        a2: f64,
        a3: f64,
        dt_seconds: i32,
        qn1_av: f64,
        dqndt: f64,
        dt_since_start: i32,
    ) -> Self {
        Self {
            state: OhmModelState {
                dt_seconds,
                dt_since_start,
                qn1_av,
                dqndt,
                a1,
                a2,
                a3,
            },
        }
    }

    pub fn step(&mut self, qn1: f64) -> Result<f64, BridgeError> {
        let result = ohm_step(
            self.state.dt_seconds,
            self.state.dt_since_start,
            self.state.qn1_av,
            self.state.dqndt,
            qn1,
            self.state.a1,
            self.state.a2,
            self.state.a3,
        )?;

        self.state.qn1_av = result.qn1_av_next;
        self.state.dqndt = result.dqndt_next;
        self.state.dt_since_start = self
            .state
            .dt_since_start
            .saturating_add(self.state.dt_seconds);

        Ok(result.qs)
    }

    pub fn state(&self) -> OhmModelState {
        self.state
    }
}

pub fn qs_calc(qn1: f64, dqndt: f64, a1: f64, a2: f64, a3: f64) -> Result<f64, BridgeError> {
    let mut qs = -999.0_f64;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_qs_calc(
            qn1,
            dqndt,
            a1,
            a2,
            a3,
            &mut qs as *mut f64,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(qs)
}

pub fn dqndt_step(
    dt: i32,
    dt_since_start: i32,
    qn1_av_prev: f64,
    qn1: f64,
    dqndt_prev: f64,
) -> Result<(f64, f64), BridgeError> {
    let mut qn1_av_next = -999.0_f64;
    let mut dqndt_next = -999.0_f64;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_dqndt_step(
            dt,
            dt_since_start,
            qn1_av_prev,
            qn1,
            dqndt_prev,
            &mut qn1_av_next as *mut f64,
            &mut dqndt_next as *mut f64,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok((qn1_av_next, dqndt_next))
}

pub fn ohm_step(
    dt: i32,
    dt_since_start: i32,
    qn1_av_prev: f64,
    dqndt_prev: f64,
    qn1: f64,
    a1: f64,
    a2: f64,
    a3: f64,
) -> Result<OhmStepResult, BridgeError> {
    let mut qn1_av_next = -999.0_f64;
    let mut dqndt_next = -999.0_f64;
    let mut qs = -999.0_f64;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_step(
            dt,
            dt_since_start,
            qn1_av_prev,
            dqndt_prev,
            qn1,
            a1,
            a2,
            a3,
            &mut qn1_av_next as *mut f64,
            &mut dqndt_next as *mut f64,
            &mut qs as *mut f64,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(OhmStepResult {
        qn1_av_next,
        dqndt_next,
        qs,
    })
}

pub fn ohm_state_schema() -> Result<(usize, usize), BridgeError> {
    let mut n_flat = -1_i32;
    let mut nsurf_out = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_state_len(
            &mut n_flat as *mut i32,
            &mut nsurf_out as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok((n_flat as usize, nsurf_out as usize))
}

pub fn ohm_state_schema_info() -> Result<OhmStateSchema, BridgeError> {
    let (flat_len, nsurf) = ohm_state_schema()?;
    let schema_version_runtime = ohm_state_schema_version_runtime()?;
    let surface_names = ohm_surface_names();
    let field_names = ohm_state_field_names();

    if schema_version_runtime != OHM_STATE_SCHEMA_VERSION
        || flat_len != field_names.len()
        || nsurf != surface_names.len()
    {
        return Err(BridgeError::BadState);
    }

    Ok(OhmStateSchema {
        schema_version: OHM_STATE_SCHEMA_VERSION,
        flat_len,
        nsurf,
        surface_names,
        field_names,
    })
}

pub fn ohm_state_field_names() -> Vec<String> {
    fn push_surface_fields(names: &mut Vec<String>, prefix: &str) {
        for surface in SURFACE_NAMES {
            names.push(format!("{prefix}.{surface}"));
        }
    }

    fn push_indexed_fields(names: &mut Vec<String>, prefix: &str, len: usize) {
        for idx in 1..=len {
            names.push(format!("{prefix}.{idx}"));
        }
    }

    let mut names = Vec::with_capacity(OHM_STATE_FLAT_LEN);

    names.push("qn_av".to_string());
    names.push("dqndt".to_string());
    push_surface_fields(&mut names, "qn_surfs");
    push_surface_fields(&mut names, "dqndt_surf");

    names.push("qn_s_av".to_string());
    names.push("dqnsdt".to_string());
    names.push("a1".to_string());
    names.push("a2".to_string());
    names.push("a3".to_string());
    names.push("t2_prev".to_string());
    names.push("ws_rav".to_string());
    names.push("tair_prev".to_string());

    push_surface_fields(&mut names, "qn_rav");
    push_surface_fields(&mut names, "dyn_a1");
    push_surface_fields(&mut names, "dyn_a2");
    push_surface_fields(&mut names, "dyn_a3");

    names.push("iter_safe".to_string());
    names.push("anohm_working_day".to_string());
    names.push("anohm_working_count".to_string());
    push_indexed_fields(&mut names, "anohm_working_t_hr", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_working_sd", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_working_ta", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_working_rh", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_working_pres", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_working_ws", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_working_ah", ANOHM_MAX_SAMPLES);

    names.push("anohm_coeff_day".to_string());
    names.push("anohm_coeff_count".to_string());
    names.push("anohm_coeff_ready".to_string());
    push_indexed_fields(&mut names, "anohm_coeff_t_hr", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_coeff_sd", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_coeff_ta", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_coeff_rh", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_coeff_pres", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_coeff_ws", ANOHM_MAX_SAMPLES);
    push_indexed_fields(&mut names, "anohm_coeff_ah", ANOHM_MAX_SAMPLES);
    push_surface_fields(&mut names, "anohm_a1_surf");
    push_surface_fields(&mut names, "anohm_a2_surf");
    push_surface_fields(&mut names, "anohm_a3_surf");

    debug_assert_eq!(names.len(), OHM_STATE_FLAT_LEN);
    names
}

pub fn ohm_state_schema_version() -> u32 {
    OHM_STATE_SCHEMA_VERSION
}

pub fn ohm_state_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_state_schema_version(&mut schema_version as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn ohm_state_field_index(name: &str) -> Option<usize> {
    let names = ohm_state_field_names();
    field_index(&names, name)
}

pub fn ohm_surface_names() -> Vec<String> {
    SURFACE_NAMES
        .iter()
        .map(|name| (*name).to_string())
        .collect()
}

pub fn ohm_state_to_map(state: &OhmState) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn ohm_state_to_ordered_values(state: &OhmState) -> Vec<f64> {
    state.to_flat()
}

pub fn ohm_state_from_ordered_values(values: &[f64]) -> Result<OhmState, BridgeError> {
    OhmState::from_flat(values)
}

pub fn ohm_state_to_values_payload(state: &OhmState) -> OhmStateValuesPayload {
    to_values_payload(state)
}

pub fn ohm_state_from_values_payload(
    payload: &OhmStateValuesPayload,
) -> Result<OhmState, BridgeError> {
    if payload.schema_version == 1 {
        return OhmState::from_v1_flat(&payload.values);
    }
    from_values_payload(payload)
}

pub fn ohm_state_from_map(values: &BTreeMap<String, f64>) -> Result<OhmState, BridgeError> {
    let default_state = ohm_state_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn ohm_state_default_from_fortran() -> Result<OhmState, BridgeError> {
    let (n_flat, nsurf_out) = ohm_state_schema()?;
    if n_flat != OHM_STATE_FLAT_LEN || nsurf_out != NSURF {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_state_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    OhmState::from_flat(&flat)
}

pub fn ohm_state_step(
    state: &mut OhmState,
    dt: i32,
    dt_since_start: i32,
    qn1: f64,
    a1: f64,
    a2: f64,
    a3: f64,
) -> Result<f64, BridgeError> {
    let flat_in = state.to_flat();
    let mut flat_out = vec![0.0_f64; OHM_STATE_FLAT_LEN];
    let mut qs = -999.0_f64;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_state_step(
            flat_in.as_ptr(),
            OHM_STATE_FLAT_LEN as i32,
            dt,
            dt_since_start,
            qn1,
            a1,
            a2,
            a3,
            flat_out.as_mut_ptr(),
            OHM_STATE_FLAT_LEN as i32,
            &mut qs as *mut f64,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    *state = OhmState::from_flat(&flat_out)?;
    Ok(qs)
}

/// Compute sun azimuth and zenith angles.
///
/// Returns `(azimuth, zenith)` in degrees.
#[cfg(feature = "physics")]
pub fn sunposition_calc(
    year: f64,
    idectime: f64,
    utc: f64,
    lat: f64,
    lon: f64,
    alt: f64,
) -> Result<(f64, f64), BridgeError> {
    let mut azimuth = 0.0_f64;
    let mut zenith = 0.0_f64;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_sunposition_calc(
            year,
            idectime,
            utc,
            lat,
            lon,
            alt,
            &mut azimuth as *mut f64,
            &mut zenith as *mut f64,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok((azimuth, zenith))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    #[test]
    fn qs_calc_matches_formula() {
        let qn1 = 250.0;
        let dqndt = 12.5;
        let a1 = 0.35;
        let a2 = 0.08;
        let a3 = 10.0;

        let qs = qs_calc(qn1, dqndt, a1, a2, a3).expect("qs_calc should succeed");
        let expected = qn1 * a1 + dqndt * a2 + a3;

        assert!((qs - expected).abs() < 1.0e-12);
    }

    #[test]
    fn model_step_updates_state() {
        let mut model = OhmModel::new(0.3, 0.1, 5.0, 300, 0.0, 0.0, 0);

        let qs = model.step(200.0).expect("step should succeed");
        let st = model.state();

        assert!(qs.is_finite());
        assert!(st.qn1_av.is_finite());
        assert!(st.dqndt.is_finite());
        assert_eq!(st.dt_since_start, 300);
    }

    #[test]
    fn step_rejects_non_positive_dt() {
        let err = ohm_step(0, 0, 0.0, 0.0, 120.0, 0.3, 0.05, 1.0)
            .expect_err("step should fail for zero dt");
        assert_eq!(err, BridgeError::BadDt);
    }

    #[test]
    fn state_schema_matches_expected_dimensions() {
        let (n_flat, nsurf_out) = ohm_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, OHM_STATE_FLAT_LEN);
        assert_eq!(nsurf_out, NSURF);
    }

    #[test]
    fn state_field_names_match_flat_schema() {
        let names = ohm_state_field_names();
        assert_eq!(names.len(), OHM_STATE_FLAT_LEN);
        assert_eq!(
            names.first().expect("field list should not be empty"),
            "qn_av"
        );
        assert_eq!(
            names.last().expect("field list should not be empty"),
            "anohm_a3_surf.water"
        );
    }

    #[test]
    fn default_state_roundtrip_and_step() {
        let mut state =
            ohm_state_default_from_fortran().expect("default state should be available");
        assert!(state.iter_safe);
        state.anohm_a1_surf = [0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17];
        state.anohm_a2_surf = [0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27];
        state.anohm_a3_surf = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0];
        let expected_a1 = state.anohm_a1_surf;
        let expected_a2 = state.anohm_a2_surf;
        let expected_a3 = state.anohm_a3_surf;

        let qs = ohm_state_step(&mut state, 300, 0, 200.0, 0.3, 0.1, 5.0)
            .expect("state step should succeed");

        assert!(qs.is_finite());
        assert!(state.qn_av.is_finite());
        assert!(state.dqndt.is_finite());
        assert_eq!(state.anohm_a1_surf, expected_a1);
        assert_eq!(state.anohm_a2_surf, expected_a2);
        assert_eq!(state.anohm_a3_surf, expected_a3);

        let flat = state.to_flat();
        let state2 = OhmState::from_flat(&flat).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = ohm_state_default_from_fortran().expect("default state should be available");
        let mut mapped = ohm_state_to_map(&state);
        mapped.insert("qn_surfs.paved".to_string(), 123.0);

        let updated = ohm_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.qn_surfs[0] - 123.0).abs() < 1.0e-12);
    }

    #[test]
    fn state_map_rejects_unknown_keys() {
        let mut mapped = BTreeMap::new();
        mapped.insert("not_a_real_field".to_string(), 1.0);

        let err = ohm_state_from_map(&mapped).expect_err("unknown field should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn state_schema_info_is_consistent() {
        let schema = ohm_state_schema_info().expect("schema info should be available");
        assert_eq!(schema.schema_version, OHM_STATE_SCHEMA_VERSION);
        assert_eq!(schema.flat_len, OHM_STATE_FLAT_LEN);
        assert_eq!(schema.nsurf, NSURF);
        assert_eq!(schema.field_names.len(), OHM_STATE_FLAT_LEN);
        assert_eq!(schema.surface_names.len(), NSURF);
    }

    #[test]
    fn runtime_schema_version_matches_static() {
        let runtime =
            ohm_state_schema_version_runtime().expect("runtime schema version should work");
        assert_eq!(runtime, OHM_STATE_SCHEMA_VERSION);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let mut state =
            ohm_state_default_from_fortran().expect("default state should be available");
        state.anohm_a1_surf = [0.31, 0.32, 0.33, 0.34, 0.35, 0.36, 0.37];
        state.anohm_a2_surf = [0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47];
        state.anohm_a3_surf = [11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0];
        let payload = ohm_state_to_values_payload(&state);
        let recovered =
            ohm_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);
        assert_eq!(recovered.anohm_a1_surf, state.anohm_a1_surf);
        assert_eq!(recovered.anohm_a2_surf, state.anohm_a2_surf);
        assert_eq!(recovered.anohm_a3_surf, state.anohm_a3_surf);

        let bad_payload = OhmStateValuesPayload {
            schema_version: OHM_STATE_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = ohm_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_restores_v1_with_default_anohm_buffers() {
        let state = ohm_state_default_from_fortran().expect("default state should be available");
        let payload = OhmStateValuesPayload {
            schema_version: 1,
            values: state.to_flat()[..OHM_STATE_V1_FLAT_LEN].to_vec(),
        };

        let recovered = ohm_state_from_values_payload(&payload)
            .expect("version-1 OHM state should remain readable");

        assert_eq!(recovered.qn_av, state.qn_av);
        assert_eq!(recovered.anohm_working_day, -999);
        assert_eq!(recovered.anohm_working_count, 0);
        assert!(!recovered.anohm_coeff_ready);
        assert_eq!(recovered.anohm_working_sd, [-999.0; ANOHM_MAX_SAMPLES]);
        assert_eq!(recovered.anohm_a1_surf, [-999.0; NSURF]);
        assert_eq!(recovered.anohm_a2_surf, [-999.0; NSURF]);
        assert_eq!(recovered.anohm_a3_surf, [-999.0; NSURF]);
    }

    #[test]
    fn from_flat_requires_exact_length() {
        let good = vec![0.0_f64; OHM_STATE_FLAT_LEN];
        assert!(OhmState::from_flat(&good).is_ok());

        let short = vec![0.0_f64; OHM_STATE_FLAT_LEN - 1];
        let err = OhmState::from_flat(&short).expect_err("short payload should fail");
        assert_eq!(err, BridgeError::BadBuffer);

        let long = vec![0.0_f64; OHM_STATE_FLAT_LEN + 1];
        let err = OhmState::from_flat(&long).expect_err("long payload should fail");
        assert_eq!(err, BridgeError::BadBuffer);
    }

    #[test]
    fn from_flat_rejects_anohm_count_outside_buffer() {
        let state = ohm_state_default_from_fortran().expect("default state should be available");
        let mut flat = state.to_flat();
        flat[OHM_STATE_V1_FLAT_LEN + 1] = (ANOHM_MAX_SAMPLES + 1) as f64;

        let err = OhmState::from_flat(&flat).expect_err("oversized sample count should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
