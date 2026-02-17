use crate::codec::{
    dims_element_count, field_index, require_field_dims, validate_flat_len, PayloadDims,
    ValuesPayloadWithDims,
};
use crate::error::BridgeError;
use crate::ffi;
use crate::NSURF;
use std::collections::BTreeMap;

const SURFACE_NAMES: [&str; NSURF] = ["paved", "bldg", "evetr", "dectr", "grass", "bsoil", "water"];

pub const HYDRO_STATE_BASE_FLAT_LEN: usize = 10 * NSURF + 34;
pub const HYDRO_STATE_SCHEMA_VERSION: u32 = 1;

pub const HYDRO_STATE_SOILSTORE_ROOF_FIELD: &str = "soilstore_roof";
pub const HYDRO_STATE_STATE_ROOF_FIELD: &str = "state_roof";
pub const HYDRO_STATE_SOILSTORE_WALL_FIELD: &str = "soilstore_wall";
pub const HYDRO_STATE_STATE_WALL_FIELD: &str = "state_wall";
pub const HYDRO_STATE_EV_ROOF_FIELD: &str = "ev_roof";
pub const HYDRO_STATE_EV_WALL_FIELD: &str = "ev_wall";

const HYDRO_STATE_ALLOC_FIELDS: [&str; 6] = [
    HYDRO_STATE_SOILSTORE_ROOF_FIELD,
    HYDRO_STATE_STATE_ROOF_FIELD,
    HYDRO_STATE_SOILSTORE_WALL_FIELD,
    HYDRO_STATE_STATE_WALL_FIELD,
    HYDRO_STATE_EV_ROOF_FIELD,
    HYDRO_STATE_EV_WALL_FIELD,
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HydroStateSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub base_flat_len: usize,
    pub field_names: Vec<String>,
    pub allocatable_dims: PayloadDims,
}

pub type HydroStateValuesPayload = ValuesPayloadWithDims;

#[derive(Debug, Clone, PartialEq)]
pub struct HydroState {
    pub soilstore_surf: [f64; NSURF],
    pub state_surf: [f64; NSURF],
    pub wuday_id: [f64; 9],
    pub soilstore_roof: Vec<f64>,
    pub state_roof: Vec<f64>,
    pub soilstore_wall: Vec<f64>,
    pub state_wall: Vec<f64>,
    pub ev_roof: Vec<f64>,
    pub ev_wall: Vec<f64>,
    pub ev0_surf: [f64; NSURF],
    pub ev_surf: [f64; NSURF],
    pub wu_surf: [f64; NSURF],
    pub runoff_soil: [f64; NSURF],
    pub smd_surf: [f64; NSURF],
    pub drain_surf: [f64; NSURF],
    pub drain_per_tstep: f64,
    pub ev_per_tstep: f64,
    pub wu_ext: f64,
    pub wu_int: f64,
    pub runoff_agveg: f64,
    pub runoff_agimpervious: f64,
    pub runoff_per_tstep: f64,
    pub runoff_pipes: f64,
    pub runoff_soil_per_tstep: f64,
    pub runoff_waterbody: f64,
    pub smd: f64,
    pub soil_state: f64,
    pub state_per_tstep: f64,
    pub surf_chang_per_tstep: f64,
    pub tot_chang_per_tstep: f64,
    pub runoff_per_interval: f64,
    pub nwstate_per_tstep: f64,
    pub soil_moist_cap: f64,
    pub vsmd: f64,
    pub additional_water: f64,
    pub add_impervious: f64,
    pub add_pipes: f64,
    pub add_veg: f64,
    pub add_waterbody: f64,
    pub add_water: [f64; NSURF],
    pub frac_water2runoff: [f64; NSURF],
    pub iter_safe: bool,
}

impl Default for HydroState {
    fn default() -> Self {
        Self {
            soilstore_surf: [0.0; NSURF],
            state_surf: [0.0; NSURF],
            wuday_id: [0.0; 9],
            soilstore_roof: Vec::new(),
            state_roof: Vec::new(),
            soilstore_wall: Vec::new(),
            state_wall: Vec::new(),
            ev_roof: Vec::new(),
            ev_wall: Vec::new(),
            ev0_surf: [0.0; NSURF],
            ev_surf: [0.0; NSURF],
            wu_surf: [0.0; NSURF],
            runoff_soil: [0.0; NSURF],
            smd_surf: [0.0; NSURF],
            drain_surf: [0.0; NSURF],
            drain_per_tstep: 0.0,
            ev_per_tstep: 0.0,
            wu_ext: 0.0,
            wu_int: 0.0,
            runoff_agveg: 0.0,
            runoff_agimpervious: 0.0,
            runoff_per_tstep: 0.0,
            runoff_pipes: 0.0,
            runoff_soil_per_tstep: 0.0,
            runoff_waterbody: 0.0,
            smd: 0.0,
            soil_state: 0.0,
            state_per_tstep: 0.0,
            surf_chang_per_tstep: 0.0,
            tot_chang_per_tstep: 0.0,
            runoff_per_interval: 0.0,
            nwstate_per_tstep: 0.0,
            soil_moist_cap: 0.0,
            vsmd: 0.0,
            additional_water: 0.0,
            add_impervious: 0.0,
            add_pipes: 0.0,
            add_veg: 0.0,
            add_waterbody: 0.0,
            add_water: [0.0; NSURF],
            frac_water2runoff: [0.0; NSURF],
            iter_safe: false,
        }
    }
}

fn alloc_lens_from_state(state: &HydroState) -> [usize; 6] {
    [
        state.soilstore_roof.len(),
        state.state_roof.len(),
        state.soilstore_wall.len(),
        state.state_wall.len(),
        state.ev_roof.len(),
        state.ev_wall.len(),
    ]
}

fn parse_alloc_field_index(name: &str, field: &str) -> Option<usize> {
    let suffix = name.strip_prefix(field)?.strip_prefix('_')?;
    let one_based = suffix.parse::<usize>().ok()?;
    one_based.checked_sub(1)
}

fn surface_index(surface_name: &str) -> Option<usize> {
    SURFACE_NAMES
        .iter()
        .position(|candidate| *candidate == surface_name)
}

fn parse_surface_field_index(name: &str, field: &str) -> Option<usize> {
    let surface_name = name.strip_prefix(field)?.strip_prefix('.')?;
    surface_index(surface_name)
}

fn parse_wuday_id_index(name: &str) -> Option<usize> {
    let suffix = name.strip_prefix("wuday_id.")?;
    let one_based = suffix.parse::<usize>().ok()?;
    let zero_based = one_based.checked_sub(1)?;
    if zero_based < 9 {
        Some(zero_based)
    } else {
        None
    }
}

impl HydroState {
    pub fn from_flat_with_lens(flat: &[f64], alloc_lens: [usize; 6]) -> Result<Self, BridgeError> {
        let expected_len = HYDRO_STATE_BASE_FLAT_LEN
            .checked_add(alloc_lens.iter().sum::<usize>())
            .ok_or(BridgeError::BadState)?;
        validate_flat_len(flat, expected_len)?;

        let mut idx = 0_usize;
        let mut next = || {
            let value = flat[idx];
            idx += 1;
            value
        };

        let mut state = Self::default();

        for i in 0..NSURF {
            state.soilstore_surf[i] = next();
        }
        for i in 0..NSURF {
            state.state_surf[i] = next();
        }
        for i in 0..9 {
            state.wuday_id[i] = next();
        }

        state.soilstore_roof = vec![0.0; alloc_lens[0]];
        for i in 0..alloc_lens[0] {
            state.soilstore_roof[i] = next();
        }

        state.state_roof = vec![0.0; alloc_lens[1]];
        for i in 0..alloc_lens[1] {
            state.state_roof[i] = next();
        }

        state.soilstore_wall = vec![0.0; alloc_lens[2]];
        for i in 0..alloc_lens[2] {
            state.soilstore_wall[i] = next();
        }

        state.state_wall = vec![0.0; alloc_lens[3]];
        for i in 0..alloc_lens[3] {
            state.state_wall[i] = next();
        }

        state.ev_roof = vec![0.0; alloc_lens[4]];
        for i in 0..alloc_lens[4] {
            state.ev_roof[i] = next();
        }

        state.ev_wall = vec![0.0; alloc_lens[5]];
        for i in 0..alloc_lens[5] {
            state.ev_wall[i] = next();
        }

        for i in 0..NSURF {
            state.ev0_surf[i] = next();
        }
        for i in 0..NSURF {
            state.ev_surf[i] = next();
        }
        for i in 0..NSURF {
            state.wu_surf[i] = next();
        }
        for i in 0..NSURF {
            state.runoff_soil[i] = next();
        }
        for i in 0..NSURF {
            state.smd_surf[i] = next();
        }
        for i in 0..NSURF {
            state.drain_surf[i] = next();
        }

        state.drain_per_tstep = next();
        state.ev_per_tstep = next();
        state.wu_ext = next();
        state.wu_int = next();
        state.runoff_agveg = next();
        state.runoff_agimpervious = next();
        state.runoff_per_tstep = next();
        state.runoff_pipes = next();
        state.runoff_soil_per_tstep = next();
        state.runoff_waterbody = next();
        state.smd = next();
        state.soil_state = next();
        state.state_per_tstep = next();
        state.surf_chang_per_tstep = next();
        state.tot_chang_per_tstep = next();
        state.runoff_per_interval = next();
        state.nwstate_per_tstep = next();
        state.soil_moist_cap = next();
        state.vsmd = next();
        state.additional_water = next();
        state.add_impervious = next();
        state.add_pipes = next();
        state.add_veg = next();
        state.add_waterbody = next();

        for i in 0..NSURF {
            state.add_water[i] = next();
        }
        for i in 0..NSURF {
            state.frac_water2runoff[i] = next();
        }

        state.iter_safe = next() >= 0.5;

        Ok(state)
    }

    pub fn from_ordered_values(values: &[f64]) -> Result<Self, BridgeError> {
        let (expected_len, alloc_lens) = hydro_state_schema()?;
        validate_flat_len(values, expected_len)?;
        Self::from_flat_with_lens(values, alloc_lens)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let alloc_lens = alloc_lens_from_state(self);
        let mut flat =
            Vec::with_capacity(HYDRO_STATE_BASE_FLAT_LEN + alloc_lens.iter().sum::<usize>());

        flat.extend_from_slice(&self.soilstore_surf);
        flat.extend_from_slice(&self.state_surf);
        flat.extend_from_slice(&self.wuday_id);
        flat.extend_from_slice(&self.soilstore_roof);
        flat.extend_from_slice(&self.state_roof);
        flat.extend_from_slice(&self.soilstore_wall);
        flat.extend_from_slice(&self.state_wall);
        flat.extend_from_slice(&self.ev_roof);
        flat.extend_from_slice(&self.ev_wall);
        flat.extend_from_slice(&self.ev0_surf);
        flat.extend_from_slice(&self.ev_surf);
        flat.extend_from_slice(&self.wu_surf);
        flat.extend_from_slice(&self.runoff_soil);
        flat.extend_from_slice(&self.smd_surf);
        flat.extend_from_slice(&self.drain_surf);

        flat.push(self.drain_per_tstep);
        flat.push(self.ev_per_tstep);
        flat.push(self.wu_ext);
        flat.push(self.wu_int);
        flat.push(self.runoff_agveg);
        flat.push(self.runoff_agimpervious);
        flat.push(self.runoff_per_tstep);
        flat.push(self.runoff_pipes);
        flat.push(self.runoff_soil_per_tstep);
        flat.push(self.runoff_waterbody);
        flat.push(self.smd);
        flat.push(self.soil_state);
        flat.push(self.state_per_tstep);
        flat.push(self.surf_chang_per_tstep);
        flat.push(self.tot_chang_per_tstep);
        flat.push(self.runoff_per_interval);
        flat.push(self.nwstate_per_tstep);
        flat.push(self.soil_moist_cap);
        flat.push(self.vsmd);
        flat.push(self.additional_water);
        flat.push(self.add_impervious);
        flat.push(self.add_pipes);
        flat.push(self.add_veg);
        flat.push(self.add_waterbody);

        flat.extend_from_slice(&self.add_water);
        flat.extend_from_slice(&self.frac_water2runoff);

        flat.push(if self.iter_safe { 1.0 } else { 0.0 });

        flat
    }
}

pub fn hydro_state_schema() -> Result<(usize, [usize; 6]), BridgeError> {
    let mut n_flat = -1_i32;
    let mut soilstore_roof_len = -1_i32;
    let mut state_roof_len = -1_i32;
    let mut soilstore_wall_len = -1_i32;
    let mut state_wall_len = -1_i32;
    let mut ev_roof_len = -1_i32;
    let mut ev_wall_len = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_hydro_state_len(
            &mut n_flat as *mut i32,
            &mut soilstore_roof_len as *mut i32,
            &mut state_roof_len as *mut i32,
            &mut soilstore_wall_len as *mut i32,
            &mut state_wall_len as *mut i32,
            &mut ev_roof_len as *mut i32,
            &mut ev_wall_len as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    if n_flat < 0
        || soilstore_roof_len < 0
        || state_roof_len < 0
        || soilstore_wall_len < 0
        || state_wall_len < 0
        || ev_roof_len < 0
        || ev_wall_len < 0
    {
        return Err(BridgeError::BadState);
    }

    Ok((
        n_flat as usize,
        [
            soilstore_roof_len as usize,
            state_roof_len as usize,
            soilstore_wall_len as usize,
            state_wall_len as usize,
            ev_roof_len as usize,
            ev_wall_len as usize,
        ],
    ))
}

pub fn hydro_state_schema_info() -> Result<HydroStateSchema, BridgeError> {
    let (flat_len, alloc_lens) = hydro_state_schema()?;
    let schema_version_runtime = hydro_state_schema_version_runtime()?;
    let field_names = hydro_state_field_names_with_lens(alloc_lens);

    let mut allocatable_dims = PayloadDims::new();
    allocatable_dims.insert(
        HYDRO_STATE_SOILSTORE_ROOF_FIELD.to_string(),
        vec![alloc_lens[0]],
    );
    allocatable_dims.insert(
        HYDRO_STATE_STATE_ROOF_FIELD.to_string(),
        vec![alloc_lens[1]],
    );
    allocatable_dims.insert(
        HYDRO_STATE_SOILSTORE_WALL_FIELD.to_string(),
        vec![alloc_lens[2]],
    );
    allocatable_dims.insert(
        HYDRO_STATE_STATE_WALL_FIELD.to_string(),
        vec![alloc_lens[3]],
    );
    allocatable_dims.insert(HYDRO_STATE_EV_ROOF_FIELD.to_string(), vec![alloc_lens[4]]);
    allocatable_dims.insert(HYDRO_STATE_EV_WALL_FIELD.to_string(), vec![alloc_lens[5]]);

    let expected_len = HYDRO_STATE_BASE_FLAT_LEN
        .checked_add(alloc_lens.iter().sum::<usize>())
        .ok_or(BridgeError::BadState)?;

    if schema_version_runtime != HYDRO_STATE_SCHEMA_VERSION
        || flat_len != expected_len
        || flat_len != field_names.len()
    {
        return Err(BridgeError::BadState);
    }

    Ok(HydroStateSchema {
        schema_version: HYDRO_STATE_SCHEMA_VERSION,
        flat_len,
        base_flat_len: HYDRO_STATE_BASE_FLAT_LEN,
        field_names,
        allocatable_dims,
    })
}

pub fn hydro_state_schema_version() -> u32 {
    HYDRO_STATE_SCHEMA_VERSION
}

pub fn hydro_state_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_hydro_state_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

fn push_surface_field_names(names: &mut Vec<String>, prefix: &str) {
    for surface in SURFACE_NAMES {
        names.push(format!("{prefix}.{surface}"));
    }
}

fn push_alloc_field_names(names: &mut Vec<String>, prefix: &str, len: usize) {
    for idx in 0..len {
        names.push(format!("{prefix}_{}", idx + 1));
    }
}

pub fn hydro_state_base_field_names() -> Vec<String> {
    let mut names = Vec::with_capacity(HYDRO_STATE_BASE_FLAT_LEN);

    push_surface_field_names(&mut names, "soilstore_surf");
    push_surface_field_names(&mut names, "state_surf");

    for idx in 1..=9 {
        names.push(format!("wuday_id.{idx}"));
    }

    push_surface_field_names(&mut names, "ev0_surf");
    push_surface_field_names(&mut names, "ev_surf");
    push_surface_field_names(&mut names, "wu_surf");
    push_surface_field_names(&mut names, "runoff_soil");
    push_surface_field_names(&mut names, "smd_surf");
    push_surface_field_names(&mut names, "drain_surf");

    names.push("drain_per_tstep".to_string());
    names.push("ev_per_tstep".to_string());
    names.push("wu_ext".to_string());
    names.push("wu_int".to_string());
    names.push("runoff_agveg".to_string());
    names.push("runoff_agimpervious".to_string());
    names.push("runoff_per_tstep".to_string());
    names.push("runoff_pipes".to_string());
    names.push("runoff_soil_per_tstep".to_string());
    names.push("runoff_waterbody".to_string());
    names.push("smd".to_string());
    names.push("soil_state".to_string());
    names.push("state_per_tstep".to_string());
    names.push("surf_chang_per_tstep".to_string());
    names.push("tot_chang_per_tstep".to_string());
    names.push("runoff_per_interval".to_string());
    names.push("nwstate_per_tstep".to_string());
    names.push("soil_moist_cap".to_string());
    names.push("vsmd".to_string());
    names.push("additional_water".to_string());
    names.push("add_impervious".to_string());
    names.push("add_pipes".to_string());
    names.push("add_veg".to_string());
    names.push("add_waterbody".to_string());

    push_surface_field_names(&mut names, "add_water");
    push_surface_field_names(&mut names, "frac_water2runoff");

    names.push("iter_safe".to_string());

    names
}

pub fn hydro_state_field_names_with_lens(alloc_lens: [usize; 6]) -> Vec<String> {
    let base_names = hydro_state_base_field_names();
    let prefix_len = 2 * NSURF + 9;
    let mut names =
        Vec::with_capacity(HYDRO_STATE_BASE_FLAT_LEN + alloc_lens.iter().sum::<usize>());

    names.extend(base_names.iter().take(prefix_len).cloned());

    push_alloc_field_names(&mut names, HYDRO_STATE_SOILSTORE_ROOF_FIELD, alloc_lens[0]);
    push_alloc_field_names(&mut names, HYDRO_STATE_STATE_ROOF_FIELD, alloc_lens[1]);
    push_alloc_field_names(&mut names, HYDRO_STATE_SOILSTORE_WALL_FIELD, alloc_lens[2]);
    push_alloc_field_names(&mut names, HYDRO_STATE_STATE_WALL_FIELD, alloc_lens[3]);
    push_alloc_field_names(&mut names, HYDRO_STATE_EV_ROOF_FIELD, alloc_lens[4]);
    push_alloc_field_names(&mut names, HYDRO_STATE_EV_WALL_FIELD, alloc_lens[5]);
    names.extend(base_names.into_iter().skip(prefix_len));

    names
}

pub fn hydro_state_field_names() -> Result<Vec<String>, BridgeError> {
    let (_, alloc_lens) = hydro_state_schema()?;
    Ok(hydro_state_field_names_with_lens(alloc_lens))
}

pub fn hydro_state_field_index(name: &str) -> Option<usize> {
    let names = hydro_state_field_names().ok()?;
    field_index(&names, name)
}

pub fn hydro_state_to_map(state: &HydroState) -> BTreeMap<String, f64> {
    let names = hydro_state_field_names_with_lens(alloc_lens_from_state(state));
    let values = state.to_flat();
    names.into_iter().zip(values).collect()
}

pub fn hydro_state_to_ordered_values(state: &HydroState) -> Vec<f64> {
    state.to_flat()
}

pub fn hydro_state_to_values_payload(state: &HydroState) -> HydroStateValuesPayload {
    let alloc_lens = alloc_lens_from_state(state);
    let mut dims = PayloadDims::new();

    dims.insert(
        HYDRO_STATE_SOILSTORE_ROOF_FIELD.to_string(),
        vec![alloc_lens[0]],
    );
    dims.insert(
        HYDRO_STATE_STATE_ROOF_FIELD.to_string(),
        vec![alloc_lens[1]],
    );
    dims.insert(
        HYDRO_STATE_SOILSTORE_WALL_FIELD.to_string(),
        vec![alloc_lens[2]],
    );
    dims.insert(
        HYDRO_STATE_STATE_WALL_FIELD.to_string(),
        vec![alloc_lens[3]],
    );
    dims.insert(HYDRO_STATE_EV_ROOF_FIELD.to_string(), vec![alloc_lens[4]]);
    dims.insert(HYDRO_STATE_EV_WALL_FIELD.to_string(), vec![alloc_lens[5]]);

    HydroStateValuesPayload {
        schema_version: HYDRO_STATE_SCHEMA_VERSION,
        values: state.to_flat(),
        dims,
    }
}

pub fn hydro_state_from_ordered_values(values: &[f64]) -> Result<HydroState, BridgeError> {
    HydroState::from_ordered_values(values)
}

pub fn hydro_state_from_values_payload(
    payload: &HydroStateValuesPayload,
) -> Result<HydroState, BridgeError> {
    if payload.schema_version != HYDRO_STATE_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    if payload.dims.len() != HYDRO_STATE_ALLOC_FIELDS.len() {
        return Err(BridgeError::BadState);
    }

    let soilstore_roof_len = dims_element_count(&require_field_dims(
        &payload.dims,
        HYDRO_STATE_SOILSTORE_ROOF_FIELD,
        1,
    )?)?;
    let state_roof_len = dims_element_count(&require_field_dims(
        &payload.dims,
        HYDRO_STATE_STATE_ROOF_FIELD,
        1,
    )?)?;
    let soilstore_wall_len = dims_element_count(&require_field_dims(
        &payload.dims,
        HYDRO_STATE_SOILSTORE_WALL_FIELD,
        1,
    )?)?;
    let state_wall_len = dims_element_count(&require_field_dims(
        &payload.dims,
        HYDRO_STATE_STATE_WALL_FIELD,
        1,
    )?)?;
    let ev_roof_len = dims_element_count(&require_field_dims(
        &payload.dims,
        HYDRO_STATE_EV_ROOF_FIELD,
        1,
    )?)?;
    let ev_wall_len = dims_element_count(&require_field_dims(
        &payload.dims,
        HYDRO_STATE_EV_WALL_FIELD,
        1,
    )?)?;

    let alloc_lens = [
        soilstore_roof_len,
        state_roof_len,
        soilstore_wall_len,
        state_wall_len,
        ev_roof_len,
        ev_wall_len,
    ];

    let expected_len = HYDRO_STATE_BASE_FLAT_LEN
        .checked_add(alloc_lens.iter().sum::<usize>())
        .ok_or(BridgeError::BadState)?;
    validate_flat_len(&payload.values, expected_len)?;

    HydroState::from_flat_with_lens(&payload.values, alloc_lens)
}

pub fn hydro_state_from_map(values: &BTreeMap<String, f64>) -> Result<HydroState, BridgeError> {
    let mut state = hydro_state_default_from_fortran()?;

    for (name, value) in values {
        if let Some(index) = parse_surface_field_index(name, "soilstore_surf") {
            state.soilstore_surf[index] = *value;
            continue;
        }
        if let Some(index) = parse_surface_field_index(name, "state_surf") {
            state.state_surf[index] = *value;
            continue;
        }
        if let Some(index) = parse_wuday_id_index(name) {
            state.wuday_id[index] = *value;
            continue;
        }
        if let Some(index) = parse_alloc_field_index(name, HYDRO_STATE_SOILSTORE_ROOF_FIELD) {
            if state.soilstore_roof.len() <= index {
                state.soilstore_roof.resize(index + 1, 0.0);
            }
            state.soilstore_roof[index] = *value;
            continue;
        }
        if let Some(index) = parse_alloc_field_index(name, HYDRO_STATE_STATE_ROOF_FIELD) {
            if state.state_roof.len() <= index {
                state.state_roof.resize(index + 1, 0.0);
            }
            state.state_roof[index] = *value;
            continue;
        }
        if let Some(index) = parse_alloc_field_index(name, HYDRO_STATE_SOILSTORE_WALL_FIELD) {
            if state.soilstore_wall.len() <= index {
                state.soilstore_wall.resize(index + 1, 0.0);
            }
            state.soilstore_wall[index] = *value;
            continue;
        }
        if let Some(index) = parse_alloc_field_index(name, HYDRO_STATE_STATE_WALL_FIELD) {
            if state.state_wall.len() <= index {
                state.state_wall.resize(index + 1, 0.0);
            }
            state.state_wall[index] = *value;
            continue;
        }
        if let Some(index) = parse_alloc_field_index(name, HYDRO_STATE_EV_ROOF_FIELD) {
            if state.ev_roof.len() <= index {
                state.ev_roof.resize(index + 1, 0.0);
            }
            state.ev_roof[index] = *value;
            continue;
        }
        if let Some(index) = parse_alloc_field_index(name, HYDRO_STATE_EV_WALL_FIELD) {
            if state.ev_wall.len() <= index {
                state.ev_wall.resize(index + 1, 0.0);
            }
            state.ev_wall[index] = *value;
            continue;
        }
        if let Some(index) = parse_surface_field_index(name, "ev0_surf") {
            state.ev0_surf[index] = *value;
            continue;
        }
        if let Some(index) = parse_surface_field_index(name, "ev_surf") {
            state.ev_surf[index] = *value;
            continue;
        }
        if let Some(index) = parse_surface_field_index(name, "wu_surf") {
            state.wu_surf[index] = *value;
            continue;
        }
        if let Some(index) = parse_surface_field_index(name, "runoff_soil") {
            state.runoff_soil[index] = *value;
            continue;
        }
        if let Some(index) = parse_surface_field_index(name, "smd_surf") {
            state.smd_surf[index] = *value;
            continue;
        }
        if let Some(index) = parse_surface_field_index(name, "drain_surf") {
            state.drain_surf[index] = *value;
            continue;
        }
        if let Some(index) = parse_surface_field_index(name, "add_water") {
            state.add_water[index] = *value;
            continue;
        }
        if let Some(index) = parse_surface_field_index(name, "frac_water2runoff") {
            state.frac_water2runoff[index] = *value;
            continue;
        }

        match name.as_str() {
            "drain_per_tstep" => state.drain_per_tstep = *value,
            "ev_per_tstep" => state.ev_per_tstep = *value,
            "wu_ext" => state.wu_ext = *value,
            "wu_int" => state.wu_int = *value,
            "runoff_agveg" => state.runoff_agveg = *value,
            "runoff_agimpervious" => state.runoff_agimpervious = *value,
            "runoff_per_tstep" => state.runoff_per_tstep = *value,
            "runoff_pipes" => state.runoff_pipes = *value,
            "runoff_soil_per_tstep" => state.runoff_soil_per_tstep = *value,
            "runoff_waterbody" => state.runoff_waterbody = *value,
            "smd" => state.smd = *value,
            "soil_state" => state.soil_state = *value,
            "state_per_tstep" => state.state_per_tstep = *value,
            "surf_chang_per_tstep" => state.surf_chang_per_tstep = *value,
            "tot_chang_per_tstep" => state.tot_chang_per_tstep = *value,
            "runoff_per_interval" => state.runoff_per_interval = *value,
            "nwstate_per_tstep" => state.nwstate_per_tstep = *value,
            "soil_moist_cap" => state.soil_moist_cap = *value,
            "vsmd" => state.vsmd = *value,
            "additional_water" => state.additional_water = *value,
            "add_impervious" => state.add_impervious = *value,
            "add_pipes" => state.add_pipes = *value,
            "add_veg" => state.add_veg = *value,
            "add_waterbody" => state.add_waterbody = *value,
            "iter_safe" => state.iter_safe = *value >= 0.5,
            _ => return Err(BridgeError::BadState),
        }
    }

    Ok(state)
}

pub fn hydro_state_default_from_fortran() -> Result<HydroState, BridgeError> {
    let (n_flat, alloc_lens_expected) = hydro_state_schema()?;
    let mut flat = vec![0.0_f64; n_flat];
    let mut soilstore_roof_len = -1_i32;
    let mut state_roof_len = -1_i32;
    let mut soilstore_wall_len = -1_i32;
    let mut state_wall_len = -1_i32;
    let mut ev_roof_len = -1_i32;
    let mut ev_wall_len = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_hydro_state_default(
            flat.as_mut_ptr(),
            n_flat as i32,
            &mut soilstore_roof_len as *mut i32,
            &mut state_roof_len as *mut i32,
            &mut soilstore_wall_len as *mut i32,
            &mut state_wall_len as *mut i32,
            &mut ev_roof_len as *mut i32,
            &mut ev_wall_len as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    if soilstore_roof_len < 0
        || state_roof_len < 0
        || soilstore_wall_len < 0
        || state_wall_len < 0
        || ev_roof_len < 0
        || ev_wall_len < 0
    {
        return Err(BridgeError::BadState);
    }

    let alloc_lens = [
        soilstore_roof_len as usize,
        state_roof_len as usize,
        soilstore_wall_len as usize,
        state_wall_len as usize,
        ev_roof_len as usize,
        ev_wall_len as usize,
    ];

    if alloc_lens != alloc_lens_expected {
        return Err(BridgeError::BadState);
    }

    HydroState::from_flat_with_lens(&flat, alloc_lens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let (flat_len, alloc_lens) = hydro_state_schema().expect("schema call should succeed");
        assert_eq!(
            flat_len,
            HYDRO_STATE_BASE_FLAT_LEN + alloc_lens.iter().sum::<usize>()
        );
    }

    #[test]
    fn default_state_roundtrip() {
        let state = hydro_state_default_from_fortran().expect("default state should be available");
        assert!(state.soilstore_roof.is_empty());
        assert!(state.state_roof.is_empty());
        assert!(state.soilstore_wall.is_empty());
        assert!(state.state_wall.is_empty());
        assert!(state.ev_roof.is_empty());
        assert!(state.ev_wall.is_empty());

        let alloc_lens = alloc_lens_from_state(&state);
        let state2 = HydroState::from_flat_with_lens(&state.to_flat(), alloc_lens)
            .expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = hydro_state_default_from_fortran().expect("default state should be available");
        let mut mapped = hydro_state_to_map(&state);
        mapped.insert("soilstore_surf.paved".to_string(), 1.2);
        mapped.insert("wuday_id.2".to_string(), 2.4);
        mapped.insert("soilstore_roof_2".to_string(), 3.6);
        mapped.insert("ev_wall_1".to_string(), 4.8);

        let updated = hydro_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.soilstore_surf[0] - 1.2).abs() < 1.0e-12);
        assert!((updated.wuday_id[1] - 2.4).abs() < 1.0e-12);
        assert_eq!(updated.soilstore_roof.len(), 2);
        assert!((updated.soilstore_roof[1] - 3.6).abs() < 1.0e-12);
        assert_eq!(updated.ev_wall.len(), 1);
        assert!((updated.ev_wall[0] - 4.8).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let mut state =
            hydro_state_default_from_fortran().expect("default state should be available");
        state.soilstore_roof = vec![1.0, 2.0];
        state.state_roof = vec![3.0];
        state.soilstore_wall = vec![4.0, 5.0, 6.0];
        state.state_wall = vec![7.0];
        state.ev_roof = vec![8.0];
        state.ev_wall = vec![9.0, 10.0];
        state.iter_safe = true;

        let payload = hydro_state_to_values_payload(&state);
        let recovered =
            hydro_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = HydroStateValuesPayload {
            schema_version: HYDRO_STATE_SCHEMA_VERSION + 1,
            values: payload.values.clone(),
            dims: payload.dims.clone(),
        };
        let err = hydro_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_missing_dims() {
        let state = hydro_state_default_from_fortran().expect("default state should be available");
        let payload = HydroStateValuesPayload {
            schema_version: HYDRO_STATE_SCHEMA_VERSION,
            values: state.to_flat(),
            dims: PayloadDims::new(),
        };
        let err = hydro_state_from_values_payload(&payload).expect_err("missing dims should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_dims_length_mismatch() {
        let mut state =
            hydro_state_default_from_fortran().expect("default state should be available");
        state.ev_wall = vec![5.0];

        let mut payload = hydro_state_to_values_payload(&state);
        payload
            .dims
            .insert(HYDRO_STATE_EV_WALL_FIELD.to_string(), vec![2]);

        let err = hydro_state_from_values_payload(&payload)
            .expect_err("dims/values mismatch should fail");
        assert_eq!(err, BridgeError::BadBuffer);
    }

    #[test]
    fn values_payload_rejects_rank_mismatch() {
        let state = hydro_state_default_from_fortran().expect("default state should be available");
        let mut payload = hydro_state_to_values_payload(&state);
        payload
            .dims
            .insert(HYDRO_STATE_SOILSTORE_ROOF_FIELD.to_string(), vec![1, 1]);

        let err = hydro_state_from_values_payload(&payload).expect_err("rank mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
