use crate::codec::{
    dims_element_count, field_index, require_field_dims, validate_flat_len, PayloadDims,
    ValuesPayloadWithDims,
};
use crate::error::BridgeError;
use crate::ffi;
use crate::NSURF;
use std::collections::BTreeMap;

const SURFACE_NAMES: [&str; NSURF] = ["paved", "bldg", "evetr", "dectr", "grass", "bsoil", "water"];
const RADIATION_BIN_COUNT: usize = 15;

pub const HEAT_STATE_BASE_FLAT_LEN: usize = 7 * NSURF + 79;
pub const HEAT_STATE_SCHEMA_VERSION: u32 = 1;

pub const HEAT_STATE_TEMP_ROOF_FIELD: &str = "temp_roof";

const HEAT_LAYER_DEPTH_FIELDS: [&str; 2] = ["temp_roof", "temp_wall"];
const HEAT_SURF_DEPTH_FIELDS: [&str; 2] = ["temp_surf", "temp_surf_dyohm"];

const HEAT_LAYER_VEC_FIELDS: [&str; 14] = [
    "tsfc_roof",
    "tsfc_wall",
    "tsfc_roof_stepstart",
    "tsfc_wall_stepstart",
    "qs_roof",
    "qn_roof",
    "qe_roof",
    "qh_roof",
    "qh_resist_roof",
    "qs_wall",
    "qn_wall",
    "qe_wall",
    "qh_wall",
    "qh_resist_wall",
];

const HEAT_SURF_VEC_FIELDS: [&str; 3] = ["tsfc_surf", "tsfc_surf_dyohm", "tsfc_surf_stepstart"];

const HEAT_ALLOC_FIELDS: [&str; 21] = [
    "temp_roof",
    "temp_wall",
    "temp_surf",
    "temp_surf_dyohm",
    "tsfc_roof",
    "tsfc_wall",
    "tsfc_surf",
    "tsfc_surf_dyohm",
    "tsfc_roof_stepstart",
    "tsfc_wall_stepstart",
    "tsfc_surf_stepstart",
    "qs_roof",
    "qn_roof",
    "qe_roof",
    "qh_roof",
    "qh_resist_roof",
    "qs_wall",
    "qn_wall",
    "qe_wall",
    "qh_wall",
    "qh_resist_wall",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HeatStateSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub base_flat_len: usize,
    pub nlayer: usize,
    pub ndepth: usize,
    pub field_names: Vec<String>,
    pub allocatable_dims: PayloadDims,
}

pub type HeatStateValuesPayload = ValuesPayloadWithDims;

#[derive(Debug, Clone, PartialEq)]
pub struct HeatState {
    pub nlayer: usize,
    pub ndepth: usize,

    pub temp_roof: Vec<f64>,
    pub temp_wall: Vec<f64>,
    pub temp_surf: Vec<f64>,
    pub temp_surf_dyohm: Vec<f64>,

    pub tsfc_roof: Vec<f64>,
    pub tsfc_wall: Vec<f64>,
    pub tsfc_surf: Vec<f64>,
    pub tsfc_surf_dyohm: Vec<f64>,
    pub tsfc_roof_stepstart: Vec<f64>,
    pub tsfc_wall_stepstart: Vec<f64>,
    pub tsfc_surf_stepstart: Vec<f64>,

    pub qs_roof: Vec<f64>,
    pub qn_roof: Vec<f64>,
    pub qe_roof: Vec<f64>,
    pub qh_roof: Vec<f64>,
    pub qh_resist_roof: Vec<f64>,

    pub qs_wall: Vec<f64>,
    pub qn_wall: Vec<f64>,
    pub qe_wall: Vec<f64>,
    pub qh_wall: Vec<f64>,
    pub qh_resist_wall: Vec<f64>,

    pub qs_surf: [f64; NSURF],
    pub qn_surf: [f64; NSURF],
    pub qe0_surf: [f64; NSURF],
    pub qe_surf: [f64; NSURF],
    pub qh_surf: [f64; NSURF],
    pub qh_resist_surf: [f64; NSURF],
    pub tsurf_ind: [f64; NSURF],

    pub qh_lumps: f64,
    pub qe_lumps: f64,
    pub kclear: f64,
    pub kup: f64,
    pub ldown: f64,
    pub lup: f64,
    pub qe: f64,
    pub qf: f64,
    pub qf_sahp: f64,
    pub qh: f64,
    pub qh_residual: f64,
    pub qh_resist: f64,
    pub qn: f64,
    pub qn_snowfree: f64,
    pub qs: f64,
    pub tsfc_c: f64,
    pub tsurf: f64,
    pub qh_init: f64,

    pub roof_in_sw_spc: [f64; RADIATION_BIN_COUNT],
    pub roof_in_lw_spc: [f64; RADIATION_BIN_COUNT],
    pub wall_in_sw_spc: [f64; RADIATION_BIN_COUNT],
    pub wall_in_lw_spc: [f64; RADIATION_BIN_COUNT],

    pub iter_safe: bool,
}

impl Default for HeatState {
    fn default() -> Self {
        Self {
            nlayer: 0,
            ndepth: 0,

            temp_roof: Vec::new(),
            temp_wall: Vec::new(),
            temp_surf: Vec::new(),
            temp_surf_dyohm: Vec::new(),

            tsfc_roof: Vec::new(),
            tsfc_wall: Vec::new(),
            tsfc_surf: Vec::new(),
            tsfc_surf_dyohm: Vec::new(),
            tsfc_roof_stepstart: Vec::new(),
            tsfc_wall_stepstart: Vec::new(),
            tsfc_surf_stepstart: Vec::new(),

            qs_roof: Vec::new(),
            qn_roof: Vec::new(),
            qe_roof: Vec::new(),
            qh_roof: Vec::new(),
            qh_resist_roof: Vec::new(),

            qs_wall: Vec::new(),
            qn_wall: Vec::new(),
            qe_wall: Vec::new(),
            qh_wall: Vec::new(),
            qh_resist_wall: Vec::new(),

            qs_surf: [0.0; NSURF],
            qn_surf: [0.0; NSURF],
            qe0_surf: [0.0; NSURF],
            qe_surf: [0.0; NSURF],
            qh_surf: [0.0; NSURF],
            qh_resist_surf: [0.0; NSURF],
            tsurf_ind: [0.0; NSURF],

            qh_lumps: 0.0,
            qe_lumps: 0.0,
            kclear: 0.0,
            kup: 0.0,
            ldown: 0.0,
            lup: 0.0,
            qe: 0.0,
            qf: 0.0,
            qf_sahp: 0.0,
            qh: 0.0,
            qh_residual: 0.0,
            qh_resist: 0.0,
            qn: 0.0,
            qn_snowfree: 0.0,
            qs: 0.0,
            tsfc_c: 0.0,
            tsurf: 0.0,
            qh_init: 0.0,

            roof_in_sw_spc: [0.0; RADIATION_BIN_COUNT],
            roof_in_lw_spc: [0.0; RADIATION_BIN_COUNT],
            wall_in_sw_spc: [0.0; RADIATION_BIN_COUNT],
            wall_in_lw_spc: [0.0; RADIATION_BIN_COUNT],

            iter_safe: true,
        }
    }
}

fn matrix_len(rows: usize, cols: usize) -> Result<usize, BridgeError> {
    rows.checked_mul(cols).ok_or(BridgeError::BadState)
}

pub fn heat_state_expected_flat_len(nlayer: usize, ndepth: usize) -> Result<usize, BridgeError> {
    if (nlayer == 0 && ndepth != 0) || (nlayer != 0 && ndepth == 0) {
        return Err(BridgeError::BadState);
    }

    if nlayer == 0 {
        return Ok(HEAT_STATE_BASE_FLAT_LEN);
    }

    let layer_depth = matrix_len(nlayer, ndepth)?;
    let surf_depth = matrix_len(NSURF, ndepth)?;

    let alloc_len = 2_usize
        .checked_mul(layer_depth)
        .and_then(|v| {
            2_usize
                .checked_mul(surf_depth)
                .and_then(|w| v.checked_add(w))
        })
        .and_then(|v| 14_usize.checked_mul(nlayer).and_then(|w| v.checked_add(w)))
        .and_then(|v| 3_usize.checked_mul(NSURF).and_then(|w| v.checked_add(w)))
        .ok_or(BridgeError::BadState)?;

    HEAT_STATE_BASE_FLAT_LEN
        .checked_add(alloc_len)
        .ok_or(BridgeError::BadState)
}

fn heat_state_dims_map(nlayer: usize, ndepth: usize) -> PayloadDims {
    let mut dims = PayloadDims::new();

    for field in HEAT_LAYER_DEPTH_FIELDS {
        dims.insert(field.to_string(), vec![nlayer, ndepth]);
    }

    let surf_depth_dims = if nlayer == 0 {
        vec![0, 0]
    } else {
        vec![NSURF, ndepth]
    };
    for field in HEAT_SURF_DEPTH_FIELDS {
        dims.insert(field.to_string(), surf_depth_dims.clone());
    }

    for field in HEAT_LAYER_VEC_FIELDS {
        dims.insert(field.to_string(), vec![nlayer]);
    }

    let surf_vec_dims = if nlayer == 0 { vec![0] } else { vec![NSURF] };
    for field in HEAT_SURF_VEC_FIELDS {
        dims.insert(field.to_string(), surf_vec_dims.clone());
    }

    dims
}

impl HeatState {
    fn validate_layout(&self) -> Result<(), BridgeError> {
        let nlayer = self.nlayer;
        let ndepth = self.ndepth;

        if (nlayer == 0 && ndepth != 0) || (nlayer != 0 && ndepth == 0) {
            return Err(BridgeError::BadState);
        }

        if nlayer == 0 {
            let alloc_fields = [
                &self.temp_roof,
                &self.temp_wall,
                &self.temp_surf,
                &self.temp_surf_dyohm,
                &self.tsfc_roof,
                &self.tsfc_wall,
                &self.tsfc_surf,
                &self.tsfc_surf_dyohm,
                &self.tsfc_roof_stepstart,
                &self.tsfc_wall_stepstart,
                &self.tsfc_surf_stepstart,
                &self.qs_roof,
                &self.qn_roof,
                &self.qe_roof,
                &self.qh_roof,
                &self.qh_resist_roof,
                &self.qs_wall,
                &self.qn_wall,
                &self.qe_wall,
                &self.qh_wall,
                &self.qh_resist_wall,
            ];
            for field in alloc_fields {
                if !field.is_empty() {
                    return Err(BridgeError::BadState);
                }
            }
            return Ok(());
        }

        let layer_depth_len = matrix_len(nlayer, ndepth)?;
        let surf_depth_len = matrix_len(NSURF, ndepth)?;

        let layer_mats = [&self.temp_roof, &self.temp_wall];
        for field in layer_mats {
            if field.len() != layer_depth_len {
                return Err(BridgeError::BadState);
            }
        }

        let surf_mats = [&self.temp_surf, &self.temp_surf_dyohm];
        for field in surf_mats {
            if field.len() != surf_depth_len {
                return Err(BridgeError::BadState);
            }
        }

        let layer_vecs = [
            &self.tsfc_roof,
            &self.tsfc_wall,
            &self.tsfc_roof_stepstart,
            &self.tsfc_wall_stepstart,
            &self.qs_roof,
            &self.qn_roof,
            &self.qe_roof,
            &self.qh_roof,
            &self.qh_resist_roof,
            &self.qs_wall,
            &self.qn_wall,
            &self.qe_wall,
            &self.qh_wall,
            &self.qh_resist_wall,
        ];
        for field in layer_vecs {
            if field.len() != nlayer {
                return Err(BridgeError::BadState);
            }
        }

        let surf_vecs = [
            &self.tsfc_surf,
            &self.tsfc_surf_dyohm,
            &self.tsfc_surf_stepstart,
        ];
        for field in surf_vecs {
            if field.len() != NSURF {
                return Err(BridgeError::BadState);
            }
        }

        Ok(())
    }

    pub fn from_flat_with_dims(
        flat: &[f64],
        nlayer: usize,
        ndepth: usize,
    ) -> Result<Self, BridgeError> {
        let expected_len = heat_state_expected_flat_len(nlayer, ndepth)?;
        validate_flat_len(flat, expected_len)?;

        let layer_depth_len = if nlayer == 0 {
            0
        } else {
            matrix_len(nlayer, ndepth)?
        };
        let surf_depth_len = if nlayer == 0 {
            0
        } else {
            matrix_len(NSURF, ndepth)?
        };
        let layer_vec_len = nlayer;
        let surf_vec_len = if nlayer == 0 { 0 } else { NSURF };

        let mut idx = 0_usize;
        let mut take = |count: usize| {
            let out = flat[idx..idx + count].to_vec();
            idx += count;
            out
        };

        let mut state = Self {
            nlayer,
            ndepth,

            temp_roof: take(layer_depth_len),
            temp_wall: take(layer_depth_len),
            temp_surf: take(surf_depth_len),
            temp_surf_dyohm: take(surf_depth_len),

            tsfc_roof: take(layer_vec_len),
            tsfc_wall: take(layer_vec_len),
            tsfc_surf: take(surf_vec_len),
            tsfc_surf_dyohm: take(surf_vec_len),
            tsfc_roof_stepstart: take(layer_vec_len),
            tsfc_wall_stepstart: take(layer_vec_len),
            tsfc_surf_stepstart: take(surf_vec_len),

            qs_roof: take(layer_vec_len),
            qn_roof: take(layer_vec_len),
            qe_roof: take(layer_vec_len),
            qh_roof: take(layer_vec_len),
            qh_resist_roof: take(layer_vec_len),

            qs_wall: take(layer_vec_len),
            qn_wall: take(layer_vec_len),
            qe_wall: take(layer_vec_len),
            qh_wall: take(layer_vec_len),
            qh_resist_wall: take(layer_vec_len),

            ..Self::default()
        };

        for i in 0..NSURF {
            state.qs_surf[i] = flat[idx];
            idx += 1;
        }
        for i in 0..NSURF {
            state.qn_surf[i] = flat[idx];
            idx += 1;
        }
        for i in 0..NSURF {
            state.qe0_surf[i] = flat[idx];
            idx += 1;
        }
        for i in 0..NSURF {
            state.qe_surf[i] = flat[idx];
            idx += 1;
        }
        for i in 0..NSURF {
            state.qh_surf[i] = flat[idx];
            idx += 1;
        }
        for i in 0..NSURF {
            state.qh_resist_surf[i] = flat[idx];
            idx += 1;
        }
        for i in 0..NSURF {
            state.tsurf_ind[i] = flat[idx];
            idx += 1;
        }

        state.qh_lumps = flat[idx];
        idx += 1;
        state.qe_lumps = flat[idx];
        idx += 1;
        state.kclear = flat[idx];
        idx += 1;
        state.kup = flat[idx];
        idx += 1;
        state.ldown = flat[idx];
        idx += 1;
        state.lup = flat[idx];
        idx += 1;
        state.qe = flat[idx];
        idx += 1;
        state.qf = flat[idx];
        idx += 1;
        state.qf_sahp = flat[idx];
        idx += 1;
        state.qh = flat[idx];
        idx += 1;
        state.qh_residual = flat[idx];
        idx += 1;
        state.qh_resist = flat[idx];
        idx += 1;
        state.qn = flat[idx];
        idx += 1;
        state.qn_snowfree = flat[idx];
        idx += 1;
        state.qs = flat[idx];
        idx += 1;
        state.tsfc_c = flat[idx];
        idx += 1;
        state.tsurf = flat[idx];
        idx += 1;
        state.qh_init = flat[idx];
        idx += 1;

        for i in 0..RADIATION_BIN_COUNT {
            state.roof_in_sw_spc[i] = flat[idx];
            idx += 1;
        }
        for i in 0..RADIATION_BIN_COUNT {
            state.roof_in_lw_spc[i] = flat[idx];
            idx += 1;
        }
        for i in 0..RADIATION_BIN_COUNT {
            state.wall_in_sw_spc[i] = flat[idx];
            idx += 1;
        }
        for i in 0..RADIATION_BIN_COUNT {
            state.wall_in_lw_spc[i] = flat[idx];
            idx += 1;
        }

        state.iter_safe = flat[idx] >= 0.5;
        state.validate_layout()?;
        Ok(state)
    }

    pub fn from_ordered_values(values: &[f64]) -> Result<Self, BridgeError> {
        let (expected_len, nlayer, ndepth) = heat_state_schema()?;
        validate_flat_len(values, expected_len)?;
        Self::from_flat_with_dims(values, nlayer, ndepth)
    }

    pub fn to_flat_checked(&self) -> Result<Vec<f64>, BridgeError> {
        self.validate_layout()?;
        let expected_len = heat_state_expected_flat_len(self.nlayer, self.ndepth)?;
        let mut flat = Vec::with_capacity(expected_len);

        flat.extend_from_slice(&self.temp_roof);
        flat.extend_from_slice(&self.temp_wall);
        flat.extend_from_slice(&self.temp_surf);
        flat.extend_from_slice(&self.temp_surf_dyohm);

        flat.extend_from_slice(&self.tsfc_roof);
        flat.extend_from_slice(&self.tsfc_wall);
        flat.extend_from_slice(&self.tsfc_surf);
        flat.extend_from_slice(&self.tsfc_surf_dyohm);
        flat.extend_from_slice(&self.tsfc_roof_stepstart);
        flat.extend_from_slice(&self.tsfc_wall_stepstart);
        flat.extend_from_slice(&self.tsfc_surf_stepstart);

        flat.extend_from_slice(&self.qs_roof);
        flat.extend_from_slice(&self.qn_roof);
        flat.extend_from_slice(&self.qe_roof);
        flat.extend_from_slice(&self.qh_roof);
        flat.extend_from_slice(&self.qh_resist_roof);

        flat.extend_from_slice(&self.qs_wall);
        flat.extend_from_slice(&self.qn_wall);
        flat.extend_from_slice(&self.qe_wall);
        flat.extend_from_slice(&self.qh_wall);
        flat.extend_from_slice(&self.qh_resist_wall);

        flat.extend_from_slice(&self.qs_surf);
        flat.extend_from_slice(&self.qn_surf);
        flat.extend_from_slice(&self.qe0_surf);
        flat.extend_from_slice(&self.qe_surf);
        flat.extend_from_slice(&self.qh_surf);
        flat.extend_from_slice(&self.qh_resist_surf);
        flat.extend_from_slice(&self.tsurf_ind);

        flat.push(self.qh_lumps);
        flat.push(self.qe_lumps);
        flat.push(self.kclear);
        flat.push(self.kup);
        flat.push(self.ldown);
        flat.push(self.lup);
        flat.push(self.qe);
        flat.push(self.qf);
        flat.push(self.qf_sahp);
        flat.push(self.qh);
        flat.push(self.qh_residual);
        flat.push(self.qh_resist);
        flat.push(self.qn);
        flat.push(self.qn_snowfree);
        flat.push(self.qs);
        flat.push(self.tsfc_c);
        flat.push(self.tsurf);
        flat.push(self.qh_init);

        flat.extend_from_slice(&self.roof_in_sw_spc);
        flat.extend_from_slice(&self.roof_in_lw_spc);
        flat.extend_from_slice(&self.wall_in_sw_spc);
        flat.extend_from_slice(&self.wall_in_lw_spc);

        flat.push(if self.iter_safe { 1.0 } else { 0.0 });

        Ok(flat)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        self.to_flat_checked()
            .expect("HeatState has inconsistent dynamic dimensions")
    }
}

pub fn heat_state_schema() -> Result<(usize, usize, usize), BridgeError> {
    let mut n_flat = -1_i32;
    let mut nlayer = -1_i32;
    let mut ndepth = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_heat_state_len(
            &mut n_flat as *mut i32,
            &mut nlayer as *mut i32,
            &mut ndepth as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || n_flat < 0 || nlayer < 0 || ndepth < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok((n_flat as usize, nlayer as usize, ndepth as usize))
}

pub fn heat_state_schema_info() -> Result<HeatStateSchema, BridgeError> {
    let (flat_len, nlayer, ndepth) = heat_state_schema()?;
    let schema_version_runtime = heat_state_schema_version_runtime()?;
    let expected_len = heat_state_expected_flat_len(nlayer, ndepth)?;
    let field_names = heat_state_field_names_with_dims(nlayer, ndepth);
    let allocatable_dims = heat_state_dims_map(nlayer, ndepth);

    if schema_version_runtime != HEAT_STATE_SCHEMA_VERSION
        || flat_len != expected_len
        || flat_len != field_names.len()
    {
        return Err(BridgeError::BadState);
    }

    Ok(HeatStateSchema {
        schema_version: HEAT_STATE_SCHEMA_VERSION,
        flat_len,
        base_flat_len: HEAT_STATE_BASE_FLAT_LEN,
        nlayer,
        ndepth,
        field_names,
        allocatable_dims,
    })
}

pub fn heat_state_schema_version() -> u32 {
    HEAT_STATE_SCHEMA_VERSION
}

pub fn heat_state_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_heat_state_schema_version(&mut schema_version as *mut i32, &mut err as *mut i32);
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

fn push_vec_field_names(names: &mut Vec<String>, prefix: &str, len: usize) {
    for idx in 0..len {
        names.push(format!("{prefix}_{:02}", idx + 1));
    }
}

fn push_mat_field_names(names: &mut Vec<String>, prefix: &str, rows: usize, cols: usize) {
    for row in 0..rows {
        for col in 0..cols {
            names.push(format!("{prefix}_{:02}_{:02}", row + 1, col + 1));
        }
    }
}

pub fn heat_state_base_field_names() -> Vec<String> {
    let mut names = Vec::with_capacity(HEAT_STATE_BASE_FLAT_LEN);

    push_surface_field_names(&mut names, "qs_surf");
    push_surface_field_names(&mut names, "qn_surf");
    push_surface_field_names(&mut names, "qe0_surf");
    push_surface_field_names(&mut names, "qe_surf");
    push_surface_field_names(&mut names, "qh_surf");
    push_surface_field_names(&mut names, "qh_resist_surf");
    push_surface_field_names(&mut names, "tsurf_ind");

    names.push("qh_lumps".to_string());
    names.push("qe_lumps".to_string());
    names.push("kclear".to_string());
    names.push("kup".to_string());
    names.push("ldown".to_string());
    names.push("lup".to_string());
    names.push("qe".to_string());
    names.push("qf".to_string());
    names.push("qf_sahp".to_string());
    names.push("qh".to_string());
    names.push("qh_residual".to_string());
    names.push("qh_resist".to_string());
    names.push("qn".to_string());
    names.push("qn_snowfree".to_string());
    names.push("qs".to_string());
    names.push("tsfc_c".to_string());
    names.push("tsurf".to_string());
    names.push("qh_init".to_string());

    for idx in 0..RADIATION_BIN_COUNT {
        names.push(format!("roof_in_sw_spc_{:02}", idx + 1));
    }
    for idx in 0..RADIATION_BIN_COUNT {
        names.push(format!("roof_in_lw_spc_{:02}", idx + 1));
    }
    for idx in 0..RADIATION_BIN_COUNT {
        names.push(format!("wall_in_sw_spc_{:02}", idx + 1));
    }
    for idx in 0..RADIATION_BIN_COUNT {
        names.push(format!("wall_in_lw_spc_{:02}", idx + 1));
    }

    names.push("iter_safe".to_string());

    names
}

pub fn heat_state_field_names_with_dims(nlayer: usize, ndepth: usize) -> Vec<String> {
    let mut names = Vec::new();

    if nlayer > 0 {
        push_mat_field_names(&mut names, "temp_roof", nlayer, ndepth);
        push_mat_field_names(&mut names, "temp_wall", nlayer, ndepth);
        push_mat_field_names(&mut names, "temp_surf", NSURF, ndepth);
        push_mat_field_names(&mut names, "temp_surf_dyohm", NSURF, ndepth);

        push_vec_field_names(&mut names, "tsfc_roof", nlayer);
        push_vec_field_names(&mut names, "tsfc_wall", nlayer);
        push_vec_field_names(&mut names, "tsfc_surf", NSURF);
        push_vec_field_names(&mut names, "tsfc_surf_dyohm", NSURF);
        push_vec_field_names(&mut names, "tsfc_roof_stepstart", nlayer);
        push_vec_field_names(&mut names, "tsfc_wall_stepstart", nlayer);
        push_vec_field_names(&mut names, "tsfc_surf_stepstart", NSURF);

        push_vec_field_names(&mut names, "qs_roof", nlayer);
        push_vec_field_names(&mut names, "qn_roof", nlayer);
        push_vec_field_names(&mut names, "qe_roof", nlayer);
        push_vec_field_names(&mut names, "qh_roof", nlayer);
        push_vec_field_names(&mut names, "qh_resist_roof", nlayer);

        push_vec_field_names(&mut names, "qs_wall", nlayer);
        push_vec_field_names(&mut names, "qn_wall", nlayer);
        push_vec_field_names(&mut names, "qe_wall", nlayer);
        push_vec_field_names(&mut names, "qh_wall", nlayer);
        push_vec_field_names(&mut names, "qh_resist_wall", nlayer);
    }

    names.extend(heat_state_base_field_names());
    names
}

pub fn heat_state_field_names() -> Result<Vec<String>, BridgeError> {
    let (_, nlayer, ndepth) = heat_state_schema()?;
    Ok(heat_state_field_names_with_dims(nlayer, ndepth))
}

pub fn heat_state_field_index(name: &str) -> Result<Option<usize>, BridgeError> {
    let names = heat_state_field_names()?;
    Ok(field_index(&names, name))
}

pub fn heat_state_to_map(state: &HeatState) -> BTreeMap<String, f64> {
    let names = heat_state_field_names_with_dims(state.nlayer, state.ndepth);
    let values = state.to_flat();
    names.into_iter().zip(values).collect()
}

pub fn heat_state_to_ordered_values(state: &HeatState) -> Vec<f64> {
    state.to_flat()
}

pub fn heat_state_to_values_payload(state: &HeatState) -> HeatStateValuesPayload {
    HeatStateValuesPayload {
        schema_version: HEAT_STATE_SCHEMA_VERSION,
        values: state.to_flat(),
        dims: heat_state_dims_map(state.nlayer, state.ndepth),
    }
}

pub fn heat_state_from_ordered_values(values: &[f64]) -> Result<HeatState, BridgeError> {
    HeatState::from_ordered_values(values)
}

pub fn heat_state_from_values_payload(
    payload: &HeatStateValuesPayload,
) -> Result<HeatState, BridgeError> {
    if payload.schema_version != HEAT_STATE_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    if payload.dims.len() != HEAT_ALLOC_FIELDS.len() {
        return Err(BridgeError::BadState);
    }

    let mut nlayer: Option<usize> = None;
    let mut ndepth: Option<usize> = None;

    for field in HEAT_LAYER_VEC_FIELDS {
        let dims = require_field_dims(&payload.dims, field, 1)?;
        let len_here = dims_element_count(&dims)?;
        if let Some(expected) = nlayer {
            if len_here != expected {
                return Err(BridgeError::BadState);
            }
        } else {
            nlayer = Some(len_here);
        }
    }

    for field in HEAT_LAYER_DEPTH_FIELDS {
        let dims = require_field_dims(&payload.dims, field, 2)?;
        let layer_here = dims[0];
        let depth_here = dims[1];
        let mat_len = dims_element_count(&dims)?;
        let expected_len = matrix_len(layer_here, depth_here)?;

        if mat_len != expected_len {
            return Err(BridgeError::BadState);
        }

        if let Some(expected) = nlayer {
            if layer_here != expected {
                return Err(BridgeError::BadState);
            }
        } else {
            nlayer = Some(layer_here);
        }

        if let Some(expected) = ndepth {
            if depth_here != expected {
                return Err(BridgeError::BadState);
            }
        } else {
            ndepth = Some(depth_here);
        }
    }

    let nlayer = nlayer.unwrap_or(0);

    for field in HEAT_SURF_DEPTH_FIELDS {
        let dims = require_field_dims(&payload.dims, field, 2)?;
        let surf_here = dims[0];
        let depth_here = dims[1];
        let mat_len = dims_element_count(&dims)?;

        if nlayer == 0 {
            if surf_here != 0 || depth_here != 0 || mat_len != 0 {
                return Err(BridgeError::BadState);
            }
            if let Some(expected) = ndepth {
                if expected != 0 {
                    return Err(BridgeError::BadState);
                }
            } else {
                ndepth = Some(0);
            }
        } else {
            if surf_here != NSURF {
                return Err(BridgeError::BadState);
            }
            let expected_len = matrix_len(NSURF, depth_here)?;
            if mat_len != expected_len {
                return Err(BridgeError::BadState);
            }

            if let Some(expected) = ndepth {
                if depth_here != expected {
                    return Err(BridgeError::BadState);
                }
            } else {
                ndepth = Some(depth_here);
            }
        }
    }

    for field in HEAT_SURF_VEC_FIELDS {
        let dims = require_field_dims(&payload.dims, field, 1)?;
        let len_here = dims_element_count(&dims)?;

        if nlayer == 0 {
            if len_here != 0 {
                return Err(BridgeError::BadState);
            }
        } else if len_here != NSURF {
            return Err(BridgeError::BadState);
        }
    }

    let ndepth = ndepth.unwrap_or(0);
    if (nlayer == 0 && ndepth != 0) || (nlayer != 0 && ndepth == 0) {
        return Err(BridgeError::BadState);
    }

    let expected_len = heat_state_expected_flat_len(nlayer, ndepth)?;
    validate_flat_len(&payload.values, expected_len)?;
    HeatState::from_flat_with_dims(&payload.values, nlayer, ndepth)
}

pub fn heat_state_from_map(values: &BTreeMap<String, f64>) -> Result<HeatState, BridgeError> {
    let default_state = heat_state_default_from_fortran()?;
    let payload = heat_state_to_values_payload(&default_state);
    let names = heat_state_field_names_with_dims(default_state.nlayer, default_state.ndepth);

    let mut flat = payload.values.clone();
    for (name, value) in values {
        let idx = field_index(&names, name).ok_or(BridgeError::BadState)?;
        flat[idx] = *value;
    }

    heat_state_from_values_payload(&HeatStateValuesPayload {
        schema_version: HEAT_STATE_SCHEMA_VERSION,
        values: flat,
        dims: payload.dims,
    })
}

pub fn heat_state_default_from_fortran() -> Result<HeatState, BridgeError> {
    let (n_flat, nlayer, ndepth) = heat_state_schema()?;
    let mut flat = vec![0.0_f64; n_flat];
    let mut nlayer_runtime = -1_i32;
    let mut ndepth_runtime = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_heat_state_default(
            flat.as_mut_ptr(),
            n_flat as i32,
            &mut nlayer_runtime as *mut i32,
            &mut ndepth_runtime as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || nlayer_runtime < 0 || ndepth_runtime < 0 {
        return Err(BridgeError::from_code(err));
    }

    if nlayer_runtime as usize != nlayer || ndepth_runtime as usize != ndepth {
        return Err(BridgeError::BadState);
    }

    HeatState::from_flat_with_dims(&flat, nlayer, ndepth)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn build_allocated_state(nlayer: usize, ndepth: usize) -> HeatState {
        let mut state = HeatState {
            nlayer,
            ndepth,
            ..HeatState::default()
        };

        let mut seed = 1.0_f64;
        let mut next_vec = |len: usize| {
            let mut out = Vec::with_capacity(len);
            for _ in 0..len {
                out.push(seed);
                seed += 1.0;
            }
            out
        };

        let layer_depth_len = nlayer * ndepth;
        let surf_depth_len = NSURF * ndepth;

        state.temp_roof = next_vec(layer_depth_len);
        state.temp_wall = next_vec(layer_depth_len);
        state.temp_surf = next_vec(surf_depth_len);
        state.temp_surf_dyohm = next_vec(surf_depth_len);

        state.tsfc_roof = next_vec(nlayer);
        state.tsfc_wall = next_vec(nlayer);
        state.tsfc_surf = next_vec(NSURF);
        state.tsfc_surf_dyohm = next_vec(NSURF);
        state.tsfc_roof_stepstart = next_vec(nlayer);
        state.tsfc_wall_stepstart = next_vec(nlayer);
        state.tsfc_surf_stepstart = next_vec(NSURF);

        state.qs_roof = next_vec(nlayer);
        state.qn_roof = next_vec(nlayer);
        state.qe_roof = next_vec(nlayer);
        state.qh_roof = next_vec(nlayer);
        state.qh_resist_roof = next_vec(nlayer);

        state.qs_wall = next_vec(nlayer);
        state.qn_wall = next_vec(nlayer);
        state.qe_wall = next_vec(nlayer);
        state.qh_wall = next_vec(nlayer);
        state.qh_resist_wall = next_vec(nlayer);

        state.qh_lumps = 42.0;
        state.iter_safe = false;
        state
    }

    #[test]
    fn schema_matches_expected_dimensions() {
        let (flat_len, nlayer, ndepth) = heat_state_schema().expect("schema call should succeed");
        let expected =
            heat_state_expected_flat_len(nlayer, ndepth).expect("expected len should fit");
        assert_eq!(flat_len, expected);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = heat_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, HeatState::default());

        let state2 = HeatState::from_flat_with_dims(&state.to_flat(), state.nlayer, state.ndepth)
            .expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = heat_state_default_from_fortran().expect("default state should be available");
        let mut mapped = heat_state_to_map(&state);
        mapped.insert("kclear".to_string(), 123.0);
        mapped.insert("qh_lumps".to_string(), -8.5);

        let updated = heat_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.kclear - 123.0).abs() < 1.0e-12);
        assert!((updated.qh_lumps + 8.5).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = build_allocated_state(2, 3);
        let payload = heat_state_to_values_payload(&state);
        let recovered =
            heat_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = HeatStateValuesPayload {
            schema_version: HEAT_STATE_SCHEMA_VERSION + 1,
            values: payload.values.clone(),
            dims: payload.dims.clone(),
        };
        let err = heat_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_missing_dims() {
        let state = heat_state_default_from_fortran().expect("default state should be available");
        let payload = HeatStateValuesPayload {
            schema_version: HEAT_STATE_SCHEMA_VERSION,
            values: state.to_flat(),
            dims: PayloadDims::new(),
        };
        let err = heat_state_from_values_payload(&payload).expect_err("missing dims should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_dims_length_mismatch() {
        let state = build_allocated_state(1, 1);
        let mut payload = heat_state_to_values_payload(&state);
        payload
            .dims
            .insert(HEAT_STATE_TEMP_ROOF_FIELD.to_string(), vec![2, 1]);

        let err =
            heat_state_from_values_payload(&payload).expect_err("dims/values mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_rank_mismatch() {
        let state = build_allocated_state(1, 1);
        let mut payload = heat_state_to_values_payload(&state);
        payload
            .dims
            .insert(HEAT_STATE_TEMP_ROOF_FIELD.to_string(), vec![1]);

        let err = heat_state_from_values_payload(&payload).expect_err("rank mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
