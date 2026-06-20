use crate::codec::{
    dims_element_count, field_index, require_field_dims, validate_flat_len, PayloadDims,
    ValuesPayloadWithDims,
};
use crate::core::NSURF;
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const EHC_PRM_SCHEMA_VERSION: u32 = 2;

const EHC_LAYER_VEC_FIELDS: [&str; 8] = [
    "soil_storecap_roof",
    "soil_storecap_wall",
    "state_limit_roof",
    "state_limit_wall",
    "wet_thresh_roof",
    "wet_thresh_wall",
    "tin_roof",
    "tin_wall",
];

const EHC_SURF_VEC_FIELDS: [&str; 1] = ["tin_surf"];

const EHC_LAYER_MAT_FIELDS: [&str; 6] = [
    "k_roof", "k_wall", "cp_roof", "cp_wall", "dz_roof", "dz_wall",
];

const EHC_SURF_MAT_FIELDS: [&str; 3] = ["k_surf", "cp_surf", "dz_surf"];

const EHC_FIELD_COUNT: usize = EHC_LAYER_VEC_FIELDS.len()
    + EHC_SURF_VEC_FIELDS.len()
    + EHC_LAYER_MAT_FIELDS.len()
    + EHC_SURF_MAT_FIELDS.len();

const EHC_LAYER_MAT_GROUPS: [[&str; 2]; 3] = [
    ["k_roof", "k_wall"],
    ["cp_roof", "cp_wall"],
    ["dz_roof", "dz_wall"],
];

const EHC_SURF_MAT_BY_GROUP: [&str; 3] = ["k_surf", "cp_surf", "dz_surf"];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EhcPrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub nlayer: usize,
    pub ndepth: usize,
    pub field_names: Vec<String>,
    pub allocatable_dims: PayloadDims,
}

pub type EhcPrmValuesPayload = ValuesPayloadWithDims;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct EhcPrm {
    pub nlayer: usize,
    pub ndepth: usize,
    pub soil_storecap_roof: Vec<f64>,
    pub soil_storecap_wall: Vec<f64>,
    pub state_limit_roof: Vec<f64>,
    pub state_limit_wall: Vec<f64>,
    pub wet_thresh_roof: Vec<f64>,
    pub wet_thresh_wall: Vec<f64>,
    pub tin_roof: Vec<f64>,
    pub tin_wall: Vec<f64>,
    pub tin_surf: Vec<f64>,
    pub k_roof: Vec<f64>,
    pub k_wall: Vec<f64>,
    pub k_surf: Vec<f64>,
    pub cp_roof: Vec<f64>,
    pub cp_wall: Vec<f64>,
    pub cp_surf: Vec<f64>,
    pub dz_roof: Vec<f64>,
    pub dz_wall: Vec<f64>,
    pub dz_surf: Vec<f64>,
}

fn matrix_len(nlayer: usize, ndepth: usize) -> Result<usize, BridgeError> {
    nlayer.checked_mul(ndepth).ok_or(BridgeError::BadState)
}

fn surface_len(nlayer: usize) -> usize {
    if nlayer == 0 {
        0
    } else {
        NSURF
    }
}

pub fn ehc_prm_expected_flat_len(nlayer: usize, ndepth: usize) -> Result<usize, BridgeError> {
    let surf_len = surface_len(nlayer);
    let layer_vec_len = EHC_LAYER_VEC_FIELDS
        .len()
        .checked_mul(nlayer)
        .ok_or(BridgeError::BadState)?;
    let layer_mat_len = EHC_LAYER_MAT_FIELDS
        .len()
        .checked_mul(matrix_len(nlayer, ndepth)?)
        .ok_or(BridgeError::BadState)?;
    let surf_mat_len = EHC_SURF_MAT_FIELDS
        .len()
        .checked_mul(matrix_len(surf_len, ndepth)?)
        .ok_or(BridgeError::BadState)?;
    layer_vec_len
        .checked_add(surf_len)
        .and_then(|v| v.checked_add(layer_mat_len))
        .and_then(|v| v.checked_add(surf_mat_len))
        .ok_or(BridgeError::BadState)
}

fn ehc_prm_dims_map(nlayer: usize, ndepth: usize) -> PayloadDims {
    let mut dims = PayloadDims::new();
    let surf_len = surface_len(nlayer);
    for field in EHC_LAYER_VEC_FIELDS {
        dims.insert(field.to_string(), vec![nlayer]);
    }
    for field in EHC_SURF_VEC_FIELDS {
        dims.insert(field.to_string(), vec![surf_len]);
    }
    for field in EHC_LAYER_MAT_FIELDS {
        dims.insert(field.to_string(), vec![nlayer, ndepth]);
    }
    for field in EHC_SURF_MAT_FIELDS {
        dims.insert(field.to_string(), vec![surf_len, ndepth]);
    }
    dims
}

pub fn ehc_prm_field_names_with_dims(nlayer: usize, ndepth: usize) -> Vec<String> {
    let mut names = Vec::new();
    let surf_len = surface_len(nlayer);

    for field in EHC_LAYER_VEC_FIELDS {
        for layer in 0..nlayer {
            names.push(format!("{field}_{:02}", layer + 1));
        }
    }
    for field in EHC_SURF_VEC_FIELDS {
        for surf in 0..surf_len {
            names.push(format!("{field}_{:02}", surf + 1));
        }
    }

    for (layer_fields, surf_field) in EHC_LAYER_MAT_GROUPS
        .iter()
        .zip(EHC_SURF_MAT_BY_GROUP.iter())
    {
        for field in layer_fields {
            for layer in 0..nlayer {
                for depth in 0..ndepth {
                    names.push(format!("{field}_{:02}_{:02}", layer + 1, depth + 1));
                }
            }
        }
        for surf in 0..surf_len {
            for depth in 0..ndepth {
                names.push(format!("{surf_field}_{:02}_{:02}", surf + 1, depth + 1));
            }
        }
    }

    names
}

impl EhcPrm {
    fn validate_layout(&self) -> Result<(), BridgeError> {
        let nlayer = self.nlayer;
        let ndepth = self.ndepth;
        let nsurf_ehc = surface_len(nlayer);
        let layer_mat_len = matrix_len(nlayer, ndepth)?;
        let surf_mat_len = matrix_len(nsurf_ehc, ndepth)?;

        let layer_vec_fields = [
            &self.soil_storecap_roof,
            &self.soil_storecap_wall,
            &self.state_limit_roof,
            &self.state_limit_wall,
            &self.wet_thresh_roof,
            &self.wet_thresh_wall,
            &self.tin_roof,
            &self.tin_wall,
        ];
        for field in layer_vec_fields {
            if field.len() != nlayer {
                return Err(BridgeError::BadState);
            }
        }
        if self.tin_surf.len() != nsurf_ehc {
            return Err(BridgeError::BadState);
        }

        let layer_mat_fields = [
            &self.k_roof,
            &self.k_wall,
            &self.cp_roof,
            &self.cp_wall,
            &self.dz_roof,
            &self.dz_wall,
        ];
        for field in layer_mat_fields {
            if field.len() != layer_mat_len {
                return Err(BridgeError::BadState);
            }
        }
        for field in [&self.k_surf, &self.cp_surf, &self.dz_surf] {
            if field.len() != surf_mat_len {
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
        let expected_len = ehc_prm_expected_flat_len(nlayer, ndepth)?;
        validate_flat_len(flat, expected_len)?;

        let layer_vec_len = nlayer;
        let surf_vec_len = surface_len(nlayer);
        let layer_mat_len = matrix_len(nlayer, ndepth)?;
        let surf_mat_len = matrix_len(surf_vec_len, ndepth)?;
        let mut idx = 0_usize;
        let mut take = |count: usize| {
            let out = flat[idx..idx + count].to_vec();
            idx += count;
            out
        };

        let state = Self {
            nlayer,
            ndepth,
            soil_storecap_roof: take(layer_vec_len),
            soil_storecap_wall: take(layer_vec_len),
            state_limit_roof: take(layer_vec_len),
            state_limit_wall: take(layer_vec_len),
            wet_thresh_roof: take(layer_vec_len),
            wet_thresh_wall: take(layer_vec_len),
            tin_roof: take(layer_vec_len),
            tin_wall: take(layer_vec_len),
            tin_surf: take(surf_vec_len),
            k_roof: take(layer_mat_len),
            k_wall: take(layer_mat_len),
            k_surf: take(surf_mat_len),
            cp_roof: take(layer_mat_len),
            cp_wall: take(layer_mat_len),
            cp_surf: take(surf_mat_len),
            dz_roof: take(layer_mat_len),
            dz_wall: take(layer_mat_len),
            dz_surf: take(surf_mat_len),
        };

        state.validate_layout()?;
        Ok(state)
    }

    pub fn to_flat_checked(&self) -> Result<Vec<f64>, BridgeError> {
        self.validate_layout()?;
        let expected_len = ehc_prm_expected_flat_len(self.nlayer, self.ndepth)?;
        let mut flat = Vec::with_capacity(expected_len);

        flat.extend_from_slice(&self.soil_storecap_roof);
        flat.extend_from_slice(&self.soil_storecap_wall);
        flat.extend_from_slice(&self.state_limit_roof);
        flat.extend_from_slice(&self.state_limit_wall);
        flat.extend_from_slice(&self.wet_thresh_roof);
        flat.extend_from_slice(&self.wet_thresh_wall);
        flat.extend_from_slice(&self.tin_roof);
        flat.extend_from_slice(&self.tin_wall);
        flat.extend_from_slice(&self.tin_surf);

        flat.extend_from_slice(&self.k_roof);
        flat.extend_from_slice(&self.k_wall);
        flat.extend_from_slice(&self.k_surf);
        flat.extend_from_slice(&self.cp_roof);
        flat.extend_from_slice(&self.cp_wall);
        flat.extend_from_slice(&self.cp_surf);
        flat.extend_from_slice(&self.dz_roof);
        flat.extend_from_slice(&self.dz_wall);
        flat.extend_from_slice(&self.dz_surf);

        Ok(flat)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        self.to_flat_checked()
            .expect("EhcPrm has inconsistent dynamic dimensions")
    }
}

pub fn ehc_prm_schema() -> Result<(usize, usize, usize), BridgeError> {
    let mut n_flat = -1_i32;
    let mut nlayer = -1_i32;
    let mut ndepth = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ehc_prm_len(
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

pub fn ehc_prm_schema_info() -> Result<EhcPrmSchema, BridgeError> {
    let (flat_len, nlayer, ndepth) = ehc_prm_schema()?;
    let schema_version_runtime = ehc_prm_schema_version_runtime()?;
    let field_names = ehc_prm_field_names_with_dims(nlayer, ndepth);
    let allocatable_dims = ehc_prm_dims_map(nlayer, ndepth);

    if schema_version_runtime != EHC_PRM_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(EhcPrmSchema {
        schema_version: EHC_PRM_SCHEMA_VERSION,
        flat_len,
        nlayer,
        ndepth,
        field_names,
        allocatable_dims,
    })
}

pub fn ehc_prm_schema_version() -> u32 {
    EHC_PRM_SCHEMA_VERSION
}

pub fn ehc_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ehc_prm_schema_version(&mut schema_version as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn ehc_prm_field_names() -> Result<Vec<String>, BridgeError> {
    let (_, nlayer, ndepth) = ehc_prm_schema()?;
    Ok(ehc_prm_field_names_with_dims(nlayer, ndepth))
}

pub fn ehc_prm_field_index(name: &str) -> Result<Option<usize>, BridgeError> {
    let names = ehc_prm_field_names()?;
    Ok(field_index(&names, name))
}

pub fn ehc_prm_to_map(state: &EhcPrm) -> BTreeMap<String, f64> {
    let names = ehc_prm_field_names_with_dims(state.nlayer, state.ndepth);
    let values = state.to_flat();
    names.into_iter().zip(values).collect()
}

pub fn ehc_prm_to_ordered_values(state: &EhcPrm) -> Vec<f64> {
    state.to_flat()
}

pub fn ehc_prm_to_values_payload(state: &EhcPrm) -> EhcPrmValuesPayload {
    EhcPrmValuesPayload {
        schema_version: EHC_PRM_SCHEMA_VERSION,
        values: state.to_flat(),
        dims: ehc_prm_dims_map(state.nlayer, state.ndepth),
    }
}

pub fn ehc_prm_from_ordered_values(values: &[f64]) -> Result<EhcPrm, BridgeError> {
    let (_, nlayer, ndepth) = ehc_prm_schema()?;
    EhcPrm::from_flat_with_dims(values, nlayer, ndepth)
}

fn ehc_prm_dims_from_payload(payload: &EhcPrmValuesPayload) -> Result<(usize, usize), BridgeError> {
    if payload.dims.len() != EHC_FIELD_COUNT {
        return Err(BridgeError::BadState);
    }

    let mut nlayer: Option<usize> = None;
    let mut ndepth: Option<usize> = None;
    let mut surf_len: Option<usize> = None;

    for field in EHC_LAYER_VEC_FIELDS {
        let dims = require_field_dims(&payload.dims, field, 1)?;
        let layer_here = dims_element_count(&dims)?;
        if let Some(expected) = nlayer {
            if layer_here != expected {
                return Err(BridgeError::BadState);
            }
        } else {
            nlayer = Some(layer_here);
        }
    }

    for field in EHC_SURF_VEC_FIELDS {
        let dims = require_field_dims(&payload.dims, field, 1)?;
        let surf_here = dims_element_count(&dims)?;
        if let Some(expected) = surf_len {
            if surf_here != expected {
                return Err(BridgeError::BadState);
            }
        } else {
            surf_len = Some(surf_here);
        }
    }

    for field in EHC_LAYER_MAT_FIELDS {
        let dims = require_field_dims(&payload.dims, field, 2)?;
        let layer_here = dims[0];
        let depth_here = dims[1];
        let mat_len = dims_element_count(&dims)?;

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

        let expected_mat_len = matrix_len(layer_here, depth_here)?;
        if mat_len != expected_mat_len {
            return Err(BridgeError::BadState);
        }
    }

    for field in EHC_SURF_MAT_FIELDS {
        let dims = require_field_dims(&payload.dims, field, 2)?;
        let surf_here = dims[0];
        let depth_here = dims[1];
        let mat_len = dims_element_count(&dims)?;

        if let Some(expected) = surf_len {
            if surf_here != expected {
                return Err(BridgeError::BadState);
            }
        } else {
            surf_len = Some(surf_here);
        }

        if let Some(expected) = ndepth {
            if depth_here != expected {
                return Err(BridgeError::BadState);
            }
        } else {
            ndepth = Some(depth_here);
        }

        let expected_mat_len = matrix_len(surf_here, depth_here)?;
        if mat_len != expected_mat_len {
            return Err(BridgeError::BadState);
        }
    }

    let nlayer = nlayer.unwrap_or(0);
    let ndepth = ndepth.unwrap_or(0);
    let surf_len = surf_len.unwrap_or(0);
    if nlayer == 0 && ndepth != 0 {
        return Err(BridgeError::BadState);
    }
    if surf_len != surface_len(nlayer) {
        return Err(BridgeError::BadState);
    }
    Ok((nlayer, ndepth))
}

pub fn ehc_prm_from_values_payload(payload: &EhcPrmValuesPayload) -> Result<EhcPrm, BridgeError> {
    if payload.schema_version != EHC_PRM_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    let (nlayer, ndepth) = ehc_prm_dims_from_payload(payload)?;
    let expected_len = ehc_prm_expected_flat_len(nlayer, ndepth)?;
    validate_flat_len(&payload.values, expected_len)?;
    EhcPrm::from_flat_with_dims(&payload.values, nlayer, ndepth)
}

pub fn ehc_prm_from_map(values: &BTreeMap<String, f64>) -> Result<EhcPrm, BridgeError> {
    let default_state = ehc_prm_default_from_fortran()?;
    let payload = ehc_prm_to_values_payload(&default_state);
    let names = ehc_prm_field_names_with_dims(default_state.nlayer, default_state.ndepth);

    let mut flat = payload.values.clone();
    for (name, value) in values {
        let idx = field_index(&names, name).ok_or(BridgeError::BadState)?;
        flat[idx] = *value;
    }

    ehc_prm_from_values_payload(&EhcPrmValuesPayload {
        schema_version: EHC_PRM_SCHEMA_VERSION,
        values: flat,
        dims: payload.dims,
    })
}

pub fn ehc_prm_default_from_fortran() -> Result<EhcPrm, BridgeError> {
    let (n_flat, nlayer, ndepth) = ehc_prm_schema()?;
    let mut flat = vec![0.0_f64; n_flat];
    let mut nlayer_runtime = -1_i32;
    let mut ndepth_runtime = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ehc_prm_default(
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

    EhcPrm::from_flat_with_dims(&flat, nlayer, ndepth)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let (n_flat, nlayer, ndepth) = ehc_prm_schema().expect("schema call should succeed");
        let expected = ehc_prm_expected_flat_len(nlayer, ndepth).expect("expected len should fit");
        assert_eq!(n_flat, expected);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = ehc_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, EhcPrm::default());

        let state2 = EhcPrm::from_flat_with_dims(&state.to_flat(), state.nlayer, state.ndepth)
            .expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = ehc_prm_default_from_fortran().expect("default state should be available");
        let mapped = ehc_prm_to_map(&state);
        let updated = ehc_prm_from_map(&mapped).expect("map to state should succeed");
        assert_eq!(updated, state);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = ehc_prm_default_from_fortran().expect("default state should be available");
        let payload = ehc_prm_to_values_payload(&state);
        let recovered = ehc_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = EhcPrmValuesPayload {
            schema_version: EHC_PRM_SCHEMA_VERSION + 1,
            values: payload.values.clone(),
            dims: payload.dims.clone(),
        };
        let err = ehc_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_missing_dims() {
        let state = ehc_prm_default_from_fortran().expect("default state should be available");
        let payload = EhcPrmValuesPayload {
            schema_version: EHC_PRM_SCHEMA_VERSION,
            values: state.to_flat(),
            dims: PayloadDims::new(),
        };
        let err = ehc_prm_from_values_payload(&payload).expect_err("missing dims should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn mixed_layer_surface_layout_uses_nlayer_and_nsurf() {
        let nlayer = 3;
        let ndepth = 2;
        let expected = 8 * nlayer + NSURF + 6 * nlayer * ndepth + 3 * NSURF * ndepth;
        assert_eq!(
            ehc_prm_expected_flat_len(nlayer, ndepth).expect("layout length should fit"),
            expected
        );

        let flat: Vec<f64> = (0..expected).map(|i| i as f64).collect();
        let state = EhcPrm::from_flat_with_dims(&flat, nlayer, ndepth)
            .expect("mixed EHC layout should decode");

        assert_eq!(state.nlayer, nlayer);
        assert_eq!(state.ndepth, ndepth);
        assert_eq!(state.tin_roof.len(), nlayer);
        assert_eq!(state.tin_wall.len(), nlayer);
        assert_eq!(state.tin_surf.len(), NSURF);
        assert_eq!(state.cp_roof.len(), nlayer * ndepth);
        assert_eq!(state.cp_wall.len(), nlayer * ndepth);
        assert_eq!(state.cp_surf.len(), NSURF * ndepth);

        let names = ehc_prm_field_names_with_dims(nlayer, ndepth);
        assert_eq!(names.len(), expected);
        assert!(names.contains(&"cp_roof_03_02".to_string()));
        assert!(names.contains(&"cp_wall_03_02".to_string()));
        assert!(names.contains(&"cp_surf_07_02".to_string()));
        assert!(!names.contains(&"cp_roof_07_01".to_string()));
    }

    #[test]
    fn values_payload_rejects_surface_arrays_sized_like_nlayer() {
        let nlayer = 3;
        let ndepth = 2;
        let bad_surface_len = nlayer;
        let mut dims = ehc_prm_dims_map(nlayer, ndepth);
        dims.insert("tin_surf".to_string(), vec![bad_surface_len]);
        dims.insert("k_surf".to_string(), vec![bad_surface_len, ndepth]);
        dims.insert("cp_surf".to_string(), vec![bad_surface_len, ndepth]);
        dims.insert("dz_surf".to_string(), vec![bad_surface_len, ndepth]);

        let bad_len =
            8 * nlayer + bad_surface_len + 6 * nlayer * ndepth + 3 * bad_surface_len * ndepth;
        let payload = EhcPrmValuesPayload {
            schema_version: EHC_PRM_SCHEMA_VERSION,
            values: vec![0.0; bad_len],
            dims,
        };

        let err = ehc_prm_from_values_payload(&payload)
            .expect_err("surface-class arrays must not be sized like vertical layers");
        assert_eq!(err, BridgeError::BadState);
    }
}
