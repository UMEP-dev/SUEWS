use crate::codec::{
    dims_element_count, field_index, require_field_dims, validate_flat_len, PayloadDims,
    ValuesPayloadWithDims,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const EHC_PRM_SCHEMA_VERSION: u32 = 1;

const EHC_VEC_FIELDS: [&str; 9] = [
    "soil_storecap_roof",
    "soil_storecap_wall",
    "state_limit_roof",
    "state_limit_wall",
    "wet_thresh_roof",
    "wet_thresh_wall",
    "tin_roof",
    "tin_wall",
    "tin_surf",
];

const EHC_MAT_FIELDS: [&str; 9] = [
    "k_roof", "k_wall", "k_surf", "cp_roof", "cp_wall", "cp_surf", "dz_roof", "dz_wall", "dz_surf",
];

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

#[derive(Debug, Clone, PartialEq)]
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

impl Default for EhcPrm {
    fn default() -> Self {
        Self {
            nlayer: 0,
            ndepth: 0,
            soil_storecap_roof: Vec::new(),
            soil_storecap_wall: Vec::new(),
            state_limit_roof: Vec::new(),
            state_limit_wall: Vec::new(),
            wet_thresh_roof: Vec::new(),
            wet_thresh_wall: Vec::new(),
            tin_roof: Vec::new(),
            tin_wall: Vec::new(),
            tin_surf: Vec::new(),
            k_roof: Vec::new(),
            k_wall: Vec::new(),
            k_surf: Vec::new(),
            cp_roof: Vec::new(),
            cp_wall: Vec::new(),
            cp_surf: Vec::new(),
            dz_roof: Vec::new(),
            dz_wall: Vec::new(),
            dz_surf: Vec::new(),
        }
    }
}

fn matrix_len(nlayer: usize, ndepth: usize) -> Result<usize, BridgeError> {
    nlayer.checked_mul(ndepth).ok_or(BridgeError::BadState)
}

pub fn ehc_prm_expected_flat_len(nlayer: usize, ndepth: usize) -> Result<usize, BridgeError> {
    let vec_len = EHC_VEC_FIELDS
        .len()
        .checked_mul(nlayer)
        .ok_or(BridgeError::BadState)?;
    let mat_len = EHC_MAT_FIELDS
        .len()
        .checked_mul(matrix_len(nlayer, ndepth)?)
        .ok_or(BridgeError::BadState)?;
    vec_len.checked_add(mat_len).ok_or(BridgeError::BadState)
}

fn ehc_prm_dims_map(nlayer: usize, ndepth: usize) -> PayloadDims {
    let mut dims = PayloadDims::new();
    for field in EHC_VEC_FIELDS {
        dims.insert(field.to_string(), vec![nlayer]);
    }
    for field in EHC_MAT_FIELDS {
        dims.insert(field.to_string(), vec![nlayer, ndepth]);
    }
    dims
}

pub fn ehc_prm_field_names_with_dims(nlayer: usize, ndepth: usize) -> Vec<String> {
    let mut names = Vec::new();

    for field in EHC_VEC_FIELDS {
        for layer in 0..nlayer {
            names.push(format!("{field}_{:02}", layer + 1));
        }
    }

    for field in EHC_MAT_FIELDS {
        for layer in 0..nlayer {
            for depth in 0..ndepth {
                names.push(format!("{field}_{:02}_{:02}", layer + 1, depth + 1));
            }
        }
    }

    names
}

impl EhcPrm {
    fn validate_layout(&self) -> Result<(), BridgeError> {
        let nlayer = self.nlayer;
        let ndepth = self.ndepth;
        let nmat = matrix_len(nlayer, ndepth)?;

        let vec_fields = [
            &self.soil_storecap_roof,
            &self.soil_storecap_wall,
            &self.state_limit_roof,
            &self.state_limit_wall,
            &self.wet_thresh_roof,
            &self.wet_thresh_wall,
            &self.tin_roof,
            &self.tin_wall,
            &self.tin_surf,
        ];
        for field in vec_fields {
            if field.len() != nlayer {
                return Err(BridgeError::BadState);
            }
        }

        let mat_fields = [
            &self.k_roof,
            &self.k_wall,
            &self.k_surf,
            &self.cp_roof,
            &self.cp_wall,
            &self.cp_surf,
            &self.dz_roof,
            &self.dz_wall,
            &self.dz_surf,
        ];
        for field in mat_fields {
            if field.len() != nmat {
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

        let vec_len = nlayer;
        let mat_len = matrix_len(nlayer, ndepth)?;
        let mut idx = 0_usize;
        let mut take = |count: usize| {
            let out = flat[idx..idx + count].to_vec();
            idx += count;
            out
        };

        let state = Self {
            nlayer,
            ndepth,
            soil_storecap_roof: take(vec_len),
            soil_storecap_wall: take(vec_len),
            state_limit_roof: take(vec_len),
            state_limit_wall: take(vec_len),
            wet_thresh_roof: take(vec_len),
            wet_thresh_wall: take(vec_len),
            tin_roof: take(vec_len),
            tin_wall: take(vec_len),
            tin_surf: take(vec_len),
            k_roof: take(mat_len),
            k_wall: take(mat_len),
            k_surf: take(mat_len),
            cp_roof: take(mat_len),
            cp_wall: take(mat_len),
            cp_surf: take(mat_len),
            dz_roof: take(mat_len),
            dz_wall: take(mat_len),
            dz_surf: take(mat_len),
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
    if payload.dims.len() != EHC_VEC_FIELDS.len() + EHC_MAT_FIELDS.len() {
        return Err(BridgeError::BadState);
    }

    let mut nlayer: Option<usize> = None;
    let mut ndepth: Option<usize> = None;

    for field in EHC_VEC_FIELDS {
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

    for field in EHC_MAT_FIELDS {
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

    let nlayer = nlayer.unwrap_or(0);
    let ndepth = ndepth.unwrap_or(0);
    if nlayer == 0 && ndepth != 0 {
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
}
