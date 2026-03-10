use crate::codec::{
    dims_element_count, field_index, require_field_dims, validate_flat_len, PayloadDims,
    ValuesPayloadWithDims,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const SPARTACUS_LAYER_PRM_SCHEMA_VERSION: u32 = 1;

const SPARTACUS_LAYER_VEC_FIELDS: [&str; 8] = [
    "building_frac",
    "building_scale",
    "veg_frac",
    "veg_scale",
    "alb_roof",
    "emis_roof",
    "alb_wall",
    "emis_wall",
];

const SPARTACUS_LAYER_MAT_FIELDS: [&str; 2] = ["roof_albedo_dir_mult_fact", "wall_specular_frac"];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpartacusLayerPrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub nlayer: usize,
    pub nspec: usize,
    pub field_names: Vec<String>,
    pub allocatable_dims: PayloadDims,
}

pub type SpartacusLayerPrmValuesPayload = ValuesPayloadWithDims;

#[derive(Debug, Clone, PartialEq)]
pub struct SpartacusLayerPrm {
    pub nlayer: usize,
    pub nspec: usize,
    pub building_frac: Vec<f64>,
    pub building_scale: Vec<f64>,
    pub veg_frac: Vec<f64>,
    pub veg_scale: Vec<f64>,
    pub alb_roof: Vec<f64>,
    pub emis_roof: Vec<f64>,
    pub alb_wall: Vec<f64>,
    pub emis_wall: Vec<f64>,
    pub roof_albedo_dir_mult_fact: Vec<f64>,
    pub wall_specular_frac: Vec<f64>,
}

impl Default for SpartacusLayerPrm {
    fn default() -> Self {
        Self {
            nlayer: 0,
            nspec: 0,
            building_frac: Vec::new(),
            building_scale: Vec::new(),
            veg_frac: Vec::new(),
            veg_scale: Vec::new(),
            alb_roof: Vec::new(),
            emis_roof: Vec::new(),
            alb_wall: Vec::new(),
            emis_wall: Vec::new(),
            roof_albedo_dir_mult_fact: Vec::new(),
            wall_specular_frac: Vec::new(),
        }
    }
}

fn matrix_len(nspec: usize, nlayer: usize) -> Result<usize, BridgeError> {
    nspec.checked_mul(nlayer).ok_or(BridgeError::BadState)
}

pub fn spartacus_layer_prm_expected_flat_len(
    nlayer: usize,
    nspec: usize,
) -> Result<usize, BridgeError> {
    let vec_len = SPARTACUS_LAYER_VEC_FIELDS
        .len()
        .checked_mul(nlayer)
        .ok_or(BridgeError::BadState)?;
    let mat_len = SPARTACUS_LAYER_MAT_FIELDS
        .len()
        .checked_mul(matrix_len(nspec, nlayer)?)
        .ok_or(BridgeError::BadState)?;
    vec_len.checked_add(mat_len).ok_or(BridgeError::BadState)
}

fn spartacus_layer_prm_dims_map(nlayer: usize, nspec: usize) -> PayloadDims {
    let mut dims = PayloadDims::new();

    for field in SPARTACUS_LAYER_VEC_FIELDS {
        dims.insert(field.to_string(), vec![nlayer]);
    }

    for field in SPARTACUS_LAYER_MAT_FIELDS {
        dims.insert(field.to_string(), vec![nspec, nlayer]);
    }

    dims
}

pub fn spartacus_layer_prm_field_names_with_dims(nlayer: usize, nspec: usize) -> Vec<String> {
    let mut names = Vec::new();

    for field in SPARTACUS_LAYER_VEC_FIELDS {
        for layer in 0..nlayer {
            names.push(format!("{field}_{:02}", layer + 1));
        }
    }

    for field in SPARTACUS_LAYER_MAT_FIELDS {
        for spec in 0..nspec {
            for layer in 0..nlayer {
                names.push(format!("{field}_{:02}_{:02}", spec + 1, layer + 1));
            }
        }
    }

    names
}

impl SpartacusLayerPrm {
    fn validate_layout(&self) -> Result<(), BridgeError> {
        let nlayer = self.nlayer;
        let nspec = self.nspec;
        let nmat = matrix_len(nspec, nlayer)?;

        let vec_fields = [
            &self.building_frac,
            &self.building_scale,
            &self.veg_frac,
            &self.veg_scale,
            &self.alb_roof,
            &self.emis_roof,
            &self.alb_wall,
            &self.emis_wall,
        ];

        for field in vec_fields {
            if field.len() != nlayer {
                return Err(BridgeError::BadState);
            }
        }

        let mat_fields = [&self.roof_albedo_dir_mult_fact, &self.wall_specular_frac];
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
        nspec: usize,
    ) -> Result<Self, BridgeError> {
        let expected = spartacus_layer_prm_expected_flat_len(nlayer, nspec)?;
        validate_flat_len(flat, expected)?;

        let vec_len = nlayer;
        let mat_len = matrix_len(nspec, nlayer)?;
        let mut idx = 0_usize;
        let mut take = |count: usize| {
            let out = flat[idx..idx + count].to_vec();
            idx += count;
            out
        };

        let state = Self {
            nlayer,
            nspec,
            building_frac: take(vec_len),
            building_scale: take(vec_len),
            veg_frac: take(vec_len),
            veg_scale: take(vec_len),
            alb_roof: take(vec_len),
            emis_roof: take(vec_len),
            alb_wall: take(vec_len),
            emis_wall: take(vec_len),
            roof_albedo_dir_mult_fact: take(mat_len),
            wall_specular_frac: take(mat_len),
        };

        state.validate_layout()?;
        Ok(state)
    }

    pub fn to_flat_checked(&self) -> Result<Vec<f64>, BridgeError> {
        self.validate_layout()?;
        let expected = spartacus_layer_prm_expected_flat_len(self.nlayer, self.nspec)?;
        let mut flat = Vec::with_capacity(expected);

        flat.extend_from_slice(&self.building_frac);
        flat.extend_from_slice(&self.building_scale);
        flat.extend_from_slice(&self.veg_frac);
        flat.extend_from_slice(&self.veg_scale);
        flat.extend_from_slice(&self.alb_roof);
        flat.extend_from_slice(&self.emis_roof);
        flat.extend_from_slice(&self.alb_wall);
        flat.extend_from_slice(&self.emis_wall);
        flat.extend_from_slice(&self.roof_albedo_dir_mult_fact);
        flat.extend_from_slice(&self.wall_specular_frac);

        Ok(flat)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        self.to_flat_checked()
            .expect("SpartacusLayerPrm has inconsistent dynamic dimensions")
    }
}

pub fn spartacus_layer_prm_schema() -> Result<(usize, usize, usize), BridgeError> {
    let mut n_flat = -1_i32;
    let mut nlayer = -1_i32;
    let mut nspec = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_spartacus_layer_prm_len(
            &mut n_flat as *mut i32,
            &mut nlayer as *mut i32,
            &mut nspec as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || n_flat < 0 || nlayer < 0 || nspec < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok((n_flat as usize, nlayer as usize, nspec as usize))
}

pub fn spartacus_layer_prm_schema_info() -> Result<SpartacusLayerPrmSchema, BridgeError> {
    let (flat_len, nlayer, nspec) = spartacus_layer_prm_schema()?;
    let schema_version_runtime = spartacus_layer_prm_schema_version_runtime()?;
    let field_names = spartacus_layer_prm_field_names_with_dims(nlayer, nspec);
    let allocatable_dims = spartacus_layer_prm_dims_map(nlayer, nspec);

    if schema_version_runtime != SPARTACUS_LAYER_PRM_SCHEMA_VERSION || flat_len != field_names.len()
    {
        return Err(BridgeError::BadState);
    }

    Ok(SpartacusLayerPrmSchema {
        schema_version: SPARTACUS_LAYER_PRM_SCHEMA_VERSION,
        flat_len,
        nlayer,
        nspec,
        field_names,
        allocatable_dims,
    })
}

pub fn spartacus_layer_prm_schema_version() -> u32 {
    SPARTACUS_LAYER_PRM_SCHEMA_VERSION
}

pub fn spartacus_layer_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_spartacus_layer_prm_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn spartacus_layer_prm_field_names() -> Result<Vec<String>, BridgeError> {
    let (_, nlayer, nspec) = spartacus_layer_prm_schema()?;
    Ok(spartacus_layer_prm_field_names_with_dims(nlayer, nspec))
}

pub fn spartacus_layer_prm_field_index(name: &str) -> Result<Option<usize>, BridgeError> {
    let names = spartacus_layer_prm_field_names()?;
    Ok(field_index(&names, name))
}

pub fn spartacus_layer_prm_to_map(state: &SpartacusLayerPrm) -> BTreeMap<String, f64> {
    let names = spartacus_layer_prm_field_names_with_dims(state.nlayer, state.nspec);
    let values = state.to_flat();
    names.into_iter().zip(values).collect()
}

pub fn spartacus_layer_prm_to_ordered_values(state: &SpartacusLayerPrm) -> Vec<f64> {
    state.to_flat()
}

pub fn spartacus_layer_prm_to_values_payload(
    state: &SpartacusLayerPrm,
) -> SpartacusLayerPrmValuesPayload {
    SpartacusLayerPrmValuesPayload {
        schema_version: SPARTACUS_LAYER_PRM_SCHEMA_VERSION,
        values: state.to_flat(),
        dims: spartacus_layer_prm_dims_map(state.nlayer, state.nspec),
    }
}

pub fn spartacus_layer_prm_from_ordered_values(
    values: &[f64],
) -> Result<SpartacusLayerPrm, BridgeError> {
    let (_, nlayer, nspec) = spartacus_layer_prm_schema()?;
    SpartacusLayerPrm::from_flat_with_dims(values, nlayer, nspec)
}

fn spartacus_layer_prm_dims_from_payload(
    payload: &SpartacusLayerPrmValuesPayload,
) -> Result<(usize, usize), BridgeError> {
    if payload.dims.len() != SPARTACUS_LAYER_VEC_FIELDS.len() + SPARTACUS_LAYER_MAT_FIELDS.len() {
        return Err(BridgeError::BadState);
    }

    let mut nlayer: Option<usize> = None;
    let mut nspec: Option<usize> = None;

    for field in SPARTACUS_LAYER_VEC_FIELDS {
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

    for field in SPARTACUS_LAYER_MAT_FIELDS {
        let dims = require_field_dims(&payload.dims, field, 2)?;
        let spec_here = dims[0];
        let layer_here = dims[1];
        let mat_len = dims_element_count(&dims)?;

        if let Some(expected) = nspec {
            if spec_here != expected {
                return Err(BridgeError::BadState);
            }
        } else {
            nspec = Some(spec_here);
        }

        if let Some(expected) = nlayer {
            if layer_here != expected {
                return Err(BridgeError::BadState);
            }
        } else {
            nlayer = Some(layer_here);
        }

        let expected_mat_len = matrix_len(spec_here, layer_here)?;
        if mat_len != expected_mat_len {
            return Err(BridgeError::BadState);
        }
    }

    let nlayer = nlayer.unwrap_or(0);
    let nspec = nspec.unwrap_or(0);
    if nlayer == 0 && nspec != 0 {
        return Err(BridgeError::BadState);
    }

    Ok((nlayer, nspec))
}

pub fn spartacus_layer_prm_from_values_payload(
    payload: &SpartacusLayerPrmValuesPayload,
) -> Result<SpartacusLayerPrm, BridgeError> {
    if payload.schema_version != SPARTACUS_LAYER_PRM_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    let (nlayer, nspec) = spartacus_layer_prm_dims_from_payload(payload)?;
    let expected = spartacus_layer_prm_expected_flat_len(nlayer, nspec)?;
    validate_flat_len(&payload.values, expected)?;

    SpartacusLayerPrm::from_flat_with_dims(&payload.values, nlayer, nspec)
}

pub fn spartacus_layer_prm_from_map(
    values: &BTreeMap<String, f64>,
) -> Result<SpartacusLayerPrm, BridgeError> {
    let default_state = spartacus_layer_prm_default_from_fortran()?;
    let payload = spartacus_layer_prm_to_values_payload(&default_state);
    let names =
        spartacus_layer_prm_field_names_with_dims(default_state.nlayer, default_state.nspec);

    let mut flat = payload.values.clone();
    for (name, value) in values {
        let idx = field_index(&names, name).ok_or(BridgeError::BadState)?;
        flat[idx] = *value;
    }

    spartacus_layer_prm_from_values_payload(&SpartacusLayerPrmValuesPayload {
        schema_version: SPARTACUS_LAYER_PRM_SCHEMA_VERSION,
        values: flat,
        dims: payload.dims,
    })
}

pub fn spartacus_layer_prm_default_from_fortran() -> Result<SpartacusLayerPrm, BridgeError> {
    let (n_flat, nlayer, nspec) = spartacus_layer_prm_schema()?;
    let mut flat = vec![0.0_f64; n_flat];
    let mut nlayer_runtime = -1_i32;
    let mut nspec_runtime = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_spartacus_layer_prm_default(
            flat.as_mut_ptr(),
            n_flat as i32,
            &mut nlayer_runtime as *mut i32,
            &mut nspec_runtime as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || nlayer_runtime < 0 || nspec_runtime < 0 {
        return Err(BridgeError::from_code(err));
    }

    if nlayer_runtime as usize != nlayer || nspec_runtime as usize != nspec {
        return Err(BridgeError::BadState);
    }

    SpartacusLayerPrm::from_flat_with_dims(&flat, nlayer, nspec)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let (n_flat, nlayer, nspec) =
            spartacus_layer_prm_schema().expect("schema call should succeed");
        let expected =
            spartacus_layer_prm_expected_flat_len(nlayer, nspec).expect("expected len should fit");
        assert_eq!(n_flat, expected);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            spartacus_layer_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, SpartacusLayerPrm::default());

        let state2 =
            SpartacusLayerPrm::from_flat_with_dims(&state.to_flat(), state.nlayer, state.nspec)
                .expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            spartacus_layer_prm_default_from_fortran().expect("default state should be available");
        let mapped = spartacus_layer_prm_to_map(&state);
        let updated = spartacus_layer_prm_from_map(&mapped).expect("map to state should succeed");
        assert_eq!(updated, state);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            spartacus_layer_prm_default_from_fortran().expect("default state should be available");
        let payload = spartacus_layer_prm_to_values_payload(&state);
        let recovered =
            spartacus_layer_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = SpartacusLayerPrmValuesPayload {
            schema_version: SPARTACUS_LAYER_PRM_SCHEMA_VERSION + 1,
            values: payload.values.clone(),
            dims: payload.dims.clone(),
        };
        let err = spartacus_layer_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_missing_dims() {
        let state =
            spartacus_layer_prm_default_from_fortran().expect("default state should be available");
        let payload = SpartacusLayerPrmValuesPayload {
            schema_version: SPARTACUS_LAYER_PRM_SCHEMA_VERSION,
            values: state.to_flat(),
            dims: PayloadDims::new(),
        };
        let err = spartacus_layer_prm_from_values_payload(&payload)
            .expect_err("missing dims should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
