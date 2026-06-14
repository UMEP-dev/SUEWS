use crate::codec::{
    dims_element_count, field_index, require_field_dims, validate_flat_len, PayloadDims,
    ValuesPayloadWithDims,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const SPARTACUS_PRM_BASE_FLAT_LEN: usize = 14;
pub const SPARTACUS_PRM_SCHEMA_VERSION: u32 = 1;
pub const SPARTACUS_PRM_HEIGHT_FIELD: &str = "height";

const SPARTACUS_PRM_HEAD_FIELDS: [&str; 4] =
    ["air_ext_lw", "air_ext_sw", "air_ssa_lw", "air_ssa_sw"];
const SPARTACUS_PRM_TAIL_FIELDS: [&str; 10] = [
    "ground_albedo_dir_mult_fact",
    "n_stream_lw_urban",
    "n_stream_sw_urban",
    "n_vegetation_region_urban",
    "sw_dn_direct_frac",
    "use_sw_direct_albedo",
    "veg_contact_fraction_const",
    "veg_fsd_const",
    "veg_ssa_lw",
    "veg_ssa_sw",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpartacusPrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub base_flat_len: usize,
    pub height_len: usize,
    pub nlayer: usize,
    pub field_names: Vec<String>,
    pub allocatable_dims: PayloadDims,
}

pub type SpartacusPrmValuesPayload = ValuesPayloadWithDims;

#[derive(Debug, Clone, PartialEq)]
pub struct SpartacusPrm {
    pub air_ext_lw: f64,
    pub air_ext_sw: f64,
    pub air_ssa_lw: f64,
    pub air_ssa_sw: f64,
    pub height: Vec<f64>,
    pub ground_albedo_dir_mult_fact: f64,
    pub n_stream_lw_urban: i32,
    pub n_stream_sw_urban: i32,
    pub n_vegetation_region_urban: i32,
    pub sw_dn_direct_frac: f64,
    pub use_sw_direct_albedo: f64,
    pub veg_contact_fraction_const: f64,
    pub veg_fsd_const: f64,
    pub veg_ssa_lw: f64,
    pub veg_ssa_sw: f64,
}

impl Default for SpartacusPrm {
    fn default() -> Self {
        Self {
            air_ext_lw: 0.0,
            air_ext_sw: 0.0,
            air_ssa_lw: 0.0,
            air_ssa_sw: 0.0,
            height: Vec::new(),
            ground_albedo_dir_mult_fact: 0.0,
            n_stream_lw_urban: 0,
            n_stream_sw_urban: 0,
            n_vegetation_region_urban: 0,
            sw_dn_direct_frac: 0.0,
            use_sw_direct_albedo: 0.0,
            veg_contact_fraction_const: 0.0,
            veg_fsd_const: 0.0,
            veg_ssa_lw: 0.0,
            veg_ssa_sw: 0.0,
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

pub fn spartacus_prm_expected_flat_len(height_len: usize) -> Result<usize, BridgeError> {
    SPARTACUS_PRM_BASE_FLAT_LEN
        .checked_add(height_len)
        .ok_or(BridgeError::BadState)
}

fn spartacus_prm_dims_map(height_len: usize) -> PayloadDims {
    let mut dims = PayloadDims::new();
    dims.insert(SPARTACUS_PRM_HEIGHT_FIELD.to_string(), vec![height_len]);
    dims
}

pub fn spartacus_prm_nlayer_from_height_len(height_len: usize) -> usize {
    if height_len == 0 {
        0
    } else {
        height_len - 1
    }
}

pub fn spartacus_prm_field_names_with_height_len(height_len: usize) -> Vec<String> {
    let mut names = Vec::with_capacity(SPARTACUS_PRM_BASE_FLAT_LEN + height_len);

    for field in SPARTACUS_PRM_HEAD_FIELDS {
        names.push(field.to_string());
    }

    for idx in 0..height_len {
        names.push(format!("height_{:02}", idx + 1));
    }

    for field in SPARTACUS_PRM_TAIL_FIELDS {
        names.push(field.to_string());
    }

    names
}

impl SpartacusPrm {
    pub fn from_flat_with_height_len(flat: &[f64], height_len: usize) -> Result<Self, BridgeError> {
        let expected = spartacus_prm_expected_flat_len(height_len)?;
        validate_flat_len(flat, expected)?;

        let mut idx = 0_usize;
        let mut next = || {
            let v = flat[idx];
            idx += 1;
            v
        };

        let air_ext_lw = next();
        let air_ext_sw = next();
        let air_ssa_lw = next();
        let air_ssa_sw = next();

        let mut height = Vec::with_capacity(height_len);
        for _ in 0..height_len {
            height.push(next());
        }

        Ok(Self {
            air_ext_lw,
            air_ext_sw,
            air_ssa_lw,
            air_ssa_sw,
            height,
            ground_albedo_dir_mult_fact: next(),
            n_stream_lw_urban: decode_int(next())?,
            n_stream_sw_urban: decode_int(next())?,
            n_vegetation_region_urban: decode_int(next())?,
            sw_dn_direct_frac: next(),
            use_sw_direct_albedo: next(),
            veg_contact_fraction_const: next(),
            veg_fsd_const: next(),
            veg_ssa_lw: next(),
            veg_ssa_sw: next(),
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(SPARTACUS_PRM_BASE_FLAT_LEN + self.height.len());

        flat.push(self.air_ext_lw);
        flat.push(self.air_ext_sw);
        flat.push(self.air_ssa_lw);
        flat.push(self.air_ssa_sw);
        flat.extend_from_slice(&self.height);
        flat.push(self.ground_albedo_dir_mult_fact);
        flat.push(self.n_stream_lw_urban as f64);
        flat.push(self.n_stream_sw_urban as f64);
        flat.push(self.n_vegetation_region_urban as f64);
        flat.push(self.sw_dn_direct_frac);
        flat.push(self.use_sw_direct_albedo);
        flat.push(self.veg_contact_fraction_const);
        flat.push(self.veg_fsd_const);
        flat.push(self.veg_ssa_lw);
        flat.push(self.veg_ssa_sw);

        flat
    }
}

pub fn spartacus_prm_schema() -> Result<(usize, usize, usize), BridgeError> {
    let mut n_flat = -1_i32;
    let mut height_len = -1_i32;
    let mut nlayer = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_spartacus_prm_len(
            &mut n_flat as *mut i32,
            &mut height_len as *mut i32,
            &mut nlayer as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || n_flat < 0 || height_len < 0 || nlayer < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok((n_flat as usize, height_len as usize, nlayer as usize))
}

pub fn spartacus_prm_schema_info() -> Result<SpartacusPrmSchema, BridgeError> {
    let (flat_len, height_len, nlayer) = spartacus_prm_schema()?;
    let schema_version_runtime = spartacus_prm_schema_version_runtime()?;
    let field_names = spartacus_prm_field_names_with_height_len(height_len);
    let allocatable_dims = spartacus_prm_dims_map(height_len);

    if schema_version_runtime != SPARTACUS_PRM_SCHEMA_VERSION
        || nlayer != spartacus_prm_nlayer_from_height_len(height_len)
        || flat_len != field_names.len()
    {
        return Err(BridgeError::BadState);
    }

    Ok(SpartacusPrmSchema {
        schema_version: SPARTACUS_PRM_SCHEMA_VERSION,
        flat_len,
        base_flat_len: SPARTACUS_PRM_BASE_FLAT_LEN,
        height_len,
        nlayer,
        field_names,
        allocatable_dims,
    })
}

pub fn spartacus_prm_schema_version() -> u32 {
    SPARTACUS_PRM_SCHEMA_VERSION
}

pub fn spartacus_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_spartacus_prm_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn spartacus_prm_field_names() -> Result<Vec<String>, BridgeError> {
    let (_, height_len, _) = spartacus_prm_schema()?;
    Ok(spartacus_prm_field_names_with_height_len(height_len))
}

pub fn spartacus_prm_field_index(name: &str) -> Result<Option<usize>, BridgeError> {
    let names = spartacus_prm_field_names()?;
    Ok(field_index(&names, name))
}

pub fn spartacus_prm_to_map(state: &SpartacusPrm) -> BTreeMap<String, f64> {
    let names = spartacus_prm_field_names_with_height_len(state.height.len());
    let values = state.to_flat();
    names.into_iter().zip(values).collect()
}

pub fn spartacus_prm_to_ordered_values(state: &SpartacusPrm) -> Vec<f64> {
    state.to_flat()
}

pub fn spartacus_prm_to_values_payload(state: &SpartacusPrm) -> SpartacusPrmValuesPayload {
    SpartacusPrmValuesPayload {
        schema_version: SPARTACUS_PRM_SCHEMA_VERSION,
        values: state.to_flat(),
        dims: spartacus_prm_dims_map(state.height.len()),
    }
}

pub fn spartacus_prm_from_ordered_values(values: &[f64]) -> Result<SpartacusPrm, BridgeError> {
    let (_, height_len, _) = spartacus_prm_schema()?;
    SpartacusPrm::from_flat_with_height_len(values, height_len)
}

pub fn spartacus_prm_from_values_payload(
    payload: &SpartacusPrmValuesPayload,
) -> Result<SpartacusPrm, BridgeError> {
    if payload.schema_version != SPARTACUS_PRM_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    if payload.dims.len() != 1 {
        return Err(BridgeError::BadState);
    }

    let height_dims = require_field_dims(&payload.dims, SPARTACUS_PRM_HEIGHT_FIELD, 1)?;
    let height_len = dims_element_count(&height_dims)?;
    let expected = spartacus_prm_expected_flat_len(height_len)?;
    validate_flat_len(&payload.values, expected)?;

    SpartacusPrm::from_flat_with_height_len(&payload.values, height_len)
}

pub fn spartacus_prm_from_map(values: &BTreeMap<String, f64>) -> Result<SpartacusPrm, BridgeError> {
    let default_state = spartacus_prm_default_from_fortran()?;
    let payload = spartacus_prm_to_values_payload(&default_state);
    let names = spartacus_prm_field_names_with_height_len(default_state.height.len());

    let mut flat = payload.values.clone();
    for (name, value) in values {
        let idx = field_index(&names, name).ok_or(BridgeError::BadState)?;
        flat[idx] = *value;
    }

    spartacus_prm_from_values_payload(&SpartacusPrmValuesPayload {
        schema_version: SPARTACUS_PRM_SCHEMA_VERSION,
        values: flat,
        dims: payload.dims,
    })
}

pub fn spartacus_prm_default_from_fortran() -> Result<SpartacusPrm, BridgeError> {
    let (n_flat, height_len, nlayer) = spartacus_prm_schema()?;
    let mut flat = vec![0.0_f64; n_flat];
    let mut height_len_runtime = -1_i32;
    let mut nlayer_runtime = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_spartacus_prm_default(
            flat.as_mut_ptr(),
            n_flat as i32,
            &mut height_len_runtime as *mut i32,
            &mut nlayer_runtime as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || height_len_runtime < 0 || nlayer_runtime < 0 {
        return Err(BridgeError::from_code(err));
    }

    if height_len_runtime as usize != height_len || nlayer_runtime as usize != nlayer {
        return Err(BridgeError::BadState);
    }

    SpartacusPrm::from_flat_with_height_len(&flat, height_len)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let (n_flat, height_len, nlayer) =
            spartacus_prm_schema().expect("schema call should succeed");
        let expected =
            spartacus_prm_expected_flat_len(height_len).expect("expected len should fit");
        assert_eq!(n_flat, expected);
        assert_eq!(nlayer, spartacus_prm_nlayer_from_height_len(height_len));
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            spartacus_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, SpartacusPrm::default());

        let state2 = SpartacusPrm::from_flat_with_height_len(&state.to_flat(), state.height.len())
            .expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            spartacus_prm_default_from_fortran().expect("default state should be available");
        let mapped = spartacus_prm_to_map(&state);
        let updated = spartacus_prm_from_map(&mapped).expect("map to state should succeed");
        assert_eq!(updated, state);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            spartacus_prm_default_from_fortran().expect("default state should be available");
        let payload = spartacus_prm_to_values_payload(&state);
        let recovered =
            spartacus_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = SpartacusPrmValuesPayload {
            schema_version: SPARTACUS_PRM_SCHEMA_VERSION + 1,
            values: payload.values.clone(),
            dims: payload.dims.clone(),
        };
        let err = spartacus_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_missing_dims() {
        let state =
            spartacus_prm_default_from_fortran().expect("default state should be available");
        let payload = SpartacusPrmValuesPayload {
            schema_version: SPARTACUS_PRM_SCHEMA_VERSION,
            values: state.to_flat(),
            dims: PayloadDims::new(),
        };
        let err =
            spartacus_prm_from_values_payload(&payload).expect_err("missing dims should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn non_integer_stream_count_is_rejected() {
        let mut state =
            spartacus_prm_default_from_fortran().expect("default state should be available");
        state.n_stream_lw_urban = 1;
        let mut payload = spartacus_prm_to_values_payload(&state);
        let names = spartacus_prm_field_names_with_height_len(state.height.len());
        let idx = names
            .iter()
            .position(|name| name == "n_stream_lw_urban")
            .expect("field should exist");
        payload.values[idx] = 1.25;

        let err = spartacus_prm_from_values_payload(&payload)
            .expect_err("non-integer stream count should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
