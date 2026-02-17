use crate::codec::{
    dims_element_count, field_index, require_field_dims, validate_flat_len, PayloadDims,
    ValuesPayloadWithDims,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const SUEWS_FORCING_BASE_FLAT_LEN: usize = 16;
pub const SUEWS_FORCING_SCHEMA_VERSION: u32 = 1;
pub const SUEWS_FORCING_TS5_FIELD: &str = "ts5mindata_ir";

const SUEWS_FORCING_BASE_FIELDS: [&str; SUEWS_FORCING_BASE_FLAT_LEN] = [
    "kdown",
    "ldown",
    "rh",
    "pres",
    "tair_av_5d",
    "u",
    "rain",
    "wu_m3",
    "fcld",
    "lai_obs",
    "snowfrac",
    "xsmd",
    "qf_obs",
    "qn1_obs",
    "qs_obs",
    "temp_c",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuewsForcingSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub base_flat_len: usize,
    pub ts5mindata_ir_len: usize,
    pub field_names: Vec<String>,
    pub allocatable_dims: PayloadDims,
}

pub type SuewsForcingValuesPayload = ValuesPayloadWithDims;

#[derive(Debug, Clone, PartialEq)]
pub struct SuewsForcing {
    pub kdown: f64,
    pub ldown: f64,
    pub rh: f64,
    pub pres: f64,
    pub tair_av_5d: f64,
    pub u: f64,
    pub rain: f64,
    pub wu_m3: f64,
    pub fcld: f64,
    pub lai_obs: f64,
    pub snowfrac: f64,
    pub xsmd: f64,
    pub qf_obs: f64,
    pub qn1_obs: f64,
    pub qs_obs: f64,
    pub temp_c: f64,
    pub ts5mindata_ir: Vec<f64>,
}

impl Default for SuewsForcing {
    fn default() -> Self {
        Self {
            kdown: 0.0,
            ldown: 0.0,
            rh: 0.0,
            pres: 0.0,
            tair_av_5d: 0.0,
            u: 0.0,
            rain: 0.0,
            wu_m3: 0.0,
            fcld: 0.0,
            lai_obs: 0.0,
            snowfrac: 0.0,
            xsmd: 0.0,
            qf_obs: 0.0,
            qn1_obs: 0.0,
            qs_obs: 0.0,
            temp_c: 0.0,
            ts5mindata_ir: Vec::new(),
        }
    }
}

impl SuewsForcing {
    pub fn from_flat_with_ts_len(
        flat: &[f64],
        ts5mindata_ir_len: usize,
    ) -> Result<Self, BridgeError> {
        let expected_len = SUEWS_FORCING_BASE_FLAT_LEN
            .checked_add(ts5mindata_ir_len)
            .ok_or(BridgeError::BadState)?;
        validate_flat_len(flat, expected_len)?;

        Ok(Self {
            kdown: flat[0],
            ldown: flat[1],
            rh: flat[2],
            pres: flat[3],
            tair_av_5d: flat[4],
            u: flat[5],
            rain: flat[6],
            wu_m3: flat[7],
            fcld: flat[8],
            lai_obs: flat[9],
            snowfrac: flat[10],
            xsmd: flat[11],
            qf_obs: flat[12],
            qn1_obs: flat[13],
            qs_obs: flat[14],
            temp_c: flat[15],
            ts5mindata_ir: flat[SUEWS_FORCING_BASE_FLAT_LEN..].to_vec(),
        })
    }

    pub fn from_ordered_values(values: &[f64]) -> Result<Self, BridgeError> {
        if values.len() < SUEWS_FORCING_BASE_FLAT_LEN {
            return Err(BridgeError::BadBuffer);
        }

        let ts5mindata_ir_len = values.len() - SUEWS_FORCING_BASE_FLAT_LEN;
        Self::from_flat_with_ts_len(values, ts5mindata_ir_len)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(SUEWS_FORCING_BASE_FLAT_LEN + self.ts5mindata_ir.len());

        flat.push(self.kdown);
        flat.push(self.ldown);
        flat.push(self.rh);
        flat.push(self.pres);
        flat.push(self.tair_av_5d);
        flat.push(self.u);
        flat.push(self.rain);
        flat.push(self.wu_m3);
        flat.push(self.fcld);
        flat.push(self.lai_obs);
        flat.push(self.snowfrac);
        flat.push(self.xsmd);
        flat.push(self.qf_obs);
        flat.push(self.qn1_obs);
        flat.push(self.qs_obs);
        flat.push(self.temp_c);
        flat.extend_from_slice(&self.ts5mindata_ir);

        flat
    }
}

fn parse_ts5mindata_ir_field_index(name: &str) -> Option<usize> {
    let suffix = name.strip_prefix("ts5mindata_ir_")?;
    let one_based = suffix.parse::<usize>().ok()?;
    one_based.checked_sub(1)
}

fn set_base_field_value(
    state: &mut SuewsForcing,
    index: usize,
    value: f64,
) -> Result<(), BridgeError> {
    match index {
        0 => state.kdown = value,
        1 => state.ldown = value,
        2 => state.rh = value,
        3 => state.pres = value,
        4 => state.tair_av_5d = value,
        5 => state.u = value,
        6 => state.rain = value,
        7 => state.wu_m3 = value,
        8 => state.fcld = value,
        9 => state.lai_obs = value,
        10 => state.snowfrac = value,
        11 => state.xsmd = value,
        12 => state.qf_obs = value,
        13 => state.qn1_obs = value,
        14 => state.qs_obs = value,
        15 => state.temp_c = value,
        _ => return Err(BridgeError::BadState),
    }
    Ok(())
}

pub fn suews_forcing_schema() -> Result<(usize, usize), BridgeError> {
    let mut n_flat = -1_i32;
    let mut ts5mindata_ir_len = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_forcing_len(
            &mut n_flat as *mut i32,
            &mut ts5mindata_ir_len as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }
    if n_flat < 0 || ts5mindata_ir_len < 0 {
        return Err(BridgeError::BadState);
    }

    Ok((n_flat as usize, ts5mindata_ir_len as usize))
}

pub fn suews_forcing_schema_info() -> Result<SuewsForcingSchema, BridgeError> {
    let (flat_len, ts5mindata_ir_len) = suews_forcing_schema()?;
    let schema_version_runtime = suews_forcing_schema_version_runtime()?;
    let field_names = suews_forcing_field_names_with_ts_len(ts5mindata_ir_len);

    let mut allocatable_dims = PayloadDims::new();
    allocatable_dims.insert(SUEWS_FORCING_TS5_FIELD.to_string(), vec![ts5mindata_ir_len]);

    if schema_version_runtime != SUEWS_FORCING_SCHEMA_VERSION
        || flat_len != SUEWS_FORCING_BASE_FLAT_LEN + ts5mindata_ir_len
        || flat_len != field_names.len()
    {
        return Err(BridgeError::BadState);
    }

    Ok(SuewsForcingSchema {
        schema_version: SUEWS_FORCING_SCHEMA_VERSION,
        flat_len,
        base_flat_len: SUEWS_FORCING_BASE_FLAT_LEN,
        ts5mindata_ir_len,
        field_names,
        allocatable_dims,
    })
}

pub fn suews_forcing_schema_version() -> u32 {
    SUEWS_FORCING_SCHEMA_VERSION
}

pub fn suews_forcing_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_forcing_schema_version(&mut schema_version as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn suews_forcing_base_field_names() -> Vec<String> {
    SUEWS_FORCING_BASE_FIELDS
        .iter()
        .map(|name| (*name).to_string())
        .collect()
}

pub fn suews_forcing_field_names_with_ts_len(ts5mindata_ir_len: usize) -> Vec<String> {
    let mut names = suews_forcing_base_field_names();
    for idx in 0..ts5mindata_ir_len {
        names.push(format!("ts5mindata_ir_{}", idx + 1));
    }
    names
}

pub fn suews_forcing_field_names() -> Result<Vec<String>, BridgeError> {
    let (_, ts5mindata_ir_len) = suews_forcing_schema()?;
    Ok(suews_forcing_field_names_with_ts_len(ts5mindata_ir_len))
}

pub fn suews_forcing_to_map(state: &SuewsForcing) -> BTreeMap<String, f64> {
    let names = suews_forcing_field_names_with_ts_len(state.ts5mindata_ir.len());
    let values = state.to_flat();
    names.into_iter().zip(values).collect()
}

pub fn suews_forcing_to_ordered_values(state: &SuewsForcing) -> Vec<f64> {
    state.to_flat()
}

pub fn suews_forcing_to_values_payload(state: &SuewsForcing) -> SuewsForcingValuesPayload {
    let mut dims = PayloadDims::new();
    dims.insert(
        SUEWS_FORCING_TS5_FIELD.to_string(),
        vec![state.ts5mindata_ir.len()],
    );

    SuewsForcingValuesPayload {
        schema_version: SUEWS_FORCING_SCHEMA_VERSION,
        values: state.to_flat(),
        dims,
    }
}

pub fn suews_forcing_from_ordered_values(values: &[f64]) -> Result<SuewsForcing, BridgeError> {
    SuewsForcing::from_ordered_values(values)
}

pub fn suews_forcing_from_values_payload(
    payload: &SuewsForcingValuesPayload,
) -> Result<SuewsForcing, BridgeError> {
    if payload.schema_version != SUEWS_FORCING_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    if payload.dims.len() != 1 {
        return Err(BridgeError::BadState);
    }

    let ts5mindata_ir_dims = require_field_dims(&payload.dims, SUEWS_FORCING_TS5_FIELD, 1)?;
    let ts5mindata_ir_len = dims_element_count(&ts5mindata_ir_dims)?;
    let expected_len = SUEWS_FORCING_BASE_FLAT_LEN
        .checked_add(ts5mindata_ir_len)
        .ok_or(BridgeError::BadState)?;
    validate_flat_len(&payload.values, expected_len)?;

    SuewsForcing::from_flat_with_ts_len(&payload.values, ts5mindata_ir_len)
}

pub fn suews_forcing_from_map(values: &BTreeMap<String, f64>) -> Result<SuewsForcing, BridgeError> {
    let mut state = suews_forcing_default_from_fortran()?;
    let base_field_names = suews_forcing_base_field_names();

    for (name, value) in values {
        if let Some(index) = field_index(&base_field_names, name) {
            set_base_field_value(&mut state, index, *value)?;
            continue;
        }

        if let Some(ts_index) = parse_ts5mindata_ir_field_index(name) {
            if state.ts5mindata_ir.len() <= ts_index {
                state.ts5mindata_ir.resize(ts_index + 1, 0.0);
            }
            state.ts5mindata_ir[ts_index] = *value;
            continue;
        }

        return Err(BridgeError::BadState);
    }

    Ok(state)
}

pub fn suews_forcing_default_from_fortran() -> Result<SuewsForcing, BridgeError> {
    let (n_flat, ts5mindata_ir_len_expected) = suews_forcing_schema()?;
    let mut flat = vec![0.0_f64; n_flat];
    let mut ts5mindata_ir_len = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_forcing_default(
            flat.as_mut_ptr(),
            n_flat as i32,
            &mut ts5mindata_ir_len as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }
    if ts5mindata_ir_len < 0 {
        return Err(BridgeError::BadState);
    }

    let ts5mindata_ir_len = ts5mindata_ir_len as usize;
    if ts5mindata_ir_len != ts5mindata_ir_len_expected {
        return Err(BridgeError::BadState);
    }

    SuewsForcing::from_flat_with_ts_len(&flat, ts5mindata_ir_len)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let (flat_len, ts5mindata_ir_len) =
            suews_forcing_schema().expect("schema call should succeed");
        assert_eq!(flat_len, SUEWS_FORCING_BASE_FLAT_LEN + ts5mindata_ir_len);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            suews_forcing_default_from_fortran().expect("default state should be available");
        assert!(state
            .ts5mindata_ir
            .iter()
            .all(|value| value.abs() < 1.0e-12));

        let state2 =
            SuewsForcing::from_flat_with_ts_len(&state.to_flat(), state.ts5mindata_ir.len())
                .expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            suews_forcing_default_from_fortran().expect("default state should be available");
        let mut mapped = suews_forcing_to_map(&state);
        mapped.insert("kdown".to_string(), 500.0);
        mapped.insert("ts5mindata_ir_2".to_string(), 12.5);

        let updated = suews_forcing_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.kdown - 500.0).abs() < 1.0e-12);
        assert!(updated.ts5mindata_ir.len() >= 2);
        assert!((updated.ts5mindata_ir[1] - 12.5).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            suews_forcing_default_from_fortran().expect("default state should be available");
        let payload = suews_forcing_to_values_payload(&state);
        let recovered =
            suews_forcing_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = SuewsForcingValuesPayload {
            schema_version: SUEWS_FORCING_SCHEMA_VERSION + 1,
            values: payload.values.clone(),
            dims: payload.dims.clone(),
        };
        let err = suews_forcing_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_missing_dims() {
        let state =
            suews_forcing_default_from_fortran().expect("default state should be available");
        let payload = SuewsForcingValuesPayload {
            schema_version: SUEWS_FORCING_SCHEMA_VERSION,
            values: state.to_flat(),
            dims: PayloadDims::new(),
        };
        let err =
            suews_forcing_from_values_payload(&payload).expect_err("missing dims should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_dims_length_mismatch() {
        let state =
            suews_forcing_default_from_fortran().expect("default state should be available");
        let mut payload = suews_forcing_to_values_payload(&state);
        payload.dims.insert(
            SUEWS_FORCING_TS5_FIELD.to_string(),
            vec![state.ts5mindata_ir.len() + 1],
        );

        let err = suews_forcing_from_values_payload(&payload)
            .expect_err("dims/values mismatch should fail");
        assert_eq!(err, BridgeError::BadBuffer);
    }
}
