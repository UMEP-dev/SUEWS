use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const OUTPUT_LINE_DATETIME_LEN: usize = 5;
pub const OUTPUT_LINE_SUEWS_LEN: usize = 118;
pub const OUTPUT_LINE_SNOW_LEN: usize = 103;
pub const OUTPUT_LINE_ESTM_LEN: usize = 32;
pub const OUTPUT_LINE_EHC_LEN: usize = 229;
pub const OUTPUT_LINE_RSL_LEN: usize = 140;
pub const OUTPUT_LINE_BEERS_LEN: usize = 34;
pub const OUTPUT_LINE_DEBUG_LEN: usize = 136;
pub const OUTPUT_LINE_SPARTACUS_LEN: usize = 199;
pub const OUTPUT_LINE_DAILYSTATE_LEN: usize = 52;
pub const OUTPUT_LINE_STEBBS_LEN: usize = 85;
pub const OUTPUT_LINE_NHOOD_LEN: usize = 6;
pub const OUTPUT_LINE_FLAT_LEN: usize = 1139;
pub const OUTPUT_LINE_SCHEMA_VERSION: u32 = 1;

pub type OutputLineSchema = crate::codec::SimpleSchema;

pub type OutputLineValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct OutputLine {
    pub datetime_line: [f64; OUTPUT_LINE_DATETIME_LEN],
    pub data_out_line_suews: [f64; OUTPUT_LINE_SUEWS_LEN],
    pub data_out_line_snow: [f64; OUTPUT_LINE_SNOW_LEN],
    pub data_out_line_estm: [f64; OUTPUT_LINE_ESTM_LEN],
    pub data_out_line_ehc: [f64; OUTPUT_LINE_EHC_LEN],
    pub data_out_line_rsl: [f64; OUTPUT_LINE_RSL_LEN],
    pub data_out_line_beers: [f64; OUTPUT_LINE_BEERS_LEN],
    pub data_out_line_debug: [f64; OUTPUT_LINE_DEBUG_LEN],
    pub data_out_line_spartacus: [f64; OUTPUT_LINE_SPARTACUS_LEN],
    pub data_out_line_daily_state: [f64; OUTPUT_LINE_DAILYSTATE_LEN],
    pub data_out_line_stebbs: [f64; OUTPUT_LINE_STEBBS_LEN],
    pub data_out_line_nhood: [f64; OUTPUT_LINE_NHOOD_LEN],
}

impl Default for OutputLine {
    fn default() -> Self {
        Self {
            datetime_line: [-999.0; OUTPUT_LINE_DATETIME_LEN],
            data_out_line_suews: [-999.0; OUTPUT_LINE_SUEWS_LEN],
            data_out_line_snow: [-999.0; OUTPUT_LINE_SNOW_LEN],
            data_out_line_estm: [-999.0; OUTPUT_LINE_ESTM_LEN],
            data_out_line_ehc: [-999.0; OUTPUT_LINE_EHC_LEN],
            data_out_line_rsl: [-999.0; OUTPUT_LINE_RSL_LEN],
            data_out_line_beers: [-999.0; OUTPUT_LINE_BEERS_LEN],
            data_out_line_debug: [-999.0; OUTPUT_LINE_DEBUG_LEN],
            data_out_line_spartacus: [-999.0; OUTPUT_LINE_SPARTACUS_LEN],
            data_out_line_daily_state: [-999.0; OUTPUT_LINE_DAILYSTATE_LEN],
            data_out_line_stebbs: [-999.0; OUTPUT_LINE_STEBBS_LEN],
            data_out_line_nhood: [-999.0; OUTPUT_LINE_NHOOD_LEN],
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

impl OutputLine {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, OUTPUT_LINE_FLAT_LEN)?;

        let mut idx = 0_usize;

        let datetime_line =
            copy_fixed::<OUTPUT_LINE_DATETIME_LEN>(&flat[idx..idx + OUTPUT_LINE_DATETIME_LEN])?;
        idx += OUTPUT_LINE_DATETIME_LEN;

        let data_out_line_suews =
            copy_fixed::<OUTPUT_LINE_SUEWS_LEN>(&flat[idx..idx + OUTPUT_LINE_SUEWS_LEN])?;
        idx += OUTPUT_LINE_SUEWS_LEN;

        let data_out_line_snow =
            copy_fixed::<OUTPUT_LINE_SNOW_LEN>(&flat[idx..idx + OUTPUT_LINE_SNOW_LEN])?;
        idx += OUTPUT_LINE_SNOW_LEN;

        let data_out_line_estm =
            copy_fixed::<OUTPUT_LINE_ESTM_LEN>(&flat[idx..idx + OUTPUT_LINE_ESTM_LEN])?;
        idx += OUTPUT_LINE_ESTM_LEN;

        let data_out_line_ehc =
            copy_fixed::<OUTPUT_LINE_EHC_LEN>(&flat[idx..idx + OUTPUT_LINE_EHC_LEN])?;
        idx += OUTPUT_LINE_EHC_LEN;

        let data_out_line_rsl =
            copy_fixed::<OUTPUT_LINE_RSL_LEN>(&flat[idx..idx + OUTPUT_LINE_RSL_LEN])?;
        idx += OUTPUT_LINE_RSL_LEN;

        let data_out_line_beers =
            copy_fixed::<OUTPUT_LINE_BEERS_LEN>(&flat[idx..idx + OUTPUT_LINE_BEERS_LEN])?;
        idx += OUTPUT_LINE_BEERS_LEN;

        let data_out_line_debug =
            copy_fixed::<OUTPUT_LINE_DEBUG_LEN>(&flat[idx..idx + OUTPUT_LINE_DEBUG_LEN])?;
        idx += OUTPUT_LINE_DEBUG_LEN;

        let data_out_line_spartacus =
            copy_fixed::<OUTPUT_LINE_SPARTACUS_LEN>(&flat[idx..idx + OUTPUT_LINE_SPARTACUS_LEN])?;
        idx += OUTPUT_LINE_SPARTACUS_LEN;

        let data_out_line_daily_state =
            copy_fixed::<OUTPUT_LINE_DAILYSTATE_LEN>(&flat[idx..idx + OUTPUT_LINE_DAILYSTATE_LEN])?;
        idx += OUTPUT_LINE_DAILYSTATE_LEN;

        let data_out_line_stebbs =
            copy_fixed::<OUTPUT_LINE_STEBBS_LEN>(&flat[idx..idx + OUTPUT_LINE_STEBBS_LEN])?;
        idx += OUTPUT_LINE_STEBBS_LEN;

        let data_out_line_nhood =
            copy_fixed::<OUTPUT_LINE_NHOOD_LEN>(&flat[idx..idx + OUTPUT_LINE_NHOOD_LEN])?;

        Ok(Self {
            datetime_line,
            data_out_line_suews,
            data_out_line_snow,
            data_out_line_estm,
            data_out_line_ehc,
            data_out_line_rsl,
            data_out_line_beers,
            data_out_line_debug,
            data_out_line_spartacus,
            data_out_line_daily_state,
            data_out_line_stebbs,
            data_out_line_nhood,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(OUTPUT_LINE_FLAT_LEN);

        flat.extend_from_slice(&self.datetime_line);
        flat.extend_from_slice(&self.data_out_line_suews);
        flat.extend_from_slice(&self.data_out_line_snow);
        flat.extend_from_slice(&self.data_out_line_estm);
        flat.extend_from_slice(&self.data_out_line_ehc);
        flat.extend_from_slice(&self.data_out_line_rsl);
        flat.extend_from_slice(&self.data_out_line_beers);
        flat.extend_from_slice(&self.data_out_line_debug);
        flat.extend_from_slice(&self.data_out_line_spartacus);
        flat.extend_from_slice(&self.data_out_line_daily_state);
        flat.extend_from_slice(&self.data_out_line_stebbs);
        flat.extend_from_slice(&self.data_out_line_nhood);

        flat
    }
}

impl StateCodec for OutputLine {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "output_line".to_string(),
            schema_version: OUTPUT_LINE_SCHEMA_VERSION,
            flat_len: OUTPUT_LINE_FLAT_LEN,
            field_names: output_line_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        OutputLine::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        OutputLine::to_flat(self)
    }
}

pub fn output_line_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_output_line_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn output_line_schema_info() -> Result<OutputLineSchema, BridgeError> {
    let flat_len = output_line_schema()?;
    let schema_version_runtime = output_line_schema_version_runtime()?;
    let field_names = output_line_field_names();

    if schema_version_runtime != OUTPUT_LINE_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(OutputLineSchema {
        schema_version: OUTPUT_LINE_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

fn append_indexed_names(names: &mut Vec<String>, prefix: &str, len: usize) {
    for idx in 1..=len {
        names.push(format!("{prefix}.{idx}"));
    }
}

pub fn output_line_field_names() -> Vec<String> {
    let mut names = Vec::with_capacity(OUTPUT_LINE_FLAT_LEN);

    append_indexed_names(&mut names, "datetime_line", OUTPUT_LINE_DATETIME_LEN);
    append_indexed_names(&mut names, "data_out_line_suews", OUTPUT_LINE_SUEWS_LEN);
    append_indexed_names(&mut names, "data_out_line_snow", OUTPUT_LINE_SNOW_LEN);
    append_indexed_names(&mut names, "data_out_line_estm", OUTPUT_LINE_ESTM_LEN);
    append_indexed_names(&mut names, "data_out_line_ehc", OUTPUT_LINE_EHC_LEN);
    append_indexed_names(&mut names, "data_out_line_rsl", OUTPUT_LINE_RSL_LEN);
    append_indexed_names(&mut names, "data_out_line_beers", OUTPUT_LINE_BEERS_LEN);
    append_indexed_names(&mut names, "data_out_line_debug", OUTPUT_LINE_DEBUG_LEN);
    append_indexed_names(
        &mut names,
        "data_out_line_spartacus",
        OUTPUT_LINE_SPARTACUS_LEN,
    );
    append_indexed_names(
        &mut names,
        "data_out_line_daily_state",
        OUTPUT_LINE_DAILYSTATE_LEN,
    );
    append_indexed_names(&mut names, "data_out_line_stebbs", OUTPUT_LINE_STEBBS_LEN);
    append_indexed_names(&mut names, "data_out_line_nhood", OUTPUT_LINE_NHOOD_LEN);

    names
}

pub fn output_line_schema_version() -> u32 {
    OUTPUT_LINE_SCHEMA_VERSION
}

pub fn output_line_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_output_line_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn output_line_field_index(name: &str) -> Option<usize> {
    let names = output_line_field_names();
    field_index(&names, name)
}

pub fn output_line_to_map(state: &OutputLine) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn output_line_to_ordered_values(state: &OutputLine) -> Vec<f64> {
    state.to_flat()
}

pub fn output_line_from_ordered_values(values: &[f64]) -> Result<OutputLine, BridgeError> {
    OutputLine::from_flat(values)
}

pub fn output_line_to_values_payload(state: &OutputLine) -> OutputLineValuesPayload {
    to_values_payload(state)
}

pub fn output_line_from_values_payload(
    payload: &OutputLineValuesPayload,
) -> Result<OutputLine, BridgeError> {
    from_values_payload(payload)
}

pub fn output_line_from_map(values: &BTreeMap<String, f64>) -> Result<OutputLine, BridgeError> {
    let default_state = output_line_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn output_line_default_from_fortran() -> Result<OutputLine, BridgeError> {
    let n_flat = output_line_schema()?;
    if n_flat != OUTPUT_LINE_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_output_line_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    OutputLine::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = output_line_schema().expect("schema call should succeed");
        assert_eq!(n_flat, OUTPUT_LINE_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = output_line_default_from_fortran().expect("default state should be available");
        assert_eq!(state, OutputLine::default());

        let state2 =
            OutputLine::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = output_line_default_from_fortran().expect("default state should be available");
        let mut mapped = output_line_to_map(&state);
        mapped.insert("datetime_line.3".to_string(), 2026.0);
        mapped.insert("data_out_line_suews.12".to_string(), 42.0);
        mapped.insert("data_out_line_nhood.6".to_string(), 1.0);

        let updated = output_line_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.datetime_line[2] - 2026.0).abs() < 1.0e-12);
        assert!((updated.data_out_line_suews[11] - 42.0).abs() < 1.0e-12);
        assert!((updated.data_out_line_nhood[5] - 1.0).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = output_line_default_from_fortran().expect("default state should be available");
        let payload = output_line_to_values_payload(&state);
        let recovered =
            output_line_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = OutputLineValuesPayload {
            schema_version: OUTPUT_LINE_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = output_line_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
