use crate::codec::{
    dims_element_count, require_field_dims, validate_flat_len, PayloadDims, ValuesPayloadWithDims,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const OUTPUT_BLOCK_FIELD_COUNT: usize = 11;
pub const OUTPUT_BLOCK_BASE_FLAT_LEN: usize = 0;
pub const OUTPUT_BLOCK_SCHEMA_VERSION: u32 = 1;

const OUTPUT_BLOCK_FIELD_ORDER: [&str; OUTPUT_BLOCK_FIELD_COUNT] = [
    "data_out_block_suews",
    "data_out_block_snow",
    "data_out_block_estm",
    "data_out_block_ehc",
    "data_out_block_rsl",
    "data_out_block_beers",
    "data_out_block_debug",
    "data_out_block_spartacus",
    "data_out_block_daily_state",
    "data_out_block_stebbs",
    "data_out_block_nhood",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutputBlockSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
    pub allocatable_dims: PayloadDims,
}

pub type OutputBlockValuesPayload = ValuesPayloadWithDims;

#[derive(Debug, Clone, PartialEq)]
pub struct OutputBlockMatrix {
    pub rows: usize,
    pub cols: usize,
    pub values: Vec<f64>,
}

impl OutputBlockMatrix {
    pub fn new(rows: usize, cols: usize, values: Vec<f64>) -> Result<Self, BridgeError> {
        let expected_len = rows.checked_mul(cols).ok_or(BridgeError::BadState)?;
        validate_flat_len(&values, expected_len)?;
        Ok(Self { rows, cols, values })
    }

    fn zeros(rows: usize, cols: usize) -> Self {
        Self {
            rows,
            cols,
            values: vec![0.0; rows.saturating_mul(cols)],
        }
    }

    fn to_rows(&self) -> Vec<Vec<f64>> {
        if self.rows == 0 || self.cols == 0 {
            return Vec::new();
        }

        self.values
            .chunks(self.cols)
            .map(|row| row.to_vec())
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OutputBlock {
    pub fields: BTreeMap<String, OutputBlockMatrix>,
}

impl OutputBlock {
    fn new(fields: BTreeMap<String, OutputBlockMatrix>) -> Result<Self, BridgeError> {
        if fields.len() != OUTPUT_BLOCK_FIELD_COUNT {
            return Err(BridgeError::BadState);
        }

        for field_name in OUTPUT_BLOCK_FIELD_ORDER {
            if !fields.contains_key(field_name) {
                return Err(BridgeError::BadState);
            }
        }

        Ok(Self { fields })
    }

    fn with_zero_rows(columns: &BTreeMap<String, usize>) -> Result<Self, BridgeError> {
        let mut fields = BTreeMap::new();

        for field_name in OUTPUT_BLOCK_FIELD_ORDER {
            let cols = *columns.get(field_name).ok_or(BridgeError::BadState)?;
            fields.insert(field_name.to_string(), OutputBlockMatrix::zeros(0, cols));
        }

        Self::new(fields)
    }

    fn matrix(&self, field_name: &str) -> Result<&OutputBlockMatrix, BridgeError> {
        self.fields.get(field_name).ok_or(BridgeError::BadState)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let total_len = OUTPUT_BLOCK_FIELD_ORDER
            .iter()
            .filter_map(|name| self.fields.get(*name))
            .map(|matrix| matrix.values.len())
            .sum();

        let mut flat = Vec::with_capacity(total_len);
        for field_name in OUTPUT_BLOCK_FIELD_ORDER {
            if let Some(matrix) = self.fields.get(field_name) {
                flat.extend_from_slice(&matrix.values);
            }
        }

        flat
    }

    pub fn dims(&self) -> PayloadDims {
        let mut dims = PayloadDims::new();
        for field_name in OUTPUT_BLOCK_FIELD_ORDER {
            if let Some(matrix) = self.fields.get(field_name) {
                dims.insert(field_name.to_string(), vec![matrix.rows, matrix.cols]);
            }
        }
        dims
    }

    pub fn from_flat_with_dims(
        values: &[f64],
        dims: &PayloadDims,
        columns: &BTreeMap<String, usize>,
    ) -> Result<Self, BridgeError> {
        if dims.len() != OUTPUT_BLOCK_FIELD_COUNT {
            return Err(BridgeError::BadState);
        }

        for name in dims.keys() {
            if !OUTPUT_BLOCK_FIELD_ORDER.contains(&name.as_str()) {
                return Err(BridgeError::BadState);
            }
        }

        let mut expected_len = 0_usize;
        for field_name in OUTPUT_BLOCK_FIELD_ORDER {
            let field_dims = require_field_dims(dims, field_name, 2)?;
            let cols = field_dims[1];
            let expected_cols = *columns.get(field_name).ok_or(BridgeError::BadState)?;
            if cols != expected_cols {
                return Err(BridgeError::BadState);
            }

            let field_len = dims_element_count(&field_dims)?;
            expected_len = expected_len
                .checked_add(field_len)
                .ok_or(BridgeError::BadState)?;
        }

        validate_flat_len(values, expected_len)?;

        let mut idx = 0_usize;
        let mut fields = BTreeMap::new();

        for field_name in OUTPUT_BLOCK_FIELD_ORDER {
            let field_dims = require_field_dims(dims, field_name, 2)?;
            let rows = field_dims[0];
            let cols = field_dims[1];
            let field_len = dims_element_count(&field_dims)?;

            let next_idx = idx.checked_add(field_len).ok_or(BridgeError::BadState)?;
            let field_values = values[idx..next_idx].to_vec();
            idx = next_idx;

            fields.insert(
                field_name.to_string(),
                OutputBlockMatrix::new(rows, cols, field_values)?,
            );
        }

        Self::new(fields)
    }
}

pub fn output_block_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_output_block_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || n_flat < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

fn output_block_columns_runtime() -> Result<BTreeMap<String, usize>, BridgeError> {
    let mut cols = vec![0_i32; OUTPUT_BLOCK_FIELD_COUNT];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_output_block_columns(
            cols.as_mut_ptr(),
            OUTPUT_BLOCK_FIELD_COUNT as i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    let mut out = BTreeMap::new();
    for (idx, field_name) in OUTPUT_BLOCK_FIELD_ORDER.iter().enumerate() {
        if cols[idx] < 0 {
            return Err(BridgeError::BadState);
        }
        out.insert((*field_name).to_string(), cols[idx] as usize);
    }

    Ok(out)
}

pub fn output_block_schema_info() -> Result<OutputBlockSchema, BridgeError> {
    let flat_len = output_block_schema()?;
    let schema_version_runtime = output_block_schema_version_runtime()?;
    let field_names = output_block_field_names();
    let columns = output_block_columns_runtime()?;

    if schema_version_runtime != OUTPUT_BLOCK_SCHEMA_VERSION
        || flat_len != OUTPUT_BLOCK_BASE_FLAT_LEN
    {
        return Err(BridgeError::BadState);
    }

    let mut allocatable_dims = PayloadDims::new();
    for field_name in OUTPUT_BLOCK_FIELD_ORDER {
        let cols = *columns.get(field_name).ok_or(BridgeError::BadState)?;
        allocatable_dims.insert(field_name.to_string(), vec![0, cols]);
    }

    Ok(OutputBlockSchema {
        schema_version: OUTPUT_BLOCK_SCHEMA_VERSION,
        flat_len,
        field_names,
        allocatable_dims,
    })
}

pub fn output_block_schema_version() -> u32 {
    OUTPUT_BLOCK_SCHEMA_VERSION
}

pub fn output_block_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_output_block_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn output_block_field_names() -> Vec<String> {
    OUTPUT_BLOCK_FIELD_ORDER
        .iter()
        .map(|name| (*name).to_string())
        .collect()
}

pub fn output_block_to_ordered_values(state: &OutputBlock) -> Vec<f64> {
    state.to_flat()
}

pub fn output_block_to_values_payload(state: &OutputBlock) -> OutputBlockValuesPayload {
    OutputBlockValuesPayload {
        schema_version: OUTPUT_BLOCK_SCHEMA_VERSION,
        values: state.to_flat(),
        dims: state.dims(),
    }
}

pub fn output_block_from_ordered_values(values: &[f64]) -> Result<OutputBlock, BridgeError> {
    if !values.is_empty() {
        return Err(BridgeError::BadBuffer);
    }

    output_block_default_from_fortran()
}

pub fn output_block_from_values_payload(
    payload: &OutputBlockValuesPayload,
) -> Result<OutputBlock, BridgeError> {
    if payload.schema_version != OUTPUT_BLOCK_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    let columns = output_block_columns_runtime()?;
    OutputBlock::from_flat_with_dims(&payload.values, &payload.dims, &columns)
}

pub fn output_block_default_from_fortran() -> Result<OutputBlock, BridgeError> {
    let n_flat = output_block_schema()?;
    if n_flat != OUTPUT_BLOCK_BASE_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_output_block_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    let columns = output_block_columns_runtime()?;
    OutputBlock::with_zero_rows(&columns)
}

pub fn output_block_to_rows_map(
    state: &OutputBlock,
) -> Result<BTreeMap<String, Vec<Vec<f64>>>, BridgeError> {
    let mut rows_map = BTreeMap::new();

    for field_name in OUTPUT_BLOCK_FIELD_ORDER {
        let matrix = state.matrix(field_name)?;
        rows_map.insert(field_name.to_string(), matrix.to_rows());
    }

    Ok(rows_map)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_contains_all_allocatable_fields() {
        let schema = output_block_schema_info().expect("schema info should be available");

        assert_eq!(schema.flat_len, OUTPUT_BLOCK_BASE_FLAT_LEN);
        assert_eq!(schema.field_names.len(), OUTPUT_BLOCK_FIELD_COUNT);
        assert_eq!(schema.allocatable_dims.len(), OUTPUT_BLOCK_FIELD_COUNT);

        for field_name in output_block_field_names() {
            let dims = schema
                .allocatable_dims
                .get(&field_name)
                .expect("field dims should be present");
            assert_eq!(dims.len(), 2);
            assert_eq!(dims[0], 0);
            assert!(dims[1] > 0);
        }
    }

    #[test]
    fn default_state_has_unallocated_blocks() {
        let state = output_block_default_from_fortran().expect("default state should be available");
        let payload = output_block_to_values_payload(&state);

        assert!(payload.values.is_empty());
        assert_eq!(payload.dims.len(), OUTPUT_BLOCK_FIELD_COUNT);
        for dims in payload.dims.values() {
            assert_eq!(dims[0], 0);
            assert!(dims[1] > 0);
        }
    }

    #[test]
    fn values_payload_roundtrip_with_nonzero_rows() {
        let schema = output_block_schema_info().expect("schema info should be available");

        let mut dims = PayloadDims::new();
        let mut expected_len = 0_usize;
        for field_name in output_block_field_names() {
            let cols = schema
                .allocatable_dims
                .get(&field_name)
                .expect("schema dims should include field")[1];
            dims.insert(field_name, vec![2, cols]);
            expected_len += 2 * cols;
        }

        let values: Vec<f64> = (0..expected_len).map(|idx| idx as f64 + 1.0).collect();
        let payload = OutputBlockValuesPayload {
            schema_version: OUTPUT_BLOCK_SCHEMA_VERSION,
            values: values.clone(),
            dims: dims.clone(),
        };

        let state = output_block_from_values_payload(&payload).expect("payload decode should work");
        let encoded = output_block_to_values_payload(&state);

        assert_eq!(encoded.schema_version, OUTPUT_BLOCK_SCHEMA_VERSION);
        assert_eq!(encoded.dims, dims);
        assert_eq!(encoded.values, values);
    }

    #[test]
    fn values_payload_rejects_column_mismatch() {
        let schema = output_block_schema_info().expect("schema info should be available");

        let mut payload = output_block_to_values_payload(
            &output_block_default_from_fortran().expect("default state should be available"),
        );

        let first_name = output_block_field_names().remove(0);
        let current_dims = schema
            .allocatable_dims
            .get(&first_name)
            .expect("schema dims should include first field");
        payload
            .dims
            .insert(first_name.clone(), vec![1, current_dims[1] + 1]);
        payload.values = vec![1.0; current_dims[1] + 1];

        let err =
            output_block_from_values_payload(&payload).expect_err("column mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_missing_dims() {
        let mut payload = output_block_to_values_payload(
            &output_block_default_from_fortran().expect("default state should be available"),
        );

        payload.dims.remove("data_out_block_suews");

        let err = output_block_from_values_payload(&payload).expect_err("missing dims should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_length_mismatch() {
        let schema = output_block_schema_info().expect("schema info should be available");

        let mut dims = PayloadDims::new();
        let mut expected_len = 0_usize;
        for field_name in output_block_field_names() {
            let cols = schema
                .allocatable_dims
                .get(&field_name)
                .expect("schema dims should include field")[1];
            dims.insert(field_name, vec![1, cols]);
            expected_len += cols;
        }

        let payload = OutputBlockValuesPayload {
            schema_version: OUTPUT_BLOCK_SCHEMA_VERSION,
            values: vec![0.0; expected_len.saturating_sub(1)],
            dims,
        };

        let err =
            output_block_from_values_payload(&payload).expect_err("length mismatch should fail");
        assert_eq!(err, BridgeError::BadBuffer);
    }
}
