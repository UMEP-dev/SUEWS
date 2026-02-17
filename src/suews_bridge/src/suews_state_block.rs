use crate::codec::{CompositeCodec, PayloadDims};
use crate::error::BridgeError;
use crate::suews_state::{
    suews_state_from_nested_payload, suews_state_schema_version,
    suews_state_schema_version_runtime, suews_state_to_nested_payload, SuewsState,
};
use serde_json::{Map, Value};

pub const SUEWS_STATE_BLOCK_SCHEMA_VERSION: u32 = 1;
pub const SUEWS_STATE_BLOCK_FIELD_BLOCK: &str = "block";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuewsStateBlockSchema {
    pub schema_version: u32,
    pub field_names: Vec<String>,
    pub allocatable_dims: PayloadDims,
    pub suews_state_schema_version: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SuewsStateBlockValuesPayload {
    pub schema_version: u32,
    pub block: Vec<Value>,
    pub dims: PayloadDims,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SuewsStateBlock {
    pub block: Vec<SuewsState>,
}

fn payload_dims_to_value(dims: &PayloadDims) -> Value {
    let mut out = Map::new();
    for (name, axes) in dims {
        out.insert(
            name.clone(),
            Value::Array(axes.iter().map(|axis| Value::from(*axis as u64)).collect()),
        );
    }
    Value::Object(out)
}

fn payload_dims_from_value(value: &Value) -> Result<PayloadDims, BridgeError> {
    let obj = value.as_object().ok_or(BridgeError::BadState)?;
    let mut dims = PayloadDims::new();

    for (name, raw_axes) in obj {
        let axes = raw_axes.as_array().ok_or(BridgeError::BadState)?;
        let mut parsed = Vec::with_capacity(axes.len());
        for axis in axes {
            let as_u64 = axis.as_u64().ok_or(BridgeError::BadState)?;
            parsed.push(usize::try_from(as_u64).map_err(|_| BridgeError::BadState)?);
        }
        dims.insert(name.clone(), parsed);
    }

    Ok(dims)
}

fn payload_to_nested_value(payload: &SuewsStateBlockValuesPayload) -> Value {
    let mut root = Map::new();
    root.insert(
        "schema_version".to_string(),
        Value::from(payload.schema_version as u64),
    );
    root.insert("block".to_string(), Value::Array(payload.block.clone()));
    root.insert("dims".to_string(), payload_dims_to_value(&payload.dims));
    Value::Object(root)
}

fn payload_from_nested_value(value: &Value) -> Result<SuewsStateBlockValuesPayload, BridgeError> {
    let root = value.as_object().ok_or(BridgeError::BadState)?;
    if root.len() != 3
        || !root.contains_key("schema_version")
        || !root.contains_key("block")
        || !root.contains_key("dims")
    {
        return Err(BridgeError::BadState);
    }

    let schema_version = root
        .get("schema_version")
        .and_then(Value::as_u64)
        .ok_or(BridgeError::BadState)
        .and_then(|v| u32::try_from(v).map_err(|_| BridgeError::BadState))?;

    let block = root
        .get("block")
        .and_then(Value::as_array)
        .ok_or(BridgeError::BadState)?
        .to_vec();

    let dims = payload_dims_from_value(root.get("dims").ok_or(BridgeError::BadState)?)?;

    Ok(SuewsStateBlockValuesPayload {
        schema_version,
        block,
        dims,
    })
}

impl CompositeCodec for SuewsStateBlock {
    fn to_nested_payload(&self) -> Value {
        suews_state_block_to_nested_payload(self)
    }

    fn from_nested_payload(payload: &Value) -> Result<Self, BridgeError> {
        suews_state_block_from_nested_payload(payload)
    }
}

pub fn suews_state_block_schema_version() -> u32 {
    SUEWS_STATE_BLOCK_SCHEMA_VERSION
}

pub fn suews_state_block_schema_version_runtime() -> Result<u32, BridgeError> {
    if suews_state_schema_version_runtime()? != suews_state_schema_version() {
        return Err(BridgeError::BadState);
    }

    Ok(SUEWS_STATE_BLOCK_SCHEMA_VERSION)
}

pub fn suews_state_block_field_names() -> Vec<String> {
    vec![SUEWS_STATE_BLOCK_FIELD_BLOCK.to_string()]
}

pub fn suews_state_block_schema_info() -> Result<SuewsStateBlockSchema, BridgeError> {
    if suews_state_block_schema_version_runtime()? != SUEWS_STATE_BLOCK_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    let mut allocatable_dims = PayloadDims::new();
    allocatable_dims.insert(SUEWS_STATE_BLOCK_FIELD_BLOCK.to_string(), vec![0]);

    Ok(SuewsStateBlockSchema {
        schema_version: SUEWS_STATE_BLOCK_SCHEMA_VERSION,
        field_names: suews_state_block_field_names(),
        allocatable_dims,
        suews_state_schema_version: suews_state_schema_version(),
    })
}

pub fn suews_state_block_default_from_fortran() -> Result<SuewsStateBlock, BridgeError> {
    Ok(SuewsStateBlock { block: Vec::new() })
}

pub fn suews_state_block_to_values_payload(
    state: &SuewsStateBlock,
) -> SuewsStateBlockValuesPayload {
    let mut dims = PayloadDims::new();
    dims.insert(
        SUEWS_STATE_BLOCK_FIELD_BLOCK.to_string(),
        vec![state.block.len()],
    );

    SuewsStateBlockValuesPayload {
        schema_version: SUEWS_STATE_BLOCK_SCHEMA_VERSION,
        block: state
            .block
            .iter()
            .map(suews_state_to_nested_payload)
            .collect(),
        dims,
    }
}

pub fn suews_state_block_from_values_payload(
    payload: &SuewsStateBlockValuesPayload,
) -> Result<SuewsStateBlock, BridgeError> {
    if payload.schema_version != SUEWS_STATE_BLOCK_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    if payload.dims.len() != 1 {
        return Err(BridgeError::BadState);
    }

    let block_dims = payload
        .dims
        .get(SUEWS_STATE_BLOCK_FIELD_BLOCK)
        .ok_or(BridgeError::BadState)?;
    if block_dims.len() != 1 {
        return Err(BridgeError::BadState);
    }

    let block_len = block_dims[0];
    if block_len != payload.block.len() {
        return Err(BridgeError::BadState);
    }

    let mut block = Vec::with_capacity(block_len);
    for nested_state in &payload.block {
        block.push(suews_state_from_nested_payload(nested_state)?);
    }

    Ok(SuewsStateBlock { block })
}

pub fn suews_state_block_to_nested_payload(state: &SuewsStateBlock) -> Value {
    payload_to_nested_value(&suews_state_block_to_values_payload(state))
}

pub fn suews_state_block_from_nested_payload(
    payload: &Value,
) -> Result<SuewsStateBlock, BridgeError> {
    let values_payload = payload_from_nested_value(payload)?;
    suews_state_block_from_values_payload(&values_payload)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_roundtrip_via_values_payload() {
        let state =
            suews_state_block_default_from_fortran().expect("default state should be available");
        let payload = suews_state_block_to_values_payload(&state);
        let decoded = suews_state_block_from_values_payload(&payload)
            .expect("values payload decode should work");
        assert_eq!(decoded, state);
    }

    #[test]
    fn empty_roundtrip_via_nested_payload() {
        let state =
            suews_state_block_default_from_fortran().expect("default state should be available");
        let payload = suews_state_block_to_nested_payload(&state);
        let decoded = suews_state_block_from_nested_payload(&payload)
            .expect("nested payload decode should work");
        assert_eq!(decoded, state);
    }

    #[test]
    fn values_payload_rejects_dims_mismatch() {
        let mut payload = suews_state_block_to_values_payload(
            &suews_state_block_default_from_fortran().expect("default state should be available"),
        );
        payload
            .dims
            .insert(SUEWS_STATE_BLOCK_FIELD_BLOCK.to_string(), vec![1]);

        let err =
            suews_state_block_from_values_payload(&payload).expect_err("dims mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
