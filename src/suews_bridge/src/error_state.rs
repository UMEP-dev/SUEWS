use crate::codec::{require_field_dims, PayloadDims};
use crate::error::BridgeError;
use crate::error_entry::{
    error_entry_from_values_payload, error_entry_schema_info, error_entry_to_values_payload,
    validate_text_field, ErrorEntry, ErrorEntrySchema, ErrorEntryValuesPayload,
};
use crate::ffi;
use std::os::raw::c_char;

pub const ERROR_STATE_SCHEMA_VERSION: u32 = 1;
pub const ERROR_STATE_MESSAGE_LEN: usize = 512;
pub const ERROR_STATE_LOG_FIELD: &str = "log";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorStateSchema {
    pub schema_version: u32,
    pub message_len: usize,
    pub field_names: Vec<String>,
    pub allocatable_dims: PayloadDims,
    pub entry_schema: ErrorEntrySchema,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorStateValuesPayload {
    pub schema_version: u32,
    pub flag: bool,
    pub code: i32,
    pub message: String,
    pub has_fatal: bool,
    pub count: usize,
    pub log: Vec<ErrorEntryValuesPayload>,
    pub dims: PayloadDims,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorState {
    pub flag: bool,
    pub code: i32,
    pub message: String,
    pub has_fatal: bool,
    pub count: usize,
    pub log: Vec<ErrorEntry>,
}

impl Default for ErrorState {
    fn default() -> Self {
        Self {
            flag: false,
            code: 0,
            message: String::new(),
            has_fatal: false,
            count: 0,
            log: Vec::new(),
        }
    }
}

fn decode_c_text(buffer: &[c_char]) -> String {
    let bytes: Vec<u8> = buffer.iter().map(|&ch| ch as u8).collect();
    let nul_idx = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    let raw = String::from_utf8_lossy(&bytes[..nul_idx]).into_owned();
    raw.trim_end_matches(' ').to_string()
}

pub fn error_state_schema() -> Result<usize, BridgeError> {
    let mut message_len = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_error_state_len(&mut message_len as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || message_len < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(message_len as usize)
}

pub fn error_state_schema_info() -> Result<ErrorStateSchema, BridgeError> {
    let message_len = error_state_schema()?;
    let schema_version_runtime = error_state_schema_version_runtime()?;

    if schema_version_runtime != ERROR_STATE_SCHEMA_VERSION
        || message_len != ERROR_STATE_MESSAGE_LEN
    {
        return Err(BridgeError::BadState);
    }

    let mut allocatable_dims = PayloadDims::new();
    allocatable_dims.insert(ERROR_STATE_LOG_FIELD.to_string(), vec![0]);

    Ok(ErrorStateSchema {
        schema_version: ERROR_STATE_SCHEMA_VERSION,
        message_len,
        field_names: error_state_field_names(),
        allocatable_dims,
        entry_schema: error_entry_schema_info()?,
    })
}

pub fn error_state_field_names() -> Vec<String> {
    vec![
        "flag".to_string(),
        "code".to_string(),
        "message".to_string(),
        "has_fatal".to_string(),
        "count".to_string(),
        ERROR_STATE_LOG_FIELD.to_string(),
    ]
}

pub fn error_state_schema_version() -> u32 {
    ERROR_STATE_SCHEMA_VERSION
}

pub fn error_state_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_error_state_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn error_state_to_values_payload(state: &ErrorState) -> ErrorStateValuesPayload {
    let mut dims = PayloadDims::new();
    dims.insert(ERROR_STATE_LOG_FIELD.to_string(), vec![state.log.len()]);

    ErrorStateValuesPayload {
        schema_version: ERROR_STATE_SCHEMA_VERSION,
        flag: state.flag,
        code: state.code,
        message: state.message.clone(),
        has_fatal: state.has_fatal,
        count: state.count,
        log: state
            .log
            .iter()
            .map(error_entry_to_values_payload)
            .collect(),
        dims,
    }
}

pub fn error_state_from_values_payload(
    payload: &ErrorStateValuesPayload,
) -> Result<ErrorState, BridgeError> {
    if payload.schema_version != ERROR_STATE_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    let message_len = error_state_schema()?;
    validate_text_field(&payload.message, message_len)?;

    if payload.dims.len() != 1 {
        return Err(BridgeError::BadState);
    }

    let log_dims = require_field_dims(&payload.dims, ERROR_STATE_LOG_FIELD, 1)?;
    let log_len = log_dims[0];

    if log_len != payload.log.len() || payload.count != log_len {
        return Err(BridgeError::BadState);
    }

    let mut log = Vec::with_capacity(log_len);
    for entry_payload in &payload.log {
        log.push(error_entry_from_values_payload(entry_payload)?);
    }

    let any_fatal = log.iter().any(|entry| entry.is_fatal);
    if payload.has_fatal != any_fatal {
        return Err(BridgeError::BadState);
    }

    Ok(ErrorState {
        flag: payload.flag,
        code: payload.code,
        message: payload.message.clone(),
        has_fatal: payload.has_fatal,
        count: payload.count,
        log,
    })
}

pub fn error_state_default_from_fortran() -> Result<ErrorState, BridgeError> {
    let message_len = error_state_schema()?;
    let message_buf_len = message_len.checked_add(1).ok_or(BridgeError::BadState)?;
    let message_c_len = i32::try_from(message_buf_len).map_err(|_| BridgeError::BadState)?;

    let mut flag = 0_i32;
    let mut code = 0_i32;
    let mut message_buf = vec![0 as c_char; message_buf_len];
    let mut has_fatal = 0_i32;
    let mut count = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_error_state_default(
            &mut flag as *mut i32,
            &mut code as *mut i32,
            message_buf.as_mut_ptr(),
            message_c_len,
            &mut has_fatal as *mut i32,
            &mut count as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }
    if count < 0 {
        return Err(BridgeError::BadState);
    }

    let message = decode_c_text(&message_buf);
    validate_text_field(&message, message_len)?;

    let count_usize = count as usize;
    if count_usize != 0 {
        return Err(BridgeError::BadState);
    }

    Ok(ErrorState {
        flag: flag >= 1,
        code,
        message,
        has_fatal: has_fatal >= 1,
        count: count_usize,
        log: Vec::new(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error_entry::{error_entry_default_from_fortran, error_entry_to_values_payload};

    #[test]
    fn schema_matches_expected_dimensions() {
        let schema = error_state_schema_info().expect("schema info should be available");
        assert_eq!(schema.message_len, ERROR_STATE_MESSAGE_LEN);

        let dims = schema
            .allocatable_dims
            .get(ERROR_STATE_LOG_FIELD)
            .expect("log dims should be present");
        assert_eq!(dims, &vec![0]);
    }

    #[test]
    fn default_state_has_empty_log() {
        let state = error_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, ErrorState::default());

        let payload = error_state_to_values_payload(&state);
        assert_eq!(payload.count, 0);
        assert!(payload.log.is_empty());
        assert_eq!(payload.dims.get(ERROR_STATE_LOG_FIELD), Some(&vec![0]));
    }

    #[test]
    fn values_payload_roundtrip_with_log_entries() {
        let mut entry = error_entry_to_values_payload(
            &error_entry_default_from_fortran().expect("default entry should be available"),
        );
        entry.message = "fatal path".to_string();
        entry.location = "module_demo".to_string();
        entry.is_fatal = true;

        let payload = ErrorStateValuesPayload {
            schema_version: ERROR_STATE_SCHEMA_VERSION,
            flag: true,
            code: 101,
            message: "runtime failure".to_string(),
            has_fatal: true,
            count: 1,
            log: vec![entry],
            dims: PayloadDims::from([(ERROR_STATE_LOG_FIELD.to_string(), vec![1])]),
        };

        let state = error_state_from_values_payload(&payload).expect("payload decode should work");
        let encoded = error_state_to_values_payload(&state);

        assert_eq!(encoded.schema_version, ERROR_STATE_SCHEMA_VERSION);
        assert_eq!(encoded.flag, payload.flag);
        assert_eq!(encoded.code, payload.code);
        assert_eq!(encoded.message, payload.message);
        assert_eq!(encoded.has_fatal, payload.has_fatal);
        assert_eq!(encoded.count, payload.count);
        assert_eq!(encoded.log, payload.log);
        assert_eq!(encoded.dims, payload.dims);
    }

    #[test]
    fn values_payload_rejects_count_mismatch() {
        let payload = ErrorStateValuesPayload {
            schema_version: ERROR_STATE_SCHEMA_VERSION,
            flag: false,
            code: 0,
            message: String::new(),
            has_fatal: false,
            count: 1,
            log: Vec::new(),
            dims: PayloadDims::from([(ERROR_STATE_LOG_FIELD.to_string(), vec![0])]),
        };

        let err =
            error_state_from_values_payload(&payload).expect_err("count mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_log_dims_mismatch() {
        let payload = ErrorStateValuesPayload {
            schema_version: ERROR_STATE_SCHEMA_VERSION,
            flag: false,
            code: 0,
            message: String::new(),
            has_fatal: false,
            count: 0,
            log: Vec::new(),
            dims: PayloadDims::from([(ERROR_STATE_LOG_FIELD.to_string(), vec![2])]),
        };

        let err = error_state_from_values_payload(&payload).expect_err("dims mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_overlong_message() {
        let payload = ErrorStateValuesPayload {
            schema_version: ERROR_STATE_SCHEMA_VERSION,
            flag: false,
            code: 0,
            message: "x".repeat(ERROR_STATE_MESSAGE_LEN + 1),
            has_fatal: false,
            count: 0,
            log: Vec::new(),
            dims: PayloadDims::from([(ERROR_STATE_LOG_FIELD.to_string(), vec![0])]),
        };

        let err =
            error_state_from_values_payload(&payload).expect_err("overlong message should fail");
        assert_eq!(err, BridgeError::BadBuffer);
    }
}
