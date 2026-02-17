use crate::codec::validate_flat_len;
use crate::error::BridgeError;
use crate::ffi;
use crate::timer::{
    suews_timer_field_names, suews_timer_from_ordered_values, suews_timer_schema,
    suews_timer_schema_version_runtime, suews_timer_to_ordered_values, SuewsTimer,
    SUEWS_TIMER_SCHEMA_VERSION,
};
use std::os::raw::c_char;

pub const ERROR_ENTRY_SCHEMA_VERSION: u32 = 1;
pub const ERROR_ENTRY_MESSAGE_LEN: usize = 256;
pub const ERROR_ENTRY_LOCATION_LEN: usize = 64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorEntrySchema {
    pub schema_version: u32,
    pub timer_flat_len: usize,
    pub message_len: usize,
    pub location_len: usize,
    pub field_names: Vec<String>,
    pub timer_field_names: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorEntryValuesPayload {
    pub schema_version: u32,
    pub timer_values: Vec<f64>,
    pub message: String,
    pub location: String,
    pub is_fatal: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorEntry {
    pub timer: SuewsTimer,
    pub message: String,
    pub location: String,
    pub is_fatal: bool,
}

impl Default for ErrorEntry {
    fn default() -> Self {
        Self {
            timer: SuewsTimer::default(),
            message: String::new(),
            location: String::new(),
            is_fatal: false,
        }
    }
}

pub(crate) fn validate_text_field(value: &str, max_len: usize) -> Result<(), BridgeError> {
    if value.as_bytes().contains(&0) {
        return Err(BridgeError::BadState);
    }
    if value.len() > max_len {
        return Err(BridgeError::BadBuffer);
    }
    Ok(())
}

fn decode_c_text(buffer: &[c_char]) -> String {
    let bytes: Vec<u8> = buffer.iter().map(|&ch| ch as u8).collect();
    let nul_idx = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    let raw = String::from_utf8_lossy(&bytes[..nul_idx]).into_owned();
    raw.trim_end_matches(' ').to_string()
}

pub fn error_entry_schema() -> Result<(usize, usize, usize), BridgeError> {
    let mut timer_flat_len = -1_i32;
    let mut message_len = -1_i32;
    let mut location_len = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_error_entry_len(
            &mut timer_flat_len as *mut i32,
            &mut message_len as *mut i32,
            &mut location_len as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || timer_flat_len < 0 || message_len < 0 || location_len < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok((
        timer_flat_len as usize,
        message_len as usize,
        location_len as usize,
    ))
}

pub fn error_entry_schema_info() -> Result<ErrorEntrySchema, BridgeError> {
    let (timer_flat_len, message_len, location_len) = error_entry_schema()?;
    let schema_version_runtime = error_entry_schema_version_runtime()?;
    let timer_flat_len_runtime = suews_timer_schema()?;
    let timer_schema_runtime = suews_timer_schema_version_runtime()?;

    if schema_version_runtime != ERROR_ENTRY_SCHEMA_VERSION
        || timer_schema_runtime != SUEWS_TIMER_SCHEMA_VERSION
        || timer_flat_len != timer_flat_len_runtime
        || message_len != ERROR_ENTRY_MESSAGE_LEN
        || location_len != ERROR_ENTRY_LOCATION_LEN
    {
        return Err(BridgeError::BadState);
    }

    Ok(ErrorEntrySchema {
        schema_version: ERROR_ENTRY_SCHEMA_VERSION,
        timer_flat_len,
        message_len,
        location_len,
        field_names: error_entry_field_names(),
        timer_field_names: suews_timer_field_names(),
    })
}

pub fn error_entry_field_names() -> Vec<String> {
    vec![
        "timer".to_string(),
        "message".to_string(),
        "location".to_string(),
        "is_fatal".to_string(),
    ]
}

pub fn error_entry_schema_version() -> u32 {
    ERROR_ENTRY_SCHEMA_VERSION
}

pub fn error_entry_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_error_entry_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn error_entry_to_values_payload(state: &ErrorEntry) -> ErrorEntryValuesPayload {
    ErrorEntryValuesPayload {
        schema_version: ERROR_ENTRY_SCHEMA_VERSION,
        timer_values: suews_timer_to_ordered_values(&state.timer),
        message: state.message.clone(),
        location: state.location.clone(),
        is_fatal: state.is_fatal,
    }
}

pub fn error_entry_from_values_payload(
    payload: &ErrorEntryValuesPayload,
) -> Result<ErrorEntry, BridgeError> {
    if payload.schema_version != ERROR_ENTRY_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    let (timer_flat_len, message_len, location_len) = error_entry_schema()?;
    validate_flat_len(&payload.timer_values, timer_flat_len)?;
    validate_text_field(&payload.message, message_len)?;
    validate_text_field(&payload.location, location_len)?;

    let timer = suews_timer_from_ordered_values(&payload.timer_values)?;

    Ok(ErrorEntry {
        timer,
        message: payload.message.clone(),
        location: payload.location.clone(),
        is_fatal: payload.is_fatal,
    })
}

pub fn error_entry_default_from_fortran() -> Result<ErrorEntry, BridgeError> {
    let (timer_flat_len, message_len, location_len) = error_entry_schema()?;

    let n_timer_flat = i32::try_from(timer_flat_len).map_err(|_| BridgeError::BadState)?;
    let message_buf_len = message_len.checked_add(1).ok_or(BridgeError::BadState)?;
    let location_buf_len = location_len.checked_add(1).ok_or(BridgeError::BadState)?;
    let message_c_len = i32::try_from(message_buf_len).map_err(|_| BridgeError::BadState)?;
    let location_c_len = i32::try_from(location_buf_len).map_err(|_| BridgeError::BadState)?;

    let mut timer_values = vec![0.0_f64; timer_flat_len];
    let mut message_buf = vec![0 as c_char; message_buf_len];
    let mut location_buf = vec![0 as c_char; location_buf_len];
    let mut is_fatal = 0_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_error_entry_default(
            timer_values.as_mut_ptr(),
            n_timer_flat,
            message_buf.as_mut_ptr(),
            message_c_len,
            location_buf.as_mut_ptr(),
            location_c_len,
            &mut is_fatal as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    let timer = suews_timer_from_ordered_values(&timer_values)?;
    let message = decode_c_text(&message_buf);
    let location = decode_c_text(&location_buf);

    validate_text_field(&message, message_len)?;
    validate_text_field(&location, location_len)?;

    Ok(ErrorEntry {
        timer,
        message,
        location,
        is_fatal: is_fatal >= 1,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let (timer_flat_len, message_len, location_len) =
            error_entry_schema().expect("schema call should succeed");

        assert_eq!(
            timer_flat_len,
            suews_timer_schema().expect("timer schema should succeed")
        );
        assert_eq!(message_len, ERROR_ENTRY_MESSAGE_LEN);
        assert_eq!(location_len, ERROR_ENTRY_LOCATION_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = error_entry_default_from_fortran().expect("default state should be available");
        assert_eq!(state, ErrorEntry::default());

        let payload = error_entry_to_values_payload(&state);
        let recovered =
            error_entry_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let mut payload = error_entry_to_values_payload(&ErrorEntry::default());
        payload.message = "warn: demo payload".to_string();
        payload.location = "module_demo".to_string();
        payload.is_fatal = true;

        let state = error_entry_from_values_payload(&payload).expect("payload decode should work");
        let encoded = error_entry_to_values_payload(&state);
        assert_eq!(encoded, payload);

        let bad_payload = ErrorEntryValuesPayload {
            schema_version: ERROR_ENTRY_SCHEMA_VERSION + 1,
            ..payload
        };
        let err = error_entry_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_overlong_message() {
        let mut payload = error_entry_to_values_payload(&ErrorEntry::default());
        payload.message = "x".repeat(ERROR_ENTRY_MESSAGE_LEN + 1);

        let err =
            error_entry_from_values_payload(&payload).expect_err("overlong message should fail");
        assert_eq!(err, BridgeError::BadBuffer);
    }

    #[test]
    fn values_payload_rejects_nul_text() {
        let mut payload = error_entry_to_values_payload(&ErrorEntry::default());
        payload.location = "bad\0location".to_string();

        let err =
            error_entry_from_values_payload(&payload).expect_err("text containing NUL should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
