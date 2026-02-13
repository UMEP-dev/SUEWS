use crate::ffi;
use thiserror::Error;

#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
pub enum BridgeError {
    #[error("invalid timestep: dt must be positive")]
    BadDt,
    #[error("invalid time: dt_since_start must be non-negative")]
    BadTime,
    #[error("invalid C buffer")]
    BadBuffer,
    #[error("invalid state payload")]
    BadState,
    #[error("unknown Fortran bridge error code: {0}")]
    Unknown(i32),
}

impl BridgeError {
    pub fn from_code(code: i32) -> Self {
        match code {
            ffi::SUEWS_CAPI_BAD_DT => Self::BadDt,
            ffi::SUEWS_CAPI_BAD_TIME => Self::BadTime,
            ffi::SUEWS_CAPI_BAD_BUFFER => Self::BadBuffer,
            ffi::SUEWS_CAPI_BAD_STATE => Self::BadState,
            other => Self::Unknown(other),
        }
    }
}
