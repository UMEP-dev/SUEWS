use crate::ffi;
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum BridgeError {
    #[error("invalid timestep: dt must be positive")]
    BadDt,
    #[error("invalid time: dt_since_start must be non-negative")]
    BadTime,
    #[error("invalid C buffer")]
    BadBuffer,
    #[error("invalid state payload")]
    BadState,
    #[error("checkpoint state contains {value_kind} at {path}")]
    NonFiniteCheckpointValue { path: String, value_kind: String },
    #[error("invalid checkpoint state value at {path}: expected a JSON number or null NaN marker, found {found}")]
    InvalidCheckpointValue { path: String, found: String },
    #[error("failed to serialise checkpoint state: {message}")]
    CheckpointSerialization { message: String },
    #[error(
        "legacy checkpoint schema version 1 has no elapsed timer metadata; rerun the preceding segment with checkpoint schema version 2"
    )]
    LegacyCheckpointMissingTimer,
    #[error("unsupported checkpoint schema version {found}; expected version {expected}")]
    UnsupportedCheckpointVersion { found: u64, expected: u32 },
    #[error("invalid checkpoint envelope: {message}")]
    InvalidCheckpointEnvelope { message: String },
    #[error("simulation failed (code {code}): {message}")]
    SimulationError { code: i32, message: String },
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
