//! Restart envelope: complete model state plus elapsed timer, not cold-start
//! configuration. A missing member or incompatible version must not fall back
//! to a freshly initialised state (`BR-06`). Timer validation here is
//! complemented by timestep compatibility checks in `sim` and nested 13-member
//! state checks in `suews_state`; preserve all three when changing restart
//! representation.
//!
//! On the Python side, `src/supy/suews_checkpoint.py::SUEWSCheckpoint` wraps
//! one JSON envelope string per grid ID and mirrors
//! [`SUEWS_CHECKPOINT_SCHEMA_VERSION`]. Float deserialisation relies on
//! `serde_json`'s `float_roundtrip` feature so chunked/restarted runs stay
//! bit-exact across JSON boundaries.

use crate::error::BridgeError;
use crate::suews_state::{
    suews_state_from_nested_payload, suews_state_to_checkpoint_value, SuewsState,
};
use crate::timer::SuewsTimer;
use serde_json::{json, Map, Value};

/// Current JSON envelope schema version carrying both elapsed timer metadata
/// and the complete nested `SuewsState` payload.
///
/// Version 3 (gh#2333) moved the DyOHM day latch (`new_day`,
/// `dt_since_start_prev`) from the envelope timer into `state.ohm_state`.
/// Version-2 envelopes are rejected, not migrated: their `ohm_state` member
/// also carries the older OHM_STATE schema.
pub const SUEWS_CHECKPOINT_SCHEMA_VERSION: u32 = 3;

/// Elapsed timing metadata carried alongside model state across restart
/// boundaries.
///
/// Mirrors the continuation fields of [`SuewsTimer`] needed to resume a
/// multi-segment run. Rolling averages and the DyOHM day-boundary latch are
/// model state and travel in `state.ohm_state`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CheckpointTimer {
    /// Elapsed simulation seconds since the original run started (`>= 0`).
    pub dt_since_start: i32,
    /// Model timestep duration in seconds (`> 0`).
    pub tstep: i32,
}

/// Decoded restart checkpoint containing the validated model state and timer.
#[derive(Debug, Clone, PartialEq)]
pub struct SuewsCheckpointData {
    /// Complete 13-member SUEWS model state snapshot.
    pub state: SuewsState,
    /// Validated elapsed timer metadata from the preceding segment.
    pub timer: CheckpointTimer,
}

fn invalid_envelope(message: impl Into<String>) -> BridgeError {
    BridgeError::InvalidCheckpointEnvelope {
        message: message.into(),
    }
}

/// Read a required JSON integer field from the `timer` object and verify it
/// fits in `i32`.
fn parse_i32_field(object: &Map<String, Value>, name: &str) -> Result<i32, BridgeError> {
    let value = object
        .get(name)
        .and_then(Value::as_i64)
        .ok_or_else(|| invalid_envelope(format!("timer.{name} must be an integer")))?;
    i32::try_from(value)
        .map_err(|_| invalid_envelope(format!("timer.{name} is outside the i32 range")))
}

/// Enforce domain invariants on elapsed timer fields before serialising or
/// after parsing a checkpoint envelope.
fn validate_timer(timer: CheckpointTimer) -> Result<CheckpointTimer, BridgeError> {
    if timer.dt_since_start < 0 {
        return Err(invalid_envelope(
            "timer.dt_since_start must be non-negative",
        ));
    }
    if timer.tstep <= 0 {
        return Err(invalid_envelope("timer.tstep must be positive"));
    }
    Ok(timer)
}

/// Check the DyOHM running-mean clock against the elapsed timer. The
/// `OhmState` codec has already checked `new_day` is `0`/`1` and
/// `dt_since_start_prev` is non-negative.
fn validate_ohm_clock(state: &SuewsState, timer: CheckpointTimer) -> Result<(), BridgeError> {
    if state.ohm_state.dt_since_start_prev > timer.dt_since_start {
        return Err(invalid_envelope(
            "state.ohm_state.dt_since_start_prev cannot exceed timer.dt_since_start",
        ));
    }
    Ok(())
}

/// Serialise a post-run model `state` and `timer` into a schema-version-3 JSON
/// checkpoint envelope.
///
/// Inactive `NaN` state entries are encoded as JSON `null`; `+/-Inf` values
/// fail with [`BridgeError::NonFiniteCheckpointValue`].
pub fn suews_checkpoint_to_json(
    state: &SuewsState,
    timer: &SuewsTimer,
) -> Result<String, BridgeError> {
    let checkpoint_timer = validate_timer(CheckpointTimer {
        dt_since_start: timer.dt_since_start,
        tstep: timer.tstep,
    })?;
    validate_ohm_clock(state, checkpoint_timer)?;
    let state_value = suews_state_to_checkpoint_value(state)?;
    let checkpoint = json!({
        "checkpoint_schema_version": SUEWS_CHECKPOINT_SCHEMA_VERSION,
        "timer": {
            "dt_since_start": checkpoint_timer.dt_since_start,
            "tstep": checkpoint_timer.tstep,
        },
        "state": state_value,
    });

    serde_json::to_string(&checkpoint).map_err(|error| BridgeError::CheckpointSerialization {
        message: error.to_string(),
    })
}

/// Deserialise and validate a schema-version-3 JSON checkpoint envelope.
///
/// Rejects legacy version-1 state-only payloads with
/// [`BridgeError::LegacyCheckpointMissingTimer`], unknown schema versions with
/// [`BridgeError::UnsupportedCheckpointVersion`], and malformed timers or
/// incomplete state member sets before returning [`SuewsCheckpointData`].
pub fn suews_checkpoint_from_json(json_text: &str) -> Result<SuewsCheckpointData, BridgeError> {
    let checkpoint: Value = serde_json::from_str(json_text)
        .map_err(|error| invalid_envelope(format!("invalid JSON: {error}")))?;
    let object = checkpoint
        .as_object()
        .ok_or_else(|| invalid_envelope("top-level value must be an object"))?;

    if !object.contains_key("checkpoint_schema_version")
        && object.contains_key("schema_version")
        && object.contains_key("members")
    {
        return Err(BridgeError::LegacyCheckpointMissingTimer);
    }

    let version = object
        .get("checkpoint_schema_version")
        .and_then(Value::as_u64)
        .ok_or_else(|| invalid_envelope("checkpoint_schema_version must be an integer"))?;
    if version != u64::from(SUEWS_CHECKPOINT_SCHEMA_VERSION) {
        return Err(BridgeError::UnsupportedCheckpointVersion {
            found: version,
            expected: SUEWS_CHECKPOINT_SCHEMA_VERSION,
        });
    }

    let timer_object = object
        .get("timer")
        .and_then(Value::as_object)
        .ok_or_else(|| invalid_envelope("timer must be an object"))?;
    let timer = validate_timer(CheckpointTimer {
        dt_since_start: parse_i32_field(timer_object, "dt_since_start")?,
        tstep: parse_i32_field(timer_object, "tstep")?,
    })?;

    let state_value = object
        .get("state")
        .ok_or_else(|| invalid_envelope("state is required"))?;
    let state = suews_state_from_nested_payload(state_value)?;
    validate_ohm_clock(&state, timer)?;

    Ok(SuewsCheckpointData { state, timer })
}

// contract: C-RESTART, C-SEQUENCE
#[cfg(test)]
mod tests {
    use super::*;
    use crate::suews_state::{suews_state_default_from_fortran, suews_state_to_nested_payload};

    #[test]
    fn checkpoint_roundtrip_preserves_elapsed_timer() {
        let mut state =
            suews_state_default_from_fortran().expect("default state should be available");
        state.ohm_state.new_day = 1;
        state.ohm_state.dt_since_start_prev = 90_000;
        let timer = SuewsTimer {
            dt_since_start: 90_300,
            tstep: 300,
            ..SuewsTimer::default()
        };

        let checkpoint_json =
            suews_checkpoint_to_json(&state, &timer).expect("checkpoint should serialise");
        let restored =
            suews_checkpoint_from_json(&checkpoint_json).expect("checkpoint should restore");

        assert_eq!(restored.state, state);
        assert_eq!(
            restored.timer,
            CheckpointTimer {
                dt_since_start: 90_300,
                tstep: 300,
            }
        );
        assert_eq!(restored.state.ohm_state.new_day, 1);
        assert_eq!(restored.state.ohm_state.dt_since_start_prev, 90_000);
    }

    #[test]
    fn checkpoint_rejects_ohm_clock_ahead_of_timer() {
        let mut state =
            suews_state_default_from_fortran().expect("default state should be available");
        state.ohm_state.dt_since_start_prev = 90_600;
        let timer = SuewsTimer {
            dt_since_start: 90_300,
            tstep: 300,
            ..SuewsTimer::default()
        };
        let error = suews_checkpoint_to_json(&state, &timer)
            .expect_err("an OHM clock ahead of the timer should be rejected");
        assert!(matches!(
            error,
            BridgeError::InvalidCheckpointEnvelope { .. }
        ));
    }

    #[test]
    fn version_two_checkpoint_is_rejected() {
        let error = suews_checkpoint_from_json(r#"{"checkpoint_schema_version":2}"#)
            .expect_err("version-2 checkpoints predate gh#2333 and should be rejected");
        assert_eq!(
            error,
            BridgeError::UnsupportedCheckpointVersion {
                found: 2,
                expected: SUEWS_CHECKPOINT_SCHEMA_VERSION,
            }
        );
    }

    #[test]
    fn checkpoint_json_parses_floats_exactly() {
        // serde_json's default float parser can return a value one ulp away
        // from the decimal it reads; the `float_roundtrip` feature makes it
        // exact. Without it, every state carried across a chunk or checkpoint
        // boundary picked up those ulps and chunked runs drifted from an
        // uninterrupted run (gh#1849). These are the stored water and soil
        // values that first moved in the sample run.
        for text in [
            "0.043194798455574876",
            "120.00361625549799",
            "120.00382255930185",
        ] {
            let parsed: f64 = serde_json::from_str(text).expect("JSON number should parse");
            let exact: f64 = text.parse().expect("decimal should parse");
            assert_eq!(
                parsed.to_bits(),
                exact.to_bits(),
                "{text} did not round-trip"
            );
        }
    }

    #[test]
    fn legacy_state_payload_reports_missing_timer() {
        let state = suews_state_default_from_fortran().expect("default state should be available");
        let legacy_json = serde_json::to_string(&suews_state_to_nested_payload(&state))
            .expect("legacy state should serialise");

        let error = suews_checkpoint_from_json(&legacy_json)
            .expect_err("legacy checkpoints should require timer metadata");
        assert_eq!(error, BridgeError::LegacyCheckpointMissingTimer);
    }

    #[test]
    fn unsupported_checkpoint_version_is_actionable() {
        let error = suews_checkpoint_from_json(r#"{"checkpoint_schema_version":4}"#)
            .expect_err("future checkpoint versions should be rejected");
        assert_eq!(
            error,
            BridgeError::UnsupportedCheckpointVersion {
                found: 4,
                expected: SUEWS_CHECKPOINT_SCHEMA_VERSION,
            }
        );
    }
}
