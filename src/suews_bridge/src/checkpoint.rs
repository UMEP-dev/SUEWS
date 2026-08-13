use crate::error::BridgeError;
use crate::suews_state::{
    suews_state_from_nested_payload, suews_state_to_checkpoint_value, SuewsState,
};
use crate::timer::SuewsTimer;
use serde_json::{json, Map, Value};

pub const SUEWS_CHECKPOINT_SCHEMA_VERSION: u32 = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CheckpointTimer {
    pub dt_since_start: i32,
    pub dt_since_start_prev: i32,
    pub tstep: i32,
    pub new_day: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SuewsCheckpointData {
    pub state: SuewsState,
    pub timer: CheckpointTimer,
}

fn invalid_envelope(message: impl Into<String>) -> BridgeError {
    BridgeError::InvalidCheckpointEnvelope {
        message: message.into(),
    }
}

fn parse_i32_field(object: &Map<String, Value>, name: &str) -> Result<i32, BridgeError> {
    let value = object
        .get(name)
        .and_then(Value::as_i64)
        .ok_or_else(|| invalid_envelope(format!("timer.{name} must be an integer")))?;
    i32::try_from(value)
        .map_err(|_| invalid_envelope(format!("timer.{name} is outside the i32 range")))
}

fn validate_timer(timer: CheckpointTimer) -> Result<CheckpointTimer, BridgeError> {
    if timer.dt_since_start < 0 {
        return Err(invalid_envelope(
            "timer.dt_since_start must be non-negative",
        ));
    }
    if timer.dt_since_start_prev < 0 {
        return Err(invalid_envelope(
            "timer.dt_since_start_prev must be non-negative",
        ));
    }
    if timer.dt_since_start_prev > timer.dt_since_start {
        return Err(invalid_envelope(
            "timer.dt_since_start_prev cannot exceed timer.dt_since_start",
        ));
    }
    if timer.tstep <= 0 {
        return Err(invalid_envelope("timer.tstep must be positive"));
    }
    if !matches!(timer.new_day, 0 | 1) {
        return Err(invalid_envelope("timer.new_day must be 0 or 1"));
    }
    Ok(timer)
}

pub fn suews_checkpoint_to_json(
    state: &SuewsState,
    timer: &SuewsTimer,
) -> Result<String, BridgeError> {
    let checkpoint_timer = validate_timer(CheckpointTimer {
        dt_since_start: timer.dt_since_start,
        dt_since_start_prev: timer.dt_since_start_prev,
        tstep: timer.tstep,
        new_day: timer.new_day,
    })?;
    let state_value = suews_state_to_checkpoint_value(state)?;
    let checkpoint = json!({
        "checkpoint_schema_version": SUEWS_CHECKPOINT_SCHEMA_VERSION,
        "timer": {
            "dt_since_start": checkpoint_timer.dt_since_start,
            "dt_since_start_prev": checkpoint_timer.dt_since_start_prev,
            "tstep": checkpoint_timer.tstep,
            "new_day": checkpoint_timer.new_day,
        },
        "state": state_value,
    });

    serde_json::to_string(&checkpoint).map_err(|error| BridgeError::CheckpointSerialization {
        message: error.to_string(),
    })
}

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
        dt_since_start_prev: parse_i32_field(timer_object, "dt_since_start_prev")?,
        tstep: parse_i32_field(timer_object, "tstep")?,
        new_day: parse_i32_field(timer_object, "new_day")?,
    })?;

    let state_value = object
        .get("state")
        .ok_or_else(|| invalid_envelope("state is required"))?;
    let state = suews_state_from_nested_payload(state_value)?;

    Ok(SuewsCheckpointData { state, timer })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::suews_state::{suews_state_default_from_fortran, suews_state_to_nested_payload};

    #[test]
    fn checkpoint_roundtrip_preserves_elapsed_timer() {
        let state = suews_state_default_from_fortran().expect("default state should be available");
        let timer = SuewsTimer {
            dt_since_start: 90_300,
            dt_since_start_prev: 90_000,
            tstep: 300,
            new_day: 1,
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
                dt_since_start_prev: 90_000,
                tstep: 300,
                new_day: 1,
            }
        );
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
        let error = suews_checkpoint_from_json(r#"{"checkpoint_schema_version":3}"#)
            .expect_err("future checkpoint versions should be rejected");
        assert_eq!(
            error,
            BridgeError::UnsupportedCheckpointVersion {
                found: 3,
                expected: SUEWS_CHECKPOINT_SCHEMA_VERSION,
            }
        );
    }
}
