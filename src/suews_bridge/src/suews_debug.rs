use crate::codec::CompositeCodec;
use crate::error::BridgeError;
use crate::suews_state::{
    suews_state_default_from_fortran, suews_state_from_nested_payload, suews_state_schema_version,
    suews_state_schema_version_runtime, suews_state_to_nested_payload, SuewsState,
};
use serde_json::{Map, Value};
use std::collections::BTreeMap;

pub const SUEWS_DEBUG_SCHEMA_VERSION: u32 = 1;

const MEMBER_STATE_01: &str = "state_01_dailystate";
const MEMBER_STATE_02: &str = "state_02_soilmoist";
const MEMBER_STATE_03: &str = "state_03_wateruse";
const MEMBER_STATE_04: &str = "state_04_anthroemis";
const MEMBER_STATE_05: &str = "state_05_qn";
const MEMBER_STATE_06: &str = "state_06_qs";
const MEMBER_STATE_07: &str = "state_07_qhqe_lumps";
const MEMBER_STATE_08: &str = "state_08_water";
const MEMBER_STATE_09: &str = "state_09_resist";
const MEMBER_STATE_10: &str = "state_10_qe";
const MEMBER_STATE_11: &str = "state_11_qh";
const MEMBER_STATE_12: &str = "state_12_tsurf";
const MEMBER_STATE_13: &str = "state_13_rsl";
const MEMBER_STATE_14: &str = "state_14_biogenco2";
const MEMBER_STATE_15: &str = "state_15_beers";

const SUEWS_DEBUG_MEMBER_ORDER: [&str; 15] = [
    MEMBER_STATE_01,
    MEMBER_STATE_02,
    MEMBER_STATE_03,
    MEMBER_STATE_04,
    MEMBER_STATE_05,
    MEMBER_STATE_06,
    MEMBER_STATE_07,
    MEMBER_STATE_08,
    MEMBER_STATE_09,
    MEMBER_STATE_10,
    MEMBER_STATE_11,
    MEMBER_STATE_12,
    MEMBER_STATE_13,
    MEMBER_STATE_14,
    MEMBER_STATE_15,
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuewsDebugSchema {
    pub schema_version: u32,
    pub member_names: Vec<String>,
    pub suews_state_schema_version: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SuewsDebugValuesPayload {
    pub schema_version: u32,
    pub members: BTreeMap<String, Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SuewsDebug {
    pub state_01_dailystate: SuewsState,
    pub state_02_soilmoist: SuewsState,
    pub state_03_wateruse: SuewsState,
    pub state_04_anthroemis: SuewsState,
    pub state_05_qn: SuewsState,
    pub state_06_qs: SuewsState,
    pub state_07_qhqe_lumps: SuewsState,
    pub state_08_water: SuewsState,
    pub state_09_resist: SuewsState,
    pub state_10_qe: SuewsState,
    pub state_11_qh: SuewsState,
    pub state_12_tsurf: SuewsState,
    pub state_13_rsl: SuewsState,
    pub state_14_biogenco2: SuewsState,
    pub state_15_beers: SuewsState,
}

fn is_known_member(name: &str) -> bool {
    SUEWS_DEBUG_MEMBER_ORDER.contains(&name)
}

fn validate_member_set(members: &BTreeMap<String, Value>) -> Result<(), BridgeError> {
    if members.len() != SUEWS_DEBUG_MEMBER_ORDER.len() {
        return Err(BridgeError::BadState);
    }

    for member in members.keys() {
        if !is_known_member(member) {
            return Err(BridgeError::BadState);
        }
    }

    for member in SUEWS_DEBUG_MEMBER_ORDER {
        if !members.contains_key(member) {
            return Err(BridgeError::BadState);
        }
    }

    Ok(())
}

fn payload_to_nested_value(payload: &SuewsDebugValuesPayload) -> Value {
    let mut root = Map::new();
    root.insert(
        "schema_version".to_string(),
        Value::from(payload.schema_version as u64),
    );

    let mut members = Map::new();
    for member in SUEWS_DEBUG_MEMBER_ORDER {
        if let Some(value) = payload.members.get(member) {
            members.insert(member.to_string(), value.clone());
        }
    }
    root.insert("members".to_string(), Value::Object(members));

    Value::Object(root)
}

fn payload_from_nested_value(value: &Value) -> Result<SuewsDebugValuesPayload, BridgeError> {
    let root = value.as_object().ok_or(BridgeError::BadState)?;
    if root.len() != 2 || !root.contains_key("schema_version") || !root.contains_key("members") {
        return Err(BridgeError::BadState);
    }

    let schema_version = root
        .get("schema_version")
        .and_then(Value::as_u64)
        .ok_or(BridgeError::BadState)
        .and_then(|v| u32::try_from(v).map_err(|_| BridgeError::BadState))?;

    let members_obj = root
        .get("members")
        .and_then(Value::as_object)
        .ok_or(BridgeError::BadState)?;

    if members_obj.len() != SUEWS_DEBUG_MEMBER_ORDER.len() {
        return Err(BridgeError::BadState);
    }

    let mut members = BTreeMap::new();
    for (member, payload) in members_obj {
        if !is_known_member(member) {
            return Err(BridgeError::BadState);
        }
        members.insert(member.clone(), payload.clone());
    }

    Ok(SuewsDebugValuesPayload {
        schema_version,
        members,
    })
}

impl CompositeCodec for SuewsDebug {
    fn to_nested_payload(&self) -> Value {
        suews_debug_to_nested_payload(self)
    }

    fn from_nested_payload(payload: &Value) -> Result<Self, BridgeError> {
        suews_debug_from_nested_payload(payload)
    }
}

pub fn suews_debug_schema_version() -> u32 {
    SUEWS_DEBUG_SCHEMA_VERSION
}

pub fn suews_debug_schema_version_runtime() -> Result<u32, BridgeError> {
    if suews_state_schema_version_runtime()? != suews_state_schema_version() {
        return Err(BridgeError::BadState);
    }

    Ok(SUEWS_DEBUG_SCHEMA_VERSION)
}

pub fn suews_debug_member_names() -> Vec<String> {
    SUEWS_DEBUG_MEMBER_ORDER
        .iter()
        .map(|member| (*member).to_string())
        .collect()
}

pub fn suews_debug_schema_info() -> Result<SuewsDebugSchema, BridgeError> {
    if suews_debug_schema_version_runtime()? != SUEWS_DEBUG_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    Ok(SuewsDebugSchema {
        schema_version: SUEWS_DEBUG_SCHEMA_VERSION,
        member_names: suews_debug_member_names(),
        suews_state_schema_version: suews_state_schema_version(),
    })
}

pub fn suews_debug_default_from_fortran() -> Result<SuewsDebug, BridgeError> {
    Ok(SuewsDebug {
        state_01_dailystate: suews_state_default_from_fortran()?,
        state_02_soilmoist: suews_state_default_from_fortran()?,
        state_03_wateruse: suews_state_default_from_fortran()?,
        state_04_anthroemis: suews_state_default_from_fortran()?,
        state_05_qn: suews_state_default_from_fortran()?,
        state_06_qs: suews_state_default_from_fortran()?,
        state_07_qhqe_lumps: suews_state_default_from_fortran()?,
        state_08_water: suews_state_default_from_fortran()?,
        state_09_resist: suews_state_default_from_fortran()?,
        state_10_qe: suews_state_default_from_fortran()?,
        state_11_qh: suews_state_default_from_fortran()?,
        state_12_tsurf: suews_state_default_from_fortran()?,
        state_13_rsl: suews_state_default_from_fortran()?,
        state_14_biogenco2: suews_state_default_from_fortran()?,
        state_15_beers: suews_state_default_from_fortran()?,
    })
}

pub fn suews_debug_to_values_payload(state: &SuewsDebug) -> SuewsDebugValuesPayload {
    let mut members = BTreeMap::new();

    members.insert(
        MEMBER_STATE_01.to_string(),
        suews_state_to_nested_payload(&state.state_01_dailystate),
    );
    members.insert(
        MEMBER_STATE_02.to_string(),
        suews_state_to_nested_payload(&state.state_02_soilmoist),
    );
    members.insert(
        MEMBER_STATE_03.to_string(),
        suews_state_to_nested_payload(&state.state_03_wateruse),
    );
    members.insert(
        MEMBER_STATE_04.to_string(),
        suews_state_to_nested_payload(&state.state_04_anthroemis),
    );
    members.insert(
        MEMBER_STATE_05.to_string(),
        suews_state_to_nested_payload(&state.state_05_qn),
    );
    members.insert(
        MEMBER_STATE_06.to_string(),
        suews_state_to_nested_payload(&state.state_06_qs),
    );
    members.insert(
        MEMBER_STATE_07.to_string(),
        suews_state_to_nested_payload(&state.state_07_qhqe_lumps),
    );
    members.insert(
        MEMBER_STATE_08.to_string(),
        suews_state_to_nested_payload(&state.state_08_water),
    );
    members.insert(
        MEMBER_STATE_09.to_string(),
        suews_state_to_nested_payload(&state.state_09_resist),
    );
    members.insert(
        MEMBER_STATE_10.to_string(),
        suews_state_to_nested_payload(&state.state_10_qe),
    );
    members.insert(
        MEMBER_STATE_11.to_string(),
        suews_state_to_nested_payload(&state.state_11_qh),
    );
    members.insert(
        MEMBER_STATE_12.to_string(),
        suews_state_to_nested_payload(&state.state_12_tsurf),
    );
    members.insert(
        MEMBER_STATE_13.to_string(),
        suews_state_to_nested_payload(&state.state_13_rsl),
    );
    members.insert(
        MEMBER_STATE_14.to_string(),
        suews_state_to_nested_payload(&state.state_14_biogenco2),
    );
    members.insert(
        MEMBER_STATE_15.to_string(),
        suews_state_to_nested_payload(&state.state_15_beers),
    );

    SuewsDebugValuesPayload {
        schema_version: SUEWS_DEBUG_SCHEMA_VERSION,
        members,
    }
}

pub fn suews_debug_from_values_payload(
    payload: &SuewsDebugValuesPayload,
) -> Result<SuewsDebug, BridgeError> {
    if payload.schema_version != SUEWS_DEBUG_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    validate_member_set(&payload.members)?;

    Ok(SuewsDebug {
        state_01_dailystate: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_01)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_02_soilmoist: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_02)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_03_wateruse: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_03)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_04_anthroemis: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_04)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_05_qn: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_05)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_06_qs: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_06)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_07_qhqe_lumps: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_07)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_08_water: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_08)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_09_resist: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_09)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_10_qe: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_10)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_11_qh: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_11)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_12_tsurf: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_12)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_13_rsl: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_13)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_14_biogenco2: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_14)
                .ok_or(BridgeError::BadState)?,
        )?,
        state_15_beers: suews_state_from_nested_payload(
            payload
                .members
                .get(MEMBER_STATE_15)
                .ok_or(BridgeError::BadState)?,
        )?,
    })
}

pub fn suews_debug_to_nested_payload(state: &SuewsDebug) -> Value {
    payload_to_nested_value(&suews_debug_to_values_payload(state))
}

pub fn suews_debug_from_nested_payload(payload: &Value) -> Result<SuewsDebug, BridgeError> {
    let values_payload = payload_from_nested_value(payload)?;
    suews_debug_from_values_payload(&values_payload)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_roundtrip_via_values_payload() {
        let state = suews_debug_default_from_fortran().expect("default state should be available");
        let payload = suews_debug_to_values_payload(&state);
        let decoded =
            suews_debug_from_values_payload(&payload).expect("values payload decode should work");
        assert_eq!(decoded, state);
    }

    #[test]
    fn default_roundtrip_via_nested_payload() {
        let state = suews_debug_default_from_fortran().expect("default state should be available");
        let payload = suews_debug_to_nested_payload(&state);
        let decoded =
            suews_debug_from_nested_payload(&payload).expect("nested payload decode should work");
        assert_eq!(decoded, state);
    }

    #[test]
    fn values_payload_rejects_unknown_member() {
        let mut payload = suews_debug_to_values_payload(
            &suews_debug_default_from_fortran().expect("default state should be available"),
        );
        payload
            .members
            .insert("state_99_extra".to_string(), Value::Null);

        let err =
            suews_debug_from_values_payload(&payload).expect_err("unknown member should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn member_names_are_deterministic() {
        let names = suews_debug_member_names();
        assert_eq!(names.first().map(String::as_str), Some(MEMBER_STATE_01));
        assert_eq!(names.last().map(String::as_str), Some(MEMBER_STATE_15));
        assert_eq!(names.len(), SUEWS_DEBUG_MEMBER_ORDER.len());
    }
}
