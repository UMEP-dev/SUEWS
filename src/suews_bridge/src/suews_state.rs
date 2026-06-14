use crate::anthroemis::{
    anthroemis_state_default_from_fortran, anthroemis_state_from_values_payload,
    anthroemis_state_schema_version, anthroemis_state_schema_version_runtime,
    anthroemis_state_to_values_payload, AnthroEmisState,
};
use crate::atm::{
    atm_state_default_from_fortran, atm_state_from_values_payload, atm_state_schema_version,
    atm_state_schema_version_runtime, atm_state_to_values_payload, AtmState,
};
use crate::codec::{CompositeCodec, PayloadDims, ValuesPayload, ValuesPayloadWithDims};
use crate::core::{
    ohm_state_default_from_fortran, ohm_state_from_values_payload, ohm_state_schema_version,
    ohm_state_schema_version_runtime, ohm_state_to_values_payload, OhmState,
};
use crate::error::BridgeError;
use crate::error_entry::ErrorEntryValuesPayload;
use crate::error_state::{
    error_state_default_from_fortran, error_state_from_values_payload, error_state_schema_version,
    error_state_schema_version_runtime, error_state_to_values_payload, ErrorState,
    ErrorStateValuesPayload,
};
use crate::flag::{
    flag_state_default_from_fortran, flag_state_from_values_payload, flag_state_schema_version,
    flag_state_schema_version_runtime, flag_state_to_values_payload, FlagState,
};
use crate::heat_state::{
    heat_state_default_from_fortran, heat_state_from_values_payload, heat_state_schema_version,
    heat_state_schema_version_runtime, heat_state_to_values_payload, HeatState,
};
use crate::hydro_state::{
    hydro_state_default_from_fortran, hydro_state_from_values_payload, hydro_state_schema_version,
    hydro_state_schema_version_runtime, hydro_state_to_values_payload, HydroState,
};
use crate::nhood::{
    nhood_state_default_from_fortran, nhood_state_from_values_payload, nhood_state_schema_version,
    nhood_state_schema_version_runtime, nhood_state_to_values_payload, NhoodState,
};
use crate::phenology::{
    phenology_state_default_from_fortran, phenology_state_from_values_payload,
    phenology_state_schema_version, phenology_state_schema_version_runtime,
    phenology_state_to_values_payload, PhenologyState,
};
use crate::roughness::{
    roughness_state_default_from_fortran, roughness_state_from_values_payload,
    roughness_state_schema_version, roughness_state_schema_version_runtime,
    roughness_state_to_values_payload, RoughnessState,
};
use crate::snow::{
    snow_state_default_from_fortran, snow_state_from_values_payload, snow_state_schema_version,
    snow_state_schema_version_runtime, snow_state_to_values_payload, SnowState,
};
use crate::solar::{
    solar_state_default_from_fortran, solar_state_from_values_payload, solar_state_schema_version,
    solar_state_schema_version_runtime, solar_state_to_values_payload, SolarState,
};
use crate::stebbs_state::{
    stebbs_state_default_from_fortran, stebbs_state_from_values_payload,
    stebbs_state_schema_version, stebbs_state_schema_version_runtime,
    stebbs_state_to_values_payload, StebbsState,
};
use serde_json::{Map, Value};
use std::collections::BTreeMap;

pub const SUEWS_STATE_SCHEMA_VERSION: u32 = 1;

const MEMBER_ERROR_STATE: &str = "error_state";
const MEMBER_FLAG_STATE: &str = "flag_state";
const MEMBER_ANTHROEMIS_STATE: &str = "anthroemis_state";
const MEMBER_OHM_STATE: &str = "ohm_state";
const MEMBER_SOLAR_STATE: &str = "solar_state";
const MEMBER_ATM_STATE: &str = "atm_state";
const MEMBER_PHENOLOGY_STATE: &str = "phenology_state";
const MEMBER_SNOW_STATE: &str = "snow_state";
const MEMBER_HYDRO_STATE: &str = "hydro_state";
const MEMBER_HEAT_STATE: &str = "heat_state";
const MEMBER_ROUGHNESS_STATE: &str = "roughness_state";
const MEMBER_STEBBS_STATE: &str = "stebbs_state";
const MEMBER_NHOOD_STATE: &str = "nhood_state";

const SUEWS_STATE_MEMBER_ORDER: [&str; 13] = [
    MEMBER_ERROR_STATE,
    MEMBER_FLAG_STATE,
    MEMBER_ANTHROEMIS_STATE,
    MEMBER_OHM_STATE,
    MEMBER_SOLAR_STATE,
    MEMBER_ATM_STATE,
    MEMBER_PHENOLOGY_STATE,
    MEMBER_SNOW_STATE,
    MEMBER_HYDRO_STATE,
    MEMBER_HEAT_STATE,
    MEMBER_ROUGHNESS_STATE,
    MEMBER_STEBBS_STATE,
    MEMBER_NHOOD_STATE,
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuewsStateSchema {
    pub schema_version: u32,
    pub member_names: Vec<String>,
    pub member_schema_versions: BTreeMap<String, u32>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SuewsStateValuesPayload {
    pub schema_version: u32,
    pub members: BTreeMap<String, Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SuewsState {
    pub error_state: ErrorState,
    pub flag_state: FlagState,
    pub anthroemis_state: AnthroEmisState,
    pub ohm_state: OhmState,
    pub solar_state: SolarState,
    pub atm_state: AtmState,
    pub phenology_state: PhenologyState,
    pub snow_state: SnowState,
    pub hydro_state: HydroState,
    pub heat_state: HeatState,
    pub roughness_state: RoughnessState,
    pub stebbs_state: StebbsState,
    pub nhood_state: NhoodState,
}

fn is_known_member(name: &str) -> bool {
    SUEWS_STATE_MEMBER_ORDER.contains(&name)
}

fn parse_values_array(value: &Value) -> Result<Vec<f64>, BridgeError> {
    let items = value.as_array().ok_or(BridgeError::BadState)?;
    items
        .iter()
        .map(|item| item.as_f64().ok_or(BridgeError::BadState))
        .collect()
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

fn scalar_values_payload_to_value(payload: &ValuesPayload) -> Value {
    let mut obj = Map::new();
    obj.insert(
        "schema_version".to_string(),
        Value::from(payload.schema_version as u64),
    );
    obj.insert(
        "values".to_string(),
        Value::Array(payload.values.iter().map(|v| Value::from(*v)).collect()),
    );
    Value::Object(obj)
}

fn scalar_values_payload_from_value(value: &Value) -> Result<ValuesPayload, BridgeError> {
    let obj = value.as_object().ok_or(BridgeError::BadState)?;
    if obj.len() != 2 || !obj.contains_key("schema_version") || !obj.contains_key("values") {
        return Err(BridgeError::BadState);
    }

    let schema_version = obj
        .get("schema_version")
        .and_then(Value::as_u64)
        .ok_or(BridgeError::BadState)
        .and_then(|v| u32::try_from(v).map_err(|_| BridgeError::BadState))?;

    let values = parse_values_array(obj.get("values").ok_or(BridgeError::BadState)?)?;

    Ok(ValuesPayload {
        schema_version,
        values,
    })
}

fn values_payload_with_dims_to_value(payload: &ValuesPayloadWithDims) -> Value {
    let mut obj = Map::new();
    obj.insert(
        "schema_version".to_string(),
        Value::from(payload.schema_version as u64),
    );
    obj.insert(
        "values".to_string(),
        Value::Array(payload.values.iter().map(|v| Value::from(*v)).collect()),
    );
    obj.insert("dims".to_string(), payload_dims_to_value(&payload.dims));
    Value::Object(obj)
}

fn values_payload_with_dims_from_value(
    value: &Value,
) -> Result<ValuesPayloadWithDims, BridgeError> {
    let obj = value.as_object().ok_or(BridgeError::BadState)?;
    if obj.len() != 3
        || !obj.contains_key("schema_version")
        || !obj.contains_key("values")
        || !obj.contains_key("dims")
    {
        return Err(BridgeError::BadState);
    }

    let schema_version = obj
        .get("schema_version")
        .and_then(Value::as_u64)
        .ok_or(BridgeError::BadState)
        .and_then(|v| u32::try_from(v).map_err(|_| BridgeError::BadState))?;

    let values = parse_values_array(obj.get("values").ok_or(BridgeError::BadState)?)?;
    let dims = payload_dims_from_value(obj.get("dims").ok_or(BridgeError::BadState)?)?;

    Ok(ValuesPayloadWithDims {
        schema_version,
        values,
        dims,
    })
}

fn error_entry_payload_to_value(payload: &ErrorEntryValuesPayload) -> Value {
    let mut obj = Map::new();
    obj.insert(
        "schema_version".to_string(),
        Value::from(payload.schema_version as u64),
    );
    obj.insert(
        "timer_values".to_string(),
        Value::Array(
            payload
                .timer_values
                .iter()
                .map(|v| Value::from(*v))
                .collect(),
        ),
    );
    obj.insert("message".to_string(), Value::from(payload.message.clone()));
    obj.insert(
        "location".to_string(),
        Value::from(payload.location.clone()),
    );
    obj.insert("is_fatal".to_string(), Value::from(payload.is_fatal));
    Value::Object(obj)
}

fn error_entry_payload_from_value(value: &Value) -> Result<ErrorEntryValuesPayload, BridgeError> {
    let obj = value.as_object().ok_or(BridgeError::BadState)?;
    if obj.len() != 5
        || !obj.contains_key("schema_version")
        || !obj.contains_key("timer_values")
        || !obj.contains_key("message")
        || !obj.contains_key("location")
        || !obj.contains_key("is_fatal")
    {
        return Err(BridgeError::BadState);
    }

    let schema_version = obj
        .get("schema_version")
        .and_then(Value::as_u64)
        .ok_or(BridgeError::BadState)
        .and_then(|v| u32::try_from(v).map_err(|_| BridgeError::BadState))?;

    let timer_values = parse_values_array(obj.get("timer_values").ok_or(BridgeError::BadState)?)?;
    let message = obj
        .get("message")
        .and_then(Value::as_str)
        .ok_or(BridgeError::BadState)?
        .to_string();
    let location = obj
        .get("location")
        .and_then(Value::as_str)
        .ok_or(BridgeError::BadState)?
        .to_string();
    let is_fatal = obj
        .get("is_fatal")
        .and_then(Value::as_bool)
        .ok_or(BridgeError::BadState)?;

    Ok(ErrorEntryValuesPayload {
        schema_version,
        timer_values,
        message,
        location,
        is_fatal,
    })
}

fn error_state_payload_to_value(payload: &ErrorStateValuesPayload) -> Value {
    let mut obj = Map::new();
    obj.insert(
        "schema_version".to_string(),
        Value::from(payload.schema_version as u64),
    );
    obj.insert("flag".to_string(), Value::from(payload.flag));
    obj.insert("code".to_string(), Value::from(payload.code));
    obj.insert("message".to_string(), Value::from(payload.message.clone()));
    obj.insert("has_fatal".to_string(), Value::from(payload.has_fatal));
    obj.insert("count".to_string(), Value::from(payload.count as u64));
    obj.insert(
        "log".to_string(),
        Value::Array(
            payload
                .log
                .iter()
                .map(error_entry_payload_to_value)
                .collect(),
        ),
    );
    obj.insert("dims".to_string(), payload_dims_to_value(&payload.dims));
    Value::Object(obj)
}

fn error_state_payload_from_value(value: &Value) -> Result<ErrorStateValuesPayload, BridgeError> {
    let obj = value.as_object().ok_or(BridgeError::BadState)?;
    if obj.len() != 8
        || !obj.contains_key("schema_version")
        || !obj.contains_key("flag")
        || !obj.contains_key("code")
        || !obj.contains_key("message")
        || !obj.contains_key("has_fatal")
        || !obj.contains_key("count")
        || !obj.contains_key("log")
        || !obj.contains_key("dims")
    {
        return Err(BridgeError::BadState);
    }

    let schema_version = obj
        .get("schema_version")
        .and_then(Value::as_u64)
        .ok_or(BridgeError::BadState)
        .and_then(|v| u32::try_from(v).map_err(|_| BridgeError::BadState))?;

    let flag = obj
        .get("flag")
        .and_then(Value::as_bool)
        .ok_or(BridgeError::BadState)?;

    let code = obj
        .get("code")
        .and_then(Value::as_i64)
        .ok_or(BridgeError::BadState)
        .and_then(|v| i32::try_from(v).map_err(|_| BridgeError::BadState))?;

    let message = obj
        .get("message")
        .and_then(Value::as_str)
        .ok_or(BridgeError::BadState)?
        .to_string();

    let has_fatal = obj
        .get("has_fatal")
        .and_then(Value::as_bool)
        .ok_or(BridgeError::BadState)?;

    let count = obj
        .get("count")
        .and_then(Value::as_u64)
        .ok_or(BridgeError::BadState)
        .and_then(|v| usize::try_from(v).map_err(|_| BridgeError::BadState))?;

    let log_values = obj
        .get("log")
        .and_then(Value::as_array)
        .ok_or(BridgeError::BadState)?;
    let mut log = Vec::with_capacity(log_values.len());
    for entry in log_values {
        log.push(error_entry_payload_from_value(entry)?);
    }

    let dims = payload_dims_from_value(obj.get("dims").ok_or(BridgeError::BadState)?)?;

    Ok(ErrorStateValuesPayload {
        schema_version,
        flag,
        code,
        message,
        has_fatal,
        count,
        log,
        dims,
    })
}

fn member_value<'a>(
    members: &'a BTreeMap<String, Value>,
    member: &str,
) -> Result<&'a Value, BridgeError> {
    members.get(member).ok_or(BridgeError::BadState)
}

fn validate_member_set(members: &BTreeMap<String, Value>) -> Result<(), BridgeError> {
    if members.len() != SUEWS_STATE_MEMBER_ORDER.len() {
        return Err(BridgeError::BadState);
    }

    for member in members.keys() {
        if !is_known_member(member) {
            return Err(BridgeError::BadState);
        }
    }

    for member in SUEWS_STATE_MEMBER_ORDER {
        if !members.contains_key(member) {
            return Err(BridgeError::BadState);
        }
    }

    Ok(())
}

fn payload_to_nested_value(payload: &SuewsStateValuesPayload) -> Value {
    let mut root = Map::new();
    root.insert(
        "schema_version".to_string(),
        Value::from(payload.schema_version as u64),
    );

    let mut members = Map::new();
    for member in SUEWS_STATE_MEMBER_ORDER {
        if let Some(value) = payload.members.get(member) {
            members.insert(member.to_string(), value.clone());
        }
    }
    root.insert("members".to_string(), Value::Object(members));

    Value::Object(root)
}

fn payload_from_nested_value(value: &Value) -> Result<SuewsStateValuesPayload, BridgeError> {
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

    if members_obj.len() != SUEWS_STATE_MEMBER_ORDER.len() {
        return Err(BridgeError::BadState);
    }

    let mut members = BTreeMap::new();
    for (member, payload) in members_obj {
        if !is_known_member(member) {
            return Err(BridgeError::BadState);
        }
        members.insert(member.clone(), payload.clone());
    }

    Ok(SuewsStateValuesPayload {
        schema_version,
        members,
    })
}

impl CompositeCodec for SuewsState {
    fn to_nested_payload(&self) -> Value {
        suews_state_to_nested_payload(self)
    }

    fn from_nested_payload(payload: &Value) -> Result<Self, BridgeError> {
        suews_state_from_nested_payload(payload)
    }
}

pub fn suews_state_schema_version() -> u32 {
    SUEWS_STATE_SCHEMA_VERSION
}

pub fn suews_state_schema_version_runtime() -> Result<u32, BridgeError> {
    if error_state_schema_version_runtime()? != error_state_schema_version()
        || flag_state_schema_version_runtime()? != flag_state_schema_version()
        || anthroemis_state_schema_version_runtime()? != anthroemis_state_schema_version()
        || ohm_state_schema_version_runtime()? != ohm_state_schema_version()
        || solar_state_schema_version_runtime()? != solar_state_schema_version()
        || atm_state_schema_version_runtime()? != atm_state_schema_version()
        || phenology_state_schema_version_runtime()? != phenology_state_schema_version()
        || snow_state_schema_version_runtime()? != snow_state_schema_version()
        || hydro_state_schema_version_runtime()? != hydro_state_schema_version()
        || heat_state_schema_version_runtime()? != heat_state_schema_version()
        || roughness_state_schema_version_runtime()? != roughness_state_schema_version()
        || stebbs_state_schema_version_runtime()? != stebbs_state_schema_version()
        || nhood_state_schema_version_runtime()? != nhood_state_schema_version()
    {
        return Err(BridgeError::BadState);
    }

    Ok(SUEWS_STATE_SCHEMA_VERSION)
}

pub fn suews_state_member_names() -> Vec<String> {
    SUEWS_STATE_MEMBER_ORDER
        .iter()
        .map(|member| (*member).to_string())
        .collect()
}

pub fn suews_state_schema_info() -> Result<SuewsStateSchema, BridgeError> {
    if suews_state_schema_version_runtime()? != SUEWS_STATE_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    let mut member_schema_versions = BTreeMap::new();
    member_schema_versions.insert(MEMBER_ERROR_STATE.to_string(), error_state_schema_version());
    member_schema_versions.insert(MEMBER_FLAG_STATE.to_string(), flag_state_schema_version());
    member_schema_versions.insert(
        MEMBER_ANTHROEMIS_STATE.to_string(),
        anthroemis_state_schema_version(),
    );
    member_schema_versions.insert(MEMBER_OHM_STATE.to_string(), ohm_state_schema_version());
    member_schema_versions.insert(MEMBER_SOLAR_STATE.to_string(), solar_state_schema_version());
    member_schema_versions.insert(MEMBER_ATM_STATE.to_string(), atm_state_schema_version());
    member_schema_versions.insert(
        MEMBER_PHENOLOGY_STATE.to_string(),
        phenology_state_schema_version(),
    );
    member_schema_versions.insert(MEMBER_SNOW_STATE.to_string(), snow_state_schema_version());
    member_schema_versions.insert(MEMBER_HYDRO_STATE.to_string(), hydro_state_schema_version());
    member_schema_versions.insert(MEMBER_HEAT_STATE.to_string(), heat_state_schema_version());
    member_schema_versions.insert(
        MEMBER_ROUGHNESS_STATE.to_string(),
        roughness_state_schema_version(),
    );
    member_schema_versions.insert(
        MEMBER_STEBBS_STATE.to_string(),
        stebbs_state_schema_version(),
    );
    member_schema_versions.insert(MEMBER_NHOOD_STATE.to_string(), nhood_state_schema_version());

    Ok(SuewsStateSchema {
        schema_version: SUEWS_STATE_SCHEMA_VERSION,
        member_names: suews_state_member_names(),
        member_schema_versions,
    })
}

pub fn suews_state_default_from_fortran() -> Result<SuewsState, BridgeError> {
    Ok(SuewsState {
        error_state: error_state_default_from_fortran()?,
        flag_state: flag_state_default_from_fortran()?,
        anthroemis_state: anthroemis_state_default_from_fortran()?,
        ohm_state: ohm_state_default_from_fortran()?,
        solar_state: solar_state_default_from_fortran()?,
        atm_state: atm_state_default_from_fortran()?,
        phenology_state: phenology_state_default_from_fortran()?,
        snow_state: snow_state_default_from_fortran()?,
        hydro_state: hydro_state_default_from_fortran()?,
        heat_state: heat_state_default_from_fortran()?,
        roughness_state: roughness_state_default_from_fortran()?,
        stebbs_state: stebbs_state_default_from_fortran()?,
        nhood_state: nhood_state_default_from_fortran()?,
    })
}

pub fn suews_state_to_values_payload(state: &SuewsState) -> SuewsStateValuesPayload {
    let mut members = BTreeMap::new();

    members.insert(
        MEMBER_ERROR_STATE.to_string(),
        error_state_payload_to_value(&error_state_to_values_payload(&state.error_state)),
    );
    members.insert(
        MEMBER_FLAG_STATE.to_string(),
        scalar_values_payload_to_value(&flag_state_to_values_payload(&state.flag_state)),
    );
    members.insert(
        MEMBER_ANTHROEMIS_STATE.to_string(),
        scalar_values_payload_to_value(&anthroemis_state_to_values_payload(
            &state.anthroemis_state,
        )),
    );
    members.insert(
        MEMBER_OHM_STATE.to_string(),
        scalar_values_payload_to_value(&ohm_state_to_values_payload(&state.ohm_state)),
    );
    members.insert(
        MEMBER_SOLAR_STATE.to_string(),
        scalar_values_payload_to_value(&solar_state_to_values_payload(&state.solar_state)),
    );
    members.insert(
        MEMBER_ATM_STATE.to_string(),
        scalar_values_payload_to_value(&atm_state_to_values_payload(&state.atm_state)),
    );
    members.insert(
        MEMBER_PHENOLOGY_STATE.to_string(),
        scalar_values_payload_to_value(&phenology_state_to_values_payload(&state.phenology_state)),
    );
    members.insert(
        MEMBER_SNOW_STATE.to_string(),
        scalar_values_payload_to_value(&snow_state_to_values_payload(&state.snow_state)),
    );
    members.insert(
        MEMBER_HYDRO_STATE.to_string(),
        values_payload_with_dims_to_value(&hydro_state_to_values_payload(&state.hydro_state)),
    );
    members.insert(
        MEMBER_HEAT_STATE.to_string(),
        values_payload_with_dims_to_value(&heat_state_to_values_payload(&state.heat_state)),
    );
    members.insert(
        MEMBER_ROUGHNESS_STATE.to_string(),
        scalar_values_payload_to_value(&roughness_state_to_values_payload(&state.roughness_state)),
    );
    members.insert(
        MEMBER_STEBBS_STATE.to_string(),
        scalar_values_payload_to_value(&stebbs_state_to_values_payload(&state.stebbs_state)),
    );
    members.insert(
        MEMBER_NHOOD_STATE.to_string(),
        scalar_values_payload_to_value(&nhood_state_to_values_payload(&state.nhood_state)),
    );

    SuewsStateValuesPayload {
        schema_version: SUEWS_STATE_SCHEMA_VERSION,
        members,
    }
}

pub fn suews_state_from_values_payload(
    payload: &SuewsStateValuesPayload,
) -> Result<SuewsState, BridgeError> {
    if payload.schema_version != SUEWS_STATE_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    validate_member_set(&payload.members)?;

    let error_state_payload =
        error_state_payload_from_value(member_value(&payload.members, MEMBER_ERROR_STATE)?)?;
    let flag_payload =
        scalar_values_payload_from_value(member_value(&payload.members, MEMBER_FLAG_STATE)?)?;
    let anthroemis_payload =
        scalar_values_payload_from_value(member_value(&payload.members, MEMBER_ANTHROEMIS_STATE)?)?;
    let ohm_payload =
        scalar_values_payload_from_value(member_value(&payload.members, MEMBER_OHM_STATE)?)?;
    let solar_payload =
        scalar_values_payload_from_value(member_value(&payload.members, MEMBER_SOLAR_STATE)?)?;
    let atm_payload =
        scalar_values_payload_from_value(member_value(&payload.members, MEMBER_ATM_STATE)?)?;
    let phenology_payload =
        scalar_values_payload_from_value(member_value(&payload.members, MEMBER_PHENOLOGY_STATE)?)?;
    let snow_payload =
        scalar_values_payload_from_value(member_value(&payload.members, MEMBER_SNOW_STATE)?)?;
    let hydro_payload =
        values_payload_with_dims_from_value(member_value(&payload.members, MEMBER_HYDRO_STATE)?)?;
    let heat_payload =
        values_payload_with_dims_from_value(member_value(&payload.members, MEMBER_HEAT_STATE)?)?;
    let roughness_payload =
        scalar_values_payload_from_value(member_value(&payload.members, MEMBER_ROUGHNESS_STATE)?)?;
    let stebbs_payload =
        scalar_values_payload_from_value(member_value(&payload.members, MEMBER_STEBBS_STATE)?)?;
    let nhood_payload =
        scalar_values_payload_from_value(member_value(&payload.members, MEMBER_NHOOD_STATE)?)?;

    Ok(SuewsState {
        error_state: error_state_from_values_payload(&error_state_payload)?,
        flag_state: flag_state_from_values_payload(&flag_payload)?,
        anthroemis_state: anthroemis_state_from_values_payload(&anthroemis_payload)?,
        ohm_state: ohm_state_from_values_payload(&ohm_payload)?,
        solar_state: solar_state_from_values_payload(&solar_payload)?,
        atm_state: atm_state_from_values_payload(&atm_payload)?,
        phenology_state: phenology_state_from_values_payload(&phenology_payload)?,
        snow_state: snow_state_from_values_payload(&snow_payload)?,
        hydro_state: hydro_state_from_values_payload(&hydro_payload)?,
        heat_state: heat_state_from_values_payload(&heat_payload)?,
        roughness_state: roughness_state_from_values_payload(&roughness_payload)?,
        stebbs_state: stebbs_state_from_values_payload(&stebbs_payload)?,
        nhood_state: nhood_state_from_values_payload(&nhood_payload)?,
    })
}

pub fn suews_state_to_nested_payload(state: &SuewsState) -> Value {
    payload_to_nested_value(&suews_state_to_values_payload(state))
}

pub fn suews_state_from_nested_payload(payload: &Value) -> Result<SuewsState, BridgeError> {
    let values_payload = payload_from_nested_value(payload)?;
    suews_state_from_values_payload(&values_payload)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_roundtrip_via_values_payload() {
        let state = suews_state_default_from_fortran().expect("default state should be available");
        let payload = suews_state_to_values_payload(&state);
        let decoded =
            suews_state_from_values_payload(&payload).expect("values payload decode should work");
        assert_eq!(decoded, state);
    }

    #[test]
    fn default_roundtrip_via_nested_payload() {
        let state = suews_state_default_from_fortran().expect("default state should be available");
        let payload = suews_state_to_nested_payload(&state);
        let decoded =
            suews_state_from_nested_payload(&payload).expect("nested payload decode should work");
        assert_eq!(decoded, state);
    }

    #[test]
    fn values_payload_rejects_missing_member() {
        let state = suews_state_default_from_fortran().expect("default state should be available");
        let mut payload = suews_state_to_values_payload(&state);
        payload.members.remove(MEMBER_HYDRO_STATE);

        let err =
            suews_state_from_values_payload(&payload).expect_err("missing member should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn nested_payload_rejects_unknown_member() {
        let payload = serde_json::json!({
            "schema_version": SUEWS_STATE_SCHEMA_VERSION,
            "members": {
                "unknown": {
                    "schema_version": 1,
                    "values": []
                }
            }
        });

        let err = suews_state_from_nested_payload(&payload)
            .expect_err("unknown member payload should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn member_names_are_deterministic() {
        let names = suews_state_member_names();
        assert_eq!(names.first().map(String::as_str), Some(MEMBER_ERROR_STATE));
        assert_eq!(names.last().map(String::as_str), Some(MEMBER_NHOOD_STATE));
        assert_eq!(names.len(), SUEWS_STATE_MEMBER_ORDER.len());
    }
}
