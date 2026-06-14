use crate::anthro_emis_prm::{
    anthro_emis_prm_default_from_fortran, anthro_emis_prm_from_map, anthro_emis_prm_to_map,
    AnthroEmisPrm,
};
use crate::building_archetype_prm::{
    building_archetype_prm_default_from_fortran, building_archetype_prm_from_map,
    building_archetype_prm_to_map, BuildingArchetypePrm,
};
use crate::codec::CompositeCodec;
use crate::conductance::{
    conductance_prm_default_from_fortran, conductance_prm_from_map, conductance_prm_to_map,
    ConductancePrm,
};
use crate::ehc_prm::{ehc_prm_default_from_fortran, ehc_prm_from_map, ehc_prm_to_map, EhcPrm};
use crate::error::BridgeError;
use crate::irrigation_prm::{
    irrigation_prm_default_from_fortran, irrigation_prm_from_map, irrigation_prm_to_map,
    IrrigationPrm,
};
use crate::lc_bldg_prm::{
    lc_bldg_prm_default_from_fortran, lc_bldg_prm_from_map, lc_bldg_prm_to_map, LcBldgPrm,
};
use crate::lc_bsoil_prm::{
    lc_bsoil_prm_default_from_fortran, lc_bsoil_prm_from_map, lc_bsoil_prm_to_map, LcBsoilPrm,
};
use crate::lc_dectr_prm::{
    lc_dectr_prm_default_from_fortran, lc_dectr_prm_from_map, lc_dectr_prm_to_map, LcDectrPrm,
};
use crate::lc_evetr_prm::{
    lc_evetr_prm_default_from_fortran, lc_evetr_prm_from_map, lc_evetr_prm_to_map, LcEvetrPrm,
};
use crate::lc_grass_prm::{
    lc_grass_prm_default_from_fortran, lc_grass_prm_from_map, lc_grass_prm_to_map, LcGrassPrm,
};
use crate::lc_paved_prm::{
    lc_paved_prm_default_from_fortran, lc_paved_prm_from_map, lc_paved_prm_to_map, LcPavedPrm,
};
use crate::lc_water_prm::{
    lc_water_prm_default_from_fortran, lc_water_prm_from_map, lc_water_prm_to_map, LcWaterPrm,
};
use crate::lumps::{
    lumps_prm_default_from_fortran, lumps_prm_from_map, lumps_prm_to_map, LumpsPrm,
};
use crate::snow_prm::{snow_prm_default_from_fortran, snow_prm_from_map, snow_prm_to_map, SnowPrm};
use crate::spartacus_layer_prm::{
    spartacus_layer_prm_default_from_fortran, spartacus_layer_prm_from_map,
    spartacus_layer_prm_to_map, SpartacusLayerPrm,
};
use crate::spartacus_prm::{
    spartacus_prm_default_from_fortran, spartacus_prm_from_map, spartacus_prm_to_map, SpartacusPrm,
};
use crate::stebbs_prm::{
    stebbs_prm_default_from_fortran, stebbs_prm_from_map, stebbs_prm_to_map, StebbsPrm,
};
use crate::surf_store::{
    surf_store_prm_default_from_fortran, surf_store_prm_from_map, surf_store_prm_to_map,
    SurfStorePrm,
};
use serde_json::{Map, Value};
use std::collections::BTreeMap;

pub const SUEWS_SITE_SCHEMA_VERSION: u32 = 1;

const MEMBER_SPARTACUS: &str = "spartacus";
const MEMBER_LUMPS: &str = "lumps";
const MEMBER_EHC: &str = "ehc";
const MEMBER_SPARTACUS_LAYER: &str = "spartacus_layer";
const MEMBER_SURF_STORE: &str = "surf_store";
const MEMBER_IRRIGATION: &str = "irrigation";
const MEMBER_ANTHROEMIS: &str = "anthroemis";
const MEMBER_SNOW: &str = "snow";
const MEMBER_CONDUCTANCE: &str = "conductance";
const MEMBER_LC_PAVED: &str = "lc_paved";
const MEMBER_LC_BLDG: &str = "lc_bldg";
const MEMBER_LC_DECTR: &str = "lc_dectr";
const MEMBER_LC_EVETR: &str = "lc_evetr";
const MEMBER_LC_GRASS: &str = "lc_grass";
const MEMBER_LC_BSOIL: &str = "lc_bsoil";
const MEMBER_LC_WATER: &str = "lc_water";
const MEMBER_BUILDING_ARCHTYPE: &str = "building_archtype";
const MEMBER_STEBBS: &str = "stebbs";

const SUEWS_SITE_MEMBER_ORDER: [&str; 18] = [
    MEMBER_SPARTACUS,
    MEMBER_LUMPS,
    MEMBER_EHC,
    MEMBER_SPARTACUS_LAYER,
    MEMBER_SURF_STORE,
    MEMBER_IRRIGATION,
    MEMBER_ANTHROEMIS,
    MEMBER_SNOW,
    MEMBER_CONDUCTANCE,
    MEMBER_LC_PAVED,
    MEMBER_LC_BLDG,
    MEMBER_LC_DECTR,
    MEMBER_LC_EVETR,
    MEMBER_LC_GRASS,
    MEMBER_LC_BSOIL,
    MEMBER_LC_WATER,
    MEMBER_BUILDING_ARCHTYPE,
    MEMBER_STEBBS,
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuewsSiteSchema {
    pub schema_version: u32,
    pub member_names: Vec<String>,
    pub field_names: Vec<String>,
    pub member_field_counts: BTreeMap<String, usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SuewsSiteValuesPayload {
    pub schema_version: u32,
    pub members: BTreeMap<String, BTreeMap<String, f64>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SuewsSite {
    pub spartacus: SpartacusPrm,
    pub lumps: LumpsPrm,
    pub ehc: EhcPrm,
    pub spartacus_layer: SpartacusLayerPrm,
    pub surf_store: SurfStorePrm,
    pub irrigation: IrrigationPrm,
    pub anthroemis: AnthroEmisPrm,
    pub snow: SnowPrm,
    pub conductance: ConductancePrm,
    pub lc_paved: LcPavedPrm,
    pub lc_bldg: LcBldgPrm,
    pub lc_dectr: LcDectrPrm,
    pub lc_evetr: LcEvetrPrm,
    pub lc_grass: LcGrassPrm,
    pub lc_bsoil: LcBsoilPrm,
    pub lc_water: LcWaterPrm,
    pub building_archtype: BuildingArchetypePrm,
    pub stebbs: StebbsPrm,
}

impl SuewsSite {
    fn to_nested_maps(&self) -> BTreeMap<String, BTreeMap<String, f64>> {
        let mut members = BTreeMap::new();
        members.insert(
            MEMBER_SPARTACUS.to_string(),
            spartacus_prm_to_map(&self.spartacus),
        );
        members.insert(MEMBER_LUMPS.to_string(), lumps_prm_to_map(&self.lumps));
        members.insert(MEMBER_EHC.to_string(), ehc_prm_to_map(&self.ehc));
        members.insert(
            MEMBER_SPARTACUS_LAYER.to_string(),
            spartacus_layer_prm_to_map(&self.spartacus_layer),
        );
        members.insert(
            MEMBER_SURF_STORE.to_string(),
            surf_store_prm_to_map(&self.surf_store),
        );
        members.insert(
            MEMBER_IRRIGATION.to_string(),
            irrigation_prm_to_map(&self.irrigation),
        );
        members.insert(
            MEMBER_ANTHROEMIS.to_string(),
            anthro_emis_prm_to_map(&self.anthroemis),
        );
        members.insert(MEMBER_SNOW.to_string(), snow_prm_to_map(&self.snow));
        members.insert(
            MEMBER_CONDUCTANCE.to_string(),
            conductance_prm_to_map(&self.conductance),
        );
        members.insert(
            MEMBER_LC_PAVED.to_string(),
            lc_paved_prm_to_map(&self.lc_paved),
        );
        members.insert(
            MEMBER_LC_BLDG.to_string(),
            lc_bldg_prm_to_map(&self.lc_bldg),
        );
        members.insert(
            MEMBER_LC_DECTR.to_string(),
            lc_dectr_prm_to_map(&self.lc_dectr),
        );
        members.insert(
            MEMBER_LC_EVETR.to_string(),
            lc_evetr_prm_to_map(&self.lc_evetr),
        );
        members.insert(
            MEMBER_LC_GRASS.to_string(),
            lc_grass_prm_to_map(&self.lc_grass),
        );
        members.insert(
            MEMBER_LC_BSOIL.to_string(),
            lc_bsoil_prm_to_map(&self.lc_bsoil),
        );
        members.insert(
            MEMBER_LC_WATER.to_string(),
            lc_water_prm_to_map(&self.lc_water),
        );
        members.insert(
            MEMBER_BUILDING_ARCHTYPE.to_string(),
            building_archetype_prm_to_map(&self.building_archtype),
        );
        members.insert(MEMBER_STEBBS.to_string(), stebbs_prm_to_map(&self.stebbs));
        members
    }
}

impl CompositeCodec for SuewsSite {
    fn to_nested_payload(&self) -> Value {
        nested_payload_to_value(&suews_site_to_values_payload(self))
    }

    fn from_nested_payload(payload: &Value) -> Result<Self, BridgeError> {
        let values_payload = nested_payload_from_value(payload)?;
        suews_site_from_values_payload(&values_payload)
    }
}

fn is_known_member(name: &str) -> bool {
    SUEWS_SITE_MEMBER_ORDER.contains(&name)
}

fn split_flat_map(
    values: &BTreeMap<String, f64>,
) -> Result<BTreeMap<String, BTreeMap<String, f64>>, BridgeError> {
    let mut nested: BTreeMap<String, BTreeMap<String, f64>> = BTreeMap::new();

    for (flat_name, value) in values {
        let mut parts = flat_name.splitn(2, '.');
        let member = parts.next().ok_or(BridgeError::BadState)?;
        let field = parts.next().ok_or(BridgeError::BadState)?;
        if member.is_empty() || field.is_empty() || !is_known_member(member) {
            return Err(BridgeError::BadState);
        }
        nested
            .entry(member.to_string())
            .or_default()
            .insert(field.to_string(), *value);
    }

    Ok(nested)
}

fn update_from_nested_maps(
    site: &mut SuewsSite,
    nested: &BTreeMap<String, BTreeMap<String, f64>>,
) -> Result<(), BridgeError> {
    if let Some(values) = nested.get(MEMBER_SPARTACUS) {
        site.spartacus = spartacus_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_LUMPS) {
        site.lumps = lumps_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_EHC) {
        site.ehc = ehc_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_SPARTACUS_LAYER) {
        site.spartacus_layer = spartacus_layer_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_SURF_STORE) {
        site.surf_store = surf_store_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_IRRIGATION) {
        site.irrigation = irrigation_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_ANTHROEMIS) {
        site.anthroemis = anthro_emis_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_SNOW) {
        site.snow = snow_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_CONDUCTANCE) {
        site.conductance = conductance_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_LC_PAVED) {
        site.lc_paved = lc_paved_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_LC_BLDG) {
        site.lc_bldg = lc_bldg_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_LC_DECTR) {
        site.lc_dectr = lc_dectr_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_LC_EVETR) {
        site.lc_evetr = lc_evetr_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_LC_GRASS) {
        site.lc_grass = lc_grass_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_LC_BSOIL) {
        site.lc_bsoil = lc_bsoil_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_LC_WATER) {
        site.lc_water = lc_water_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_BUILDING_ARCHTYPE) {
        site.building_archtype = building_archetype_prm_from_map(values)?;
    }
    if let Some(values) = nested.get(MEMBER_STEBBS) {
        site.stebbs = stebbs_prm_from_map(values)?;
    }

    Ok(())
}

fn nested_payload_to_value(payload: &SuewsSiteValuesPayload) -> Value {
    let mut root = Map::new();
    root.insert(
        "schema_version".to_string(),
        Value::from(payload.schema_version as u64),
    );

    let mut members = Map::new();
    for member in SUEWS_SITE_MEMBER_ORDER {
        if let Some(fields) = payload.members.get(member) {
            let mut field_obj = Map::new();
            for (field, value) in fields {
                field_obj.insert(field.clone(), Value::from(*value));
            }
            members.insert(member.to_string(), Value::Object(field_obj));
        }
    }

    root.insert("members".to_string(), Value::Object(members));
    Value::Object(root)
}

fn nested_payload_from_value(payload: &Value) -> Result<SuewsSiteValuesPayload, BridgeError> {
    let root = payload.as_object().ok_or(BridgeError::BadState)?;
    let schema_version = root
        .get("schema_version")
        .and_then(Value::as_u64)
        .ok_or(BridgeError::BadState)? as u32;

    let members_obj = root
        .get("members")
        .and_then(Value::as_object)
        .ok_or(BridgeError::BadState)?;

    let mut members = BTreeMap::new();
    for (member, fields_value) in members_obj {
        if !is_known_member(member) {
            return Err(BridgeError::BadState);
        }

        let fields_obj = fields_value.as_object().ok_or(BridgeError::BadState)?;
        let mut fields = BTreeMap::new();
        for (field, value) in fields_obj {
            let number = value.as_f64().ok_or(BridgeError::BadState)?;
            fields.insert(field.clone(), number);
        }
        members.insert(member.clone(), fields);
    }

    Ok(SuewsSiteValuesPayload {
        schema_version,
        members,
    })
}

pub fn suews_site_schema_version() -> u32 {
    SUEWS_SITE_SCHEMA_VERSION
}

pub fn suews_site_schema_version_runtime() -> Result<u32, BridgeError> {
    Ok(SUEWS_SITE_SCHEMA_VERSION)
}

pub fn suews_site_member_names() -> Vec<String> {
    SUEWS_SITE_MEMBER_ORDER
        .iter()
        .map(|member| (*member).to_string())
        .collect()
}

pub fn suews_site_default_from_fortran() -> Result<SuewsSite, BridgeError> {
    Ok(SuewsSite {
        spartacus: spartacus_prm_default_from_fortran()?,
        lumps: lumps_prm_default_from_fortran()?,
        ehc: ehc_prm_default_from_fortran()?,
        spartacus_layer: spartacus_layer_prm_default_from_fortran()?,
        surf_store: surf_store_prm_default_from_fortran()?,
        irrigation: irrigation_prm_default_from_fortran()?,
        anthroemis: anthro_emis_prm_default_from_fortran()?,
        snow: snow_prm_default_from_fortran()?,
        conductance: conductance_prm_default_from_fortran()?,
        lc_paved: lc_paved_prm_default_from_fortran()?,
        lc_bldg: lc_bldg_prm_default_from_fortran()?,
        lc_dectr: lc_dectr_prm_default_from_fortran()?,
        lc_evetr: lc_evetr_prm_default_from_fortran()?,
        lc_grass: lc_grass_prm_default_from_fortran()?,
        lc_bsoil: lc_bsoil_prm_default_from_fortran()?,
        lc_water: lc_water_prm_default_from_fortran()?,
        building_archtype: building_archetype_prm_default_from_fortran()?,
        stebbs: stebbs_prm_default_from_fortran()?,
    })
}

pub fn suews_site_to_map(state: &SuewsSite) -> BTreeMap<String, f64> {
    let nested = state.to_nested_maps();
    let mut flat = BTreeMap::new();

    for member in SUEWS_SITE_MEMBER_ORDER {
        if let Some(fields) = nested.get(member) {
            for (field, value) in fields {
                flat.insert(format!("{member}.{field}"), *value);
            }
        }
    }

    flat
}

pub fn suews_site_from_map(values: &BTreeMap<String, f64>) -> Result<SuewsSite, BridgeError> {
    let nested = split_flat_map(values)?;
    let mut site = suews_site_default_from_fortran()?;
    update_from_nested_maps(&mut site, &nested)?;
    Ok(site)
}

pub fn suews_site_to_values_payload(state: &SuewsSite) -> SuewsSiteValuesPayload {
    SuewsSiteValuesPayload {
        schema_version: SUEWS_SITE_SCHEMA_VERSION,
        members: state.to_nested_maps(),
    }
}

pub fn suews_site_from_values_payload(
    payload: &SuewsSiteValuesPayload,
) -> Result<SuewsSite, BridgeError> {
    if payload.schema_version != SUEWS_SITE_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    for member in payload.members.keys() {
        if !is_known_member(member) {
            return Err(BridgeError::BadState);
        }
    }

    let mut site = suews_site_default_from_fortran()?;
    update_from_nested_maps(&mut site, &payload.members)?;
    Ok(site)
}

pub fn suews_site_field_names() -> Result<Vec<String>, BridgeError> {
    let default = suews_site_default_from_fortran()?;
    let nested = default.to_nested_maps();
    let mut names = Vec::new();

    for member in SUEWS_SITE_MEMBER_ORDER {
        if let Some(fields) = nested.get(member) {
            for field in fields.keys() {
                names.push(format!("{member}.{field}"));
            }
        }
    }

    Ok(names)
}

pub fn suews_site_schema_info() -> Result<SuewsSiteSchema, BridgeError> {
    let default = suews_site_default_from_fortran()?;
    let nested = default.to_nested_maps();

    let mut member_field_counts = BTreeMap::new();
    for member in SUEWS_SITE_MEMBER_ORDER {
        let count = nested.get(member).map_or(0, BTreeMap::len);
        member_field_counts.insert(member.to_string(), count);
    }

    Ok(SuewsSiteSchema {
        schema_version: SUEWS_SITE_SCHEMA_VERSION,
        member_names: suews_site_member_names(),
        field_names: suews_site_field_names()?,
        member_field_counts,
    })
}

pub fn suews_site_to_nested_payload(state: &SuewsSite) -> Value {
    nested_payload_to_value(&suews_site_to_values_payload(state))
}

pub fn suews_site_from_nested_payload(payload: &Value) -> Result<SuewsSite, BridgeError> {
    SuewsSite::from_nested_payload(payload)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_roundtrip_via_map() {
        let default = suews_site_default_from_fortran().expect("default site should be available");
        let mapped = suews_site_to_map(&default);
        let decoded = suews_site_from_map(&mapped).expect("map decode should succeed");
        assert_eq!(decoded, default);
    }

    #[test]
    fn default_roundtrip_via_values_payload() {
        let default = suews_site_default_from_fortran().expect("default site should be available");
        let payload = suews_site_to_values_payload(&default);
        let decoded =
            suews_site_from_values_payload(&payload).expect("values payload decode should succeed");
        assert_eq!(decoded, default);
    }

    #[test]
    fn default_roundtrip_via_nested_payload() {
        let default = suews_site_default_from_fortran().expect("default site should be available");
        let payload = suews_site_to_nested_payload(&default);
        let decoded =
            suews_site_from_nested_payload(&payload).expect("nested payload decode should succeed");
        assert_eq!(decoded, default);
    }

    #[test]
    fn field_names_are_prefixed_and_deterministic() {
        let names = suews_site_field_names().expect("field names should be available");
        assert!(!names.is_empty());
        assert!(names.iter().all(|name| name.contains('.')));
        assert!(names
            .iter()
            .any(|name| name.starts_with("spartacus.") && name.ends_with("air_ext_lw")));
    }

    #[test]
    fn map_decode_rejects_unknown_member_prefix() {
        let mut bad = BTreeMap::new();
        bad.insert("unknown.field".to_string(), 1.0);
        let err = suews_site_from_map(&bad).expect_err("unknown member should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn nested_payload_decode_rejects_unknown_member() {
        let payload = serde_json::json!({
            "schema_version": SUEWS_SITE_SCHEMA_VERSION,
            "members": {
                "unknown": { "x": 1.0 }
            }
        });
        let err = suews_site_from_nested_payload(&payload).expect_err("unknown member should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
