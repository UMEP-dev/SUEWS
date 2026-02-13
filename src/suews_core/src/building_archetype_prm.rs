use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const BUILDING_ARCHETYPE_PRM_PROFILE_STEPS: usize = 144;
pub const BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS: usize = 2;
pub const BUILDING_ARCHETYPE_PRM_FLAT_LEN: usize = 639;
pub const BUILDING_ARCHETYPE_PRM_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuildingArchetypePrmSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

pub type BuildingArchetypePrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct BuildingArchetypePrm {
    pub buildingcount: f64,
    pub occupants: f64,
    pub hhs0: f64,
    pub age_0_4: f64,
    pub age_5_11: f64,
    pub age_12_18: f64,
    pub age_19_64: f64,
    pub age_65plus: f64,
    pub stebbs_height: f64,
    pub footprintarea: f64,
    pub wallexternalarea: f64,
    pub ratiointernalvolume: f64,
    pub wwr: f64,
    pub wallthickness: f64,
    pub walleffectiveconductivity: f64,
    pub walldensity: f64,
    pub wallcp: f64,
    pub wallextthickness: f64,
    pub wallexteffectiveconductivity: f64,
    pub wallextdensity: f64,
    pub wallextcp: f64,
    pub walloutercapfrac: f64,
    pub wallexternalemissivity: f64,
    pub wallinternalemissivity: f64,
    pub walltransmissivity: f64,
    pub wallabsorbtivity: f64,
    pub wallreflectivity: f64,
    pub roofthickness: f64,
    pub roofeffectiveconductivity: f64,
    pub roofdensity: f64,
    pub roofcp: f64,
    pub roofextthickness: f64,
    pub roofexteffectiveconductivity: f64,
    pub roofextdensity: f64,
    pub roofextcp: f64,
    pub roofoutercapfrac: f64,
    pub roofexternalemissivity: f64,
    pub roofinternalemissivity: f64,
    pub rooftransmissivity: f64,
    pub roofabsorbtivity: f64,
    pub roofreflectivity: f64,
    pub floorthickness: f64,
    pub groundflooreffectiveconductivity: f64,
    pub groundfloordensity: f64,
    pub groundfloorcp: f64,
    pub windowthickness: f64,
    pub windoweffectiveconductivity: f64,
    pub windowdensity: f64,
    pub windowcp: f64,
    pub windowexternalemissivity: f64,
    pub windowinternalemissivity: f64,
    pub windowtransmissivity: f64,
    pub windowabsorbtivity: f64,
    pub windowreflectivity: f64,
    pub internalmassdensity: f64,
    pub internalmasscp: f64,
    pub internalmassemissivity: f64,
    pub maxheatingpower: f64,
    pub watertankwatervolume: f64,
    pub maximumhotwaterheatingpower: f64,
    pub heatingsetpointtemperature: f64,
    pub coolingsetpointtemperature: f64,
    pub metabolismprofile:
        [[f64; BUILDING_ARCHETYPE_PRM_PROFILE_STEPS]; BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS],
    pub applianceprofile:
        [[f64; BUILDING_ARCHETYPE_PRM_PROFILE_STEPS]; BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS],
    pub iter_safe: bool,
}

impl Default for BuildingArchetypePrm {
    fn default() -> Self {
        Self {
            buildingcount: 0.0,
            occupants: 0.0,
            hhs0: 0.0,
            age_0_4: 0.0,
            age_5_11: 0.0,
            age_12_18: 0.0,
            age_19_64: 0.0,
            age_65plus: 0.0,
            stebbs_height: 0.0,
            footprintarea: 0.0,
            wallexternalarea: 0.0,
            ratiointernalvolume: 0.0,
            wwr: 0.0,
            wallthickness: 0.0,
            walleffectiveconductivity: 0.0,
            walldensity: 0.0,
            wallcp: 0.0,
            wallextthickness: 0.0,
            wallexteffectiveconductivity: 0.0,
            wallextdensity: 0.0,
            wallextcp: 0.0,
            walloutercapfrac: 0.0,
            wallexternalemissivity: 0.0,
            wallinternalemissivity: 0.0,
            walltransmissivity: 0.0,
            wallabsorbtivity: 0.0,
            wallreflectivity: 0.0,
            roofthickness: 0.0,
            roofeffectiveconductivity: 0.0,
            roofdensity: 0.0,
            roofcp: 0.0,
            roofextthickness: 0.0,
            roofexteffectiveconductivity: 0.0,
            roofextdensity: 0.0,
            roofextcp: 0.0,
            roofoutercapfrac: 0.0,
            roofexternalemissivity: 0.0,
            roofinternalemissivity: 0.0,
            rooftransmissivity: 0.0,
            roofabsorbtivity: 0.0,
            roofreflectivity: 0.0,
            floorthickness: 0.0,
            groundflooreffectiveconductivity: 0.0,
            groundfloordensity: 0.0,
            groundfloorcp: 0.0,
            windowthickness: 0.0,
            windoweffectiveconductivity: 0.0,
            windowdensity: 0.0,
            windowcp: 0.0,
            windowexternalemissivity: 0.0,
            windowinternalemissivity: 0.0,
            windowtransmissivity: 0.0,
            windowabsorbtivity: 0.0,
            windowreflectivity: 0.0,
            internalmassdensity: 0.0,
            internalmasscp: 0.0,
            internalmassemissivity: 0.0,
            maxheatingpower: 0.0,
            watertankwatervolume: 0.0,
            maximumhotwaterheatingpower: 0.0,
            heatingsetpointtemperature: 0.0,
            coolingsetpointtemperature: 0.0,
            metabolismprofile: [[0.0; BUILDING_ARCHETYPE_PRM_PROFILE_STEPS];
                BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS],
            applianceprofile: [[0.0; BUILDING_ARCHETYPE_PRM_PROFILE_STEPS];
                BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS],
            iter_safe: true,
        }
    }
}

impl BuildingArchetypePrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, BUILDING_ARCHETYPE_PRM_FLAT_LEN)?;

        let mut idx = 0_usize;
        let mut next = || {
            let value = flat[idx];
            idx += 1;
            value
        };

        let buildingcount = next();
        let occupants = next();
        let hhs0 = next();
        let age_0_4 = next();
        let age_5_11 = next();
        let age_12_18 = next();
        let age_19_64 = next();
        let age_65plus = next();
        let stebbs_height = next();
        let footprintarea = next();
        let wallexternalarea = next();
        let ratiointernalvolume = next();
        let wwr = next();
        let wallthickness = next();
        let walleffectiveconductivity = next();
        let walldensity = next();
        let wallcp = next();
        let wallextthickness = next();
        let wallexteffectiveconductivity = next();
        let wallextdensity = next();
        let wallextcp = next();
        let walloutercapfrac = next();
        let wallexternalemissivity = next();
        let wallinternalemissivity = next();
        let walltransmissivity = next();
        let wallabsorbtivity = next();
        let wallreflectivity = next();
        let roofthickness = next();
        let roofeffectiveconductivity = next();
        let roofdensity = next();
        let roofcp = next();
        let roofextthickness = next();
        let roofexteffectiveconductivity = next();
        let roofextdensity = next();
        let roofextcp = next();
        let roofoutercapfrac = next();
        let roofexternalemissivity = next();
        let roofinternalemissivity = next();
        let rooftransmissivity = next();
        let roofabsorbtivity = next();
        let roofreflectivity = next();
        let floorthickness = next();
        let groundflooreffectiveconductivity = next();
        let groundfloordensity = next();
        let groundfloorcp = next();
        let windowthickness = next();
        let windoweffectiveconductivity = next();
        let windowdensity = next();
        let windowcp = next();
        let windowexternalemissivity = next();
        let windowinternalemissivity = next();
        let windowtransmissivity = next();
        let windowabsorbtivity = next();
        let windowreflectivity = next();
        let internalmassdensity = next();
        let internalmasscp = next();
        let internalmassemissivity = next();
        let maxheatingpower = next();
        let watertankwatervolume = next();
        let maximumhotwaterheatingpower = next();
        let heatingsetpointtemperature = next();
        let coolingsetpointtemperature = next();

        let mut metabolismprofile = [[0.0_f64; BUILDING_ARCHETYPE_PRM_PROFILE_STEPS];
            BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS];
        for day_type in 0..BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS {
            for step in 0..BUILDING_ARCHETYPE_PRM_PROFILE_STEPS {
                metabolismprofile[day_type][step] = next();
            }
        }

        let mut applianceprofile = [[0.0_f64; BUILDING_ARCHETYPE_PRM_PROFILE_STEPS];
            BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS];
        for day_type in 0..BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS {
            for step in 0..BUILDING_ARCHETYPE_PRM_PROFILE_STEPS {
                applianceprofile[day_type][step] = next();
            }
        }

        Ok(Self {
            buildingcount,
            occupants,
            hhs0,
            age_0_4,
            age_5_11,
            age_12_18,
            age_19_64,
            age_65plus,
            stebbs_height,
            footprintarea,
            wallexternalarea,
            ratiointernalvolume,
            wwr,
            wallthickness,
            walleffectiveconductivity,
            walldensity,
            wallcp,
            wallextthickness,
            wallexteffectiveconductivity,
            wallextdensity,
            wallextcp,
            walloutercapfrac,
            wallexternalemissivity,
            wallinternalemissivity,
            walltransmissivity,
            wallabsorbtivity,
            wallreflectivity,
            roofthickness,
            roofeffectiveconductivity,
            roofdensity,
            roofcp,
            roofextthickness,
            roofexteffectiveconductivity,
            roofextdensity,
            roofextcp,
            roofoutercapfrac,
            roofexternalemissivity,
            roofinternalemissivity,
            rooftransmissivity,
            roofabsorbtivity,
            roofreflectivity,
            floorthickness,
            groundflooreffectiveconductivity,
            groundfloordensity,
            groundfloorcp,
            windowthickness,
            windoweffectiveconductivity,
            windowdensity,
            windowcp,
            windowexternalemissivity,
            windowinternalemissivity,
            windowtransmissivity,
            windowabsorbtivity,
            windowreflectivity,
            internalmassdensity,
            internalmasscp,
            internalmassemissivity,
            maxheatingpower,
            watertankwatervolume,
            maximumhotwaterheatingpower,
            heatingsetpointtemperature,
            coolingsetpointtemperature,
            metabolismprofile,
            applianceprofile,
            iter_safe: next() >= 0.5,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(BUILDING_ARCHETYPE_PRM_FLAT_LEN);

        flat.push(self.buildingcount);
        flat.push(self.occupants);
        flat.push(self.hhs0);
        flat.push(self.age_0_4);
        flat.push(self.age_5_11);
        flat.push(self.age_12_18);
        flat.push(self.age_19_64);
        flat.push(self.age_65plus);
        flat.push(self.stebbs_height);
        flat.push(self.footprintarea);
        flat.push(self.wallexternalarea);
        flat.push(self.ratiointernalvolume);
        flat.push(self.wwr);
        flat.push(self.wallthickness);
        flat.push(self.walleffectiveconductivity);
        flat.push(self.walldensity);
        flat.push(self.wallcp);
        flat.push(self.wallextthickness);
        flat.push(self.wallexteffectiveconductivity);
        flat.push(self.wallextdensity);
        flat.push(self.wallextcp);
        flat.push(self.walloutercapfrac);
        flat.push(self.wallexternalemissivity);
        flat.push(self.wallinternalemissivity);
        flat.push(self.walltransmissivity);
        flat.push(self.wallabsorbtivity);
        flat.push(self.wallreflectivity);
        flat.push(self.roofthickness);
        flat.push(self.roofeffectiveconductivity);
        flat.push(self.roofdensity);
        flat.push(self.roofcp);
        flat.push(self.roofextthickness);
        flat.push(self.roofexteffectiveconductivity);
        flat.push(self.roofextdensity);
        flat.push(self.roofextcp);
        flat.push(self.roofoutercapfrac);
        flat.push(self.roofexternalemissivity);
        flat.push(self.roofinternalemissivity);
        flat.push(self.rooftransmissivity);
        flat.push(self.roofabsorbtivity);
        flat.push(self.roofreflectivity);
        flat.push(self.floorthickness);
        flat.push(self.groundflooreffectiveconductivity);
        flat.push(self.groundfloordensity);
        flat.push(self.groundfloorcp);
        flat.push(self.windowthickness);
        flat.push(self.windoweffectiveconductivity);
        flat.push(self.windowdensity);
        flat.push(self.windowcp);
        flat.push(self.windowexternalemissivity);
        flat.push(self.windowinternalemissivity);
        flat.push(self.windowtransmissivity);
        flat.push(self.windowabsorbtivity);
        flat.push(self.windowreflectivity);
        flat.push(self.internalmassdensity);
        flat.push(self.internalmasscp);
        flat.push(self.internalmassemissivity);
        flat.push(self.maxheatingpower);
        flat.push(self.watertankwatervolume);
        flat.push(self.maximumhotwaterheatingpower);
        flat.push(self.heatingsetpointtemperature);
        flat.push(self.coolingsetpointtemperature);

        for day_type in 0..BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS {
            flat.extend_from_slice(&self.metabolismprofile[day_type]);
        }

        for day_type in 0..BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS {
            flat.extend_from_slice(&self.applianceprofile[day_type]);
        }

        flat.push(if self.iter_safe { 1.0 } else { 0.0 });

        flat
    }
}

impl StateCodec for BuildingArchetypePrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "BUILDING_ARCHETYPE_PRM".to_string(),
            schema_version: BUILDING_ARCHETYPE_PRM_SCHEMA_VERSION,
            flat_len: BUILDING_ARCHETYPE_PRM_FLAT_LEN,
            field_names: building_archetype_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        BuildingArchetypePrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        BuildingArchetypePrm::to_flat(self)
    }
}

pub fn building_archetype_prm_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_building_archetype_prm_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn building_archetype_prm_schema_info() -> Result<BuildingArchetypePrmSchema, BridgeError> {
    let flat_len = building_archetype_prm_schema()?;
    let schema_version_runtime = building_archetype_prm_schema_version_runtime()?;
    let field_names = building_archetype_prm_field_names();

    if schema_version_runtime != BUILDING_ARCHETYPE_PRM_SCHEMA_VERSION
        || flat_len != field_names.len()
    {
        return Err(BridgeError::BadState);
    }

    Ok(BuildingArchetypePrmSchema {
        schema_version: BUILDING_ARCHETYPE_PRM_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn building_archetype_prm_field_names() -> Vec<String> {
    let mut names = vec![
        "buildingcount".to_string(),
        "occupants".to_string(),
        "hhs0".to_string(),
        "age_0_4".to_string(),
        "age_5_11".to_string(),
        "age_12_18".to_string(),
        "age_19_64".to_string(),
        "age_65plus".to_string(),
        "stebbs_height".to_string(),
        "footprintarea".to_string(),
        "wallexternalarea".to_string(),
        "ratiointernalvolume".to_string(),
        "wwr".to_string(),
        "wallthickness".to_string(),
        "walleffectiveconductivity".to_string(),
        "walldensity".to_string(),
        "wallcp".to_string(),
        "wallextthickness".to_string(),
        "wallexteffectiveconductivity".to_string(),
        "wallextdensity".to_string(),
        "wallextcp".to_string(),
        "walloutercapfrac".to_string(),
        "wallexternalemissivity".to_string(),
        "wallinternalemissivity".to_string(),
        "walltransmissivity".to_string(),
        "wallabsorbtivity".to_string(),
        "wallreflectivity".to_string(),
        "roofthickness".to_string(),
        "roofeffectiveconductivity".to_string(),
        "roofdensity".to_string(),
        "roofcp".to_string(),
        "roofextthickness".to_string(),
        "roofexteffectiveconductivity".to_string(),
        "roofextdensity".to_string(),
        "roofextcp".to_string(),
        "roofoutercapfrac".to_string(),
        "roofexternalemissivity".to_string(),
        "roofinternalemissivity".to_string(),
        "rooftransmissivity".to_string(),
        "roofabsorbtivity".to_string(),
        "roofreflectivity".to_string(),
        "floorthickness".to_string(),
        "groundflooreffectiveconductivity".to_string(),
        "groundfloordensity".to_string(),
        "groundfloorcp".to_string(),
        "windowthickness".to_string(),
        "windoweffectiveconductivity".to_string(),
        "windowdensity".to_string(),
        "windowcp".to_string(),
        "windowexternalemissivity".to_string(),
        "windowinternalemissivity".to_string(),
        "windowtransmissivity".to_string(),
        "windowabsorbtivity".to_string(),
        "windowreflectivity".to_string(),
        "internalmassdensity".to_string(),
        "internalmasscp".to_string(),
        "internalmassemissivity".to_string(),
        "maxheatingpower".to_string(),
        "watertankwatervolume".to_string(),
        "maximumhotwaterheatingpower".to_string(),
        "heatingsetpointtemperature".to_string(),
        "coolingsetpointtemperature".to_string(),
    ];

    for day_type in 0..BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS {
        for step in 0..BUILDING_ARCHETYPE_PRM_PROFILE_STEPS {
            names.push(format!("metabolismprofile.{step:03}.{}", day_type + 1));
        }
    }

    for day_type in 0..BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS {
        for step in 0..BUILDING_ARCHETYPE_PRM_PROFILE_STEPS {
            names.push(format!("applianceprofile.{step:03}.{}", day_type + 1));
        }
    }

    names.push("iter_safe".to_string());

    names
}

pub fn building_archetype_prm_schema_version() -> u32 {
    BUILDING_ARCHETYPE_PRM_SCHEMA_VERSION
}

pub fn building_archetype_prm_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_building_archetype_prm_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn building_archetype_prm_field_index(name: &str) -> Option<usize> {
    let names = building_archetype_prm_field_names();
    field_index(&names, name)
}

pub fn building_archetype_prm_to_map(state: &BuildingArchetypePrm) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn building_archetype_prm_to_ordered_values(state: &BuildingArchetypePrm) -> Vec<f64> {
    state.to_flat()
}

pub fn building_archetype_prm_from_ordered_values(
    values: &[f64],
) -> Result<BuildingArchetypePrm, BridgeError> {
    BuildingArchetypePrm::from_flat(values)
}

pub fn building_archetype_prm_to_values_payload(
    state: &BuildingArchetypePrm,
) -> BuildingArchetypePrmValuesPayload {
    to_values_payload(state)
}

pub fn building_archetype_prm_from_values_payload(
    payload: &BuildingArchetypePrmValuesPayload,
) -> Result<BuildingArchetypePrm, BridgeError> {
    from_values_payload(payload)
}

pub fn building_archetype_prm_from_map(
    values: &BTreeMap<String, f64>,
) -> Result<BuildingArchetypePrm, BridgeError> {
    let default_state = building_archetype_prm_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn building_archetype_prm_default_from_fortran() -> Result<BuildingArchetypePrm, BridgeError> {
    let n_flat = building_archetype_prm_schema()?;
    if n_flat != BUILDING_ARCHETYPE_PRM_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_building_archetype_prm_default(
            flat.as_mut_ptr(),
            n_flat as i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    BuildingArchetypePrm::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = building_archetype_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, BUILDING_ARCHETYPE_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = building_archetype_prm_default_from_fortran()
            .expect("default state should be available");
        assert_eq!(state, BuildingArchetypePrm::default());

        let state2 = BuildingArchetypePrm::from_flat(&state.to_flat())
            .expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = building_archetype_prm_default_from_fortran()
            .expect("default state should be available");
        let mut mapped = building_archetype_prm_to_map(&state);
        mapped.insert("buildingcount".to_string(), 12.0);
        mapped.insert("metabolismprofile.012.2".to_string(), 55.0);
        mapped.insert("iter_safe".to_string(), 0.0);

        let updated =
            building_archetype_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.buildingcount - 12.0).abs() < 1.0e-12);
        assert!((updated.metabolismprofile[1][12] - 55.0).abs() < 1.0e-12);
        assert!(!updated.iter_safe);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = building_archetype_prm_default_from_fortran()
            .expect("default state should be available");
        let payload = building_archetype_prm_to_values_payload(&state);
        let recovered = building_archetype_prm_from_values_payload(&payload)
            .expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = BuildingArchetypePrmValuesPayload {
            schema_version: BUILDING_ARCHETYPE_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = building_archetype_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
