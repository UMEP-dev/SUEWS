use crate::bioco2::{bioco2_prm_field_names, BioCo2Prm};
use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;
use crate::lai::{lai_prm_field_names, LaiPrm};
use crate::ohm_prm::{ohm_prm_field_names, OhmPrm};
use crate::soil::{soil_prm_field_names, SoilPrm};
use crate::water_dist::{water_dist_prm_field_names, WaterDistPrm};

pub const LC_GRASS_PRM_FLAT_LEN: usize = 55;
pub const LC_GRASS_PRM_SCHEMA_VERSION: u32 = 1;

pub type LcGrassPrmSchema = crate::codec::SimpleSchema;

pub type LcGrassPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct LcGrassPrm {
    pub sfr: f64,
    pub emis: f64,
    pub alb_min: f64,
    pub alb_max: f64,
    pub ohm: OhmPrm,
    pub soil: SoilPrm,
    pub statelimit: f64,
    pub irrfracgrass: f64,
    pub wetthresh: f64,
    pub bioco2: BioCo2Prm,
    pub maxconductance: f64,
    pub lai: LaiPrm,
    pub waterdist: WaterDistPrm,
}

impl Default for LcGrassPrm {
    fn default() -> Self {
        Self {
            sfr: 0.0,
            emis: 0.0,
            alb_min: 0.0,
            alb_max: 0.0,
            ohm: OhmPrm::default(),
            soil: SoilPrm::default(),
            statelimit: 0.0,
            irrfracgrass: 0.0,
            wetthresh: 0.0,
            bioco2: BioCo2Prm::default(),
            maxconductance: 0.0,
            lai: LaiPrm::default(),
            waterdist: WaterDistPrm::default(),
        }
    }
}

impl LcGrassPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, LC_GRASS_PRM_FLAT_LEN)?;

        Ok(Self {
            sfr: flat[0],
            emis: flat[1],
            alb_min: flat[2],
            alb_max: flat[3],
            ohm: OhmPrm::from_flat(&flat[4..21])?,
            soil: SoilPrm::from_flat(&flat[21..24])?,
            statelimit: flat[24],
            irrfracgrass: flat[25],
            wetthresh: flat[26],
            bioco2: BioCo2Prm::from_flat(&flat[27..35])?,
            maxconductance: flat[35],
            lai: LaiPrm::from_flat(&flat[36..47])?,
            waterdist: WaterDistPrm::from_flat(&flat[47..55])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = vec![self.sfr, self.emis, self.alb_min, self.alb_max];
        out.extend(self.ohm.to_flat());
        out.extend(self.soil.to_flat());
        out.push(self.statelimit);
        out.push(self.irrfracgrass);
        out.push(self.wetthresh);
        out.extend(self.bioco2.to_flat());
        out.push(self.maxconductance);
        out.extend(self.lai.to_flat());
        out.extend(self.waterdist.to_flat());
        out
    }
}

impl StateCodec for LcGrassPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "LC_GRASS_PRM".to_string(),
            schema_version: LC_GRASS_PRM_SCHEMA_VERSION,
            flat_len: LC_GRASS_PRM_FLAT_LEN,
            field_names: lc_grass_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        LcGrassPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        LcGrassPrm::to_flat(self)
    }
}

pub fn lc_grass_prm_field_names() -> Vec<String> {
    let mut names = vec![
        "sfr".to_string(),
        "emis".to_string(),
        "alb_min".to_string(),
        "alb_max".to_string(),
    ];

    for field in ohm_prm_field_names() {
        names.push(format!("ohm.{field}"));
    }
    for field in soil_prm_field_names() {
        names.push(format!("soil.{field}"));
    }

    names.push("statelimit".to_string());
    names.push("irrfracgrass".to_string());
    names.push("wetthresh".to_string());

    for field in bioco2_prm_field_names() {
        names.push(format!("bioco2.{field}"));
    }

    names.push("maxconductance".to_string());

    for field in lai_prm_field_names() {
        names.push(format!("lai.{field}"));
    }
    for field in water_dist_prm_field_names() {
        names.push(format!("waterdist.{field}"));
    }

    names
}

crate::codec::impl_state_module_fns! {
    prefix = lc_grass_prm,
    state_type = LcGrassPrm,
    schema_type = LcGrassPrmSchema,
    payload_type = LcGrassPrmValuesPayload,
    flat_len_const = LC_GRASS_PRM_FLAT_LEN,
    schema_version_const = LC_GRASS_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_lc_grass_prm_len,
    ffi_schema_version_fn = ffi::suews_lc_grass_prm_schema_version,
    ffi_default_fn = ffi::suews_lc_grass_prm_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = lc_grass_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, LC_GRASS_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = lc_grass_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, LcGrassPrm::default());

        let state2 =
            LcGrassPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = lc_grass_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = lc_grass_prm_to_map(&state);
        mapped.insert("irrfracgrass".to_string(), 0.25);
        mapped.insert("bioco2.theta_bioco2".to_string(), 0.85);
        mapped.insert("lai.laitype".to_string(), 1.0);
        mapped.insert("waterdist.to_grass".to_string(), 0.45);

        let updated = lc_grass_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.irrfracgrass - 0.25).abs() < 1.0e-12);
        assert!((updated.bioco2.theta_bioco2 - 0.85).abs() < 1.0e-12);
        assert_eq!(updated.lai.laitype, 1);
        assert!((updated.waterdist.to_grass - 0.45).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = lc_grass_prm_default_from_fortran().expect("default state should be available");
        let payload = lc_grass_prm_to_values_payload(&state);
        let recovered =
            lc_grass_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = LcGrassPrmValuesPayload {
            schema_version: LC_GRASS_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = lc_grass_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
