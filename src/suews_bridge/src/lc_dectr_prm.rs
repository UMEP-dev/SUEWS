use crate::bioco2::{bioco2_prm_field_names, BioCo2Prm};
use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;
use crate::lai::{lai_prm_field_names, LaiPrm};
use crate::ohm_prm::{ohm_prm_field_names, OhmPrm};
use crate::soil::{soil_prm_field_names, SoilPrm};
use crate::water_dist::{water_dist_prm_field_names, WaterDistPrm};

pub const LC_DECTR_PRM_FLAT_LEN: usize = 61;
pub const LC_DECTR_PRM_SCHEMA_VERSION: u32 = 1;

pub type LcDectrPrmSchema = crate::codec::SimpleSchema;

pub type LcDectrPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct LcDectrPrm {
    pub sfr: f64,
    pub emis: f64,
    pub fai_deciduous_tree: f64,
    pub height_deciduous_tree: f64,
    pub porosity_min_deciduous: f64,
    pub porosity_max_deciduous: f64,
    pub alb_min: f64,
    pub alb_max: f64,
    pub ohm: OhmPrm,
    pub soil: SoilPrm,
    pub state_limit: f64,
    pub capacity_max_deciduous: f64,
    pub capacity_min_deciduous: f64,
    pub irrfracdectr: f64,
    pub wet_threshold: f64,
    pub bioco2: BioCo2Prm,
    pub max_conductance: f64,
    pub lai: LaiPrm,
    pub waterdist: WaterDistPrm,
}

impl Default for LcDectrPrm {
    fn default() -> Self {
        Self {
            sfr: 0.0,
            emis: 0.0,
            fai_deciduous_tree: 0.0,
            height_deciduous_tree: 0.0,
            porosity_min_deciduous: 0.0,
            porosity_max_deciduous: 0.0,
            alb_min: 0.0,
            alb_max: 0.0,
            ohm: OhmPrm::default(),
            soil: SoilPrm::default(),
            state_limit: 0.0,
            capacity_max_deciduous: 0.0,
            capacity_min_deciduous: 0.0,
            irrfracdectr: 0.0,
            wet_threshold: 0.0,
            bioco2: BioCo2Prm::default(),
            max_conductance: 0.0,
            lai: LaiPrm::default(),
            waterdist: WaterDistPrm::default(),
        }
    }
}

impl LcDectrPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, LC_DECTR_PRM_FLAT_LEN)?;

        Ok(Self {
            sfr: flat[0],
            emis: flat[1],
            fai_deciduous_tree: flat[2],
            height_deciduous_tree: flat[3],
            porosity_min_deciduous: flat[4],
            porosity_max_deciduous: flat[5],
            alb_min: flat[6],
            alb_max: flat[7],
            ohm: OhmPrm::from_flat(&flat[8..25])?,
            soil: SoilPrm::from_flat(&flat[25..28])?,
            state_limit: flat[28],
            capacity_max_deciduous: flat[29],
            capacity_min_deciduous: flat[30],
            irrfracdectr: flat[31],
            wet_threshold: flat[32],
            bioco2: BioCo2Prm::from_flat(&flat[33..41])?,
            max_conductance: flat[41],
            lai: LaiPrm::from_flat(&flat[42..53])?,
            waterdist: WaterDistPrm::from_flat(&flat[53..61])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = vec![
            self.sfr,
            self.emis,
            self.fai_deciduous_tree,
            self.height_deciduous_tree,
            self.porosity_min_deciduous,
            self.porosity_max_deciduous,
            self.alb_min,
            self.alb_max,
        ];
        out.extend(self.ohm.to_flat());
        out.extend(self.soil.to_flat());
        out.push(self.state_limit);
        out.push(self.capacity_max_deciduous);
        out.push(self.capacity_min_deciduous);
        out.push(self.irrfracdectr);
        out.push(self.wet_threshold);
        out.extend(self.bioco2.to_flat());
        out.push(self.max_conductance);
        out.extend(self.lai.to_flat());
        out.extend(self.waterdist.to_flat());
        out
    }
}

impl StateCodec for LcDectrPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "LC_DECTR_PRM".to_string(),
            schema_version: LC_DECTR_PRM_SCHEMA_VERSION,
            flat_len: LC_DECTR_PRM_FLAT_LEN,
            field_names: lc_dectr_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        LcDectrPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        LcDectrPrm::to_flat(self)
    }
}

pub fn lc_dectr_prm_field_names() -> Vec<String> {
    let mut names = vec![
        "sfr".to_string(),
        "emis".to_string(),
        "faidectree".to_string(),
        "dectreeh".to_string(),
        "pormin_dec".to_string(),
        "pormax_dec".to_string(),
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
    names.push("capmax_dec".to_string());
    names.push("capmin_dec".to_string());
    names.push("irrfracdectr".to_string());
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
    prefix = lc_dectr_prm,
    state_type = LcDectrPrm,
    schema_type = LcDectrPrmSchema,
    payload_type = LcDectrPrmValuesPayload,
    flat_len_const = LC_DECTR_PRM_FLAT_LEN,
    schema_version_const = LC_DECTR_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_lc_dectr_prm_len,
    ffi_schema_version_fn = ffi::suews_lc_dectr_prm_schema_version,
    ffi_default_fn = ffi::suews_lc_dectr_prm_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = lc_dectr_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, LC_DECTR_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = lc_dectr_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, LcDectrPrm::default());

        let state2 =
            LcDectrPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = lc_dectr_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = lc_dectr_prm_to_map(&state);
        mapped.insert("capmax_dec".to_string(), 1.2);
        mapped.insert("bioco2.beta_bioco2".to_string(), 0.9);
        mapped.insert("lai.laimax".to_string(), 5.5);
        mapped.insert("waterdist.to_dectr".to_string(), 0.4);

        let updated = lc_dectr_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.capacity_max_deciduous - 1.2).abs() < 1.0e-12);
        assert!((updated.bioco2.beta_bio_co2 - 0.9).abs() < 1.0e-12);
        assert!((updated.lai.lai_max - 5.5).abs() < 1.0e-12);
        assert!((updated.waterdist.to_dectr - 0.4).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = lc_dectr_prm_default_from_fortran().expect("default state should be available");
        let payload = lc_dectr_prm_to_values_payload(&state);
        let recovered =
            lc_dectr_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = LcDectrPrmValuesPayload {
            schema_version: LC_DECTR_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = lc_dectr_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
