use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;
use crate::ohm_prm::{ohm_prm_field_names, OhmPrm};
use crate::soil::{soil_prm_field_names, SoilPrm};

pub const LC_WATER_PRM_FLAT_LEN: usize = 26;
pub const LC_WATER_PRM_SCHEMA_VERSION: u32 = 1;

pub type LcWaterPrmSchema = crate::codec::SimpleSchema;

pub type LcWaterPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct LcWaterPrm {
    pub sfr: f64,
    pub emis: f64,
    pub ohm: OhmPrm,
    pub soil: SoilPrm,
    pub statelimit: f64,
    pub irrfracwater: f64,
    pub wetthresh: f64,
    pub flowchange: f64,
}

impl Default for LcWaterPrm {
    fn default() -> Self {
        Self {
            sfr: 0.0,
            emis: 0.0,
            ohm: OhmPrm::default(),
            soil: SoilPrm::default(),
            statelimit: 0.0,
            irrfracwater: 0.0,
            wetthresh: 0.0,
            flowchange: 0.0,
        }
    }
}

impl LcWaterPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, LC_WATER_PRM_FLAT_LEN)?;

        Ok(Self {
            sfr: flat[0],
            emis: flat[1],
            ohm: OhmPrm::from_flat(&flat[2..19])?,
            soil: SoilPrm::from_flat(&flat[19..22])?,
            statelimit: flat[22],
            irrfracwater: flat[23],
            wetthresh: flat[24],
            flowchange: flat[25],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = vec![self.sfr, self.emis];
        out.extend(self.ohm.to_flat());
        out.extend(self.soil.to_flat());
        out.push(self.statelimit);
        out.push(self.irrfracwater);
        out.push(self.wetthresh);
        out.push(self.flowchange);
        out
    }
}

impl StateCodec for LcWaterPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "LC_WATER_PRM".to_string(),
            schema_version: LC_WATER_PRM_SCHEMA_VERSION,
            flat_len: LC_WATER_PRM_FLAT_LEN,
            field_names: lc_water_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        LcWaterPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        LcWaterPrm::to_flat(self)
    }
}

pub fn lc_water_prm_field_names() -> Vec<String> {
    let mut names = vec!["sfr".to_string(), "emis".to_string()];

    for field in ohm_prm_field_names() {
        names.push(format!("ohm.{field}"));
    }
    for field in soil_prm_field_names() {
        names.push(format!("soil.{field}"));
    }

    names.push("statelimit".to_string());
    names.push("irrfracwater".to_string());
    names.push("wetthresh".to_string());
    names.push("flowchange".to_string());

    names
}

crate::codec::impl_state_module_fns! {
    prefix = lc_water_prm,
    state_type = LcWaterPrm,
    schema_type = LcWaterPrmSchema,
    payload_type = LcWaterPrmValuesPayload,
    flat_len_const = LC_WATER_PRM_FLAT_LEN,
    schema_version_const = LC_WATER_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_lc_water_prm_len,
    ffi_schema_version_fn = ffi::suews_lc_water_prm_schema_version,
    ffi_default_fn = ffi::suews_lc_water_prm_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = lc_water_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, LC_WATER_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = lc_water_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, LcWaterPrm::default());

        let state2 =
            LcWaterPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = lc_water_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = lc_water_prm_to_map(&state);
        mapped.insert("irrfracwater".to_string(), 0.15);
        mapped.insert("ohm.ohm_coef_lc_1.summer_wet".to_string(), 0.9);
        mapped.insert("flowchange".to_string(), -0.2);

        let updated = lc_water_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.irrfracwater - 0.15).abs() < 1.0e-12);
        assert!((updated.ohm.ohm_coef_lc[0].summer_wet - 0.9).abs() < 1.0e-12);
        assert!((updated.flowchange + 0.2).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = lc_water_prm_default_from_fortran().expect("default state should be available");
        let payload = lc_water_prm_to_values_payload(&state);
        let recovered =
            lc_water_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = LcWaterPrmValuesPayload {
            schema_version: LC_WATER_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = lc_water_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
