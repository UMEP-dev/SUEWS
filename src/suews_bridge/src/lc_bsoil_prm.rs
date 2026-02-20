use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;
use crate::ohm_prm::{ohm_prm_field_names, OhmPrm};
use crate::soil::{soil_prm_field_names, SoilPrm};
use crate::water_dist::{water_dist_prm_field_names, WaterDistPrm};

pub const LC_BSOIL_PRM_FLAT_LEN: usize = 33;
pub const LC_BSOIL_PRM_SCHEMA_VERSION: u32 = 1;

pub type LcBsoilPrmSchema = crate::codec::SimpleSchema;

pub type LcBsoilPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct LcBsoilPrm {
    pub sfr: f64,
    pub emis: f64,
    pub ohm: OhmPrm,
    pub soil: SoilPrm,
    pub statelimit: f64,
    pub irrfracbsoil: f64,
    pub wetthresh: f64,
    pub waterdist: WaterDistPrm,
}

impl Default for LcBsoilPrm {
    fn default() -> Self {
        Self {
            sfr: 0.0,
            emis: 0.0,
            ohm: OhmPrm::default(),
            soil: SoilPrm::default(),
            statelimit: 0.0,
            irrfracbsoil: 0.0,
            wetthresh: 0.0,
            waterdist: WaterDistPrm::default(),
        }
    }
}

impl LcBsoilPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, LC_BSOIL_PRM_FLAT_LEN)?;

        Ok(Self {
            sfr: flat[0],
            emis: flat[1],
            ohm: OhmPrm::from_flat(&flat[2..19])?,
            soil: SoilPrm::from_flat(&flat[19..22])?,
            statelimit: flat[22],
            irrfracbsoil: flat[23],
            wetthresh: flat[24],
            waterdist: WaterDistPrm::from_flat(&flat[25..33])?,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = vec![self.sfr, self.emis];
        out.extend(self.ohm.to_flat());
        out.extend(self.soil.to_flat());
        out.push(self.statelimit);
        out.push(self.irrfracbsoil);
        out.push(self.wetthresh);
        out.extend(self.waterdist.to_flat());
        out
    }
}

impl StateCodec for LcBsoilPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "LC_BSOIL_PRM".to_string(),
            schema_version: LC_BSOIL_PRM_SCHEMA_VERSION,
            flat_len: LC_BSOIL_PRM_FLAT_LEN,
            field_names: lc_bsoil_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        LcBsoilPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        LcBsoilPrm::to_flat(self)
    }
}

pub fn lc_bsoil_prm_field_names() -> Vec<String> {
    let mut names = vec!["sfr".to_string(), "emis".to_string()];

    for field in ohm_prm_field_names() {
        names.push(format!("ohm.{field}"));
    }
    for field in soil_prm_field_names() {
        names.push(format!("soil.{field}"));
    }

    names.push("statelimit".to_string());
    names.push("irrfracbsoil".to_string());
    names.push("wetthresh".to_string());

    for field in water_dist_prm_field_names() {
        names.push(format!("waterdist.{field}"));
    }

    names
}

crate::codec::impl_state_module_fns! {
    prefix = lc_bsoil_prm,
    state_type = LcBsoilPrm,
    schema_type = LcBsoilPrmSchema,
    payload_type = LcBsoilPrmValuesPayload,
    flat_len_const = LC_BSOIL_PRM_FLAT_LEN,
    schema_version_const = LC_BSOIL_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_lc_bsoil_prm_len,
    ffi_schema_version_fn = ffi::suews_lc_bsoil_prm_schema_version,
    ffi_default_fn = ffi::suews_lc_bsoil_prm_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = lc_bsoil_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, LC_BSOIL_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = lc_bsoil_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, LcBsoilPrm::default());

        let state2 =
            LcBsoilPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = lc_bsoil_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = lc_bsoil_prm_to_map(&state);
        mapped.insert("statelimit".to_string(), 20.0);
        mapped.insert("soil.soilstorecap".to_string(), 120.0);
        mapped.insert("waterdist.to_bsoil".to_string(), 0.6);

        let updated = lc_bsoil_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.statelimit - 20.0).abs() < 1.0e-12);
        assert!((updated.soil.soilstorecap - 120.0).abs() < 1.0e-12);
        assert!((updated.waterdist.to_bsoil - 0.6).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = lc_bsoil_prm_default_from_fortran().expect("default state should be available");
        let payload = lc_bsoil_prm_to_values_payload(&state);
        let recovered =
            lc_bsoil_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = LcBsoilPrmValuesPayload {
            schema_version: LC_BSOIL_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = lc_bsoil_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
