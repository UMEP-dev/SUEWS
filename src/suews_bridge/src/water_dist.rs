use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const WATER_DIST_PRM_FLAT_LEN: usize = 8;
pub const WATER_DIST_PRM_SCHEMA_VERSION: u32 = 1;

pub type WaterDistPrmSchema = crate::codec::SimpleSchema;

pub type WaterDistPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct WaterDistPrm {
    pub to_paved: f64,
    pub to_bldg: f64,
    pub to_evetr: f64,
    pub to_dectr: f64,
    pub to_grass: f64,
    pub to_bsoil: f64,
    pub to_water: f64,
    pub to_soilstore: f64,
}

impl Default for WaterDistPrm {
    fn default() -> Self {
        Self {
            to_paved: 0.0,
            to_bldg: 0.0,
            to_evetr: 0.0,
            to_dectr: 0.0,
            to_grass: 0.0,
            to_bsoil: 0.0,
            to_water: 0.0,
            to_soilstore: 0.0,
        }
    }
}

impl WaterDistPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, WATER_DIST_PRM_FLAT_LEN)?;
        Ok(Self {
            to_paved: flat[0],
            to_bldg: flat[1],
            to_evetr: flat[2],
            to_dectr: flat[3],
            to_grass: flat[4],
            to_bsoil: flat[5],
            to_water: flat[6],
            to_soilstore: flat[7],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.to_paved,
            self.to_bldg,
            self.to_evetr,
            self.to_dectr,
            self.to_grass,
            self.to_bsoil,
            self.to_water,
            self.to_soilstore,
        ]
    }
}

impl StateCodec for WaterDistPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "WATER_DIST_PRM".to_string(),
            schema_version: WATER_DIST_PRM_SCHEMA_VERSION,
            flat_len: WATER_DIST_PRM_FLAT_LEN,
            field_names: water_dist_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        WaterDistPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        WaterDistPrm::to_flat(self)
    }
}

pub fn water_dist_prm_field_names() -> Vec<String> {
    vec![
        "to_paved".to_string(),
        "to_bldg".to_string(),
        "to_evetr".to_string(),
        "to_dectr".to_string(),
        "to_grass".to_string(),
        "to_bsoil".to_string(),
        "to_water".to_string(),
        "to_soilstore".to_string(),
    ]
}

crate::codec::impl_state_module_fns! {
    prefix = water_dist_prm,
    state_type = WaterDistPrm,
    schema_type = WaterDistPrmSchema,
    payload_type = WaterDistPrmValuesPayload,
    flat_len_const = WATER_DIST_PRM_FLAT_LEN,
    schema_version_const = WATER_DIST_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_water_dist_prm_len,
    ffi_schema_version_fn = ffi::suews_water_dist_prm_schema_version,
    ffi_default_fn = ffi::suews_water_dist_prm_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = water_dist_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, WATER_DIST_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            water_dist_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, WaterDistPrm::default());

        let state2 =
            WaterDistPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            water_dist_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = water_dist_prm_to_map(&state);
        mapped.insert("to_paved".to_string(), 0.3);
        mapped.insert("to_soilstore".to_string(), 0.1);

        let updated = water_dist_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.to_paved - 0.3).abs() < 1.0e-12);
        assert!((updated.to_soilstore - 0.1).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            water_dist_prm_default_from_fortran().expect("default state should be available");
        let payload = water_dist_prm_to_values_payload(&state);
        let recovered =
            water_dist_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = WaterDistPrmValuesPayload {
            schema_version: WATER_DIST_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = water_dist_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
