use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const SUEWS_CONFIG_FLAT_LEN: usize = 21;
pub const SUEWS_CONFIG_SCHEMA_VERSION: u32 = 1;

pub type SuewsConfigSchema = crate::codec::SimpleSchema;

pub type SuewsConfigValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuewsConfig {
    pub rsl_method: i32,
    pub emissions_method: i32,
    pub rough_len_heat_method: i32,
    pub rough_len_mom_method: i32,
    pub fai_method: i32,
    pub smd_method: i32,
    pub water_use_method: i32,
    pub net_radiation_method: i32,
    pub stability_method: i32,
    pub storage_heat_method: i32,
    pub diagnose: i32,
    pub snow_use: i32,
    pub use_sw_direct_albedo: bool,
    pub ohm_inc_qf: i32,
    pub diag_qs: i32,
    pub evap_method: i32,
    pub lai_method: i32,
    pub rsl_level: i32,
    pub stebbs_method: i32,
    pub rc_method: i32,
    pub flag_test: bool,
}

impl Default for SuewsConfig {
    fn default() -> Self {
        Self {
            rsl_method: 0,
            emissions_method: 0,
            rough_len_heat_method: 0,
            rough_len_mom_method: 0,
            fai_method: 0,
            smd_method: 0,
            water_use_method: 0,
            net_radiation_method: 0,
            stability_method: 0,
            storage_heat_method: 0,
            diagnose: 0,
            snow_use: 0,
            use_sw_direct_albedo: false,
            ohm_inc_qf: 0,
            diag_qs: 0,
            evap_method: 0,
            lai_method: 0,
            rsl_level: 0,
            stebbs_method: 0,
            rc_method: 0,
            flag_test: false,
        }
    }
}

fn decode_int(value: f64) -> Result<i32, BridgeError> {
    if !value.is_finite() {
        return Err(BridgeError::BadState);
    }

    let rounded = value.round();
    if (value - rounded).abs() > 1.0e-9 {
        return Err(BridgeError::BadState);
    }

    if rounded < i32::MIN as f64 || rounded > i32::MAX as f64 {
        return Err(BridgeError::BadState);
    }

    Ok(rounded as i32)
}

impl SuewsConfig {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, SUEWS_CONFIG_FLAT_LEN)?;

        Ok(Self {
            rsl_method: decode_int(flat[0])?,
            emissions_method: decode_int(flat[1])?,
            rough_len_heat_method: decode_int(flat[2])?,
            rough_len_mom_method: decode_int(flat[3])?,
            fai_method: decode_int(flat[4])?,
            smd_method: decode_int(flat[5])?,
            water_use_method: decode_int(flat[6])?,
            net_radiation_method: decode_int(flat[7])?,
            stability_method: decode_int(flat[8])?,
            storage_heat_method: decode_int(flat[9])?,
            diagnose: decode_int(flat[10])?,
            snow_use: decode_int(flat[11])?,
            use_sw_direct_albedo: flat[12] >= 0.5,
            ohm_inc_qf: decode_int(flat[13])?,
            diag_qs: decode_int(flat[14])?,
            evap_method: decode_int(flat[15])?,
            lai_method: decode_int(flat[16])?,
            rsl_level: decode_int(flat[17])?,
            stebbs_method: decode_int(flat[18])?,
            rc_method: decode_int(flat[19])?,
            flag_test: flat[20] >= 0.5,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        vec![
            self.rsl_method as f64,
            self.emissions_method as f64,
            self.rough_len_heat_method as f64,
            self.rough_len_mom_method as f64,
            self.fai_method as f64,
            self.smd_method as f64,
            self.water_use_method as f64,
            self.net_radiation_method as f64,
            self.stability_method as f64,
            self.storage_heat_method as f64,
            self.diagnose as f64,
            self.snow_use as f64,
            if self.use_sw_direct_albedo { 1.0 } else { 0.0 },
            self.ohm_inc_qf as f64,
            self.diag_qs as f64,
            self.evap_method as f64,
            self.lai_method as f64,
            self.rsl_level as f64,
            self.stebbs_method as f64,
            self.rc_method as f64,
            if self.flag_test { 1.0 } else { 0.0 },
        ]
    }
}

impl StateCodec for SuewsConfig {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "SUEWS_CONFIG".to_string(),
            schema_version: SUEWS_CONFIG_SCHEMA_VERSION,
            flat_len: SUEWS_CONFIG_FLAT_LEN,
            field_names: suews_config_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        SuewsConfig::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        SuewsConfig::to_flat(self)
    }
}

pub fn suews_config_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_config_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn suews_config_schema_info() -> Result<SuewsConfigSchema, BridgeError> {
    let flat_len = suews_config_schema()?;
    let schema_version_runtime = suews_config_schema_version_runtime()?;
    let field_names = suews_config_field_names();

    if schema_version_runtime != SUEWS_CONFIG_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(SuewsConfigSchema {
        schema_version: SUEWS_CONFIG_SCHEMA_VERSION,
        flat_len,
        field_names,
    })
}

pub fn suews_config_field_names() -> Vec<String> {
    vec![
        "rsl_method".to_string(),
        "emissions_method".to_string(),
        "rough_len_heat_method".to_string(),
        "rough_len_mom_method".to_string(),
        "fai_method".to_string(),
        "smd_method".to_string(),
        "water_use_method".to_string(),
        "net_radiation_method".to_string(),
        "stability_method".to_string(),
        "storage_heat_method".to_string(),
        "diagnose".to_string(),
        "snow_use".to_string(),
        "use_sw_direct_albedo".to_string(),
        "ohm_inc_qf".to_string(),
        "diag_qs".to_string(),
        "evap_method".to_string(),
        "lai_method".to_string(),
        "rsl_level".to_string(),
        "stebbs_method".to_string(),
        "rc_method".to_string(),
        "flag_test".to_string(),
    ]
}

pub fn suews_config_schema_version() -> u32 {
    SUEWS_CONFIG_SCHEMA_VERSION
}

pub fn suews_config_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_config_schema_version(&mut schema_version as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn suews_config_field_index(name: &str) -> Option<usize> {
    let names = suews_config_field_names();
    field_index(&names, name)
}

pub fn suews_config_to_map(state: &SuewsConfig) -> BTreeMap<String, f64> {
    to_map(state)
}

pub fn suews_config_to_ordered_values(state: &SuewsConfig) -> Vec<f64> {
    state.to_flat()
}

pub fn suews_config_from_ordered_values(values: &[f64]) -> Result<SuewsConfig, BridgeError> {
    SuewsConfig::from_flat(values)
}

pub fn suews_config_to_values_payload(state: &SuewsConfig) -> SuewsConfigValuesPayload {
    to_values_payload(state)
}

pub fn suews_config_from_values_payload(
    payload: &SuewsConfigValuesPayload,
) -> Result<SuewsConfig, BridgeError> {
    from_values_payload(payload)
}

pub fn suews_config_from_map(values: &BTreeMap<String, f64>) -> Result<SuewsConfig, BridgeError> {
    let default_state = suews_config_default_from_fortran()?;
    from_map(values, &default_state)
}

pub fn suews_config_default_from_fortran() -> Result<SuewsConfig, BridgeError> {
    let n_flat = suews_config_schema()?;
    if n_flat != SUEWS_CONFIG_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_config_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    SuewsConfig::from_flat(&flat)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = suews_config_schema().expect("schema call should succeed");
        assert_eq!(n_flat, SUEWS_CONFIG_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = suews_config_default_from_fortran().expect("default state should be available");
        assert_eq!(state, SuewsConfig::default());

        let state2 =
            SuewsConfig::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = suews_config_default_from_fortran().expect("default state should be available");
        let mut mapped = suews_config_to_map(&state);
        mapped.insert("water_use_method".to_string(), 2.0);
        mapped.insert("use_sw_direct_albedo".to_string(), 1.0);
        mapped.insert("flag_test".to_string(), 1.0);

        let updated = suews_config_from_map(&mapped).expect("map to state should succeed");
        assert_eq!(updated.water_use_method, 2);
        assert!(updated.use_sw_direct_albedo);
        assert!(updated.flag_test);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = suews_config_default_from_fortran().expect("default state should be available");
        let payload = suews_config_to_values_payload(&state);
        let recovered =
            suews_config_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = SuewsConfigValuesPayload {
            schema_version: SUEWS_CONFIG_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = suews_config_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn from_flat_rejects_non_integer_method_fields() {
        let mut flat = vec![0.0_f64; SUEWS_CONFIG_FLAT_LEN];
        flat[6] = 2.5;
        let err = SuewsConfig::from_flat(&flat).expect_err("fractional integer should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
