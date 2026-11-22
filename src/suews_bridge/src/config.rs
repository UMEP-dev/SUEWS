use crate::codec::{
    field_index, from_map, from_values_payload, to_map, to_values_payload, validate_flat_len,
    StateCodec, TypeSchema, ValuesPayload,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const SUEWS_CONFIG_FLAT_LEN: usize = 29;
pub const SUEWS_CONFIG_SCHEMA_VERSION: u32 = 5;

pub type SuewsConfigSchema = crate::codec::SimpleSchema;

pub type SuewsConfigValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
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
    pub setpoint_method: i32,
    pub flag_test: bool,
    pub kdown_split_method: i32,
    pub forcing_timestamp_reference: i32,
    pub ehc_surface_coupling: i32,
    pub control_ehc_initial_profile: bool,
    pub resistance_ehc_indoor_roof: f64,
    pub resistance_ehc_indoor_wall: f64,
    pub control_ehc_stability: i32,
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
            setpoint_method: 0,
            flag_test: false,
            kdown_split_method: 3,
            forcing_timestamp_reference: 0,
            ehc_surface_coupling: 1,
            control_ehc_initial_profile: true,
            resistance_ehc_indoor_roof: 0.10,
            resistance_ehc_indoor_wall: 0.13,
            control_ehc_stability: 0,
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
        if flat[26..28].iter().any(|v| !v.is_finite() || *v < 0.0) {
            return Err(BridgeError::BadState);
        }
        if flat[24] != 0.0 && flat[24] != 1.0 {
            return Err(BridgeError::BadState);
        }
        let control_ehc_stability = decode_int(flat[28])?;
        if !(0..=2).contains(&control_ehc_stability) {
            return Err(BridgeError::BadState);
        }

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
            setpoint_method: decode_int(flat[20])?,
            flag_test: flat[21] >= 0.5,
            kdown_split_method: decode_int(flat[22])?,
            forcing_timestamp_reference: decode_int(flat[23])?,
            ehc_surface_coupling: decode_int(flat[24])?,
            control_ehc_initial_profile: flat[25] >= 0.5,
            resistance_ehc_indoor_roof: flat[26],
            resistance_ehc_indoor_wall: flat[27],
            control_ehc_stability,
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
            self.setpoint_method as f64,
            if self.flag_test { 1.0 } else { 0.0 },
            self.kdown_split_method as f64,
            self.forcing_timestamp_reference as f64,
            self.ehc_surface_coupling as f64,
            if self.control_ehc_initial_profile {
                1.0
            } else {
                0.0
            },
            self.resistance_ehc_indoor_roof,
            self.resistance_ehc_indoor_wall,
            self.control_ehc_stability as f64,
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
        "setpoint_method".to_string(),
        "flag_test".to_string(),
        "kdown_split_method".to_string(),
        "forcing_timestamp_reference".to_string(),
        "ehc_surface_coupling".to_string(),
        "control_ehc_initial_profile".to_string(),
        "resistance_ehc_indoor_roof".to_string(),
        "resistance_ehc_indoor_wall".to_string(),
        "control_ehc_stability".to_string(),
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

// contract: C-CODEC
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
        mapped.insert("control_ehc_initial_profile".to_string(), 0.0);
        mapped.insert("resistance_ehc_indoor_roof".to_string(), 0.25);
        mapped.insert("resistance_ehc_indoor_wall".to_string(), 0.30);
        mapped.insert("control_ehc_stability".to_string(), 2.0);

        let updated = suews_config_from_map(&mapped).expect("map to state should succeed");
        assert_eq!(updated.water_use_method, 2);
        assert!(updated.use_sw_direct_albedo);
        assert!(updated.flag_test);
        assert!(!updated.control_ehc_initial_profile);
        assert_eq!(updated.resistance_ehc_indoor_roof, 0.25);
        assert_eq!(updated.resistance_ehc_indoor_wall, 0.30);
        assert_eq!(updated.control_ehc_stability, 2);
    }

    #[test]
    fn ehc_coupling_rejects_invalid_flat_values() {
        for value in [
            -999.0, -1.0, 2.0, 0.5, 1.5, 1.0000000001, f64::NAN, f64::INFINITY,
        ] {
            let mut flat = SuewsConfig::default().to_flat();
            flat[24] = value;
            assert_eq!(
                SuewsConfig::from_flat(&flat).unwrap_err(),
                BridgeError::BadState
            );
        }
        for value in [0, 1] {
            let mut flat = SuewsConfig::default().to_flat();
            flat[24] = value as f64;
            assert_eq!(
                SuewsConfig::from_flat(&flat).unwrap().ehc_surface_coupling,
                value
            );
        }
    }

    #[test]
    fn stability_control_rejects_invalid_flat_values() {
        for value in [-999.0, -1.0, 3.0, 1.5, f64::NAN, f64::INFINITY] {
            let mut flat = SuewsConfig::default().to_flat();
            flat[28] = value;
            assert_eq!(
                SuewsConfig::from_flat(&flat).unwrap_err(),
                BridgeError::BadState
            );
        }
        for value in [0, 1, 2] {
            let mut flat = SuewsConfig::default().to_flat();
            flat[28] = value as f64;
            assert_eq!(
                SuewsConfig::from_flat(&flat).unwrap().control_ehc_stability,
                value
            );
        }
    }

    #[test]
    fn indoor_resistance_rejects_invalid_flat_values() {
        for index in [26, 27] {
            for value in [-1.0, -999.0, f64::NAN, f64::INFINITY] {
                let mut flat = SuewsConfig::default().to_flat();
                flat[index] = value;
                assert_eq!(SuewsConfig::from_flat(&flat), Err(BridgeError::BadState));
            }
        }
    }

    #[cfg(feature = "physics")]
    #[test]
    fn batch_driver_rejects_short_configuration_buffers() {
        // An invalid layer count is a sentinel immediately after the buffer
        // checks: a full config reaches BAD_STATE, a short one must not.
        // All backing buffers are valid; the early guard reads none of them.
        for config_len in 25..=SUEWS_CONFIG_FLAT_LEN {
            let timer = [0.0; 18];
            let config = SuewsConfig::default().to_flat();
            let scalars = [0.0; 24];
            let flat = [0.0];
            let toc = [0];
            let mut timer_out = [0.0; 18];
            let mut state_out = [0.0];
            let mut output = [0.0];
            let mut datetime = [0.0];
            let mut sim_err = 0;
            let mut message = [0; 1];
            let mut warn_total = 0;
            let mut warn_stored = 0;
            let mut warn_timer = [0];
            let mut warn_text = [0; 1];
            let mut err = -1;
            unsafe {
                ffi::suews_cal_multitsteps_c(
                    timer.as_ptr(),
                    18,
                    config.as_ptr(),
                    config_len as i32,
                    scalars.as_ptr(),
                    24,
                    flat.as_ptr(),
                    1,
                    toc.as_ptr(),
                    1,
                    0,
                    flat.as_ptr(),
                    1,
                    toc.as_ptr(),
                    1,
                    0,
                    flat.as_ptr(),
                    1,
                    32,
                    0,
                    5,
                    timer_out.as_mut_ptr(),
                    18,
                    state_out.as_mut_ptr(),
                    1,
                    output.as_mut_ptr(),
                    1,
                    1,
                    datetime.as_mut_ptr(),
                    1,
                    1,
                    &mut sim_err,
                    message.as_mut_ptr(),
                    1,
                    &mut warn_total,
                    &mut warn_stored,
                    0,
                    warn_timer.as_mut_ptr(),
                    warn_text.as_mut_ptr(),
                    1,
                    &mut err,
                );
            }
            let expected = if config_len < SUEWS_CONFIG_FLAT_LEN {
                ffi::SUEWS_CAPI_BAD_BUFFER
            } else {
                ffi::SUEWS_CAPI_BAD_STATE
            };
            assert_eq!(err, expected, "configuration length {config_len}");
        }
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
