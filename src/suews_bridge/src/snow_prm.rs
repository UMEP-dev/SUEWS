use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;
use crate::NSURF;

pub const SNOW_PRM_FLAT_LEN: usize = 71;
pub const SNOW_PRM_SCHEMA_VERSION: u32 = 1;

const HOURS_PER_DAY: usize = 24;

pub type SnowPrmSchema = crate::codec::SimpleSchema;

pub type SnowPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct SnowPrm {
    pub water_holding_capacity_max: f64,
    pub water_holding_capacity_min: f64,
    pub narp_emis_snow: f64,
    pub precip_limit: f64,
    pub precip_limit_albedo: f64,
    pub snow_albedo_max: f64,
    pub snow_albedo_min: f64,
    pub snow_density_max: f64,
    pub snow_density_min: f64,
    pub snow_limit_building: f64,
    pub snow_limit_paved: f64,
    pub snowpack_limit: [f64; NSURF],
    pub snowprof_24hr_working: [f64; HOURS_PER_DAY],
    pub snowprof_24hr_holiday: [f64; HOURS_PER_DAY],
    pub tau_a: f64,
    pub tau_f: f64,
    pub tau_r: f64,
    pub temp_melt_factor: f64,
    pub rad_melt_factor: f64,
}

impl Default for SnowPrm {
    fn default() -> Self {
        Self {
            water_holding_capacity_max: 0.0,
            water_holding_capacity_min: 0.0,
            narp_emis_snow: 0.0,
            precip_limit: 0.0,
            precip_limit_albedo: 0.0,
            snow_albedo_max: 0.0,
            snow_albedo_min: 0.0,
            snow_density_max: 0.0,
            snow_density_min: 0.0,
            snow_limit_building: 0.0,
            snow_limit_paved: 0.0,
            snowpack_limit: [0.0; NSURF],
            snowprof_24hr_working: [0.0; HOURS_PER_DAY],
            snowprof_24hr_holiday: [0.0; HOURS_PER_DAY],
            tau_a: 0.0,
            tau_f: 0.0,
            tau_r: 0.0,
            temp_melt_factor: 0.0,
            rad_melt_factor: 0.0,
        }
    }
}

fn copy_fixed<const N: usize>(src: &[f64]) -> Result<[f64; N], BridgeError> {
    if src.len() != N {
        return Err(BridgeError::BadState);
    }

    let mut out = [0.0_f64; N];
    out.copy_from_slice(src);
    Ok(out)
}

impl SnowPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, SNOW_PRM_FLAT_LEN)?;

        Ok(Self {
            water_holding_capacity_max: flat[0],
            water_holding_capacity_min: flat[1],
            narp_emis_snow: flat[2],
            precip_limit: flat[3],
            precip_limit_albedo: flat[4],
            snow_albedo_max: flat[5],
            snow_albedo_min: flat[6],
            snow_density_max: flat[7],
            snow_density_min: flat[8],
            snow_limit_building: flat[9],
            snow_limit_paved: flat[10],
            snowpack_limit: copy_fixed(&flat[11..18])?,
            snowprof_24hr_working: copy_fixed(&flat[18..42])?,
            snowprof_24hr_holiday: copy_fixed(&flat[42..66])?,
            tau_a: flat[66],
            tau_f: flat[67],
            tau_r: flat[68],
            temp_melt_factor: flat[69],
            rad_melt_factor: flat[70],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = Vec::with_capacity(SNOW_PRM_FLAT_LEN);

        out.push(self.water_holding_capacity_max);
        out.push(self.water_holding_capacity_min);
        out.push(self.narp_emis_snow);
        out.push(self.precip_limit);
        out.push(self.precip_limit_albedo);
        out.push(self.snow_albedo_max);
        out.push(self.snow_albedo_min);
        out.push(self.snow_density_max);
        out.push(self.snow_density_min);
        out.push(self.snow_limit_building);
        out.push(self.snow_limit_paved);
        out.extend_from_slice(&self.snowpack_limit);
        out.extend_from_slice(&self.snowprof_24hr_working);
        out.extend_from_slice(&self.snowprof_24hr_holiday);
        out.push(self.tau_a);
        out.push(self.tau_f);
        out.push(self.tau_r);
        out.push(self.temp_melt_factor);
        out.push(self.rad_melt_factor);

        out
    }
}

impl StateCodec for SnowPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "SNOW_PRM".to_string(),
            schema_version: SNOW_PRM_SCHEMA_VERSION,
            flat_len: SNOW_PRM_FLAT_LEN,
            field_names: snow_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        SnowPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        SnowPrm::to_flat(self)
    }
}

pub fn snow_prm_field_names() -> Vec<String> {
    let mut names = vec![
        "crwmax".to_string(),
        "crwmin".to_string(),
        "narp_emis_snow".to_string(),
        "preciplimit".to_string(),
        "preciplimitalb".to_string(),
        "snowalbmax".to_string(),
        "snowalbmin".to_string(),
        "snowdensmax".to_string(),
        "snowdensmin".to_string(),
        "snowlimbldg".to_string(),
        "snowlimpaved".to_string(),
    ];

    for surface in ["paved", "bldg", "evetr", "dectr", "grass", "bsoil", "water"] {
        names.push(format!("snowpacklimit.{surface}"));
    }

    for i in 0..HOURS_PER_DAY {
        names.push(format!("snowprof_24hr_working_{i:02}"));
    }

    for i in 0..HOURS_PER_DAY {
        names.push(format!("snowprof_24hr_holiday_{i:02}"));
    }

    names.push("tau_a".to_string());
    names.push("tau_f".to_string());
    names.push("tau_r".to_string());
    names.push("tempmeltfact".to_string());
    names.push("radmeltfact".to_string());

    names
}

crate::codec::impl_state_module_fns! {
    prefix = snow_prm,
    state_type = SnowPrm,
    schema_type = SnowPrmSchema,
    payload_type = SnowPrmValuesPayload,
    flat_len_const = SNOW_PRM_FLAT_LEN,
    schema_version_const = SNOW_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_snow_prm_len,
    ffi_schema_version_fn = ffi::suews_snow_prm_schema_version,
    ffi_default_fn = ffi::suews_snow_prm_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = snow_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, SNOW_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = snow_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, SnowPrm::default());

        let state2 = SnowPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = snow_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = snow_prm_to_map(&state);
        mapped.insert("snowalbmax".to_string(), 0.91);
        mapped.insert("snowpacklimit.water".to_string(), 12.5);
        mapped.insert("snowprof_24hr_working_07".to_string(), 0.3);

        let updated = snow_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.snow_albedo_max - 0.91).abs() < 1.0e-12);
        assert!((updated.snowpack_limit[6] - 12.5).abs() < 1.0e-12);
        assert!((updated.snowprof_24hr_working[7] - 0.3).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = snow_prm_default_from_fortran().expect("default state should be available");
        let payload = snow_prm_to_values_payload(&state);
        let recovered = snow_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = SnowPrmValuesPayload {
            schema_version: SNOW_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = snow_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
