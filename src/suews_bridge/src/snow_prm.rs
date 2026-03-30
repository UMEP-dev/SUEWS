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
    pub crwmax: f64,
    pub crwmin: f64,
    pub narp_emis_snow: f64,
    pub preciplimit: f64,
    pub preciplimitalb: f64,
    pub snowalbmax: f64,
    pub snowalbmin: f64,
    pub snowdensmax: f64,
    pub snowdensmin: f64,
    pub snowlimbldg: f64,
    pub snowlimpaved: f64,
    pub snowpacklimit: [f64; NSURF],
    pub snowprof_24hr_working: [f64; HOURS_PER_DAY],
    pub snowprof_24hr_holiday: [f64; HOURS_PER_DAY],
    pub tau_a: f64,
    pub tau_f: f64,
    pub tau_r: f64,
    pub tempmeltfact: f64,
    pub radmeltfact: f64,
}

impl Default for SnowPrm {
    fn default() -> Self {
        Self {
            crwmax: 0.0,
            crwmin: 0.0,
            narp_emis_snow: 0.0,
            preciplimit: 0.0,
            preciplimitalb: 0.0,
            snowalbmax: 0.0,
            snowalbmin: 0.0,
            snowdensmax: 0.0,
            snowdensmin: 0.0,
            snowlimbldg: 0.0,
            snowlimpaved: 0.0,
            snowpacklimit: [0.0; NSURF],
            snowprof_24hr_working: [0.0; HOURS_PER_DAY],
            snowprof_24hr_holiday: [0.0; HOURS_PER_DAY],
            tau_a: 0.0,
            tau_f: 0.0,
            tau_r: 0.0,
            tempmeltfact: 0.0,
            radmeltfact: 0.0,
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
            crwmax: flat[0],
            crwmin: flat[1],
            narp_emis_snow: flat[2],
            preciplimit: flat[3],
            preciplimitalb: flat[4],
            snowalbmax: flat[5],
            snowalbmin: flat[6],
            snowdensmax: flat[7],
            snowdensmin: flat[8],
            snowlimbldg: flat[9],
            snowlimpaved: flat[10],
            snowpacklimit: copy_fixed(&flat[11..18])?,
            snowprof_24hr_working: copy_fixed(&flat[18..42])?,
            snowprof_24hr_holiday: copy_fixed(&flat[42..66])?,
            tau_a: flat[66],
            tau_f: flat[67],
            tau_r: flat[68],
            tempmeltfact: flat[69],
            radmeltfact: flat[70],
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut out = Vec::with_capacity(SNOW_PRM_FLAT_LEN);

        out.push(self.crwmax);
        out.push(self.crwmin);
        out.push(self.narp_emis_snow);
        out.push(self.preciplimit);
        out.push(self.preciplimitalb);
        out.push(self.snowalbmax);
        out.push(self.snowalbmin);
        out.push(self.snowdensmax);
        out.push(self.snowdensmin);
        out.push(self.snowlimbldg);
        out.push(self.snowlimpaved);
        out.extend_from_slice(&self.snowpacklimit);
        out.extend_from_slice(&self.snowprof_24hr_working);
        out.extend_from_slice(&self.snowprof_24hr_holiday);
        out.push(self.tau_a);
        out.push(self.tau_f);
        out.push(self.tau_r);
        out.push(self.tempmeltfact);
        out.push(self.radmeltfact);

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
        assert!((updated.snowalbmax - 0.91).abs() < 1.0e-12);
        assert!((updated.snowpacklimit[6] - 12.5).abs() < 1.0e-12);
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
