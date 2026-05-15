use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;
use crate::NSURF;

pub const SNOW_STATE_REMOVAL_LEN: usize = 2;
pub const SNOW_STATE_FLAT_LEN: usize = 79;
pub const SNOW_STATE_SCHEMA_VERSION: u32 = 1;

pub type SnowStateSchema = crate::codec::SimpleSchema;

pub type SnowStateValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct SnowState {
    pub snowfall_cum: f64,
    pub snowalb: f64,
    pub ch_snow_per_interval: f64,
    pub mwh: f64,
    pub mwstore: f64,
    pub qn_snow: f64,
    pub qm: f64,
    pub qm_freez: f64,
    pub qm_rain: f64,
    pub swe: f64,
    pub z0v_snow: f64,
    pub ra_snow: f64,
    pub sice_hpa: f64,
    pub snow_removal: [f64; SNOW_STATE_REMOVAL_LEN],
    pub icefrac: [f64; NSURF],
    pub snowdens: [f64; NSURF],
    pub snowfrac: [f64; NSURF],
    pub snowpack: [f64; NSURF],
    pub snowwater: [f64; NSURF],
    pub kup_ind_snow: [f64; NSURF],
    pub qn_ind_snow: [f64; NSURF],
    pub delta_qi: [f64; NSURF],
    pub tsurf_ind_snow: [f64; NSURF],
    pub iter_safe: bool,
}

impl Default for SnowState {
    fn default() -> Self {
        Self {
            snowfall_cum: 0.0,
            snowalb: 0.0,
            ch_snow_per_interval: 0.0,
            mwh: 0.0,
            mwstore: 0.0,
            qn_snow: 0.0,
            qm: 0.0,
            qm_freez: 0.0,
            qm_rain: 0.0,
            swe: 0.0,
            z0v_snow: 0.0,
            ra_snow: 0.0,
            sice_hpa: 0.0,
            snow_removal: [0.0; SNOW_STATE_REMOVAL_LEN],
            icefrac: [0.0; NSURF],
            snowdens: [0.0; NSURF],
            snowfrac: [0.0; NSURF],
            snowpack: [0.0; NSURF],
            snowwater: [0.0; NSURF],
            kup_ind_snow: [0.0; NSURF],
            qn_ind_snow: [0.0; NSURF],
            delta_qi: [0.0; NSURF],
            tsurf_ind_snow: [0.0; NSURF],
            iter_safe: false,
        }
    }
}

impl SnowState {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, SNOW_STATE_FLAT_LEN)?;

        let mut idx = 0_usize;
        let mut next = || {
            let value = flat[idx];
            idx += 1;
            value
        };

        let mut state = Self {
            snowfall_cum: next(),
            snowalb: next(),
            ch_snow_per_interval: next(),
            mwh: next(),
            mwstore: next(),
            qn_snow: next(),
            qm: next(),
            qm_freez: next(),
            qm_rain: next(),
            swe: next(),
            z0v_snow: next(),
            ra_snow: next(),
            sice_hpa: next(),
            ..Self::default()
        };

        for i in 0..SNOW_STATE_REMOVAL_LEN {
            state.snow_removal[i] = next();
        }

        for i in 0..NSURF {
            state.icefrac[i] = next();
        }

        for i in 0..NSURF {
            state.snowdens[i] = next();
        }

        for i in 0..NSURF {
            state.snowfrac[i] = next();
        }

        for i in 0..NSURF {
            state.snowpack[i] = next();
        }

        for i in 0..NSURF {
            state.snowwater[i] = next();
        }

        for i in 0..NSURF {
            state.kup_ind_snow[i] = next();
        }

        for i in 0..NSURF {
            state.qn_ind_snow[i] = next();
        }

        for i in 0..NSURF {
            state.delta_qi[i] = next();
        }

        for i in 0..NSURF {
            state.tsurf_ind_snow[i] = next();
        }

        state.iter_safe = next() >= 0.5;

        Ok(state)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(SNOW_STATE_FLAT_LEN);

        flat.push(self.snowfall_cum);
        flat.push(self.snowalb);
        flat.push(self.ch_snow_per_interval);
        flat.push(self.mwh);
        flat.push(self.mwstore);
        flat.push(self.qn_snow);
        flat.push(self.qm);
        flat.push(self.qm_freez);
        flat.push(self.qm_rain);
        flat.push(self.swe);
        flat.push(self.z0v_snow);
        flat.push(self.ra_snow);
        flat.push(self.sice_hpa);
        flat.extend_from_slice(&self.snow_removal);
        flat.extend_from_slice(&self.icefrac);
        flat.extend_from_slice(&self.snowdens);
        flat.extend_from_slice(&self.snowfrac);
        flat.extend_from_slice(&self.snowpack);
        flat.extend_from_slice(&self.snowwater);
        flat.extend_from_slice(&self.kup_ind_snow);
        flat.extend_from_slice(&self.qn_ind_snow);
        flat.extend_from_slice(&self.delta_qi);
        flat.extend_from_slice(&self.tsurf_ind_snow);
        flat.push(if self.iter_safe { 1.0 } else { 0.0 });

        flat
    }
}

impl StateCodec for SnowState {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "SNOW_STATE".to_string(),
            schema_version: SNOW_STATE_SCHEMA_VERSION,
            flat_len: SNOW_STATE_FLAT_LEN,
            field_names: snow_state_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        SnowState::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        SnowState::to_flat(self)
    }
}

pub fn snow_state_field_names() -> Vec<String> {
    fn push_surface_fields(names: &mut Vec<String>, prefix: &str) {
        let surfaces = ["paved", "bldg", "evetr", "dectr", "grass", "bsoil", "water"];
        for surface in surfaces {
            names.push(format!("{prefix}.{surface}"));
        }
    }

    let mut names = vec![
        "snowfall_cum".to_string(),
        "snowalb".to_string(),
        "ch_snow_per_interval".to_string(),
        "mwh".to_string(),
        "mwstore".to_string(),
        "qn_snow".to_string(),
        "qm".to_string(),
        "qm_freez".to_string(),
        "qm_rain".to_string(),
        "swe".to_string(),
        "z0v_snow".to_string(),
        "ra_snow".to_string(),
        "sice_hpa".to_string(),
        "snow_removal.1".to_string(),
        "snow_removal.2".to_string(),
    ];

    push_surface_fields(&mut names, "icefrac");
    push_surface_fields(&mut names, "snowdens");
    push_surface_fields(&mut names, "snowfrac");
    push_surface_fields(&mut names, "snowpack");
    push_surface_fields(&mut names, "snowwater");
    push_surface_fields(&mut names, "kup_ind_snow");
    push_surface_fields(&mut names, "qn_ind_snow");
    push_surface_fields(&mut names, "delta_qi");
    push_surface_fields(&mut names, "tsurf_ind_snow");

    names.push("iter_safe".to_string());

    names
}

crate::codec::impl_state_module_fns! {
    prefix = snow_state,
    state_type = SnowState,
    schema_type = SnowStateSchema,
    payload_type = SnowStateValuesPayload,
    flat_len_const = SNOW_STATE_FLAT_LEN,
    schema_version_const = SNOW_STATE_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_snow_state_len,
    ffi_schema_version_fn = ffi::suews_snow_state_schema_version,
    ffi_default_fn = ffi::suews_snow_state_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = snow_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, SNOW_STATE_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = snow_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, SnowState::default());
        let state2 = SnowState::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = snow_state_default_from_fortran().expect("default state should be available");
        let mut mapped = snow_state_to_map(&state);
        mapped.insert("snowalb".to_string(), 0.73);
        mapped.insert("snowpack.water".to_string(), 12.5);
        mapped.insert("iter_safe".to_string(), 1.0);

        let updated = snow_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.snowalb - 0.73).abs() < 1.0e-12);
        assert!((updated.snowpack[6] - 12.5).abs() < 1.0e-12);
        assert!(updated.iter_safe);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = snow_state_default_from_fortran().expect("default state should be available");
        let payload = snow_state_to_values_payload(&state);
        let recovered =
            snow_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = SnowStateValuesPayload {
            schema_version: SNOW_STATE_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = snow_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
