use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;
use crate::NSURF;

pub const ATM_STATE_FLAT_LEN: usize = 39;
pub const ATM_STATE_SCHEMA_VERSION: u32 = 1;

pub type AtmStateSchema = crate::codec::SimpleSchema;

pub type AtmStateValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct AtmState {
    pub fcld: f64,
    pub avcp: f64,
    pub dens_dry: f64,
    pub avdens: f64,
    pub dq: f64,
    pub ea_hpa: f64,
    pub es_hpa: f64,
    pub lv_j_kg: f64,
    pub lvs_j_kg: f64,
    pub tlv: f64,
    pub psyc_hpa: f64,
    pub psycice_hpa: f64,
    pub s_pa: f64,
    pub s_hpa: f64,
    pub sice_hpa: f64,
    pub vpd_hpa: f64,
    pub vpd_pa: f64,
    pub u10_ms: f64,
    pub u_hbh: f64,
    pub t2_c: f64,
    pub t_half_bldg_c: f64,
    pub q2_gkg: f64,
    pub rh2: f64,
    pub l_mod: f64,
    pub zl: f64,
    pub ra_h: f64,
    pub rs: f64,
    pub ustar: f64,
    pub tstar: f64,
    pub rb: f64,
    pub tair_av: f64,
    pub rss_surf: [f64; NSURF],
    pub iter_safe: bool,
}

impl Default for AtmState {
    fn default() -> Self {
        Self {
            fcld: 0.0,
            avcp: 0.0,
            dens_dry: 0.0,
            avdens: 0.0,
            dq: 0.0,
            ea_hpa: 0.0,
            es_hpa: 0.0,
            lv_j_kg: 0.0,
            lvs_j_kg: 0.0,
            tlv: 0.0,
            psyc_hpa: 0.0,
            psycice_hpa: 0.0,
            s_pa: 0.0,
            s_hpa: 0.0,
            sice_hpa: 0.0,
            vpd_hpa: 0.0,
            vpd_pa: 0.0,
            u10_ms: 0.0,
            u_hbh: 0.0,
            t2_c: 0.0,
            t_half_bldg_c: 0.0,
            q2_gkg: 0.0,
            rh2: 0.0,
            l_mod: 0.0,
            zl: 0.0,
            ra_h: 0.0,
            rs: 0.0,
            ustar: 0.0,
            tstar: 0.0,
            rb: 0.0,
            tair_av: 0.0,
            rss_surf: [0.0; NSURF],
            iter_safe: true,
        }
    }
}

impl AtmState {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, ATM_STATE_FLAT_LEN)?;

        let mut idx = 0_usize;
        let mut next = || {
            let value = flat[idx];
            idx += 1;
            value
        };

        let mut state = Self {
            fcld: next(),
            avcp: next(),
            dens_dry: next(),
            avdens: next(),
            dq: next(),
            ea_hpa: next(),
            es_hpa: next(),
            lv_j_kg: next(),
            lvs_j_kg: next(),
            tlv: next(),
            psyc_hpa: next(),
            psycice_hpa: next(),
            s_pa: next(),
            s_hpa: next(),
            sice_hpa: next(),
            vpd_hpa: next(),
            vpd_pa: next(),
            u10_ms: next(),
            u_hbh: next(),
            t2_c: next(),
            t_half_bldg_c: next(),
            q2_gkg: next(),
            rh2: next(),
            l_mod: next(),
            zl: next(),
            ra_h: next(),
            rs: next(),
            ustar: next(),
            tstar: next(),
            rb: next(),
            tair_av: next(),
            ..Self::default()
        };

        for i in 0..NSURF {
            state.rss_surf[i] = next();
        }
        state.iter_safe = next() >= 0.5;

        Ok(state)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(ATM_STATE_FLAT_LEN);
        flat.push(self.fcld);
        flat.push(self.avcp);
        flat.push(self.dens_dry);
        flat.push(self.avdens);
        flat.push(self.dq);
        flat.push(self.ea_hpa);
        flat.push(self.es_hpa);
        flat.push(self.lv_j_kg);
        flat.push(self.lvs_j_kg);
        flat.push(self.tlv);
        flat.push(self.psyc_hpa);
        flat.push(self.psycice_hpa);
        flat.push(self.s_pa);
        flat.push(self.s_hpa);
        flat.push(self.sice_hpa);
        flat.push(self.vpd_hpa);
        flat.push(self.vpd_pa);
        flat.push(self.u10_ms);
        flat.push(self.u_hbh);
        flat.push(self.t2_c);
        flat.push(self.t_half_bldg_c);
        flat.push(self.q2_gkg);
        flat.push(self.rh2);
        flat.push(self.l_mod);
        flat.push(self.zl);
        flat.push(self.ra_h);
        flat.push(self.rs);
        flat.push(self.ustar);
        flat.push(self.tstar);
        flat.push(self.rb);
        flat.push(self.tair_av);
        flat.extend_from_slice(&self.rss_surf);
        flat.push(if self.iter_safe { 1.0 } else { 0.0 });
        flat
    }
}

impl StateCodec for AtmState {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "atm_state".to_string(),
            schema_version: ATM_STATE_SCHEMA_VERSION,
            flat_len: ATM_STATE_FLAT_LEN,
            field_names: atm_state_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        AtmState::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        AtmState::to_flat(self)
    }
}

pub fn atm_state_field_names() -> Vec<String> {
    fn push_surface_fields(names: &mut Vec<String>, prefix: &str) {
        let surfaces = ["paved", "bldg", "evetr", "dectr", "grass", "bsoil", "water"];
        for surface in surfaces {
            names.push(format!("{prefix}.{surface}"));
        }
    }

    let mut names = vec![
        "fcld".to_string(),
        "avcp".to_string(),
        "dens_dry".to_string(),
        "avdens".to_string(),
        "dq".to_string(),
        "ea_hpa".to_string(),
        "es_hpa".to_string(),
        "lv_j_kg".to_string(),
        "lvs_j_kg".to_string(),
        "tlv".to_string(),
        "psyc_hpa".to_string(),
        "psycice_hpa".to_string(),
        "s_pa".to_string(),
        "s_hpa".to_string(),
        "sice_hpa".to_string(),
        "vpd_hpa".to_string(),
        "vpd_pa".to_string(),
        "u10_ms".to_string(),
        "u_hbh".to_string(),
        "t2_c".to_string(),
        "t_half_bldg_c".to_string(),
        "q2_gkg".to_string(),
        "rh2".to_string(),
        "l_mod".to_string(),
        "zl".to_string(),
        "ra_h".to_string(),
        "rs".to_string(),
        "ustar".to_string(),
        "tstar".to_string(),
        "rb".to_string(),
        "tair_av".to_string(),
    ];
    push_surface_fields(&mut names, "rss_surf");
    names.push("iter_safe".to_string());
    names
}

crate::codec::impl_state_module_fns! {
    prefix = atm_state,
    state_type = AtmState,
    schema_type = AtmStateSchema,
    payload_type = AtmStateValuesPayload,
    flat_len_const = ATM_STATE_FLAT_LEN,
    schema_version_const = ATM_STATE_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_atm_state_len,
    ffi_schema_version_fn = ffi::suews_atm_state_schema_version,
    ffi_default_fn = ffi::suews_atm_state_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = atm_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, ATM_STATE_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = atm_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, AtmState::default());
        let state2 = AtmState::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = atm_state_default_from_fortran().expect("default state should be available");
        let mut mapped = atm_state_to_map(&state);
        mapped.insert("fcld".to_string(), 0.8);
        mapped.insert("rss_surf.water".to_string(), 12.0);
        let updated = atm_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.fcld - 0.8).abs() < 1.0e-12);
        assert!((updated.rss_surf[6] - 12.0).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = atm_state_default_from_fortran().expect("default state should be available");
        let payload = atm_state_to_values_payload(&state);
        let recovered =
            atm_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = AtmStateValuesPayload {
            schema_version: ATM_STATE_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = atm_state_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
