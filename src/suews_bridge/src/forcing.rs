use crate::codec::{
    dims_element_count, field_index, require_field_dims, validate_flat_len, PayloadDims,
    ValuesPayloadWithDims,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const SUEWS_FORCING_SCHEMA_VERSION: u32 = 2;
pub const SUEWS_FORCING_TS5_FIELD: &str = "ts5mindata_ir";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuewsForcingSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub base_flat_len: usize,
    pub ts5mindata_ir_len: usize,
    pub field_names: Vec<String>,
    pub allocatable_dims: PayloadDims,
}

pub type SuewsForcingValuesPayload = ValuesPayloadWithDims;


// gh#1372 — Baseline-required columns; the loader errors if any is missing.
// Names are lower-cased; lookups are case-insensitive.
const BASELINE_FORCING_COLUMNS: &[&str] = &[
    "iy", "id", "it", "imin", "tair", "rh", "u", "pres", "kdown", "rain",
];
// gh#1372 — Per-landcover whitelist: <var>_<surface>. `wuh` covers
// per-surface external water use — irrigation and impervious-surface
// washing on land surfaces, fountains and ornamental water features
// on the open-water surface — and is therefore accepted on every
// surface. `lai` is leaf-area index and is meaningful only for the
// three vegetated surfaces. The bulk site-level columns `Wuh` /
// `xsmd` remain in the canonical block — `xsmd` is intentionally NOT
// per-landcover (it is fed in as a single bulk soil-moisture-deficit
// value).
// const PER_LANDCOVER_FORCING_VARS: &[&str] = &["lai", "wuh"];

const LANDCOVER_SUFFIXES: &[&str] = &[
    "paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water",
];
const LAI_LANDCOVER_SUFFIXES: &[&str] = &["evetr", "dectr", "grass"];

pub struct PerLandcoverVar {
    pub prefix: &'static str,
    pub allowed_suffixes: &'static [&'static str],
}

static LAI: PerLandcoverVar = PerLandcoverVar {
    prefix: "lai",
    allowed_suffixes: LAI_LANDCOVER_SUFFIXES,
};

static WUH: PerLandcoverVar = PerLandcoverVar {
    prefix: "wuh",
    allowed_suffixes: LANDCOVER_SUFFIXES,
};

pub static PER_LANDCOVER_FORCING_VARS: &[&PerLandcoverVar] = &[
    &LAI,
    &WUH,
];


macro_rules! suews_fields {
    ($($field:ident),* $(,)?) => {
        #[repr(usize)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum SuewsField {
            $($field),*
        }

        impl SuewsField {
            pub const fn name(self) -> &'static str {
                match self {
                    $(Self::$field => stringify!($field)),*
                }
            }
            pub fn from_index(index: usize) -> Option<Self> {
                match index {
                    $(x if x == Self::$field as usize => Some(Self::$field),)*
                    _ => None,
                }
            }
        }

        #[derive(Debug, Clone, PartialEq, Default)]
        pub struct SuewsForcing {
            $(pub $field: f64,)*
            pub ts5mindata_ir: Vec<f64>,
        }

        pub const SUEWS_FORCING_BASE_FIELDS: &[&str] = &[
            $(stringify!($field),)*
        ];

        pub const SUEWS_FORCING_BASE_FLAT_LEN: usize = SUEWS_FORCING_BASE_FIELDS.len();

        impl SuewsForcing {
            
            pub fn from_flat_with_ts_len(
                flat: &[f64],
                ts5mindata_ir_len: usize,
            ) -> Result<Self, BridgeError> {
                let expected_len = SUEWS_FORCING_BASE_FLAT_LEN
                    .checked_add(ts5mindata_ir_len)
                    .ok_or(BridgeError::BadState)?;
                validate_flat_len(flat, expected_len)?;

                Ok(Self {
                    $($field: flat[SuewsField::$field as usize],)*
                    ts5mindata_ir: flat[SUEWS_FORCING_BASE_FLAT_LEN..].to_vec(),
                })
            }
            
            pub fn from_ordered_values(values: &[f64]) -> Result<Self, BridgeError> {
                if values.len() < SUEWS_FORCING_BASE_FLAT_LEN {
                    return Err(BridgeError::BadBuffer);
                }

                let ts5mindata_ir_len = values.len() - SUEWS_FORCING_BASE_FLAT_LEN;
                Self::from_flat_with_ts_len(values, ts5mindata_ir_len)
            }
            
            pub fn to_flat(&self) -> Vec<f64> {
                let mut flat = Vec::with_capacity(SUEWS_FORCING_BASE_FLAT_LEN + self.ts5mindata_ir.len());

                $(flat.push(self.$field);)*
                flat.extend_from_slice(&self.ts5mindata_ir);

                flat
            }
        }

        fn set_base_field_value(
            state: &mut SuewsForcing,
            index: usize,
            value: f64,
        ) -> Result<(), BridgeError> {
            match SuewsField::from_index(index) {
                $(
                    Some(SuewsField::$field) => {
                        state.$field = value;
                    },
                )*
                None => return Err(BridgeError::BadState),
            }
            
            Ok(())
        }
    }
}

suews_fields! {
    kdown,
    ldown,
    rh,
    pres,
    tair_av_5d,
    u,
    rain,
    wu_m3,
    fcld,
    snowfrac,
    xsmd,
    qf_obs,
    qn1_obs,
    qs_obs,
    temp_c,
    lai_evetr,
    lai_dectr,
    lai_grass,
    wuh_paved,
    wuh_bldgs,
    wuh_evetr,
    wuh_dectr,
    wuh_grass,
    wuh_bsoil,
    wuh_water
}


fn parse_ts5mindata_ir_field_index(name: &str) -> Option<usize> {
    let suffix = name.strip_prefix("ts5mindata_ir_")?;
    let one_based = suffix.parse::<usize>().ok()?;
    one_based.checked_sub(1)
}

pub fn suews_forcing_schema() -> Result<(usize, usize), BridgeError> {
    let mut n_flat = -1_i32;
    let mut ts5mindata_ir_len = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_forcing_len(
            &mut n_flat as *mut i32,
            &mut ts5mindata_ir_len as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }
    if n_flat < 0 || ts5mindata_ir_len < 0 {
        return Err(BridgeError::BadState);
    }

    Ok((n_flat as usize, ts5mindata_ir_len as usize))
}

pub fn suews_forcing_schema_info() -> Result<SuewsForcingSchema, BridgeError> {
    let (flat_len, ts5mindata_ir_len) = suews_forcing_schema()?;
    let schema_version_runtime = suews_forcing_schema_version_runtime()?;
    let field_names = suews_forcing_field_names_with_ts_len(ts5mindata_ir_len);

    let mut allocatable_dims = PayloadDims::new();
    allocatable_dims.insert(SUEWS_FORCING_TS5_FIELD.to_string(), vec![ts5mindata_ir_len]);

    if schema_version_runtime != SUEWS_FORCING_SCHEMA_VERSION
        || flat_len != SUEWS_FORCING_BASE_FLAT_LEN + ts5mindata_ir_len
        || flat_len != field_names.len()
    {
        return Err(BridgeError::BadState);
    }

    Ok(SuewsForcingSchema {
        schema_version: SUEWS_FORCING_SCHEMA_VERSION,
        flat_len,
        base_flat_len: SUEWS_FORCING_BASE_FLAT_LEN,
        ts5mindata_ir_len,
        field_names,
        allocatable_dims,
    })
}

pub fn suews_forcing_schema_version() -> u32 {
    SUEWS_FORCING_SCHEMA_VERSION
}

pub fn suews_forcing_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_forcing_schema_version(&mut schema_version as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn suews_forcing_base_field_names() -> Vec<String> {
    SUEWS_FORCING_BASE_FIELDS
        .iter()
        .map(|name| (*name).to_string())
        .collect()
}

pub fn suews_forcing_field_names_with_ts_len(ts5mindata_ir_len: usize) -> Vec<String> {
    let mut names = suews_forcing_base_field_names();
    for idx in 0..ts5mindata_ir_len {
        names.push(format!("ts5mindata_ir_{}", idx + 1));
    }
    names
}

pub fn suews_forcing_field_names() -> Result<Vec<String>, BridgeError> {
    let (_, ts5mindata_ir_len) = suews_forcing_schema()?;
    Ok(suews_forcing_field_names_with_ts_len(ts5mindata_ir_len))
}

pub fn suews_forcing_to_map(state: &SuewsForcing) -> BTreeMap<String, f64> {
    let names = suews_forcing_field_names_with_ts_len(state.ts5mindata_ir.len());
    let values = state.to_flat();
    names.into_iter().zip(values).collect()
}

pub fn suews_forcing_to_ordered_values(state: &SuewsForcing) -> Vec<f64> {
    state.to_flat()
}

pub fn suews_forcing_to_values_payload(state: &SuewsForcing) -> SuewsForcingValuesPayload {
    let mut dims = PayloadDims::new();
    dims.insert(
        SUEWS_FORCING_TS5_FIELD.to_string(),
        vec![state.ts5mindata_ir.len()],
    );

    SuewsForcingValuesPayload {
        schema_version: SUEWS_FORCING_SCHEMA_VERSION,
        values: state.to_flat(),
        dims,
    }
}

pub fn suews_forcing_from_ordered_values(values: &[f64]) -> Result<SuewsForcing, BridgeError> {
    SuewsForcing::from_ordered_values(values)
}

pub fn suews_forcing_from_values_payload(
    payload: &SuewsForcingValuesPayload,
) -> Result<SuewsForcing, BridgeError> {
    if payload.schema_version != SUEWS_FORCING_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    if payload.dims.len() != 1 {
        return Err(BridgeError::BadState);
    }

    let ts5mindata_ir_dims = require_field_dims(&payload.dims, SUEWS_FORCING_TS5_FIELD, 1)?;
    let ts5mindata_ir_len = dims_element_count(&ts5mindata_ir_dims)?;
    let expected_len = SUEWS_FORCING_BASE_FLAT_LEN
        .checked_add(ts5mindata_ir_len)
        .ok_or(BridgeError::BadState)?;
    validate_flat_len(&payload.values, expected_len)?;

    SuewsForcing::from_flat_with_ts_len(&payload.values, ts5mindata_ir_len)
}

pub fn suews_forcing_from_map(values: &BTreeMap<String, f64>) -> Result<SuewsForcing, BridgeError> {
    let mut state = suews_forcing_default_from_fortran()?;
    let base_field_names = suews_forcing_base_field_names();

    for (name, value) in values {
        if let Some(index) = field_index(&base_field_names, name) {
            set_base_field_value(&mut state, index, *value)?;
            continue;
        }

        if let Some(ts_index) = parse_ts5mindata_ir_field_index(name) {
            if state.ts5mindata_ir.len() <= ts_index {
                state.ts5mindata_ir.resize(ts_index + 1, 0.0);
            }
            state.ts5mindata_ir[ts_index] = *value;
            continue;
        }

        return Err(BridgeError::BadState);
    }

    Ok(state)
}

pub fn suews_forcing_default_from_fortran() -> Result<SuewsForcing, BridgeError> {
    let (n_flat, ts5mindata_ir_len_expected) = suews_forcing_schema()?;
    let mut flat = vec![0.0_f64; n_flat];
    let mut ts5mindata_ir_len = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_forcing_default(
            flat.as_mut_ptr(),
            n_flat as i32,
            &mut ts5mindata_ir_len as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }
    if ts5mindata_ir_len < 0 {
        return Err(BridgeError::BadState);
    }

    let ts5mindata_ir_len = ts5mindata_ir_len as usize;
    if ts5mindata_ir_len != ts5mindata_ir_len_expected {
        return Err(BridgeError::BadState);
    }

    SuewsForcing::from_flat_with_ts_len(&flat, ts5mindata_ir_len)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let (flat_len, ts5mindata_ir_len) =
            suews_forcing_schema().expect("schema call should succeed");
        assert_eq!(flat_len, SUEWS_FORCING_BASE_FLAT_LEN + ts5mindata_ir_len);
    }

    #[test]
    fn default_state_roundtrip() {
        let state =
            suews_forcing_default_from_fortran().expect("default state should be available");
        assert!(state
            .ts5mindata_ir
            .iter()
            .all(|value| value.abs() < 1.0e-12));

        let state2 =
            SuewsForcing::from_flat_with_ts_len(&state.to_flat(), state.ts5mindata_ir.len())
                .expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state =
            suews_forcing_default_from_fortran().expect("default state should be available");
        let mut mapped = suews_forcing_to_map(&state);
        mapped.insert("kdown".to_string(), 500.0);
        mapped.insert("ts5mindata_ir_2".to_string(), 12.5);

        let updated = suews_forcing_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.kdown - 500.0).abs() < 1.0e-12);
        assert!(updated.ts5mindata_ir.len() >= 2);
        assert!((updated.ts5mindata_ir[1] - 12.5).abs() < 1.0e-12);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state =
            suews_forcing_default_from_fortran().expect("default state should be available");
        let payload = suews_forcing_to_values_payload(&state);
        let recovered =
            suews_forcing_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = SuewsForcingValuesPayload {
            schema_version: SUEWS_FORCING_SCHEMA_VERSION + 1,
            values: payload.values.clone(),
            dims: payload.dims.clone(),
        };
        let err = suews_forcing_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_missing_dims() {
        let state =
            suews_forcing_default_from_fortran().expect("default state should be available");
        let payload = SuewsForcingValuesPayload {
            schema_version: SUEWS_FORCING_SCHEMA_VERSION,
            values: state.to_flat(),
            dims: PayloadDims::new(),
        };
        let err =
            suews_forcing_from_values_payload(&payload).expect_err("missing dims should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn values_payload_rejects_dims_length_mismatch() {
        let state =
            suews_forcing_default_from_fortran().expect("default state should be available");
        let mut payload = suews_forcing_to_values_payload(&state);
        payload.dims.insert(
            SUEWS_FORCING_TS5_FIELD.to_string(),
            vec![state.ts5mindata_ir.len() + 1],
        );

        let err = suews_forcing_from_values_payload(&payload)
            .expect_err("dims/values mismatch should fail");
        assert_eq!(err, BridgeError::BadBuffer);
    }
}
