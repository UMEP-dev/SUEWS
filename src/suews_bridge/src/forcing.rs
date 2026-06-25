use crate::codec::{
    dims_element_count, require_field_dims, validate_flat_len, PayloadDims,
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

pub const BASELINE_FORCING_COLUMNS: &[&str] = &[
    "iy", "id", "it", "imin", "tair", "rh", "u", "pres", "kdown", "rain",
];
// Rust-side mirror of supy._load.LANDCOVER_SUFFIXES. No Rust code references
// it directly, but the gh#1372 cross-language guard
// (test/data_model/test_forcing_validation.py::test_python_rust_whitelist_parity)
// parses this source file as text and asserts the two whitelists stay in sync,
// so clippy's dead_code lint is a false positive here.
#[allow(dead_code)]
const LANDCOVER_SUFFIXES: &[&str] = &[
    "paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water",
];
const LAI_LANDCOVER_SUFFIXES: &[&str] = &["evetr", "dectr", "grass"];
// External water use (wuh) is accepted on every surface, so its suffix
// whitelist mirrors the full LANDCOVER_SUFFIXES set. Kept as a separate
// named const (referenced by the WUH var below) so the gh#1372
// cross-language parity guard can assert it against
// supy._load.WUH_LANDCOVER_SUFFIXES.
const WU_LANDCOVER_SUFFIXES: &[&str] = &[
    "paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water",
];

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
    allowed_suffixes: WU_LANDCOVER_SUFFIXES,
};

pub static PER_LANDCOVER_FORCING_VARS: &[&PerLandcoverVar] = &[
    &LAI,
    &WUH,
];

/// Instantaneous: Forcing columns interpolated as instantaneous point values (linear).
/// Average: Forcing columns interpolated as period averages (shift by -tstep_in/2, then linear).
/// Sum: Forcing columns interpolated as period sums (proportional redistribution).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpKind {
    Instantaneous,
    Average,
    Sum,
}

pub struct FieldDescriptor {
    pub field: SuewsField,
    pub interp: InterpKind,
    pub csv_names: &'static [&'static str],
    pub required: bool,
    pub get: fn(&SuewsForcing) -> f64,
    pub set: fn(&mut SuewsForcing, f64),
}


macro_rules! suews_fields {
    ($(($field:ident, $index:expr, [$($csv:expr),+], $interp:expr, $required:expr)),* $(,)?) => {
        #[repr(usize)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum SuewsField {
            $($field = $index),*
        }

        impl SuewsField {
            pub const fn index(self) -> usize {
                self as usize
            }
            pub const fn name(self) -> &'static str {
                match self {
                    $(Self::$field => stringify!($field)),*
                }
            }
        }

        pub const SUEWS_FORCING_BASE_FIELDS: &[&str] = &[
            $(stringify!($field),)*
        ];

        pub const FIELD_DESCRIPTORS: &[FieldDescriptor] = &[
            $(
                FieldDescriptor {
                    field: SuewsField::$field,
                    csv_names: &[$($csv),+],
                    interp: $interp,
                    required: $required,
                    get: |s| s.$field,
                    set: |s, v| { s.$field = v; },
                },
            )*
        ];

        pub const SUEWS_FORCING_BASE_FLAT_LEN: usize = {
            let mut n = 0;
            $(let _ = stringify!($field); n += 1;)*
            n
        };

        #[derive(Debug, Clone, PartialEq, Default)]
        pub struct SuewsForcing {
            $(pub $field: f64,)*
            pub ts5mindata_ir: Vec<f64>,
        }

        impl SuewsForcing {
            
            pub fn from_flat_with_ts_len(
                flat: &[f64],
                ts5mindata_ir_len: usize,
            ) -> Result<Self, BridgeError> {
                let expected_len = SUEWS_FORCING_BASE_FLAT_LEN
                    .checked_add(ts5mindata_ir_len)
                    .ok_or(BridgeError::BadState)?;

                validate_flat_len(flat, expected_len)?;

                let mut state = Self {
                    ts5mindata_ir: flat[SUEWS_FORCING_BASE_FLAT_LEN..].to_vec(),
                    ..Default::default()
                };

                for desc in FIELD_DESCRIPTORS {
                    let idx = desc.field.index();
                    let value = flat[idx];
                    (desc.set)(&mut state, value);
                }

                Ok(state)
            }
            
            pub fn from_ordered_values(values: &[f64]) -> Result<Self, BridgeError> {
                if values.len() < SUEWS_FORCING_BASE_FLAT_LEN {
                    return Err(BridgeError::BadBuffer);
                }

                let ts5mindata_ir_len = values.len() - SUEWS_FORCING_BASE_FLAT_LEN;
                Self::from_flat_with_ts_len(values, ts5mindata_ir_len)
            }
            
            pub fn to_flat(&self) -> Vec<f64> {
                let total_len = SUEWS_FORCING_BASE_FLAT_LEN + self.ts5mindata_ir.len();
                let mut flat = vec![0.0_f64; total_len];

                for desc in FIELD_DESCRIPTORS {
                    flat[desc.field.index()] = (desc.get)(self);
                }
                flat[SUEWS_FORCING_BASE_FLAT_LEN..].copy_from_slice(&self.ts5mindata_ir);

                flat
            }
        }
    }
}

suews_fields! {
    (qn1_obs, 4, ["qn1_obs", "qn"], InterpKind::Average, false),
    (qh, 5, ["qh"], InterpKind::Average, false),
    (qe, 6, ["qe"], InterpKind::Average, false),
    (qs_obs, 7, ["qs_obs", "qs"], InterpKind::Average, false),
    (qf_obs, 8, ["qf_obs", "qf"], InterpKind::Average, false),

    (u, 9, ["u"], InterpKind::Instantaneous, true),
    (rh, 10, ["rh"], InterpKind::Instantaneous, true),

    (temp_c, 11, ["temp_c", "tair"], InterpKind::Instantaneous, true),

    (pres, 12, ["pres"], InterpKind::Instantaneous, true),

    (rain, 13, ["rain"], InterpKind::Sum, true),

    (kdown, 14, ["kdown"], InterpKind::Average, true),

    (snowfrac, 15, ["snowfrac", "snow"], InterpKind::Instantaneous, false),

    (ldown, 16, ["ldown"], InterpKind::Average, false),

    (fcld, 17, ["fcld"], InterpKind::Instantaneous, false),

    (wu_mm, 18, ["wu_mm", "wuh"], InterpKind::Instantaneous, false),

    (xsmd, 19, ["xsmd"], InterpKind::Instantaneous, false),

    (lai_evetr, 20, ["lai_evetr"], InterpKind::Instantaneous, false),
    (lai_dectr, 21, ["lai_dectr"], InterpKind::Instantaneous, false),
    (lai_grass, 22, ["lai_grass"], InterpKind::Instantaneous, false),

    (wu_mm_paved, 23, ["wu_mm_paved", "wuh_paved"], InterpKind::Sum, false),
    (wu_mm_bldgs, 24, ["wu_mm_bldgs", "wuh_bldgs"], InterpKind::Sum, false),
    (wu_mm_evetr, 25, ["wu_mm_evetr", "wuh_evetr"], InterpKind::Sum, false),
    (wu_mm_dectr, 26, ["wu_mm_dectr", "wuh_dectr"], InterpKind::Sum, false),
    (wu_mm_grass, 27, ["wu_mm_grass", "wuh_grass"], InterpKind::Sum, false),
    (wu_mm_bsoil, 28, ["wu_mm_bsoil", "wuh_bsoil"], InterpKind::Sum, false),
    (wu_mm_water, 29, ["wu_mm_water", "wuh_water"], InterpKind::Sum, false),

    (kdiff, 30, ["kdiff"], InterpKind::Average, false),
    (kdir, 31, ["kdir"], InterpKind::Average, false),

    // (tair_av_5d, 1, ["tair_av_5d"], InterpKind::Instantaneous),
}

pub fn field_by_name(name: &str) -> Option<SuewsField> {
    FIELD_DESCRIPTORS
        .iter()
        .find(|d| d.field.name().eq_ignore_ascii_case(name))
        .map(|d| d.field)
}

pub fn descriptor_by_name(name: &str) -> Result<&'static FieldDescriptor, BridgeError> {
    FIELD_DESCRIPTORS
        .iter()
        .find(|d| d.field.name() == name)
        .ok_or(BridgeError::BadState)
}

pub fn descriptor_by_index(index: usize) -> Option<&'static FieldDescriptor> {
    FIELD_DESCRIPTORS
        .iter()
        .find(|d| d.field.index() == index)
}

fn parse_ts5mindata_ir_field_index(name: &str) -> Option<usize> {
    let suffix = name.strip_prefix("ts5mindata_ir_")?;
    let one_based = suffix.parse::<usize>().ok()?;
    one_based.checked_sub(1)
}

pub fn descriptor_by_csv_name(name: &str) -> Option<&'static FieldDescriptor> {
    FIELD_DESCRIPTORS.iter().find(|d| {
        d.csv_names
            .iter()
            .any(|n| n.eq_ignore_ascii_case(name))
    })
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

    let expected_flat_len = SUEWS_FORCING_BASE_FLAT_LEN + ts5mindata_ir_len;

    if schema_version_runtime != SUEWS_FORCING_SCHEMA_VERSION {
        return Err(BridgeError::BadState);
    }

    if flat_len != expected_flat_len {
        return Err(BridgeError::BadState);
    }

    if field_names.len() != expected_flat_len {
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

pub fn suews_forcing_field_names() -> Vec<String> {
    FIELD_DESCRIPTORS
        .iter()
        .map(|d| d.field.name().to_string())
        .collect()
}

pub fn suews_forcing_field_names_with_ts_len(ts5mindata_ir_len: usize) -> Vec<String> {
    let mut names: Vec<String> = FIELD_DESCRIPTORS
        .iter()
        .map(|d| d.field.name().to_string())
        .collect();

    for idx in 0..ts5mindata_ir_len {
        names.push(format!("ts5mindata_ir_{}", idx + 1));
    }

    names
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

    for (name, value) in values {
        if let Ok(desc) = descriptor_by_name(name) {
            (desc.set)(&mut state, *value);
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
