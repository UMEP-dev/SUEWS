//! Flat-buffer codec: the contract between each Rust parameter or state
//! struct and its Fortran C-API facade (`c_api/<type>.f95`).
//!
//! Every bridged Fortran derived type travels as one flat `[f64]` buffer.
//! Three things must agree, and the codec checks them where it can:
//!
//! - the length: `SUEWS_CAPI_<TYPE>_LEN` in the facade and `<TYPE>_FLAT_LEN`
//!   in Rust (checked at run time by `<prefix>_default_from_fortran` and
//!   `<prefix>_schema_info`, and by each module's
//!   `schema_matches_expected_dimensions` test);
//! - the schema version: `SUEWS_CAPI_<TYPE>_SCHEMA_VERSION` and
//!   `<TYPE>_SCHEMA_VERSION` (checked by `<prefix>_schema_info` and by
//!   `from_values_payload`);
//! - the order: the position of each field in the facade's `*_pack` and
//!   `*_unpack` and in Rust's `from_flat`, `to_flat` and `*_field_names`.
//!   Length/schema checks and default round trips do not prove this order.
//!   A misplaced field can deliver the right number to the wrong variable;
//!   use distinct-value, kernel-facing wiring checks for affected mappings.
//!
//! Legacy construction uses Fortran defaults: `<prefix>_default_from_fortran`
//! asks the facade to pack a freshly initialised *shadow* type, so the
//! shadow's initialisers (mostly `0.0`) are the values any field keeps when
//! nothing overwrites it. The YAML reader (`yaml_config.rs`) starts from those
//! defaults and overwrites only the keys it finds.
//! This describes current fallback behaviour, not the required ownership rule:
//! model defaults belong in the Python data model. Representation initialisers
//! must not supply missing model inputs; see the BR rules in
//! `.claude/rules/rust/bridge-contract.md` and `dev-ref/RUST_BRIDGE_DESIGN.md`.
//!
//! Step-by-step instructions for adding a field: `dev-ref/RUST_BRIDGE_ADD_PARAMETER.md`.

use crate::error::BridgeError;
use serde_json::Value;
use std::collections::BTreeMap;

/// Complete schema metadata for a fixed-length [`StateCodec`] type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeSchema {
    /// Upper-case Fortran derived-type name (for example `"STEBBS_PRM"`).
    pub type_name: String,
    /// Schema version matched against the Fortran C-API facade at runtime.
    pub schema_version: u32,
    /// Expected number of `f64` elements in the flat ABI buffer.
    pub flat_len: usize,
    /// Ordered field names aligned 1:1 with the flat buffer slots.
    pub field_names: Vec<String>,
}

/// Compact schema metadata returned by per-type `*_schema_info()` helpers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimpleSchema {
    /// Schema version matched against the Fortran C-API facade at runtime.
    pub schema_version: u32,
    /// Expected number of `f64` elements in the flat ABI buffer.
    pub flat_len: usize,
    /// Ordered field names aligned 1:1 with the flat buffer slots.
    pub field_names: Vec<String>,
}

/// Versioned flat `f64` payload for a fixed-length bridged type.
#[derive(Debug, Clone, PartialEq)]
pub struct ValuesPayload {
    /// Schema version tag checked before decoding.
    pub schema_version: u32,
    /// Ordered flat values matching [`StateCodec::to_flat`].
    pub values: Vec<f64>,
}

/// Per-field axis lengths for allocatable / variable-dimension payloads
/// (for example `hydro_state`, `heat_state`, `ehc_prm`, `output_block`).
pub type PayloadDims = BTreeMap<String, Vec<usize>>;

/// Versioned flat `f64` payload augmented with runtime dimensions for
/// allocatable Fortran array members.
#[derive(Debug, Clone, PartialEq)]
pub struct ValuesPayloadWithDims {
    /// Schema version tag checked before decoding.
    pub schema_version: u32,
    /// Ordered flat values across scalar and allocatable members.
    pub values: Vec<f64>,
    /// Explicit axis dimensions required to partition allocatable slices.
    pub dims: PayloadDims,
}

impl ValuesPayloadWithDims {
    /// Wrap a fixed-length [`ValuesPayload`] with an empty dimension table.
    pub fn from_legacy(payload: &ValuesPayload) -> Self {
        Self {
            schema_version: payload.schema_version,
            values: payload.values.clone(),
            dims: PayloadDims::new(),
        }
    }

    /// Drop dimension metadata and return the underlying [`ValuesPayload`].
    pub fn into_legacy(self) -> ValuesPayload {
        ValuesPayload {
            schema_version: self.schema_version,
            values: self.values,
        }
    }
}

/// Conversion between a typed struct and its flat Fortran buffer.
///
/// `from_flat` must read, `to_flat` must write, and the module's
/// `*_field_names()` must list the fields in exactly the order the Fortran
/// facade packs them. Only the length is checked (`validate_flat_len`).
pub trait StateCodec: Sized {
    /// Return static schema metadata (type name, version, flat length, field names).
    fn schema() -> TypeSchema;
    /// Deserialise from a flat Fortran `[f64]` slice, checking length and domain rules.
    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError>;
    /// Serialise into the ordered flat `[f64]` buffer expected by `c_api/*_unpack`.
    fn to_flat(&self) -> Vec<f64>;
}

/// JSON nested-payload codec for composite containers (`SuewsSite`,
/// `SuewsState`, `SuewsStateBlock`, `SuewsDebug`) that hold heterogeneous
/// child members rather than a single fixed flat slice.
pub trait CompositeCodec: Sized {
    /// Serialise this composite container into its versioned nested JSON value.
    fn to_nested_payload(&self) -> Value;
    /// Deserialise from a versioned nested JSON value, validating member sets and schemas.
    fn from_nested_payload(payload: &Value) -> Result<Self, BridgeError>;
}

/// Return `Ok(())` when `flat.len() == expected`, or [`BridgeError::BadBuffer`] otherwise.
pub fn validate_flat_len(flat: &[f64], expected: usize) -> Result<(), BridgeError> {
    if flat.len() != expected {
        return Err(BridgeError::BadBuffer);
    }
    Ok(())
}

/// Look up the zero-based flat buffer index of `name` in `field_names`.
pub fn field_index(field_names: &[String], name: &str) -> Option<usize> {
    field_names.iter().position(|field| field == name)
}

/// Flat values keyed by field name. `zip` stops at the shorter list, so a
/// `*_field_names()` shorter than `to_flat()` silently drops the trailing
/// values; `<prefix>_schema_info` is what catches that mismatch.
pub fn to_map<T: StateCodec>(state: &T) -> BTreeMap<String, f64> {
    let schema = T::schema();
    let values = state.to_flat();
    schema.field_names.into_iter().zip(values).collect()
}

/// Build a struct from `default_state` with the named fields overwritten.
///
/// Every field not named in `values` keeps its value in `default_state`,
/// which in practice is the Fortran shadow default (see the module docs).
/// An unknown name is an error here (`BadState`); callers that match YAML
/// keys loosely, such as `yaml_config::set_mapped_value`, drop unknown keys
/// before they reach this function.
pub fn from_map<T: StateCodec>(
    values: &BTreeMap<String, f64>,
    default_state: &T,
) -> Result<T, BridgeError> {
    let schema = T::schema();
    let mut flat = default_state.to_flat();

    for (name, value) in values {
        let idx = field_index(&schema.field_names, name).ok_or(BridgeError::BadState)?;
        flat[idx] = *value;
    }

    T::from_flat(&flat)
}

/// Encode `state` into a versioned [`ValuesPayload`].
pub fn to_values_payload<T: StateCodec>(state: &T) -> ValuesPayload {
    let schema = T::schema();
    ValuesPayload {
        schema_version: schema.schema_version,
        values: state.to_flat(),
    }
}

/// Decode a versioned [`ValuesPayload`], rejecting schema-version or flat-length mismatches.
pub fn from_values_payload<T: StateCodec>(payload: &ValuesPayload) -> Result<T, BridgeError> {
    let schema = T::schema();
    if payload.schema_version != schema.schema_version {
        return Err(BridgeError::BadState);
    }

    validate_flat_len(&payload.values, schema.flat_len)?;
    T::from_flat(&payload.values)
}

/// Encode a fixed-length [`StateCodec`] value as a [`ValuesPayloadWithDims`] with empty `dims`.
pub fn to_values_payload_with_dims<T: StateCodec>(state: &T) -> ValuesPayloadWithDims {
    ValuesPayloadWithDims::from_legacy(&to_values_payload(state))
}

/// Decode a [`ValuesPayloadWithDims`] for a fixed-length [`StateCodec`] type.
pub fn from_values_payload_with_dims<T: StateCodec>(
    payload: &ValuesPayloadWithDims,
) -> Result<T, BridgeError> {
    from_values_payload(&ValuesPayload {
        schema_version: payload.schema_version,
        values: payload.values.clone(),
    })
}

/// Return the recorded axis lengths for `field_name`, if present.
pub fn field_dims<'a>(dims: &'a PayloadDims, field_name: &str) -> Option<&'a [usize]> {
    dims.get(field_name).map(Vec::as_slice)
}

/// Look up `field_name` in `dims` and require it to have `expected_rank` axes.
pub fn require_field_dims(
    dims: &PayloadDims,
    field_name: &str,
    expected_rank: usize,
) -> Result<Vec<usize>, BridgeError> {
    let field_dims = field_dims(dims, field_name).ok_or(BridgeError::BadState)?;
    validate_dims_rank(field_dims, expected_rank)?;
    Ok(field_dims.to_vec())
}

/// Verify that `dims` has exactly `expected_rank` axes, returning [`BridgeError::BadState`] otherwise.
pub fn validate_dims_rank(dims: &[usize], expected_rank: usize) -> Result<(), BridgeError> {
    if dims.len() != expected_rank {
        return Err(BridgeError::BadState);
    }
    Ok(())
}

/// Compute the total element count across `dims` using checked multiplication.
pub fn dims_element_count(dims: &[usize]) -> Result<usize, BridgeError> {
    dims.iter()
        .try_fold(1_usize, |acc, dim| acc.checked_mul(*dim))
        .ok_or(BridgeError::BadState)
}

/// Generate the per-type public functions (`<prefix>_schema`,
/// `<prefix>_schema_info`, `<prefix>_default_from_fortran`,
/// `<prefix>_from_map`, `<prefix>_to_map`, payload helpers, ...) for one
/// bridged type.
///
/// Each parameter or state module invokes it once at the bottom of the file,
/// naming its struct, its `*_FLAT_LEN` and `*_SCHEMA_VERSION` constants and the
/// three `ffi` functions of its facade (`suews_<type>_len`,
/// `suews_<type>_schema_version`, `suews_<type>_default`). Adding a field to an
/// existing type does not touch the invocation; adding a new type needs one.
///
/// Invariants this macro enforces at run time:
/// - `<prefix>_default_from_fortran`: the Fortran length equals
///   `flat_len_const`, otherwise `BadState`;
/// - `<prefix>_schema_info`: the Fortran schema version equals
///   `schema_version_const` and the length equals the number of field names.
///
/// `<prefix>_from_map` starts from `<prefix>_default_from_fortran`, so an
/// absent key means "the Fortran shadow default", usually zero.
macro_rules! impl_state_module_fns {
    (
        prefix = $prefix:ident,
        state_type = $StateType:ty,
        schema_type = $SchemaType:ty,
        payload_type = $PayloadType:ty,
        flat_len_const = $flat_len_const:expr,
        schema_version_const = $schema_version_const:expr,
        ffi_len_fn = $ffi_len_fn:path,
        ffi_schema_version_fn = $ffi_schema_version_fn:path,
        ffi_default_fn = $ffi_default_fn:path,
    ) => {
        paste::paste! {
            pub fn [<$prefix _schema>]() -> Result<usize, crate::error::BridgeError> {
                let mut n_flat = -1_i32;
                let mut err = -1_i32;

                unsafe {
                    $ffi_len_fn(&mut n_flat as *mut i32, &mut err as *mut i32);
                }

                if err != crate::ffi::SUEWS_CAPI_OK {
                    return Err(crate::error::BridgeError::from_code(err));
                }

                Ok(n_flat as usize)
            }

            pub fn [<$prefix _schema_info>]() -> Result<$SchemaType, crate::error::BridgeError> {
                let flat_len = [<$prefix _schema>]()?;
                let schema_version_runtime = [<$prefix _schema_version_runtime>]()?;
                let field_names = [<$prefix _field_names>]();

                if schema_version_runtime != $schema_version_const || flat_len != field_names.len() {
                    return Err(crate::error::BridgeError::BadState);
                }

                Ok($SchemaType {
                    schema_version: $schema_version_const,
                    flat_len,
                    field_names,
                })
            }

            pub fn [<$prefix _schema_version>]() -> u32 {
                $schema_version_const
            }

            pub fn [<$prefix _schema_version_runtime>]() -> Result<u32, crate::error::BridgeError> {
                let mut schema_version = -1_i32;
                let mut err = -1_i32;

                unsafe {
                    $ffi_schema_version_fn(&mut schema_version as *mut i32, &mut err as *mut i32);
                }

                if err != crate::ffi::SUEWS_CAPI_OK || schema_version < 0 {
                    return Err(crate::error::BridgeError::from_code(err));
                }

                Ok(schema_version as u32)
            }

            pub fn [<$prefix _field_index>](name: &str) -> Option<usize> {
                let names = [<$prefix _field_names>]();
                crate::codec::field_index(&names, name)
            }

            pub fn [<$prefix _to_map>](state: &$StateType) -> std::collections::BTreeMap<String, f64> {
                crate::codec::to_map(state)
            }

            pub fn [<$prefix _to_ordered_values>](state: &$StateType) -> Vec<f64> {
                state.to_flat()
            }

            pub fn [<$prefix _from_ordered_values>](values: &[f64]) -> Result<$StateType, crate::error::BridgeError> {
                <$StateType>::from_flat(values)
            }

            pub fn [<$prefix _to_values_payload>](state: &$StateType) -> $PayloadType {
                crate::codec::to_values_payload(state)
            }

            pub fn [<$prefix _from_values_payload>](
                payload: &$PayloadType,
            ) -> Result<$StateType, crate::error::BridgeError> {
                crate::codec::from_values_payload(payload)
            }

            pub fn [<$prefix _from_map>](
                values: &std::collections::BTreeMap<String, f64>,
            ) -> Result<$StateType, crate::error::BridgeError> {
                let default_state = [<$prefix _default_from_fortran>]()?;
                crate::codec::from_map(values, &default_state)
            }

            pub fn [<$prefix _default_from_fortran>]() -> Result<$StateType, crate::error::BridgeError> {
                let n_flat = [<$prefix _schema>]()?;
                if n_flat != $flat_len_const {
                    return Err(crate::error::BridgeError::BadState);
                }

                let mut flat = vec![0.0_f64; n_flat];
                let mut err = -1_i32;

                unsafe {
                    $ffi_default_fn(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
                }

                if err != crate::ffi::SUEWS_CAPI_OK {
                    return Err(crate::error::BridgeError::from_code(err));
                }

                <$StateType>::from_flat(&flat)
            }
        }
    };
}

pub(crate) use impl_state_module_fns;

// contract: C-CODEC
#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    struct DummyState {
        a: f64,
        b: f64,
    }

    impl StateCodec for DummyState {
        fn schema() -> TypeSchema {
            TypeSchema {
                type_name: "DUMMY_STATE".to_string(),
                schema_version: 1,
                flat_len: 2,
                field_names: vec!["a".to_string(), "b".to_string()],
            }
        }

        fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
            validate_flat_len(flat, 2)?;
            Ok(Self {
                a: flat[0],
                b: flat[1],
            })
        }

        fn to_flat(&self) -> Vec<f64> {
            vec![self.a, self.b]
        }
    }

    #[test]
    fn payload_roundtrip_works() {
        let state = DummyState { a: 1.0, b: 2.0 };
        let payload = to_values_payload(&state);
        let decoded = from_values_payload::<DummyState>(&payload).expect("payload should decode");
        assert_eq!(decoded, state);
    }

    #[test]
    fn payload_with_dims_roundtrip_works_for_fixed_state() {
        let state = DummyState { a: 2.5, b: -1.0 };
        let mut payload = to_values_payload_with_dims(&state);
        payload.dims.insert("alloc".to_string(), vec![3, 4]);

        // Legacy fixed-length codecs should decode unchanged values and ignore dims metadata.
        let decoded = from_values_payload_with_dims::<DummyState>(&payload)
            .expect("payload with dims should decode");
        assert_eq!(decoded, state);
    }

    #[test]
    fn payload_with_dims_from_legacy_starts_empty() {
        let payload = ValuesPayload {
            schema_version: 7,
            values: vec![1.0, 2.0, 3.0],
        };
        let enriched = ValuesPayloadWithDims::from_legacy(&payload);

        assert_eq!(enriched.schema_version, 7);
        assert_eq!(enriched.values, vec![1.0, 2.0, 3.0]);
        assert!(enriched.dims.is_empty());
    }

    #[test]
    fn dims_helpers_validate_rank_and_count() {
        let mut dims = PayloadDims::new();
        dims.insert("arr".to_string(), vec![2, 3]);

        let arr_dims = require_field_dims(&dims, "arr", 2).expect("dims should be present");
        assert_eq!(arr_dims, vec![2, 3]);
        assert_eq!(
            dims_element_count(&arr_dims).expect("dims product should fit"),
            6
        );

        let err = require_field_dims(&dims, "arr", 1).expect_err("rank mismatch should fail");
        assert_eq!(err, BridgeError::BadState);

        let err = require_field_dims(&dims, "missing", 1).expect_err("missing dims should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn from_map_updates_default_values() {
        let default = DummyState { a: 0.0, b: 0.0 };
        let mut mapped = BTreeMap::new();
        mapped.insert("b".to_string(), 5.0);
        let state = from_map::<DummyState>(&mapped, &default).expect("map decode should work");
        assert_eq!(state, DummyState { a: 0.0, b: 5.0 });
    }
}
