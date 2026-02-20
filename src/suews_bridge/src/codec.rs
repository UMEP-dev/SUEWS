use crate::error::BridgeError;
use serde_json::Value;
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeSchema {
    pub type_name: String,
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimpleSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub field_names: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValuesPayload {
    pub schema_version: u32,
    pub values: Vec<f64>,
}

pub type PayloadDims = BTreeMap<String, Vec<usize>>;

#[derive(Debug, Clone, PartialEq)]
pub struct ValuesPayloadWithDims {
    pub schema_version: u32,
    pub values: Vec<f64>,
    pub dims: PayloadDims,
}

impl ValuesPayloadWithDims {
    pub fn from_legacy(payload: &ValuesPayload) -> Self {
        Self {
            schema_version: payload.schema_version,
            values: payload.values.clone(),
            dims: PayloadDims::new(),
        }
    }

    pub fn into_legacy(self) -> ValuesPayload {
        ValuesPayload {
            schema_version: self.schema_version,
            values: self.values,
        }
    }
}

pub trait StateCodec: Sized {
    fn schema() -> TypeSchema;
    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError>;
    fn to_flat(&self) -> Vec<f64>;
}

pub trait CompositeCodec: Sized {
    fn to_nested_payload(&self) -> Value;
    fn from_nested_payload(payload: &Value) -> Result<Self, BridgeError>;
}

pub fn validate_flat_len(flat: &[f64], expected: usize) -> Result<(), BridgeError> {
    if flat.len() != expected {
        return Err(BridgeError::BadBuffer);
    }
    Ok(())
}

pub fn field_index(field_names: &[String], name: &str) -> Option<usize> {
    field_names.iter().position(|field| field == name)
}

pub fn to_map<T: StateCodec>(state: &T) -> BTreeMap<String, f64> {
    let schema = T::schema();
    let values = state.to_flat();
    schema.field_names.into_iter().zip(values).collect()
}

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

pub fn to_values_payload<T: StateCodec>(state: &T) -> ValuesPayload {
    let schema = T::schema();
    ValuesPayload {
        schema_version: schema.schema_version,
        values: state.to_flat(),
    }
}

pub fn from_values_payload<T: StateCodec>(payload: &ValuesPayload) -> Result<T, BridgeError> {
    let schema = T::schema();
    if payload.schema_version != schema.schema_version {
        return Err(BridgeError::BadState);
    }

    validate_flat_len(&payload.values, schema.flat_len)?;
    T::from_flat(&payload.values)
}

pub fn to_values_payload_with_dims<T: StateCodec>(state: &T) -> ValuesPayloadWithDims {
    ValuesPayloadWithDims::from_legacy(&to_values_payload(state))
}

pub fn from_values_payload_with_dims<T: StateCodec>(
    payload: &ValuesPayloadWithDims,
) -> Result<T, BridgeError> {
    from_values_payload(&ValuesPayload {
        schema_version: payload.schema_version,
        values: payload.values.clone(),
    })
}

pub fn field_dims<'a>(dims: &'a PayloadDims, field_name: &str) -> Option<&'a [usize]> {
    dims.get(field_name).map(Vec::as_slice)
}

pub fn require_field_dims(
    dims: &PayloadDims,
    field_name: &str,
    expected_rank: usize,
) -> Result<Vec<usize>, BridgeError> {
    let field_dims = field_dims(dims, field_name).ok_or(BridgeError::BadState)?;
    validate_dims_rank(field_dims, expected_rank)?;
    Ok(field_dims.to_vec())
}

pub fn validate_dims_rank(dims: &[usize], expected_rank: usize) -> Result<(), BridgeError> {
    if dims.len() != expected_rank {
        return Err(BridgeError::BadState);
    }
    Ok(())
}

pub fn dims_element_count(dims: &[usize]) -> Result<usize, BridgeError> {
    dims.iter()
        .try_fold(1_usize, |acc, dim| acc.checked_mul(*dim))
        .ok_or(BridgeError::BadState)
}

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
