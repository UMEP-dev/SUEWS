mod core;
mod error;
mod ffi;

pub use core::{
    dqndt_step, ohm_state_default_from_fortran, ohm_state_field_index, ohm_state_field_names,
    ohm_state_from_map, ohm_state_from_ordered_values, ohm_state_from_values_payload,
    ohm_state_schema, ohm_state_schema_info, ohm_state_schema_version,
    ohm_state_schema_version_runtime, ohm_state_step, ohm_state_to_map,
    ohm_state_to_ordered_values, ohm_state_to_values_payload, ohm_step, ohm_surface_names, qs_calc,
    OhmModel, OhmModelState, OhmState, OhmStateSchema, OhmStateValuesPayload, OhmStepResult, NSURF,
    OHM_STATE_FLAT_LEN, OHM_STATE_SCHEMA_VERSION, SURFACE_NAMES,
};
pub use error::BridgeError;

#[cfg(feature = "python")]
mod python_bindings {
    use crate::{
        ohm_state_default_from_fortran, ohm_state_field_index, ohm_state_field_names,
        ohm_state_from_map, ohm_state_from_ordered_values, ohm_state_from_values_payload,
        ohm_state_schema, ohm_state_schema_info, ohm_state_schema_version,
        ohm_state_schema_version_runtime, ohm_state_step, ohm_state_to_map,
        ohm_state_to_ordered_values, ohm_state_to_values_payload, ohm_step, ohm_surface_names,
        BridgeError, OhmModel, OhmState, OhmStateValuesPayload, NSURF,
    };
    use pyo3::exceptions::{PyRuntimeError, PyValueError};
    use pyo3::prelude::*;
    use std::collections::{BTreeMap, HashMap};

    fn map_bridge_error(err: BridgeError) -> PyErr {
        PyRuntimeError::new_err(err.to_string())
    }

    #[pyclass(name = "OhmModel")]
    pub struct PyOhmModel {
        model: OhmModel,
    }

    #[pymethods]
    impl PyOhmModel {
        #[new]
        #[pyo3(signature = (a1, a2, a3, dt_seconds, qn1_av=0.0, dqndt=0.0, dt_since_start=0))]
        fn new(
            a1: f64,
            a2: f64,
            a3: f64,
            dt_seconds: i32,
            qn1_av: f64,
            dqndt: f64,
            dt_since_start: i32,
        ) -> Self {
            Self {
                model: OhmModel::new(a1, a2, a3, dt_seconds, qn1_av, dqndt, dt_since_start),
            }
        }

        fn step(&mut self, qn1: f64) -> PyResult<f64> {
            self.model.step(qn1).map_err(map_bridge_error)
        }

        fn state(&self) -> (f64, f64, i32, i32, f64, f64, f64) {
            let st = self.model.state();
            (
                st.qn1_av,
                st.dqndt,
                st.dt_since_start,
                st.dt_seconds,
                st.a1,
                st.a2,
                st.a3,
            )
        }
    }

    #[pyclass(name = "OhmState")]
    pub struct PyOhmState {
        state: OhmState,
    }

    #[pymethods]
    impl PyOhmState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = ohm_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = OhmState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = ohm_state_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = OhmStateValuesPayload {
                schema_version,
                values,
            };
            let state = ohm_state_from_values_payload(&payload)
                .map_err(|_| PyValueError::new_err("invalid OHM_STATE values payload"))?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = ohm_state_from_map(&mapped)
                .map_err(|_| PyValueError::new_err("invalid OHM_STATE field mapping"))?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            ohm_state_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = ohm_state_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn flat_pairs(&self) -> Vec<(String, f64)> {
            let names = ohm_state_field_names();
            let values = self.state.to_flat();
            names.into_iter().zip(values).collect()
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            ohm_state_to_map(&self.state)
        }

        fn update_from_dict(&mut self, values: HashMap<String, f64>) -> PyResult<()> {
            let mut mapped = ohm_state_to_map(&self.state);
            for (name, value) in values {
                mapped.insert(name, value);
            }

            self.state = ohm_state_from_map(&mapped)
                .map_err(|_| PyValueError::new_err("invalid OHM_STATE field mapping"))?;
            Ok(())
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            ohm_state_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = ohm_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown OHM_STATE field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = ohm_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown OHM_STATE field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = OhmState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }

        #[getter]
        fn qn_av(&self) -> f64 {
            self.state.qn_av
        }

        #[setter]
        fn set_qn_av(&mut self, value: f64) {
            self.state.qn_av = value;
        }

        #[getter]
        fn dqndt(&self) -> f64 {
            self.state.dqndt
        }

        #[setter]
        fn set_dqndt(&mut self, value: f64) {
            self.state.dqndt = value;
        }

        #[getter]
        fn qn_s_av(&self) -> f64 {
            self.state.qn_s_av
        }

        #[setter]
        fn set_qn_s_av(&mut self, value: f64) {
            self.state.qn_s_av = value;
        }

        #[getter]
        fn dqnsdt(&self) -> f64 {
            self.state.dqnsdt
        }

        #[setter]
        fn set_dqnsdt(&mut self, value: f64) {
            self.state.dqnsdt = value;
        }

        #[getter]
        fn a1(&self) -> f64 {
            self.state.a1
        }

        #[setter]
        fn set_a1(&mut self, value: f64) {
            self.state.a1 = value;
        }

        #[getter]
        fn a2(&self) -> f64 {
            self.state.a2
        }

        #[setter]
        fn set_a2(&mut self, value: f64) {
            self.state.a2 = value;
        }

        #[getter]
        fn a3(&self) -> f64 {
            self.state.a3
        }

        #[setter]
        fn set_a3(&mut self, value: f64) {
            self.state.a3 = value;
        }

        #[getter]
        fn t2_prev(&self) -> f64 {
            self.state.t2_prev
        }

        #[setter]
        fn set_t2_prev(&mut self, value: f64) {
            self.state.t2_prev = value;
        }

        #[getter]
        fn ws_rav(&self) -> f64 {
            self.state.ws_rav
        }

        #[setter]
        fn set_ws_rav(&mut self, value: f64) {
            self.state.ws_rav = value;
        }

        #[getter]
        fn tair_prev(&self) -> f64 {
            self.state.tair_prev
        }

        #[setter]
        fn set_tair_prev(&mut self, value: f64) {
            self.state.tair_prev = value;
        }

        #[getter]
        fn iter_safe(&self) -> bool {
            self.state.iter_safe
        }

        #[setter]
        fn set_iter_safe(&mut self, value: bool) {
            self.state.iter_safe = value;
        }

        fn qn_surfs(&self) -> Vec<f64> {
            self.state.qn_surfs.to_vec()
        }

        fn set_qn_surfs(&mut self, values: Vec<f64>) -> PyResult<()> {
            if values.len() != NSURF {
                return Err(PyValueError::new_err(format!(
                    "qn_surfs requires length {NSURF}, got {}",
                    values.len()
                )));
            }
            self.state.qn_surfs.copy_from_slice(&values);
            Ok(())
        }

        fn dqndt_surf(&self) -> Vec<f64> {
            self.state.dqndt_surf.to_vec()
        }

        fn set_dqndt_surf(&mut self, values: Vec<f64>) -> PyResult<()> {
            if values.len() != NSURF {
                return Err(PyValueError::new_err(format!(
                    "dqndt_surf requires length {NSURF}, got {}",
                    values.len()
                )));
            }
            self.state.dqndt_surf.copy_from_slice(&values);
            Ok(())
        }

        fn qn_rav(&self) -> Vec<f64> {
            self.state.qn_rav.to_vec()
        }

        fn set_qn_rav(&mut self, values: Vec<f64>) -> PyResult<()> {
            if values.len() != NSURF {
                return Err(PyValueError::new_err(format!(
                    "qn_rav requires length {NSURF}, got {}",
                    values.len()
                )));
            }
            self.state.qn_rav.copy_from_slice(&values);
            Ok(())
        }

        fn dyn_a1(&self) -> Vec<f64> {
            self.state.dyn_a1.to_vec()
        }

        fn set_dyn_a1(&mut self, values: Vec<f64>) -> PyResult<()> {
            if values.len() != NSURF {
                return Err(PyValueError::new_err(format!(
                    "dyn_a1 requires length {NSURF}, got {}",
                    values.len()
                )));
            }
            self.state.dyn_a1.copy_from_slice(&values);
            Ok(())
        }

        fn dyn_a2(&self) -> Vec<f64> {
            self.state.dyn_a2.to_vec()
        }

        fn set_dyn_a2(&mut self, values: Vec<f64>) -> PyResult<()> {
            if values.len() != NSURF {
                return Err(PyValueError::new_err(format!(
                    "dyn_a2 requires length {NSURF}, got {}",
                    values.len()
                )));
            }
            self.state.dyn_a2.copy_from_slice(&values);
            Ok(())
        }

        fn dyn_a3(&self) -> Vec<f64> {
            self.state.dyn_a3.to_vec()
        }

        fn set_dyn_a3(&mut self, values: Vec<f64>) -> PyResult<()> {
            if values.len() != NSURF {
                return Err(PyValueError::new_err(format!(
                    "dyn_a3 requires length {NSURF}, got {}",
                    values.len()
                )));
            }
            self.state.dyn_a3.copy_from_slice(&values);
            Ok(())
        }

        fn step(
            &mut self,
            dt: i32,
            dt_since_start: i32,
            qn1: f64,
            a1: f64,
            a2: f64,
            a3: f64,
        ) -> PyResult<f64> {
            ohm_state_step(&mut self.state, dt, dt_since_start, qn1, a1, a2, a3)
                .map_err(map_bridge_error)
        }
    }

    #[pyfunction(name = "ohm_step")]
    fn ohm_step_py(
        dt: i32,
        dt_since_start: i32,
        qn1_av_prev: f64,
        dqndt_prev: f64,
        qn1: f64,
        a1: f64,
        a2: f64,
        a3: f64,
    ) -> PyResult<(f64, f64, f64)> {
        let out = ohm_step(dt, dt_since_start, qn1_av_prev, dqndt_prev, qn1, a1, a2, a3)
            .map_err(map_bridge_error)?;

        Ok((out.qn1_av_next, out.dqndt_next, out.qs))
    }

    #[pyfunction(name = "ohm_state_schema")]
    fn ohm_state_schema_py() -> PyResult<(usize, usize)> {
        ohm_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "ohm_state_schema_version")]
    fn ohm_state_schema_version_py() -> u32 {
        ohm_state_schema_version()
    }

    #[pyfunction(name = "ohm_state_schema_version_runtime")]
    fn ohm_state_schema_version_runtime_py() -> PyResult<u32> {
        ohm_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "ohm_state_schema_meta")]
    fn ohm_state_schema_meta_py() -> PyResult<(u32, usize, usize, Vec<String>, Vec<String>)> {
        let meta = ohm_state_schema_info().map_err(map_bridge_error)?;
        Ok((
            meta.schema_version,
            meta.flat_len,
            meta.nsurf,
            meta.field_names,
            meta.surface_names,
        ))
    }

    #[pyfunction(name = "ohm_state_fields")]
    fn ohm_state_fields_py() -> Vec<String> {
        ohm_state_field_names()
    }

    #[pyfunction(name = "ohm_surface_names")]
    fn ohm_surface_names_py() -> Vec<String> {
        ohm_surface_names()
    }

    #[pymodule]
    fn suews_core(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
        m.add_class::<PyOhmModel>()?;
        m.add_class::<PyOhmState>()?;
        m.add_function(wrap_pyfunction!(ohm_step_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_surface_names_py, m)?)?;
        Ok(())
    }
}
