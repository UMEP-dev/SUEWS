mod core;
mod error;
mod ffi;

pub use core::{
    dqndt_step, ohm_state_default_from_fortran, ohm_state_field_names, ohm_state_schema,
    ohm_state_step, ohm_step, qs_calc, OhmModel, OhmModelState, OhmState, OhmStepResult, NSURF,
    OHM_STATE_FLAT_LEN, SURFACE_NAMES,
};
pub use error::BridgeError;

#[cfg(feature = "python")]
mod python_bindings {
    use crate::{
        ohm_state_default_from_fortran, ohm_state_field_names, ohm_state_schema, ohm_state_step,
        ohm_step, BridgeError, OhmModel, OhmState, NSURF,
    };
    use pyo3::exceptions::{PyRuntimeError, PyValueError};
    use pyo3::prelude::*;

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

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
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

    #[pyfunction(name = "ohm_state_fields")]
    fn ohm_state_fields_py() -> Vec<String> {
        ohm_state_field_names()
    }

    #[pymodule]
    fn suews_core(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
        m.add_class::<PyOhmModel>()?;
        m.add_class::<PyOhmState>()?;
        m.add_function(wrap_pyfunction!(ohm_step_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_fields_py, m)?)?;
        Ok(())
    }
}
