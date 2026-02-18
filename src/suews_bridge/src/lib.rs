mod anthro_emis_prm;
mod anthro_heat_prm;
mod anthroemis;
mod atm;
mod bioco2;
mod building_archetype_prm;
mod codec;
mod conductance;
mod config;
mod core;
mod ehc_prm;
mod error;
mod error_entry;
mod error_state;
mod ffi;
mod flag;
mod forcing;
#[cfg(feature = "physics")]
mod forcing_io;
mod heat_state;
mod hydro_state;
mod irrig_daywater;
mod irrigation_prm;
mod lai;
mod lc_bldg_prm;
mod lc_bsoil_prm;
mod lc_dectr_prm;
mod lc_evetr_prm;
mod lc_grass_prm;
mod lc_paved_prm;
mod lc_water_prm;
mod lumps;
mod nhood;
mod ohm_coef_lc;
mod ohm_prm;
mod output_block;
#[cfg(feature = "physics")]
mod output_io;
mod output_line;
mod phenology;
mod roughness;
#[cfg(feature = "physics")]
mod sim;
mod snow;
mod snow_prm;
mod soil;
mod solar;
mod spartacus_layer_prm;
mod spartacus_prm;
mod stebbs_prm;
mod stebbs_state;
mod suews_debug;
mod suews_site;
mod suews_state;
mod suews_state_block;
mod surf_store;
mod timer;
mod water_dist;
#[cfg(feature = "physics")]
mod yaml_config;

pub use anthro_emis_prm::*;
pub use anthro_heat_prm::*;
pub use anthroemis::*;
pub use atm::*;
pub use bioco2::*;
pub use building_archetype_prm::*;
pub use codec::*;
pub use conductance::*;
pub use config::*;
pub use core::*;
pub use ehc_prm::*;
pub use error::*;
pub use error_entry::*;
pub use error_state::*;
pub use flag::*;
pub use forcing::*;
#[cfg(feature = "physics")]
pub use forcing_io::*;
pub use heat_state::*;
pub use hydro_state::*;
pub use irrig_daywater::*;
pub use irrigation_prm::*;
pub use lai::*;
pub use lc_bldg_prm::*;
pub use lc_bsoil_prm::*;
pub use lc_dectr_prm::*;
pub use lc_evetr_prm::*;
pub use lc_grass_prm::*;
pub use lc_paved_prm::*;
pub use lc_water_prm::*;
pub use lumps::*;
pub use nhood::*;
pub use ohm_coef_lc::*;
pub use ohm_prm::*;
pub use output_block::*;
#[cfg(feature = "physics")]
pub use output_io::*;
pub use output_line::*;
pub use phenology::*;
pub use roughness::*;
#[cfg(feature = "physics")]
pub use sim::{
    run_from_config_str_and_forcing, run_simulation, SimulationInput, SimulationOutput,
    SiteScalars, OUTPUT_SUEWS_COLS,
};
pub use snow::*;
pub use snow_prm::*;
pub use soil::*;
pub use solar::*;
pub use spartacus_layer_prm::*;
pub use spartacus_prm::*;
pub use stebbs_prm::*;
pub use stebbs_state::*;
pub use suews_debug::*;
pub use suews_site::*;
pub use suews_state::*;
pub use suews_state_block::*;
pub use surf_store::*;
pub use timer::*;
pub use water_dist::*;
#[cfg(feature = "physics")]
pub use yaml_config::*;
#[cfg(feature = "python")]
mod python_bindings {
    use crate::*;
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
            let state = ohm_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid OHM_STATE values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = ohm_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid OHM_STATE field mapping: {err}"))
            })?;
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

            self.state = ohm_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid OHM_STATE field mapping: {err}"))
            })?;
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

    #[pyclass(name = "SuewsConfig")]
    pub struct PySuewsConfig {
        state: SuewsConfig,
    }

    #[pymethods]
    impl PySuewsConfig {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = suews_config_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = SuewsConfig::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = suews_config_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = SuewsConfigValuesPayload {
                schema_version,
                values,
            };
            let state = suews_config_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid SUEWS_CONFIG values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = suews_config_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid SUEWS_CONFIG field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            suews_config_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = suews_config_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            suews_config_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            suews_config_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = suews_config_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SUEWS_CONFIG field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = suews_config_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SUEWS_CONFIG field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = SuewsConfig::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "SuewsForcing")]
    pub struct PySuewsForcing {
        state: SuewsForcing,
    }

    #[pymethods]
    impl PySuewsForcing {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = suews_forcing_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = suews_forcing_from_ordered_values(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = suews_forcing_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(
            schema_version: u32,
            values: Vec<f64>,
            dims: HashMap<String, Vec<usize>>,
        ) -> PyResult<Self> {
            let payload = SuewsForcingValuesPayload {
                schema_version,
                values,
                dims: dims.into_iter().collect(),
            };
            let state = suews_forcing_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid SUEWS_FORCING values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = suews_forcing_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid SUEWS_FORCING field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            suews_forcing_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>, HashMap<String, Vec<usize>>) {
            let payload = suews_forcing_to_values_payload(&self.state);
            (
                payload.schema_version,
                payload.values,
                payload.dims.into_iter().collect(),
            )
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            suews_forcing_to_map(&self.state)
        }

        fn update_from_dict(&mut self, values: HashMap<String, f64>) -> PyResult<()> {
            let mut mapped = suews_forcing_to_map(&self.state);
            for (name, value) in values {
                mapped.insert(name, value);
            }

            self.state = suews_forcing_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid SUEWS_FORCING field mapping: {err}"))
            })?;
            Ok(())
        }

        #[staticmethod]
        fn field_names() -> PyResult<Vec<String>> {
            suews_forcing_field_names().map_err(map_bridge_error)
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            suews_forcing_to_map(&self.state)
                .get(name)
                .copied()
                .ok_or_else(|| {
                    PyValueError::new_err(format!("unknown SUEWS_FORCING field name: {name}"))
                })
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let mut mapped = suews_forcing_to_map(&self.state);
            mapped.insert(name.to_string(), value);
            self.state = suews_forcing_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid SUEWS_FORCING field mapping: {err}"))
            })?;
            Ok(())
        }

        fn ts5mindata_ir(&self) -> Vec<f64> {
            self.state.ts5mindata_ir.clone()
        }

        fn set_ts5mindata_ir(&mut self, values: Vec<f64>) {
            self.state.ts5mindata_ir = values;
        }
    }

    #[pyclass(name = "HydroState")]
    pub struct PyHydroState {
        state: crate::HydroState,
    }

    #[pymethods]
    impl PyHydroState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::hydro_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = crate::hydro_state_from_ordered_values(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state =
                crate::hydro_state_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(
            schema_version: u32,
            values: Vec<f64>,
            dims: HashMap<String, Vec<usize>>,
        ) -> PyResult<Self> {
            let payload = crate::HydroStateValuesPayload {
                schema_version,
                values,
                dims: dims.into_iter().collect(),
            };
            let state = crate::hydro_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid HYDRO_STATE values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = crate::hydro_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid HYDRO_STATE field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            crate::hydro_state_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>, HashMap<String, Vec<usize>>) {
            let payload = crate::hydro_state_to_values_payload(&self.state);
            (
                payload.schema_version,
                payload.values,
                payload.dims.into_iter().collect(),
            )
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            crate::hydro_state_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> PyResult<Vec<String>> {
            crate::hydro_state_field_names().map_err(map_bridge_error)
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            crate::hydro_state_to_map(&self.state)
                .get(name)
                .copied()
                .ok_or_else(|| {
                    PyValueError::new_err(format!("unknown HYDRO_STATE field name: {name}"))
                })
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let mut mapped = crate::hydro_state_to_map(&self.state);
            mapped.insert(name.to_string(), value);
            self.state = crate::hydro_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid HYDRO_STATE field mapping: {err}"))
            })?;
            Ok(())
        }
    }

    #[pyclass(name = "HeatState")]
    pub struct PyHeatState {
        state: crate::HeatState,
    }

    #[pymethods]
    impl PyHeatState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::heat_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = crate::heat_state_from_ordered_values(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = crate::heat_state_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(
            schema_version: u32,
            values: Vec<f64>,
            dims: HashMap<String, Vec<usize>>,
        ) -> PyResult<Self> {
            let payload = crate::HeatStateValuesPayload {
                schema_version,
                values,
                dims: dims.into_iter().collect(),
            };
            let state = crate::heat_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid HEAT_STATE values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = crate::heat_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid HEAT_STATE field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            crate::heat_state_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>, HashMap<String, Vec<usize>>) {
            let payload = crate::heat_state_to_values_payload(&self.state);
            (
                payload.schema_version,
                payload.values,
                payload.dims.into_iter().collect(),
            )
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            crate::heat_state_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> PyResult<Vec<String>> {
            crate::heat_state_field_names().map_err(map_bridge_error)
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            crate::heat_state_to_map(&self.state)
                .get(name)
                .copied()
                .ok_or_else(|| {
                    PyValueError::new_err(format!("unknown HEAT_STATE field name: {name}"))
                })
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let mut mapped = crate::heat_state_to_map(&self.state);
            mapped.insert(name.to_string(), value);
            self.state = crate::heat_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid HEAT_STATE field mapping: {err}"))
            })?;
            Ok(())
        }
    }

    #[pyclass(name = "SuewsTimer")]
    pub struct PySuewsTimer {
        state: SuewsTimer,
    }

    #[pymethods]
    impl PySuewsTimer {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = suews_timer_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = SuewsTimer::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = suews_timer_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = SuewsTimerValuesPayload {
                schema_version,
                values,
            };
            let state = suews_timer_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid SUEWS_TIMER values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = suews_timer_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid SUEWS_TIMER field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            suews_timer_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = suews_timer_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            suews_timer_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            suews_timer_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = suews_timer_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SUEWS_TIMER field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = suews_timer_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SUEWS_TIMER field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = SuewsTimer::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "FlagState")]
    pub struct PyFlagState {
        state: FlagState,
    }

    #[pymethods]
    impl PyFlagState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = flag_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = FlagState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = flag_state_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = FlagStateValuesPayload {
                schema_version,
                values,
            };
            let state = flag_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid FLAG_STATE values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = flag_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid flag_STATE field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            flag_state_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = flag_state_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            flag_state_to_map(&self.state)
        }

        fn update_from_dict(&mut self, values: HashMap<String, f64>) -> PyResult<()> {
            let mut mapped = flag_state_to_map(&self.state);
            for (name, value) in values {
                mapped.insert(name, value);
            }

            self.state = flag_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid flag_STATE field mapping: {err}"))
            })?;
            Ok(())
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            flag_state_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = flag_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown flag_STATE field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = flag_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown flag_STATE field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = FlagState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }

        #[getter]
        fn flag_converge(&self) -> bool {
            self.state.flag_converge
        }

        #[setter]
        fn set_flag_converge(&mut self, value: bool) {
            self.state.flag_converge = value;
        }

        #[getter]
        fn i_iter(&self) -> i32 {
            self.state.i_iter
        }

        #[setter]
        fn set_i_iter(&mut self, value: i32) {
            self.state.i_iter = value;
        }

        #[getter]
        fn stebbs_bldg_init(&self) -> i32 {
            self.state.stebbs_bldg_init
        }

        #[setter]
        fn set_stebbs_bldg_init(&mut self, value: i32) {
            self.state.stebbs_bldg_init = value;
        }

        #[getter]
        fn snow_warning_shown(&self) -> bool {
            self.state.snow_warning_shown
        }

        #[setter]
        fn set_snow_warning_shown(&mut self, value: bool) {
            self.state.snow_warning_shown = value;
        }

        #[getter]
        fn iter_safe(&self) -> bool {
            self.state.iter_safe
        }

        #[setter]
        fn set_iter_safe(&mut self, value: bool) {
            self.state.iter_safe = value;
        }
    }

    #[pyclass(name = "AnthroEmisState")]
    pub struct PyAnthroEmisState {
        state: AnthroEmisState,
    }

    #[pymethods]
    impl PyAnthroEmisState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = anthroemis_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = AnthroEmisState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = anthroemis_state_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = AnthroEmisStateValuesPayload {
                schema_version,
                values,
            };
            let state = anthroemis_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid anthroEmis_STATE values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = anthroemis_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid anthroEmis_STATE field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            anthroemis_state_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = anthroemis_state_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            anthroemis_state_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            anthroemis_state_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = anthroemis_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown anthroEmis_STATE field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = anthroemis_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown anthroEmis_STATE field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = AnthroEmisState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "AnthroHeatPrm")]
    pub struct PyAnthroHeatPrm {
        state: AnthroHeatPrm,
    }

    #[pymethods]
    impl PyAnthroHeatPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = anthro_heat_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = AnthroHeatPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = anthro_heat_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = AnthroHeatPrmValuesPayload {
                schema_version,
                values,
            };
            let state = anthro_heat_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid anthroHEAT_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = anthro_heat_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid anthroHEAT_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            anthro_heat_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = anthro_heat_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            anthro_heat_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            anthro_heat_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = anthro_heat_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown anthroHEAT_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = anthro_heat_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown anthroHEAT_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = AnthroHeatPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "AnthroEmisPrm")]
    pub struct PyAnthroEmisPrm {
        state: AnthroEmisPrm,
    }

    #[pymethods]
    impl PyAnthroEmisPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = anthro_emis_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = AnthroEmisPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = anthro_emis_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = AnthroEmisPrmValuesPayload {
                schema_version,
                values,
            };
            let state = anthro_emis_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid anthroEMIS_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = anthro_emis_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid anthroEMIS_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            anthro_emis_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = anthro_emis_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            anthro_emis_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            anthro_emis_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = anthro_emis_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown anthroEMIS_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = anthro_emis_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown anthroEMIS_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = AnthroEmisPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "SolarState")]
    pub struct PySolarState {
        state: SolarState,
    }

    #[pymethods]
    impl PySolarState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = solar_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = SolarState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = solar_state_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = SolarStateValuesPayload {
                schema_version,
                values,
            };
            let state = solar_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid solar_State values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = solar_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid solar_State field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            solar_state_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = solar_state_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            solar_state_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            solar_state_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = solar_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown solar_State field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = solar_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown solar_State field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = SolarState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "AtmState")]
    pub struct PyAtmState {
        state: AtmState,
    }

    #[pymethods]
    impl PyAtmState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = atm_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = AtmState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = atm_state_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = AtmStateValuesPayload {
                schema_version,
                values,
            };
            let state = atm_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid atm_state values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = atm_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid atm_state field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            atm_state_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = atm_state_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            atm_state_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            atm_state_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = atm_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown atm_state field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = atm_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown atm_state field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = AtmState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "PhenologyState")]
    pub struct PyPhenologyState {
        state: PhenologyState,
    }

    #[pymethods]
    impl PyPhenologyState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = phenology_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = PhenologyState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = phenology_state_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = PhenologyStateValuesPayload {
                schema_version,
                values,
            };
            let state = phenology_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid PHENOLOGY_STATE values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = phenology_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid PHENOLOGY_STATE field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            phenology_state_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = phenology_state_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            phenology_state_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            phenology_state_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = phenology_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown PHENOLOGY_STATE field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = phenology_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown PHENOLOGY_STATE field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = PhenologyState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "SnowState")]
    pub struct PySnowState {
        state: SnowState,
    }

    #[pymethods]
    impl PySnowState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = snow_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = SnowState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = snow_state_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = SnowStateValuesPayload {
                schema_version,
                values,
            };
            let state = snow_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid SNOW_STATE values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = snow_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid SNOW_STATE field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            snow_state_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = snow_state_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            snow_state_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            snow_state_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = snow_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SNOW_STATE field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = snow_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SNOW_STATE field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = SnowState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "SnowPrm")]
    pub struct PySnowPrm {
        state: SnowPrm,
    }

    #[pymethods]
    impl PySnowPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = snow_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = SnowPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = snow_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = SnowPrmValuesPayload {
                schema_version,
                values,
            };
            let state = snow_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid SNOW_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = snow_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid SNOW_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            snow_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = snow_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            snow_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            snow_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = snow_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SNOW_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = snow_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SNOW_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = SnowPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "SoilPrm")]
    pub struct PySoilPrm {
        state: SoilPrm,
    }

    #[pymethods]
    impl PySoilPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = soil_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = SoilPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = soil_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = SoilPrmValuesPayload {
                schema_version,
                values,
            };
            let state = soil_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid SOIL_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = soil_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid SOIL_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            soil_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = soil_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            soil_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            soil_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = soil_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SOIL_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = soil_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SOIL_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = SoilPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "LumpsPrm")]
    pub struct PyLumpsPrm {
        state: LumpsPrm,
    }

    #[pymethods]
    impl PyLumpsPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = lumps_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = LumpsPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = lumps_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = LumpsPrmValuesPayload {
                schema_version,
                values,
            };
            let state = lumps_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid LUMPS_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = lumps_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid LUMPS_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            lumps_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = lumps_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            lumps_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            lumps_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = lumps_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LUMPS_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = lumps_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LUMPS_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = LumpsPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "BioCo2Prm")]
    pub struct PyBioCo2Prm {
        state: BioCo2Prm,
    }

    #[pymethods]
    impl PyBioCo2Prm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = bioco2_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = BioCo2Prm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = bioco2_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = BioCo2PrmValuesPayload {
                schema_version,
                values,
            };
            let state = bioco2_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid bioCO2_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = bioco2_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid bioCO2_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            bioco2_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = bioco2_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            bioco2_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            bioco2_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = bioco2_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown bioCO2_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = bioco2_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown bioCO2_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = BioCo2Prm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "LaiPrm")]
    pub struct PyLaiPrm {
        state: LaiPrm,
    }

    #[pymethods]
    impl PyLaiPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = lai_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = LaiPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = lai_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = LaiPrmValuesPayload {
                schema_version,
                values,
            };
            let state = lai_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid LAI_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = lai_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid LAI_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            lai_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = lai_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            lai_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            lai_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = lai_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LAI_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = lai_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LAI_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = LaiPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "ConductancePrm")]
    pub struct PyConductancePrm {
        state: ConductancePrm,
    }

    #[pymethods]
    impl PyConductancePrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = conductance_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = ConductancePrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = conductance_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = ConductancePrmValuesPayload {
                schema_version,
                values,
            };
            let state = conductance_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid CONDUCTANCE_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = conductance_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid CONDUCTANCE_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            conductance_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = conductance_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            conductance_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            conductance_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = conductance_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown CONDUCTANCE_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = conductance_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown CONDUCTANCE_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = ConductancePrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "SurfStorePrm")]
    pub struct PySurfStorePrm {
        state: SurfStorePrm,
    }

    #[pymethods]
    impl PySurfStorePrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = surf_store_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = SurfStorePrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = surf_store_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = SurfStorePrmValuesPayload {
                schema_version,
                values,
            };
            let state = surf_store_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid SURF_STORE_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = surf_store_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid SURF_STORE_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            surf_store_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = surf_store_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            surf_store_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            surf_store_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = surf_store_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SURF_STORE_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = surf_store_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown SURF_STORE_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = SurfStorePrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "WaterDistPrm")]
    pub struct PyWaterDistPrm {
        state: WaterDistPrm,
    }

    #[pymethods]
    impl PyWaterDistPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = water_dist_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = WaterDistPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = water_dist_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = WaterDistPrmValuesPayload {
                schema_version,
                values,
            };
            let state = water_dist_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid WATER_DIST_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = water_dist_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid WATER_DIST_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            water_dist_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = water_dist_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            water_dist_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            water_dist_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = water_dist_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown WATER_DIST_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = water_dist_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown WATER_DIST_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = WaterDistPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "LcPavedPrm")]
    pub struct PyLcPavedPrm {
        state: crate::LcPavedPrm,
    }

    #[pymethods]
    impl PyLcPavedPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::lc_paved_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = crate::LcPavedPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state =
                crate::lc_paved_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = crate::LcPavedPrmValuesPayload {
                schema_version,
                values,
            };
            let state = crate::lc_paved_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_PAVED_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = crate::lc_paved_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_PAVED_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            crate::lc_paved_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = crate::lc_paved_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            crate::lc_paved_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            crate::lc_paved_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = crate::lc_paved_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_PAVED_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = crate::lc_paved_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_PAVED_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = crate::LcPavedPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "LcBldgPrm")]
    pub struct PyLcBldgPrm {
        state: crate::LcBldgPrm,
    }

    #[pymethods]
    impl PyLcBldgPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::lc_bldg_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = crate::LcBldgPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state =
                crate::lc_bldg_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = crate::LcBldgPrmValuesPayload {
                schema_version,
                values,
            };
            let state = crate::lc_bldg_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_BLDG_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = crate::lc_bldg_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_BLDG_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            crate::lc_bldg_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = crate::lc_bldg_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            crate::lc_bldg_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            crate::lc_bldg_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = crate::lc_bldg_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_BLDG_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = crate::lc_bldg_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_BLDG_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = crate::LcBldgPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "LcBsoilPrm")]
    pub struct PyLcBsoilPrm {
        state: crate::LcBsoilPrm,
    }

    #[pymethods]
    impl PyLcBsoilPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::lc_bsoil_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = crate::LcBsoilPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state =
                crate::lc_bsoil_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = crate::LcBsoilPrmValuesPayload {
                schema_version,
                values,
            };
            let state = crate::lc_bsoil_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_BSOIL_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = crate::lc_bsoil_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_BSOIL_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            crate::lc_bsoil_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = crate::lc_bsoil_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            crate::lc_bsoil_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            crate::lc_bsoil_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = crate::lc_bsoil_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_BSOIL_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = crate::lc_bsoil_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_BSOIL_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = crate::LcBsoilPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "LcWaterPrm")]
    pub struct PyLcWaterPrm {
        state: crate::LcWaterPrm,
    }

    #[pymethods]
    impl PyLcWaterPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::lc_water_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = crate::LcWaterPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state =
                crate::lc_water_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = crate::LcWaterPrmValuesPayload {
                schema_version,
                values,
            };
            let state = crate::lc_water_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_WATER_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = crate::lc_water_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_WATER_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            crate::lc_water_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = crate::lc_water_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            crate::lc_water_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            crate::lc_water_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = crate::lc_water_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_WATER_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = crate::lc_water_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_WATER_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = crate::LcWaterPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "LcDectrPrm")]
    pub struct PyLcDectrPrm {
        state: crate::LcDectrPrm,
    }

    #[pymethods]
    impl PyLcDectrPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::lc_dectr_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = crate::LcDectrPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state =
                crate::lc_dectr_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = crate::LcDectrPrmValuesPayload {
                schema_version,
                values,
            };
            let state = crate::lc_dectr_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_DECTR_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = crate::lc_dectr_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_DECTR_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            crate::lc_dectr_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = crate::lc_dectr_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            crate::lc_dectr_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            crate::lc_dectr_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = crate::lc_dectr_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_DECTR_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = crate::lc_dectr_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_DECTR_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = crate::LcDectrPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "LcEvetrPrm")]
    pub struct PyLcEvetrPrm {
        state: crate::LcEvetrPrm,
    }

    #[pymethods]
    impl PyLcEvetrPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::lc_evetr_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = crate::LcEvetrPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state =
                crate::lc_evetr_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = crate::LcEvetrPrmValuesPayload {
                schema_version,
                values,
            };
            let state = crate::lc_evetr_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_EVETR_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = crate::lc_evetr_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_EVETR_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            crate::lc_evetr_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = crate::lc_evetr_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            crate::lc_evetr_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            crate::lc_evetr_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = crate::lc_evetr_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_EVETR_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = crate::lc_evetr_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_EVETR_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = crate::LcEvetrPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "LcGrassPrm")]
    pub struct PyLcGrassPrm {
        state: crate::LcGrassPrm,
    }

    #[pymethods]
    impl PyLcGrassPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::lc_grass_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = crate::LcGrassPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state =
                crate::lc_grass_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = crate::LcGrassPrmValuesPayload {
                schema_version,
                values,
            };
            let state = crate::lc_grass_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_GRASS_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = crate::lc_grass_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid LC_GRASS_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            crate::lc_grass_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = crate::lc_grass_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            crate::lc_grass_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            crate::lc_grass_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = crate::lc_grass_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_GRASS_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = crate::lc_grass_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown LC_GRASS_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = crate::LcGrassPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "IrrigDaywater")]
    pub struct PyIrrigDaywater {
        state: IrrigDaywater,
    }

    #[pymethods]
    impl PyIrrigDaywater {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = irrig_daywater_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = IrrigDaywater::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = irrig_daywater_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = IrrigDaywaterValuesPayload {
                schema_version,
                values,
            };
            let state = irrig_daywater_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid IRRIG_daywater values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = irrig_daywater_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid IRRIG_daywater field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            irrig_daywater_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = irrig_daywater_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            irrig_daywater_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            irrig_daywater_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = irrig_daywater_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown IRRIG_daywater field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = irrig_daywater_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown IRRIG_daywater field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = IrrigDaywater::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "IrrigationPrm")]
    pub struct PyIrrigationPrm {
        state: IrrigationPrm,
    }

    #[pymethods]
    impl PyIrrigationPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = irrigation_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = IrrigationPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = irrigation_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = IrrigationPrmValuesPayload {
                schema_version,
                values,
            };
            let state = irrigation_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid IRRIGATION_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = irrigation_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid IRRIGATION_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            irrigation_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = irrigation_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            irrigation_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            irrigation_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = irrigation_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown IRRIGATION_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = irrigation_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown IRRIGATION_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = IrrigationPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "OhmCoefLc")]
    pub struct PyOhmCoefLc {
        state: OhmCoefLc,
    }

    #[pymethods]
    impl PyOhmCoefLc {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = ohm_coef_lc_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = OhmCoefLc::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = ohm_coef_lc_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = OhmCoefLcValuesPayload {
                schema_version,
                values,
            };
            let state = ohm_coef_lc_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid OHM_COEF_LC values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = ohm_coef_lc_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid OHM_COEF_LC field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            ohm_coef_lc_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = ohm_coef_lc_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            ohm_coef_lc_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            ohm_coef_lc_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = ohm_coef_lc_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown OHM_COEF_LC field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = ohm_coef_lc_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown OHM_COEF_LC field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = OhmCoefLc::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "OhmPrm")]
    pub struct PyOhmPrm {
        state: OhmPrm,
    }

    #[pymethods]
    impl PyOhmPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = ohm_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = OhmPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = ohm_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = OhmPrmValuesPayload {
                schema_version,
                values,
            };
            let state = ohm_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid OHM_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = ohm_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid OHM_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            ohm_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = ohm_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            ohm_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            ohm_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = ohm_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown OHM_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = ohm_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown OHM_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = OhmPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "BuildingArchetypePrm")]
    pub struct PyBuildingArchetypePrm {
        state: BuildingArchetypePrm,
    }

    #[pymethods]
    impl PyBuildingArchetypePrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = building_archetype_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = BuildingArchetypePrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state =
                building_archetype_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = BuildingArchetypePrmValuesPayload {
                schema_version,
                values,
            };
            let state = building_archetype_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!(
                    "invalid BUILDING_ARCHETYPE_PRM values payload: {err}"
                ))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = building_archetype_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!(
                    "invalid BUILDING_ARCHETYPE_PRM field mapping: {err}"
                ))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            building_archetype_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = building_archetype_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            building_archetype_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            building_archetype_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = building_archetype_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown BUILDING_ARCHETYPE_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = building_archetype_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown BUILDING_ARCHETYPE_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = BuildingArchetypePrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "StebbsPrm")]
    pub struct PyStebbsPrm {
        state: StebbsPrm,
    }

    #[pymethods]
    impl PyStebbsPrm {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = stebbs_prm_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = StebbsPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = stebbs_prm_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = StebbsPrmValuesPayload {
                schema_version,
                values,
            };
            let state = stebbs_prm_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid STEBBS_PRM values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = stebbs_prm_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid STEBBS_PRM field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            stebbs_prm_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = stebbs_prm_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            stebbs_prm_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            stebbs_prm_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = stebbs_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown STEBBS_PRM field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = stebbs_prm_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown STEBBS_PRM field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = StebbsPrm::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "OutputLine")]
    pub struct PyOutputLine {
        state: OutputLine,
    }

    #[pymethods]
    impl PyOutputLine {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = output_line_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = OutputLine::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = output_line_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = OutputLineValuesPayload {
                schema_version,
                values,
            };
            let state = output_line_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid output_line values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = output_line_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid output_line field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            output_line_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = output_line_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            output_line_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            output_line_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = output_line_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown output_line field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = output_line_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown output_line field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = OutputLine::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "OutputBlock")]
    pub struct PyOutputBlock {
        state: crate::OutputBlock,
    }

    #[pymethods]
    impl PyOutputBlock {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::output_block_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(
            schema_version: u32,
            values: Vec<f64>,
            dims: HashMap<String, Vec<usize>>,
        ) -> PyResult<Self> {
            let payload = crate::OutputBlockValuesPayload {
                schema_version,
                values,
                dims: dims.into_iter().collect(),
            };
            let state = crate::output_block_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid output_block values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>, HashMap<String, Vec<usize>>) {
            let payload = crate::output_block_to_values_payload(&self.state);
            (
                payload.schema_version,
                payload.values,
                payload.dims.into_iter().collect(),
            )
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            crate::output_block_field_names()
        }

        fn to_rows_dict(&self) -> PyResult<BTreeMap<String, Vec<Vec<f64>>>> {
            crate::output_block_to_rows_map(&self.state).map_err(map_bridge_error)
        }
    }

    #[pyclass(name = "ErrorEntry")]
    pub struct PyErrorEntry {
        state: crate::ErrorEntry,
    }

    #[pymethods]
    impl PyErrorEntry {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::error_entry_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(
            schema_version: u32,
            timer_values: Vec<f64>,
            message: String,
            location: String,
            is_fatal: bool,
        ) -> PyResult<Self> {
            let payload = crate::ErrorEntryValuesPayload {
                schema_version,
                timer_values,
                message,
                location,
                is_fatal,
            };
            let state = crate::error_entry_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid error_entry values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>, String, String, bool) {
            let payload = crate::error_entry_to_values_payload(&self.state);
            (
                payload.schema_version,
                payload.timer_values,
                payload.message,
                payload.location,
                payload.is_fatal,
            )
        }

        fn timer_dict(&self) -> BTreeMap<String, f64> {
            crate::suews_timer_to_map(&self.state.timer)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            crate::error_entry_field_names()
        }
    }

    #[pyclass(name = "ErrorState")]
    pub struct PyErrorState {
        state: crate::ErrorState,
    }

    #[pymethods]
    impl PyErrorState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = crate::error_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(
            schema_version: u32,
            flag: bool,
            code: i32,
            message: String,
            has_fatal: bool,
            count: usize,
            log: Vec<(Vec<f64>, String, String, bool)>,
            dims: HashMap<String, Vec<usize>>,
        ) -> PyResult<Self> {
            let log_payload = log
                .into_iter()
                .map(
                    |(timer_values, message, location, is_fatal)| crate::ErrorEntryValuesPayload {
                        schema_version: crate::error_entry_schema_version(),
                        timer_values,
                        message,
                        location,
                        is_fatal,
                    },
                )
                .collect();

            let payload = crate::ErrorStateValuesPayload {
                schema_version,
                flag,
                code,
                message,
                has_fatal,
                count,
                log: log_payload,
                dims: dims.into_iter().collect(),
            };
            let state = crate::error_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid error_state values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_values_payload(
            &self,
        ) -> (
            u32,
            bool,
            i32,
            String,
            bool,
            usize,
            Vec<(Vec<f64>, String, String, bool)>,
            HashMap<String, Vec<usize>>,
        ) {
            let payload = crate::error_state_to_values_payload(&self.state);
            let log = payload
                .log
                .into_iter()
                .map(|entry| {
                    (
                        entry.timer_values,
                        entry.message,
                        entry.location,
                        entry.is_fatal,
                    )
                })
                .collect();

            (
                payload.schema_version,
                payload.flag,
                payload.code,
                payload.message,
                payload.has_fatal,
                payload.count,
                log,
                payload.dims.into_iter().collect(),
            )
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            crate::error_state_field_names()
        }
    }

    #[pyclass(name = "RoughnessState")]
    pub struct PyRoughnessState {
        state: RoughnessState,
    }

    #[pymethods]
    impl PyRoughnessState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = roughness_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = RoughnessState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = roughness_state_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = RoughnessStateValuesPayload {
                schema_version,
                values,
            };
            let state = roughness_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid ROUGHNESS_STATE values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = roughness_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid ROUGHNESS_STATE field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            roughness_state_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = roughness_state_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            roughness_state_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            roughness_state_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = roughness_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown ROUGHNESS_STATE field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = roughness_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown ROUGHNESS_STATE field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = RoughnessState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
        }
    }

    #[pyclass(name = "NhoodState")]
    pub struct PyNhoodState {
        state: NhoodState,
    }

    #[pymethods]
    impl PyNhoodState {
        #[staticmethod]
        fn default() -> PyResult<Self> {
            let state = nhood_state_default_from_fortran().map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_flat(flat: Vec<f64>) -> PyResult<Self> {
            let state = NhoodState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values(values: Vec<f64>) -> PyResult<Self> {
            let state = nhood_state_from_ordered_values(&values).map_err(map_bridge_error)?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_values_payload(schema_version: u32, values: Vec<f64>) -> PyResult<Self> {
            let payload = NhoodStateValuesPayload {
                schema_version,
                values,
            };
            let state = nhood_state_from_values_payload(&payload).map_err(|err| {
                PyValueError::new_err(format!("invalid NHOOD_STATE values payload: {err}"))
            })?;
            Ok(Self { state })
        }

        #[staticmethod]
        fn from_dict(values: HashMap<String, f64>) -> PyResult<Self> {
            let mapped: BTreeMap<String, f64> = values.into_iter().collect();
            let state = nhood_state_from_map(&mapped).map_err(|err| {
                PyValueError::new_err(format!("invalid NHOOD_STATE field mapping: {err}"))
            })?;
            Ok(Self { state })
        }

        fn to_flat(&self) -> Vec<f64> {
            self.state.to_flat()
        }

        fn to_values(&self) -> Vec<f64> {
            nhood_state_to_ordered_values(&self.state)
        }

        fn to_values_payload(&self) -> (u32, Vec<f64>) {
            let payload = nhood_state_to_values_payload(&self.state);
            (payload.schema_version, payload.values)
        }

        fn to_dict(&self) -> BTreeMap<String, f64> {
            nhood_state_to_map(&self.state)
        }

        #[staticmethod]
        fn field_names() -> Vec<String> {
            nhood_state_field_names()
        }

        fn field_value(&self, name: &str) -> PyResult<f64> {
            let idx = nhood_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown NHOOD_STATE field name: {name}"))
            })?;
            Ok(self.state.to_flat()[idx])
        }

        fn set_field_value(&mut self, name: &str, value: f64) -> PyResult<()> {
            let idx = nhood_state_field_index(name).ok_or_else(|| {
                PyValueError::new_err(format!("unknown NHOOD_STATE field name: {name}"))
            })?;

            let mut flat = self.state.to_flat();
            flat[idx] = value;
            self.state = NhoodState::from_flat(&flat).map_err(map_bridge_error)?;
            Ok(())
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

    #[cfg(feature = "physics")]
    #[pyfunction(name = "run_suews")]
    fn run_suews_py(
        config_yaml: &str,
        forcing_block: Vec<f64>,
        len_sim: usize,
    ) -> PyResult<(Vec<f64>, usize)> {
        run_from_config_str_and_forcing(config_yaml, forcing_block, len_sim)
            .map_err(map_bridge_error)
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

    #[pyfunction(name = "suews_config_schema")]
    fn suews_config_schema_py() -> PyResult<usize> {
        suews_config_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "suews_config_schema_version")]
    fn suews_config_schema_version_py() -> u32 {
        suews_config_schema_version()
    }

    #[pyfunction(name = "suews_config_schema_version_runtime")]
    fn suews_config_schema_version_runtime_py() -> PyResult<u32> {
        suews_config_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "suews_config_schema_meta")]
    fn suews_config_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = suews_config_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "suews_config_fields")]
    fn suews_config_fields_py() -> Vec<String> {
        suews_config_field_names()
    }

    #[pyfunction(name = "suews_forcing_schema")]
    fn suews_forcing_schema_py() -> PyResult<(usize, usize)> {
        suews_forcing_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "suews_forcing_schema_version")]
    fn suews_forcing_schema_version_py() -> u32 {
        suews_forcing_schema_version()
    }

    #[pyfunction(name = "suews_forcing_schema_version_runtime")]
    fn suews_forcing_schema_version_runtime_py() -> PyResult<u32> {
        suews_forcing_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "suews_forcing_schema_meta")]
    fn suews_forcing_schema_meta_py() -> PyResult<(u32, usize, usize, Vec<String>, Vec<usize>)> {
        let meta = suews_forcing_schema_info().map_err(map_bridge_error)?;
        let ts5mindata_ir_dims = meta
            .allocatable_dims
            .get("ts5mindata_ir")
            .cloned()
            .unwrap_or_default();
        Ok((
            meta.schema_version,
            meta.flat_len,
            meta.base_flat_len,
            meta.field_names,
            ts5mindata_ir_dims,
        ))
    }

    #[pyfunction(name = "suews_forcing_fields")]
    fn suews_forcing_fields_py() -> PyResult<Vec<String>> {
        suews_forcing_field_names().map_err(map_bridge_error)
    }

    #[pyfunction(name = "hydro_state_schema")]
    fn hydro_state_schema_py() -> PyResult<(usize, [usize; 6])> {
        crate::hydro_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "hydro_state_schema_version")]
    fn hydro_state_schema_version_py() -> u32 {
        crate::hydro_state_schema_version()
    }

    #[pyfunction(name = "hydro_state_schema_version_runtime")]
    fn hydro_state_schema_version_runtime_py() -> PyResult<u32> {
        crate::hydro_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "hydro_state_schema_meta")]
    fn hydro_state_schema_meta_py(
    ) -> PyResult<(u32, usize, usize, Vec<String>, HashMap<String, Vec<usize>>)> {
        let meta = crate::hydro_state_schema_info().map_err(map_bridge_error)?;
        Ok((
            meta.schema_version,
            meta.flat_len,
            meta.base_flat_len,
            meta.field_names,
            meta.allocatable_dims.into_iter().collect(),
        ))
    }

    #[pyfunction(name = "hydro_state_fields")]
    fn hydro_state_fields_py() -> PyResult<Vec<String>> {
        crate::hydro_state_field_names().map_err(map_bridge_error)
    }

    #[pyfunction(name = "heat_state_schema")]
    fn heat_state_schema_py() -> PyResult<(usize, usize, usize)> {
        crate::heat_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "heat_state_schema_version")]
    fn heat_state_schema_version_py() -> u32 {
        crate::heat_state_schema_version()
    }

    #[pyfunction(name = "heat_state_schema_version_runtime")]
    fn heat_state_schema_version_runtime_py() -> PyResult<u32> {
        crate::heat_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "heat_state_schema_meta")]
    fn heat_state_schema_meta_py() -> PyResult<(
        u32,
        usize,
        usize,
        usize,
        usize,
        Vec<String>,
        HashMap<String, Vec<usize>>,
    )> {
        let meta = crate::heat_state_schema_info().map_err(map_bridge_error)?;
        Ok((
            meta.schema_version,
            meta.flat_len,
            meta.base_flat_len,
            meta.nlayer,
            meta.ndepth,
            meta.field_names,
            meta.allocatable_dims.into_iter().collect(),
        ))
    }

    #[pyfunction(name = "heat_state_fields")]
    fn heat_state_fields_py() -> PyResult<Vec<String>> {
        crate::heat_state_field_names().map_err(map_bridge_error)
    }

    #[pyfunction(name = "suews_timer_schema")]
    fn suews_timer_schema_py() -> PyResult<usize> {
        suews_timer_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "suews_timer_schema_version")]
    fn suews_timer_schema_version_py() -> u32 {
        suews_timer_schema_version()
    }

    #[pyfunction(name = "suews_timer_schema_version_runtime")]
    fn suews_timer_schema_version_runtime_py() -> PyResult<u32> {
        suews_timer_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "suews_timer_schema_meta")]
    fn suews_timer_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = suews_timer_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "suews_timer_fields")]
    fn suews_timer_fields_py() -> Vec<String> {
        suews_timer_field_names()
    }

    #[pyfunction(name = "flag_state_schema")]
    fn flag_state_schema_py() -> PyResult<usize> {
        flag_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "flag_state_schema_version")]
    fn flag_state_schema_version_py() -> u32 {
        flag_state_schema_version()
    }

    #[pyfunction(name = "flag_state_schema_version_runtime")]
    fn flag_state_schema_version_runtime_py() -> PyResult<u32> {
        flag_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "flag_state_schema_meta")]
    fn flag_state_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = flag_state_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "flag_state_fields")]
    fn flag_state_fields_py() -> Vec<String> {
        flag_state_field_names()
    }

    #[pyfunction(name = "anthroemis_state_schema")]
    fn anthroemis_state_schema_py() -> PyResult<usize> {
        anthroemis_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "anthroemis_state_schema_version")]
    fn anthroemis_state_schema_version_py() -> u32 {
        anthroemis_state_schema_version()
    }

    #[pyfunction(name = "anthroemis_state_schema_version_runtime")]
    fn anthroemis_state_schema_version_runtime_py() -> PyResult<u32> {
        anthroemis_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "anthroemis_state_schema_meta")]
    fn anthroemis_state_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = anthroemis_state_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "anthroemis_state_fields")]
    fn anthroemis_state_fields_py() -> Vec<String> {
        anthroemis_state_field_names()
    }

    #[pyfunction(name = "anthro_heat_prm_schema")]
    fn anthro_heat_prm_schema_py() -> PyResult<usize> {
        anthro_heat_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "anthro_heat_prm_schema_version")]
    fn anthro_heat_prm_schema_version_py() -> u32 {
        anthro_heat_prm_schema_version()
    }

    #[pyfunction(name = "anthro_heat_prm_schema_version_runtime")]
    fn anthro_heat_prm_schema_version_runtime_py() -> PyResult<u32> {
        anthro_heat_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "anthro_heat_prm_schema_meta")]
    fn anthro_heat_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = anthro_heat_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "anthro_heat_prm_fields")]
    fn anthro_heat_prm_fields_py() -> Vec<String> {
        anthro_heat_prm_field_names()
    }

    #[pyfunction(name = "anthro_emis_prm_schema")]
    fn anthro_emis_prm_schema_py() -> PyResult<usize> {
        anthro_emis_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "anthro_emis_prm_schema_version")]
    fn anthro_emis_prm_schema_version_py() -> u32 {
        anthro_emis_prm_schema_version()
    }

    #[pyfunction(name = "anthro_emis_prm_schema_version_runtime")]
    fn anthro_emis_prm_schema_version_runtime_py() -> PyResult<u32> {
        anthro_emis_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "anthro_emis_prm_schema_meta")]
    fn anthro_emis_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = anthro_emis_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "anthro_emis_prm_fields")]
    fn anthro_emis_prm_fields_py() -> Vec<String> {
        anthro_emis_prm_field_names()
    }

    #[pyfunction(name = "atm_state_schema")]
    fn atm_state_schema_py() -> PyResult<usize> {
        atm_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "atm_state_schema_version")]
    fn atm_state_schema_version_py() -> u32 {
        atm_state_schema_version()
    }

    #[pyfunction(name = "atm_state_schema_version_runtime")]
    fn atm_state_schema_version_runtime_py() -> PyResult<u32> {
        atm_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "atm_state_schema_meta")]
    fn atm_state_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = atm_state_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "atm_state_fields")]
    fn atm_state_fields_py() -> Vec<String> {
        atm_state_field_names()
    }

    #[pyfunction(name = "phenology_state_schema")]
    fn phenology_state_schema_py() -> PyResult<usize> {
        phenology_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "phenology_state_schema_version")]
    fn phenology_state_schema_version_py() -> u32 {
        phenology_state_schema_version()
    }

    #[pyfunction(name = "phenology_state_schema_version_runtime")]
    fn phenology_state_schema_version_runtime_py() -> PyResult<u32> {
        phenology_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "phenology_state_schema_meta")]
    fn phenology_state_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = phenology_state_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "phenology_state_fields")]
    fn phenology_state_fields_py() -> Vec<String> {
        phenology_state_field_names()
    }

    #[pyfunction(name = "snow_state_schema")]
    fn snow_state_schema_py() -> PyResult<usize> {
        snow_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "snow_state_schema_version")]
    fn snow_state_schema_version_py() -> u32 {
        snow_state_schema_version()
    }

    #[pyfunction(name = "snow_state_schema_version_runtime")]
    fn snow_state_schema_version_runtime_py() -> PyResult<u32> {
        snow_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "snow_state_schema_meta")]
    fn snow_state_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = snow_state_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "snow_state_fields")]
    fn snow_state_fields_py() -> Vec<String> {
        snow_state_field_names()
    }

    #[pyfunction(name = "snow_prm_schema")]
    fn snow_prm_schema_py() -> PyResult<usize> {
        snow_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "snow_prm_schema_version")]
    fn snow_prm_schema_version_py() -> u32 {
        snow_prm_schema_version()
    }

    #[pyfunction(name = "snow_prm_schema_version_runtime")]
    fn snow_prm_schema_version_runtime_py() -> PyResult<u32> {
        snow_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "snow_prm_schema_meta")]
    fn snow_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = snow_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "snow_prm_fields")]
    fn snow_prm_fields_py() -> Vec<String> {
        snow_prm_field_names()
    }

    #[pyfunction(name = "soil_prm_schema")]
    fn soil_prm_schema_py() -> PyResult<usize> {
        soil_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "soil_prm_schema_version")]
    fn soil_prm_schema_version_py() -> u32 {
        soil_prm_schema_version()
    }

    #[pyfunction(name = "soil_prm_schema_version_runtime")]
    fn soil_prm_schema_version_runtime_py() -> PyResult<u32> {
        soil_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "soil_prm_schema_meta")]
    fn soil_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = soil_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "soil_prm_fields")]
    fn soil_prm_fields_py() -> Vec<String> {
        soil_prm_field_names()
    }

    #[pyfunction(name = "lc_paved_prm_schema")]
    fn lc_paved_prm_schema_py() -> PyResult<usize> {
        crate::lc_paved_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_paved_prm_schema_version")]
    fn lc_paved_prm_schema_version_py() -> u32 {
        crate::lc_paved_prm_schema_version()
    }

    #[pyfunction(name = "lc_paved_prm_schema_version_runtime")]
    fn lc_paved_prm_schema_version_runtime_py() -> PyResult<u32> {
        crate::lc_paved_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_paved_prm_schema_meta")]
    fn lc_paved_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = crate::lc_paved_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "lc_paved_prm_fields")]
    fn lc_paved_prm_fields_py() -> Vec<String> {
        crate::lc_paved_prm_field_names()
    }

    #[pyfunction(name = "lc_bldg_prm_schema")]
    fn lc_bldg_prm_schema_py() -> PyResult<usize> {
        crate::lc_bldg_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_bldg_prm_schema_version")]
    fn lc_bldg_prm_schema_version_py() -> u32 {
        crate::lc_bldg_prm_schema_version()
    }

    #[pyfunction(name = "lc_bldg_prm_schema_version_runtime")]
    fn lc_bldg_prm_schema_version_runtime_py() -> PyResult<u32> {
        crate::lc_bldg_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_bldg_prm_schema_meta")]
    fn lc_bldg_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = crate::lc_bldg_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "lc_bldg_prm_fields")]
    fn lc_bldg_prm_fields_py() -> Vec<String> {
        crate::lc_bldg_prm_field_names()
    }

    #[pyfunction(name = "lc_bsoil_prm_schema")]
    fn lc_bsoil_prm_schema_py() -> PyResult<usize> {
        crate::lc_bsoil_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_bsoil_prm_schema_version")]
    fn lc_bsoil_prm_schema_version_py() -> u32 {
        crate::lc_bsoil_prm_schema_version()
    }

    #[pyfunction(name = "lc_bsoil_prm_schema_version_runtime")]
    fn lc_bsoil_prm_schema_version_runtime_py() -> PyResult<u32> {
        crate::lc_bsoil_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_bsoil_prm_schema_meta")]
    fn lc_bsoil_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = crate::lc_bsoil_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "lc_bsoil_prm_fields")]
    fn lc_bsoil_prm_fields_py() -> Vec<String> {
        crate::lc_bsoil_prm_field_names()
    }

    #[pyfunction(name = "lc_water_prm_schema")]
    fn lc_water_prm_schema_py() -> PyResult<usize> {
        crate::lc_water_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_water_prm_schema_version")]
    fn lc_water_prm_schema_version_py() -> u32 {
        crate::lc_water_prm_schema_version()
    }

    #[pyfunction(name = "lc_water_prm_schema_version_runtime")]
    fn lc_water_prm_schema_version_runtime_py() -> PyResult<u32> {
        crate::lc_water_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_water_prm_schema_meta")]
    fn lc_water_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = crate::lc_water_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "lc_water_prm_fields")]
    fn lc_water_prm_fields_py() -> Vec<String> {
        crate::lc_water_prm_field_names()
    }

    #[pyfunction(name = "lc_dectr_prm_schema")]
    fn lc_dectr_prm_schema_py() -> PyResult<usize> {
        crate::lc_dectr_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_dectr_prm_schema_version")]
    fn lc_dectr_prm_schema_version_py() -> u32 {
        crate::lc_dectr_prm_schema_version()
    }

    #[pyfunction(name = "lc_dectr_prm_schema_version_runtime")]
    fn lc_dectr_prm_schema_version_runtime_py() -> PyResult<u32> {
        crate::lc_dectr_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_dectr_prm_schema_meta")]
    fn lc_dectr_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = crate::lc_dectr_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "lc_dectr_prm_fields")]
    fn lc_dectr_prm_fields_py() -> Vec<String> {
        crate::lc_dectr_prm_field_names()
    }

    #[pyfunction(name = "lc_evetr_prm_schema")]
    fn lc_evetr_prm_schema_py() -> PyResult<usize> {
        crate::lc_evetr_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_evetr_prm_schema_version")]
    fn lc_evetr_prm_schema_version_py() -> u32 {
        crate::lc_evetr_prm_schema_version()
    }

    #[pyfunction(name = "lc_evetr_prm_schema_version_runtime")]
    fn lc_evetr_prm_schema_version_runtime_py() -> PyResult<u32> {
        crate::lc_evetr_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_evetr_prm_schema_meta")]
    fn lc_evetr_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = crate::lc_evetr_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "lc_evetr_prm_fields")]
    fn lc_evetr_prm_fields_py() -> Vec<String> {
        crate::lc_evetr_prm_field_names()
    }

    #[pyfunction(name = "lc_grass_prm_schema")]
    fn lc_grass_prm_schema_py() -> PyResult<usize> {
        crate::lc_grass_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_grass_prm_schema_version")]
    fn lc_grass_prm_schema_version_py() -> u32 {
        crate::lc_grass_prm_schema_version()
    }

    #[pyfunction(name = "lc_grass_prm_schema_version_runtime")]
    fn lc_grass_prm_schema_version_runtime_py() -> PyResult<u32> {
        crate::lc_grass_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lc_grass_prm_schema_meta")]
    fn lc_grass_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = crate::lc_grass_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "lc_grass_prm_fields")]
    fn lc_grass_prm_fields_py() -> Vec<String> {
        crate::lc_grass_prm_field_names()
    }

    #[pyfunction(name = "surf_store_prm_schema")]
    fn surf_store_prm_schema_py() -> PyResult<usize> {
        surf_store_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "surf_store_prm_schema_version")]
    fn surf_store_prm_schema_version_py() -> u32 {
        surf_store_prm_schema_version()
    }

    #[pyfunction(name = "surf_store_prm_schema_version_runtime")]
    fn surf_store_prm_schema_version_runtime_py() -> PyResult<u32> {
        surf_store_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "surf_store_prm_schema_meta")]
    fn surf_store_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = surf_store_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "surf_store_prm_fields")]
    fn surf_store_prm_fields_py() -> Vec<String> {
        surf_store_prm_field_names()
    }

    #[pyfunction(name = "water_dist_prm_schema")]
    fn water_dist_prm_schema_py() -> PyResult<usize> {
        water_dist_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "water_dist_prm_schema_version")]
    fn water_dist_prm_schema_version_py() -> u32 {
        water_dist_prm_schema_version()
    }

    #[pyfunction(name = "water_dist_prm_schema_version_runtime")]
    fn water_dist_prm_schema_version_runtime_py() -> PyResult<u32> {
        water_dist_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "water_dist_prm_schema_meta")]
    fn water_dist_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = water_dist_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "water_dist_prm_fields")]
    fn water_dist_prm_fields_py() -> Vec<String> {
        water_dist_prm_field_names()
    }

    #[pyfunction(name = "irrig_daywater_schema")]
    fn irrig_daywater_schema_py() -> PyResult<usize> {
        irrig_daywater_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "irrig_daywater_schema_version")]
    fn irrig_daywater_schema_version_py() -> u32 {
        irrig_daywater_schema_version()
    }

    #[pyfunction(name = "irrig_daywater_schema_version_runtime")]
    fn irrig_daywater_schema_version_runtime_py() -> PyResult<u32> {
        irrig_daywater_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "irrig_daywater_schema_meta")]
    fn irrig_daywater_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = irrig_daywater_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "irrig_daywater_fields")]
    fn irrig_daywater_fields_py() -> Vec<String> {
        irrig_daywater_field_names()
    }

    #[pyfunction(name = "irrigation_prm_schema")]
    fn irrigation_prm_schema_py() -> PyResult<usize> {
        irrigation_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "irrigation_prm_schema_version")]
    fn irrigation_prm_schema_version_py() -> u32 {
        irrigation_prm_schema_version()
    }

    #[pyfunction(name = "irrigation_prm_schema_version_runtime")]
    fn irrigation_prm_schema_version_runtime_py() -> PyResult<u32> {
        irrigation_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "irrigation_prm_schema_meta")]
    fn irrigation_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = irrigation_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "irrigation_prm_fields")]
    fn irrigation_prm_fields_py() -> Vec<String> {
        irrigation_prm_field_names()
    }

    #[pyfunction(name = "bioco2_prm_schema")]
    fn bioco2_prm_schema_py() -> PyResult<usize> {
        bioco2_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "bioco2_prm_schema_version")]
    fn bioco2_prm_schema_version_py() -> u32 {
        bioco2_prm_schema_version()
    }

    #[pyfunction(name = "bioco2_prm_schema_version_runtime")]
    fn bioco2_prm_schema_version_runtime_py() -> PyResult<u32> {
        bioco2_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "bioco2_prm_schema_meta")]
    fn bioco2_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = bioco2_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "bioco2_prm_fields")]
    fn bioco2_prm_fields_py() -> Vec<String> {
        bioco2_prm_field_names()
    }

    #[pyfunction(name = "lai_prm_schema")]
    fn lai_prm_schema_py() -> PyResult<usize> {
        lai_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lai_prm_schema_version")]
    fn lai_prm_schema_version_py() -> u32 {
        lai_prm_schema_version()
    }

    #[pyfunction(name = "lai_prm_schema_version_runtime")]
    fn lai_prm_schema_version_runtime_py() -> PyResult<u32> {
        lai_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lai_prm_schema_meta")]
    fn lai_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = lai_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "lai_prm_fields")]
    fn lai_prm_fields_py() -> Vec<String> {
        lai_prm_field_names()
    }

    #[pyfunction(name = "lumps_prm_schema")]
    fn lumps_prm_schema_py() -> PyResult<usize> {
        lumps_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lumps_prm_schema_version")]
    fn lumps_prm_schema_version_py() -> u32 {
        lumps_prm_schema_version()
    }

    #[pyfunction(name = "lumps_prm_schema_version_runtime")]
    fn lumps_prm_schema_version_runtime_py() -> PyResult<u32> {
        lumps_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "lumps_prm_schema_meta")]
    fn lumps_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = lumps_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "lumps_prm_fields")]
    fn lumps_prm_fields_py() -> Vec<String> {
        lumps_prm_field_names()
    }

    #[pyfunction(name = "conductance_prm_schema")]
    fn conductance_prm_schema_py() -> PyResult<usize> {
        conductance_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "conductance_prm_schema_version")]
    fn conductance_prm_schema_version_py() -> u32 {
        conductance_prm_schema_version()
    }

    #[pyfunction(name = "conductance_prm_schema_version_runtime")]
    fn conductance_prm_schema_version_runtime_py() -> PyResult<u32> {
        conductance_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "conductance_prm_schema_meta")]
    fn conductance_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = conductance_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "conductance_prm_fields")]
    fn conductance_prm_fields_py() -> Vec<String> {
        conductance_prm_field_names()
    }

    #[pyfunction(name = "ohm_coef_lc_schema")]
    fn ohm_coef_lc_schema_py() -> PyResult<usize> {
        ohm_coef_lc_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "ohm_coef_lc_schema_version")]
    fn ohm_coef_lc_schema_version_py() -> u32 {
        ohm_coef_lc_schema_version()
    }

    #[pyfunction(name = "ohm_coef_lc_schema_version_runtime")]
    fn ohm_coef_lc_schema_version_runtime_py() -> PyResult<u32> {
        ohm_coef_lc_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "ohm_coef_lc_schema_meta")]
    fn ohm_coef_lc_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = ohm_coef_lc_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "ohm_coef_lc_fields")]
    fn ohm_coef_lc_fields_py() -> Vec<String> {
        ohm_coef_lc_field_names()
    }

    #[pyfunction(name = "ohm_prm_schema")]
    fn ohm_prm_schema_py() -> PyResult<usize> {
        ohm_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "ohm_prm_schema_version")]
    fn ohm_prm_schema_version_py() -> u32 {
        ohm_prm_schema_version()
    }

    #[pyfunction(name = "ohm_prm_schema_version_runtime")]
    fn ohm_prm_schema_version_runtime_py() -> PyResult<u32> {
        ohm_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "ohm_prm_schema_meta")]
    fn ohm_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = ohm_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "ohm_prm_fields")]
    fn ohm_prm_fields_py() -> Vec<String> {
        ohm_prm_field_names()
    }

    #[pyfunction(name = "building_archetype_prm_schema")]
    fn building_archetype_prm_schema_py() -> PyResult<usize> {
        building_archetype_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "building_archetype_prm_schema_version")]
    fn building_archetype_prm_schema_version_py() -> u32 {
        building_archetype_prm_schema_version()
    }

    #[pyfunction(name = "building_archetype_prm_schema_version_runtime")]
    fn building_archetype_prm_schema_version_runtime_py() -> PyResult<u32> {
        building_archetype_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "building_archetype_prm_schema_meta")]
    fn building_archetype_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = building_archetype_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "building_archetype_prm_fields")]
    fn building_archetype_prm_fields_py() -> Vec<String> {
        building_archetype_prm_field_names()
    }

    #[pyfunction(name = "stebbs_prm_schema")]
    fn stebbs_prm_schema_py() -> PyResult<usize> {
        stebbs_prm_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "stebbs_prm_schema_version")]
    fn stebbs_prm_schema_version_py() -> u32 {
        stebbs_prm_schema_version()
    }

    #[pyfunction(name = "stebbs_prm_schema_version_runtime")]
    fn stebbs_prm_schema_version_runtime_py() -> PyResult<u32> {
        stebbs_prm_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "stebbs_prm_schema_meta")]
    fn stebbs_prm_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = stebbs_prm_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "stebbs_prm_fields")]
    fn stebbs_prm_fields_py() -> Vec<String> {
        stebbs_prm_field_names()
    }

    #[pyfunction(name = "output_line_schema")]
    fn output_line_schema_py() -> PyResult<usize> {
        output_line_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "output_line_schema_version")]
    fn output_line_schema_version_py() -> u32 {
        output_line_schema_version()
    }

    #[pyfunction(name = "output_line_schema_version_runtime")]
    fn output_line_schema_version_runtime_py() -> PyResult<u32> {
        output_line_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "output_line_schema_meta")]
    fn output_line_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = output_line_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "output_line_fields")]
    fn output_line_fields_py() -> Vec<String> {
        output_line_field_names()
    }

    #[pyfunction(name = "output_block_schema")]
    fn output_block_schema_py() -> PyResult<usize> {
        crate::output_block_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "output_block_schema_version")]
    fn output_block_schema_version_py() -> u32 {
        crate::output_block_schema_version()
    }

    #[pyfunction(name = "output_block_schema_version_runtime")]
    fn output_block_schema_version_runtime_py() -> PyResult<u32> {
        crate::output_block_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "output_block_schema_meta")]
    fn output_block_schema_meta_py(
    ) -> PyResult<(u32, usize, Vec<String>, HashMap<String, Vec<usize>>)> {
        let meta = crate::output_block_schema_info().map_err(map_bridge_error)?;
        Ok((
            meta.schema_version,
            meta.flat_len,
            meta.field_names,
            meta.allocatable_dims.into_iter().collect(),
        ))
    }

    #[pyfunction(name = "output_block_fields")]
    fn output_block_fields_py() -> Vec<String> {
        crate::output_block_field_names()
    }

    #[pyfunction(name = "error_entry_schema")]
    fn error_entry_schema_py() -> PyResult<(usize, usize, usize)> {
        crate::error_entry_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "error_entry_schema_version")]
    fn error_entry_schema_version_py() -> u32 {
        crate::error_entry_schema_version()
    }

    #[pyfunction(name = "error_entry_schema_version_runtime")]
    fn error_entry_schema_version_runtime_py() -> PyResult<u32> {
        crate::error_entry_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "error_entry_schema_meta")]
    fn error_entry_schema_meta_py() -> PyResult<(u32, usize, usize, Vec<String>, Vec<String>)> {
        let meta = crate::error_entry_schema_info().map_err(map_bridge_error)?;
        Ok((
            meta.schema_version,
            meta.message_len,
            meta.location_len,
            meta.field_names,
            meta.timer_field_names,
        ))
    }

    #[pyfunction(name = "error_entry_fields")]
    fn error_entry_fields_py() -> Vec<String> {
        crate::error_entry_field_names()
    }

    #[pyfunction(name = "error_state_schema")]
    fn error_state_schema_py() -> PyResult<usize> {
        crate::error_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "error_state_schema_version")]
    fn error_state_schema_version_py() -> u32 {
        crate::error_state_schema_version()
    }

    #[pyfunction(name = "error_state_schema_version_runtime")]
    fn error_state_schema_version_runtime_py() -> PyResult<u32> {
        crate::error_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "error_state_schema_meta")]
    fn error_state_schema_meta_py(
    ) -> PyResult<(u32, usize, Vec<String>, HashMap<String, Vec<usize>>)> {
        let meta = crate::error_state_schema_info().map_err(map_bridge_error)?;
        Ok((
            meta.schema_version,
            meta.message_len,
            meta.field_names,
            meta.allocatable_dims.into_iter().collect(),
        ))
    }

    #[pyfunction(name = "error_state_fields")]
    fn error_state_fields_py() -> Vec<String> {
        crate::error_state_field_names()
    }

    #[pyfunction(name = "solar_state_schema")]
    fn solar_state_schema_py() -> PyResult<usize> {
        solar_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "solar_state_schema_version")]
    fn solar_state_schema_version_py() -> u32 {
        solar_state_schema_version()
    }

    #[pyfunction(name = "solar_state_schema_version_runtime")]
    fn solar_state_schema_version_runtime_py() -> PyResult<u32> {
        solar_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "solar_state_schema_meta")]
    fn solar_state_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = solar_state_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "solar_state_fields")]
    fn solar_state_fields_py() -> Vec<String> {
        solar_state_field_names()
    }

    #[pyfunction(name = "roughness_state_schema")]
    fn roughness_state_schema_py() -> PyResult<usize> {
        roughness_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "roughness_state_schema_version")]
    fn roughness_state_schema_version_py() -> u32 {
        roughness_state_schema_version()
    }

    #[pyfunction(name = "roughness_state_schema_version_runtime")]
    fn roughness_state_schema_version_runtime_py() -> PyResult<u32> {
        roughness_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "roughness_state_schema_meta")]
    fn roughness_state_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = roughness_state_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "roughness_state_fields")]
    fn roughness_state_fields_py() -> Vec<String> {
        roughness_state_field_names()
    }

    #[pyfunction(name = "nhood_state_schema")]
    fn nhood_state_schema_py() -> PyResult<usize> {
        nhood_state_schema().map_err(map_bridge_error)
    }

    #[pyfunction(name = "nhood_state_schema_version")]
    fn nhood_state_schema_version_py() -> u32 {
        nhood_state_schema_version()
    }

    #[pyfunction(name = "nhood_state_schema_version_runtime")]
    fn nhood_state_schema_version_runtime_py() -> PyResult<u32> {
        nhood_state_schema_version_runtime().map_err(map_bridge_error)
    }

    #[pyfunction(name = "nhood_state_schema_meta")]
    fn nhood_state_schema_meta_py() -> PyResult<(u32, usize, Vec<String>)> {
        let meta = nhood_state_schema_info().map_err(map_bridge_error)?;
        Ok((meta.schema_version, meta.flat_len, meta.field_names))
    }

    #[pyfunction(name = "nhood_state_fields")]
    fn nhood_state_fields_py() -> Vec<String> {
        nhood_state_field_names()
    }

    #[pymodule]
    fn suews_bridge(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
        m.add_class::<PyOhmModel>()?;
        m.add_class::<PyOhmState>()?;
        m.add_class::<PySuewsConfig>()?;
        m.add_class::<PySuewsForcing>()?;
        m.add_class::<PyHydroState>()?;
        m.add_class::<PyHeatState>()?;
        m.add_class::<PySuewsTimer>()?;
        m.add_class::<PyFlagState>()?;
        m.add_class::<PyAnthroEmisState>()?;
        m.add_class::<PyAnthroHeatPrm>()?;
        m.add_class::<PyAnthroEmisPrm>()?;
        m.add_class::<PyAtmState>()?;
        m.add_class::<PyBuildingArchetypePrm>()?;
        m.add_class::<PyStebbsPrm>()?;
        m.add_class::<PyOutputLine>()?;
        m.add_class::<PyOutputBlock>()?;
        m.add_class::<PyErrorEntry>()?;
        m.add_class::<PyErrorState>()?;
        m.add_class::<PyPhenologyState>()?;
        m.add_class::<PySnowState>()?;
        m.add_class::<PySnowPrm>()?;
        m.add_class::<PySoilPrm>()?;
        m.add_class::<PyLcPavedPrm>()?;
        m.add_class::<PyLcBldgPrm>()?;
        m.add_class::<PyLcBsoilPrm>()?;
        m.add_class::<PyLcWaterPrm>()?;
        m.add_class::<PyLcDectrPrm>()?;
        m.add_class::<PyLcEvetrPrm>()?;
        m.add_class::<PyLcGrassPrm>()?;
        m.add_class::<PyLumpsPrm>()?;
        m.add_class::<PyBioCo2Prm>()?;
        m.add_class::<PyLaiPrm>()?;
        m.add_class::<PyConductancePrm>()?;
        m.add_class::<PySurfStorePrm>()?;
        m.add_class::<PyWaterDistPrm>()?;
        m.add_class::<PyIrrigDaywater>()?;
        m.add_class::<PyIrrigationPrm>()?;
        m.add_class::<PyOhmCoefLc>()?;
        m.add_class::<PyOhmPrm>()?;
        m.add_class::<PySolarState>()?;
        m.add_class::<PyRoughnessState>()?;
        m.add_class::<PyNhoodState>()?;
        #[cfg(feature = "physics")]
        m.add_function(wrap_pyfunction!(run_suews_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_step_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_surface_names_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_config_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_config_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_config_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_config_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_config_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_forcing_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_forcing_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            suews_forcing_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(suews_forcing_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_forcing_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(hydro_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(hydro_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(hydro_state_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(hydro_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(hydro_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(heat_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(heat_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(heat_state_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(heat_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(heat_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_timer_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_timer_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_timer_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_timer_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(suews_timer_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(flag_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(flag_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(flag_state_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(flag_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(flag_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(anthroemis_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(anthroemis_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            anthroemis_state_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(anthroemis_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(anthroemis_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(anthro_heat_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(anthro_heat_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            anthro_heat_prm_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(anthro_heat_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(anthro_heat_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(anthro_emis_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(anthro_emis_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            anthro_emis_prm_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(anthro_emis_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(anthro_emis_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(atm_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(atm_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(atm_state_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(atm_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(atm_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(phenology_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(phenology_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            phenology_state_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(phenology_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(phenology_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(snow_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(snow_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(snow_state_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(snow_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(snow_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(snow_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(snow_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(snow_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(snow_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(snow_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(soil_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(soil_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(soil_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(soil_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(soil_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_paved_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_paved_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_paved_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_paved_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_paved_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_bldg_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_bldg_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_bldg_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_bldg_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_bldg_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_bsoil_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_bsoil_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_bsoil_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_bsoil_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_bsoil_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_water_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_water_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_water_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_water_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_water_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_dectr_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_dectr_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_dectr_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_dectr_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_dectr_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_evetr_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_evetr_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_evetr_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_evetr_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_evetr_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_grass_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_grass_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_grass_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_grass_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(lc_grass_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(surf_store_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(surf_store_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            surf_store_prm_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(surf_store_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(surf_store_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(water_dist_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(water_dist_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            water_dist_prm_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(water_dist_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(water_dist_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(irrig_daywater_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(irrig_daywater_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            irrig_daywater_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(irrig_daywater_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(irrig_daywater_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(irrigation_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(irrigation_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            irrigation_prm_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(irrigation_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(irrigation_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(bioco2_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(bioco2_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(bioco2_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(bioco2_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(bioco2_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(lai_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(lai_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(lai_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(lai_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(lai_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(lumps_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(lumps_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(lumps_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(lumps_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(lumps_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(conductance_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(conductance_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            conductance_prm_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(conductance_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(conductance_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_coef_lc_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_coef_lc_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_coef_lc_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_coef_lc_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_coef_lc_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(building_archetype_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            building_archetype_prm_schema_version_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(
            building_archetype_prm_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(building_archetype_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(building_archetype_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(stebbs_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(stebbs_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(stebbs_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(stebbs_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(stebbs_prm_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(output_line_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(output_line_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(output_line_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(output_line_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(output_line_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(output_block_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(output_block_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(output_block_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(output_block_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(output_block_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(error_entry_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(error_entry_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(error_entry_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(error_entry_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(error_entry_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(error_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(error_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(error_state_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(error_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(error_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(solar_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(solar_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(solar_state_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(solar_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(solar_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(roughness_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(roughness_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(
            roughness_state_schema_version_runtime_py,
            m
        )?)?;
        m.add_function(wrap_pyfunction!(roughness_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(roughness_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(nhood_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(nhood_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(nhood_state_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(nhood_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(nhood_state_fields_py, m)?)?;
        Ok(())
    }
}
