mod anthroemis;
mod atm;
mod bioco2;
mod codec;
mod conductance;
mod core;
mod error;
mod ffi;
mod flag;
mod lumps;
mod nhood;
mod ohm_coef_lc;
mod phenology;
mod roughness;
mod snow;
mod soil;
mod solar;
mod surf_store;
mod water_dist;

pub use anthroemis::{
    anthroemis_state_default_from_fortran, anthroemis_state_field_index,
    anthroemis_state_field_names, anthroemis_state_from_map, anthroemis_state_from_ordered_values,
    anthroemis_state_from_values_payload, anthroemis_state_schema, anthroemis_state_schema_info,
    anthroemis_state_schema_version, anthroemis_state_schema_version_runtime,
    anthroemis_state_to_map, anthroemis_state_to_ordered_values,
    anthroemis_state_to_values_payload, AnthroEmisState, AnthroEmisStateSchema,
    AnthroEmisStateValuesPayload, ANTHROEMIS_STATE_FLAT_LEN, ANTHROEMIS_STATE_HDD_LEN,
    ANTHROEMIS_STATE_SCHEMA_VERSION,
};
pub use atm::{
    atm_state_default_from_fortran, atm_state_field_index, atm_state_field_names,
    atm_state_from_map, atm_state_from_ordered_values, atm_state_from_values_payload,
    atm_state_schema, atm_state_schema_info, atm_state_schema_version,
    atm_state_schema_version_runtime, atm_state_to_map, atm_state_to_ordered_values,
    atm_state_to_values_payload, AtmState, AtmStateSchema, AtmStateValuesPayload,
    ATM_STATE_FLAT_LEN, ATM_STATE_SCHEMA_VERSION,
};
pub use bioco2::{
    bioco2_prm_default_from_fortran, bioco2_prm_field_index, bioco2_prm_field_names,
    bioco2_prm_from_map, bioco2_prm_from_ordered_values, bioco2_prm_from_values_payload,
    bioco2_prm_schema, bioco2_prm_schema_info, bioco2_prm_schema_version,
    bioco2_prm_schema_version_runtime, bioco2_prm_to_map, bioco2_prm_to_ordered_values,
    bioco2_prm_to_values_payload, BioCo2Prm, BioCo2PrmSchema, BioCo2PrmValuesPayload,
    BIOCO2_PRM_FLAT_LEN, BIOCO2_PRM_SCHEMA_VERSION,
};
pub use codec::{CompositeCodec, StateCodec, TypeSchema, ValuesPayload};
pub use conductance::{
    conductance_prm_default_from_fortran, conductance_prm_field_index, conductance_prm_field_names,
    conductance_prm_from_map, conductance_prm_from_ordered_values,
    conductance_prm_from_values_payload, conductance_prm_schema, conductance_prm_schema_info,
    conductance_prm_schema_version, conductance_prm_schema_version_runtime, conductance_prm_to_map,
    conductance_prm_to_ordered_values, conductance_prm_to_values_payload, ConductancePrm,
    ConductancePrmSchema, ConductancePrmValuesPayload, CONDUCTANCE_PRM_FLAT_LEN,
    CONDUCTANCE_PRM_SCHEMA_VERSION,
};
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
pub use flag::{
    flag_state_default_from_fortran, flag_state_field_index, flag_state_field_names,
    flag_state_from_map, flag_state_from_ordered_values, flag_state_from_values_payload,
    flag_state_schema, flag_state_schema_info, flag_state_schema_version,
    flag_state_schema_version_runtime, flag_state_to_map, flag_state_to_ordered_values,
    flag_state_to_values_payload, FlagState, FlagStateSchema, FlagStateValuesPayload,
    FLAG_STATE_FLAT_LEN, FLAG_STATE_SCHEMA_VERSION,
};
pub use lumps::{
    lumps_prm_default_from_fortran, lumps_prm_field_index, lumps_prm_field_names,
    lumps_prm_from_map, lumps_prm_from_ordered_values, lumps_prm_from_values_payload,
    lumps_prm_schema, lumps_prm_schema_info, lumps_prm_schema_version,
    lumps_prm_schema_version_runtime, lumps_prm_to_map, lumps_prm_to_ordered_values,
    lumps_prm_to_values_payload, LumpsPrm, LumpsPrmSchema, LumpsPrmValuesPayload,
    LUMPS_PRM_FLAT_LEN, LUMPS_PRM_SCHEMA_VERSION,
};
pub use nhood::{
    nhood_state_default_from_fortran, nhood_state_field_index, nhood_state_field_names,
    nhood_state_from_map, nhood_state_from_ordered_values, nhood_state_from_values_payload,
    nhood_state_schema, nhood_state_schema_info, nhood_state_schema_version,
    nhood_state_schema_version_runtime, nhood_state_to_map, nhood_state_to_ordered_values,
    nhood_state_to_values_payload, NhoodState, NhoodStateSchema, NhoodStateValuesPayload,
    NHOOD_STATE_FLAT_LEN, NHOOD_STATE_SCHEMA_VERSION,
};
pub use ohm_coef_lc::{
    ohm_coef_lc_default_from_fortran, ohm_coef_lc_field_index, ohm_coef_lc_field_names,
    ohm_coef_lc_from_map, ohm_coef_lc_from_ordered_values, ohm_coef_lc_from_values_payload,
    ohm_coef_lc_schema, ohm_coef_lc_schema_info, ohm_coef_lc_schema_version,
    ohm_coef_lc_schema_version_runtime, ohm_coef_lc_to_map, ohm_coef_lc_to_ordered_values,
    ohm_coef_lc_to_values_payload, OhmCoefLc, OhmCoefLcSchema, OhmCoefLcValuesPayload,
    OHM_COEF_LC_FLAT_LEN, OHM_COEF_LC_SCHEMA_VERSION,
};
pub use phenology::{
    phenology_state_default_from_fortran, phenology_state_field_index, phenology_state_field_names,
    phenology_state_from_map, phenology_state_from_ordered_values,
    phenology_state_from_values_payload, phenology_state_schema, phenology_state_schema_info,
    phenology_state_schema_version, phenology_state_schema_version_runtime, phenology_state_to_map,
    phenology_state_to_ordered_values, phenology_state_to_values_payload, PhenologyState,
    PhenologyStateSchema, PhenologyStateValuesPayload, PHENOLOGY_STATE_FLAT_LEN,
    PHENOLOGY_STATE_NVEGSURF, PHENOLOGY_STATE_SCHEMA_VERSION, PHENOLOGY_STATE_STORE_DRAIN_ROWS,
};
pub use roughness::{
    roughness_state_default_from_fortran, roughness_state_field_index, roughness_state_field_names,
    roughness_state_from_map, roughness_state_from_ordered_values,
    roughness_state_from_values_payload, roughness_state_schema, roughness_state_schema_info,
    roughness_state_schema_version, roughness_state_schema_version_runtime, roughness_state_to_map,
    roughness_state_to_ordered_values, roughness_state_to_values_payload, RoughnessState,
    RoughnessStateSchema, RoughnessStateValuesPayload, ROUGHNESS_STATE_FLAT_LEN,
    ROUGHNESS_STATE_SCHEMA_VERSION,
};
pub use snow::{
    snow_state_default_from_fortran, snow_state_field_index, snow_state_field_names,
    snow_state_from_map, snow_state_from_ordered_values, snow_state_from_values_payload,
    snow_state_schema, snow_state_schema_info, snow_state_schema_version,
    snow_state_schema_version_runtime, snow_state_to_map, snow_state_to_ordered_values,
    snow_state_to_values_payload, SnowState, SnowStateSchema, SnowStateValuesPayload,
    SNOW_STATE_FLAT_LEN, SNOW_STATE_REMOVAL_LEN, SNOW_STATE_SCHEMA_VERSION,
};
pub use soil::{
    soil_prm_default_from_fortran, soil_prm_field_index, soil_prm_field_names, soil_prm_from_map,
    soil_prm_from_ordered_values, soil_prm_from_values_payload, soil_prm_schema,
    soil_prm_schema_info, soil_prm_schema_version, soil_prm_schema_version_runtime,
    soil_prm_to_map, soil_prm_to_ordered_values, soil_prm_to_values_payload, SoilPrm,
    SoilPrmSchema, SoilPrmValuesPayload, SOIL_PRM_FLAT_LEN, SOIL_PRM_SCHEMA_VERSION,
};
pub use solar::{
    solar_state_default_from_fortran, solar_state_field_index, solar_state_field_names,
    solar_state_from_map, solar_state_from_ordered_values, solar_state_from_values_payload,
    solar_state_schema, solar_state_schema_info, solar_state_schema_version,
    solar_state_schema_version_runtime, solar_state_to_map, solar_state_to_ordered_values,
    solar_state_to_values_payload, SolarState, SolarStateSchema, SolarStateValuesPayload,
    SOLAR_STATE_FLAT_LEN, SOLAR_STATE_SCHEMA_VERSION,
};
pub use surf_store::{
    surf_store_prm_default_from_fortran, surf_store_prm_field_index, surf_store_prm_field_names,
    surf_store_prm_from_map, surf_store_prm_from_ordered_values,
    surf_store_prm_from_values_payload, surf_store_prm_schema, surf_store_prm_schema_info,
    surf_store_prm_schema_version, surf_store_prm_schema_version_runtime, surf_store_prm_to_map,
    surf_store_prm_to_ordered_values, surf_store_prm_to_values_payload, SurfStorePrm,
    SurfStorePrmSchema, SurfStorePrmValuesPayload, SURF_STORE_PRM_FLAT_LEN,
    SURF_STORE_PRM_SCHEMA_VERSION,
};
pub use water_dist::{
    water_dist_prm_default_from_fortran, water_dist_prm_field_index, water_dist_prm_field_names,
    water_dist_prm_from_map, water_dist_prm_from_ordered_values,
    water_dist_prm_from_values_payload, water_dist_prm_schema, water_dist_prm_schema_info,
    water_dist_prm_schema_version, water_dist_prm_schema_version_runtime, water_dist_prm_to_map,
    water_dist_prm_to_ordered_values, water_dist_prm_to_values_payload, WaterDistPrm,
    WaterDistPrmSchema, WaterDistPrmValuesPayload, WATER_DIST_PRM_FLAT_LEN,
    WATER_DIST_PRM_SCHEMA_VERSION,
};

#[cfg(feature = "python")]
mod python_bindings {
    use crate::{
        anthroemis_state_default_from_fortran, anthroemis_state_field_index,
        anthroemis_state_field_names, anthroemis_state_from_map,
        anthroemis_state_from_ordered_values, anthroemis_state_from_values_payload,
        anthroemis_state_schema, anthroemis_state_schema_info, anthroemis_state_schema_version,
        anthroemis_state_schema_version_runtime, anthroemis_state_to_map,
        anthroemis_state_to_ordered_values, anthroemis_state_to_values_payload,
        atm_state_default_from_fortran, atm_state_field_index, atm_state_field_names,
        atm_state_from_map, atm_state_from_ordered_values, atm_state_from_values_payload,
        atm_state_schema, atm_state_schema_info, atm_state_schema_version,
        atm_state_schema_version_runtime, atm_state_to_map, atm_state_to_ordered_values,
        atm_state_to_values_payload, bioco2_prm_default_from_fortran, bioco2_prm_field_index,
        bioco2_prm_field_names, bioco2_prm_from_map, bioco2_prm_from_ordered_values,
        bioco2_prm_from_values_payload, bioco2_prm_schema, bioco2_prm_schema_info,
        bioco2_prm_schema_version, bioco2_prm_schema_version_runtime, bioco2_prm_to_map,
        bioco2_prm_to_ordered_values, bioco2_prm_to_values_payload,
        conductance_prm_default_from_fortran, conductance_prm_field_index,
        conductance_prm_field_names, conductance_prm_from_map, conductance_prm_from_ordered_values,
        conductance_prm_from_values_payload, conductance_prm_schema, conductance_prm_schema_info,
        conductance_prm_schema_version, conductance_prm_schema_version_runtime,
        conductance_prm_to_map, conductance_prm_to_ordered_values,
        conductance_prm_to_values_payload, flag_state_default_from_fortran, flag_state_field_index,
        flag_state_field_names, flag_state_from_map, flag_state_from_ordered_values,
        flag_state_from_values_payload, flag_state_schema, flag_state_schema_info,
        flag_state_schema_version, flag_state_schema_version_runtime, flag_state_to_map,
        flag_state_to_ordered_values, flag_state_to_values_payload, lumps_prm_default_from_fortran,
        lumps_prm_field_index, lumps_prm_field_names, lumps_prm_from_map,
        lumps_prm_from_ordered_values, lumps_prm_from_values_payload, lumps_prm_schema,
        lumps_prm_schema_info, lumps_prm_schema_version, lumps_prm_schema_version_runtime,
        lumps_prm_to_map, lumps_prm_to_ordered_values, lumps_prm_to_values_payload,
        nhood_state_default_from_fortran, nhood_state_field_index, nhood_state_field_names,
        nhood_state_from_map, nhood_state_from_ordered_values, nhood_state_from_values_payload,
        nhood_state_schema, nhood_state_schema_info, nhood_state_schema_version,
        nhood_state_schema_version_runtime, nhood_state_to_map, nhood_state_to_ordered_values,
        nhood_state_to_values_payload, ohm_coef_lc_default_from_fortran, ohm_coef_lc_field_index,
        ohm_coef_lc_field_names, ohm_coef_lc_from_map, ohm_coef_lc_from_ordered_values,
        ohm_coef_lc_from_values_payload, ohm_coef_lc_schema, ohm_coef_lc_schema_info,
        ohm_coef_lc_schema_version, ohm_coef_lc_schema_version_runtime, ohm_coef_lc_to_map,
        ohm_coef_lc_to_ordered_values, ohm_coef_lc_to_values_payload,
        ohm_state_default_from_fortran, ohm_state_field_index, ohm_state_field_names,
        ohm_state_from_map, ohm_state_from_ordered_values, ohm_state_from_values_payload,
        ohm_state_schema, ohm_state_schema_info, ohm_state_schema_version,
        ohm_state_schema_version_runtime, ohm_state_step, ohm_state_to_map,
        ohm_state_to_ordered_values, ohm_state_to_values_payload, ohm_step, ohm_surface_names,
        phenology_state_default_from_fortran, phenology_state_field_index,
        phenology_state_field_names, phenology_state_from_map, phenology_state_from_ordered_values,
        phenology_state_from_values_payload, phenology_state_schema, phenology_state_schema_info,
        phenology_state_schema_version, phenology_state_schema_version_runtime,
        phenology_state_to_map, phenology_state_to_ordered_values,
        phenology_state_to_values_payload, roughness_state_default_from_fortran,
        roughness_state_field_index, roughness_state_field_names, roughness_state_from_map,
        roughness_state_from_ordered_values, roughness_state_from_values_payload,
        roughness_state_schema, roughness_state_schema_info, roughness_state_schema_version,
        roughness_state_schema_version_runtime, roughness_state_to_map,
        roughness_state_to_ordered_values, roughness_state_to_values_payload,
        snow_state_default_from_fortran, snow_state_field_index, snow_state_field_names,
        snow_state_from_map, snow_state_from_ordered_values, snow_state_from_values_payload,
        snow_state_schema, snow_state_schema_info, snow_state_schema_version,
        snow_state_schema_version_runtime, snow_state_to_map, snow_state_to_ordered_values,
        snow_state_to_values_payload, soil_prm_default_from_fortran, soil_prm_field_index,
        soil_prm_field_names, soil_prm_from_map, soil_prm_from_ordered_values,
        soil_prm_from_values_payload, soil_prm_schema, soil_prm_schema_info,
        soil_prm_schema_version, soil_prm_schema_version_runtime, soil_prm_to_map,
        soil_prm_to_ordered_values, soil_prm_to_values_payload, solar_state_default_from_fortran,
        solar_state_field_index, solar_state_field_names, solar_state_from_map,
        solar_state_from_ordered_values, solar_state_from_values_payload, solar_state_schema,
        solar_state_schema_info, solar_state_schema_version, solar_state_schema_version_runtime,
        solar_state_to_map, solar_state_to_ordered_values, solar_state_to_values_payload,
        surf_store_prm_default_from_fortran, surf_store_prm_field_index,
        surf_store_prm_field_names, surf_store_prm_from_map, surf_store_prm_from_ordered_values,
        surf_store_prm_from_values_payload, surf_store_prm_schema, surf_store_prm_schema_info,
        surf_store_prm_schema_version, surf_store_prm_schema_version_runtime,
        surf_store_prm_to_map, surf_store_prm_to_ordered_values, surf_store_prm_to_values_payload,
        water_dist_prm_default_from_fortran, water_dist_prm_field_index,
        water_dist_prm_field_names, water_dist_prm_from_map, water_dist_prm_from_ordered_values,
        water_dist_prm_from_values_payload, water_dist_prm_schema, water_dist_prm_schema_info,
        water_dist_prm_schema_version, water_dist_prm_schema_version_runtime,
        water_dist_prm_to_map, water_dist_prm_to_ordered_values, water_dist_prm_to_values_payload,
        AnthroEmisState, AnthroEmisStateValuesPayload, AtmState, AtmStateValuesPayload, BioCo2Prm,
        BioCo2PrmValuesPayload, BridgeError, ConductancePrm, ConductancePrmValuesPayload,
        FlagState, FlagStateValuesPayload, LumpsPrm, LumpsPrmValuesPayload, NhoodState,
        NhoodStateValuesPayload, OhmCoefLc, OhmCoefLcValuesPayload, OhmModel, OhmState,
        OhmStateValuesPayload, PhenologyState, PhenologyStateValuesPayload, RoughnessState,
        RoughnessStateValuesPayload, SnowState, SnowStateValuesPayload, SoilPrm,
        SoilPrmValuesPayload, SolarState, SolarStateValuesPayload, SurfStorePrm,
        SurfStorePrmValuesPayload, WaterDistPrm, WaterDistPrmValuesPayload, NSURF,
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
        m.add_class::<PyFlagState>()?;
        m.add_class::<PyAnthroEmisState>()?;
        m.add_class::<PyAtmState>()?;
        m.add_class::<PyPhenologyState>()?;
        m.add_class::<PySnowState>()?;
        m.add_class::<PySoilPrm>()?;
        m.add_class::<PyLumpsPrm>()?;
        m.add_class::<PyBioCo2Prm>()?;
        m.add_class::<PyConductancePrm>()?;
        m.add_class::<PySurfStorePrm>()?;
        m.add_class::<PyWaterDistPrm>()?;
        m.add_class::<PyOhmCoefLc>()?;
        m.add_class::<PySolarState>()?;
        m.add_class::<PyRoughnessState>()?;
        m.add_class::<PyNhoodState>()?;
        m.add_function(wrap_pyfunction!(ohm_step_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_state_fields_py, m)?)?;
        m.add_function(wrap_pyfunction!(ohm_surface_names_py, m)?)?;
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
        m.add_function(wrap_pyfunction!(soil_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(soil_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(soil_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(soil_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(soil_prm_fields_py, m)?)?;
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
        m.add_function(wrap_pyfunction!(bioco2_prm_schema_py, m)?)?;
        m.add_function(wrap_pyfunction!(bioco2_prm_schema_version_py, m)?)?;
        m.add_function(wrap_pyfunction!(bioco2_prm_schema_version_runtime_py, m)?)?;
        m.add_function(wrap_pyfunction!(bioco2_prm_schema_meta_py, m)?)?;
        m.add_function(wrap_pyfunction!(bioco2_prm_fields_py, m)?)?;
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
