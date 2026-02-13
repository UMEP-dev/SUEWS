use std::os::raw::{c_double, c_int};

pub const SUEWS_CAPI_OK: c_int = 0;
pub const SUEWS_CAPI_BAD_DT: c_int = 1;
pub const SUEWS_CAPI_BAD_TIME: c_int = 2;
pub const SUEWS_CAPI_BAD_BUFFER: c_int = 3;
pub const SUEWS_CAPI_BAD_STATE: c_int = 4;

#[link(name = "suews_bridge", kind = "static")]
unsafe extern "C" {
    pub fn suews_ohm_qs_calc(
        qn1: c_double,
        dqndt: c_double,
        a1: c_double,
        a2: c_double,
        a3: c_double,
        qs: *mut c_double,
        err: *mut c_int,
    );

    pub fn suews_ohm_dqndt_step(
        dt: c_int,
        dt_since_start: c_int,
        qn1_av_prev: c_double,
        qn1: c_double,
        dqndt_prev: c_double,
        qn1_av_next: *mut c_double,
        dqndt_next: *mut c_double,
        err: *mut c_int,
    );

    pub fn suews_ohm_step(
        dt: c_int,
        dt_since_start: c_int,
        qn1_av_prev: c_double,
        dqndt_prev: c_double,
        qn1: c_double,
        a1: c_double,
        a2: c_double,
        a3: c_double,
        qn1_av_next: *mut c_double,
        dqndt_next: *mut c_double,
        qs: *mut c_double,
        err: *mut c_int,
    );

    pub fn suews_ohm_state_len(n_flat: *mut c_int, nsurf_out: *mut c_int, err: *mut c_int);

    pub fn suews_ohm_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_ohm_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_ohm_state_step(
        flat_in: *const c_double,
        n_flat_in: c_int,
        dt: c_int,
        dt_since_start: c_int,
        qn1: c_double,
        a1: c_double,
        a2: c_double,
        a3: c_double,
        flat_out: *mut c_double,
        n_flat_out: c_int,
        qs: *mut c_double,
        err: *mut c_int,
    );

    pub fn suews_config_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_config_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_config_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_flag_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_flag_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_flag_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_solar_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_solar_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_solar_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_roughness_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_roughness_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_roughness_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_nhood_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_nhood_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_nhood_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_anthroemis_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_anthroemis_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_anthroemis_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_atm_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_atm_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_atm_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_phenology_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_phenology_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_phenology_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_snow_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_snow_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_snow_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_soil_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_soil_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_soil_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_lumps_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_lumps_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_lumps_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_ohm_coef_lc_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_ohm_coef_lc_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_ohm_coef_lc_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_conductance_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_conductance_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_conductance_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_bioco2_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_bioco2_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_bioco2_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_lai_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_lai_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_lai_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_surf_store_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_surf_store_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_surf_store_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_water_dist_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_water_dist_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_water_dist_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_irrig_daywater_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_irrig_daywater_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_irrig_daywater_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);
}
