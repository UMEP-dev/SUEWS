//! Raw declarations of the Fortran C ABI, not a safe invocation interface.
//!
//! Callers own pointer lifetime, allocation reach, layout and non-aliasing, and
//! must check sizes before converting them to ABI integers. The declarations
//! cannot enforce those conditions. Parameter/state facades use flat codecs;
//! the simulation driver additionally takes forcing and output matrices with
//! their own layout and leading-dimension contracts (`sim::run_simulation_into`).
//! Always inspect returned status and kernel diagnostics before decoding results.
//!
//! Every `extern "C"` symbol below is defined in `src/suews_bridge/c_api/*.f95`,
//! compiled into the static archive `libsuews_bridge.a` by `build.rs`, and
//! reports transport status through its trailing `err: *mut c_int` out-parameter
//! (one of the `SUEWS_CAPI_*` constants below, mirrored in `c_api/common.f95`).
//!
//! Per-type facades follow a uniform triple:
//! - `suews_<type>_len`: flat `f64` buffer length (plus allocatable dimensions
//!   where applicable).
//! - `suews_<type>_schema_version`: compiled facade schema version checked
//!   against the Rust codec constant at runtime.
//! - `suews_<type>_default`: packs a freshly initialised Fortran shadow struct
//!   into a caller-supplied flat buffer (used for schema/codec inspection and
//!   legacy fallback construction; never an authority for missing user inputs).

use std::os::raw::{c_char, c_double, c_int};

/// Status code returned when a Fortran C-API call completes without transport error.
pub const SUEWS_CAPI_OK: c_int = 0;
/// Status code returned when a timestep `dt` is non-positive (`c_api/ohm.f95`).
pub const SUEWS_CAPI_BAD_DT: c_int = 1;
/// Status code returned when elapsed time `dt_since_start` is negative (`c_api/ohm.f95`).
pub const SUEWS_CAPI_BAD_TIME: c_int = 2;
/// Status code returned when a caller-supplied flat buffer, TOC or output slice
/// is too small or inconsistent with the expected layout.
pub const SUEWS_CAPI_BAD_BUFFER: c_int = 3;
/// Status code returned when a packed state or parameter payload fails facade
/// validation (invalid dimensions, member count or domain constraints).
pub const SUEWS_CAPI_BAD_STATE: c_int = 4;

#[link(name = "suews_bridge", kind = "static")]
unsafe extern "C" {
    // -----------------------------------------------------------------------
    // Batch multi-timestep simulation driver (`c_api/driver.f95`, physics)
    // -----------------------------------------------------------------------

    /// Execute `SUEWS_cal_multitsteps` over `len_sim` forcing timesteps.
    ///
    /// Unpacks `timer_flat`, `config_flat`, `site_scalars_flat`, the 18-member
    /// `site_flat` + `site_toc` buffer and the 13-member `state_flat` +
    /// `state_toc` buffer into kernel derived types, writes column-major data
    /// and datetime matrices using leading dimensions `output_ld` and
    /// `datetime_ld`, copies kernel error/warning diagnostics, and packs the
    /// updated post-run timer and state back into `timer_out` / `state_out_flat`.
    #[cfg(feature = "physics")]
    pub fn suews_cal_multitsteps_c(
        timer_flat: *const c_double,
        timer_len: c_int,
        config_flat: *const c_double,
        config_len: c_int,
        site_scalars_flat: *const c_double,
        site_scalars_len: c_int,
        site_flat: *const c_double,
        site_flat_len: c_int,
        site_toc: *const c_int,
        site_toc_len: c_int,
        site_member_count: c_int,
        state_flat: *const c_double,
        state_flat_len: c_int,
        state_toc: *const c_int,
        state_toc_len: c_int,
        state_member_count: c_int,
        forcing_flat: *const c_double,
        len_sim: c_int,
        forcing_cols: c_int,
        nlayer: c_int,
        ndepth: c_int,
        timer_out: *mut c_double,
        timer_out_len: c_int,
        state_out_flat: *mut c_double,
        state_out_len: c_int,
        state_out_toc: *const c_int,
        state_out_toc_len: c_int,
        output_flat: *mut c_double,
        output_len: i64,
        output_ld: c_int,
        datetime_flat: *mut c_double,
        datetime_len: i64,
        datetime_ld: c_int,
        sim_err_code: *mut c_int,
        sim_err_message: *mut c_char,
        sim_err_message_len: c_int,
        warn_total: *mut c_int,
        warn_stored: *mut c_int,
        warn_max: c_int,
        warn_timer: *mut c_int,
        warn_text: *mut c_char,
        warn_text_len: c_int,
        err: *mut c_int,
    );

    // -----------------------------------------------------------------------
    // Legacy scalar OHM API and `OHM_STATE` facade (`c_api/ohm.f95`)
    // Note: the scalar helpers duplicate `module_phys_ohm` formulas; this is
    // a recorded BR-01 deviation (see `dev-ref/RUST_BRIDGE_AUDIT.md`).
    // -----------------------------------------------------------------------

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

    // -----------------------------------------------------------------------
    // Core control, forcing, timer and subsystem state facades
    // -----------------------------------------------------------------------

    pub fn suews_config_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_config_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_config_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_forcing_len(n_flat: *mut c_int, ts5mindata_ir_len: *mut c_int, err: *mut c_int);

    pub fn suews_forcing_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_forcing_default(
        flat: *mut c_double,
        n_flat: c_int,
        ts5mindata_ir_len: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_hydro_state_len(
        n_flat: *mut c_int,
        soilstore_roof_len: *mut c_int,
        state_roof_len: *mut c_int,
        soilstore_wall_len: *mut c_int,
        state_wall_len: *mut c_int,
        ev_roof_len: *mut c_int,
        ev_wall_len: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_hydro_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_hydro_state_default(
        flat: *mut c_double,
        n_flat: c_int,
        soilstore_roof_len: *mut c_int,
        state_roof_len: *mut c_int,
        soilstore_wall_len: *mut c_int,
        state_wall_len: *mut c_int,
        ev_roof_len: *mut c_int,
        ev_wall_len: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_heat_state_len(
        n_flat: *mut c_int,
        nlayer: *mut c_int,
        ndepth: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_heat_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_heat_state_default(
        flat: *mut c_double,
        n_flat: c_int,
        nlayer: *mut c_int,
        ndepth: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_timer_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_timer_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_timer_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

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

    // -----------------------------------------------------------------------
    // Subsystem and land-cover parameter facades (`*_prm` and nested types)
    // -----------------------------------------------------------------------

    pub fn suews_anthro_heat_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_anthro_heat_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_anthro_heat_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_anthro_emis_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_anthro_emis_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_anthro_emis_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_ehc_prm_len(
        n_flat: *mut c_int,
        nlayer: *mut c_int,
        ndepth: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_ehc_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_ehc_prm_default(
        flat: *mut c_double,
        n_flat: c_int,
        nlayer: *mut c_int,
        ndepth: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_spartacus_prm_len(
        n_flat: *mut c_int,
        height_len: *mut c_int,
        nlayer: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_spartacus_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_spartacus_prm_default(
        flat: *mut c_double,
        n_flat: c_int,
        height_len: *mut c_int,
        nlayer: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_spartacus_layer_prm_len(
        n_flat: *mut c_int,
        nlayer: *mut c_int,
        nspec: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_spartacus_layer_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_spartacus_layer_prm_compile_nspec(compile_nspec: *mut c_int, err: *mut c_int);

    pub fn suews_spartacus_layer_prm_default(
        flat: *mut c_double,
        n_flat: c_int,
        nlayer: *mut c_int,
        nspec: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_atm_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_atm_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_atm_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_building_archetype_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_building_archetype_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_building_archetype_prm_default(
        flat: *mut c_double,
        n_flat: c_int,
        err: *mut c_int,
    );

    pub fn suews_phenology_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_phenology_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_phenology_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_snow_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_snow_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_snow_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_snow_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_snow_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_snow_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_soil_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_soil_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_soil_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_lumps_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_lumps_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_lumps_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_ohm_coef_lc_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_ohm_coef_lc_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_ohm_coef_lc_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_ohm_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_ohm_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_ohm_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_conductance_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_conductance_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_conductance_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_bioco2_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_bioco2_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_bioco2_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_lai_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_lai_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_lai_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_lc_paved_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_lc_paved_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_lc_paved_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_lc_bldg_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_lc_bldg_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_lc_bldg_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_lc_bsoil_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_lc_bsoil_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_lc_bsoil_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_lc_dectr_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_lc_dectr_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_lc_dectr_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_lc_evetr_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_lc_evetr_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_lc_evetr_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_lc_grass_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_lc_grass_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_lc_grass_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_lc_water_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_lc_water_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_lc_water_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_stebbs_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_stebbs_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_stebbs_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_stebbs_state_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_stebbs_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_stebbs_state_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    #[cfg(test)]
    pub fn suews_stebbs_state_roundtrip(
        in_flat: *const c_double,
        n_in: c_int,
        in_bldg_nlayer: c_int,
        out_flat: *mut c_double,
        n_out: c_int,
        out_bldg_nlayer: c_int,
        err: *mut c_int,
    );

    // -----------------------------------------------------------------------
    // Output, diagnostic and water/irrigation facades
    // -----------------------------------------------------------------------

    pub fn suews_output_line_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_output_group_ncolumns(
        ncols_arr: *mut c_int,
        n_groups: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_output_line_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_output_line_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_error_entry_len(
        timer_flat_len: *mut c_int,
        message_len: *mut c_int,
        location_len: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_error_entry_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_error_entry_default(
        timer_flat: *mut c_double,
        n_timer_flat: c_int,
        message: *mut c_char,
        message_len: c_int,
        location: *mut c_char,
        location_len: c_int,
        is_fatal: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_error_state_len(message_len: *mut c_int, err: *mut c_int);

    pub fn suews_error_state_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_error_state_default(
        flag: *mut c_int,
        code: *mut c_int,
        message: *mut c_char,
        message_len: c_int,
        has_fatal: *mut c_int,
        count: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_surf_store_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_surf_store_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_surf_store_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_water_dist_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_water_dist_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_water_dist_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_irrig_daywater_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_irrig_daywater_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_irrig_daywater_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_irrigation_prm_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_irrigation_prm_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_irrigation_prm_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    // -----------------------------------------------------------------------
    // Solar geometry helper (`c_api/sunposition.f95`, physics)
    // -----------------------------------------------------------------------

    /// Compute solar azimuth and zenith angles (degrees) via
    /// `module_phys_narp::NARP_cal_SunPosition`.
    #[cfg(feature = "physics")]
    pub fn suews_sunposition_calc(
        year: c_double,
        idectime: c_double,
        utc: c_double,
        lat: c_double,
        lon: c_double,
        alt: c_double,
        azimuth: *mut c_double,
        zenith: *mut c_double,
        err: *mut c_int,
    );
}
