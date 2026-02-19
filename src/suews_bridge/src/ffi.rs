use std::os::raw::{c_char, c_double, c_int};

pub const SUEWS_CAPI_OK: c_int = 0;
pub const SUEWS_CAPI_BAD_DT: c_int = 1;
pub const SUEWS_CAPI_BAD_TIME: c_int = 2;
pub const SUEWS_CAPI_BAD_BUFFER: c_int = 3;
pub const SUEWS_CAPI_BAD_STATE: c_int = 4;

#[link(name = "suews_bridge", kind = "static")]
unsafe extern "C" {
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
        output_flat: *mut c_double,
        output_len: c_int,
        sim_err_code: *mut c_int,
        sim_err_message: *mut c_char,
        sim_err_message_len: c_int,
        err: *mut c_int,
    );

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

    pub fn suews_output_line_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_output_group_ncolumns(
        ncols_arr: *mut c_int,
        n_groups: *mut c_int,
        err: *mut c_int,
    );

    pub fn suews_output_line_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_output_line_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

    pub fn suews_output_block_len(n_flat: *mut c_int, err: *mut c_int);

    pub fn suews_output_block_schema_version(schema_version: *mut c_int, err: *mut c_int);

    pub fn suews_output_block_columns(cols: *mut c_int, n_cols: c_int, err: *mut c_int);

    pub fn suews_output_block_default(flat: *mut c_double, n_flat: c_int, err: *mut c_int);

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
}
