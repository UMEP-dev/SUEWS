// contract: C-CAPI, C-NATIVE-ERROR
//! Integration tests for exact-when-present C-API buffer length enforcement
//! across variable-length site and state receivers (`driver.f95`, gh#2131).
#![cfg(feature = "physics")]

use suews_bridge::{
    heat_state_expected_flat_len, load_run_config_from_str, run_simulation_into,
    spartacus_layer_prm_compile_nspec, stebbs_state_expected_flat_len, BridgeError, EhcPrm,
    HeatState, HydroState, OutputBuffers, OutputMatrix, SimulationInput, SpartacusLayerPrm,
    StebbsBldg, FORCING_BLOCK_STRIDE, HYDRO_STATE_BASE_FLAT_LEN, OUTPUT_DATA_COLS,
    OUTPUT_DATETIME_COLS,
};

const SAMPLE_YAML: &str = include_str!("../../../test/fixtures/bridge/sample_resolved.yml");

fn prepare_sample_input() -> SimulationInput {
    let mut run_cfg =
        load_run_config_from_str(SAMPLE_YAML).expect("sample_resolved.yml should parse cleanly");
    let mut row = vec![-999.0; FORCING_BLOCK_STRIDE];
    row[..4].copy_from_slice(&[2012.0, 1.0, 12.0, 0.0]);
    row[9..15].copy_from_slice(&[2.0, 60.0, 20.0, 1013.0, 0.0, 500.0]);
    row[16] = 350.0;

    let tstep = run_cfg.timer.tstep;
    run_cfg.timer.iy = 2012;
    run_cfg.timer.id = 1;
    run_cfg.timer.it = 12;
    run_cfg.timer.imin = 0;
    run_cfg.timer.isec = 0;
    run_cfg.timer.tstep_prev = tstep;
    run_cfg.timer.tstep_real = f64::from(tstep);
    run_cfg.timer.nsh_real = 3600.0 / f64::from(tstep);
    run_cfg.timer.nsh = (3600 / tstep).max(1);
    run_cfg.timer.dt_since_start = tstep;
    run_cfg.timer.dt_since_start_prev = 0;
    run_cfg.timer.dectime = 0.5;
    run_cfg.timer.dayofweek_id = [1, 1, 2];

    for v in &mut run_cfg.state.heat_state.temp_surf_dyohm {
        *v = row[11];
    }
    for v in &mut run_cfg.state.heat_state.tsfc_surf_dyohm {
        *v = row[11];
    }
    run_cfg.state.ohm_state.ws_rav = 2.0;

    SimulationInput {
        timer: run_cfg.timer,
        config: run_cfg.config,
        site: run_cfg.site,
        site_scalars: run_cfg.site_scalars,
        state: run_cfg.state,
        forcing_block: row,
        len_sim: 1,
        nlayer: run_cfg.nlayer,
        ndepth: run_cfg.ndepth,
    }
}

fn run_expect_ok(input: SimulationInput, context: &str) {
    let mut data = vec![0.0; OUTPUT_DATA_COLS];
    let mut datetime = vec![0.0; OUTPUT_DATETIME_COLS];
    run_simulation_into(
        input,
        OutputBuffers {
            data: OutputMatrix::from_slice(&mut data, 1, OUTPUT_DATA_COLS).unwrap(),
            datetime: OutputMatrix::from_slice(&mut datetime, 1, OUTPUT_DATETIME_COLS).unwrap(),
        },
    )
    .unwrap_or_else(|err| panic!("{context} should succeed, got {err:?}"));
}

fn run_expect_bad_buffer_detail(input: SimulationInput, context: &str) -> String {
    let mut data = vec![0.0; OUTPUT_DATA_COLS];
    let mut datetime = vec![0.0; OUTPUT_DATETIME_COLS];
    let err = run_simulation_into(
        input,
        OutputBuffers {
            data: OutputMatrix::from_slice(&mut data, 1, OUTPUT_DATA_COLS).unwrap(),
            datetime: OutputMatrix::from_slice(&mut datetime, 1, OUTPUT_DATETIME_COLS).unwrap(),
        },
    )
    .expect_err(context);

    match err {
        BridgeError::BadBufferDetail(msg) => msg,
        other => panic!("{context}: expected BridgeError::BadBufferDetail, got {other:?}"),
    }
}

#[test]
fn yaml_config_spartacus_layer_nspec_matches_fortran_compile_nspec() {
    let run_cfg =
        load_run_config_from_str(SAMPLE_YAML).expect("sample_resolved.yml should parse cleanly");
    let compile_nspec = spartacus_layer_prm_compile_nspec()
        .expect("Fortran compile-time nspec query should succeed");
    assert_eq!(
        run_cfg.site.spartacus_layer.nspec, compile_nspec,
        "yaml_config spartacus_layer.nspec must match Fortran module_ctrl_const_allocate::nspec"
    );
}

#[test]
fn spartacus_prm_short_v2_length_is_rejected_at_ffi_boundary() {
    let base = prepare_sample_input();
    let nlayer = base.nlayer as usize;
    let expected_len = base.site.spartacus.to_flat().len();
    assert!(nlayer >= 2, "sample fixture must have at least 2 layers");

    // SUEWS_CAPI_SPARTACUS_PRM_BASE_LEN is 17, so truncating `height` from
    // `nlayer + 1` to `nlayer` (short by 1) produces `17 + nlayer == 16 + (nlayer + 1)`
    // (the legacy v2 length accepted by Rust's `validate_site_codec`), and truncating
    // to `nlayer - 2` (short by 3) produces `17 + (nlayer - 2) == 14 + (nlayer + 1)`
    // (the legacy v1 length). Both reach Fortran `unpack_site_members` directly.
    for (height_len, short_by) in [(nlayer, 1_usize), (nlayer - 2, 3_usize)] {
        let mut input = prepare_sample_input();
        input.site.spartacus.height.truncate(height_len);
        let received_len = input.site.spartacus.to_flat().len();
        assert_eq!(received_len, expected_len - short_by);

        let msg = run_expect_bad_buffer_detail(
            input,
            &format!(
                "short non-zero spartacus_prm buffer (height_len={height_len}) must be rejected by unpack_site_members"
            ),
        );
        assert!(msg.contains("spartacus_prm"), "got: {msg}");
        assert!(
            msg.contains(&format!("received {received_len}"))
                && msg.contains(&format!("expected {expected_len}")),
            "expected received/expected lengths in diagnostic, got: {msg}"
        );
        assert!(
            msg.contains(&format!("nlayer={nlayer}")),
            "expected nlayer in diagnostic, got: {msg}"
        );
    }
}

#[test]
fn spartacus_layer_absent_short_and_oversized_contract_at_ffi_boundary() {
    let compile_nspec = spartacus_layer_prm_compile_nspec().unwrap();

    // Absent (member_len == 0): accepted on a non-SPARTACUS baseline run.
    let mut absent = prepare_sample_input();
    assert!(absent.config.net_radiation_method <= 1000);
    absent.site.spartacus_layer = SpartacusLayerPrm::default();
    assert!(absent.site.spartacus_layer.to_flat().is_empty());
    run_expect_ok(absent, "absent spartacus_layer on non-SPARTACUS run");

    // Short non-zero (nspec = 0 with nlayer > 0 -> 9 * nlayer).
    let mut short = prepare_sample_input();
    let nlayer = short.nlayer as usize;
    let expected_len = 9 * nlayer + 2 * compile_nspec * nlayer;
    short.site.spartacus_layer.nspec = 0;
    short.site.spartacus_layer.roof_albedo_dir_mult_fact.clear();
    short.site.spartacus_layer.wall_specular_frac.clear();
    let short_received = short.site.spartacus_layer.to_flat().len();
    assert_eq!(short_received, 9 * nlayer);
    let short_msg = run_expect_bad_buffer_detail(
        short,
        "short non-zero spartacus_layer buffer must be rejected",
    );
    assert!(short_msg.contains("spartacus_layer"), "got: {short_msg}");
    assert!(
        short_msg.contains(&format!("received {short_received}"))
            && short_msg.contains(&format!("expected {expected_len}")),
        "got: {short_msg}"
    );
    assert!(
        short_msg.contains(&format!("nlayer={nlayer}"))
            && short_msg.contains(&format!("nspec={compile_nspec}")),
        "got: {short_msg}"
    );

    // Oversized (nspec = compile_nspec + 1).
    let mut oversized = prepare_sample_input();
    let over_nspec = compile_nspec + 1;
    oversized.site.spartacus_layer.nspec = over_nspec;
    oversized
        .site
        .spartacus_layer
        .roof_albedo_dir_mult_fact
        .resize(over_nspec * nlayer, 1.0);
    oversized
        .site
        .spartacus_layer
        .wall_specular_frac
        .resize(over_nspec * nlayer, 0.0);
    let over_received = oversized.site.spartacus_layer.to_flat().len();
    let over_msg = run_expect_bad_buffer_detail(
        oversized,
        "oversized spartacus_layer buffer must be rejected",
    );
    assert!(over_msg.contains("spartacus_layer"), "got: {over_msg}");
    assert!(
        over_msg.contains(&format!("received {over_received}"))
            && over_msg.contains(&format!("expected {expected_len}")),
        "got: {over_msg}"
    );
}

#[test]
fn ehc_prm_absent_is_accepted_on_non_ehc_run() {
    let mut input = prepare_sample_input();
    assert_eq!(input.config.storage_heat_method, 1);
    assert!(input.config.net_radiation_method <= 1000);
    input.site.ehc = EhcPrm::default();
    assert!(input.site.ehc.to_flat().is_empty());
    run_expect_ok(input, "absent ehc_prm on non-EHC baseline run");
}

#[test]
fn hydro_state_short_and_oversized_are_rejected_with_detail() {
    let base = prepare_sample_input();
    let nlayer = base.nlayer as usize;
    let expected_len = HYDRO_STATE_BASE_FLAT_LEN + 6 * nlayer;

    for bad_nlayer in [0_usize, nlayer - 1, nlayer + 1] {
        let mut input = prepare_sample_input();
        let received_len = HYDRO_STATE_BASE_FLAT_LEN + 6 * bad_nlayer;
        input.state.hydro_state =
            HydroState::from_flat_with_lens(&vec![0.0; received_len], [bad_nlayer; 6]).unwrap();

        let msg = run_expect_bad_buffer_detail(
            input,
            &format!("hydro_state with nlayer={bad_nlayer} must be rejected"),
        );
        assert!(msg.contains("hydro_state"), "got: {msg}");
        assert!(
            msg.contains(&format!("received {received_len}"))
                && msg.contains(&format!("expected {expected_len}")),
            "got: {msg}"
        );
        assert!(msg.contains(&format!("nlayer={nlayer}")), "got: {msg}");
    }
}

#[test]
fn heat_state_short_and_oversized_are_rejected_with_detail() {
    let base = prepare_sample_input();
    let nlayer = base.nlayer as usize;
    let ndepth = base.ndepth as usize;
    let expected_len = heat_state_expected_flat_len(nlayer, ndepth).unwrap();

    for (bad_nlayer, bad_ndepth) in [
        (0_usize, 0_usize),
        (nlayer - 1, ndepth),
        (nlayer + 1, ndepth),
        (nlayer, ndepth + 1),
    ] {
        let mut input = prepare_sample_input();
        let received_len = heat_state_expected_flat_len(bad_nlayer, bad_ndepth).unwrap();
        input.state.heat_state =
            HeatState::from_flat_with_dims(&vec![0.0; received_len], bad_nlayer, bad_ndepth)
                .unwrap();

        let msg = run_expect_bad_buffer_detail(
            input,
            &format!("heat_state with ({bad_nlayer}, {bad_ndepth}) must be rejected"),
        );
        assert!(msg.contains("heat_state"), "got: {msg}");
        assert!(
            msg.contains(&format!("received {received_len}"))
                && msg.contains(&format!("expected {expected_len}")),
            "got: {msg}"
        );
        assert!(
            msg.contains(&format!("nlayer={nlayer}")) && msg.contains(&format!("ndepth={ndepth}")),
            "got: {msg}"
        );
    }
}

#[test]
fn stebbs_state_mismatched_bldg_nlayer_is_rejected_with_detail() {
    let base = prepare_sample_input();
    let nlayer = base.nlayer as usize;

    for bad_bldg_nlayer in [nlayer - 1, nlayer + 1] {
        let mut input = prepare_sample_input();
        input.state.stebbs_state.bldg_nlayer = bad_bldg_nlayer;
        input.state.stebbs_state.buildings = vec![StebbsBldg::with_nlayer(bad_bldg_nlayer)];
        let received_len = stebbs_state_expected_flat_len(1, bad_bldg_nlayer).unwrap();

        let msg = run_expect_bad_buffer_detail(
            input,
            &format!("stebbs_state with bldg_nlayer={bad_bldg_nlayer} must be rejected"),
        );
        assert!(msg.contains("stebbs_state"), "got: {msg}");
        assert!(
            msg.contains(&format!("received {received_len}")),
            "got: {msg}"
        );
        assert!(msg.contains(&format!("nlayer={nlayer}")), "got: {msg}");
    }
}

#[test]
fn suews_cal_multitsteps_rejects_overflowing_dims_before_allocation() {
    // nlayer=1, ndepth=159_072_863 makes the mathematical EHC length equal
    // 2^32 + 20 = 4_294_967_316 (wrapping to 20 in unchecked 32-bit c_int);
    // ndepth=268_435_456 also wraps heat_state's 16 * ndepth term. Both must
    // be rejected by validate_simulation_dims before initialise_site_state
    // allocates from the supplied dimensions.
    for overflowing_ndepth in [159_072_863_i32, 268_435_456_i32] {
        let mut input = prepare_sample_input();
        input.nlayer = 1;
        input.ndepth = overflowing_ndepth;
        input.site.spartacus.height.truncate(2);
        input.site.ehc = EhcPrm::default();
        input.site.spartacus_layer = SpartacusLayerPrm::default();

        let msg = run_expect_bad_buffer_detail(
            input,
            &format!("overflowing ndepth={overflowing_ndepth} must be rejected before allocation"),
        );
        assert!(
            msg.contains("overflows c_int"),
            "expected overflow diagnostic, got: {msg}"
        );
        assert!(
            msg.contains(&format!("ndepth={overflowing_ndepth}")),
            "expected ndepth in overflow diagnostic, got: {msg}"
        );
    }
}
