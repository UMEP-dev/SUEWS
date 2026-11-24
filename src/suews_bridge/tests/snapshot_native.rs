//! Native Fortran receiver proofs for `SUEWS_DEBUG` (all 15 stages via
//! `SUEWS_cal_Main`), `SUEWS_STATE_BLOCK`, and `output_block` (via the shared
//! production multiline core `run_multitsteps_block` used by
//! `SUEWS_cal_multitsteps`). Test-only under `capi-observer`; no production
//! snapshot wire is created or advertised.
// contract: C-CAPI, C-SEQUENCE

use suews_bridge::{
    encode_site_members_flat, encode_site_scalars_flat, encode_state_members_flat,
    load_run_config_from_str, output_block_field_names, output_block_schema_info, ConductancePrm,
    RunConfig,
};

extern "C" {
    fn snapshot_observe_stages(
        config: *const f64,
        timer: *const f64,
        scalars: *const f64,
        site: *const f64,
        site_len: i32,
        site_toc: *const i32,
        state: *const f64,
        state_len: i32,
        state_toc: *const i32,
        nlayer: i32,
        ndepth: i32,
        forcing_rows: *const f64,
        wu_rows: *const f64,
        stage_dims: *mut i32,
        pre_post: *mut f64,
        aux: *mut f64,
        snow_cleared_max: *mut f64,
        err: *mut i32,
    );

    fn snapshot_observe_lumps_first_iter(
        config: *const f64,
        timer: *const f64,
        scalars: *const f64,
        site: *const f64,
        site_len: i32,
        site_toc: *const i32,
        state: *const f64,
        state_len: i32,
        state_toc: *const i32,
        nlayer: i32,
        ndepth: i32,
        forcing_rows: *const f64,
        wu_rows: *const f64,
        lumps_pre_post: *mut f64,
        lumps_aux: *mut f64,
        err: *mut i32,
    );

    fn multiline_block_observe(
        config: *const f64,
        timer: *const f64,
        scalars: *const f64,
        site: *const f64,
        site_len: i32,
        site_toc: *const i32,
        state: *const f64,
        state_len: i32,
        state_toc: *const i32,
        nlayer: i32,
        ndepth: i32,
        forcing_rows: *const f64,
        block_widths: *mut i32,
        state_block_out: *mut f64,
        output_block_out: *mut f64,
        final_debug_out: *mut f64,
        mode_flags: *mut i32,
        wall_rect_len: i32,
        wall_temp_block_out: *mut f64,
        wall_temp_ref_out: *mut f64,
        wall_final_len: i32,
        wall_temp_dts_final_out: *mut f64,
        err: *mut i32,
    );
}

#[derive(Clone, Copy)]
struct MetRow {
    iy: f64,
    id: f64,
    it: f64,
    imin: f64,
    qn1_obs: f64,
    qs_obs: f64,
    qf_obs: f64,
    u: f64,
    rh: f64,
    temp_c: f64,
    pres: f64,
    rain: f64,
    kdown: f64,
    snow_fraction: f64,
    l_down: f64,
    f_cloud: f64,
    xsmd: f64,
    lai_evetr: f64,
    lai_dectr: f64,
    lai_grass: f64,
}

impl MetRow {
    fn to_col24(self) -> [f64; 24] {
        let mut col = [0.0_f64; 24];
        col[0] = self.iy;
        col[1] = self.id;
        col[2] = self.it;
        col[3] = self.imin;
        col[4] = self.qn1_obs;
        col[7] = self.qs_obs;
        col[8] = self.qf_obs;
        col[9] = self.u;
        col[10] = self.rh;
        col[11] = self.temp_c;
        col[12] = self.pres;
        col[13] = self.rain;
        col[14] = self.kdown;
        col[15] = self.snow_fraction;
        col[16] = self.l_down;
        col[17] = self.f_cloud;
        col[19] = self.xsmd;
        col[20] = self.lai_evetr;
        col[21] = self.lai_dectr;
        col[22] = self.lai_grass;
        col
    }
}

struct StageObservation {
    stage_dims: [[[i32; 2]; 15]; 3],
    pre_post: [[[f64; 2]; 15]; 3],
    aux: [[f64; 40]; 3],
    snow_cleared_max: [f64; 3],
}

struct LumpsFirstIterObservation {
    pre_post: [[[f64; 2]; 7]; 3],
    aux: [[f64; 14]; 3],
}

struct MultilineObservation {
    nlayer: usize,
    ndepth: usize,
    block_widths: [i32; 11],
    state_block: [[f64; 16]; 3],
    output_block: [[f64; 20]; 3],
    final_debug: [f64; 15],
    mode_flags: [i32; 5],
    wall_temp_block: Vec<f64>,
    wall_temp_ref: Vec<f64>,
    wall_temp_dts_final: Vec<f64>,
}

impl MultilineObservation {
    fn wall_block_at(&self, row: usize, layer: usize, depth: usize) -> f64 {
        self.wall_temp_block[(row * self.nlayer + layer) * self.ndepth + depth]
    }

    fn wall_ref_at(&self, row: usize, layer: usize, depth: usize) -> f64 {
        self.wall_temp_ref[(row * self.nlayer + layer) * self.ndepth + depth]
    }

    fn wall_dts_final_at(&self, layer: usize, depth: usize) -> f64 {
        self.wall_temp_dts_final[layer * self.ndepth + depth]
    }
}

fn flatten_forcing_rows(rows: &[MetRow; 3]) -> [f64; 72] {
    let mut flat = [0.0_f64; 72];
    for (r, row) in rows.iter().enumerate() {
        let col = row.to_col24();
        flat[r * 24..(r + 1) * 24].copy_from_slice(&col);
    }
    flat
}

fn flatten_wu_rows(rows: &[[f64; 7]; 3]) -> [f64; 21] {
    let mut flat = [0.0_f64; 21];
    for (r, wu) in rows.iter().enumerate() {
        flat[r * 7..(r + 1) * 7].copy_from_slice(wu);
    }
    flat
}

fn run_stage_observer(
    run: &RunConfig,
    rows: &[MetRow; 3],
    wu_rows: &[[f64; 7]; 3],
) -> StageObservation {
    let config = run.config.to_flat();
    let timer = run.timer.to_flat();
    let scalars = encode_site_scalars_flat(&run.site_scalars);
    let (site, site_toc) = encode_site_members_flat(&run.site).unwrap();
    let (state, state_toc) = encode_state_members_flat(&run.state).unwrap();
    assert_eq!((config.len(), timer.len(), scalars.len()), (29, 18, 24));
    assert_eq!((site_toc.len(), state_toc.len()), (36, 26));

    let forcing_flat = flatten_forcing_rows(rows);
    let wu_flat = flatten_wu_rows(wu_rows);
    let mut stage_dims_flat = [-1_i32; 90];
    let mut pre_post_flat = [f64::NAN; 90];
    let mut aux_flat = [f64::NAN; 120];
    let mut snow_cleared_max = [f64::NAN; 3];
    let mut err = -1_i32;

    // Fixed buffers match the test-only bind(c) contract. Inputs are live,
    // nonoverlapping immutable buffers; outputs and status are exclusive.
    unsafe {
        snapshot_observe_stages(
            config.as_ptr(),
            timer.as_ptr(),
            scalars.as_ptr(),
            site.as_ptr(),
            i32::try_from(site.len()).unwrap(),
            site_toc.as_ptr(),
            state.as_ptr(),
            i32::try_from(state.len()).unwrap(),
            state_toc.as_ptr(),
            run.nlayer,
            run.ndepth,
            forcing_flat.as_ptr(),
            wu_flat.as_ptr(),
            stage_dims_flat.as_mut_ptr(),
            pre_post_flat.as_mut_ptr(),
            aux_flat.as_mut_ptr(),
            snow_cleared_max.as_mut_ptr(),
            &mut err,
        );
    }
    assert_eq!(err, 0, "snapshot_observe_stages failed with err={err}");

    let mut stage_dims = [[[-1_i32; 2]; 15]; 3];
    let mut pre_post = [[[f64::NAN; 2]; 15]; 3];
    let mut aux = [[f64::NAN; 40]; 3];
    for r in 0..3 {
        for s in 0..15 {
            let base = r * 30 + s * 2;
            stage_dims[r][s] = [stage_dims_flat[base], stage_dims_flat[base + 1]];
            pre_post[r][s] = [pre_post_flat[base], pre_post_flat[base + 1]];
        }
        aux[r].copy_from_slice(&aux_flat[r * 40..(r + 1) * 40]);
    }
    StageObservation {
        stage_dims,
        pre_post,
        aux,
        snow_cleared_max,
    }
}

fn run_lumps_first_iter_observer(
    run: &RunConfig,
    rows: &[MetRow; 3],
    wu_rows: &[[f64; 7]; 3],
) -> LumpsFirstIterObservation {
    let config = run.config.to_flat();
    let timer = run.timer.to_flat();
    let scalars = encode_site_scalars_flat(&run.site_scalars);
    let (site, site_toc) = encode_site_members_flat(&run.site).unwrap();
    let (state, state_toc) = encode_state_members_flat(&run.state).unwrap();
    assert_eq!((config.len(), timer.len(), scalars.len()), (29, 18, 24));
    assert_eq!((site_toc.len(), state_toc.len()), (36, 26));

    let forcing_flat = flatten_forcing_rows(rows);
    let wu_flat = flatten_wu_rows(wu_rows);
    let mut pre_post_flat = [f64::NAN; 42];
    let mut aux_flat = [f64::NAN; 42];
    let mut err = -1_i32;

    // Fixed buffers match the test-only bind(c) contract. Inputs are live,
    // nonoverlapping immutable buffers; outputs and status are exclusive.
    unsafe {
        snapshot_observe_lumps_first_iter(
            config.as_ptr(),
            timer.as_ptr(),
            scalars.as_ptr(),
            site.as_ptr(),
            i32::try_from(site.len()).unwrap(),
            site_toc.as_ptr(),
            state.as_ptr(),
            i32::try_from(state.len()).unwrap(),
            state_toc.as_ptr(),
            run.nlayer,
            run.ndepth,
            forcing_flat.as_ptr(),
            wu_flat.as_ptr(),
            pre_post_flat.as_mut_ptr(),
            aux_flat.as_mut_ptr(),
            &mut err,
        );
    }
    assert_eq!(
        err, 0,
        "snapshot_observe_lumps_first_iter failed with err={err}"
    );

    let mut pre_post = [[[f64::NAN; 2]; 7]; 3];
    let mut aux = [[f64::NAN; 14]; 3];
    for r in 0..3 {
        for (f, slot) in pre_post[r].iter_mut().enumerate() {
            let base = r * 14 + f * 2;
            *slot = [pre_post_flat[base], pre_post_flat[base + 1]];
        }
        aux[r].copy_from_slice(&aux_flat[r * 14..(r + 1) * 14]);
    }
    LumpsFirstIterObservation { pre_post, aux }
}

fn run_multiline_observer(run: &RunConfig, rows: &[MetRow; 3]) -> MultilineObservation {
    let config = run.config.to_flat();
    let timer = run.timer.to_flat();
    let scalars = encode_site_scalars_flat(&run.site_scalars);
    let (site, site_toc) = encode_site_members_flat(&run.site).unwrap();
    let (state, state_toc) = encode_state_members_flat(&run.state).unwrap();
    assert_eq!((config.len(), timer.len(), scalars.len()), (29, 18, 24));
    assert_eq!((site_toc.len(), state_toc.len()), (36, 26));

    let nlayer = usize::try_from(run.nlayer).unwrap();
    let ndepth = usize::try_from(run.ndepth).unwrap();
    let wall_final_len = nlayer.checked_mul(ndepth).unwrap();
    let wall_rect_len = wall_final_len.checked_mul(3).unwrap();

    let forcing_flat = flatten_forcing_rows(rows);
    let mut block_widths = [0_i32; 11];
    let mut state_block_flat = [f64::NAN; 48];
    let mut output_block_flat = [f64::NAN; 60];
    let mut final_debug = [f64::NAN; 15];
    let mut mode_flags = [0_i32; 5];
    let mut wall_temp_block = vec![f64::NAN; wall_rect_len];
    let mut wall_temp_ref = vec![f64::NAN; wall_rect_len];
    let mut wall_temp_dts_final = vec![f64::NAN; wall_final_len];
    let mut err = -1_i32;

    // Checked buffers match the test-only bind(c) contract. Inputs are live,
    // nonoverlapping immutable buffers; outputs and status are exclusive.
    unsafe {
        multiline_block_observe(
            config.as_ptr(),
            timer.as_ptr(),
            scalars.as_ptr(),
            site.as_ptr(),
            i32::try_from(site.len()).unwrap(),
            site_toc.as_ptr(),
            state.as_ptr(),
            i32::try_from(state.len()).unwrap(),
            state_toc.as_ptr(),
            run.nlayer,
            run.ndepth,
            forcing_flat.as_ptr(),
            block_widths.as_mut_ptr(),
            state_block_flat.as_mut_ptr(),
            output_block_flat.as_mut_ptr(),
            final_debug.as_mut_ptr(),
            mode_flags.as_mut_ptr(),
            i32::try_from(wall_rect_len).unwrap(),
            wall_temp_block.as_mut_ptr(),
            wall_temp_ref.as_mut_ptr(),
            i32::try_from(wall_final_len).unwrap(),
            wall_temp_dts_final.as_mut_ptr(),
            &mut err,
        );
    }
    assert_eq!(err, 0, "multiline_block_observe failed with err={err}");

    let mut state_block = [[f64::NAN; 16]; 3];
    let mut output_block = [[f64::NAN; 20]; 3];
    for r in 0..3 {
        state_block[r].copy_from_slice(&state_block_flat[r * 16..(r + 1) * 16]);
        output_block[r].copy_from_slice(&output_block_flat[r * 20..(r + 1) * 20]);
    }
    MultilineObservation {
        nlayer,
        ndepth,
        block_widths,
        state_block,
        output_block,
        final_debug,
        mode_flags,
        wall_temp_block,
        wall_temp_ref,
        wall_temp_dts_final,
    }
}

fn load_sample_fixture() -> RunConfig {
    load_run_config_from_str(include_str!(
        "../../../test/fixtures/bridge/sample_resolved.yml"
    ))
    .unwrap()
}

fn prepare_wet_partially_dry_state(run: &mut RunConfig) {
    // Follow the production bridge fresh-run convention (gh#1919): dt_since_start
    // equals tstep on the first timestep so daily accumulators reset on row 0
    // rather than resetting again on row 1.
    run.timer.dt_since_start = run.timer.tstep;
    let caps = [
        run.site.lc_paved.soil.soil_store_capacity,
        run.site.lc_bldg.soil.soil_store_capacity,
        run.site.lc_evetr.soil.soil_store_capacity,
        run.site.lc_dectr.soil.soil_store_capacity,
        run.site.lc_grass.soil.soil_store_capacity,
        run.site.lc_bsoil.soil.soil_store_capacity,
        run.site.lc_water.soil.soil_store_capacity,
    ];
    for (idx, cap) in caps.iter().enumerate().take(6) {
        run.state.hydro_state.soilstore_surf[idx] = 0.60 * cap;
    }
    // Start with wet impervious/vegetated surfaces above drainage threshold so
    // stage 08 (drainage), stage 10 (evaporation), and stage 02 (vsmd) move on
    // every timestep.
    run.state.hydro_state.state_surf = [1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 0.0];
    run.state.phenology_state.lai_id = [3.5, 4.0, 2.5];
    // Seed non-zero snow values while SnowUse == 0 to prove #2014 clearing.
    run.state.snow_state.snowpack = [2.5, 1.5, 1.0, 1.0, 1.0, 1.0, 0.0];
    run.state.snow_state.snowfrac = [0.5, 0.4, 0.3, 0.2, 0.2, 0.1, 0.0];
    run.state.snow_state.snowdens = [250.0; 7];
    run.state.snow_state.snowalb = 0.7;
}

fn sample_met_rows_ascending() -> [MetRow; 3] {
    [
        MetRow {
            iy: 2012.0,
            id: 180.0,
            it: 12.0,
            imin: 5.0,
            qn1_obs: 150.0,
            qs_obs: 15.0,
            qf_obs: 12.0,
            u: 2.2,
            rh: 62.0,
            temp_c: 20.0,
            pres: 1013.0,
            rain: 1.2,
            kdown: 260.0,
            snow_fraction: 0.0,
            l_down: 340.0,
            f_cloud: 0.25,
            xsmd: 0.0,
            lai_evetr: 3.5,
            lai_dectr: 4.0,
            lai_grass: 2.5,
        },
        MetRow {
            iy: 2012.0,
            id: 180.0,
            it: 12.0,
            imin: 10.0,
            qn1_obs: 235.0,
            qs_obs: 31.0,
            qf_obs: 27.0,
            u: 2.8,
            rh: 56.0,
            temp_c: 22.5,
            pres: 1012.5,
            rain: 2.6,
            kdown: 470.0,
            snow_fraction: 0.0,
            l_down: 355.0,
            f_cloud: 0.18,
            xsmd: 0.0,
            lai_evetr: 3.5,
            lai_dectr: 4.0,
            lai_grass: 2.5,
        },
        MetRow {
            iy: 2012.0,
            id: 180.0,
            it: 12.0,
            imin: 15.0,
            qn1_obs: 340.0,
            qs_obs: 52.0,
            qf_obs: 41.0,
            u: 3.4,
            rh: 50.0,
            temp_c: 25.0,
            pres: 1012.0,
            rain: 4.1,
            kdown: 680.0,
            snow_fraction: 0.0,
            l_down: 370.0,
            f_cloud: 0.10,
            xsmd: 0.0,
            lai_evetr: 3.5,
            lai_dectr: 4.0,
            lai_grass: 2.5,
        },
    ]
}

fn sample_wu_rows() -> [[f64; 7]; 3] {
    [
        [0.20, 0.0, 0.60, 0.80, 1.00, 0.30, 0.0],
        [0.45, 0.0, 1.25, 1.60, 2.00, 0.60, 0.0],
        [0.70, 0.0, 1.90, 2.40, 3.00, 0.90, 0.0],
    ]
}

fn assert_all_stage_dims(obs: &StageObservation, nlayer: i32, ndepth: i32) {
    for (r, row_dims) in obs.stage_dims.iter().enumerate() {
        for (s, &dims) in row_dims.iter().enumerate() {
            assert_eq!(
                dims,
                [nlayer, ndepth],
                "SUEWS_DEBUG stage {:02} shape mismatch on row {r}",
                s + 1
            );
        }
    }
}

fn assert_approx(actual: f64, expected: f64, tol: f64, label: &str) {
    assert!(
        actual.is_finite() && expected.is_finite(),
        "{label}: non-finite actual={actual}, expected={expected}"
    );
    assert!(
        (actual - expected).abs() <= tol,
        "{label}: actual={actual} expected={expected} diff={} tol={tol}",
        (actual - expected).abs()
    );
}

/// Independent analytical Jarvis (1976) / Jarvi et al. (2011) / Ward et al. (2016)
/// shortwave conductance factor computed directly from forcing and site inputs.
fn expected_g_kdown(kdown: f64, cond: &ConductancePrm) -> f64 {
    let qnm = cond.kmax / (cond.kmax + cond.g_k);
    let val = (kdown / (kdown + cond.g_k)) / qnm;
    if (cond.gs_model == 2 || cond.gs_model == 4) && kdown >= cond.kmax {
        1.0
    } else {
        val
    }
}

/// Independent analytical Grimmond & Oke (2002) LUMPS partitioning computed from
/// site vegetation parameters and stage-06 available energy / psychometric terms.
fn expected_lumps_qhqe(
    run: &RunConfig,
    qn: f64,
    qf: f64,
    qs: f64,
    qm: f64,
    s_hpa: f64,
    psyc_hpa: f64,
) -> (f64, f64, f64, f64) {
    let sfr = run.site_scalars.sfr_surf;
    let sfr_veg = [sfr[2], sfr[3], sfr[4]];
    let veg_fraction: f64 = sfr_veg.iter().sum();
    let lai_id = run.state.phenology_state.lai_id;
    let lai_max = [
        run.site.lc_evetr.lai.lai_max,
        run.site.lc_dectr.lai.lai_max,
        run.site.lc_grass.lai.lai_max,
    ];
    let veg_phen: f64 = sfr_veg.iter().zip(lai_id.iter()).map(|(f, l)| f * l).sum();
    let veg_max: f64 = sfr_veg.iter().zip(lai_max.iter()).map(|(f, m)| f * m).sum();
    let (veg_phen_lumps, temp_veg) = if veg_max <= 0.01 {
        (0.0, 0.0)
    } else {
        let ratio = veg_phen / veg_max;
        (ratio, veg_fraction * ratio)
    };
    let (alpha_qhqe, beta) = if temp_veg > 0.9 {
        (temp_veg * 0.8 + 0.2, (20.0 - 3.0) * temp_veg + 3.0)
    } else {
        let (alpha_sl, alpha_in) = match run.site.lumps.veg_type {
            1 => (0.686, 0.189),
            2 => (0.610, 0.222),
            _ => (0.6, 0.2),
        };
        (temp_veg * alpha_sl + alpha_in, 3.0)
    };
    let psyc_s = psyc_hpa / s_hpa;
    let avail = qn + qf - qs - qm;
    let qh_lumps = ((1.0 - alpha_qhqe) + psyc_s) / (1.0 + psyc_s) * avail - beta;
    let qe_lumps = (alpha_qhqe / (1.0 + psyc_s)) * avail + beta;
    (qh_lumps, qe_lumps, veg_phen_lumps, temp_veg)
}

fn assert_wall_rectangle_matches_noblock(obs: &MultilineObservation, label: &str) {
    assert_eq!(
        obs.wall_temp_block.len(),
        3 * obs.nlayer * obs.ndepth,
        "{label}: wall_temp_block rectangle length mismatch"
    );
    assert_eq!(
        obs.wall_temp_ref.len(),
        3 * obs.nlayer * obs.ndepth,
        "{label}: wall_temp_ref rectangle length mismatch"
    );
    assert_eq!(
        obs.wall_temp_dts_final.len(),
        obs.nlayer * obs.ndepth,
        "{label}: wall_temp_dts_final rectangle length mismatch"
    );
    for r in 0..3 {
        for layer in 0..obs.nlayer {
            for depth in 0..obs.ndepth {
                let actual = obs.wall_block_at(r, layer, depth);
                let expected = obs.wall_ref_at(r, layer, depth);
                assert_approx(
                    actual,
                    expected,
                    1e-12,
                    &format!(
                        "{label}: block temp_wall({},{}) row {r} vs no-block SUEWS_cal_Main",
                        layer + 1,
                        depth + 1
                    ),
                );
            }
        }
    }
    for layer in 0..obs.nlayer {
        for depth in 0..obs.ndepth {
            let actual = obs.wall_block_at(2, layer, depth);
            let expected = obs.wall_dts_final_at(layer, depth);
            assert_approx(
                actual,
                expected,
                1e-12,
                &format!(
                    "{label}: final-row block temp_wall({},{}) vs no-block SUEWS_cal_multitsteps_dts",
                    layer + 1,
                    depth + 1
                ),
            );
        }
    }
}

#[test]
fn all_15_debug_stages_discriminate_named_transitions_and_controls() {
    let mut run = load_sample_fixture();
    run.config.net_radiation_method = 0;
    run.config.storage_heat_method = 0;
    run.config.emissions_method = 0;
    run.config.water_use_method = 1;
    run.config.rsl_level = 0;
    run.config.snow_use = 0;
    // Exercise the iteration-reset branch on heatState so stages 04, 05, 06,
    // 10, 11, and 12 each visibly transition on the final iteration.
    run.state.heat_state.iter_safe = false;
    prepare_wet_partially_dry_state(&mut run);

    let rows = sample_met_rows_ascending();
    let wu_rows = sample_wu_rows();
    let obs = run_stage_observer(&run, &rows, &wu_rows);
    assert_all_stage_dims(&obs, run.nlayer, run.ndepth);

    let sfr = run.site_scalars.sfr_surf;
    let non_water: f64 = sfr[..6].iter().sum();
    let caps = [
        run.site.lc_paved.soil.soil_store_capacity,
        run.site.lc_bldg.soil.soil_store_capacity,
        run.site.lc_evetr.soil.soil_store_capacity,
        run.site.lc_dectr.soil.soil_store_capacity,
        run.site.lc_grass.soil.soil_store_capacity,
        run.site.lc_bsoil.soil.soil_store_capacity,
    ];
    let expected_soil_cap: f64 = (0..6).map(|i| caps[i] * sfr[i] / non_water).sum();
    let nsh_real = 3600.0 / f64::from(run.timer.tstep);
    let internal_wu = run.site.irrigation.internalwateruse_h / nsh_real;

    let mut cum_rain = 0.0_f64;
    let mut running_tmax = f64::NEG_INFINITY;

    for (r, &met) in rows.iter().enumerate() {
        let pp = obs.pre_post[r];
        let aux = obs.aux[r];
        cum_rain += met.rain;
        running_tmax = running_tmax.max(met.temp_c);

        // #2014 check: inactive snow fields must be cleared to exact 0.0.
        assert_eq!(
            obs.snow_cleared_max[r], 0.0,
            "#2014 inactive snow not cleared on row {r}"
        );

        // Stage 01 (dailystate): sunlit day length [h], cumulative rain [mm], Tmax [degC].
        let exp_len_pre = (r as f64) / nsh_real;
        let exp_len_post = ((r + 1) as f64) / nsh_real;
        assert_approx(pp[0][0], exp_len_pre, 1e-12, "stage 01 pre lenDay_id");
        assert_approx(pp[0][1], exp_len_post, 1e-12, "stage 01 post lenDay_id");
        assert!(pp[0][1] > pp[0][0], "stage 01 must advance lenDay_id");
        assert_approx(
            aux[0],
            cum_rain,
            1e-12,
            "stage 01 HDD_id(5) cumulative rain",
        );
        assert_approx(aux[1], running_tmax, 1e-12, "stage 01 Tmax_id");

        // Stage 02 (soilmoist): soil_moist_cap, smd = cap - soil_state, vsmd transition.
        assert_approx(aux[2], expected_soil_cap, 1e-12, "stage 02 soil_moist_cap");
        assert_approx(aux[4], aux[2] - aux[3], 1e-12, "stage 02 smd identity");
        assert!(
            pp[1][1] > 0.0,
            "stage 02 vsmd must be positive for dry veg soil"
        );
        assert_ne!(pp[1][0], pp[1][1], "stage 02 must update vsmd on row {r}");

        // Stage 03 (wateruse): exact observed external water use [mm].
        let wu_total: f64 = wu_rows[r].iter().zip(sfr.iter()).map(|(w, f)| w * f).sum();
        let exp_wu_ext = (wu_total - internal_wu).max(0.0);
        assert_approx(pp[2][1], exp_wu_ext, 1e-12, "stage 03 wu_ext");
        assert_ne!(pp[2][0], pp[2][1], "stage 03 must update wu_ext on row {r}");

        // Stage 04 (anthroemis): exact observed QF [W m-2].
        let prev_qf = if r == 0 { 0.0 } else { rows[r - 1].qf_obs };
        assert_approx(pp[3][0], prev_qf, 1e-12, "stage 04 pre qf");
        assert_approx(pp[3][1], met.qf_obs, 1e-12, "stage 04 post qf");

        // Stage 05 (qn): exact observed QN [W m-2] while QS remains previous step.
        let prev_qn = if r == 0 { 0.0 } else { rows[r - 1].qn1_obs };
        assert_approx(pp[4][0], prev_qn, 1e-12, "stage 05 pre qn");
        assert_approx(pp[4][1], met.qn1_obs, 1e-12, "stage 05 post qn");

        // Stage 06 (qs): exact observed QS [W m-2].
        let prev_qs = if r == 0 { 0.0 } else { rows[r - 1].qs_obs };
        assert_approx(pp[5][0], prev_qs, 1e-12, "stage 06 pre qs");
        assert_approx(pp[5][1], met.qs_obs, 1e-12, "stage 06 post qs");

        // Stage 07 (qhqe_lumps): with heatState%iter_safe == false, i_iter >= 2
        // resets heatState to step-start and LUMPS_cal_QHQE only computes QH_LUMPS
        // on i_iter == 1; Case B (iter_safe == true) verifies active LUMPS closure.
        assert_eq!(
            pp[6][0], pp[6][1],
            "stage 07 iter_safe=false mode equality on row {r}"
        );

        // Stage 08 (water): positive per-step drainage [mm], transitions on all rows.
        assert!(
            pp[7][1] > 0.0,
            "stage 08 drain_per_tstep must be > 0 on row {r}"
        );
        assert_ne!(
            pp[7][0], pp[7][1],
            "stage 08 must update drain_per_tstep on row {r}"
        );

        // Stage 09 (resist): independent analytical shortwave conductance g_kdown,
        // paired no-debug state & public Debug(120) output witnesses, plus RA_h/RS > 0.
        let exp_g_kdown = expected_g_kdown(met.kdown, &run.site.conductance);
        assert!(
            pp[8][1] > 0.0 && pp[8][1] <= 1.0,
            "stage 09 g_kdown out of (0,1] on row {r}: {}",
            pp[8][1]
        );
        assert_ne!(
            pp[8][0], pp[8][1],
            "stage 09 must update g_kdown on row {r}"
        );
        assert_approx(
            pp[8][1],
            exp_g_kdown,
            1e-12,
            "stage 09 g_kdown matches independent Jarvis/Ward formula",
        );
        assert_approx(
            pp[8][1],
            aux[32],
            1e-12,
            "stage 09 g_kdown matches paired no-debug control_state",
        );
        assert_approx(
            pp[8][1],
            aux[33],
            1e-12,
            "stage 09 g_kdown matches paired no-debug public output Debug(120)",
        );
        assert_approx(
            pp[8][1],
            aux[34],
            1e-12,
            "stage 09 g_kdown matches public output Debug(120)",
        );
        assert!(
            aux[10] > 0.0 && aux[11] > 0.0,
            "stage 09 RA_h/RS must be > 0"
        );

        // Stage 10 (qe): QE [W m-2] matches exact latent heat unit identity & paired control.
        assert!(pp[9][1] > 0.0, "stage 10 qe must be > 0 on wet row {r}");
        assert_ne!(pp[9][0], pp[9][1], "stage 10 must update qe on row {r}");
        assert_ne!(
            aux[12], aux[13],
            "stage 10 must update ev_per_tstep on row {r}"
        );
        assert_approx(
            pp[9][1],
            aux[14],
            1e-9,
            "stage 10 qe == dot(lv/tstep*ev_surf, sfr)",
        );
        assert_approx(
            pp[9][1],
            aux[27],
            1e-12,
            "stage 10 qe matches paired control",
        );

        // Stage 11 (qh): exact energy closure QH == QN + QF - QS - QE & paired control.
        let exp_qh = met.qn1_obs + met.qf_obs - met.qs_obs - pp[9][1];
        assert_approx(
            pp[10][1],
            exp_qh,
            1e-9,
            "stage 11 energy closure QH=QN+QF-QS-QE",
        );
        assert_ne!(pp[10][0], pp[10][1], "stage 11 must update qh on row {r}");
        assert_approx(
            pp[10][1],
            aux[26],
            1e-12,
            "stage 11 qh matches paired control",
        );

        // Stage 12 (tsurf): aerodynamic TSfc_C == QH / (rho * cp) * RA_h + temp_c.
        // With heatState%iter_safe == false, resetting heatState on every iteration
        // keeps flag_converge false; converged flag_converge == true is verified in
        // the paired production-default iter_safe == true pass below.
        let exp_tsfc = pp[10][1] / (aux[16] * aux[17]) * aux[18] + met.temp_c;
        assert_approx(
            pp[11][1],
            exp_tsfc,
            1e-9,
            "stage 12 aerodynamic TSfc_C equation",
        );
        assert_ne!(
            pp[11][0], pp[11][1],
            "stage 12 must update TSfc_C on row {r}"
        );
        assert_eq!(
            aux[19], 0.0,
            "stage 11 pre-tsurf flag_converge must be false"
        );
        assert_eq!(
            aux[20], 0.0,
            "stage 12 iter_safe=false stays unconverged on row {r}"
        );

        // Stage 13 (rsl): post-loop t2_C [degC] and U10_ms [m s-1], matches paired control.
        assert!(
            pp[12][1] > 10.0 && pp[12][1] < 45.0,
            "stage 13 plausible t2_C"
        );
        assert_ne!(pp[12][0], pp[12][1], "stage 13 must update t2_C on row {r}");
        assert!(aux[21] > 0.0, "stage 13 U10_ms must be > 0");
        assert_approx(
            pp[12][1],
            aux[28],
            1e-12,
            "stage 13 t2_C matches paired control",
        );

        // Stage 14 (biogenco2): EmissionsMethod == 0 unchanged control -> exact 0.0.
        assert_eq!(pp[13][0], 0.0, "stage 14 unchanged control pre Fc_biogen");
        assert_eq!(pp[13][1], 0.0, "stage 14 unchanged control post Fc_biogen");

        // Stage 15 (beers): post-loop localised kdown_2d > 0 & lup_2d > 0, matches paired & outputLine.
        assert!(
            pp[14][1] > 0.0,
            "stage 15 kdown_2d must be > 0 on sunlit row {r}"
        );
        assert_ne!(
            pp[14][0], pp[14][1],
            "stage 15 must update kdown_2d on row {r}"
        );
        assert!(
            aux[25] > 250.0,
            "stage 15 lup_2d plausible terrestrial LW [W m-2]"
        );
        assert_approx(
            pp[14][1],
            aux[29],
            1e-12,
            "stage 15 kdown_2d matches paired control",
        );
        assert_approx(
            pp[14][1],
            aux[31],
            1e-12,
            "stage 15 kdown_2d matches outputLine BEERS(11)",
        );
    }

    // Distinctness across timesteps for key stage outputs.
    for stage_idx in [0, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 14] {
        assert_ne!(
            obs.pre_post[0][stage_idx][1],
            obs.pre_post[2][stage_idx][1],
            "stage {:02} must distinguish row 0 vs row 2",
            stage_idx + 1
        );
    }

    // Reversed-forcing control: swapping forcing row order changes per-row outputs
    // while keeping timestamps ascending (isolates forcing-row order sensitivity).
    let mut rev_rows = [rows[2], rows[1], rows[0]];
    for (r, row) in rev_rows.iter_mut().enumerate() {
        row.imin = 5.0 * (r + 1) as f64;
    }
    let rev_wu = [wu_rows[2], wu_rows[1], wu_rows[0]];
    let rev_obs = run_stage_observer(&run, &rev_rows, &rev_wu);
    assert_approx(rev_obs.pre_post[0][4][1], 340.0, 1e-12, "reversed row 0 QN");
    assert_approx(rev_obs.pre_post[2][4][1], 150.0, 1e-12, "reversed row 2 QN");
    assert_approx(rev_obs.pre_post[0][5][1], 52.0, 1e-12, "reversed row 0 QS");
    assert_approx(rev_obs.pre_post[2][5][1], 15.0, 1e-12, "reversed row 2 QS");
    assert!(
        rev_obs.pre_post[0][14][1] > rev_obs.pre_post[2][14][1],
        "reversed sunlit forcing must invert stage 15 kdown_2d ordering"
    );
    for (r, &met) in rev_rows.iter().enumerate() {
        let exp_rev_g = expected_g_kdown(met.kdown, &run.site.conductance);
        assert_approx(
            rev_obs.pre_post[r][8][1],
            exp_rev_g,
            1e-12,
            "reversed stage 09 g_kdown analytical formula",
        );
        assert_approx(
            rev_obs.pre_post[r][8][1],
            rev_obs.aux[r][32],
            1e-12,
            "reversed stage 09 g_kdown no-debug control_state",
        );
        assert_approx(
            rev_obs.pre_post[r][8][1],
            rev_obs.aux[r][33],
            1e-12,
            "reversed stage 09 g_kdown no-debug public Debug(120)",
        );
    }

    // Stage 07 (qhqe_lumps) direct SUEWS_cal_Main receiver discriminator:
    // Because SUEWS_cal_Main_impl iterates `DO WHILE (i_iter < 3 ...)` and
    // overwrites SUEWS_DEBUG on every pass, published debugState reflects
    // i_iter >= 2, where LUMPS_cal_QHQE executes its ELSE branch `QH_Init = QH`.
    // Under heatState%iter_safe == false with distinct step-start qh != qh_init,
    // check_and_reset_states restores heatState before i_iter >= 2 so stage 07
    // genuinely transitions qh_init from step-start qh_init to step-start qh on
    // row 0 (discriminating `state_07_qhqe_lumps = state_06_qs`), while rows
    // 1..2 (after row 0's stage 11 sets qh_init = qh) form an unchanged control.
    for &(seed_qh, seed_qh_init) in &[(45.0_f64, 10.0_f64), (72.5_f64, -15.0_f64)] {
        let mut run_s07 = run.clone();
        run_s07.state.heat_state.qh = seed_qh;
        run_s07.state.heat_state.qh_init = seed_qh_init;
        let obs_s07 = run_stage_observer(&run_s07, &rows, &wu_rows);
        assert_approx(
            obs_s07.aux[0][35],
            seed_qh_init,
            1e-12,
            "stage 07 iter_safe=false row 0 pre-stage-07 qh_init",
        );
        assert_approx(
            obs_s07.aux[0][37],
            seed_qh,
            1e-12,
            "stage 07 iter_safe=false row 0 pre-stage-07 qh",
        );
        assert_approx(
            obs_s07.aux[0][36],
            seed_qh,
            1e-12,
            "stage 07 iter_safe=false row 0 post-stage-07 qh_init == qh",
        );
        assert_ne!(
            obs_s07.aux[0][35], obs_s07.aux[0][36],
            "stage 07 iter_safe=false row 0 must separate state_06_qs and state_07_qhqe_lumps"
        );
        for aux_row in &obs_s07.aux[1..3] {
            assert_approx(
                aux_row[35],
                aux_row[36],
                1e-12,
                "stage 07 iter_safe=false rows 1..2 post-sync unchanged control",
            );
            assert_approx(
                aux_row[36],
                aux_row[37],
                1e-12,
                "stage 07 iter_safe=false rows 1..2 qh_init == qh",
            );
        }
    }

    // Adjacent unchanged control: perturbing ONLY qf_obs changes stage 04 (QF) and
    // downstream energy stages (10/11/12) while leaving earlier stages 01, 02, 03
    // and observed QN/QS (05/06) bit-for-bit identical on row 0.
    let mut pert_rows = rows;
    pert_rows[0].qf_obs += 35.0;
    let pert_obs = run_stage_observer(&run, &pert_rows, &wu_rows);
    for unchanged_stage in [0, 1, 2, 4, 5] {
        assert_eq!(
            pert_obs.pre_post[0][unchanged_stage][1],
            obs.pre_post[0][unchanged_stage][1],
            "qf_obs perturbation must not alter stage {:02} on row 0",
            unchanged_stage + 1
        );
    }
    assert_approx(
        pert_obs.pre_post[0][3][1] - obs.pre_post[0][3][1],
        35.0,
        1e-12,
        "qf_obs perturbation delta at stage 04",
    );
    assert_ne!(
        pert_obs.pre_post[0][10][1], obs.pre_post[0][10][1],
        "qf_obs perturbation must propagate to stage 11 QH"
    );

    // Paired production-default control (heatState%iter_safe == true) on the same
    // observed-QN/QS/QF forcing: stage 07 retains i_iter == 1 LUMPS fluxes with
    // exact Grimmond & Oke (2002) closure QH_LUMPS + QE_LUMPS == QN + QF - QS
    // and exact match to outputLine SUEWS(16).
    let mut run_lumps = run.clone();
    run_lumps.state.heat_state.iter_safe = true;
    let obs_lumps = run_stage_observer(&run_lumps, &rows, &wu_rows);
    for (r, &met) in rows.iter().enumerate() {
        let pp = obs_lumps.pre_post[r];
        let aux = obs_lumps.aux[r];
        let qh_lumps = pp[6][1];
        let qe_lumps = aux[9];
        assert!(
            qh_lumps.abs() > 1.0 && qe_lumps.abs() > 1.0,
            "iter_safe=true stage 07 active LUMPS fluxes on row {r}"
        );
        assert_approx(
            qh_lumps + qe_lumps,
            met.qn1_obs + met.qf_obs - met.qs_obs,
            1e-9,
            "stage 07 exact LUMPS closure QH_LUMPS + QE_LUMPS == QN + QF - QS",
        );
        assert_approx(
            qh_lumps,
            aux[30],
            1e-9,
            "stage 07 QH_LUMPS matches outputLine SUEWS(16)",
        );
        assert_approx(
            qh_lumps,
            aux[38],
            1e-9,
            "stage 07 QH_LUMPS matches no-debug control outputLine SUEWS(16)",
        );
        assert_approx(
            qe_lumps,
            aux[39],
            1e-9,
            "stage 07 QE_LUMPS matches no-debug control outputLine SUEWS(17)",
        );
        let exp_g_lumps = expected_g_kdown(met.kdown, &run_lumps.site.conductance);
        assert_approx(
            pp[8][1],
            exp_g_lumps,
            1e-12,
            "iter_safe=true stage 09 g_kdown analytical formula",
        );
        assert_approx(
            pp[8][1],
            aux[32],
            1e-12,
            "iter_safe=true stage 09 g_kdown no-debug control_state",
        );
        assert_approx(
            pp[8][1],
            aux[33],
            1e-12,
            "iter_safe=true stage 09 g_kdown no-debug public Debug(120)",
        );
        // Under production-default iter_safe == true, stage 12 converges cleanly
        // and satisfies the aerodynamic TSfc_C identity.
        assert_eq!(
            aux[20], 1.0,
            "iter_safe=true stage 12 post-tsurf flag_converge must be true on row {r}"
        );
        let exp_tsfc_lumps = pp[10][1] / (aux[16] * aux[17]) * aux[18] + met.temp_c;
        assert_approx(
            pp[11][1],
            exp_tsfc_lumps,
            1e-9,
            "iter_safe=true stage 12 aerodynamic TSfc_C equation",
        );
    }
    assert_ne!(
        obs_lumps.pre_post[0][6][1], obs_lumps.pre_post[2][6][1],
        "stage 07 QH_LUMPS must vary across forcing rows"
    );

    // First-iteration (i_iter == 1) LUMPS physical transition and explicit
    // fundamental-limitation verification under production-default iter_safe == true:
    // On SUEWS_cal_Main's published final iteration (i_iter >= 2), state_06_qs
    // and state_07_qhqe_lumps are intrinsically identical under iter_safe == true.
    // The real physical LUMPS transition (QH_LUMPS, QE_LUMPS, QH_Init = QH_LUMPS,
    // s_hPa, psyc_hPa, veg_phen_lumps, temp_veg) happens on i_iter == 1.
    let lumps_i1 = run_lumps_first_iter_observer(&run_lumps, &rows, &wu_rows);
    for r in 0..3 {
        let lp = lumps_i1.pre_post[r];
        let la = lumps_i1.aux[r];
        // Fundamental limitation on published final iteration (iter_safe == true):
        assert!(
            la[10] >= 2.0,
            "published final iteration must have i_iter >= 2 on row {r}, got {}",
            la[10]
        );
        assert_eq!(
            la[4], la[5],
            "iter_safe=true final iteration state_06_qs and state_07_qhqe_lumps QH_LUMPS are intrinsically identical on row {r}"
        );
        assert_eq!(
            la[6], la[7],
            "iter_safe=true final iteration state_06_qs and state_07_qhqe_lumps QE_LUMPS are intrinsically identical on row {r}"
        );
        assert_eq!(
            la[8], la[9],
            "iter_safe=true final iteration state_06_qs and state_07_qhqe_lumps QH_Init are intrinsically identical on row {r}"
        );

        // Genuine separating physical transitions on i_iter == 1:
        for (idx, field) in [
            (0, "QH_LUMPS"),
            (1, "QE_LUMPS"),
            (2, "QH_Init"),
            (3, "s_hPa"),
            (4, "psyc_hPa"),
            (5, "veg_phen_lumps"),
            (6, "temp_veg"),
        ] {
            assert_ne!(
                lp[idx][0], lp[idx][1],
                "i_iter == 1 LUMPS_cal_QHQE must transition {field} on row {r}"
            );
        }
        assert!(lp[3][1] > 0.0, "i_iter == 1 s_hPa > 0 on row {r}");
        assert!(lp[4][1] > 0.0, "i_iter == 1 psyc_hPa > 0 on row {r}");
        assert_approx(
            lp[2][1],
            lp[0][1],
            1e-12,
            "i_iter == 1 LUMPS sets QH_Init == QH_LUMPS",
        );

        let (exp_qh_lumps, exp_qe_lumps, exp_veg_phen, exp_temp_veg) =
            expected_lumps_qhqe(&run_lumps, la[0], la[1], la[2], la[3], lp[3][1], lp[4][1]);
        // `suews_phys_lumps.f95` assigns default-REAL literals (0.686, 0.189) to
        // double-precision alpha_sl/alpha_in, introducing O(1e-6 W m-2) rounding
        // relative to exact f64 Grimmond & Oke (2002) arithmetic.
        assert_approx(
            lp[0][1],
            exp_qh_lumps,
            1e-5,
            "i_iter == 1 QH_LUMPS matches analytical Grimmond & Oke (2002)",
        );
        assert_approx(
            lp[1][1],
            exp_qe_lumps,
            1e-5,
            "i_iter == 1 QE_LUMPS matches analytical Grimmond & Oke (2002)",
        );
        assert_approx(
            lp[5][1],
            exp_veg_phen,
            1e-12,
            "i_iter == 1 veg_phen_lumps analytical identity",
        );
        assert_approx(
            lp[6][1],
            exp_temp_veg,
            1e-12,
            "i_iter == 1 temp_veg analytical identity",
        );
        // Cross-check i_iter == 1 LUMPS outputs against published state_07_qhqe_lumps
        // and no-debug public outputLine SUEWS(16..17).
        assert_approx(
            la[5],
            lp[0][1],
            1e-9,
            "state_07_qhqe_lumps QH_LUMPS matches i_iter == 1 LUMPS",
        );
        assert_approx(
            la[7],
            lp[1][1],
            1e-9,
            "state_07_qhqe_lumps QE_LUMPS matches i_iter == 1 LUMPS",
        );
        assert_approx(
            la[5],
            la[11],
            1e-9,
            "state_07_qhqe_lumps QH_LUMPS matches no-debug SUEWS(16)",
        );
        assert_approx(
            la[7],
            la[12],
            1e-9,
            "state_07_qhqe_lumps QE_LUMPS matches no-debug SUEWS(17)",
        );
    }
    assert_ne!(
        lumps_i1.pre_post[0][0][1], lumps_i1.pre_post[2][0][1],
        "i_iter == 1 QH_LUMPS must vary across rows"
    );
}

#[test]
fn activating_ehc_and_biogenic_co2_case_discriminates_lumps_ehc_and_stage14() {
    let mut run = load_sample_fixture();
    run.config.net_radiation_method = 3; // NARP modelled QN
    run.config.storage_heat_method = 5; // EHC modelled QS & roof temperature
    run.config.emissions_method = 11; // SAHP QF + rectangular-hyperbola biogenic CO2
    run.config.water_use_method = 1;
    run.config.snow_use = 0;
    // Production default: heatState%iter_safe == true retains LUMPS QH/QE from i_iter == 1.
    run.state.heat_state.iter_safe = true;
    prepare_wet_partially_dry_state(&mut run);

    let rows = sample_met_rows_ascending();
    let wu_rows = sample_wu_rows();
    let obs = run_stage_observer(&run, &rows, &wu_rows);
    assert_all_stage_dims(&obs, run.nlayer, run.ndepth);

    let sfr = run.site_scalars.sfr_surf;
    let veg_bio = [
        (
            sfr[2],
            run.site.lc_evetr.bioco2.alpha_bio_co2,
            run.site.lc_evetr.bioco2.beta_bio_co2,
            run.state.phenology_state.lai_id[0],
            run.site.lc_evetr.lai.lai_max,
        ),
        (
            sfr[3],
            run.site.lc_dectr.bioco2.alpha_bio_co2,
            run.site.lc_dectr.bioco2.beta_bio_co2,
            run.state.phenology_state.lai_id[1],
            run.site.lc_dectr.lai.lai_max,
        ),
        (
            sfr[4],
            run.site.lc_grass.bioco2.alpha_bio_co2,
            run.site.lc_grass.bioco2.beta_bio_co2,
            run.state.phenology_state.lai_id[2],
            run.site.lc_grass.lai.lai_max,
        ),
    ];

    for (r, &met) in rows.iter().enumerate() {
        let pp = obs.pre_post[r];
        let aux = obs.aux[r];

        // Stage 04 (anthroemis): modelled QF > 0 and Fc_anthro transitions cleanly 03 -> 04.
        assert!(pp[3][1] > 0.0, "Case B stage 04 modelled QF > 0 on row {r}");
        assert!(aux[5] > 0.0, "Case B stage 04 Fc_anthro > 0 on row {r}");
        assert_ne!(
            aux[6], aux[5],
            "Case B stage 04 must update Fc_anthro on row {r}"
        );

        // Stage 05 (qn): NARP modelled QN > 0, distinct from observed QN override.
        assert!(
            pp[4][1] > 50.0,
            "Case B stage 05 NARP QN > 50 W m-2 on row {r}"
        );

        // Stage 06 (qs): EHC modelled QS is finite non-zero and temp_roof(1,1) is updated.
        assert!(
            pp[5][1].is_finite() && pp[5][1] != 0.0,
            "Case B stage 06 EHC QS non-zero"
        );
        assert!(
            aux[7].is_finite(),
            "Case B stage 06 EHC temp_roof(1,1) finite"
        );

        // Stage 07 (qhqe_lumps): active Grimmond & Oke (2002) fluxes from i_iter == 1
        // and outputLine echo (note: under EHC, i_iter >= 2 updates QS while LUMPS
        // keeps its i_iter == 1 values; exact QH_LUMPS + QE_LUMPS == QN + QF - QS
        // closure is verified in Case A with fixed observed QN/QS).
        let qh_lumps = pp[6][1];
        let qe_lumps = aux[9];
        assert!(
            qh_lumps.abs() > 1.0 && qe_lumps.abs() > 1.0,
            "Case B stage 07 active LUMPS fluxes non-trivial on row {r}: qh={qh_lumps}, qe={qe_lumps}"
        );
        assert!(
            qh_lumps + qe_lumps > 0.0,
            "Case B stage 07 LUMPS available energy positive on sunlit row {r}"
        );
        assert_approx(
            qh_lumps,
            aux[30],
            1e-9,
            "Case B stage 07 QH_LUMPS matches outputLine SUEWS(16)",
        );
        assert_approx(
            qh_lumps,
            aux[38],
            1e-9,
            "Case B stage 07 QH_LUMPS matches no-debug outputLine SUEWS(16)",
        );
        assert_approx(
            qe_lumps,
            aux[39],
            1e-9,
            "Case B stage 07 QE_LUMPS matches no-debug outputLine SUEWS(17)",
        );

        // Stage 09 (resist): independent analytical g_kdown + no-debug witnesses on Case B.
        let exp_g_b = expected_g_kdown(met.kdown, &run.site.conductance);
        assert_approx(
            pp[8][1],
            exp_g_b,
            1e-12,
            "Case B stage 09 g_kdown analytical formula",
        );
        assert_approx(
            pp[8][1],
            aux[32],
            1e-12,
            "Case B stage 09 g_kdown no-debug control_state",
        );
        assert_approx(
            pp[8][1],
            aux[33],
            1e-12,
            "Case B stage 09 g_kdown no-debug public Debug(120)",
        );

        // Stage 10 (qe) hydrology transition remains visible under iter_safe == true.
        assert_ne!(aux[12], aux[13], "Case B stage 10 ev_per_tstep transition");
        assert_approx(
            pp[9][1],
            aux[14],
            1e-9,
            "Case B stage 10 latent heat identity",
        );

        // Stage 14 (biogenco2): active rectangular hyperbola photosynthesis + respiration.
        let par = 4.6 * 0.46 * met.kdown;
        let expected_fc_photo: f64 = veg_bio
            .iter()
            .map(|&(f, alpha, beta, lai, lai_max)| {
                let active_fr = f * (lai / lai_max);
                (-beta * alpha * par / (alpha * par + beta)) * active_fr
            })
            .sum();
        let fc_photo = aux[22];
        let fc_respi = aux[23];
        let fc_biogen = pp[13][1];
        let fc_total = aux[24];
        assert!(
            fc_photo < 0.0,
            "Case B stage 14 Fc_photo must be uptake (< 0)"
        );
        assert!(
            fc_respi > 0.0,
            "Case B stage 14 Fc_respi must be emission (> 0)"
        );
        assert_approx(
            fc_photo,
            expected_fc_photo,
            1e-9,
            "Case B stage 14 analytical rectangular-hyperbola Fc_photo",
        );
        assert_approx(
            fc_biogen,
            fc_photo + fc_respi,
            1e-12,
            "Case B stage 14 Fc_biogen == Fc_photo + Fc_respi",
        );
        assert_approx(
            fc_total,
            aux[5] + fc_biogen,
            1e-12,
            "Case B stage 14 Fc == Fc_anthro + Fc_biogen",
        );
        assert_ne!(
            pp[13][0], pp[13][1],
            "Case B stage 14 must transition Fc_biogen from stage 13 on row {r}"
        );

        // Stage 15 (beers): active localised radiation transition.
        assert!(pp[14][1] > 0.0);
        assert_ne!(pp[14][0], pp[14][1], "Case B stage 15 kdown_2d transition");
        assert_approx(
            pp[14][1],
            aux[31],
            1e-12,
            "Case B stage 15 outputLine BEERS(11)",
        );
    }

    assert_ne!(
        obs.pre_post[0][6][1], obs.pre_post[2][6][1],
        "Case B stage 07 QH_LUMPS must vary across forcing rows"
    );
    assert_ne!(
        obs.pre_post[0][13][1], obs.pre_post[2][13][1],
        "Case B stage 14 Fc_biogen must vary across forcing rows"
    );
}

fn assert_widths_match_schema(widths: &[i32; 11]) {
    let schema = output_block_schema_info().unwrap();
    let names = output_block_field_names();
    assert_eq!(names.len(), 11);
    for (idx, name) in names.iter().enumerate() {
        let expected_cols = schema.allocatable_dims.get(name).unwrap()[1];
        assert_eq!(
            widths[idx] as usize, expected_cols,
            "production output_block width mismatch for {name}"
        );
    }
}

#[test]
fn production_multiline_driver_captures_state_block_output_block_and_optional_debug() {
    // Case 1A: Observed QN/QS/QF baseline + reversed row-order control.
    let mut run_a = load_sample_fixture();
    run_a.config.net_radiation_method = 0;
    run_a.config.storage_heat_method = 0;
    run_a.config.emissions_method = 0;
    run_a.config.water_use_method = 0;
    run_a.config.snow_use = 0;
    prepare_wet_partially_dry_state(&mut run_a);
    let rows_a = sample_met_rows_ascending();
    let obs_a = run_multiline_observer(&run_a, &rows_a);

    assert_eq!(obs_a.mode_flags, [1, 1, 1, 1, 1]);
    assert_widths_match_schema(&obs_a.block_widths);
    for (r, &met) in rows_a.iter().enumerate() {
        let sb = obs_a.state_block[r];
        let ob = obs_a.output_block[r];
        assert_approx(ob[0], met.imin, 1e-12, "Case 1A SUEWS imin");
        assert_approx(sb[0], met.qn1_obs, 1e-12, "Case 1A block_mod_state QN");
        assert_approx(sb[1], met.qs_obs, 1e-12, "Case 1A block_mod_state QS");
        assert_approx(sb[2], met.qf_obs, 1e-12, "Case 1A block_mod_state QF");
        assert_approx(
            ob[1],
            sb[0],
            1e-12,
            "Case 1A output_block SUEWS QN == state_block",
        );
        assert_approx(
            ob[2],
            sb[2],
            1e-12,
            "Case 1A output_block SUEWS QF == state_block",
        );
        assert_approx(
            ob[3],
            sb[1],
            1e-12,
            "Case 1A output_block SUEWS QS == state_block",
        );
        assert_approx(
            ob[4],
            sb[3],
            1e-12,
            "Case 1A output_block SUEWS QH == state_block",
        );
        assert_approx(
            ob[5],
            sb[4],
            1e-12,
            "Case 1A output_block SUEWS QE == state_block",
        );
        assert_approx(
            ob[6],
            sb[12],
            1e-12,
            "Case 1A output_block SUEWS T2 == state_block",
        );
        assert_eq!(ob[8], 0.0, "Case 1A Snow inactive -> 0.0");
        assert_eq!(ob[9], 0.0, "Case 1A ESTM disabled -> 0.0");
        assert_eq!(ob[10], 0.0, "Case 1A EHC inactive -> 0.0");
        assert!(ob[11].is_finite() && ob[11] > 5.0, "Case 1A RSL T_1 active");
        assert_approx(
            ob[12],
            sb[14],
            1e-12,
            "Case 1A BEERS Kdown2d == state_block",
        );
        assert!(ob[13] > 250.0, "Case 1A BEERS Lup2d active");
        assert_approx(
            ob[14],
            sb[11],
            1e-12,
            "Case 1A Debug g_kdown == state_block",
        );
        assert_eq!(ob[15], -999.0, "Case 1A SPARTACUS inactive -> -999.0");
        assert_approx(
            sb[10],
            (r + 1) as f64 / 12.0,
            1e-12,
            "Case 1A block_mod_state lenDay_id",
        );
        assert_eq!(
            ob[16], -999.0,
            "Case 1A mid-day DailyState DLHrs stays -999.0"
        );
        assert_eq!(ob[18], -999.0, "Case 1A STEBBS inactive -> -999.0");
        assert_eq!(ob[19], -999.0, "Case 1A NHood legacy unassigned -> -999.0");
        assert_eq!(sb[15], 1.0, "Case 1A converged");
    }
    assert_ne!(obs_a.state_block[0][3], obs_a.state_block[2][3]);
    assert_ne!(obs_a.state_block[0][7], obs_a.state_block[2][7]);
    // Final-row state_debug captured by run_multitsteps_block matches row 2.
    assert_approx(
        obs_a.final_debug[4],
        rows_a[2].qn1_obs,
        1e-12,
        "Case 1A final_debug QN",
    );
    assert_approx(
        obs_a.final_debug[5],
        rows_a[2].qs_obs,
        1e-12,
        "Case 1A final_debug QS",
    );
    assert_approx(
        obs_a.final_debug[10],
        obs_a.state_block[2][3],
        1e-12,
        "Case 1A final_debug QH",
    );
    assert_approx(
        obs_a.final_debug[14],
        obs_a.state_block[2][14],
        1e-12,
        "Case 1A final_debug kdown_2d",
    );
    let exp_g_a2 = expected_g_kdown(rows_a[2].kdown, &run_a.site.conductance);
    assert_approx(
        obs_a.final_debug[8],
        exp_g_a2,
        1e-12,
        "Case 1A final_debug stage 09 g_kdown analytical formula",
    );
    assert_approx(
        obs_a.final_debug[8],
        obs_a.state_block[2][11],
        1e-12,
        "Case 1A final_debug stage 09 g_kdown == state_block",
    );
    assert_approx(
        obs_a.final_debug[8],
        obs_a.output_block[2][14],
        1e-12,
        "Case 1A final_debug stage 09 g_kdown == output_block Debug(120)",
    );
    assert_wall_rectangle_matches_noblock(&obs_a, "Case 1A");

    // Reversed row-order control on run_multitsteps_block.
    let mut rows_a_rev = [rows_a[2], rows_a[1], rows_a[0]];
    for (r, row) in rows_a_rev.iter_mut().enumerate() {
        row.imin = 5.0 * (r + 1) as f64;
    }
    let obs_a_rev = run_multiline_observer(&run_a, &rows_a_rev);
    assert_approx(
        obs_a_rev.state_block[0][0],
        340.0,
        1e-12,
        "Case 1A reversed row 0 QN",
    );
    assert_approx(
        obs_a_rev.state_block[2][0],
        150.0,
        1e-12,
        "Case 1A reversed row 2 QN",
    );
    assert_approx(
        obs_a_rev.output_block[0][1],
        340.0,
        1e-12,
        "Case 1A reversed output row 0 QN",
    );
    assert_approx(
        obs_a_rev.output_block[2][1],
        150.0,
        1e-12,
        "Case 1A reversed output row 2 QN",
    );
    assert_approx(
        obs_a_rev.final_debug[8],
        expected_g_kdown(rows_a_rev[2].kdown, &run_a.site.conductance),
        1e-12,
        "Case 1A reversed final_debug stage 09 g_kdown analytical formula",
    );
    assert_wall_rectangle_matches_noblock(&obs_a_rev, "Case 1A reversed");

    // Case 1B: Activating EHC + NARP + biogenic CO2 -> populates dataOutBlockEHC & Fc.
    let mut run_b = load_sample_fixture();
    run_b.config.net_radiation_method = 3;
    run_b.config.storage_heat_method = 5;
    run_b.config.emissions_method = 11;
    run_b.config.water_use_method = 0;
    run_b.config.snow_use = 0;
    prepare_wet_partially_dry_state(&mut run_b);
    let obs_b = run_multiline_observer(&run_b, &rows_a);
    assert_eq!(obs_b.mode_flags, [1, 1, 1, 1, 1]);
    assert_widths_match_schema(&obs_b.block_widths);
    for r in 0..3 {
        assert!(
            obs_b.output_block[r][10] > 0.0 && obs_b.output_block[r][10] < 80.0,
            "Case 1B dataOutBlockEHC Ts_Paved active on row {r}: {}",
            obs_b.output_block[r][10]
        );
        assert!(
            obs_b.state_block[r][13] != 0.0,
            "Case 1B block_mod_state Fc_biogen active on row {r}"
        );
        assert!(obs_b.output_block[r][7] != 0.0, "Case 1B SUEWS Fc active");
    }
    assert_ne!(
        obs_b.output_block[0][10], obs_b.output_block[2][10],
        "Case 1B dataOutBlockEHC Ts_Paved must evolve across timesteps"
    );
    assert_approx(
        obs_b.final_debug[8],
        expected_g_kdown(rows_a[2].kdown, &run_b.site.conductance),
        1e-12,
        "Case 1B final_debug stage 09 g_kdown analytical formula",
    );
    assert_wall_rectangle_matches_noblock(&obs_b, "Case 1B");

    // Case 1C: Activating Snow (SnowUse == 1, sub-zero snowfall) -> populates dataOutBlockSnow.
    let mut run_c = load_sample_fixture();
    run_c.config.net_radiation_method = 0;
    run_c.config.storage_heat_method = 0;
    run_c.config.emissions_method = 0;
    run_c.config.water_use_method = 0;
    run_c.config.snow_use = 1;
    run_c.state.snow_state.snowpack = [2.0; 7];
    run_c.state.snow_state.snowfrac = [0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.0];
    run_c.state.snow_state.snowdens = [250.0; 7];
    run_c.state.snow_state.snowalb = 0.75;
    let mut rows_c = sample_met_rows_ascending();
    for (r, row) in rows_c.iter_mut().enumerate() {
        row.id = 15.0;
        row.temp_c = -3.0 - 0.5 * (r as f64);
        row.qn1_obs = 20.0 + 10.0 * (r as f64);
        row.qs_obs = 2.0 + (r as f64);
        row.qf_obs = 15.0;
        row.rain = 1.0 + (r as f64);
        row.kdown = 80.0 + 30.0 * (r as f64);
        row.snow_fraction = 0.6;
        row.l_down = 250.0;
    }
    let obs_c = run_multiline_observer(&run_c, &rows_c);
    assert_eq!(obs_c.mode_flags, [1, 1, 1, 1, 1]);
    assert_widths_match_schema(&obs_c.block_widths);
    assert!(
        obs_c.output_block[0][8] > 2.0
            && obs_c.output_block[1][8] > obs_c.output_block[0][8]
            && obs_c.output_block[2][8] > obs_c.output_block[1][8],
        "Case 1C dataOutBlockSnow SWE_Paved must accumulate snowfall: {:?}",
        [
            obs_c.output_block[0][8],
            obs_c.output_block[1][8],
            obs_c.output_block[2][8]
        ]
    );

    // Case 1D: Activating STEBBS + SPARTACUS fixture -> populates dataOutBlockSPARTACUS & STEBBS.
    let run_d = load_run_config_from_str(include_str!(
        "../../../test/fixtures/bridge/stebbs_resolved.yml"
    ))
    .unwrap();
    let mut rows_d = sample_met_rows_ascending();
    for (r, row) in rows_d.iter_mut().enumerate() {
        row.iy = 2017.0;
        row.id = 238.0;
        row.it = 12.0;
        row.imin = 5.0 * (r + 1) as f64;
        row.u = 2.5 + 0.6 * (r as f64);
        row.temp_c = 19.0 + 1.5 * (r as f64);
        row.rain = 0.0;
    }
    let obs_d = run_multiline_observer(&run_d, &rows_d);
    assert_eq!(obs_d.mode_flags, [1, 1, 1, 1, 1]);
    assert_widths_match_schema(&obs_d.block_widths);
    for (r, out_row) in obs_d.output_block.iter().enumerate() {
        assert!(
            out_row[15] > 0.0 && out_row[15] < 1.0,
            "Case 1D dataOutBlockSPARTACUS alb active on row {r}: {}",
            out_row[15]
        );
        assert!(
            out_row[18] > 0.0 && out_row[18] < 20.0,
            "Case 1D dataOutBlockSTEBBS ws_10 active on row {r}: {}",
            out_row[18]
        );
    }
    assert_ne!(
        obs_d.output_block[0][18], obs_d.output_block[2][18],
        "Case 1D dataOutBlockSTEBBS ws_10 must vary across forcing rows"
    );

    // Case 1E: End-of-day boundary (23:45, 23:50, 23:55) activates real
    // dataOutBlockDailyState population on row 2 while rows 0..1 stay -999.0.
    let mut rows_e = sample_met_rows_ascending();
    for (r, row) in rows_e.iter_mut().enumerate() {
        row.it = 23.0;
        row.imin = 45.0 + 5.0 * (r as f64);
        row.kdown = 25.0;
    }
    let obs_e = run_multiline_observer(&run_a, &rows_e);
    assert_eq!(obs_e.mode_flags, [1, 1, 1, 1, 1]);
    assert_widths_match_schema(&obs_e.block_widths);
    assert_eq!(
        obs_e.output_block[0][16], -999.0,
        "Case 1E row 0 pre-EOD DailyState"
    );
    assert_eq!(
        obs_e.output_block[1][16], -999.0,
        "Case 1E row 1 pre-EOD DailyState"
    );
    assert_approx(
        obs_e.output_block[2][16],
        3.0 / 12.0,
        1e-12,
        "Case 1E row 2 (23:55) populated DailyState DLHrs",
    );
    assert_approx(
        obs_e.output_block[2][17],
        rows_e[0].rain + rows_e[1].rain + rows_e[2].rain,
        1e-12,
        "Case 1E row 2 (23:55) populated DailyState P_day",
    );
    assert_wall_rectangle_matches_noblock(&obs_c, "Case 1C");
    assert_wall_rectangle_matches_noblock(&obs_d, "Case 1D");
    assert_wall_rectangle_matches_noblock(&obs_e, "Case 1E");

    // Case 1F: Facet-resolved SPARTACUS + EHC (NetRadiationMethod = 1003 > 1000,
    // StorageHeatMethod = 5) on building-active KCL site (sfr_bldgs = 0.38,
    // nlayer = 3, ndepth = 5). Exercises live 1D wall heat conduction across all
    // 3 rows and 3 vertical layers so block_mod_state%BLOCK(r)%heatState%temp_wall
    // (including temp_wall(1,1)) is non-zero, physically evolving, layer-distinct,
    // and proved element-by-element against independently produced no-block state.
    let mut run_f = load_sample_fixture();
    run_f.config.net_radiation_method = 1003;
    run_f.config.storage_heat_method = 5;
    run_f.config.emissions_method = 0;
    run_f.config.water_use_method = 0;
    run_f.config.snow_use = 0;
    run_f.config.stebbs_method = 0;
    prepare_wet_partially_dry_state(&mut run_f);
    assert!(
        run_f.site_scalars.sfr_surf[1] > 0.1,
        "Case 1F requires active building fraction, got {}",
        run_f.site_scalars.sfr_surf[1]
    );
    assert!(
        run_f.nlayer >= 2 && run_f.ndepth >= 2,
        "Case 1F requires at least 2 wall layers and depths, got ({}, {})",
        run_f.nlayer,
        run_f.ndepth
    );
    let init_wall_11 = run_f.state.heat_state.temp_wall[0];
    let obs_f = run_multiline_observer(&run_f, &rows_a);
    assert_eq!(obs_f.mode_flags, [1, 1, 1, 1, 1]);
    assert_widths_match_schema(&obs_f.block_widths);
    assert_wall_rectangle_matches_noblock(&obs_f, "Case 1F");
    for r in 0..3 {
        assert!(
            obs_f.output_block[r][10].is_finite() && obs_f.output_block[r][10] > 0.0,
            "Case 1F EHC active on row {r}: {}",
            obs_f.output_block[r][10]
        );
        assert!(
            obs_f.output_block[r][15] > 0.0 && obs_f.output_block[r][15] < 1.0,
            "Case 1F SPARTACUS albedo active on row {r}: {}",
            obs_f.output_block[r][15]
        );
        for layer in 0..obs_f.nlayer {
            for depth in 0..obs_f.ndepth {
                let tw = obs_f.wall_block_at(r, layer, depth);
                assert!(
                    tw.is_finite() && tw > 0.0 && tw < 80.0,
                    "Case 1F non-trivial physical wall temp at row {r} layer {} depth {}: {tw}",
                    layer + 1,
                    depth + 1
                );
            }
        }
    }
    let w11_r0 = obs_f.wall_block_at(0, 0, 0);
    let w11_r1 = obs_f.wall_block_at(1, 0, 0);
    let w11_r2 = obs_f.wall_block_at(2, 0, 0);
    assert_ne!(
        w11_r0, init_wall_11,
        "Case 1F temp_wall(1,1) must evolve away from initial seed {init_wall_11}"
    );
    assert!(
        w11_r0 != w11_r1 && w11_r1 != w11_r2 && w11_r0 != w11_r2,
        "Case 1F temp_wall(1,1) must be distinct across all 3 rows: [{w11_r0}, {w11_r1}, {w11_r2}]"
    );
    let w21_r0 = obs_f.wall_block_at(0, 1, 0);
    let w21_r1 = obs_f.wall_block_at(1, 1, 0);
    let w21_r2 = obs_f.wall_block_at(2, 1, 0);
    assert!(
        w21_r0 != w21_r1 && w21_r1 != w21_r2,
        "Case 1F layer-2 temp_wall(2,1) must evolve across rows: [{w21_r0}, {w21_r1}, {w21_r2}]"
    );
    assert_ne!(
        w11_r2, w21_r2,
        "Case 1F wall layers 1 and 2 must be physically distinct at depth 1: {w11_r2} vs {w21_r2}"
    );
    assert_ne!(
        w11_r2,
        obs_f.wall_block_at(2, 0, obs_f.ndepth - 1),
        "Case 1F outer vs inner wall depth nodes must differ"
    );
    assert_approx(
        obs_f.final_debug[8],
        expected_g_kdown(rows_a[2].kdown, &run_f.site.conductance),
        1e-12,
        "Case 1F final_debug stage 09 g_kdown analytical formula",
    );
}
