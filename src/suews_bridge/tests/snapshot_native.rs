//! Lower-level optional Fortran producer proof; no production snapshot wire.
use suews_bridge::{
    encode_site_members_flat, encode_site_scalars_flat, encode_state_members_flat,
    load_run_config_from_str,
};

extern "C" {
    fn snapshot_observe(
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
        observed: *mut f64,
        err: *mut i32,
    );
}

#[test]
fn optional_main_captures_real_named_stages_and_allocated_shapes() {
    let mut run = load_run_config_from_str(include_str!(
        "../../../test/fixtures/bridge/sample_resolved.yml"
    ))
    .unwrap();
    run.config.net_radiation_method = 0;
    run.config.storage_heat_method = 0;
    // Exercise the existing iteration reset branch: Qs before its stage
    // remains the preceding timestep, unlike the new observed Qs.
    run.state.heat_state.iter_safe = false;
    let config = run.config.to_flat();
    let timer = run.timer.to_flat();
    let scalars = encode_site_scalars_flat(&run.site_scalars);
    let (site, site_toc) = encode_site_members_flat(&run.site).unwrap();
    let (state, state_toc) = encode_state_members_flat(&run.state).unwrap();
    assert_eq!((config.len(), timer.len(), scalars.len()), (24, 18, 24));
    assert_eq!((site_toc.len(), state_toc.len()), (36, 26));
    let mut observed = [f64::NAN; 27];
    let mut err = -1;
    // Fixed buffers match the test-only bind(c) contract. Inputs are live,
    // nonoverlapping immutable buffers; output and status are exclusive.
    unsafe {
        snapshot_observe(
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
            observed.as_mut_ptr(),
            &mut err,
        );
    }
    assert_eq!(err, 0, "native optional producer failed");
    eprintln!("native snapshots: {observed:?}");
    for (index, row) in observed.chunks_exact(9).enumerate() {
        let step = (index + 1) as f64;
        assert_eq!(row[0], 100.0 + 37.0 * step, "stage 05 Qn, row {index}");
        assert_eq!(
            row[1],
            if index == 0 {
                0.0
            } else {
                10.0 + 13.0 * (step - 1.0)
            },
            "stage 05 retains previous Qs"
        );
        assert_eq!(row[2], 10.0 + 13.0 * step, "stage 06 Qs, row {index}");
        assert!(row[3].is_finite(), "stage 11 QH");
        assert!(row[4] == 0.0 || row[4] == 1.0);
        assert!(row[5] == 0.0 || row[5] == 1.0);
        assert_eq!(row[6], f64::from(run.nlayer));
        assert_eq!(row[7], f64::from(run.ndepth));
        assert_eq!(row[8], 100.0 + 37.0 * step, "harness deep-copy row {index}");
    }
}
