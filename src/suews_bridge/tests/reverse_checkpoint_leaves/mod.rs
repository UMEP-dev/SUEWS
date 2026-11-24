//! Observe native typed members across the production return buffer and an on-disk checkpoint.
// contract: C-CAPI, C-CODEC

use std::fs;
use suews_bridge::{
    decode_state_members, encode_state_members_flat, heat_state_expected_flat_len,
    hydro_state_field_names_with_lens, suews_checkpoint_from_json, suews_checkpoint_to_json,
    suews_state_default_from_fortran, HeatState, HydroState, SuewsState, SuewsTimer, NSURF,
};

extern "C" {
    fn suews_obs_reverse_checkpoint(
        nlayer: i32,
        ndepth: i32,
        seeds: *const f64,
        source: *const f64,
        nflat: i32,
        toc: *const i32,
        ntoc: i32,
        packed: *mut f64,
        err: *mut i32,
    );
}

fn check_members(state: &SuewsState, nlayer: usize, ndepth: usize, seeds: [f64; 3]) {
    let heat = &state.heat_state;
    let hydro = &state.hydro_state;
    let stebbs = &state.stebbs_state;
    assert_eq!(
        stebbs.indoor_air_start_temperature, seeds[0],
        "indoor_air_start_temperature"
    );
    assert_eq!(
        stebbs.indoor_mass_start_temperature, seeds[1],
        "indoor_mass_start_temperature"
    );
    assert_eq!(
        stebbs.wall_indoor_surface_temperature, seeds[2],
        "wall_indoor_surface_temperature"
    );
    assert_eq!((heat.nlayer, heat.ndepth), (nlayer, ndepth));

    // Field IDs are independent of slot numbers. Native coordinates start at one.
    for (field, values, tag, rows) in [
        ("temp_roof", &heat.temp_roof, 1, nlayer),
        ("temp_wall", &heat.temp_wall, 2, nlayer),
        ("temp_surf", &heat.temp_surf, 3, NSURF),
        ("temp_surf_dyohm", &heat.temp_surf_dyohm, 4, NSURF),
    ] {
        assert_eq!(values.len(), rows * ndepth, "{field} dimensions");
        for (index, &value) in values.iter().enumerate() {
            let row = index / ndepth + 1;
            let col = index % ndepth + 1;
            let expected = f64::from(tag) + row as f64 / 16.0 + col as f64 / 1024.0;
            assert_eq!(value, expected, "{field}({row},{col})");
        }
    }
    for (field, values, tag, length) in [
        ("tsfc_roof", &heat.tsfc_roof, 5, nlayer),
        ("tsfc_wall", &heat.tsfc_wall, 6, nlayer),
        ("tsfc_surf", &heat.tsfc_surf, 7, NSURF),
        ("tsfc_surf_dyohm", &heat.tsfc_surf_dyohm, 8, NSURF),
        ("tsfc_roof_stepstart", &heat.tsfc_roof_stepstart, 9, nlayer),
        ("tsfc_wall_stepstart", &heat.tsfc_wall_stepstart, 10, nlayer),
        ("tsfc_surf_stepstart", &heat.tsfc_surf_stepstart, 11, NSURF),
        ("qs_roof", &heat.qs_roof, 12, nlayer),
        ("qn_roof", &heat.qn_roof, 13, nlayer),
        ("qe_roof", &heat.qe_roof, 14, nlayer),
        ("qh_roof", &heat.qh_roof, 15, nlayer),
        ("qh_resist_roof", &heat.qh_resist_roof, 16, nlayer),
        ("qs_wall", &heat.qs_wall, 17, nlayer),
        ("qn_wall", &heat.qn_wall, 18, nlayer),
        ("qe_wall", &heat.qe_wall, 19, nlayer),
        ("qh_wall", &heat.qh_wall, 20, nlayer),
        ("qh_resist_wall", &heat.qh_resist_wall, 21, nlayer),
        ("soilstore_roof", &hydro.soilstore_roof, 22, nlayer),
        ("state_roof", &hydro.state_roof, 23, nlayer),
        ("soilstore_wall", &hydro.soilstore_wall, 24, nlayer),
        ("state_wall", &hydro.state_wall, 25, nlayer),
        ("ev_roof", &hydro.ev_roof, 26, nlayer),
        ("ev_wall", &hydro.ev_wall, 27, nlayer),
    ] {
        assert_eq!(values.len(), length, "{field} length");
        for (index, &value) in values.iter().enumerate() {
            let row = index + 1;
            assert_eq!(value, f64::from(tag) + row as f64 / 16.0, "{field}({row})");
        }
    }
}

fn check_checkpoint(nlayer: usize, ndepth: usize, seeds: [f64; 3]) {
    let mut template = suews_state_default_from_fortran().unwrap();
    let heat_len = heat_state_expected_flat_len(nlayer, ndepth).unwrap();
    template.heat_state =
        HeatState::from_flat_with_dims(&vec![0.0; heat_len], nlayer, ndepth).unwrap();
    let hydro_lens = [nlayer; 6];
    let hydro_len = hydro_state_field_names_with_lens(hydro_lens).len();
    template.hydro_state =
        HydroState::from_flat_with_lens(&vec![0.0; hydro_len], hydro_lens).unwrap();

    let (input, toc) = encode_state_members_flat(&template).unwrap();
    let mut output = vec![f64::NAN; input.len()];
    let mut error = -1;
    // All lengths come from the production encoder. The output has its own full allocation.
    unsafe {
        suews_obs_reverse_checkpoint(
            nlayer.try_into().unwrap(),
            ndepth.try_into().unwrap(),
            seeds.as_ptr(),
            input.as_ptr(),
            input.len().try_into().unwrap(),
            toc.as_ptr(),
            toc.len().try_into().unwrap(),
            output.as_mut_ptr(),
            &mut error,
        );
    }
    assert_eq!(error, 0, "Fortran aggregate pack failed");
    let returned = decode_state_members(&template, &output, &toc, nlayer, ndepth, nlayer).unwrap();
    check_members(&returned, nlayer, ndepth, seeds);

    let timer = SuewsTimer {
        tstep: 300,
        ..Default::default()
    };
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("checkpoint.json");
    fs::write(&path, suews_checkpoint_to_json(&returned, &timer).unwrap()).unwrap();
    let restored = suews_checkpoint_from_json(&fs::read_to_string(path).unwrap()).unwrap();
    check_members(&restored.state, nlayer, ndepth, seeds);
}

#[test]
fn reverse_checkpoint_three_layers_five_depths() {
    check_checkpoint(3, 5, [21.25, 19.75, 17.5]);
}

#[test]
fn reverse_checkpoint_two_layers_four_depths() {
    check_checkpoint(2, 4, [12.5, -1.75, 14.25]);
}
