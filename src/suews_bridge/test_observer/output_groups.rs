//! Native typed output -> production writer -> named registry columns,
//! plus native OUTPUT_BLOCK lifecycle and Rust OutputBlock codec proof.
// contract: C-CAPI, C-OUTPUT
use std::collections::BTreeMap;

use suews_bridge::{
    output_block_field_names, output_block_from_values_payload, output_block_schema_info,
    output_block_to_rows_map, output_block_to_values_payload, BridgeError,
    OutputBlockValuesPayload, OUTPUT_BLOCK_SCHEMA_VERSION, OUTPUT_DATA_COLS, OUTPUT_GROUP_LAYOUT,
};

unsafe extern "C" {
    fn suews_obs_output_groups(data: *mut f64, dates: *mut f64, ld: i32, ncols: i32, err: *mut i32);
    fn suews_obs_output_block(
        nrows: i32,
        widths_out: *mut i32,
        corners_out: *mut f64,
        err: *mut i32,
    );
}

#[test]
fn native_output_groups_preserve_named_columns_and_strided_rows() {
    const LD: usize = 5;
    const GUARD: f64 = -7654321.0;
    let mut data = vec![GUARD; LD * OUTPUT_DATA_COLS];
    let mut dates = vec![GUARD; LD * 5];
    let mut err = -1;
    // SAFETY: disjoint owned arrays, exactly the declared extents; the witness
    // validates dimensions before calling the same writer as the simulation.
    unsafe {
        suews_obs_output_groups(
            data.as_mut_ptr(),
            dates.as_mut_ptr(),
            LD as i32,
            OUTPUT_DATA_COLS as i32,
            &mut err,
        );
    }
    assert_eq!(err, 0);
    let before = data.clone();
    // The test-only boundary refuses a shorter matrix before writing anything.
    unsafe {
        suews_obs_output_groups(
            data.as_mut_ptr(),
            dates.as_mut_ptr(),
            LD as i32,
            OUTPUT_DATA_COLS as i32 - 1,
            &mut err,
        );
    }
    assert_ne!(err, 0);
    assert_eq!(data, before);

    let catalogue = read_output_catalogue();
    let variables = catalogue["variables"].as_array().unwrap();
    // Independently check every single one of the 1,355 data columns across all
    // 11 groups (including NHood/iter_count) against globally distinct sentinels.
    let mut expected = vec![f64::NAN; OUTPUT_DATA_COLS * 2];
    let mut offset = 0;
    let mut named_witnesses: Vec<(&str, &str, [f64; 2])> = Vec::new();
    let spot_checks = [
        ("SUEWS", "Kdown"),
        ("SUEWS", "UStar"),
        ("SUEWS", "QS_Water"),
        ("snow", "SWE_Paved"),
        ("snow", "Qn_BldgsSnow"),
        ("snow", "SnowAlb"),
        ("BEERS", "azimuth"),
        ("BEERS", "Lwest"),
        ("BEERS", "Tw"),
        ("ESTM", "QS"),
        ("ESTM", "TROOF3"),
        ("ESTM", "TaBLD"),
        ("EHC", "Ts_Paved"),
        ("EHC", "Ts_Roof_1"),
        ("EHC", "SS_Wall_15"),
        ("DailyState", "HDD1_h"),
        ("DailyState", "WU_EveTr1"),
        ("DailyState", "a3_bldg"),
        ("RSL", "z_1"),
        ("RSL", "T_8"),
        ("RSL", "flag_RSL"),
        ("debug", "flag_test"),
        ("debug", "drain_Bldgs"),
        ("debug", "dqndt"),
        ("SPARTACUS", "alb"),
        ("SPARTACUS", "LNet_Wall_8"),
        ("SPARTACUS", "LVegAbs_15"),
        ("STEBBS", "ws_10"),
        ("STEBBS", "Qlw_net_vessel_FA"),
        ("STEBBS", "Vwater_vessel"),
        ("NHood", "iter_count"),
    ];
    for (group_idx, &(group, width)) in OUTPUT_GROUP_LAYOUT.iter().enumerate() {
        let mut names: Vec<_> = variables.iter().filter(|v| v["group"] == group).collect();
        names.sort_by_key(|v| v["ordinal"].as_u64().unwrap());
        let n_data = width - 5;
        assert_eq!(names.len(), n_data, "{group}");
        let base = ((group_idx + 1) * 10000) as f64;
        for (ord, var) in names.iter().enumerate() {
            let col = offset + ord;
            let v1 = base + (ord + 1) as f64 + 0.25;
            let v2 = base + (ord + 1) as f64 + 0.5;
            expected[2 * col] = v1;
            expected[2 * col + 1] = v2;
            let name = var["name"].as_str().unwrap();
            assert_eq!(data[LD * col + 1], v1, "{group}/{name} row 0");
            assert_eq!(data[LD * col + 3], v2, "{group}/{name} row 1");
        }
        for &(sample_group, sample_name) in &spot_checks {
            if sample_group == group {
                let ord = names
                    .iter()
                    .position(|v| v["name"] == sample_name)
                    .unwrap_or_else(|| panic!("missing catalogue entry {group}/{sample_name}"));
                let col = offset + ord;
                named_witnesses.push((
                    sample_group,
                    sample_name,
                    [expected[2 * col], expected[2 * col + 1]],
                ));
            }
        }
        offset += n_data;
    }
    assert_eq!(offset, OUTPUT_DATA_COLS);
    assert_eq!(named_witnesses.len(), spot_checks.len());
    for col in 0..OUTPUT_DATA_COLS {
        for row in 0..LD {
            let want = match row {
                1 => expected[2 * col],
                3 => expected[2 * col + 1],
                _ => GUARD,
            };
            assert_eq!(data[LD * col + row], want, "data row {row}, column {col}");
        }
    }
    let expected_dates = [
        [2012.0, 37.0, 9.0, 5.0, 37.375 + 1.0 / 288.0],
        [2012.0, 37.0, 9.0, 10.0, 37.375 + 2.0 / 288.0],
    ];
    for col in 0..5 {
        for row in 0..LD {
            let want = match row {
                1 => expected_dates[0][col],
                3 => expected_dates[1][col],
                _ => GUARD,
            };
            assert_eq!(
                dates[LD * col + row],
                want,
                "datetime row {row}, column {col}"
            );
        }
    }
    // Optional evidence hand-off to the ordinary Python frame constructor.
    // This test-only file export is absent from every production API.
    if let Ok(path) = std::env::var("SUEWS_OUTPUT_WITNESS_JSON") {
        std::fs::write(
            path,
            serde_json::to_vec(&serde_json::json!({
                "data": data, "datetime": dates, "ld": LD, "samples": named_witnesses,
            }))
            .unwrap(),
        )
        .unwrap();
    }
}

#[test]
fn native_output_block_init_asymmetric_2d_and_rust_codec_roundtrip() {
    const NROWS: usize = 3;
    let mut widths = [0_i32; 11];
    let mut corners = [f64::NAN; 44];
    let mut err = -1_i32;
    // Reject invalid row counts before allocating anything.
    unsafe {
        suews_obs_output_block(1, widths.as_mut_ptr(), corners.as_mut_ptr(), &mut err);
    }
    assert_ne!(err, 0, "nrows < 2 must be rejected");

    // SAFETY: disjoint owned fixed-size buffers match the BIND(C) contract.
    unsafe {
        suews_obs_output_block(
            NROWS as i32,
            widths.as_mut_ptr(),
            corners.as_mut_ptr(),
            &mut err,
        );
    }
    assert_eq!(err, 0, "native TYPE(output_block) init/seed/cleanup failed");

    let schema = output_block_schema_info().unwrap();
    let field_names = output_block_field_names();
    assert_eq!(field_names.len(), 11);

    let mut payload_values = Vec::new();
    let mut payload_dims = BTreeMap::new();
    for (idx, field_name) in field_names.iter().enumerate() {
        let gidx = idx + 1;
        let schema_cols = schema.allocatable_dims[field_name][1];
        let native_cols = usize::try_from(widths[idx]).unwrap();
        assert_eq!(
            native_cols, schema_cols,
            "Fortran TYPE(output_block)%{field_name} width mismatch vs suews_output_block_columns"
        );
        let expected_corners = [
            (gidx * 100000) as f64 + (1000 + 1) as f64 + 0.125,
            (gidx * 100000) as f64 + (1000 + native_cols) as f64 + 0.125,
            (gidx * 100000) as f64 + (NROWS * 1000 + 1) as f64 + 0.125 * NROWS as f64,
            (gidx * 100000) as f64 + (NROWS * 1000 + native_cols) as f64 + 0.125 * NROWS as f64,
        ];
        assert_eq!(
            &corners[4 * idx..4 * idx + 4],
            &expected_corners,
            "asymmetric 2D corners for native output_block%{field_name}"
        );
        payload_dims.insert(field_name.clone(), vec![NROWS, native_cols]);
        for r in 1..=NROWS {
            for c in 1..=native_cols {
                payload_values
                    .push((gidx * 100000) as f64 + (r * 1000 + c) as f64 + 0.125 * r as f64);
            }
        }
    }

    let payload = OutputBlockValuesPayload {
        schema_version: OUTPUT_BLOCK_SCHEMA_VERSION,
        values: payload_values,
        dims: payload_dims,
    };
    let decoded = output_block_from_values_payload(&payload).unwrap();
    let reencoded = output_block_to_values_payload(&decoded);
    assert_eq!(reencoded, payload);

    let rows_map = output_block_to_rows_map(&decoded).unwrap();
    assert_eq!(rows_map.len(), 11);
    for (idx, field_name) in field_names.iter().enumerate() {
        let gidx = idx + 1;
        let rows = &rows_map[field_name];
        assert_eq!(rows.len(), NROWS);
        let ncols = usize::try_from(widths[idx]).unwrap();
        assert_eq!(rows[0].len(), ncols);
        assert_eq!(rows[0][0], (gidx * 100000 + 1001) as f64 + 0.125);
        assert_eq!(
            rows[NROWS - 1][ncols - 1],
            (gidx * 100000 + NROWS * 1000 + ncols) as f64 + 0.125 * NROWS as f64
        );
    }

    // Negative bound: wrong column width on any group must be rejected.
    let mut bad_payload = payload;
    bad_payload.dims.get_mut("data_out_block_nhood").unwrap()[1] += 1;
    assert_eq!(
        output_block_from_values_payload(&bad_payload).unwrap_err(),
        BridgeError::BadState
    );
}

fn read_output_catalogue() -> serde_json::Value {
    let output_dir =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../supy/data_model/output");
    let version_source = std::fs::read_to_string(output_dir.join("version.py")).unwrap();
    let version = version_source
        .lines()
        .find_map(|line| {
            let rest = line.trim().strip_prefix("CURRENT_OUTPUT_VERSION")?;
            rest.split_once('=')?
                .1
                .trim()
                .strip_prefix('"')?
                .strip_suffix('"')
        })
        .expect("current output contract version");
    serde_json::from_str(
        &std::fs::read_to_string(
            output_dir
                .join("artefacts")
                .join(version)
                .join("catalogue.json"),
        )
        .unwrap(),
    )
    .unwrap()
}

#[test]
fn incoming_shortwave_and_rain_reach_the_named_public_outputs() {
    use suews_bridge::{
        run_from_config_str_and_forcing, SuewsField, FORCING_BLOCK_STRIDE, OUTPUT_ALL_COLS,
        OUTPUT_DATETIME_COLS,
    };

    let config = include_str!("../../../test/fixtures/bridge/sample_resolved.yml");
    let shortwave = [230.0, 490.0, 360.0];
    let rainfall = [0.5, 0.0, 0.125];
    let cloud_fraction = [0.125, 0.5, 0.875];
    let mut forcing = Vec::new();
    for step in 0..shortwave.len() {
        let mut row = [0.0; FORCING_BLOCK_STRIDE];
        row[..4].copy_from_slice(&[2012.0, 180.0, 12.0, (5 * (step + 1)) as f64]);
        for (field, value) in [
            (SuewsField::u, 2.0 + 0.5 * step as f64),
            (SuewsField::rh, 60.0 + 5.0 * step as f64),
            (SuewsField::temp_c, 21.0 + step as f64),
            (SuewsField::pres, 1013.0 - step as f64),
            (SuewsField::kdown, shortwave[step]),
            (SuewsField::rain, rainfall[step]),
            (SuewsField::ldown, 350.0 + 10.0 * step as f64),
            (SuewsField::fcld, cloud_fraction[step]),
        ] {
            row[field.index()] = value;
        }
        forcing.extend(row);
    }
    let (output, _, _, steps, _) =
        run_from_config_str_and_forcing(config, forcing, shortwave.len()).unwrap();
    assert_eq!(steps, shortwave.len());
    assert_eq!(output.len(), steps * OUTPUT_ALL_COLS);
    assert_eq!(OUTPUT_GROUP_LAYOUT[0].0, "SUEWS");
    let catalogue = read_output_catalogue();
    let variables = catalogue["variables"].as_array().unwrap();
    for (step, row) in output.chunks_exact(OUTPUT_ALL_COLS).enumerate() {
        assert_eq!(row[0], 2012.0, "datetime Year, step {step}");
        assert_eq!(row[1], 180.0, "datetime DOY, step {step}");
        assert_eq!(row[2], 12.0, "datetime Hour, step {step}");
        assert_eq!(row[3], (5 * (step + 1)) as f64, "datetime Min, step {step}");
    }
    for (name, inputs) in [
        ("Kdown", shortwave),
        ("Rain", rainfall),
        ("Fcld", cloud_fraction),
    ] {
        let variable = variables
            .iter()
            .find(|v| v["group"] == "SUEWS" && v["name"] == name)
            .expect("public output catalogue entry");
        let column = OUTPUT_DATETIME_COLS + variable["ordinal"].as_u64().unwrap() as usize;
        for (step, row) in output.chunks_exact(OUTPUT_ALL_COLS).enumerate() {
            assert_eq!(row[column], inputs[step], "SUEWS/{name}, step {step}");
        }
    }
}
