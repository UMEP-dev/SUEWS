//! Resolved file-run period enforcement and direct-buffer preservation tests.
#![cfg(feature = "physics")]

use serde_yaml::Value;
use std::fs;
use suews_bridge::{
    load_run_config, load_run_config_from_str, load_run_config_from_value,
    run_from_config_str_and_forcing, SuewsField, FORCING_BLOCK_STRIDE, OUTPUT_ALL_COLS,
    TIME_COLUMNS,
};

const SAMPLE_FIXTURE: &str = include_str!("../../../test/fixtures/bridge/sample_resolved.yml");

fn config_with_bounds(
    start_yaml: Option<&str>,
    end_yaml: Option<&str>,
    forcing_name: &str,
    output_dir: &str,
) -> Value {
    let mut root: Value = serde_yaml::from_str(SAMPLE_FIXTURE).unwrap();
    root["model"]["control"]["forcing"]["file"] = Value::from(forcing_name);
    root["model"]["control"]["output"]["dir"] = Value::from(output_dir);
    let control = root["model"]["control"].as_mapping_mut().unwrap();
    control.remove("start_time");
    control.remove("end_time");
    if let Some(s) = start_yaml {
        control.insert(Value::from("start_time"), serde_yaml::from_str(s).unwrap());
    }
    if let Some(e) = end_yaml {
        control.insert(Value::from("end_time"), serde_yaml::from_str(e).unwrap());
    }
    root
}

#[test]
fn file_loader_rejects_malformed_and_reversed_bounds_with_field_names() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("forcing.txt"), "").unwrap();
    let cfg_path = dir.path().join("resolved.yml");

    let bad_values = [
        "{value: null}",
        "{}",
        "{ref: citation}",
        "{other: '2012-01-01'}",
        "{value: '2012-01-01', extra: 1}",
        "{value: {value: '2012-01-01'}}",
        "20120101",
        "true",
        "[]",
        "''",
        "'   '",
        "'NaT'",
        "'spring'",
        "'2012/01/01'",
        "'2013-02-29'",
        "'2012-04-31'",
        "'2012-01-01 24:00'",
        "'2012-01-01 12:60'",
        "'2012-01-01T00:05:00Z'",
        "'2012-01-01T00:05:00+00:00'",
        "'2012-01-01 00:05:00-05:00'",
    ];

    for bad in bad_values {
        let root_start = config_with_bounds(Some(bad), None, "forcing.txt", ".");
        let text_start = serde_yaml::to_string(&root_start).unwrap();
        fs::write(&cfg_path, &text_start).unwrap();
        let err = load_run_config(&cfg_path).unwrap_err();
        assert!(
            err.contains("model.control.start_time"),
            "start={bad}: {err}"
        );
        // Direct-buffer readers preserve existing behaviour.
        assert!(load_run_config_from_str(&text_start)
            .unwrap()
            .run_period
            .is_unbounded());
        assert!(load_run_config_from_value(&mut root_start.clone())
            .unwrap()
            .run_period
            .is_unbounded());

        let root_end = config_with_bounds(None, Some(bad), "forcing.txt", ".");
        let text_end = serde_yaml::to_string(&root_end).unwrap();
        fs::write(&cfg_path, &text_end).unwrap();
        let err = load_run_config(&cfg_path).unwrap_err();
        assert!(err.contains("model.control.end_time"), "end={bad}: {err}");
    }

    for (start, end) in [
        ("'2012-01-02'", "'2012-01-01'"),
        ("'2012-01-02'", "'2012-01-02 00:00:00'"),
        ("'2012-01-02 00:05'", "'2012-01-02 00:00'"),
    ] {
        let root = config_with_bounds(Some(start), Some(end), "forcing.txt", ".");
        fs::write(&cfg_path, serde_yaml::to_string(&root).unwrap()).unwrap();
        let err = load_run_config(&cfg_path).unwrap_err();
        assert!(err.contains("model.control.start_time"), "{err}");
        assert!(err.contains("model.control.end_time"), "{err}");
    }
}

#[test]
fn direct_buffer_run_preserves_caller_chunk_independent_of_config_bounds() {
    // Config bounds span 2012-01-01..2012-12-31, yet direct-buffer callers
    // routinely pass chunks smaller than or outside those bounds.
    let make_chunk = |times: &[(i32, i32, i32, i32)]| {
        let mut block = vec![-999.0_f64; times.len() * FORCING_BLOCK_STRIDE];
        for (row, &(iy, id, it, imin)) in times.iter().enumerate() {
            let base = row * FORCING_BLOCK_STRIDE;
            block[base + TIME_COLUMNS.iy] = f64::from(iy);
            block[base + TIME_COLUMNS.id] = f64::from(id);
            block[base + TIME_COLUMNS.it] = f64::from(it);
            block[base + TIME_COLUMNS.imin] = f64::from(imin);
            block[base + SuewsField::u.index()] = 3.0;
            block[base + SuewsField::rh.index()] = 75.0;
            block[base + SuewsField::temp_c.index()] = 10.0;
            block[base + SuewsField::pres.index()] = 1013.0;
            block[base + SuewsField::rain.index()] = 0.0;
            block[base + SuewsField::kdown.index()] = 0.0;
        }
        block
    };

    let outside_chunk = [
        (2011, 365, 23, 55),
        (2012, 1, 0, 0),
        (2012, 1, 0, 5),
        (2012, 1, 0, 10),
    ];
    let (out, _state, _timer, len, _warnings) = run_from_config_str_and_forcing(
        SAMPLE_FIXTURE,
        make_chunk(&outside_chunk),
        outside_chunk.len(),
    )
    .expect("direct-buffer run must accept pre-selected chunk outside config bounds");
    assert_eq!(len, outside_chunk.len());
    for (row, &(iy, id, it, imin)) in outside_chunk.iter().enumerate() {
        let base = row * OUTPUT_ALL_COLS;
        assert_eq!(
            (
                out[base] as i32,
                out[base + 1] as i32,
                out[base + 2] as i32,
                out[base + 3] as i32
            ),
            (iy, id, it, imin)
        );
    }

    let sub_chunk = [(2012, 2, 10, 5), (2012, 2, 10, 10), (2012, 2, 10, 15)];
    let (out_sub, _state, _timer, len_sub, _warnings) =
        run_from_config_str_and_forcing(SAMPLE_FIXTURE, make_chunk(&sub_chunk), sub_chunk.len())
            .expect("direct-buffer run must accept short sub-interval chunk");
    assert_eq!(len_sub, sub_chunk.len());
    let base = 0;
    assert_eq!(
        (
            out_sub[base] as i32,
            out_sub[base + 1] as i32,
            out_sub[base + 2] as i32,
            out_sub[base + 3] as i32
        ),
        (2012, 2, 10, 5)
    );
}

#[cfg(all(feature = "physics", feature = "arrow-output"))]
mod cli {
    use super::*;
    use arrow::array::Float64Array;
    use arrow::ipc::reader::FileReader;
    use std::fs::File;
    use std::path::Path;
    use std::process::Command;
    use suews_bridge::{interpolate_forcing, read_forcing_block};

    const SAMPLE_FORCING_REL: &str = "../supy/sample_data/Kc_2012_data_60.txt";

    fn sample_forcing_lines() -> Vec<String> {
        let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(SAMPLE_FORCING_REL);
        fs::read_to_string(path)
            .expect("sample forcing file must exist")
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(ToOwned::to_owned)
            .collect()
    }

    fn write_forcing_slice(dest: &Path, lines: &[String], start_row_1based: usize, count: usize) {
        let mut out = String::new();
        out.push_str(&lines[0]);
        out.push('\n');
        for line in &lines[start_row_1based..start_row_1based + count] {
            out.push_str(line);
            out.push('\n');
        }
        fs::write(dest, out).unwrap();
    }

    fn run_engine(config_path: &Path) -> std::process::Output {
        Command::new(env!("CARGO_BIN_EXE_suews-engine"))
            .arg("run")
            .arg(config_path)
            .output()
            .expect("suews-engine binary must execute")
    }

    struct ArrowSeries {
        timestamps: Vec<(i32, i32, i32, i32)>,
        qh: Vec<f64>,
        qs: Vec<f64>,
        qn: Vec<f64>,
        qf: Vec<f64>,
    }

    fn read_arrow_series(path: &Path) -> ArrowSeries {
        let file = File::open(path).expect("arrow output must exist");
        let reader = FileReader::try_new(file, None).expect("valid arrow IPC file");
        let mut timestamps = Vec::new();
        let mut qh = Vec::new();
        let mut qs = Vec::new();
        let mut qn = Vec::new();
        let mut qf = Vec::new();

        for batch_res in reader {
            let batch = batch_res.expect("valid batch");
            let col = |name: &str| -> &Float64Array {
                let idx = batch.schema().index_of(name).expect(name);
                batch
                    .column(idx)
                    .as_any()
                    .downcast_ref::<Float64Array>()
                    .expect("f64 column")
            };
            let iy = col("iy");
            let id = col("id");
            let it = col("it");
            let imin = col("imin");
            let c_qh = col("QH");
            let c_qs = col("QS");
            let c_qn = col("QN");
            let c_qf = col("QF");
            for i in 0..batch.num_rows() {
                timestamps.push((
                    iy.value(i) as i32,
                    id.value(i) as i32,
                    it.value(i) as i32,
                    imin.value(i) as i32,
                ));
                qh.push(c_qh.value(i));
                qs.push(c_qs.value(i));
                qn.push(c_qn.value(i));
                qf.push(c_qf.value(i));
            }
        }
        ArrowSeries {
            timestamps,
            qh,
            qs,
            qn,
            qf,
        }
    }

    fn write_5min_forcing_from_hourly(hourly_path: &Path, dest: &Path) {
        let raw = read_forcing_block(hourly_path).unwrap();
        let interp = interpolate_forcing(&raw, 300).unwrap();
        let mut out = String::from(
            "iy id it imin qn qh qe qs qf U RH Tair pres rain kdown snow ldown fcld wuh sm lai kdiff kdir wdir\n",
        );
        let t = TIME_COLUMNS;
        for row in 0..interp.len_sim {
            let b = row * interp.stride;
            let iy = interp.block[b + t.iy] as i32;
            let id = interp.block[b + t.id] as i32;
            let it = interp.block[b + t.it] as i32;
            let imin = interp.block[b + t.imin] as i32;
            // Convert kernel pressure (hPa) back to file units (kPa).
            let pres_kpa = interp.block[b + SuewsField::pres.index()] / 10.0;
            out.push_str(&format!(
                "{iy} {id} {it} {imin} -999 -999 -999 -999 -999 {:.6} {:.6} {:.6} {:.6} {:.6} {:.6} -999 -999 -999 -999 -999 -999 -999 -999 -999\n",
                interp.block[b + SuewsField::u.index()],
                interp.block[b + SuewsField::rh.index()],
                interp.block[b + SuewsField::temp_c.index()],
                pres_kpa,
                interp.block[b + SuewsField::rain.index()],
                interp.block[b + SuewsField::kdown.index()],
            ));
        }
        fs::write(dest, out).unwrap();
    }

    struct ValidCase {
        label: &'static str,
        start: Option<&'static str>,
        end: Option<&'static str>,
        expected_rows: usize,
        first_ts: (i32, i32, i32, i32),
        last_ts: (i32, i32, i32, i32),
    }

    struct InvalidCase {
        label: &'static str,
        start: Option<&'static str>,
        end: Option<&'static str>,
        needles: &'static [&'static str],
    }

    #[test]
    fn cli_enforces_period_bounds_before_kernel_on_hourly_and_5min_forcing() {
        let dir = tempfile::tempdir().unwrap();
        let lines = sample_forcing_lines();
        let hourly_path = dir.path().join("weather_60.txt");
        write_forcing_slice(&hourly_path, &lines, 1, 48); // 2012-01-01 01:00 .. 2012-01-03 00:00
        let fivemin_path = dir.path().join("weather_5.txt");
        write_5min_forcing_from_hourly(&hourly_path, &fivemin_path);

        let cases = [
            ValidCase {
                label: "unbounded",
                start: Some("null"),
                end: Some("null"),
                expected_rows: 576,
                first_ts: (2012, 1, 0, 5),
                last_ts: (2012, 3, 0, 0),
            },
            ValidCase {
                label: "inside_late",
                start: Some("'2012-01-01 12:05'"),
                end: Some("'2012-01-02 12:00'"),
                expected_rows: 288,
                first_ts: (2012, 1, 12, 5),
                last_ts: (2012, 2, 12, 0),
            },
            ValidCase {
                label: "between_rows",
                start: Some("'2012-01-01 06:03'"),
                end: Some("'2012-01-01 18:02'"),
                expected_rows: 144,
                first_ts: (2012, 1, 6, 5),
                last_ts: (2012, 1, 18, 0),
            },
            ValidCase {
                label: "whole_day2",
                start: Some("'2012-01-02'"),
                end: Some("'2012-01-02'"),
                expected_rows: 288,
                first_ts: (2012, 2, 0, 5),
                last_ts: (2012, 3, 0, 0),
            },
            ValidCase {
                label: "start_only",
                start: Some("'2012-01-02 06:05'"),
                end: None,
                expected_rows: 216,
                first_ts: (2012, 2, 6, 5),
                last_ts: (2012, 3, 0, 0),
            },
            ValidCase {
                label: "end_only",
                start: None,
                end: Some("'2012-01-01 12:00'"),
                expected_rows: 144,
                first_ts: (2012, 1, 0, 5),
                last_ts: (2012, 1, 12, 0),
            },
            ValidCase {
                label: "single_row",
                start: Some("'2012-01-01 12:00'"),
                end: Some("'2012-01-01 12:00'"),
                expected_rows: 1,
                first_ts: (2012, 1, 12, 0),
                last_ts: (2012, 1, 12, 0),
            },
        ];

        for forcing_name in ["weather_60.txt", "weather_5.txt"] {
            let mut full_series: Option<ArrowSeries> = None;
            let mut late_series: Option<ArrowSeries> = None;

            for case in &cases {
                let out_dir = dir
                    .path()
                    .join(format!("out_{forcing_name}_{}", case.label));
                let cfg = config_with_bounds(
                    case.start,
                    case.end,
                    forcing_name,
                    out_dir.to_str().unwrap(),
                );
                let cfg_path = dir
                    .path()
                    .join(format!("cfg_{forcing_name}_{}.yml", case.label));
                fs::write(&cfg_path, serde_yaml::to_string(&cfg).unwrap()).unwrap();

                let output = run_engine(&cfg_path);
                assert!(
                    output.status.success(),
                    "{forcing_name}/{} failed: {}",
                    case.label,
                    String::from_utf8_lossy(&output.stderr)
                );
                let series = read_arrow_series(&out_dir.join("suews_output.arrow"));
                assert_eq!(
                    series.timestamps.len(),
                    case.expected_rows,
                    "{forcing_name}/{}",
                    case.label
                );
                assert_eq!(
                    series.timestamps[0], case.first_ts,
                    "{forcing_name}/{} first",
                    case.label
                );
                assert_eq!(
                    *series.timestamps.last().unwrap(),
                    case.last_ts,
                    "{forcing_name}/{} last",
                    case.label
                );
                if case.label == "unbounded" {
                    full_series = Some(series);
                } else if case.label == "inside_late" {
                    late_series = Some(series);
                }
            }

            // Negative control against post-run cropping: cold-starting at 12:05
            // produces meaningfully different fluxes than cropping the 00:05 run.
            let full = full_series.unwrap();
            let late = late_series.unwrap();
            let offset = full
                .timestamps
                .iter()
                .position(|&ts| ts == (2012, 1, 12, 5))
                .unwrap();
            assert_eq!(
                &full.timestamps[offset..offset + late.timestamps.len()],
                &late.timestamps[..]
            );
            let max_qh_diff = late
                .qh
                .iter()
                .enumerate()
                .map(|(i, &v)| (v - full.qh[offset + i]).abs())
                .fold(0.0_f64, f64::max);
            let max_qs_diff = late
                .qs
                .iter()
                .enumerate()
                .map(|(i, &v)| (v - full.qs[offset + i]).abs())
                .fold(0.0_f64, f64::max);
            assert!(
                max_qh_diff > 1.0 && max_qs_diff > 1.0,
                "{forcing_name}: expected pre-kernel evolution difference vs post-run crop, got dQH={max_qh_diff}, dQS={max_qs_diff}"
            );
        }
    }

    #[test]
    fn cli_enforces_leap_day_calendar_bounds() {
        let dir = tempfile::tempdir().unwrap();
        let lines = sample_forcing_lines();
        // 2012-02-28 01:00 is doy 59 hour 1 -> 0-based hour offset (58 * 24) = 1392 -> 1-based line 1393.
        let leap_start = lines
            .iter()
            .position(|l| {
                let mut cols = l.split_whitespace();
                cols.next() == Some("2012")
                    && cols.next() == Some("59")
                    && cols.next() == Some("1")
                    && cols.next() == Some("0")
            })
            .expect("2012-02-28 01:00 row must exist");
        let forcing_path = dir.path().join("leap_60.txt");
        write_forcing_slice(&forcing_path, &lines, leap_start, 72); // Feb 28, Feb 29, Mar 1

        let out_dir = dir.path().join("out_leap");
        let cfg = config_with_bounds(
            Some("'2012-02-29'"),
            Some("'2012-02-29'"),
            "leap_60.txt",
            out_dir.to_str().unwrap(),
        );
        let cfg_path = dir.path().join("leap.yml");
        fs::write(&cfg_path, serde_yaml::to_string(&cfg).unwrap()).unwrap();

        let output = run_engine(&cfg_path);
        assert!(
            output.status.success(),
            "leap-day run failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        let series = read_arrow_series(&out_dir.join("suews_output.arrow"));
        assert_eq!(series.timestamps.len(), 288);
        assert_eq!(series.timestamps[0], (2012, 60, 0, 5));
        assert_eq!(*series.timestamps.last().unwrap(), (2012, 61, 0, 0));
    }

    #[test]
    fn cli_keeps_period_on_forcing_clock_under_utc_with_nonzero_timezone() {
        let dir = tempfile::tempdir().unwrap();
        let lines = sample_forcing_lines();
        write_forcing_slice(&dir.path().join("weather_60.txt"), &lines, 1, 48);

        let mut results = Vec::new();
        for (tag, tz) in [("plus575", 5.75_f64), ("minus350", -3.5_f64)] {
            let out_dir = dir.path().join(format!("out_utc_{tag}"));
            let mut cfg = config_with_bounds(
                Some("'2012-01-01 06:05'"),
                Some("'2012-01-01 18:00'"),
                "weather_60.txt",
                out_dir.to_str().unwrap(),
            );
            cfg["model"]["control"]["forcing"]["timestamp_reference"] = Value::from("utc");
            cfg["sites"][0]["properties"]["timezone"] = Value::from(tz);
            let cfg_path = dir.path().join(format!("utc_{tag}.yml"));
            fs::write(&cfg_path, serde_yaml::to_string(&cfg).unwrap()).unwrap();

            let output = run_engine(&cfg_path);
            assert!(
                output.status.success(),
                "utc tz={tz} failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            let series = read_arrow_series(&out_dir.join("suews_output.arrow"));
            // Selection and output timestamps stay strictly on the UTC forcing clock.
            assert_eq!(series.timestamps.len(), 144);
            assert_eq!(series.timestamps[0], (2012, 1, 6, 5));
            assert_eq!(*series.timestamps.last().unwrap(), (2012, 1, 18, 0));
            results.push(series);
        }

        // Different timezones under UTC must shift local solar/diurnal physics.
        let max_qf_diff = results[0]
            .qf
            .iter()
            .zip(&results[1].qf)
            .map(|(a, b)| (a - b).abs())
            .fold(0.0_f64, f64::max);
        let max_qn_diff = results[0]
            .qn
            .iter()
            .zip(&results[1].qn)
            .map(|(a, b)| (a - b).abs())
            .fold(0.0_f64, f64::max);
        assert!(
            max_qf_diff > 0.1 || max_qn_diff > 0.1,
            "nonzero timezone under utc should shift solar/diurnal physics: dQF={max_qf_diff}, dQN={max_qn_diff}"
        );
    }

    #[test]
    fn cli_rejects_invalid_and_uncovered_periods_with_zero_output_created() {
        let dir = tempfile::tempdir().unwrap();
        let lines = sample_forcing_lines();
        write_forcing_slice(&dir.path().join("weather_60.txt"), &lines, 1, 48);

        let cases = [
            InvalidCase {
                label: "reversed",
                start: Some("'2012-01-02'"),
                end: Some("'2012-01-01'"),
                needles: &["model.control.start_time", "model.control.end_time"],
            },
            InvalidCase {
                label: "outside",
                start: Some("'2012-01-10'"),
                end: Some("'2012-01-11'"),
                needles: &[
                    "does not overlap",
                    "model.control.start_time",
                    "model.control.end_time",
                ],
            },
            InvalidCase {
                label: "between_adjacent_rows",
                start: Some("'2012-01-01 00:06'"),
                end: Some("'2012-01-01 00:09'"),
                needles: &[
                    "does not overlap",
                    "model.control.start_time",
                    "model.control.end_time",
                ],
            },
            InvalidCase {
                label: "partial_early_start",
                start: Some("'2011-12-31'"),
                end: Some("'2012-01-01'"),
                needles: &[
                    "does not cover",
                    "model.control.start_time",
                    "Timestamp convention",
                ],
            },
            InvalidCase {
                label: "partial_late_end",
                start: Some("'2012-01-01'"),
                end: Some("'2012-01-05'"),
                needles: &[
                    "does not cover",
                    "model.control.end_time",
                    "Timestamp convention",
                ],
            },
            InvalidCase {
                label: "bad_calendar",
                start: Some("'2013-02-29'"),
                end: None,
                needles: &["model.control.start_time"],
            },
            InvalidCase {
                label: "tz_offset",
                start: None,
                end: Some("'2012-01-01T12:00:00Z'"),
                needles: &["model.control.end_time", "timezone"],
            },
            InvalidCase {
                label: "malformed_type",
                start: Some("20120101"),
                end: None,
                needles: &["model.control.start_time"],
            },
        ];

        for case in &cases {
            let out_dir = dir.path().join(format!("should_not_exist_{}", case.label));
            let cfg = config_with_bounds(
                case.start,
                case.end,
                "weather_60.txt",
                out_dir.to_str().unwrap(),
            );
            let cfg_path = dir.path().join(format!("bad_{}.yml", case.label));
            fs::write(&cfg_path, serde_yaml::to_string(&cfg).unwrap()).unwrap();

            let output = run_engine(&cfg_path);
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                !output.status.success(),
                "{}: engine unexpectedly succeeded",
                case.label
            );
            for needle in case.needles {
                assert!(
                    stderr.contains(needle),
                    "{}: expected {needle:?} in stderr: {stderr}",
                    case.label
                );
            }
            assert!(
                !out_dir.exists(),
                "{}: failed validation must not create output directory {}",
                case.label,
                out_dir.display()
            );
        }
    }

    #[test]
    fn cli_validates_observed_qf_on_selected_period_only() {
        let dir = tempfile::tempdir().unwrap();
        let lines = sample_forcing_lines();
        let header: Vec<&str> = lines[0].split_whitespace().collect();
        let qf_idx = header
            .iter()
            .position(|c| c.eq_ignore_ascii_case("qf"))
            .expect("qf column");

        // Day 1 (first 24 hourly rows) + first hour of day 2 (knot at 00:30) has valid qf;
        // later day 2 hours carry -999 sentinel.
        let mut mutated = vec![lines[0].clone()];
        for (i, line) in lines[1..49].iter().enumerate() {
            let mut cols: Vec<&str> = line.split_whitespace().collect();
            cols[qf_idx] = if i < 25 { "25.0" } else { "-999" };
            mutated.push(cols.join(" "));
        }
        fs::write(dir.path().join("qf_split.txt"), mutated.join("\n") + "\n").unwrap();

        // Day 1 is fully valid -> succeeds.
        let out_ok = dir.path().join("out_qf_day1");
        let mut cfg_ok = config_with_bounds(
            Some("'2012-01-01'"),
            Some("'2012-01-01'"),
            "qf_split.txt",
            out_ok.to_str().unwrap(),
        );
        cfg_ok["model"]["physics"]["emissions"] = serde_yaml::from_str("{value: 0}").unwrap();
        let cfg_ok_path = dir.path().join("qf_day1.yml");
        fs::write(&cfg_ok_path, serde_yaml::to_string(&cfg_ok).unwrap()).unwrap();
        let res_ok = run_engine(&cfg_ok_path);
        assert!(
            res_ok.status.success(),
            "day 1 with valid qf should succeed: {}",
            String::from_utf8_lossy(&res_ok.stderr)
        );
        assert!(out_ok.join("suews_output.arrow").is_file());

        // Day 2 contains missing qf -> fails with no outputs written.
        let out_bad = dir.path().join("out_qf_day2");
        let mut cfg_bad = config_with_bounds(
            Some("'2012-01-02'"),
            Some("'2012-01-02'"),
            "qf_split.txt",
            out_bad.to_str().unwrap(),
        );
        cfg_bad["model"]["physics"]["emissions"] = serde_yaml::from_str("{value: 0}").unwrap();
        let cfg_bad_path = dir.path().join("qf_day2.yml");
        fs::write(&cfg_bad_path, serde_yaml::to_string(&cfg_bad).unwrap()).unwrap();
        let res_bad = run_engine(&cfg_bad_path);
        let stderr = String::from_utf8_lossy(&res_bad.stderr);
        assert!(!res_bad.status.success());
        assert!(stderr.contains("forcing column 'qf'"), "{stderr}");
        assert!(!out_bad.exists());
    }
}
