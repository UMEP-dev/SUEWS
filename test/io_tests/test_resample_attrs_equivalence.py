"""Equivalence and attrs-isolation tests for attrs-stripped resampling and saving.

Verifies that stripping ``DataFrame.attrs`` during internal resampling,
aggregation, unit conversion and save slicing produces frames and ``attrs``
identical to the original attrs-carrying algorithm, while keeping ``attrs``
independently owned across returned frames.
"""

import copy
import json

from conftest import TIMESTEPS_PER_DAY
import pandas as pd
import pytest

from supy._output_metadata import (
    output_covered_seconds,
    prepare_output,
    resolve_held_interval,
    water_flux_variables,
    with_daily_coverage,
)
from supy._post import (
    _aggregate_bins,
    _calendar_binning,
    _dailystate_period_end,
    _index_freq_matches,
    _is_calendar_frequency,
    _resample_output,
    dict_var_aggm,
)
from supy._save import (
    StreamedParquetParts,
    save_df_output,
    save_df_output_parquet,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.contract("C-RESAMPLE", "C-ISOLATION", "C-SAVE"),
]

DAYS_SAMPLE = 3
TSTEP_S = 300


def _reference_resample_output_with_attrs(
    df_output,
    freq="60min",
    dict_aggm=dict_var_aggm,
    groups=None,
    freq_source=None,
    drop_empty_rows=True,
):
    """Reference copy of the original attrs-carrying ``_resample_output`` path."""
    if hasattr(df_output, "_df_output"):
        held = df_output._held_interval(freq_source)
        df_output = df_output._df_output
    else:
        held = resolve_held_interval(df_output, freq_source)

    if held.kind == "fixed":
        try:
            target_s = pd.tseries.frequencies.to_offset(freq).nanos / 1e9
        except ValueError:
            target_s = None
        if target_s == held.seconds:
            return df_output
    elif held.kind == "unknown" and _index_freq_matches(df_output, freq):
        return df_output

    source_s = held.seconds if held.kind == "fixed" else None
    df_output = with_daily_coverage(df_output, source_s)
    dict_daily_coverage = {}
    dict_units = df_output.attrs.get("supy_output_units", {})
    water_columns = [c for c in df_output.columns if c in water_flux_variables()]
    rate_columns = [c for c in water_columns if c[1] in dict_units.get(c[0], {})]
    ser_coverage = (
        output_covered_seconds(df_output, source_s) if water_columns else None
    )
    dict_coverage = {}

    def _resample_group(
        df_group, freq, label, dict_aggm_group, group_name=None, grid=None
    ):
        if group_name == "DailyState":
            df_to_resample = df_group.dropna(how="all")
            if df_to_resample.empty:
                return pd.DataFrame(
                    index=pd.DatetimeIndex([]), columns=df_group.columns
                )
            coverage = df_output.attrs.get("supy_output_daily_coverage_s") or {}
            ser_daily_seconds = pd.Series(
                [
                    coverage.get(f"{grid}|{date.isoformat()}", float("nan"))
                    for date in df_to_resample.index
                ],
                index=df_to_resample.index,
            )
            ser_daily_seconds = _dailystate_period_end(ser_daily_seconds, source_s)
            df_to_resample = _dailystate_period_end(df_to_resample, source_s)
            daily_seconds = (
                _calendar_binning(ser_daily_seconds, freq)
                .resample(freq, closed="right", label=label)
                .sum(min_count=1)
            )
            dict_daily_coverage.update({
                f"{grid}|{date.isoformat()}": value
                for date, value in daily_seconds.dropna().items()
            })
            return _aggregate_bins(
                _calendar_binning(df_to_resample, freq).resample(
                    freq, closed="right", label=label
                ),
                dict_aggm_group,
                df_to_resample.columns,
            )
        else:
            df_to_resample = df_group

        columns_rate = [name for group, name in rate_columns if group == group_name]
        covered = None
        if ser_coverage is not None:
            covered = ser_coverage.xs(grid, level="grid")
            target = (
                _calendar_binning(covered, freq)
                .resample(freq, closed="right", label=label)
                .sum()
            )
            dict_coverage.update({
                f"{grid}|{date.isoformat()}": value for date, value in target.items()
            })
        if columns_rate:
            df_to_resample = df_to_resample.copy(deep=False)
            df_to_resample[columns_rate] = df_to_resample[columns_rate].mul(
                covered, axis=0
            )
        bins = _calendar_binning(df_to_resample, freq).resample(
            freq, closed="right", label=label
        )
        result = _aggregate_bins(bins, dict_aggm_group, df_to_resample.columns)
        if columns_rate:
            result[columns_rate] = result[columns_rate].div(
                target.where(target > 0), axis=0
            )
        occupied = bins.size().loc[lambda counts: counts > 0].index
        if len(occupied):
            result = result.loc[occupied[0] : occupied[-1]]
        return result

    calendar = _is_calendar_frequency(freq)
    list_grid = df_output.index.get_level_values("grid").unique()
    list_group = df_output.columns.get_level_values("group").unique()
    if groups is not None:
        list_group = [group for group in list_group if group in groups]

    df_rsmp = pd.concat(
        {
            grid: pd.concat(
                {
                    group: _resample_group(
                        df_output[group].xs(grid, level="grid"),
                        freq,
                        "left" if group == "DailyState" and not calendar else "right",
                        dict_aggm[group],
                        group_name=group,
                        grid=grid,
                    )
                    for group in list_group
                    if group in dict_aggm
                },
                axis=1,
                names=["group", "var"],
            )
            for grid in list_grid
        },
        names=["grid"],
    )

    if drop_empty_rows:
        df_rsmp = df_rsmp.dropna(how="all", axis=0)

    if isinstance(df_rsmp.index, pd.MultiIndex):
        if df_rsmp.index.names[1] is None:
            df_rsmp.index = df_rsmp.index.set_names(["grid", "datetime"])

    df_rsmp.attrs = df_output.attrs.copy()
    try:
        interval_s = pd.tseries.frequencies.to_offset(freq).nanos / 1e9
    except ValueError:
        interval_s = None
    df_rsmp.attrs["supy_output_interval_s"] = interval_s
    if dict_daily_coverage:
        df_rsmp.attrs["supy_output_daily_coverage_s"] = dict_daily_coverage
    elif "DailyState" in df_rsmp.columns.get_level_values("group"):
        df_rsmp.attrs["supy_output_daily_coverage_s"] = df_output.attrs.get(
            "supy_output_daily_coverage_s"
        )
    if dict_coverage:
        dict_actual = {
            f"{grid}|{date.isoformat()}": dict_coverage.get(
                f"{grid}|{date.isoformat()}", 0.0
            )
            for grid, date in df_rsmp.index
        }
        df_rsmp.attrs["supy_output_coverage_s"] = {
            key: value for key, value in dict_actual.items() if value != interval_s
        }
    return df_rsmp


def _run_short_sample(short_sample_sim, list_gridiv=None):
    """Run a 3-day slice of the short sample for 1 or 2 grids."""
    sim = short_sample_sim()
    forcing = sim.forcing.df.iloc[: DAYS_SAMPLE * TIMESTEPS_PER_DAY].copy()
    update = {
        "model": {
            "control": {
                "start_time": str(forcing.index[0]),
                "end_time": str(forcing.index[-1]),
            }
        }
    }
    if list_gridiv is not None:
        config = sim.config.model_copy(deep=True)
        site0 = config.sites[0]
        sites = []
        for pos, gridiv in enumerate(list_gridiv):
            site = site0.model_copy(deep=True)
            site.gridiv = gridiv
            site.name = f"grid{gridiv}"
            site.properties.lat.value += 10.0 * pos
            sites.append(site)
        update["sites"] = [
            site.model_dump(exclude_unset=True, round_trip=True, mode="json")
            for site in sites
        ]
    sim.update_config(update, auto_load_forcing=False)
    sim.update_forcing(forcing)
    return sim.run()


@pytest.fixture(scope="module")
def sample_outputs(short_sample_sim):
    """Module-scoped 1-grid and 2-grid 3-day sample outputs."""
    return {
        "1grid": _run_short_sample(short_sample_sim, list_gridiv=None),
        "2grids": _run_short_sample(short_sample_sim, list_gridiv=[2, 1]),
    }


@pytest.mark.parametrize("case", ["1grid", "2grids"])
@pytest.mark.parametrize("with_units", [False, True])
@pytest.mark.parametrize(
    "freq",
    [pd.Timedelta(TSTEP_S, "s"), pd.Timedelta(3600, "s"), "D"],
    ids=["300s", "3600s", "daily"],
)
def test_resample_matches_attrs_carrying_reference(
    sample_outputs, case, with_units, freq
):
    """Stripped resampling matches the attrs-carrying reference bit for bit."""
    output = sample_outputs[case]
    source = output.to_units({"Rain": "mm h-1"}) if with_units else output
    df_in = source.df.copy()
    df_in.attrs["custom_meta"] = {"case": case, "items": [1, 2]}
    attrs_before = copy.deepcopy(df_in.attrs)

    actual = _resample_output(df_in, freq=freq)
    expected = _reference_resample_output_with_attrs(df_in, freq=freq)

    pd.testing.assert_frame_equal(actual, expected, check_exact=True)
    assert actual.attrs == expected.attrs
    assert df_in.attrs == attrs_before
    if freq == pd.Timedelta(TSTEP_S, "s"):
        assert actual is df_in


@pytest.mark.parametrize("case", ["1grid", "2grids"])
def test_resample_output_attrs_are_independent(sample_outputs, case):
    """Mutating one resampled frame's attrs never affects another or its source."""
    output = sample_outputs[case].to_units({"Rain": "mm h-1"})
    df_in = output.df.copy()
    df_in.attrs["custom_meta"] = {"nested": ["a", "b"]}
    source_snapshot = copy.deepcopy(df_in.attrs)

    rsmp_a = _resample_output(df_in, freq=pd.Timedelta(3600, "s"))
    rsmp_b = _resample_output(df_in, freq=pd.Timedelta(3600, "s"))
    snapshot_b = copy.deepcopy(rsmp_b.attrs)

    assert rsmp_a.attrs == rsmp_b.attrs
    assert rsmp_a.attrs is not rsmp_b.attrs
    assert rsmp_a.attrs is not df_in.attrs

    # Mutate every nested mutable container in rsmp_a.attrs in place.
    rsmp_a.attrs["custom_meta"]["nested"].append("mutated")
    rsmp_a.attrs["supy_output_units"]["SUEWS"]["Rain"] = "m s-1"
    rsmp_a.attrs["supy_water_rate_columns"].append(("SUEWS", "fake"))
    rsmp_a.attrs["supy_output_daily_coverage_s"]["extra"] = 1.0
    rsmp_a.attrs["supy_output_coverage_s"]["extra"] = 2.0

    assert rsmp_b.attrs == snapshot_b
    assert df_in.attrs == source_snapshot

    # Verify the same isolation through SUEWSOutput.resample().
    out_a = output.resample("h")
    out_b = output.resample("h")
    df_out_a = out_a.df
    df_out_b = out_b.df
    out_b_snapshot = copy.deepcopy(df_out_b.attrs)
    df_out_a.attrs["supy_output_units"]["SUEWS"]["Rain"] = "m s-1"
    df_out_a.attrs["supy_output_daily_coverage_s"]["extra"] = 9.0

    assert out_b.df.attrs == out_b_snapshot
    assert output.df.attrs["supy_output_units"]["SUEWS"]["Rain"] == "mm h-1"


def test_prepare_output_and_save_preserve_attrs_without_aliasing(
    sample_outputs, tmp_path
):
    """Unit preparation and text/Parquet saves keep attrs accurate and isolated."""
    output = sample_outputs["2grids"]
    df_in = output.df.copy()
    df_in.attrs["custom_meta"] = {"tags": ["x"]}
    attrs_before = copy.deepcopy(df_in.attrs)

    prep_a = prepare_output(df_in, units={"Rain": "mm h-1"}, interval_s=TSTEP_S)
    prep_b = prepare_output(df_in, units={"Rain": "mm h-1"}, interval_s=TSTEP_S)
    snapshot_b = copy.deepcopy(prep_b.attrs)

    pd.testing.assert_frame_equal(prep_a, prep_b, check_exact=True)
    assert prep_a.attrs == prep_b.attrs
    assert prep_a.attrs is not prep_b.attrs
    prep_a.attrs["custom_meta"]["tags"].append("mutated")
    prep_a.attrs["supy_output_units"]["SUEWS"]["Rain"] = "m s-1"
    prep_a.attrs["supy_output_daily_coverage_s"]["extra"] = 1.0
    assert prep_b.attrs == snapshot_b
    assert df_in.attrs == attrs_before

    # Text save path (multi-grid, DailyState + SUEWS at 3600 s).
    dir_txt = tmp_path / "txt"
    dir_txt.mkdir()
    txt_paths = save_df_output(
        df_in,
        freq_s=3600,
        path_dir_save=dir_txt,
        output_groups=["SUEWS", "DailyState"],
        df_state_final=output.state_final,
    )
    assert df_in.attrs == attrs_before
    daily_sidecars = sorted(dir_txt.glob("*DailyState*.units.json"))
    assert len(daily_sidecars) == 2
    for sidecar in daily_sidecars:
        payload = json.loads(sidecar.read_text(encoding="utf-8"))
        grid_prefix = sidecar.name.split("_", 1)[0]
        expected_grid_coverage = {
            key: val
            for key, val in df_in.attrs["supy_output_daily_coverage_s"].items()
            if key.startswith(f"{grid_prefix}|")
        }
        assert payload["daily_coverage_s"]
        assert payload["daily_coverage_s"] == expected_grid_coverage
    assert any(p.name.endswith("_SUEWS_60.txt") for p in txt_paths)

    # Streamed Parquet chunk path (exercises _parquet_chunk_frame).
    dir_pq = tmp_path / "pq"
    dir_pq.mkdir()
    stream = StreamedParquetParts(dir_pq / "SUEWS_output.parquet", tstep_s=TSTEP_S)
    stream.prepare()
    stream.start_chunk(2012, 1)
    save_df_output_parquet(
        df_in,
        output.state_final,
        freq_s=3600,
        path_dir_save=dir_pq,
        save_state=False,
        streamed_parquet=stream,
    )
    pq_paths = stream.finish()
    reloaded = pd.read_parquet(pq_paths[0])
    assert reloaded.attrs["supy_output_interval_s"] == 3600
    assert (
        reloaded.attrs["supy_output_daily_coverage_s"]
        == df_in.attrs["supy_output_daily_coverage_s"]
    )
    assert df_in.attrs == attrs_before
