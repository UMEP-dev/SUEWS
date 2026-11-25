"""DailyState aligns with SUEWS under ``closed="right", label="right"``.

Verifies that DailyState and SUEWS share the same end-of-period row labels at
hourly, daily and monthly frequencies, and that text, non-streamed Parquet and
streamed Parquet writers all label day D's DailyState record as ``D+1 00:00``.
"""

import json
from pathlib import Path
import warnings

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSOutput, SUEWSSimulation
from supy.data_model.core.model import OutputControl, OutputFormat

pytestmark = [
    pytest.mark.api,
    pytest.mark.contract("C-RESAMPLE", "C-CLOCK", "C-SAVE"),
]


def _read_txt_timestamps(path: Path) -> pd.DatetimeIndex:
    """Reconstruct DatetimeIndex from a saved SUEWS text file's time columns."""
    df_file = pd.read_csv(path, sep=r"\s+")
    idx = (
        pd.to_datetime(df_file["Year"].astype(str), format="%Y")
        + pd.to_timedelta(df_file["DOY"] - 1, unit="D")
        + pd.to_timedelta(df_file["Hour"], unit="h")
        + pd.to_timedelta(df_file["Min"], unit="min")
    )
    return pd.DatetimeIndex(idx, name="datetime")


@pytest.fixture(scope="module")
def sample_three_day_output():
    """Three complete right-closed days: 2012-01-01 to 2012-01-03."""
    sim = SUEWSSimulation.from_sample_data()
    return sim.run(start_date="2012-01-01", end_date="2012-01-03")


def test_dailystate_aligns_with_suews_at_hourly_daily_and_monthly(
    sample_three_day_output,
):
    """Hourly, daily and monthly resamples align DailyState and SUEWS rows."""
    output = sample_three_day_output
    expected_days = pd.DatetimeIndex([
        "2012-01-02 00:00",
        "2012-01-03 00:00",
        "2012-01-04 00:00",
    ])

    # Hourly: day D's DailyState lands on D+1 00:00 alongside SUEWS (23:00, 00:00],
    # never on 23:00 alongside SUEWS (22:00, 23:00].
    hourly = output.resample("h").df.droplevel("grid")
    ds_h = hourly["DailyState", "P_day"].dropna()
    assert ds_h.index.equals(expected_days)
    assert hourly.loc[expected_days, ("SUEWS", "Rain")].notna().all()
    assert pd.isna(hourly.loc["2012-01-01 23:00", ("DailyState", "P_day")])
    assert pd.Timestamp("2012-01-01 00:00") not in hourly.index

    # Daily: SUEWS and DailyState have identical timestamps (no extra Jan 1 row).
    daily = output.resample("D").df.droplevel("grid")
    assert daily.index.equals(expected_days)
    assert daily["DailyState", "P_day"].notna().all()
    assert daily["SUEWS", "Rain"].notna().all()
    assert pd.Timestamp("2012-01-01 00:00") not in daily.index

    # Monthly: both groups occupy the single 2012-01-31 period row.
    monthly = output.resample("ME").df.droplevel("grid")
    expected_month = pd.DatetimeIndex(["2012-01-31"])
    assert monthly.index.equals(expected_month)
    assert monthly["DailyState", "P_day"].notna().all()
    assert monthly["SUEWS", "Rain"].notna().all()
    assert monthly.loc["2012-01-31", ("DailyState", "P_day")] == pytest.approx(
        daily["DailyState", "P_day"].sum()
    )


@pytest.mark.parametrize("freq_s", [3600, 86400])
def test_text_parquet_and_streamed_parquet_share_dailystate_labels(
    sample_three_day_output, tmp_path, freq_s
):
    """Text, unstreamed Parquet and streamed Parquet all label day D as D+1 00:00."""
    output = sample_three_day_output
    expected_days = pd.DatetimeIndex([
        "2012-01-02 00:00",
        "2012-01-03 00:00",
        "2012-01-04 00:00",
    ])

    # 1. Text writer
    txt_paths = output.save(
        tmp_path / "txt", format="txt", freq_s=freq_s, groups=["SUEWS", "DailyState"]
    )
    ds_txt_path = next(p for p in txt_paths if p.name.endswith("_DailyState.txt"))
    assert _read_txt_timestamps(ds_txt_path).equals(expected_days)
    txt_p_day = pd.read_csv(ds_txt_path, sep=r"\s+")["P_day"].to_numpy()

    # 2. Unstreamed Parquet writer
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", FutureWarning)
        pq_paths = output.save(tmp_path / "parquet", format="parquet", freq_s=freq_s)
    df_pq = pd.read_parquet(
        next(p for p in pq_paths if p.name.endswith("_output.parquet"))
    ).droplevel("grid")
    pq_ds = df_pq["DailyState", "P_day"].dropna()
    assert pq_ds.index.equals(expected_days)
    assert df_pq.loc[expected_days, ("SUEWS", "Rain")].notna().all()
    if freq_s == 86400:
        assert df_pq.index.equals(expected_days)

    # 3. Streamed Parquet writer (via SUEWSSimulation with stream_chunk=1)
    sim_stream = SUEWSSimulation.from_sample_data()
    sim_stream.config.model.control.output = OutputControl(
        format=OutputFormat.PARQUET,
        freq=freq_s,
        groups=["SUEWS", "DailyState"],
        stream_chunk=1,
        dir=str(tmp_path / "streamed"),
    )
    sim_stream.run(start_date="2012-01-01", end_date="2012-01-03")
    [stream_folder] = (tmp_path / "streamed").glob("*SUEWS_output*.parquet")
    df_stream = pd.read_parquet(stream_folder).sort_index().droplevel("grid")
    stream_ds = df_stream["DailyState", "P_day"].dropna()
    assert stream_ds.index.equals(expected_days)
    assert df_stream.index.equals(df_pq.index)
    assert df_stream.loc[expected_days, ("SUEWS", "Rain")].notna().all()

    # All three writers agree on DailyState values (text has 4 decimal places).
    np.testing.assert_allclose(stream_ds.to_numpy(), pq_ds.to_numpy(), rtol=0, atol=0)
    np.testing.assert_allclose(txt_p_day, pq_ds.to_numpy(), atol=1e-4)


def test_streamed_parquet_keeps_dailystate_columns_on_partial_day(tmp_path):
    """A streamed Parquet run ending before 23:55 retains NaN DailyState columns."""
    dir_stream = tmp_path / "partial"
    sim = SUEWSSimulation.from_sample_data()
    sim.config.model.control.output = OutputControl(
        format=OutputFormat.PARQUET,
        freq=3600,
        groups=["SUEWS", "DailyState"],
        stream_chunk=1,
        dir=str(dir_stream),
    )
    sim.run(start_date="2012-01-01 00:05", end_date="2012-01-01 01:00")
    [stream_folder] = dir_stream.glob("*SUEWS_output*.parquet")
    saved = pd.read_parquet(stream_folder).sort_index().droplevel("grid")
    assert saved.index.equals(pd.DatetimeIndex(["2012-01-01 01:00"]))
    assert ("DailyState", "P_day") in saved.columns
    assert saved["DailyState", "P_day"].isna().all()
    assert saved["SUEWS", "Rain"].notna().all()


@pytest.mark.parametrize(
    ("tz", "start", "end", "rule"),
    [
        ("UTC", "2012-01-01 00:05", "2012-01-05 00:00", "2D"),
        ("UTC", "2012-01-01 00:05", "2012-01-05 00:00", "48h"),
        ("Europe/London", "2012-03-24 00:05", "2012-03-28 00:00", "2D"),
    ],
)
def test_tz_aware_chained_multiday_resample_matches_direct(tz, start, end, rule):
    """Chained ``D`` -> multi-day resample matches direct resampling on tz-aware indexes."""
    idx = pd.date_range(start, end, freq="5min", tz=tz)
    mi = pd.MultiIndex.from_product([[1], idx], names=["grid", "datetime"])
    cols = pd.MultiIndex.from_tuples(
        [("SUEWS", "Rain"), ("DailyState", "P_day")], names=["group", "var"]
    )
    df = pd.DataFrame(np.nan, index=mi, columns=cols)
    df["SUEWS", "Rain"] = np.linspace(0.1, 1.0, len(idx))
    daily_marks = idx[(idx.hour == 23) & (idx.minute == 55)]
    for pos, mark in enumerate(daily_marks, start=1):
        df.loc[(1, mark), ("DailyState", "P_day")] = float(pos * 10)
    df.attrs["supy_output_daily_coverage_s"] = {
        f"1|{mark.isoformat()}": 86400.0 for mark in daily_marks
    }
    state = pd.DataFrame({("tstep", "0"): [300]}, index=[1])
    output = SUEWSOutput(df, state, metadata={"tstep_s": 300})

    with warnings.catch_warnings():
        warnings.simplefilter("error", RuntimeWarning)
        direct = output.resample(rule)
        chained = output.resample("D").resample(rule)

    pd.testing.assert_frame_equal(chained.df, direct.df)
    assert (
        chained.df.attrs["supy_output_daily_coverage_s"]
        == direct.df.attrs["supy_output_daily_coverage_s"]
    )


def test_native_timestep_parquet_keeps_kernel_stamp_while_text_is_daily(
    sample_three_day_output, tmp_path
):
    """Native-step Parquet preserves kernel 23:55 stamps; text DailyState is daily."""
    output = sample_three_day_output
    expected_native = pd.DatetimeIndex([
        "2012-01-01 23:55",
        "2012-01-02 23:55",
        "2012-01-03 23:55",
    ])
    expected_daily = pd.DatetimeIndex([
        "2012-01-02 00:00",
        "2012-01-03 00:00",
        "2012-01-04 00:00",
    ])

    # Unstreamed Parquet at native 300 s keeps kernel 23:55 DailyState rows.
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", FutureWarning)
        pq_paths = output.save(tmp_path / "pq300", format="parquet", freq_s=300)
    df_pq = pd.read_parquet(
        next(p for p in pq_paths if p.name.endswith("_output.parquet"))
    ).droplevel("grid")
    assert df_pq["DailyState", "P_day"].dropna().index.equals(expected_native)

    # Streamed Parquet at native 300 s agrees with unstreamed Parquet and output.df.
    sim = SUEWSSimulation.from_sample_data()
    sim.config.model.control.output = OutputControl(
        format=OutputFormat.PARQUET,
        freq=300,
        groups=["SUEWS", "DailyState"],
        stream_chunk=1,
        dir=str(tmp_path / "stream300"),
    )
    sim.run(start_date="2012-01-01", end_date="2012-01-03")
    [stream_folder] = (tmp_path / "stream300").glob("*SUEWS_output*.parquet")
    df_stream = pd.read_parquet(stream_folder).sort_index().droplevel("grid")
    assert df_stream["DailyState", "P_day"].dropna().index.equals(expected_native)

    # Text DailyState is always a daily (86400 s) file labelled D+1 00:00.
    txt_paths = output.save(
        tmp_path / "txt300", format="txt", freq_s=300, groups=["SUEWS", "DailyState"]
    )
    ds_txt = next(p for p in txt_paths if p.name.endswith("_DailyState.txt"))
    assert _read_txt_timestamps(ds_txt).equals(expected_daily)


def test_year_boundary_dailystate_text_file_and_coverage_sidecar(
    sample_three_day_output, tmp_path
):
    """31 Dec (labelled 1 Jan 00:00) lands in the prior year's file and sidecar."""
    # Shift the 3-day sample back by 1 day so it covers 2011-12-31 .. 2012-01-02.
    df_cross = sample_three_day_output.df.copy()
    shifted_dt = df_cross.index.get_level_values("datetime") - pd.Timedelta(days=1)
    df_cross.index = pd.MultiIndex.from_arrays(
        [df_cross.index.get_level_values("grid"), shifted_dt],
        names=["grid", "datetime"],
    )
    df_cross.attrs = {
        k: v
        for k, v in df_cross.attrs.items()
        if k != "supy_output_daily_coverage_s"
    }
    output_cross = SUEWSOutput(
        df_cross, sample_three_day_output.state_final, metadata={"tstep_s": 300}
    )

    paths = output_cross.save(
        tmp_path, format="txt", freq_s=3600, groups=["SUEWS", "DailyState"]
    )
    ds_paths = sorted(p for p in paths if p.name.endswith("_DailyState.txt"))
    assert [p.name for p in ds_paths] == [
        "1_2011_DailyState.txt",
        "1_2012_DailyState.txt",
    ]
    assert _read_txt_timestamps(ds_paths[0]).equals(
        pd.DatetimeIndex(["2012-01-01 00:00"])
    )
    assert _read_txt_timestamps(ds_paths[1]).equals(
        pd.DatetimeIndex(["2012-01-02 00:00", "2012-01-03 00:00"])
    )

    meta_2011 = json.loads(
        ds_paths[0].with_name(ds_paths[0].name + ".units.json").read_text(encoding="utf-8")
    )
    meta_2012 = json.loads(
        ds_paths[1].with_name(ds_paths[1].name + ".units.json").read_text(encoding="utf-8")
    )
    assert meta_2011["daily_coverage_s"] == {"1|2012-01-01T00:00:00": 86100.0}
    assert meta_2012["daily_coverage_s"] == {
        "1|2012-01-02T00:00:00": 86400.0,
        "1|2012-01-03T00:00:00": 86400.0,
    }


def test_gapped_dailystate_year_split_uses_daily_step_not_row_gap(
    sample_three_day_output, tmp_path
):
    """Missing days must not shift 2 Jan (day 1 Jan) into the previous year's file."""
    daily = sample_three_day_output.resample("D").df.loc[:, ["DailyState"]]
    # Keep 2 Jan (closing 1 Jan) and 4 Jan (closing 3 Jan), dropping 3 Jan.
    gapped = daily.loc[
        daily.index.get_level_values("datetime").isin([
            pd.Timestamp("2012-01-02 00:00"),
            pd.Timestamp("2012-01-04 00:00"),
        ])
    ].copy()
    gapped.attrs = daily.attrs.copy()
    output_gapped = SUEWSOutput(
        gapped, sample_three_day_output.state_final, metadata={"tstep_s": 86400}
    )

    paths = output_gapped.save(
        tmp_path, format="txt", freq_s=86400, groups=["DailyState"]
    )
    ds_paths = [p for p in paths if p.suffix == ".txt"]
    assert [p.name for p in ds_paths] == ["1_2012_DailyState.txt"]
    assert _read_txt_timestamps(ds_paths[0]).equals(
        pd.DatetimeIndex(["2012-01-02 00:00", "2012-01-04 00:00"])
    )
    meta = json.loads(
        ds_paths[0].with_name(ds_paths[0].name + ".units.json").read_text(encoding="utf-8")
    )
    assert list(meta["daily_coverage_s"].keys()) == [
        "1|2012-01-02T00:00:00",
        "1|2012-01-04T00:00:00",
    ]
