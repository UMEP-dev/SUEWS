"""DailyState aligns with SUEWS under ``closed="right", label="right"``.

Verifies that DailyState and SUEWS share the same end-of-period row labels at
hourly, daily and monthly frequencies, and that text, non-streamed Parquet and
streamed Parquet writers all label day D's DailyState record as ``D+1 00:00``.
"""

from pathlib import Path
import warnings

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation
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
