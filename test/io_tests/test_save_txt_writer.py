"""The block-wise text writer reproduces the former pandas writer byte for byte.

The text saver used to format every cell of a grid-group-year table to a
Python ``str`` and hand the table to ``DataFrame.to_csv``. It now formats and
writes the table a block of rows at a time (gh#1849). ``_reference_text``
below is a frozen copy of the former path, so these tests pin that the saved
bytes did not change.
"""

import os
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from supy import _save  # noqa: PLC2701
from supy._save import _write_df_txt, save_df_grid_group  # noqa: PLC2701

pytestmark = pytest.mark.api


def _reference_text(df_grid_group: pd.DataFrame, path_out: Path) -> bytes:
    """Write ``df_grid_group`` the former way and return the bytes."""
    idx_dt = df_grid_group.index
    ser_year = pd.Series(idx_dt.year, index=idx_dt, name="Year")
    ser_DOY = pd.Series(idx_dt.dayofyear, index=idx_dt, name="DOY")
    ser_hour = pd.Series(idx_dt.hour, index=idx_dt, name="Hour")
    ser_min = pd.Series(idx_dt.minute, index=idx_dt, name="Min")
    df_datetime = pd.concat([ser_year, ser_DOY, ser_hour, ser_min], axis=1)
    dt_delta = idx_dt - idx_dt.to_period("D").to_timestamp()
    df_datetime["Dectime"] = ser_DOY - 1 + dt_delta.total_seconds() / (24 * 60 * 60)
    df_save = pd.concat([df_datetime, df_grid_group], axis=1)

    for var in df_save.columns[:4]:
        width_var_name = max([3, len(var)])
        df_save[var] = df_save[var].map(
            lambda s, n=width_var_name: "{s:{c}>{n}}".format(s=s, n=n, c=" ")
        )
    df_save.Dectime = df_save.Dectime.map(
        lambda s: "{s:{c}>{n}.4f}".format(s=s, n=8, c=" ")
    )
    df_save = df_save.fillna(-999.0)
    for var in df_save.columns[5:]:
        width_var_name = max([8, len(var)])
        df_save[var] = df_save[var].map(lambda s, n=width_var_name: f"{s:{' '}>{n}.4f}")
    col_fmt = df_save.columns.to_series()
    col_fmt[4:] = col_fmt[4:].map(
        lambda s: "{s:{c}>{n}}".format(s=s, n=max([8, len(s)]), c=" ")
    )
    df_save.columns = col_fmt
    df_save.to_csv(path_out, index=False, sep="\t")
    return path_out.read_bytes()


def _written_text(df_grid_group: pd.DataFrame, path_out: Path) -> bytes:
    _write_df_txt(df_grid_group, path_out)
    return path_out.read_bytes()


def _awkward_values(n_row: int) -> np.ndarray:
    """Values that stress fixed-point formatting and missing-value filling."""
    rng = np.random.default_rng(1849)
    ar_value = rng.normal(scale=300.0, size=(n_row, 6))
    special = [
        np.nan,
        -0.0,
        0.0,
        np.inf,
        -np.inf,
        1e20,
        -1e-9,
        0.00005,
        0.00015,
        2.5e-5,
        -999.0,
        123456789.12345,
    ]
    ar_value.flat[: min(len(special), ar_value.size)] = special[: ar_value.size]
    if n_row > 3:
        ar_value[3, :] = np.nan  # a row with no data at all
    return ar_value


@pytest.mark.parametrize(
    ("freq", "n_row"),
    [("5min", 400), ("h", 50), ("D", 3), ("5min", 1)],
)
def test_writer_matches_former_pandas_writer(tmp_path: Path, freq, n_row):
    """Float tables, with NaN, signed zero, infinities and rounding ties."""
    index = pd.date_range("2012-12-31 22:05", periods=n_row, freq=freq)
    df_year = pd.DataFrame(
        _awkward_values(n_row),
        index=index,
        columns=["QN", "QH", "LongVariableName", "T2", "x", "AlbedoDirect"],
    )

    expected = _reference_text(df_year.copy(), tmp_path / "expected.txt")
    written = _written_text(df_year, tmp_path / "written.txt")

    assert written == expected


def test_writer_matches_for_integer_columns(tmp_path: Path):
    """Integer tables are written as their float values."""
    index = pd.date_range("2012-01-01 00:05", periods=30, freq="5min")
    df_year = pd.DataFrame(
        {"QN": range(30), "Neg": range(-15, 15)}, index=index, dtype="int64"
    )

    expected = _reference_text(df_year.copy(), tmp_path / "expected.txt")
    written = _written_text(df_year, tmp_path / "written.txt")

    assert written == expected


def test_writer_matches_without_value_columns(tmp_path: Path):
    """A table with no value columns keeps only the date columns."""
    index = pd.date_range("2012-01-01 00:05", periods=4, freq="h")
    df_year = pd.DataFrame(index=index)

    expected = _reference_text(df_year.copy(), tmp_path / "expected.txt")
    written = _written_text(df_year, tmp_path / "written.txt")

    assert written == expected


def test_writer_blocks_do_not_change_bytes(tmp_path: Path, monkeypatch):
    """Rows split across many write blocks give the same bytes as one block."""
    index = pd.date_range("2012-06-01 00:05", periods=97, freq="5min")
    df_year = pd.DataFrame(_awkward_values(97), index=index).add_prefix("v")

    one_block = _written_text(df_year, tmp_path / "one.txt")
    monkeypatch.setattr(_save, "_CHUNK_CELLS", 7)
    many_blocks = _written_text(df_year, tmp_path / "many.txt")

    assert many_blocks == one_block
    assert one_block.count(os.linesep.encode()) == 98


def test_save_df_grid_group_matches_former_writer(tmp_path: Path):
    """The saved file of a grid-group-year table is unchanged."""
    index = pd.date_range("2012-01-01 00:05", periods=288, freq="5min")
    df_year = pd.DataFrame(_awkward_values(288), index=index, columns=list("ABCDEF"))

    path_save = save_df_grid_group(df_year, 3, "SUEWS", tmp_path, "site")
    expected = _reference_text(df_year.asfreq("5min"), tmp_path / "expected.txt")

    assert path_save.name == "site3_2012_SUEWS_5.txt"
    assert path_save.read_bytes() == expected
