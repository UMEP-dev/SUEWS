"""Observable output writer layout parity tests."""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest
import pyarrow.parquet as pq

from supy._save import save_df_grid_group, save_df_output_parquet

pytestmark = pytest.mark.api


def test_text_layout_prefix_sentinel_and_filenames(tmp_path: Path):
    """Preserve text prefix order, missing sentinel, and group filenames."""
    index = pd.date_range("2012-01-01 00:05", periods=2, freq="5min")
    suews = pd.DataFrame(
        {"QH": [10.0, np.nan], "Rain": [0.0, 1.5]},
        index=index,
    )

    path_suews = save_df_grid_group(suews, 7, "SUEWS", tmp_path, "site")
    header, *rows = path_suews.read_text(encoding="utf-8").splitlines()

    assert path_suews.name == "site7_2012_SUEWS_5.txt"
    assert [field.strip() for field in header.split("\t")] == [
        "Year",
        "DOY",
        "Hour",
        "Min",
        "Dectime",
        "QH",
        "Rain",
    ]
    assert rows[1].split("\t")[5].strip() == "-999.0000"

    daily = pd.DataFrame({"HDD1_h": [2.0]}, index=index[:1])
    path_daily = save_df_grid_group(daily, 7, "DailyState", tmp_path, "site")
    assert path_daily.name == "site7_2012_DailyState.txt"


def test_parquet_layout_and_null_round_trip(tmp_path: Path):
    """Preserve ordered output identities and represent missing values as null."""
    index = pd.MultiIndex.from_product(
        [[7], pd.date_range("2012-01-01 00:05", periods=2, freq="5min")],
        names=["grid", "datetime"],
    )
    columns = pd.MultiIndex.from_tuples(
        [("SUEWS", "QH"), ("SUEWS", "Rain"), ("ESTM", "Ts")],
        names=["group", "var"],
    )
    output = pd.DataFrame(
        [[10.0, 0.0, 12.0], [np.nan, 1.5, 13.0]],
        index=index,
        columns=columns,
    )

    paths = save_df_output_parquet(
        output,
        pd.DataFrame(),
        site="site",
        path_dir_save=tmp_path,
        save_tstep=True,
        save_state=False,
    )
    saved = pd.read_parquet(paths[0])
    arrow_table = pq.read_table(paths[0])

    assert [path.name for path in paths] == [
        "site_SUEWS_output.parquet",
        "site_SUEWS_metadata.parquet",
    ]
    assert saved.columns.to_list() == columns.to_list()
    assert saved.columns.names == ["group", "var"]
    pd.testing.assert_index_equal(saved.index, index)
    assert pd.isna(saved.loc[(7, index.levels[1][1]), ("SUEWS", "QH")])
    assert arrow_table.column("('SUEWS', 'QH')").null_count == 1
