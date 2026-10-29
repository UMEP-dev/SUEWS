"""Observable output writer layout parity tests."""

from pathlib import Path
import warnings

import numpy as np
import pandas as pd
import pytest
import pyarrow.parquet as pq

from supy._save import StreamedParquetParts, save_df_grid_group, save_df_output_parquet

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


@pytest.mark.parametrize("streamed", [False, True])
def test_parquet_layout_and_null_round_trip(tmp_path: Path, streamed: bool):
    """Warn only for whole-file output and preserve the saved frame exactly."""
    # Parquet does not store an index freq, so the expected index has none;
    # pandas 3.1 warns that assert_index_equal will compare freq by default.
    index = pd.MultiIndex.from_product(
        [
            [7],
            pd.DatetimeIndex(
                pd.date_range("2012-01-01 00:05", periods=2, freq="5min"), freq=None
            ),
        ],
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

    writer = None
    if streamed:
        writer = StreamedParquetParts(
            tmp_path / "site_SUEWS_output.parquet", tstep_s=300
        )
        writer.prepare()
        writer.start_chunk(2012, 1)

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always", FutureWarning)
        paths = save_df_output_parquet(
            output,
            pd.DataFrame(),
            freq_s=300,
            site="site",
            path_dir_save=tmp_path,
            save_tstep=True,
            save_state=False,
            streamed_parquet=writer,
        )
        if streamed:
            paths = writer.finish()

    deprecations = [w for w in caught if issubclass(w.category, FutureWarning)]
    if streamed:
        assert not deprecations
        assert paths[0].is_dir()
    else:
        assert len(deprecations) == 1
        message = str(deprecations[0].message)
        assert "will be removed in the next release" in message
        assert "chunked streaming Parquet output" in message
        assert "model.control.output.stream_chunk" in message
        assert "SUEWSSimulation.run()" in message
        assert deprecations[0].filename == __file__
        assert paths[0].is_file()
    saved = pd.read_parquet(paths[0])
    arrow_table = pq.read_table(paths[0])

    assert [path.name for path in paths] == [
        "site_SUEWS_output.parquet",
        "site_SUEWS_metadata.parquet",
    ]
    assert saved.columns.to_list() == columns.to_list()
    assert saved.columns.names == ["group", "var"]
    pd.testing.assert_index_equal(saved.index, index)
    pd.testing.assert_frame_equal(saved, output, check_exact=True)
    assert pd.isna(saved.loc[(7, index.levels[1][1]), ("SUEWS", "QH")])
    assert arrow_table.column("('SUEWS', 'QH')").null_count == 1
