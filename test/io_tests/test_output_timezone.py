"""Tests for presentation-only output timestamp relabelling."""

from pathlib import Path

import pandas as pd
import pytest
import yaml

from supy._save import (
    relabel_output_timestamps,
    save_df_grid_group,
    save_df_output_parquet,
)
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.model import OutputTimestampReference
from supy.data_model.schema import CURRENT_SCHEMA_VERSION
from supy.util.converter.yaml_upgrade import upgrade_yaml

pytestmark = pytest.mark.api


def _state_for_grid(rows):
    rows = [row.copy() for row in rows]
    grids = [row.pop("grid") for row in rows]
    df = pd.DataFrame(rows, index=pd.Index(grids, name="grid"))
    df.columns = pd.MultiIndex.from_tuples(
        [(name, "0") for name in df.columns], names=["var", "ind_dim"]
    )
    return df


def _output_for_grid(grid, dates):
    index = pd.MultiIndex.from_product(
        [[grid], pd.DatetimeIndex(dates)], names=["grid", "datetime"]
    )
    columns = pd.MultiIndex.from_tuples([("SUEWS", "QN")], names=["group", "var"])
    return pd.DataFrame(range(len(index)), index=index, columns=columns)


def test_output_timestamp_reference_defaults_to_follow():
    config = SUEWSConfig(sites=[{}])
    assert config.model.control.output.timestamp_reference == OutputTimestampReference.FOLLOW


def test_yaml_output_timestamp_reference_loads():
    config = SUEWSConfig(
        sites=[{}],
        model={"control": {"output": {"timestamp_reference": "utc"}}},
    )
    assert config.model.control.output.timestamp_reference == OutputTimestampReference.UTC


def test_daylight_requires_dls_window():
    with pytest.raises(ValueError, match="timestamp_reference='daylight' requires"):
        SUEWSConfig(
            sites=[{}],
            model={"control": {"output": {"timestamp_reference": "daylight"}}},
        )


def test_standard_zero_offset_warns():
    with pytest.warns(UserWarning, match="zero UTC offset"):
        SUEWSConfig(
            sites=[{}],
            model={"control": {"output": {"timestamp_reference": "standard"}}},
        )


def test_follow_returns_byte_identity_input():
    df = _output_for_grid(1, pd.date_range("2020-01-01", periods=2, freq="h"))
    state = _state_for_grid([
        {"grid": 1, "timezone": 5.5, "startdls": 80, "enddls": 300}
    ])

    relabelled = relabel_output_timestamps(df, "follow", state)

    assert relabelled is df


def test_utc_relabels_fractional_offset():
    dates = pd.date_range("2020-01-01 12:00", periods=2, freq="h")
    df = _output_for_grid(1, dates)
    state = _state_for_grid([
        {"grid": 1, "timezone": 5.5, "startdls": 80, "enddls": 300}
    ])

    relabelled = relabel_output_timestamps(df, "utc", state)

    got = relabelled.index.get_level_values("datetime")
    assert list(got) == list(dates - pd.Timedelta(hours=5.5))


def test_daylight_relabels_inside_northern_window_only():
    dates = pd.to_datetime(["2020-01-15 12:00", "2020-04-01 12:00"])
    df = _output_for_grid(1, dates)
    state = _state_for_grid([{"grid": 1, "timezone": 0, "startdls": 80, "enddls": 300}])

    relabelled = relabel_output_timestamps(df, "daylight", state)

    got = relabelled.index.get_level_values("datetime")
    assert list(got) == [dates[0], dates[1] + pd.Timedelta(hours=1)]


def test_daylight_relabels_southern_hemisphere_wrap():
    dates = pd.to_datetime(["2020-01-15 12:00", "2020-04-01 12:00", "2020-12-01 12:00"])
    df = _output_for_grid(1, dates)
    state = _state_for_grid([{"grid": 1, "timezone": 0, "startdls": 300, "enddls": 80}])

    relabelled = relabel_output_timestamps(df, "daylight", state)

    got = relabelled.index.get_level_values("datetime")
    assert list(got) == [
        dates[0] + pd.Timedelta(hours=1),
        dates[1],
        dates[2] + pd.Timedelta(hours=1),
    ]


def test_relabels_each_grid_with_own_offset():
    df = pd.concat([
        _output_for_grid(1, ["2020-01-01 12:00"]),
        _output_for_grid(2, ["2020-01-01 12:00"]),
    ])
    state = _state_for_grid([
        {"grid": 1, "timezone": 5.5, "startdls": 80, "enddls": 300},
        {"grid": 2, "timezone": -3.75, "startdls": 80, "enddls": 300},
    ])

    relabelled = relabel_output_timestamps(df, "utc", state)

    got = relabelled.index.get_level_values("datetime")
    assert list(got) == [
        pd.Timestamp("2020-01-01 06:30"),
        pd.Timestamp("2020-01-01 15:45"),
    ]


def test_text_filename_marks_explicit_timestamp_reference(tmp_path: Path):
    df_year = pd.DataFrame(
        {"QN": [1.0, 2.0]},
        index=pd.date_range("2020-01-01 12:00", periods=2, freq="h"),
    )

    path = save_df_grid_group(
        df_year,
        grid=1,
        group="SUEWS",
        dir_save=tmp_path,
        site="",
        timestamp_reference="utc",
    )

    assert path.name == "1_2020_SUEWS_60_UTC.txt"


def test_parquet_uses_same_relabelled_index(tmp_path: Path):
    pytest.importorskip("pyarrow")
    dates = pd.date_range("2020-01-01 12:00", periods=2, freq="h")
    df = _output_for_grid(1, dates)
    state = _state_for_grid([{"grid": 1, "timezone": 1, "startdls": 80, "enddls": 300}])

    paths = save_df_output_parquet(
        df,
        state,
        path_dir_save=tmp_path,
        save_state=False,
        timestamp_reference="utc",
    )

    output_path = next(
        path for path in paths if path.name == "SUEWS_output_UTC.parquet"
    )
    saved = pd.read_parquet(output_path)
    expected = relabel_output_timestamps(df, "utc", state)
    pd.testing.assert_index_equal(saved.index, expected.index)


def test_parquet_bundle_marks_explicit_timestamp_reference(tmp_path: Path):
    pytest.importorskip("pyarrow")
    dates = pd.date_range("2020-01-01 12:00", periods=2, freq="h")
    df = _output_for_grid(1, dates)
    state = _state_for_grid([{"grid": 1, "timezone": 1, "startdls": 80, "enddls": 300}])

    paths = save_df_output_parquet(
        df,
        state,
        path_dir_save=tmp_path,
        timestamp_reference="daylight",
    )

    assert {path.name for path in paths} == {
        "SUEWS_output_DAYLIGHT.parquet",
        "SUEWS_state_final_DAYLIGHT.parquet",
        "SUEWS_metadata_DAYLIGHT.parquet",
    }
    metadata = pd.read_parquet(tmp_path / "SUEWS_metadata_DAYLIGHT.parquet")
    assert metadata.loc[0, "timestamp_reference"] == "daylight"


def test_dev1_schema_migrates_to_current_with_default_timestamp_reference(tmp_path: Path):
    source = tmp_path / "dev1.yml"
    target = tmp_path / "current.yml"
    source.write_text(
        """
schema_version: '2026.6.dev1'
model:
  control:
    output:
      format: txt
sites:
  - {}
""",
        encoding="utf-8",
    )

    upgrade_yaml(source, target)

    payload = yaml.safe_load(target.read_text(encoding="utf-8"))
    assert payload["schema_version"] == CURRENT_SCHEMA_VERSION
    assert "timestamp_reference" not in payload["model"]["control"]["output"]
    config = SUEWSConfig(**payload)
    assert config.model.control.output.timestamp_reference == OutputTimestampReference.FOLLOW
