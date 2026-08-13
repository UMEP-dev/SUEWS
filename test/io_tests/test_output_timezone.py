"""Tests for presentation-only output timestamp relabelling."""

from pathlib import Path

import pandas as pd
import pytest

from supy._save import (
    df_var_out,
    relabel_output_timestamps,
    save_df_grid_group,
    save_df_output,
    save_df_output_parquet,
)
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.model import OutputTimestampReference
from supy.suews_output import SUEWSOutput

pytestmark = pytest.mark.api


def _state_for_grid(rows):
    rows = [row.copy() for row in rows]
    grids = [row.pop("grid") for row in rows]
    df_state = pd.DataFrame(rows, index=pd.Index(grids, name="grid"))
    df_state.columns = pd.MultiIndex.from_tuples(
        [(name, "0") for name in df_state.columns], names=["var", "ind_dim"]
    )
    return df_state


def _output_for_grid(grid, dates):
    index = pd.MultiIndex.from_product(
        [[grid], pd.DatetimeIndex(dates)], names=["grid", "datetime"]
    )
    columns = pd.MultiIndex.from_tuples([("SUEWS", "QN")], names=["group", "var"])
    return pd.DataFrame(range(len(index)), index=index, columns=columns)


def _timestamps(df_output):
    return list(df_output.index.get_level_values("datetime"))


def test_output_timestamp_reference_defaults_to_follow():
    config = SUEWSConfig(sites=[{}])

    assert (
        config.model.control.output.timestamp_reference
        == OutputTimestampReference.FOLLOW
    )


def test_output_timestamp_reference_values_are_stable():
    assert [reference.value for reference in OutputTimestampReference] == [
        "follow",
        "utc",
        "local_standard_time",
        "daylight",
    ]


def test_yaml_output_timestamp_reference_loads():
    config = SUEWSConfig(
        sites=[{}],
        model={
            "control": {
                "forcing": {"timestamp_reference": "utc"},
                "output": {"timestamp_reference": "local_standard_time"},
            }
        },
    )

    assert (
        config.model.control.output.timestamp_reference
        == OutputTimestampReference.LOCAL_STANDARD_TIME
    )


def test_daylight_requires_dls_window():
    with pytest.raises(ValueError, match="timestamp_reference='daylight' requires"):
        SUEWSConfig(
            sites=[{}],
            model={"control": {"output": {"timestamp_reference": "daylight"}}},
        )


def test_local_standard_time_zero_offset_warns():
    with pytest.warns(UserWarning, match="zero UTC offset"):
        SUEWSConfig(
            sites=[{}],
            model={
                "control": {"output": {"timestamp_reference": "local_standard_time"}}
            },
        )


@pytest.mark.parametrize("forcing_reference", ["local_standard_time", "utc"])
def test_follow_returns_byte_identity_input(forcing_reference):
    df_output = _output_for_grid(1, pd.date_range("2020-01-01", periods=2, freq="h"))
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 5.5, "startdls": 80, "enddls": 300}
    ])

    relabelled = relabel_output_timestamps(
        df_output,
        "follow",
        forcing_reference,
        df_state,
    )

    assert relabelled is df_output


def test_local_standard_time_to_utc_supports_fractional_offset():
    dates = pd.date_range("2020-01-01 12:00", periods=2, freq="h")
    df_output = _output_for_grid(1, dates)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 5.5, "startdls": 80, "enddls": 300}
    ])

    relabelled = relabel_output_timestamps(
        df_output,
        "utc",
        "local_standard_time",
        df_state,
    )

    assert _timestamps(relabelled) == list(dates - pd.Timedelta(hours=5.5))


def test_utc_to_local_standard_time_supports_fractional_offset():
    dates = pd.date_range("2020-01-01 06:30", periods=2, freq="h")
    df_output = _output_for_grid(1, dates)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 5.5, "startdls": 80, "enddls": 300}
    ])

    relabelled = relabel_output_timestamps(
        df_output,
        "local_standard_time",
        "utc",
        df_state,
    )

    assert _timestamps(relabelled) == list(dates + pd.Timedelta(hours=5.5))


def test_explicit_source_reference_is_byte_identity():
    df_output = _output_for_grid(1, pd.date_range("2020-01-01", periods=2, freq="h"))
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 5.5, "startdls": 80, "enddls": 300}
    ])

    relabelled = relabel_output_timestamps(
        df_output,
        "utc",
        "utc",
        df_state,
    )

    assert relabelled is df_output


def test_daylight_from_utc_uses_local_standard_dls_window():
    dates = pd.to_datetime(["2020-01-15 11:00", "2020-04-01 11:00"])
    df_output = _output_for_grid(1, dates)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 1, "startdls": 80, "enddls": 300}
    ])

    relabelled = relabel_output_timestamps(
        df_output,
        "daylight",
        "utc",
        df_state,
    )

    assert _timestamps(relabelled) == [
        pd.Timestamp("2020-01-15 12:00"),
        pd.Timestamp("2020-04-01 13:00"),
    ]


def test_daylight_relabels_southern_hemisphere_wrap():
    dates = pd.to_datetime(["2020-01-15 12:00", "2020-04-01 12:00", "2020-12-01 12:00"])
    df_output = _output_for_grid(1, dates)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 0, "startdls": 300, "enddls": 80}
    ])

    relabelled = relabel_output_timestamps(
        df_output,
        "daylight",
        "local_standard_time",
        df_state,
    )

    assert _timestamps(relabelled) == [
        dates[0] + pd.Timedelta(hours=1),
        dates[1],
        dates[2] + pd.Timedelta(hours=1),
    ]


def test_daylight_rejects_missing_window_in_low_level_path():
    df_output = _output_for_grid(1, ["2020-04-01 12:00"])
    df_state = _state_for_grid([{"grid": 1, "timezone": 0, "startdls": 0, "enddls": 0}])

    with pytest.raises(ValueError, match="requires startdls and enddls"):
        relabel_output_timestamps(
            df_output,
            "daylight",
            "local_standard_time",
            df_state,
        )


def test_relabels_each_grid_with_own_offset():
    df_output = pd.concat([
        _output_for_grid(1, ["2020-01-01 12:00"]),
        _output_for_grid(2, ["2020-01-01 12:00"]),
    ])
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 5.5, "startdls": 80, "enddls": 300},
        {"grid": 2, "timezone": -3.75, "startdls": 80, "enddls": 300},
    ])

    relabelled = relabel_output_timestamps(
        df_output,
        "local_standard_time",
        "utc",
        df_state,
    )

    assert _timestamps(relabelled) == [
        pd.Timestamp("2020-01-01 17:30"),
        pd.Timestamp("2020-01-01 08:15"),
    ]


def test_text_save_relabels_timestamp_and_marks_filename(tmp_path: Path):
    dates = pd.date_range("2020-07-01 12:00", periods=3, freq="h")
    df_output = _output_for_grid(1, dates)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 1, "startdls": 80, "enddls": 300}
    ])

    paths = save_df_output(
        df_output,
        freq_s=3600,
        path_dir_save=tmp_path,
        save_snow=False,
        output_groups=["SUEWS"],
        timestamp_reference="utc",
        forcing_timestamp_reference="local_standard_time",
        df_state_final=df_state,
    )

    output_path = next(path for path in paths if path.suffix == ".txt")
    saved = pd.read_csv(output_path, sep="\t")
    assert output_path.name == "1_2020_SUEWS_60_UTC.txt"
    assert (saved.loc[0, "Year"], saved.loc[0, "Hour"], saved.loc[0, "Min"]) == (
        2020,
        11,
        0,
    )


def test_text_filename_marks_explicit_standard_reference(tmp_path: Path):
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
        timestamp_reference="local_standard_time",
    )

    assert path.name == "1_2020_SUEWS_60_STANDARD.txt"


def test_text_daylight_transition_keeps_native_cadence(tmp_path: Path):
    dates = pd.to_datetime(["2020-10-25 23:00", "2020-10-26 00:00", "2020-10-26 01:00"])
    df_output = _output_for_grid(1, dates)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 0, "startdls": 80, "enddls": 300}
    ])

    paths = save_df_output(
        df_output,
        freq_s=3600,
        path_dir_save=tmp_path,
        save_snow=False,
        output_groups=["SUEWS"],
        timestamp_reference="daylight",
        forcing_timestamp_reference="local_standard_time",
        df_state_final=df_state,
    )

    output_path = next(path for path in paths if path.suffix == ".txt")
    saved = pd.read_csv(output_path, sep="\t")
    assert output_path.name == "1_2020_SUEWS_60_DAYLIGHT.txt"
    assert list(zip(saved["Hour"], saved["Min"], strict=True)) == [
        (0, 0),
        (0, 0),
        (1, 0),
    ]


def test_text_daylight_keeps_dailystate_filename_contract(tmp_path: Path):
    dates = pd.to_datetime(["2020-04-01 00:00", "2020-04-02 00:00"])
    index = pd.MultiIndex.from_product([[1], dates], names=["grid", "datetime"])
    columns = pd.MultiIndex.from_tuples(
        [("DailyState", "Tmin")], names=["group", "var"]
    )
    df_output = pd.DataFrame([1.0, 2.0], index=index, columns=columns)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 0, "startdls": 80, "enddls": 300}
    ])

    paths = save_df_output(
        df_output,
        freq_s=3600,
        path_dir_save=tmp_path,
        output_groups=["DailyState"],
        timestamp_reference="daylight",
        forcing_timestamp_reference="local_standard_time",
        df_state_final=df_state,
    )

    assert [path.name for path in paths] == ["1_2020_DailyState_DAYLIGHT.txt"]


def test_text_daylight_saves_single_dailystate_record(tmp_path: Path):
    dates = pd.to_datetime(["2020-04-01 00:00"])
    index = pd.MultiIndex.from_product([[1], dates], names=["grid", "datetime"])
    columns = pd.MultiIndex.from_tuples(
        [("DailyState", "Tmin")], names=["group", "var"]
    )
    df_output = pd.DataFrame([1.0], index=index, columns=columns)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 0, "startdls": 80, "enddls": 300}
    ])

    paths = save_df_output(
        df_output,
        freq_s=3600,
        path_dir_save=tmp_path,
        output_groups=["DailyState"],
        timestamp_reference="daylight",
        forcing_timestamp_reference="local_standard_time",
        df_state_final=df_state,
    )

    output_path = paths[0]
    saved = pd.read_csv(output_path, sep="\t")
    assert output_path.name == "1_2020_DailyState_DAYLIGHT.txt"
    assert len(saved) == 1


@pytest.mark.parametrize("save_tstep", [False, True])
def test_text_daylight_skips_empty_dailystate_group(tmp_path: Path, save_tstep: bool):
    dates = pd.date_range("2020-04-01 00:05", periods=25, freq="5min")
    index = pd.MultiIndex.from_product([[1], dates], names=["grid", "datetime"])
    suews_columns = [("SUEWS", var) for var in df_var_out.loc["SUEWS"].index]
    columns = pd.MultiIndex.from_tuples(
        [*suews_columns, ("DailyState", "Tmin")], names=["group", "var"]
    )
    df_output = pd.DataFrame(0.0, index=index, columns=columns)
    df_output.loc[:, ("DailyState", "Tmin")] = float("nan")
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 0, "startdls": 80, "enddls": 300}
    ])

    paths = save_df_output(
        df_output,
        freq_s=3600,
        path_dir_save=tmp_path,
        save_tstep=save_tstep,
        output_groups=["SUEWS", "DailyState"],
        timestamp_reference="daylight",
        forcing_timestamp_reference="local_standard_time",
        df_state_final=df_state,
    )

    assert paths
    assert all("DailyState" not in path.name for path in paths)


def test_text_explicit_reference_keeps_native_and_resampled_cadences(
    tmp_path: Path,
):
    dates = pd.date_range("2020-01-01 00:05", periods=25, freq="5min")
    index = pd.MultiIndex.from_product([[1], dates], names=["grid", "datetime"])
    columns = pd.MultiIndex.from_product(
        [["SUEWS"], df_var_out.loc["SUEWS"].index], names=["group", "var"]
    )
    df_output = pd.DataFrame(0.0, index=index, columns=columns)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 1, "startdls": 80, "enddls": 300}
    ])

    paths = save_df_output(
        df_output,
        freq_s=3600,
        path_dir_save=tmp_path,
        save_tstep=True,
        save_snow=False,
        output_groups=["SUEWS"],
        timestamp_reference="utc",
        forcing_timestamp_reference="local_standard_time",
        df_state_final=df_state,
    )

    assert {path.name for path in paths} == {
        "1_2019_SUEWS_5_UTC.txt",
        "1_2020_SUEWS_5_UTC.txt",
        "1_2020_SUEWS_60_UTC.txt",
    }


def test_text_utc_conversion_partitions_files_by_saved_year(tmp_path: Path):
    dates = pd.to_datetime(["2020-01-01 00:00", "2020-01-01 01:00", "2020-01-01 02:00"])
    df_output = _output_for_grid(1, dates)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 1, "startdls": 80, "enddls": 300}
    ])

    paths = save_df_output(
        df_output,
        freq_s=3600,
        path_dir_save=tmp_path,
        save_snow=False,
        output_groups=["SUEWS"],
        timestamp_reference="utc",
        forcing_timestamp_reference="local_standard_time",
        df_state_final=df_state,
    )

    assert {path.name for path in paths} == {
        "1_2019_SUEWS_60_UTC.txt",
        "1_2020_SUEWS_60_UTC.txt",
    }


def test_parquet_uses_same_relabelled_index(tmp_path: Path):
    pytest.importorskip("pyarrow", reason="Parquet output tests require pyarrow")
    dates = pd.date_range("2020-01-01 06:30", periods=2, freq="h")
    df_output = _output_for_grid(1, dates)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 5.5, "startdls": 80, "enddls": 300}
    ])

    paths = save_df_output_parquet(
        df_output,
        df_state,
        path_dir_save=tmp_path,
        save_tstep=True,
        save_state=False,
        timestamp_reference="local_standard_time",
        forcing_timestamp_reference="utc",
    )

    output_path = next(
        path for path in paths if path.name == "SUEWS_output_STANDARD.parquet"
    )
    saved = pd.read_parquet(output_path)
    expected = relabel_output_timestamps(
        df_output,
        "local_standard_time",
        "utc",
        df_state,
    )
    pd.testing.assert_index_equal(saved.index, expected.index)


def test_parquet_bundle_marks_explicit_timestamp_reference(tmp_path: Path):
    pytest.importorskip("pyarrow", reason="Parquet output tests require pyarrow")
    dates = pd.date_range("2020-01-01 12:00", periods=2, freq="h")
    df_output = _output_for_grid(1, dates)
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 1, "startdls": 80, "enddls": 300}
    ])

    paths = save_df_output_parquet(
        df_output,
        df_state,
        path_dir_save=tmp_path,
        save_tstep=True,
        timestamp_reference="daylight",
        forcing_timestamp_reference="local_standard_time",
    )

    assert {path.name for path in paths} == {
        "SUEWS_output_DAYLIGHT.parquet",
        "SUEWS_state_final_DAYLIGHT.parquet",
        "SUEWS_metadata_DAYLIGHT.parquet",
    }
    metadata = pd.read_parquet(tmp_path / "SUEWS_metadata_DAYLIGHT.parquet")
    assert metadata.loc[0, "timestamp_reference"] == "daylight"


def test_output_object_passes_forcing_reference_to_save_backend(
    tmp_path: Path, monkeypatch
):
    config = SUEWSConfig(
        sites=[{}],
        model={
            "control": {
                "forcing": {"timestamp_reference": "utc"},
                "output": {"timestamp_reference": "utc"},
            }
        },
    )
    df_output = _output_for_grid(1, ["2020-01-01 12:00"])
    df_state = _state_for_grid([
        {"grid": 1, "timezone": 1, "startdls": 80, "enddls": 300}
    ])
    output = SUEWSOutput(df_output, df_state, config=config)
    captured = {}

    def fake_save_supy(**kwargs):
        captured.update(kwargs)
        return []

    monkeypatch.setattr("supy._supy_module._save_supy", fake_save_supy)

    output.save(tmp_path)

    reference = captured["forcing_timestamp_reference"]
    assert getattr(reference, "value", reference) == "utc"
