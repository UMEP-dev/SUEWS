"""Registry metadata survives each output writer without changing values."""

import json

import pandas as pd
import pyarrow.parquet as pq
import pytest

from supy._save import StreamedParquetParts, save_df_output, save_df_output_parquet
from supy.data_model.output.registry import get_output_contract_catalogue
from supy.data_model.output.version import CURRENT_OUTPUT_VERSION
from supy.suews_output import SUEWSOutput
from supy.units import parse_unit

pytestmark = pytest.mark.api


@pytest.fixture
def frames():
    columns = pd.MultiIndex.from_tuples(
        [
            (v.group, v.name)
            for v in get_output_contract_catalogue().variables
            if v.group == "SUEWS"
        ],
        names=["group", "var"],
    )
    index = pd.MultiIndex.from_product(
        [[1], pd.date_range("2012-01-01 00:05", periods=24, freq="5min")],
        names=["grid", "datetime"],
    )
    df_output = pd.DataFrame(0.5, index=index, columns=columns)
    df_state = pd.DataFrame({("tstep", "0"): [300]}, index=[1])
    return df_output, df_state


def check_metadata(metadata, name, interval):
    entry = next(
        v
        for v in get_output_contract_catalogue().variables
        if (v.group, v.name) == ("SUEWS", name)
    )
    assert metadata["units"] == entry.unit
    parse_unit(metadata["units"])
    assert metadata["description"] == entry.description
    assert metadata["interval"] == interval
    expected = entry.cell_methods
    if expected and expected != "time: point":
        expected += f" (interval: {interval})"
    assert metadata["cell_methods"] == expected
    assert metadata.get("units_metadata") == entry.units_metadata


@pytest.mark.parametrize("frequency,interval", [(300, "5 minutes"), (3600, "1 hour")])
@pytest.mark.parametrize("streamed", [False, True])
def test_parquet_metadata_roundtrip(frames, tmp_path, frequency, interval, streamed):
    df_output, df_state = frames
    stream = None
    if streamed:
        stream = StreamedParquetParts(tmp_path / "SUEWS_output.parquet", tstep_s=300)
        stream.prepare()
    chunks = [df_output.iloc[:12], df_output.iloc[12:]] if streamed else [df_output]
    for number, chunk in enumerate(chunks):
        if stream:
            stream.start_chunk(2012, number)
        paths = save_df_output_parquet(
            chunk,
            df_state,
            freq_s=frequency,
            path_dir_save=tmp_path,
            save_state=False,
            streamed_parquet=stream,
        )
    files = sorted(paths[0].glob("*.parquet")) if streamed else paths[:1]
    for path in files:
        schema = pq.read_schema(path)
        assert (
            schema.metadata[b"output_contract_version"].decode()
            == CURRENT_OUTPUT_VERSION
        )
        for field, (_, name) in zip(schema, df_output.columns):
            metadata = {k.decode(): v.decode() for k, v in field.metadata.items()}
            check_metadata(metadata, name, interval)
    saved = pd.read_parquet(paths[0])
    assert len(saved) == (24 if frequency == 300 else 2)
    assert saved[("SUEWS", "Rain")].sum() * frequency == pytest.approx(12)
    if frequency == 300:
        from supy._output_metadata import prepare_output
        pd.testing.assert_frame_equal(saved, prepare_output(df_output, interval_s=300, write=True))


@pytest.mark.parametrize("frequency,interval", [(300, "5 minutes"), (3600, "1 hour")])
def test_text_metadata_roundtrip(frames, tmp_path, frequency, interval):
    df_output, _ = frames
    paths = save_df_output(
        df_output, freq_s=frequency, path_dir_save=tmp_path, output_groups=["SUEWS"]
    )
    for path in paths:
        payload = json.loads(
            path.with_name(path.name + ".units.json").read_text(encoding="utf-8")
        )
        assert payload["output_contract_version"] == CURRENT_OUTPUT_VERSION
        assert (
            list(payload["columns"])
            == path.read_text(encoding="utf-8").splitlines()[0].split()
        )
        for _, name in df_output.columns:
            check_metadata(payload["columns"][name], name, interval)


def test_units_read_only_and_unknown(frames):
    df_output, df_state = frames
    df_output[("extra", "unknown")] = 0.0
    output = SUEWSOutput(df_output, df_state, metadata={"tstep_s": 300})
    units = output.units
    assert units[("SUEWS", "Rain")] == "mm"
    check_metadata(units.metadata[("SUEWS", "T2")], "T2", "5 minutes")
    assert units[("extra", "unknown")] is None
    assert units.metadata[("extra", "unknown")] == {"status": "unknown"}
    with pytest.raises(TypeError):
        units[("SUEWS", "Rain")] = "m"
    with pytest.raises(TypeError):
        units.metadata[("SUEWS", "Rain")]["units"] = "m"


def test_native_parquet_uses_state_timestep(frames, tmp_path):
    df_output, df_state = frames
    paths = save_df_output_parquet(
        df_output,
        df_state,
        freq_s=3600,
        save_tstep=True,
        save_state=False,
        path_dir_save=tmp_path,
    )
    metadata = pq.read_schema(paths[0]).field(str(("SUEWS", "Rain"))).metadata
    assert metadata[b"cell_methods"] == b"time: mean (interval: 5 minutes)"
    from supy._output_metadata import prepare_output
    pd.testing.assert_frame_equal(pd.read_parquet(paths[0]), prepare_output(df_output, interval_s=300, write=True))


def test_text_unknown_and_daily_time_base(tmp_path):
    from supy._save import save_df_grid_group

    frame = pd.DataFrame(
        {"HDD1_h": [2.0], "not_registered": [4.0]},
        index=pd.date_range("2012-01-02", periods=1, freq="D"),
    )
    path = save_df_grid_group(frame, 1, "DailyState", tmp_path, "site", freq_s=86400)
    payload = json.loads(
        path.with_name(path.name + ".units.json").read_text(encoding="utf-8")
    )
    assert payload["columns"]["not_registered"] == {"status": "unknown"}
    assert payload["columns"]["HDD1_h"]["interval"] == "1 day"
    assert payload["columns"]["Year"]["units"] == "1"
    assert payload["columns"]["Year"]["cell_methods"] == ""


def test_resampled_units_describe_held_rows(frames):
    df_output, df_state = frames
    output = SUEWSOutput(df_output, df_state, metadata={"tstep_s": 300})
    hourly = output.resample("1h")
    assert (
        hourly.units.metadata[("SUEWS", "Rain")]["cell_methods"]
        == "time: sum (interval: 1 hour)"
    )


@pytest.mark.parametrize("writer", ["text", "parquet"])
@pytest.mark.parametrize("units", [None, {"Rain": "mm h-1"}])
def test_save_tstep_preserves_resampled_water_interval(frames, tmp_path, writer, units):
    """Saving held hourly rows uses their coverage, including the final quarter-hour."""
    frame, state = frames
    hourly = SUEWSOutput(frame.iloc[:15], state).resample("h")
    kwargs = dict(
        df_state_final=state, freq_s=3600, save_tstep=True,
        path_dir_save=tmp_path, units=units,
    )
    if writer == "text":
        paths = save_df_output(hourly.df, output_groups=["SUEWS"], **kwargs)
        saved = pd.read_csv(paths[0], sep=r"\s+")
        values = saved["Rain"]
        metadata = json.loads(
            paths[0].with_name(paths[0].name + ".units.json").read_text(encoding="utf-8")
        )["columns"]["Rain"]
    else:
        paths = save_df_output_parquet(hourly.df, save_state=False, **kwargs)
        values = pd.read_parquet(paths[0])[("SUEWS", "Rain")]
        metadata = {
            key.decode(): value.decode()
            for key, value in pq.read_schema(paths[0]).field(str(("SUEWS", "Rain"))).metadata.items()
        }
    expected = 6 if units else 0.5 / 300
    assert values.to_numpy() == pytest.approx([expected, expected])
    assert metadata["units"] == ("mm h-1" if units else "kg m-2 s-1")
    assert metadata["interval"] == "1 hour"
    assert metadata["cell_methods"] == "time: mean (interval: 1 hour)"


@pytest.mark.parametrize("water_column", [False, True])
def test_trailing_partial_bin_without_unit_request(frames, water_column):
    """A run ending at 01:15 retains the 02:00 bin for ordinary variables too."""
    from supy._output_metadata import output_covered_seconds

    frame, state = frames
    columns = [("SUEWS", "QH")]
    if water_column:
        columns.append(("SUEWS", "Rain"))
    frame = frame.iloc[:15].loc[:, columns].copy()
    frame[("SUEWS", "QH")] = list(range(15))
    hourly = SUEWSOutput(frame, state).resample("h")

    assert list(hourly.times.hour) == [1, 2]
    assert hourly.df[("SUEWS", "QH")].to_numpy() == pytest.approx([5.5, 13])
    if water_column:
        assert output_covered_seconds(hourly.df).to_numpy() == pytest.approx([3600, 900])
        assert hourly.df[("SUEWS", "Rain")].to_numpy() == pytest.approx([6, 1.5])
        assert hourly.units[("SUEWS", "Rain")] == "mm"


def test_empty_coverage_uses_regular_cadence(frames):
    """An empty coverage mapping is equivalent to absent coverage."""
    from supy._output_metadata import output_covered_seconds

    frame, _ = frames
    expected = output_covered_seconds(frame)
    frame.attrs["supy_output_coverage_s"] = {}
    pd.testing.assert_series_equal(output_covered_seconds(frame), expected)


def test_requested_units_affine_scale_and_repeated_conversion(frames):
    from supy._output_metadata import prepare_output

    df_output, df_state = frames
    df_output[("BL", "Press_hPa")] = 1000.0
    original = df_output.copy(deep=True)
    output = SUEWSOutput(df_output, df_state, units={"QE": "kW m-2", "T2": "K", "Press_hPa": "Pa"})
    assert output.df[("SUEWS", "QE")].iloc[0] == pytest.approx(0.0005)
    assert output.df[("SUEWS", "T2")].iloc[0] == pytest.approx(273.65)
    assert output.df[("BL", "Press_hPa")].iloc[0] == 100000.0
    assert output.units[("SUEWS", "T2")] == "K"
    assert output.units.metadata[("SUEWS", "T2")]["units_metadata"] == "temperature: on_scale"
    pd.testing.assert_frame_equal(df_output, original)
    assert prepare_output(df_output) is df_output
    assert prepare_output(df_output, {}) is df_output
    again = output.to_units({"T2": "K", "QE": "W m-2"})
    assert again.df[("SUEWS", "T2")].iloc[0] == pytest.approx(273.65)
    assert again.df[("SUEWS", "QE")].iloc[0] == 0.5


def test_temperature_difference_uses_registry_kind(frames):
    df_output, df_state = frames
    difference = next(v for v in get_output_contract_catalogue().variables
                      if v.unit == "K" and v.units_metadata == "temperature: difference")
    key = (difference.group, difference.name)
    df_output[key] = 2.5
    output = SUEWSOutput(df_output, df_state).to_units({key: "degC"})
    assert (output.df[key] == 2.5).all()
    assert (output.to_units({key: "K"}).df[key] == 2.5).all()
    assert output.units.metadata[key]["units_metadata"] == "temperature: difference"


@pytest.mark.parametrize("unit_request,match", [
    ({"QE": "K"}, "QE.*dimensions differ"),
    ({"QE": "watts"}, "QE.*"),
    ({"QE": None}, "QE.*unit string"),
    ({"missing": "K"}, "missing.*not present"),
    ({"unknown": "1"}, "unknown.*without a unit"),
])
def test_invalid_output_unit_requests(frames, unit_request, match):
    df_output, df_state = frames
    df_output[("extra", "unknown")] = 1
    original = df_output.copy(deep=True)
    with pytest.raises(ValueError, match=match):
        SUEWSOutput(df_output, df_state, units=unit_request)
    pd.testing.assert_frame_equal(df_output, original)


@pytest.mark.parametrize("frequency", [300, 3600])
@pytest.mark.parametrize("streamed", [False, True])
def test_converted_writer_values_and_metadata(frames, tmp_path, frequency, streamed):
    from supy._save import StreamedTextFiles

    df_output, df_state = frames
    request = {"QE": "kW m-2", "T2": "K"}
    path_txt = tmp_path / "txt"
    path_txt.mkdir()
    stream = None
    text_stream = None
    if streamed:
        stream = StreamedParquetParts(tmp_path / "SUEWS_output.parquet", tstep_s=300)
        stream.prepare()
        stream.start_chunk(2012, 0)
        text_stream = StreamedTextFiles()
    paths = save_df_output_parquet(df_output, df_state, freq_s=frequency,
                                  path_dir_save=tmp_path, save_state=False,
                                  streamed_parquet=stream, units=request)
    file = next(paths[0].glob("*.parquet")) if streamed else paths[0]
    saved = pd.read_parquet(file)
    assert saved[("SUEWS", "QE")].to_numpy() == pytest.approx(0.0005)
    assert saved[("SUEWS", "T2")].to_numpy() == pytest.approx(273.65)
    schema = pq.read_schema(file)
    for name, unit in request.items():
        assert schema.field(str(("SUEWS", name))).metadata[b"units"] == unit.encode()
    text_paths = save_df_output(df_output, freq_s=frequency, path_dir_save=path_txt,
                               streamed_files=text_stream, units=request)
    text = pd.read_csv(text_paths[0], sep=r"\s+")
    assert text["QE"].to_numpy() == pytest.approx(0.0005)
    assert text["T2"].to_numpy() == pytest.approx(273.65)
    metadata = json.loads(text_paths[0].with_name(text_paths[0].name + ".units.json").read_text(encoding="utf-8"))
    for name, unit in request.items():
        assert metadata["columns"][name]["units"] == unit


def test_converted_output_resample_and_public_saves(frames, tmp_path):
    from supy.suews_sim import SUEWSSimulation

    df_output, df_state = frames
    df_state[("snowuse", "0")] = 0
    output = SUEWSOutput(df_output, df_state).to_units({"T2": "K"})
    resampled = output.resample("h")
    assert resampled.units[("SUEWS", "T2")] == "K"
    for label, owner in [("output", output), ("simulation", SUEWSSimulation())]:
        if label == "simulation":
            owner._df_output = df_output
            owner._df_state_final = df_state
            owner._run_completed = True
            # Exercise the real save settings and output writer, excluding run provenance.
            owner._write_save_sidecars = lambda paths, settings, **kwargs: paths
        paths = owner.save(tmp_path / label, format="parquet", units={"T2": "K", "QE": "kW m-2"})
        path = next(p for p in paths if str(p).endswith("SUEWS_output.parquet"))
        saved = pd.read_parquet(path)
        assert saved[("SUEWS", "T2")].to_numpy() == pytest.approx(273.65)
        assert saved[("SUEWS", "QE")].to_numpy() == pytest.approx(0.0005)


def test_every_registered_unit_converts_to_itself():
    """A native-unit request is valid for every variable that has a unit."""
    from supy.data_model.output import get_output_contract_catalogue
    from supy.units import convert, parse_unit

    for entry in get_output_contract_catalogue().variables:
        if not entry.unit or parse_unit(entry.unit).is_no_unit:
            continue
        assert convert(
            1.0, entry.unit, entry.unit, units_metadata=entry.units_metadata
        ) == pytest.approx(1.0), (entry.group, entry.name)


@pytest.mark.parametrize("frequency", [3600, 86400])
def test_all_written_water_fluxes_are_interval_rates(tmp_path, frequency):
    import numpy as np

    catalogue = get_output_contract_catalogue().variables
    water = [(v.group, v.name) for v in catalogue if v.unit == "kg m-2 s-1" and v.group != "DailyState"]
    columns = pd.MultiIndex.from_tuples([(v.group, v.name) for v in catalogue if v.group in {"SUEWS", "snow", "debug"}], names=["group", "var"])
    index = pd.MultiIndex.from_product([[1], pd.date_range("2012-01-01 00:05", periods=288, freq="5min")], names=["grid", "datetime"])
    frame = pd.DataFrame(0.5, index=index, columns=columns)
    state = pd.DataFrame({("tstep", "0"): [300]}, index=[1])
    paths = save_df_output_parquet(frame, state, freq_s=frequency, path_dir_save=tmp_path, save_state=False)
    saved = pd.read_parquet(paths[0])
    np.testing.assert_allclose(saved[water], 0.5 / 300)
    np.testing.assert_allclose(saved[("SUEWS", "SMD")], 0.5)
    schema = pq.read_schema(paths[0])
    for column in water:
        metadata = schema.field(str(column)).metadata
        assert metadata[b"units"] == b"kg m-2 s-1"
        assert metadata[b"cell_methods"].startswith(b"time: mean")
    np.testing.assert_allclose(frame, 0.5)


@pytest.mark.parametrize("unit,expected", [("mm h-1", 6), ("mm d-1", 144)])
def test_water_rate_requests_resample_and_round_trip(frames, tmp_path, unit, expected):
    import numpy as np

    frame, state = frames
    output = SUEWSOutput(frame, state, metadata={"tstep_s": 300})
    converted = output.to_units({"Rain": unit})
    hourly = converted.resample("h")
    np.testing.assert_allclose(hourly.df[("SUEWS", "Rain")], expected)
    restored = hourly.to_units({"Rain": "kg m-2 s-1"})
    np.testing.assert_allclose(restored.df[("SUEWS", "Rain")], 0.5 / 300)
    paths = save_df_output_parquet(frame, state, freq_s=3600, path_dir_save=tmp_path, save_state=False, units={"Rain": unit})
    np.testing.assert_allclose(pd.read_parquet(paths[0])[("SUEWS", "Rain")], expected)
    metadata = pq.read_schema(paths[0]).field(str(("SUEWS", "Rain"))).metadata
    assert metadata[b"units"] == unit.encode()


def test_text_preserves_small_rain_rates(frames, tmp_path):
    frame, _ = frames
    frame[("SUEWS", "Rain")] = 0.00003
    paths = save_df_output(frame, freq_s=3600, path_dir_save=tmp_path, output_groups=["SUEWS"])
    saved = pd.read_csv(paths[0], sep=r"\s+")
    assert saved.Rain.to_numpy() == pytest.approx(1e-7, rel=1e-10, abs=0)
    metadata = json.loads(paths[0].with_name(paths[0].name + ".units.json").read_text(encoding="utf-8"))
    assert metadata["columns"]["Rain"]["units"] == "kg m-2 s-1"
    assert metadata["columns"]["Rain"]["cell_methods"] == "time: mean (interval: 1 hour)"


def test_daily_totals_use_daily_interval(tmp_path):
    from supy._output_metadata import prepare_output

    columns = pd.MultiIndex.from_tuples(
        [(v.group, v.name) for v in get_output_contract_catalogue().variables if v.group == "DailyState" and v.unit == "kg m-2 s-1"],
        names=["group", "var"],
    )
    index = pd.MultiIndex.from_product([[1], pd.date_range("2012-01-01", periods=288, freq="5min")], names=["grid", "datetime"])
    frame = pd.DataFrame(float("nan"), index=index, columns=columns)
    frame.iloc[-1] = 86400.0
    result = prepare_output(frame, interval_s=300, write=True)
    assert (result.dropna() == 1.0).all().all()
    assert (prepare_output(result, interval_s=300, write=True).dropna() == 1.0).all().all()
    from supy._save import save_df_grid_group

    daily = result["DailyState"].dropna().xs(1, level="grid").copy()
    path = save_df_grid_group(daily, 1, "DailyState", tmp_path, "site", freq_s=86400)
    saved = pd.read_csv(path, sep=r"\s+")
    assert (saved[daily.columns] == 1.0).all().all()
    metadata = json.loads(path.with_name(path.name + ".units.json").read_text(encoding="utf-8"))
    assert metadata["columns"]["P_day"]["cell_methods"] == "time: mean"



def test_single_row_rate_request_uses_state_interval(frames):
    frame, state = frames
    single = frame.iloc[:1].copy()
    single.index = single.index.remove_unused_levels()
    converted = SUEWSOutput(single, state).to_units({"Rain": "mm h-1"})
    assert converted.df[("SUEWS", "Rain")].iloc[0] == pytest.approx(6.0)



def test_streamed_requested_water_rates_keep_daily_metadata(frames, tmp_path):
    import numpy as np

    frame, state = frames
    frame[("DailyState", "P_day")] = np.nan
    frame.loc[frame.index[-1], ("DailyState", "P_day")] = 24.0
    stream = StreamedParquetParts(tmp_path / "SUEWS_output.parquet", tstep_s=300)
    stream.prepare()
    stream.start_chunk(2012, 0)
    paths = save_df_output_parquet(
        frame, state, freq_s=3600, path_dir_save=tmp_path, save_state=False,
        streamed_parquet=stream, units={"Rain": "mm h-1", "P_day": "mm h-1"},
    )
    saved = pd.read_parquet(paths[0])
    np.testing.assert_allclose(saved[("SUEWS", "Rain")].dropna(), 6)
    np.testing.assert_allclose(saved[("DailyState", "P_day")].dropna(), 12)
    schema = pq.read_schema(next(paths[0].glob("*.parquet")))
    for column in [("SUEWS", "Rain"), ("DailyState", "P_day")]:
        assert schema.field(str(column)).metadata[b"units"] == b"mm h-1"


@pytest.mark.parametrize("frequency", ["ME", "MS", "W", "YE"])
@pytest.mark.parametrize("rates", [False, True])
def test_calendar_resample(frames, frequency, rates):
    """Calendar bins accept native amounts and explicitly requested rates."""
    frame, state = frames
    output = SUEWSOutput(frame, state)
    if rates:
        output = output.to_units({"Rain": "mm h-1"})
    result = output.resample(frequency)
    assert len(result.df) == 1
    assert result.df[("SUEWS", "Rain")].iloc[0] == pytest.approx(6 if rates else 12)
    assert result.units[("SUEWS", "Rain")] == ("mm h-1" if rates else "mm")
    converted = result.to_units({"Rain": "mm h-1"})
    assert converted.df[("SUEWS", "Rain")].iloc[0] == pytest.approx(6)


@pytest.mark.parametrize("frequency", ["ME", "MS", "W", "YE"])
def test_calendar_resample_without_water_columns(frames, frequency):
    frame, state = frames
    result = SUEWSOutput(frame.loc[:, [("SUEWS", "QH")]], state).resample(frequency)
    assert result.df[("SUEWS", "QH")].to_numpy() == pytest.approx([0.5])


@pytest.mark.parametrize("convert_first", [False, True])
def test_partial_bins_use_covered_seconds(frames, convert_first):
    """A 10:30-12:15 run covers 30, 60 and 15 minutes in its hourly bins."""
    frame, state = frames
    frame = frame.iloc[:21].copy()
    frame.index = pd.MultiIndex.from_product(
        [[1], pd.date_range("2012-01-01 10:35", periods=21, freq="5min")],
        names=["grid", "datetime"],
    )
    # Distinct bin rates expose incorrect weighting on a second resample.
    frame[("SUEWS", "Rain")] = [0.5] * 6 + [1.0] * 12 + [2.0] * 3
    output = SUEWSOutput(frame, state)
    if convert_first:
        output = output.to_units({"Rain": "mm h-1"})
    hourly = output.resample("h").to_units({"Rain": "mm h-1"})
    assert list(hourly.df.index.get_level_values("datetime").hour) == [11, 12, 13]
    assert hourly.df[("SUEWS", "Rain")].to_numpy() == pytest.approx([6, 12, 24])
    daily = hourly.resample("D")
    assert daily.df[("SUEWS", "Rain")].to_numpy() == pytest.approx([21 / 1.75])


@pytest.mark.parametrize("writer", ["text", "parquet", "streamed"])
def test_partial_written_bins(frames, tmp_path, writer):
    frame, state = frames
    frame = frame.iloc[:21].copy()
    frame.index = pd.MultiIndex.from_product(
        [[1], pd.date_range("2012-01-01 10:35", periods=21, freq="5min")],
        names=["grid", "datetime"],
    )
    if writer == "text":
        paths = save_df_output(frame, freq_s=3600, path_dir_save=tmp_path, output_groups=["SUEWS"])
        values = pd.read_csv(paths[0], sep=r"\s+")["Rain"]
    else:
        stream = None
        if writer == "streamed":
            stream = StreamedParquetParts(tmp_path / "SUEWS_output.parquet", tstep_s=300)
            stream.prepare()
            stream.start_chunk(2012, 0)
        paths = save_df_output_parquet(frame, state, freq_s=3600, path_dir_save=tmp_path,
                                      save_state=False, streamed_parquet=stream)
        values = pd.read_parquet(paths[0])[("SUEWS", "Rain")]
    assert values.to_numpy() == pytest.approx([0.5 / 300] * 3)


def test_text_requested_rain_rate_and_sidecar(frames, tmp_path):
    frame, _ = frames
    paths = save_df_output(frame, units={"Rain": "mm h-1"}, freq_s=3600,
                           path_dir_save=tmp_path, output_groups=["SUEWS"])
    assert pd.read_csv(paths[0], sep=r"\s+")["Rain"].to_numpy() == pytest.approx([6, 6])
    payload = json.loads(paths[0].with_name(paths[0].name + ".units.json").read_text(encoding="utf-8"))
    assert payload["columns"]["Rain"]["units"] == "mm h-1"
    assert payload["columns"]["Rain"]["cell_methods"] == "time: mean (interval: 1 hour)"


@pytest.mark.parametrize("converted", [False, True])
def test_inconsistent_interval_is_rejected(frames, converted):
    from supy._output_metadata import prepare_output

    frame, state = frames
    hourly = SUEWSOutput(frame, state).resample("h")
    if converted:
        hourly = hourly.to_units({"Rain": "mm h-1"})
    with pytest.raises(ValueError, match="interval_s disagrees"):
        prepare_output(hourly.df, {"Rain": "mm h-1"}, interval_s=300)
    assert hourly.to_units({"Rain": "mm h-1"}).df[("SUEWS", "Rain")].to_numpy() == pytest.approx([6, 6])


def test_water_flux_amount_unit_is_rejected(frames):
    frame, state = frames
    with pytest.raises(ValueError, match="Rain"):
        SUEWSOutput(frame, state).to_units({"Rain": "m"})


@pytest.mark.parametrize("convert_first", [False, True])
def test_monthly_rates_use_each_months_coverage(frames, convert_first):
    frame, state = frames
    frame = frame.iloc[:1].copy()
    frame = pd.concat([frame] * 60, ignore_index=True)
    frame.index = pd.MultiIndex.from_product(
        [[1], pd.date_range("2012-01-01", periods=60, freq="D")],
        names=["grid", "datetime"],
    )
    state[("tstep", "0")] = 86400
    output = SUEWSOutput(frame, state)
    if convert_first:
        output = output.to_units({"Rain": "mm d-1"})
    monthly = output.resample("ME").to_units({"Rain": "mm d-1"})
    assert monthly.df[("SUEWS", "Rain")].to_numpy() == pytest.approx([0.5, 0.5])



def test_partial_coverage_survives_saved_clock_change(frames, tmp_path):
    """Reloaded rates retain their weights after timestamp relabelling."""
    frame, state = frames
    frame = frame.iloc[:21].copy()
    frame.index = pd.MultiIndex.from_product(
        [[1], pd.date_range("2012-01-01 10:35", periods=21, freq="5min")],
        names=["grid", "datetime"],
    )
    frame[("SUEWS", "Rain")] = [0.5] * 6 + [1.0] * 12 + [2.0] * 3
    state[("timezone", "0")] = 1.0
    paths = save_df_output_parquet(
        frame, state, freq_s=3600, path_dir_save=tmp_path, save_state=False,
        timestamp_reference="utc", forcing_timestamp_reference="local_standard_time",
        units={"Rain": "mm h-1"},
    )
    saved = pd.read_parquet(paths[0])
    assert list(saved.index.get_level_values("datetime").hour) == [10, 11, 12]
    assert saved[("SUEWS", "Rain")].to_numpy() == pytest.approx([6, 12, 24])
    schema = pq.read_schema(paths[0])
    rain = schema.field(str(("SUEWS", "Rain"))).metadata
    assert rain[b"units"] == b"mm h-1"
    assert rain[b"cell_methods"] == b"time: mean (interval: 1 hour)"
    attributes = json.loads(schema.metadata[b"PANDAS_ATTRS"])
    assert attributes["supy_output_interval_s"] == 3600
    assert saved.attrs == attributes
    assert saved.attrs["supy_output_coverage_s"] == {
        "1|2012-01-01T10:00:00": 1800,
        "1|2012-01-01T12:00:00": 900,
    }
    reloaded = SUEWSOutput(saved, state).to_units({"Rain": "mm h-1"})
    assert reloaded.df[("SUEWS", "Rain")].to_numpy() == pytest.approx([6, 12, 24])
    assert reloaded.units.metadata[("SUEWS", "Rain")]["interval"] == "1 hour"
    daily = reloaded.resample("D")
    assert daily.df[("SUEWS", "Rain")].to_numpy() == pytest.approx([21 / 1.75])
    (tmp_path / "again").mkdir()
    paths_again = save_df_output_parquet(
        reloaded.df, state, freq_s=3600, path_dir_save=tmp_path / "again",
        save_state=False, save_tstep=True,
    )
    saved_again = pd.read_parquet(paths_again[0])
    pd.testing.assert_frame_equal(saved_again, saved)


@pytest.mark.parametrize("start,steps", [("00:00", 288), ("12:00", 144), ("00:00", 145)])
@pytest.mark.parametrize("writer", ["text", "parquet", "streamed"])
def test_daily_water_uses_run_coverage(frames, tmp_path, start, steps, writer):
    """Full, partial first and partial last days use counted model steps."""
    _, state = frames
    columns = pd.MultiIndex.from_tuples(
        [(v.group, v.name) for v in get_output_contract_catalogue().variables
         if v.group == "DailyState" and v.interval_water_flux], names=["group", "var"],
    )
    dates = pd.date_range(f"2012-01-01 {start}", periods=steps, freq="5min")
    index = pd.MultiIndex.from_product([[1], dates], names=["grid", "datetime"])
    frame = pd.DataFrame(float("nan"), index=index, columns=columns)
    amount = 7.25
    frame.iloc[-1] = amount
    expected = amount / (steps * 300)
    converted = SUEWSOutput(frame, state).to_units(dict.fromkeys(columns, "kg m-2 s-1"))
    assert converted.df.dropna().to_numpy().ravel() == pytest.approx([expected] * len(columns))
    if writer == "text":
        paths = save_df_output(frame, df_state_final=state, freq_s=3600, path_dir_save=tmp_path, output_groups=["DailyState"])
        saved = pd.read_csv(paths[0], sep=r"\s+")
        assert saved[list(columns.get_level_values(1))].to_numpy().ravel() == pytest.approx([expected] * len(columns))
        metadata = json.loads(paths[0].with_name(paths[0].name + ".units.json").read_text(encoding="utf-8"))
        assert list(metadata["daily_coverage_s"].values()) == [steps * 300]
        assert metadata["columns"]["P_day"]["interval"] == "covered day"
    else:
        stream = None
        if writer == "streamed":
            stream = StreamedParquetParts(tmp_path / "SUEWS_output.parquet", tstep_s=300)
            stream.prepare()
            stream.start_chunk(2012, 0)
        paths = save_df_output_parquet(frame, state, freq_s=3600, path_dir_save=tmp_path,
                                      save_state=False, streamed_parquet=stream)
        if stream:
            stream.finish()
        saved = pd.read_parquet(paths[0])
        assert saved.dropna().to_numpy().ravel() == pytest.approx([expected] * len(columns))
        assert list(saved.attrs["supy_output_daily_coverage_s"].values()) == [steps * 300]


@pytest.mark.parametrize("user_value", ["private note", object()])
def test_parquet_only_persists_owned_attrs(frames, tmp_path, user_value):
    frame, state = frames
    frame.attrs["user_note"] = user_value
    paths = save_df_output_parquet(frame, state, path_dir_save=tmp_path, save_state=False)
    saved = pd.read_parquet(paths[0])
    assert "user_note" not in saved.attrs
    schema = pq.read_schema(paths[0])
    assert b"user_note" not in schema.metadata[b"PANDAS_ATTRS"]
    assert b"user_note" not in schema.metadata[b"pandas"]
    assert "user_note" in frame.attrs


def test_streamed_folder_retains_later_partial_coverage(frames, tmp_path):
    """Folder reads and individual parts preserve the later quarter-hour bin."""
    frame, state = frames
    stream = StreamedParquetParts(tmp_path / "SUEWS_output.parquet", tstep_s=300)
    stream.prepare()
    chunks = [frame.iloc[:12].copy(), frame.iloc[12:15].copy()]
    chunks[1][("SUEWS", "Rain")] = 1.0
    for number, chunk in enumerate(chunks):
        stream.start_chunk(2012, number)
        save_df_output_parquet(chunk, state, freq_s=3600, path_dir_save=tmp_path,
                               save_state=False, streamed_parquet=stream)
    paths = stream.finish()
    for path in [paths[0], *sorted(paths[0].glob("part-*.parquet"))]:
        saved = pd.read_parquet(path)
        result = SUEWSOutput(saved, state).resample("D")
        amounts = 6 if len(saved) == 1 and saved.index[0][1].hour == 1 else 3
        seconds = 3600 if amounts == 6 else 900
        if len(saved) == 2:
            amounts, seconds = 9, 4500
        assert result.df[("SUEWS", "Rain")].iloc[0] == pytest.approx(amounts / seconds)
    later = pd.read_parquet(sorted(paths[0].glob("part-*.parquet"))[1])
    assert later[("SUEWS", "Rain")].iloc[0] == pytest.approx(1 / 300)
    assert later.attrs["supy_output_coverage_s"]["1|2012-01-01T02:00:00"] == 900


@pytest.mark.parametrize("writer", [save_df_output, save_df_output_parquet])
def test_calendar_save_tstep_has_actionable_error(frames, tmp_path, writer):
    frame, state = frames
    monthly = SUEWSOutput(frame, state).resample("ME")
    with pytest.raises(ValueError, match=r"save_tstep=True conflicts with calendar-resampled output.*native output"):
        writer(monthly.df, df_state_final=state, path_dir_save=tmp_path, save_tstep=True)



@pytest.mark.parametrize("regular", [False, True])
def test_daily_water_without_model_step_defers_error_until_conversion(tmp_path, regular):
    """Legacy frames remain readable, but daily rates need the model timestep."""
    dates = pd.date_range("2012-01-01 00:05", periods=4, freq="5min")
    if not regular:
        dates = dates[[0, 1, 3]]
    frame = pd.DataFrame(
        float("nan"),
        index=pd.MultiIndex.from_product([[1], dates], names=["grid", "datetime"]),
        columns=pd.MultiIndex.from_tuples(
            [("DailyState", "P_day")], names=["group", "var"],
        ),
    )
    frame.iloc[-1, 0] = 9.0
    output = SUEWSOutput(frame, None)
    pd.testing.assert_frame_equal(output.df, frame)
    assert output.df.attrs.get("supy_output_daily_coverage_s") is None
    message = (
        "DailyState water rates require daily coverage; retain native run rows "
        "and the model timestep before saving or resampling"
    )
    with pytest.raises(ValueError, match=message):
        output.to_units({"P_day": "mm h-1"})
    with pytest.raises(ValueError, match=message):
        output.resample("h").resample("D").to_units({"P_day": "mm h-1"})
    for writer in (save_df_output, save_df_output_parquet):
        with pytest.raises(ValueError, match=message):
            writer(output.df, df_state_final=None, path_dir_save=tmp_path)


def test_daily_coverage_counts_rows_with_configured_step(frames):
    """An irregular selection uses the held timestep, never gaps in its index."""
    frame, state = frames
    frame = frame.iloc[[0, 3, 8]].copy()
    frame.index = frame.index.remove_unused_levels()
    frame[("DailyState", "P_day")] = [float("nan"), float("nan"), 9.0]
    output = SUEWSOutput(frame, state).to_units({"P_day": "kg m-2 s-1"})
    assert output.df[("DailyState", "P_day")].dropna().iloc[0] == pytest.approx(9 / 900)



def test_daily_coverage_survives_selecting_sparse_output_rows(frames):
    frame, state = frames
    frame[("DailyState", "P_day")] = float("nan")
    frame.loc[frame.index[-1], ("DailyState", "P_day")] = 12.0
    output = SUEWSOutput(frame, state)
    daily = output.df.loc[:, [("DailyState", "P_day")]].dropna()
    converted = SUEWSOutput(daily, state).to_units({"P_day": "mm h-1"})
    assert converted.df.iloc[0, 0] == pytest.approx(6.0)



@pytest.mark.parametrize("writer", ["text", "parquet"])
def test_daily_coverage_varies_across_one_run(frames, tmp_path, writer):
    """Partial first and last days surround a full day in the same output."""
    _, state = frames
    dates = pd.date_range("2012-01-01 12:00", "2012-01-03 11:55", freq="5min")
    columns = pd.MultiIndex.from_tuples([("DailyState", "P_day")], names=["group", "var"])
    frame = pd.DataFrame(float("nan"), index=pd.MultiIndex.from_product([[1], dates], names=["grid", "datetime"]), columns=columns)
    for date in ["2012-01-01 23:55", "2012-01-02 23:55", "2012-01-03 11:55"]:
        frame.loc[(1, pd.Timestamp(date)), ("DailyState", "P_day")] = 8.64
    if writer == "text":
        paths = save_df_output(frame, df_state_final=state, path_dir_save=tmp_path, output_groups=["DailyState"])
        values = pd.read_csv(paths[0], sep=r"\s+")["P_day"]
    else:
        paths = save_df_output_parquet(frame, state, path_dir_save=tmp_path, save_state=False)
        values = pd.read_parquet(paths[0])["DailyState", "P_day"].dropna()
    assert values.to_numpy() == pytest.approx([0.0002, 0.0001, 0.0002])
