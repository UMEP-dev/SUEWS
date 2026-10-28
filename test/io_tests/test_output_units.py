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
    frame = pd.DataFrame(86400.0, index=[0], columns=columns)
    result = prepare_output(frame, interval_s=300, write=True)
    assert (result == 1.0).all().all()
    assert (prepare_output(result, interval_s=300, write=True) == 1.0).all().all()
    from supy._save import save_df_grid_group

    daily = frame["DailyState"].copy()
    daily.index = pd.date_range("2012-01-02", periods=1, freq="D")
    path = save_df_grid_group(daily, 1, "DailyState", tmp_path, "site", freq_s=86400)
    saved = pd.read_csv(path, sep=r"\s+")
    assert (saved[daily.columns] == 1.0).all().all()
    metadata = json.loads(path.with_name(path.name + ".units.json").read_text(encoding="utf-8"))
    assert metadata["columns"]["P_day"]["cell_methods"] == "time: mean (interval: 1 day)"



def test_single_row_rate_request_uses_state_interval(frames):
    frame, state = frames
    single = frame.iloc[:1].copy()
    single.index = single.index.remove_unused_levels()
    converted = SUEWSOutput(single, state).to_units({"Rain": "mm h-1"})
    assert converted.df[("SUEWS", "Rain")].iloc[0] == pytest.approx(6.0)
