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
    assert saved[("SUEWS", "Rain")].sum() == 12
    if frequency == 300:
        pd.testing.assert_frame_equal(saved, df_output)


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
    assert metadata[b"cell_methods"] == b"time: sum (interval: 5 minutes)"
    pd.testing.assert_frame_equal(pd.read_parquet(paths[0]), df_output)


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
