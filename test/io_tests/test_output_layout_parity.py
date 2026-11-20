"""Observable output writer layout parity tests."""

from importlib.resources import as_file, files
import os
from pathlib import Path
import subprocess
import warnings

import numpy as np
import pandas as pd
import pyarrow as pa
from pyarrow import ipc
import pyarrow.parquet as pq
import pytest
import yaml

from supy._post import gen_index  # noqa: PLC2701
from supy._run_rust import config_to_engine_yaml  # noqa: PLC2701
from supy._save import (
    StreamedParquetParts,  # noqa: PLC2701
    save_df_grid_group,  # noqa: PLC2701
    save_df_output_parquet,  # noqa: PLC2701
)
from supy.data_model import SUEWSConfig

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
    # Water fluxes are written as interval-mean rates (output contract 2.0.0):
    # 1.5 mm over a 300 s step is 0.005 kg m-2 s-1. Other columns round-trip
    # exactly.
    rain = ("SUEWS", "Rain")
    others = [column for column in columns if column != rain]
    pd.testing.assert_frame_equal(saved[others], output[others], check_exact=True)
    np.testing.assert_allclose(saved[rain], output[rain] / 300, rtol=1e-12, atol=0)
    assert pd.isna(saved.loc[(7, index.levels[1][1]), ("SUEWS", "QH")])
    assert arrow_table.column("('SUEWS', 'QH')").null_count == 1


def _locate_suews_engine() -> Path:
    """Return the bundled or locally built ``suews-engine`` binary."""
    binary_name = "suews-engine.exe" if os.name == "nt" else "suews-engine"
    bundled = files("supy").joinpath("bin").joinpath(binary_name)
    with as_file(bundled) as bundled_path:
        if bundled_path.exists():
            return Path(bundled_path)

    repo_root = Path(__file__).resolve().parents[2]
    dev_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / binary_name
    if dev_binary.exists():
        return dev_binary
    pytest.skip("Rust CLI binary not found; build with: make bridge")


def _write_short_engine_inputs(run_dir: Path) -> Path:
    """Write a 2-row forcing prefix and resolved config for ``suews-engine run``."""
    sample_dir = files("supy").joinpath("sample_data")
    with sample_dir.joinpath("sample_config.yml").open(encoding="utf-8") as handle:
        cfg = yaml.safe_load(handle)

    control = cfg["model"]["control"]
    control["output"]["dir"] = str(run_dir)
    forcing_entry = control.get("forcing", {}).get("file")
    forcing_name = (
        forcing_entry["value"] if isinstance(forcing_entry, dict) else forcing_entry
    )
    assert isinstance(forcing_name, str) and forcing_name

    forcing_lines = [
        line
        for line in sample_dir.joinpath(forcing_name)
        .read_text(encoding="utf-8")
        .splitlines()
        if line.strip()
    ]
    (run_dir / forcing_name).write_text(
        "\n".join(forcing_lines[:3]) + "\n", encoding="utf-8"
    )
    config_path = run_dir / "sample_config.yml"
    config_path.write_text(
        config_to_engine_yaml(SUEWSConfig.from_dict(cfg)), encoding="utf-8"
    )
    return config_path


def _read_arrow_schema_summary(
    arrow_path: Path,
) -> tuple[list[str], int, bytes | None, list[pa.DataType], list[bool]]:
    """Read schema details from an Arrow IPC file and release the mmap cleanly."""
    with pa.memory_map(str(arrow_path), "rb") as source:
        reader = ipc.open_file(source)
        schema = reader.schema
        table = reader.read_all()
        summary = (
            list(table.column_names),
            table.num_rows,
            (schema.metadata or {}).get(b"group_layout"),
            [field.type for field in schema],
            [field.nullable for field in schema],
        )
        del table, reader
    return summary


def test_engine_arrow_output_matches_suews_registry(tmp_path: Path):
    """Verify real engine Arrow IPC schema matches SuPy's canonical output registry."""
    engine_bin = _locate_suews_engine()
    config_path = _write_short_engine_inputs(tmp_path)

    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode == 0, (
        f"suews-engine exited with {result.returncode}: {result.stderr}"
    )

    arrow_path = tmp_path / "suews_output.arrow"
    assert arrow_path.is_file()

    (
        column_names,
        num_rows,
        group_layout_raw,
        field_types,
        field_nullables,
    ) = _read_arrow_schema_summary(arrow_path)

    assert num_rows > 0
    assert all(dtype == pa.float64() for dtype in field_types)
    assert all(not nullable for nullable in field_nullables)

    expected_suews = list(gen_index("dataoutlinesuews").get_level_values(1))
    assert len(expected_suews) == 113
    assert column_names[:5] == ["iy", "id", "it", "imin", "dectime"]
    assert column_names[5:118] == expected_suews

    assert group_layout_raw is not None
    parsed_groups = [
        (name, int(width))
        for name, width in (
            entry.split(":", 1) for entry in group_layout_raw.decode("utf-8").split(",")
        )
    ]
    assert sum(width for _, width in parsed_groups) == len(column_names)
    for group_name, width in parsed_groups:
        expected_data_cols = len(gen_index(f"dataoutline{group_name.lower()}"))
        assert width == expected_data_cols + 5, (
            f"group '{group_name}' width {width} != {expected_data_cols} + 5"
        )
