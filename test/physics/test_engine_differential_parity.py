"""Live differential parity between Python ``SUEWSSimulation`` and ``suews-engine``.

Exercises the two-layer execution contract (``W2-3``, #2100) across the coupled
physics regimes defined in ``test/data_model/_physics_anchors.py``:

1. Six coupled configurations execute live through both ``SUEWSSimulation``
   (Python -> PyO3/Rust -> Fortran) and the standalone ``suews-engine run`` CLI
   (resolved YAML -> Rust -> Fortran -> Arrow IPC) on the exact same 5-minute
   forcing file.
2. Every test asserts Arrow schema metadata (``group_layout`` across all 11
   output groups and the 118 ``SUEWS`` column names), exact timestamp alignment,
   bit-identical values across all 1,302 data columns (with exact ``-999.0`` ->
   ``NaN`` sentinel mask parity), and a live active-physics witness proving the
   target subsystem ran.
3. The unmodified ``observed_and_dyohm`` anchor (``soil_moisture = "observed"``)
   verifies the boundary contract that ``SUEWSSimulation`` performs canonical
   soil-content preprocessing while standalone ``suews-engine`` refuses raw
   observed soil moisture explicitly.
4. A negative control mutates a real Arrow output buffer (finite drift and
   sentinel flip) to verify the comparator rejects any single-cell divergence.
"""

from __future__ import annotations

import copy
from datetime import datetime, timedelta
from importlib import import_module
from importlib.resources import files
import importlib.util
from pathlib import Path
import subprocess
import sys
from typing import Any

import numpy as np
import pandas as pd
import pyarrow as pa
from pyarrow import ipc
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig
from supy.data_model.output.registry import OUTPUT_REGISTRY
from supy.data_model.output.variables import OutputGroup

_run_rust = import_module("supy._run_rust")
OUTPUT_GROUP_LAYOUT: list[tuple[str, int]] = _run_rust.OUTPUT_GROUP_LAYOUT
OUTPUT_TIME_COLS: int = _run_rust.OUTPUT_TIME_COLS
config_to_engine_yaml = _run_rust.config_to_engine_yaml


def _load_physics_anchors():
    module_name = "_suews_test_physics_anchors"
    existing = sys.modules.get(module_name)
    if existing is not None:
        return existing
    anchor_path = (
        Path(__file__).resolve().parents[1] / "data_model" / "_physics_anchors.py"
    )
    spec = importlib.util.spec_from_file_location(module_name, anchor_path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module


_physics_anchors = _load_physics_anchors()
build_anchor_dict = _physics_anchors.build_anchor_dict

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract("C-PARITY", "C-OUTPUT", "C-CLOCK"),
]

_TIME_COLS = ("iy", "id", "it", "imin", "dectime")
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)


@pytest.fixture(scope="module")
def sample_forcing_header() -> list[str]:
    """Column header from the packaged sample forcing file."""
    raw = (
        files("supy")
        .joinpath("sample_data", "Kc_2012_data_60.txt")
        .read_text(encoding="utf-8")
    )
    for line in raw.splitlines():
        if line.strip():
            return line.split()
    raise AssertionError("Sample forcing file has no header row")


def _write_5min_forcing(
    df_forcing: pd.DataFrame, header: list[str], out_path: Path
) -> None:
    """Serialise an in-memory 5-minute forcing frame to standard SUEWS text format."""
    col_map = {str(col).lower(): col for col in df_forcing.columns}
    lines = [" ".join(header)]
    for _, row in df_forcing.iterrows():
        values: list[str] = []
        for col in header:
            low = col.lower()
            if low in {"iy", "id", "it", "imin"}:
                values.append(str(round(float(row[col_map[low]]))))
            elif low in col_map:
                val = float(row[col_map[low]])
                if low == "pres" and np.isfinite(val) and val > -900.0:
                    # Text forcing stores pressure in kPa; in-memory frames use hPa.
                    val /= 10.0
                values.append(
                    "-999" if not np.isfinite(val) or val <= -900.0 else f"{val:.6f}"
                )
            else:
                values.append("-999")
        lines.append(" ".join(values))
    out_path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def _read_engine_arrow_full(
    arrow_path: Path,
) -> tuple[pd.DatetimeIndex, np.ndarray, list[str], list[tuple[str, int]]]:
    """Read timestamps, all 11 concatenated data groups, column names, and layout."""
    with pa.memory_map(str(arrow_path), "rb") as source:
        reader = ipc.open_file(source)
        schema = reader.schema
        table = reader.read_all()

        assert all(field.type == pa.float64() for field in schema), (
            "Engine Arrow output must emit Float64 fields"
        )
        assert all(not field.nullable for field in schema), (
            "Engine Arrow output must emit non-nullable fields"
        )

        layout_raw = (schema.metadata or {}).get(b"group_layout")
        assert layout_raw is not None, "Engine Arrow schema missing group_layout"
        parsed_layout = [
            (name, int(width))
            for name, width in (
                entry.split(":", 1) for entry in layout_raw.decode("utf-8").split(",")
            )
        ]

        column_names = list(table.column_names)
        raw_columns = [
            table.column(i).to_numpy(zero_copy_only=False).copy()
            for i in range(table.num_columns)
        ]
        del table, reader

    iy, idoy, it, imin = raw_columns[0], raw_columns[1], raw_columns[2], raw_columns[3]
    timestamps = pd.DatetimeIndex(
        [
            datetime(int(year), 1, 1)
            + timedelta(days=int(doy) - 1, hours=int(hour), minutes=int(minute))
            for year, doy, hour, minute in zip(iy, idoy, it, imin, strict=True)
        ],
        name="datetime",
    )
    full_matrix = np.column_stack(raw_columns)

    data_slices: list[np.ndarray] = []
    offset = 0
    for _, width in parsed_layout:
        data_slices.append(full_matrix[:, offset + OUTPUT_TIME_COLS : offset + width])
        offset += width
    assert offset == full_matrix.shape[1], (
        f"group_layout sum {offset} != Arrow column count {full_matrix.shape[1]}"
    )

    eng_data = np.hstack(data_slices)
    return timestamps, eng_data, column_names, parsed_layout


def _assert_full_buffer_bit_identical(
    py_df: pd.DataFrame,
    eng_timestamps: pd.DatetimeIndex,
    eng_raw_data: np.ndarray,
    *,
    case_name: str,
) -> None:
    """Assert exact timestamp and bitwise 11-group data parity (with -999 -> NaN)."""
    assert py_df.index.is_unique, f"{case_name}: duplicate Python output timestamps"
    assert py_df.index.equals(eng_timestamps), (
        f"{case_name}: timestamp mismatch between Python ({py_df.index[0]}.."
        f"{py_df.index[-1]}, n={len(py_df)}) and engine ({eng_timestamps[0]}.."
        f"{eng_timestamps[-1]}, n={len(eng_timestamps)})"
    )

    py_data = py_df.to_numpy(dtype=np.float64)
    eng_data = np.where(eng_raw_data == -999, np.nan, eng_raw_data)
    assert py_data.shape == eng_data.shape, (
        f"{case_name}: shape mismatch py={py_data.shape} vs eng={eng_data.shape}"
    )

    py_nan = np.isnan(py_data)
    eng_nan = np.isnan(eng_data)
    both_nan = py_nan & eng_nan
    equal = (py_data.view(np.uint64) == eng_data.view(np.uint64)) | both_nan
    if not equal.all():
        row, col = np.argwhere(~equal)[0]
        diff_cols = list(py_df.columns[(~equal).any(axis=0)])
        raise AssertionError(
            f"{case_name}: parity drift at {py_df.columns[col]} @ {py_df.index[row]}: "
            f"py={py_data[row, col]!r}, eng={eng_data[row, col]!r}, "
            f"eng_raw={eng_raw_data[row, col]!r}; "
            f"differing_columns={diff_cols[:10]!r} (total {len(diff_cols)})"
        )


def _prepare_parity_case(
    case_name: str, df_full: pd.DataFrame
) -> tuple[dict[str, Any], pd.DataFrame]:
    """Build raw config dict and 5-minute forcing slice for one parity regime."""
    if case_name == "all_surfaces_baseline":
        cfg = build_anchor_dict("all_surfaces_baseline")
        # Daytime 4-hour window so solar radiation and net radiation vary actively.
        df = df_full.iloc[120:168].copy()
    elif case_name == "co2_methods":
        cfg = build_anchor_dict("co2_methods")
        df = df_full.iloc[120:168].copy()
    elif case_name == "snow_cold":
        cfg = build_anchor_dict("snow_cold")
        df = df_full.iloc[120:168].copy()
        df["Tair"] = -5.0
        df["rain"] = 0.1
        df["RH"] = 80.0
        df["U"] = 3.0
        df["pres"] = 1013.0
    elif case_name == "spartacus_ehc_stebbs":
        cfg = build_anchor_dict("spartacus_ehc_stebbs")
        df = df_full.iloc[120:168].copy()
    elif case_name == "dyohm_observed_inputs":
        cfg = build_anchor_dict("observed_and_dyohm")
        # Standalone suews-engine requires modelled soil moisture (tested separately
        # in test_engine_refuses_observed_soil_moisture_anchor); keep DyOHM plus
        # observed Ldown, water use, and LAI active.
        cfg["model"]["physics"]["soil_moisture"] = "modelled"
        # 48-hour window extends past day-1 DyOHM coefficient spin-up so day-2 QS
        # diverges cleanly from an otherwise identical ordinary-OHM control.
        df = df_full.iloc[:576].copy()
        df["ldown"] = 320.0
        df["Wuh"] = 0.05
        df["lai"] = 2.5
    elif case_name == "irrigation_and_dls":
        cfg = build_anchor_dict("irrigation_and_dls")
        # Two summer days inside ie_start=100..ie_end=280 and startdls=86..enddls=303
        # so daily update_WaterUse activates evening irrigation on day 2.
        df = df_full.loc["2012-07-01 00:05:00":"2012-07-03 00:00:00"].copy()
        df["rain"] = 0.0
    else:
        raise KeyError(f"Unknown parity case: {case_name!r}")
    return cfg, df


def _assert_active_physics_witness(
    case_name: str,
    py_df: pd.DataFrame,
    *,
    ohm_control: pd.DataFrame | None = None,
) -> None:
    """Confirm each coupled parity case genuinely activated its target physics."""
    if case_name == "all_surfaces_baseline":
        series = py_df["SUEWS", "QN"]
        assert np.isfinite(series).all() and series.nunique() > 1
    elif case_name == "co2_methods":
        photo = py_df["SUEWS", "FcPhoto"]
        point = py_df["SUEWS", "FcPoint"]
        assert np.isfinite(photo).all() and photo.ne(0).any()
        assert np.isfinite(point).all() and (point > 0.0).all()
    elif case_name == "snow_cold":
        swe = py_df["SUEWS", "SWE"]
        assert np.isfinite(swe).all() and (swe > 0.0).any()
    elif case_name == "spartacus_ehc_stebbs":
        knet = py_df["SPARTACUS", "KNet_Grnd"]
        ts_roof = py_df["EHC", "Ts_Roof_1"]
        tind = py_df["STEBBS", "Tair_ind"]
        assert np.isfinite(knet).all() and knet.nunique() > 1
        assert np.isfinite(ts_roof).all() and ts_roof.nunique() > 1
        assert np.isfinite(tind).all() and tind.nunique() > 1
    elif case_name == "dyohm_observed_inputs":
        qs = py_df["SUEWS", "QS"]
        ldown = py_df["SUEWS", "Ldown"]
        irr = py_df["SUEWS", "Irr"]
        lai = py_df["SUEWS", "LAI"]
        assert np.isfinite(qs).all() and qs.nunique() > 1
        assert ohm_control is not None, "Missing ordinary-OHM control for DyOHM check"
        pd.testing.assert_index_equal(py_df.index, ohm_control.index)
        warm_mask = py_df.index >= (py_df.index.min() + pd.Timedelta(days=1))
        assert warm_mask.any(), "DyOHM window must extend past day-1 spin-up"
        max_qs_delta = float(
            np.max(
                np.abs(qs.to_numpy() - ohm_control["SUEWS", "QS"].to_numpy())[warm_mask]
            )
        )
        assert max_qs_delta > 1.0e-4, (
            "DyOHM storage_heat=6 did not diverge from ordinary OHM after day-1 "
            f"spin-up (max |delta QS| = {max_qs_delta:.3e})"
        )
        np.testing.assert_allclose(ldown.to_numpy(), 320.0)
        np.testing.assert_allclose(irr.to_numpy(), 0.05)
        assert np.isfinite(lai).all() and np.isclose(float(lai.iloc[-1]), 2.5)
    elif case_name == "irrigation_and_dls":
        irr = py_df["SUEWS", "Irr"]
        assert np.isfinite(irr).all() and (irr > 0.0).any()
    else:
        raise KeyError(f"Unknown parity case: {case_name!r}")


def _run_paired_python_and_engine(
    case_dir: Path,
    cfg_dict: dict[str, Any],
    df_forcing: pd.DataFrame,
    header: list[str],
    engine_bin: Path,
) -> tuple[
    pd.DataFrame, pd.DatetimeIndex, np.ndarray, list[str], list[tuple[str, int]]
]:
    """Run identical 5-minute forcing and config through Python and ``suews-engine``."""
    forcing_path = case_dir / "forcing_5.txt"
    _write_5min_forcing(df_forcing, header, forcing_path)

    out_dir = case_dir / "engine_out"
    control = cfg_dict["model"]["control"]
    control["forcing"]["file"] = forcing_path.name
    control["start_time"] = None
    control["end_time"] = None
    control["output"]["dir"] = str(out_dir)

    source_path = case_dir / "source.yml"
    source_path.write_text(
        yaml.dump(cfg_dict, sort_keys=False, Dumper=_YAML_DUMPER),
        encoding="utf-8",
    )
    resolved_path = case_dir / "resolved.yml"
    resolved_path.write_text(
        config_to_engine_yaml(SUEWSConfig.from_dict(cfg_dict)),
        encoding="utf-8",
    )

    sim = SUEWSSimulation(source_path)
    py_df = sim.run(n_jobs=1).df.droplevel("grid")

    proc = subprocess.run(
        [str(engine_bin), "run", str(resolved_path)],
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
    )
    assert proc.returncode == 0, (
        f"suews-engine failed (rc={proc.returncode}): {proc.stderr}"
    )

    arrow_path = out_dir / "suews_output.arrow"
    assert arrow_path.is_file(), f"Missing Arrow output at {arrow_path}"
    eng_timestamps, eng_raw_data, column_names, parsed_layout = _read_engine_arrow_full(
        arrow_path
    )
    return py_df, eng_timestamps, eng_raw_data, column_names, parsed_layout


@pytest.mark.parametrize(
    "case_name",
    [
        "all_surfaces_baseline",
        "co2_methods",
        "snow_cold",
        "spartacus_ehc_stebbs",
        "dyohm_observed_inputs",
        "irrigation_and_dls",
    ],
)
def test_live_python_and_engine_bit_identical_across_coupled_anchors(
    case_name: str,
    tmp_path: Path,
    sample_data_loaded,
    sample_forcing_header: list[str],
    suews_engine_bin: Path,
) -> None:
    """Python SUEWSSimulation and standalone suews-engine match bit-for-bit across all 11 groups."""
    _, df_full = sample_data_loaded
    cfg_dict, df_forcing = _prepare_parity_case(case_name, df_full)

    py_df, eng_timestamps, eng_raw_data, column_names, parsed_layout = (
        _run_paired_python_and_engine(
            tmp_path, cfg_dict, df_forcing, sample_forcing_header, suews_engine_bin
        )
    )

    assert parsed_layout == OUTPUT_GROUP_LAYOUT
    expected_suews = [v.name for v in OUTPUT_REGISTRY.by_group(OutputGroup.SUEWS)]
    assert column_names[: len(_TIME_COLS)] == list(_TIME_COLS)
    assert column_names[len(_TIME_COLS) : len(_TIME_COLS) + len(expected_suews)] == (
        expected_suews
    )

    _assert_full_buffer_bit_identical(
        py_df, eng_timestamps, eng_raw_data, case_name=case_name
    )
    ohm_control: pd.DataFrame | None = None
    if case_name == "dyohm_observed_inputs":
        ohm_cfg = copy.deepcopy(cfg_dict)
        ohm_cfg["model"]["physics"]["storage_heat"] = 1
        ohm_path = tmp_path / "ohm_control.yml"
        ohm_path.write_text(
            yaml.dump(ohm_cfg, sort_keys=False, Dumper=_YAML_DUMPER),
            encoding="utf-8",
        )
        ohm_control = SUEWSSimulation(ohm_path).run(n_jobs=1).df.droplevel("grid")
        with pytest.raises(AssertionError, match="ordinary OHM"):
            _assert_active_physics_witness(
                case_name, ohm_control, ohm_control=ohm_control
            )
    _assert_active_physics_witness(case_name, py_df, ohm_control=ohm_control)


def test_engine_refuses_observed_soil_moisture_anchor(
    tmp_path: Path,
    sample_data_loaded,
    sample_forcing_header: list[str],
    suews_engine_bin: Path,
) -> None:
    """Standalone suews-engine refuses observed soil moisture while Python converts canonical sm."""
    _, df_full = sample_data_loaded

    cfg_dict = build_anchor_dict("observed_and_dyohm")
    df_forcing = df_full.iloc[120:132].copy()
    df_forcing["ldown"] = 320.0
    df_forcing["sm"] = 80.0
    df_forcing["Wuh"] = 0.05
    df_forcing["lai"] = 2.5

    forcing_path = tmp_path / "forcing_5.txt"
    _write_5min_forcing(df_forcing, sample_forcing_header, forcing_path)

    out_dir = tmp_path / "engine_out"
    control = cfg_dict["model"]["control"]
    control["forcing"]["file"] = forcing_path.name
    control["start_time"] = None
    control["end_time"] = None
    control["output"]["dir"] = str(out_dir)

    source_path = tmp_path / "source.yml"
    source_path.write_text(
        yaml.dump(cfg_dict, sort_keys=False, Dumper=_YAML_DUMPER),
        encoding="utf-8",
    )
    resolved_path = tmp_path / "resolved.yml"
    resolved_path.write_text(
        config_to_engine_yaml(SUEWSConfig.from_dict(cfg_dict)),
        encoding="utf-8",
    )

    py_df = SUEWSSimulation(source_path).run(n_jobs=1).df.droplevel("grid")
    assert len(py_df) == len(df_forcing)
    np.testing.assert_allclose(py_df["SUEWS", "SMD"].to_numpy(), 40.0)

    proc = subprocess.run(
        [str(suews_engine_bin), "run", str(resolved_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert proc.returncode != 0
    assert "Observed soil moisture requires Python soil-content" in proc.stderr
    assert not (out_dir / "suews_output.arrow").exists()


def test_differential_comparator_rejects_value_and_sentinel_drift(
    tmp_path: Path,
    sample_data_loaded,
    sample_forcing_header: list[str],
    suews_engine_bin: Path,
) -> None:
    """Negative control: single-cell numerical drift or sentinel mismatch fails parity."""
    _, df_full = sample_data_loaded
    cfg_dict = build_anchor_dict("all_surfaces_baseline")
    df_forcing = df_full.iloc[120:132].copy()

    py_df, eng_timestamps, eng_raw_data, _, _ = _run_paired_python_and_engine(
        tmp_path, cfg_dict, df_forcing, sample_forcing_header, suews_engine_bin
    )
    _assert_full_buffer_bit_identical(
        py_df, eng_timestamps, eng_raw_data, case_name="control"
    )

    qn_col = py_df.columns.get_loc(("SUEWS", "QN"))
    drifted = eng_raw_data.copy()
    drifted[3, qn_col] += 1e-12
    with pytest.raises(AssertionError, match=r"\('SUEWS', 'QN'\).*2012-01-01 10:20:00"):
        _assert_full_buffer_bit_identical(
            py_df, eng_timestamps, drifted, case_name="value_drift"
        )

    sentinel_flip = eng_raw_data.copy()
    sentinel_flip[3, qn_col] = -999.0
    with pytest.raises(AssertionError, match=r"\('SUEWS', 'QN'\).*2012-01-01 10:20:00"):
        _assert_full_buffer_bit_identical(
            py_df, eng_timestamps, sentinel_flip, case_name="sentinel_drift"
        )
