from importlib import import_module
from importlib.resources import files
from pathlib import Path

import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig
from supy.data_model.core.model import ModelPhysics, SMDMethod
from supy.data_model.core.type import RefValue

pytestmark = [pytest.mark.api, pytest.mark.contract("C-FORCING", "C-OUTPUT")]

_run_rust = import_module("supy._run_rust")


class FakeRustModule:
    def __init__(self, group_ncolumns):
        self._group_ncolumns = group_ncolumns

    def output_group_ncolumns(self):
        return self._group_ncolumns


@pytest.fixture(autouse=True)
def reset_output_layout_validation(monkeypatch):
    _run_rust._validate_output_layout.cache_clear()
    yield
    _run_rust._validate_output_layout.cache_clear()


def test_validate_output_layout_ignores_registry_only_groups(monkeypatch):
    monkeypatch.setattr(_run_rust, "_GROUP_ORDER", ("SUEWS", "DailyState"))
    monkeypatch.setattr(
        _run_rust,
        "_GROUP_DATA_COLS_BY_NAME",
        {"SUEWS": 2, "DailyState": 3},
    )

    rust_module = FakeRustModule([
        ("datetime", _run_rust.OUTPUT_TIME_COLS),
        ("SUEWS", 2),
        ("DailyState", 3),
        ("BL", 999),
    ])

    _run_rust._validate_output_layout(rust_module)

    assert _run_rust._validate_output_layout.cache_info().currsize == 1


def test_validate_output_layout_reports_generated_registry_path(monkeypatch):
    monkeypatch.setattr(_run_rust, "_GROUP_ORDER", ("SUEWS", "DailyState"))
    monkeypatch.setattr(
        _run_rust,
        "_GROUP_DATA_COLS_BY_NAME",
        {"SUEWS": 2, "DailyState": 3},
    )

    rust_module = FakeRustModule([
        ("datetime", _run_rust.OUTPUT_TIME_COLS),
        ("SUEWS", 2),
    ])

    with pytest.raises(RuntimeError, match="DailyState") as exc_info:
        _run_rust._validate_output_layout(rust_module)

    assert "src/supy/data_model/output/dailystate_vars.py" in str(exc_info.value)


def test_parse_output_block_preserves_registry_layout_and_missing_values():
    """Label emitted groups in exact registry order and expose sentinels as NaN."""
    output = np.zeros((_run_rust.OUTPUT_ALL_COLS,), dtype=np.float64)
    output[:4] = [2012, 1, 0, 5]

    offset = 0
    for group_ordinal, (_, ncols) in enumerate(_run_rust.OUTPUT_GROUP_LAYOUT):
        data_start = offset + _run_rust.OUTPUT_TIME_COLS
        output[data_start : offset + ncols] = group_ordinal + 1
        offset += ncols
    output[_run_rust.OUTPUT_TIME_COLS] = -999.0

    parsed = _run_rust._parse_output_block(output.tolist(), len_sim=1, grid_id=7)
    expected_columns = [
        (group, variable)
        for group, _ in _run_rust.OUTPUT_GROUP_LAYOUT
        for variable in _run_rust.df_var.xs(group, level="group").index
    ]

    assert parsed.columns.to_list() == expected_columns
    assert parsed.columns.names == ["group", "var"]
    assert parsed.index.names == ["grid", "datetime"]
    assert pd.isna(parsed.iloc[0, 0])


def _minimal_forcing_frame() -> pd.DataFrame:
    index = pd.date_range("2011-01-01 00:05", periods=2, freq="5min")
    return pd.DataFrame(
        {
            "qn": [0.0, 0.0],
            "qh": [0.0, 0.0],
            "qe": [0.0, 0.0],
            "qs": [0.0, 0.0],
            "qf": [0.0, 0.0],
            "U": [2.0, 2.1],
            "RH": [80.0, 79.0],
            "Tair": [18.0, 18.5],
            "pres": [1000.0, 1001.0],
            "rain": [0.0, 0.0],
            "kdown": [100.0, 120.0],
            "snow": [0.0, 0.0],
            "ldown": [300.0, 305.0],
            "fcld": [0.0, 0.0],
            "Wuh": [0.0, 0.0],
            "sm": [-999.0, -999.0],
            "lai": [2.0, 2.2],
        },
        index=index,
    )


def test_prepare_forcing_block_broadcasts_bulk_lai_to_three_kernel_columns():
    df_forcing = _minimal_forcing_frame()

    block = _run_rust._prepare_forcing_block(df_forcing)

    assert block.shape == (len(df_forcing), _run_rust.MET_FORCING_COLS)
    np.testing.assert_allclose(block[:, 20], df_forcing["lai"])
    np.testing.assert_allclose(block[:, 21], df_forcing["lai"])
    np.testing.assert_allclose(block[:, 22], df_forcing["lai"])


def test_prepare_forcing_block_prefers_per_vegetation_lai():
    df_forcing = _minimal_forcing_frame()
    df_forcing["lai_evetr"] = [1.1, 1.2]
    df_forcing["lai_dectr"] = [2.1, 2.2]
    df_forcing["lai_grass"] = [3.1, 3.2]

    block = _run_rust._prepare_forcing_block(df_forcing)

    assert block.shape == (len(df_forcing), _run_rust.MET_FORCING_COLS)
    np.testing.assert_allclose(block[:, 20], df_forcing["lai_evetr"])
    np.testing.assert_allclose(block[:, 21], df_forcing["lai_dectr"])
    np.testing.assert_allclose(block[:, 22], df_forcing["lai_grass"])


def test_prepare_forcing_block_broadcasts_bulk_wuh_to_seven_kernel_columns():
    df_forcing = _minimal_forcing_frame()
    df_forcing["Wuh"] = [0.4, 0.5]

    block = _run_rust._prepare_forcing_block(df_forcing)

    assert block.shape == (len(df_forcing), _run_rust.MET_FORCING_COLS)
    for col in range(23, 30):
        np.testing.assert_allclose(block[:, col], df_forcing["Wuh"])


def test_prepare_forcing_block_prefers_per_surface_wuh():
    df_forcing = _minimal_forcing_frame()
    per_surface = {
        "wuh_paved": [0.11, 0.12],
        "wuh_bldgs": [0.21, 0.22],
        "wuh_evetr": [0.31, 0.32],
        "wuh_dectr": [0.41, 0.42],
        "wuh_grass": [0.51, 0.52],
        "wuh_bsoil": [0.61, 0.62],
        "wuh_water": [0.71, 0.72],
    }
    for name, values in per_surface.items():
        df_forcing[name] = values

    block = _run_rust._prepare_forcing_block(df_forcing)

    assert block.shape == (len(df_forcing), _run_rust.MET_FORCING_COLS)
    for offset, name in enumerate(per_surface):
        np.testing.assert_allclose(block[:, 23 + offset], df_forcing[name])


@pytest.mark.parametrize(
    "raw_value",
    [
        RefValue(SMDMethod.MODELLED),
        SMDMethod.MODELLED,
        RefValue(0),
        0,
    ],
)
def test_run_suews_rust_accepts_modelled_soil_moisture_representations(
    raw_value, sample_config_loaded, sample_forcing_parsed
):
    """Real configs with wrapped, bare enum, or plain int soil_moisture=0 run cleanly (#2192)."""
    cfg = sample_config_loaded.model_copy(deep=True)
    cfg.model.physics.soil_moisture = raw_value
    _, df_forcing = sample_forcing_parsed(2)

    df_out, state_json, _ = _run_rust.run_suews_rust(cfg, df_forcing, grid_id=1)

    assert len(df_out) == 2
    assert isinstance(state_json, str) and state_json


@pytest.mark.parametrize(
    "raw_value",
    [
        RefValue(SMDMethod.OBSERVED),
        SMDMethod.OBSERVED,
        RefValue(1),
        1,
    ],
)
def test_run_suews_rust_routes_observed_soil_moisture_representations_to_method_1(
    raw_value, sample_config_loaded, sample_forcing_parsed
):
    """Real observed-soil configs with wrapped, bare enum, or plain int soil_moisture=1 run method=1 (#2192)."""
    cfg = sample_config_loaded.model_copy(deep=True)
    props = cfg.sites[0].properties
    props.soil_observation = {
        "layer_top": 0.0,
        "layer_depth": 0.2,
        "soil_not_rocks": 0.9,
    }
    for surf_name in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil"):
        surf = getattr(props.land_cover, surf_name)
        surf.soil_depth = RefValue(200.0)
        surf.soil_store_capacity = RefValue(80.0)
        surf.soil_field_capacity = RefValue(0.6)
    cfg.model.physics.soil_moisture = raw_value
    _, df_forcing = sample_forcing_parsed(2)
    df_forcing = df_forcing.assign(sm=80.0)

    # Direct _run_rust call without prepared deficits proves soil_moisture_method=1
    # reached _prepare_forcing_block rather than falling back to modelled (0).
    with pytest.raises(
        ValueError, match="Observed soil moisture requires prepared deficits"
    ):
        _run_rust.run_suews_rust(cfg, df_forcing, grid_id=1)

    # Full SUEWSSimulation path converts canonical sm=80.0 against 120 kg m-2 field
    # capacity to 40.0 mm deficit and completes cleanly.
    sim = SUEWSSimulation()
    sim.update_config(cfg, auto_load_forcing=False)
    sim.update_forcing(df_forcing)
    out = sim.run(
        start_date=df_forcing.index[0], end_date=df_forcing.index[-1], n_jobs=1
    ).df
    assert len(out) == 2
    np.testing.assert_allclose(out.loc[1, ("SUEWS", "SMD")].to_numpy(), 40.0)


@pytest.mark.parametrize("bad_value", [None, RefValue(None), True, False, "observed"])
def test_run_suews_rust_rejects_non_integer_soil_moisture_values(
    bad_value, sample_config_loaded, sample_forcing_parsed
):
    """Reject corrupted missing, boolean, or non-integer soil_moisture values on real configs (#2192)."""
    base = sample_config_loaded.model_copy(deep=True)
    corrupted_physics = ModelPhysics.model_construct(**{
        **dict(base.model.physics),
        "soil_moisture": bad_value,
    })
    corrupted_model = base.model.model_construct(**{
        **dict(base.model),
        "physics": corrupted_physics,
    })
    cfg = SUEWSConfig.model_construct(**{**dict(base), "model": corrupted_model})
    _, df_forcing = sample_forcing_parsed(2)

    with pytest.raises(ValueError, match="soil_moisture"):
        _run_rust.run_suews_rust(cfg, df_forcing, grid_id=1)


def _sample_config_omitting_soil_moisture(tmp_path: Path) -> SUEWSConfig:
    """Load sample YAML with ``model.physics.soil_moisture`` omitted so Pydantic supplies the bare enum default."""
    raw = yaml.safe_load(
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )
    raw["model"]["physics"].pop("soil_moisture", None)
    raw["model"]["physics"].pop("smdmethod", None)
    cfg_path = tmp_path / "omitted_soil_moisture.yml"
    cfg_path.write_text(yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    cfg = SUEWSConfig.from_yaml(str(cfg_path))
    assert type(cfg.model.physics.soil_moisture) is SMDMethod
    return cfg


def test_run_rust_entrypoints_accept_omitted_yaml_soil_moisture(
    tmp_path: Path, sample_forcing_parsed
):
    """All Rust execution entry points handle bare enum soil_moisture defaults (#2192)."""
    cfg = _sample_config_omitting_soil_moisture(tmp_path)
    _, df_forcing = sample_forcing_parsed(4)

    # 1. Public SUEWSSimulation path (exercises run_suews_rust_chunked -> run_suews_rust_multi)
    sim = SUEWSSimulation()
    sim.update_config(cfg, auto_load_forcing=False)
    sim.update_forcing(df_forcing)
    out_sim = sim.run(
        start_date=df_forcing.index[0], end_date=df_forcing.index[-1], n_jobs=1
    ).df
    assert len(out_sim) == len(df_forcing)

    # 2. Direct single-grid, multi-grid, state-injected, and chunked _run_rust entry points
    out_single, state_single, _ = _run_rust.run_suews_rust(
        cfg, df_forcing.iloc[:2], grid_id=1
    )
    assert len(out_single) == 2
    assert isinstance(state_single, str) and state_single

    out_with_state, state_next, _ = _run_rust.run_suews_rust_with_state(
        cfg, df_forcing.iloc[2:4], grid_id=1, state_json=state_single
    )
    assert len(out_with_state) == 2
    assert isinstance(state_next, str) and state_next

    out_multi, states_multi, _ = _run_rust.run_suews_rust_multi(
        cfg, df_forcing.iloc[:2], serial_mode=True
    )
    assert len(out_multi) == 2
    assert states_multi is not None and 1 in states_multi

    out_multi_state, _, _ = _run_rust.run_suews_rust_multi_with_state(
        cfg, df_forcing.iloc[2:4], states_multi, serial_mode=True
    )
    assert len(out_multi_state) == 2

    # 3. Multi-chunk path (chunk_day=1 across a 2-day slice triggers run_suews_rust_chunked loop)
    _, df_two_days = sample_forcing_parsed(289)
    out_chunked, _, _ = _run_rust.run_suews_rust_chunked(
        cfg, df_two_days, chunk_day=1, serial_mode=True
    )
    assert len(out_chunked) == len(df_two_days)
