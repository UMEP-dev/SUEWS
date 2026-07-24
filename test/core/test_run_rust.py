from importlib import import_module

import numpy as np
import pandas as pd
import pytest

pytestmark = pytest.mark.api

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
            "xsmd": [5.0, 5.0],
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
