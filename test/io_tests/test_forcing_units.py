"""Declared forcing units cross one file boundary before runtime scaling."""

from pathlib import Path
from unittest.mock import Mock

import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSForcing, SUEWSSimulation, load_forcing_grid
from supy import _load
from supy._env import trv_supy_module
from supy.units import convert
from supy.util import read_forcing

pytestmark = pytest.mark.api


@pytest.fixture
def sample_path():
    return Path(str(trv_supy_module / "sample_data" / "Kc_2012_data_60.txt"))


@pytest.fixture
def canonical_path(sample_path, tmp_path):
    path = tmp_path / "canonical" / "forcing.txt"
    path.parent.mkdir()
    raw = pd.read_csv(sample_path, sep=r"\s+").iloc[:48].copy()
    raw.to_csv(path, sep="\t", index=False, float_format="%.17g")
    return path


@pytest.fixture
def alternate_file(canonical_path, tmp_path):
    raw = pd.read_csv(canonical_path, sep=r"\s+")
    for column, source, target in (
        ("Tair", "degC", "K"), ("pres", "kPa", "hPa")
    ):
        raw[column] = convert(raw[column], source, target)
    path = tmp_path / "alternate.txt"
    raw.to_csv(path, sep="\t", index=False, float_format="%.17g")
    return path


def test_no_declaration_full_sample_is_exact(sample_path, monkeypatch):
    """Compare all 2012 rows against the unchanged pre-boundary reader path."""
    actual = read_forcing(str(sample_path))
    monkeypatch.setattr(_load, "_convert_forcing_units", lambda frame, units=None: frame)
    expected = read_forcing(str(sample_path))
    pd.testing.assert_frame_equal(actual, expected, check_exact=True)
    assert len(actual) == 105408
    for column in actual:
        assert actual[column].to_numpy().tobytes() == expected[column].to_numpy().tobytes()


def test_boundary_without_mapping_returns_same_object():
    frame = pd.DataFrame({"Tair": [0.0]})
    assert _load._convert_forcing_units(frame) is frame


@pytest.mark.parametrize("entry", ["read", "oop", "list", "yaml_reader", "simulation", "simulation_list", "directory"])
def test_alternate_units_round_trip(canonical_path, alternate_file, entry):
    units = {"Tair": "K", "pres": "hPa", "rain": "mm h-1"}
    if entry == "read":
        actual = read_forcing(str(alternate_file), units=units)
    elif entry in {"oop", "list"}:
        paths = [alternate_file] if entry == "list" else alternate_file
        actual = SUEWSForcing.from_file(paths, units=units).df
    elif entry == "yaml_reader":
        actual = _load.load_SUEWS_Forcing_met_df_yaml([alternate_file], units=units)
    else:
        paths = [alternate_file] if entry == "simulation_list" else alternate_file
        if entry == "directory":
            paths = alternate_file.parent
        actual = SUEWSSimulation().update_forcing(paths, units=units).forcing.df
    expected = read_forcing(str(canonical_path), tstep_mod=None if entry == "yaml_reader" else 300)
    expected = expected.loc[actual.index]
    # Text round trip and affine offsets introduce only floating-point rounding.
    pd.testing.assert_frame_equal(actual, expected, check_exact=False, rtol=1e-12, atol=1e-12)
    np.testing.assert_allclose(actual.pres, expected.pres, rtol=1e-14)


@pytest.mark.parametrize("entry", ["constructor", "update_config", "grid", "dict", "object", "partial"])
def test_yaml_python_declaration(canonical_path, alternate_file, tmp_path, entry):
    text = (trv_supy_module / "sample_data" / "sample_config.yml").read_text(encoding="utf-8")
    config = yaml.safe_load(text)
    config["model"]["control"]["forcing"]["file"] = {"value": alternate_file.name}
    path = tmp_path / "config.yml"
    path.write_text(yaml.safe_dump(config), encoding="utf-8")
    units = {"Tair": "K", "pres": "hPa", "rain": "mm h-1"}
    if entry == "constructor":
        actual = SUEWSSimulation(path, units=units).forcing.df
    elif entry == "update_config":
        actual = SUEWSSimulation().update_config(path, units=units).forcing.df
    elif entry == "grid":
        actual = load_forcing_grid(str(path), 1, units=units)
    else:
        from supy.data_model import SUEWSConfig

        config["model"]["control"]["forcing"]["file"] = {"value": str(alternate_file)}
        if entry == "dict":
            actual = SUEWSSimulation(config, units=units).forcing.df
        elif entry == "object":
            actual = SUEWSSimulation(SUEWSConfig.from_dict(config), units=units).forcing.df
        else:
            sim = SUEWSSimulation().update_config(path, auto_load_forcing=False)
            actual = sim.update_config({}, units=units).forcing.df
    expected = read_forcing(str(canonical_path)).loc[actual.index]
    pd.testing.assert_frame_equal(actual, expected, check_exact=False, rtol=1e-12, atol=1e-12)
    with pytest.raises(ValueError, match="Tair.*dimensions"):
        SUEWSSimulation(path, units={"Tair": "m"})


@pytest.mark.parametrize("column,unit,error", [
    ("Tair", "m", ValueError), ("unknown", "K", ValueError),
    ("pres", 12, TypeError), ("rain", None, TypeError),
    ("Tair", "invalid_unit", ValueError), ("Tair", "", ValueError),
])
def test_invalid_declaration_names_column(sample_path, column, unit, error):
    with pytest.raises(error, match=column):
        read_forcing(str(sample_path), units={column: unit})


def test_no_registry_unit(monkeypatch):
    variable = _load.FORCING_REGISTRY.by_file_name("Tair").model_copy(update={"unit": None})
    monkeypatch.setattr(_load, "FORCING_REGISTRY", Mock(by_file_name=Mock(return_value=variable)))
    with pytest.raises(ValueError, match="Tair.*no unconditional registry unit"):
        _load._convert_forcing_units(pd.DataFrame({"Tair": [273.15]}), {"Tair": "K"})


def test_alias_sentinels_affine_and_scale():
    frame = pd.DataFrame({"temp_c": [273.15, 293.15, -999, np.nan], "rain": [1.0, 0, -999, np.nan]})
    frame.index = pd.date_range("2020-01-01", periods=4, freq="h")
    actual = _load._convert_forcing_units(frame, {"tair": "K", "rain": "mm h-1"})
    np.testing.assert_allclose(actual.temp_c, [0, 20, -999, np.nan], atol=1e-12)
    np.testing.assert_allclose(actual.rain, [1, 0, -999, np.nan])
    np.testing.assert_allclose(frame.temp_c, [273.15, 293.15, -999, np.nan])


def test_in_memory_declaration_refused(sample_path):
    frame = read_forcing(str(sample_path))
    with pytest.raises(ValueError, match="only to forcing files"):
        SUEWSSimulation().update_forcing(frame, units={"pres": "kPa"})


@pytest.mark.parametrize("units,error,match", [
    ({"Tair": "K", "temp_c": "K"}, ValueError, "temp_c.*duplicate"),
    ({"rain": "mm h-1"}, ValueError, "rain.*absent"),
    ({1: "K"}, ValueError, "1.*unknown"),
    (["K"], TypeError, "mapping"),
])
def test_invalid_mapping(units, error, match):
    with pytest.raises(error, match=match):
        _load._convert_forcing_units(pd.DataFrame({"Tair": [273.15]}), units)


def test_all_missing_values_still_check_dimension():
    with pytest.raises(ValueError, match="Tair.*dimensions"):
        _load._convert_forcing_units(pd.DataFrame({"Tair": [-999, np.nan]}), {"Tair": "m"})


@pytest.mark.parametrize("seconds", [300, 3600])
@pytest.mark.parametrize("unit,value", [("kg m-2 s-1", 1 / 3600), ("mm h-1", 1), ("mm d-1", 24)])
@pytest.mark.parametrize("column", ["rain", "Wuh", "wuh_paved", "wuh_bldgs", "wuh_evetr", "wuh_dectr", "wuh_grass", "wuh_bsoil", "wuh_water"])
def test_declared_water_rates(seconds, unit, value, column):
    frame = pd.DataFrame({column: [value, -999, np.nan]}, index=pd.date_range("2020-01-01", periods=3, freq=f"{seconds}s"))
    actual = _load._convert_forcing_units(frame, {column: unit})
    np.testing.assert_allclose(actual[column], [seconds / 3600, -999, np.nan])


@pytest.mark.parametrize("unit", ["mm", "kg m-2"])
@pytest.mark.parametrize("column", ["rain", "Wuh", "wuh_grass"])
def test_water_amount_declaration_requires_time(column, unit):
    with pytest.raises(ValueError, match=f"{column}.*time unit"):
        _load._convert_forcing_units(pd.DataFrame({column: [-999.0]}), {column: unit})
