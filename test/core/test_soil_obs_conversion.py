"""Independent soil-content algebra and mandatory kernel-boundary checks."""
# Tests deliberately exercise the private forcing/kernel preparation boundary.
# ruff: disable[import-private-name]
import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy._run_rust import _prepare_forcing_block
from supy.data_model.core.config import SUEWSConfig
from supy.util import soil_moisture_to_content
from supy.util._forcing import (
    convert_observed_soil_moisture,
    interpolate_soil_content,
    validate_soil_content,
)

# ruff: enable[import-private-name]

pytestmark = [pytest.mark.core, pytest.mark.physics]


def state(method=1):
    # Non-water weights 0.3/0.2: AWC=(30+10)/0.5=80;
    # FC=(0.3*0.6*200+0.2*0.6*200)/0.5=120 mm.
    row = {("smdmethod", "0"): method, ("obs_sm_layer_depth", "0"): 0.2,
           ("obs_sm_layer_top", "0"): 0.0}
    for i, f in enumerate([0.3, 0.2, 0, 0, 0, 0, 0.5]):
        row["sfr_surf", f"({i},)"] = f
        if i < 2:
            row["soildepth", f"({i},)"] = 200
            row["soilstorecap_surf", f"({i},)"] = [100, 50][i]
            row["soil_field_capacity", f"({i},)"] = 0.6
    result = pd.DataFrame([row], index=[1], dtype=float)
    result.columns = pd.MultiIndex.from_tuples(result.columns)
    return result


def forcing(values):
    return pd.DataFrame({"sm": values}, index=pd.date_range("2024-07-01", periods=len(values), freq="h"))


def test_independent_field_capacity_algebra_and_immutability():
    df = forcing([0, 50, 80, 100, 120, 140])
    original = df.copy()
    with pytest.warns(UserWarning) as caught:
        result = convert_observed_soil_moisture(df, state())
    np.testing.assert_array_equal(result._soil_deficit, [80, 70, 40, 20, 0, 0])
    pd.testing.assert_frame_equal(df, original)
    assert len(caught) == 2
    assert all("Count=" in str(w.message) and "maximum excess=" in str(w.message) for w in caught)
    with pytest.warns(UserWarning):
        pd.testing.assert_frame_equal(result, convert_observed_soil_moisture(df, state()))


def test_interpolation_before_clamp():
    df = forcing([140, 60])  # FC=120: deficits 0/60; midpoint must be 20, not 30.
    target = pd.date_range(df.index[0], df.index[-1], freq="30min")
    content = interpolate_soil_content(df.sm, target).to_frame()
    with pytest.warns(UserWarning):
        result = convert_observed_soil_moisture(content, state())
    np.testing.assert_array_equal(result.sm, [140, 100, 60])
    np.testing.assert_array_equal(result._soil_deficit, [0, 20, 60])


@pytest.mark.parametrize("missing", [np.nan, -999])
def test_missing_barriers_and_no_extrapolation(missing):
    df = forcing([60, missing, 100])
    target = pd.date_range(df.index[0] - pd.Timedelta("30min"), df.index[-1] + pd.Timedelta("30min"), freq="30min")
    result = interpolate_soil_content(df.sm, target)
    assert result.iloc[[0, 2, 3, 4, 6]].isna().all()
    with pytest.raises(ValueError, match="contains missing values"):
        convert_observed_soil_moisture(df, state())


@pytest.mark.parametrize("bad", [-1, np.inf, -np.inf])
@pytest.mark.parametrize("method", [0, 1])
def test_malformed_content_rejected_in_both_modes(bad, method):
    with pytest.raises(ValueError, match="finite and non-negative"):
        convert_observed_soil_moisture(forcing([50, bad]), state(method))


def test_modelled_mode_no_conversion_or_metadata_requirement():
    df = forcing([np.nan, -999, 80])
    result = convert_observed_soil_moisture(df, pd.DataFrame({("smdmethod", "0"): [0]}))
    pd.testing.assert_frame_equal(df, result)
    assert "_soil_deficit" not in result


@pytest.mark.parametrize("column,value,message", [
    (("obs_sm_layer_depth", "0"), 0.1, "full configured"),
    (("obs_sm_layer_top", "0"), 0.01, "full configured"),
    (("soil_field_capacity", "(0,)"), 0.1, "field-capacity content"),
    (("soil_field_capacity", "(0,)"), np.nan, "soil_field_capacity"),
    (("soildepth", "(0,)"), np.inf, "finite positive"),
    (("soilstorecap_surf", "(0,)"), -1, "finite positive"),
    (("sfr_surf", "(0,)"), -1, "valid surface fractions"),
])
def test_active_surface_validation(column, value, message):
    initial = state()
    initial.loc[1, column] = value
    with pytest.raises(ValueError, match=message):
        convert_observed_soil_moisture(forcing([70, 80]), initial)


def test_all_water_and_shared_grid_rules():
    initial = state()
    initial.loc[1, ("sfr_surf", "(0,)")] = 0
    initial.loc[1, ("sfr_surf", "(1,)")] = 0
    with pytest.raises(ValueError, match="positive non-water area"):
        convert_observed_soil_moisture(forcing([70, 80]), initial)
    initial = pd.concat([state(), state().rename(index={1: 2})])
    convert_observed_soil_moisture(forcing([70, 80]), initial)
    initial.loc[2, ("soil_field_capacity", "(0,)")] = 0.7
    with pytest.raises(ValueError, match="Shared forcing"):
        convert_observed_soil_moisture(forcing([70, 80]), initial)


@pytest.mark.parametrize("source_unit,values,kwargs,expected", [
    ("m3 m-3", [0.25], {}, [50]),
    ("m3 m-3", [0.25], {"soil_not_rocks": 0.8, "volume_basis": "fine_earth"}, [40]),
    ("kg kg-1", [0.2], {"bulk_density": 1250}, [50]),
    ("kg kg-1", [1.2], {"bulk_density": 1250}, [300]),
    ("mm", [50], {}, [50]),
])
def test_conversion_routes_independent_algebra(source_unit, values, kwargs, expected):
    result = soil_moisture_to_content(values, source_unit=source_unit, layer_depth=0.2, **kwargs)
    np.testing.assert_allclose(result, expected, rtol=1e-14)
    assert result.name == "sm"
    assert result.attrs["layer_depth"] == pytest.approx(0.2)
    assert result.attrs["unit"] == "kg m-2"


@pytest.mark.parametrize("kwargs,message", [
    ({"source_unit": "kg kg-1"}, "bulk_density"),
    ({"soil_not_rocks": 0.8}, "fine_earth"),
    ({"soil_not_rocks": 0}, "soil_not_rocks"),
    ({"layer_depth": 0}, "layer_depth"),
    ({"layer_top": -1}, "layer_top"),
])
def test_conversion_metadata_errors(kwargs, message):
    options = {"source_unit": "m3 m-3", "layer_depth": 0.2}
    options.update(kwargs)
    with pytest.raises(ValueError, match=message):
        soil_moisture_to_content([0.2], **options)


def test_conversion_missing_values_and_range():
    result = soil_moisture_to_content([np.nan, -999, 0.2], source_unit="m3 m-3", layer_depth=0.2)
    assert result.iloc[:2].isna().all()
    assert result.iloc[2] == 40
    with pytest.raises(ValueError, match="Volumetric"):
        soil_moisture_to_content([1.2], source_unit="m3 m-3", layer_depth=0.2)


def test_retired_names_and_selector():
    with pytest.raises(ValueError, match="Legacy"):
        validate_soil_content(pd.DataFrame({"xsmd": [0.2]}))
    with pytest.raises(ValueError, match="Both legacy"):
        validate_soil_content(pd.DataFrame({"xsmd": [0.2], "sm": [50]}))
    with pytest.raises(ValueError, match="Legacy"):
        convert_observed_soil_moisture(forcing([50]), state(2))


def test_kernel_handoff_is_deficit_and_raw_entry_is_refused():
    df = forcing([50, 80])
    with pytest.raises(ValueError, match="prepared deficits"):
        _prepare_forcing_block(df, soil_moisture_method=1)
    prepared = convert_observed_soil_moisture(df, state())
    block = _prepare_forcing_block(prepared, soil_moisture_method=1)
    np.testing.assert_array_equal(block[:, 19], [70, 40])
    np.testing.assert_array_equal(df.sm, [50, 80])


@pytest.mark.parametrize("conductance", [1, 2])
def test_observed_simulation_unchecked_gate_and_kernel_run(short_sample_sim, conductance):
    """Observed daytime execution works; bypass mode still rejects missing rows."""
    sim = short_sample_sim()
    data = sim.config.model_dump(mode="json")
    data["model"]["physics"]["soil_moisture"] = "observed"
    data["model"]["physics"]["surface_conductance"] = conductance
    for site in data["sites"]:
        props = site["properties"]
        props["soil_observation"] = {"layer_depth": 0.2}
        for name in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil"):
            props["land_cover"][name].update(
                soil_depth=200.0, soil_store_capacity=80.0, soil_field_capacity=0.6,
            )
    config = SUEWSConfig.model_validate(data)
    frame = sim.forcing.df.loc["2012-01-02 10:05":"2012-01-02 11:00"].copy()
    frame["sm"] = 50.0  # FC=120, hence D=70 mm.
    sim.update_config(config, auto_load_forcing=False)
    sim._df_forcing = frame.copy()
    sim._df_forcing.loc[frame.index[3], "sm"] = -999
    with pytest.raises(ValueError, match="missing values"):
        sim.run(start_date=frame.index[0], end_date=frame.index[-1], _validate_forcing=False)
    sim._df_forcing = frame.copy()
    first = sim.run(start_date=frame.index[0], end_date=frame.index[-1], _validate_forcing=False, n_jobs=1).df
    second_sim = SUEWSSimulation.from_state(sim.state_init)
    converted = frame.copy()
    converted["sm"] = soil_moisture_to_content(
        pd.Series(0.25, index=frame.index), source_unit="m3 m-3", layer_depth=0.2,
    )
    second_sim.update_forcing(converted)
    second = second_sim.run(start_date=frame.index[0], end_date=frame.index[-1], n_jobs=1).df
    np.testing.assert_allclose(first["SUEWS", "SMD"], 70.0, rtol=0, atol=1e-12)
    assert (first["SUEWS", "QE"] > 0).any()
    pd.testing.assert_frame_equal(first, second)
    pd.testing.assert_frame_equal(frame, sim.forcing.df)
