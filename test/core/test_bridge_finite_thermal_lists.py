"""Reject missing numerical material entries after data-model serialisation."""

from importlib import import_module

import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model.core.surface import ThermalLayers

_run_rust = import_module("supy._run_rust")
pytestmark = [pytest.mark.api, pytest.mark.core, pytest.mark.rust]


@pytest.mark.parametrize("wrapped", [False, True])
@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -float("inf")])
def test_non_finite_material_crosses_model_boundary_then_is_rejected(wrapped, bad):
    sim = SUEWSSimulation.from_sample_data()
    dict_config = sim.config.model_dump(exclude_none=True, mode="json")
    values = [0.03, bad, 0.1, 0.1, 0.1]
    layers = ThermalLayers(dz={"value": values} if wrapped else values)
    dict_config["sites"][0]["properties"]["land_cover"]["paved"]["thermal_layers"] = (
        layers.model_dump(exclude_none=True, mode="json")
    )
    forcing = _run_rust._prepare_forcing_block(sim.forcing.df.iloc[:1])
    with pytest.raises(RuntimeError) as exc:
        _run_rust._check_rust_available().run_suews(
            yaml.safe_dump(dict_config), forcing.ravel(order="C").tolist(), 1
        )
    message = str(exc.value)
    assert "properties.land_cover.paved.thermal_layers.dz" in message
    assert "[1]" in message
