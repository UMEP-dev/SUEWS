"""Reject missing numerical material entries after data-model serialisation."""

from importlib import import_module

import numpy as np
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model.core.surface import ThermalLayers

_run_rust = import_module("supy._run_rust")
pytestmark = [
    pytest.mark.api,
    pytest.mark.core,
    pytest.mark.rust,
    pytest.mark.contract("C-STRICT"),
]


@pytest.mark.parametrize("wrapped", [False, True])
@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -float("inf")])
def test_non_finite_material_crosses_model_boundary_then_is_rejected(wrapped, bad):
    sim = SUEWSSimulation.from_sample_data()
    dict_config = sim.config.model_dump(exclude_none=True, mode="json")
    # EHC (storage heat method 5) reads every material layer, so dz[1] is
    # read; under the sample's own method it is not, and the bridge then
    # carries it as absent instead of rejecting it.
    dict_config["model"]["physics"]["storage_heat"] = {"value": 5}
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


def _list_of(node):
    """The list inside a bare or RefValue-wrapped list node."""
    return node["value"] if isinstance(node, dict) else node


def _null_unread_entry(site, case):
    if case == "paved_dz":
        _list_of(site["properties"]["land_cover"]["paved"]["thermal_layers"]["dz"])[
            1
        ] = None
    elif case == "paved_k":
        _list_of(site["properties"]["land_cover"]["paved"]["thermal_layers"]["k"])[
            1
        ] = None
    elif case == "paved_temperature":
        _list_of(site["initial_states"]["paved"]["temperature"])[2] = None
    elif case == "paved_temperature_inf":
        _list_of(site["initial_states"]["paved"]["temperature"])[2] = float("inf")
    elif case == "roof_temperature":
        _list_of(site["initial_states"]["roofs"][0]["temperature"])[1] = None
    elif case == "no_building_frac":
        del site["properties"]["vertical_layers"]["building_frac"]
    else:
        raise ValueError(case)


@pytest.mark.parametrize(
    "case",
    [
        "paved_dz",
        "paved_k",
        "paved_temperature",
        "paved_temperature_inf",
        "roof_temperature",
        "no_building_frac",
    ],
)
def test_null_in_an_unread_entry_runs_and_leaves_output_unchanged(case):
    """Under the sample's own options (no SPARTACUS, storage heat not EHC) the
    kernel does not read these entries: the run completes and its timestep
    output equals the clean run's, compared numerically with NaN-aware
    equality (not bitwise).

    Only the timestep output is compared. The returned state is not: an
    unread null or non-finite number is carried as NaN in it (the heat state
    is packed unconditionally), which the checkpoint codec accepts, so the
    run returns its state. A run that loads such an entry under an option
    reading it rejects it at load; continuing the returned checkpoint under
    such an option is rejected at resume (see the test below)."""
    sim = SUEWSSimulation.from_sample_data()
    rust = _run_rust._check_rust_available()
    forcing = _run_rust._prepare_forcing_block(sim.forcing.df.iloc[:24])
    clean = sim.config.model_dump(exclude_none=True, mode="json")
    edited = sim.config.model_dump(exclude_none=True, mode="json")
    _null_unread_entry(edited["sites"][0], case)

    def run(config):
        output, _state, length, _warnings = rust.run_suews(
            yaml.safe_dump(config), forcing.ravel(order="C").tolist(), 24
        )
        block = np.frombuffer(output, dtype=np.float64)
        return block.reshape((length, -1), order="C"), length

    expected, expected_len = run(clean)
    actual, actual_len = run(edited)
    assert actual_len == expected_len
    new_nan_columns = np.flatnonzero(
        np.isnan(actual).any(axis=0) & ~np.isnan(expected).any(axis=0)
    )
    assert new_nan_columns.size == 0, f"NaN only in edited run: {new_nan_columns}"
    np.testing.assert_array_equal(actual, expected)


def test_resume_under_ehc_rejects_an_unread_initial_temperature():
    """A null initial temperature that the first run did not read is carried
    as NaN in its checkpoint. Continuing that checkpoint under storage heat
    method 5, whose EHC reads it, fails at resume naming the entry, instead
    of handing NaN to the kernel."""
    sim = SUEWSSimulation.from_sample_data()
    rust = _run_rust._check_rust_available()
    forcing = _run_rust._prepare_forcing_block(sim.forcing.df.iloc[:2])
    first = sim.config.model_dump(exclude_none=True, mode="json")
    _null_unread_entry(first["sites"][0], "paved_temperature")
    _output, state_json, _length, _warnings = rust.run_suews(
        yaml.safe_dump(first), forcing[:1].ravel(order="C").tolist(), 1
    )

    resumed = sim.config.model_dump(exclude_none=True, mode="json")
    resumed["model"]["physics"]["storage_heat"] = {"value": 5}
    with pytest.raises(RuntimeError) as exc:
        rust.run_suews_with_state(
            yaml.safe_dump(resumed),
            forcing[1:2].ravel(order="C").tolist(),
            1,
            state_json,
        )
    message = str(exc.value)
    assert "heat_state.temp_surf[2]" in message
    assert "paved" in message


def test_resume_under_ehc_accepts_a_checkpoint_written_under_ehc():
    """Positive control for the resume check above: a checkpoint written by a
    run under storage heat method 5, where EHC writes the temperatures it
    reads, continues under the same configuration."""
    sim = SUEWSSimulation.from_sample_data()
    rust = _run_rust._check_rust_available()
    forcing = _run_rust._prepare_forcing_block(sim.forcing.df.iloc[:4])
    config = sim.config.model_dump(exclude_none=True, mode="json")
    config["model"]["physics"]["storage_heat"] = {"value": 5}
    config_yaml = yaml.safe_dump(config)
    _output, state_json, _length, _warnings = rust.run_suews(
        config_yaml, forcing[:2].ravel(order="C").tolist(), 2
    )
    _output, _state, length, _warnings = rust.run_suews_with_state(
        config_yaml, forcing[2:4].ravel(order="C").tolist(), 2, state_json
    )
    assert length == 2
