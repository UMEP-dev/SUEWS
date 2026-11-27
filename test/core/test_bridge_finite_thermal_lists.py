"""Reject missing numerical material entries after data-model serialisation."""

from copy import deepcopy
from dataclasses import dataclass
from importlib import import_module

import numpy as np
from pydantic import ValidationError
import pytest
import yaml

from supy.data_model.core.surface import ThermalLayers

_run_rust = import_module("supy._run_rust")
pytestmark = [
    pytest.mark.native,
    pytest.mark.core,
    pytest.mark.rust,
    pytest.mark.contract("C-STRICT"),
]

# The longest window any test below runs. Shorter runs take its leading rows,
# which equal a separately prepared block because the block is built row by row.
_N_STEPS = 24


@dataclass(frozen=True)
class _SampleInputs:
    """The sample's JSON config dump and kernel forcing block, shared read-only."""

    _config: dict
    _forcing: np.ndarray

    def config(self) -> dict:
        """A private deep copy of the config dump, free to edit."""
        return deepcopy(self._config)

    def forcing(self, start: int, stop: int) -> list[float]:
        """Forcing rows ``start:stop`` as the flat list the bridge takes."""
        return self._forcing[start:stop].ravel(order="C").tolist()


@pytest.fixture(scope="module")
def sample_inputs(sample_config_loaded, sample_forcing_parsed):
    """Build the shipped sample's config dump and first forcing rows once.

    Every test here needs only these two artefacts. Each test used to build
    ``SUEWSSimulation.from_sample_data()`` for them, re-parsing a full year of
    forcing per parametrised case; both now come from the session's single
    sample parse. READ-ONLY: the forcing block is write-protected and
    ``config()`` hands out deep copies.
    """
    _state, df_forcing = sample_forcing_parsed(_N_STEPS)
    forcing = _run_rust._prepare_forcing_block(df_forcing)
    forcing.setflags(write=False)
    return _SampleInputs(
        sample_config_loaded.model_dump(exclude_none=True, mode="json"), forcing
    )


def _run_timesteps(config, forcing, n_steps):
    """Run the bridge on ``config`` and return its timestep output block."""
    output, _state, length, _warnings = _run_rust._check_rust_available().run_suews(
        yaml.safe_dump(config), forcing, n_steps
    )
    block = np.frombuffer(output, dtype=np.float64)
    return block.reshape((length, -1), order="C"), length


@pytest.fixture(scope="module")
def clean_sample_run(sample_inputs):
    """The unedited sample's 24-step timestep output, run once per module.

    The reference every unread-entry case below compares against; it used to
    be recomputed in each parametrised case. It has no consumer outside this
    module, so module scope already computes it once per session (per xdist
    worker that runs the module) without adding a ``test/conftest.py``
    fixture. READ-ONLY (write-protected).
    """
    expected, length = _run_timesteps(
        sample_inputs.config(), sample_inputs.forcing(0, _N_STEPS), _N_STEPS
    )
    expected.setflags(write=False)
    return expected, length


@pytest.mark.parametrize("wrapped", [False, True])
@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -float("inf")])
def test_non_finite_material_crosses_model_boundary_then_is_rejected(
    sample_inputs, wrapped, bad
):
    dict_config = sample_inputs.config()
    # EHC (storage heat method 5) reads every material layer, so dz[1] is
    # read; under the sample's own method it is not, and the bridge then
    # carries it as absent instead of rejecting it.
    dict_config["model"]["physics"]["storage_heat"] = {"value": 5}
    values = [0.03, bad, 0.1, 0.1, 0.1]
    # Data-model validation rejects non-finite material layers up front.
    with pytest.raises(ValidationError, match="thermal layer"):
        ThermalLayers(dz={"value": values} if wrapped else values)
    # Direct YAML input bypassing Pydantic is still rejected by the Rust bridge.
    dict_config["sites"][0]["properties"]["land_cover"]["paved"]["thermal_layers"][
        "dz"
    ] = {"value": values} if wrapped else values
    with pytest.raises(RuntimeError) as exc:
        _run_rust._check_rust_available().run_suews(
            yaml.safe_dump(dict_config), sample_inputs.forcing(0, 1), 1
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
def test_null_in_an_unread_entry_runs_and_leaves_output_unchanged(
    sample_inputs, clean_sample_run, case
):
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
    edited = sample_inputs.config()
    _null_unread_entry(edited["sites"][0], case)

    expected, expected_len = clean_sample_run
    actual, actual_len = _run_timesteps(
        edited, sample_inputs.forcing(0, _N_STEPS), _N_STEPS
    )
    assert actual_len == expected_len
    new_nan_columns = np.flatnonzero(
        np.isnan(actual).any(axis=0) & ~np.isnan(expected).any(axis=0)
    )
    assert new_nan_columns.size == 0, f"NaN only in edited run: {new_nan_columns}"
    np.testing.assert_array_equal(actual, expected)


def test_resume_under_ehc_rejects_an_unread_initial_temperature(sample_inputs):
    """A null initial temperature that the first run did not read is carried
    as NaN in its checkpoint. Continuing that checkpoint under storage heat
    method 5, whose EHC reads it, fails at resume naming the entry, instead
    of handing NaN to the kernel."""
    rust = _run_rust._check_rust_available()
    first = sample_inputs.config()
    _null_unread_entry(first["sites"][0], "paved_temperature")
    _output, state_json, _length, _warnings = rust.run_suews(
        yaml.safe_dump(first), sample_inputs.forcing(0, 1), 1
    )

    resumed = sample_inputs.config()
    resumed["model"]["physics"]["storage_heat"] = {"value": 5}
    with pytest.raises(RuntimeError) as exc:
        rust.run_suews_with_state(
            yaml.safe_dump(resumed),
            sample_inputs.forcing(1, 2),
            1,
            state_json,
        )
    message = str(exc.value)
    assert "heat_state.temp_surf[2]" in message
    assert "paved" in message


def test_resume_under_ehc_accepts_a_checkpoint_written_under_ehc(sample_inputs):
    """Positive control for the resume check above: a checkpoint written by a
    run under storage heat method 5, where EHC writes the temperatures it
    reads, continues under the same configuration."""
    rust = _run_rust._check_rust_available()
    config = sample_inputs.config()
    config["model"]["physics"]["storage_heat"] = {"value": 5}
    config_yaml = yaml.safe_dump(config)
    _output, state_json, _length, _warnings = rust.run_suews(
        config_yaml, sample_inputs.forcing(0, 2), 2
    )
    _output, _state, length, _warnings = rust.run_suews_with_state(
        config_yaml, sample_inputs.forcing(2, 4), 2, state_json
    )
    assert length == 2
