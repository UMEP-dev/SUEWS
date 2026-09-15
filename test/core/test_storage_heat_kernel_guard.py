"""The kernel refuses a storage-heat method it has no scheme for (gh#1802).

The data model rejects ``storage_heat = 4`` (ESTM) before a run starts
(gh#1785), but the Rust bridge can also be driven directly with a
configuration mapping that never passed through the data model. With the
ESTM branch removed from ``SUEWS_cal_Qs``, such a value must be refused at
the kernel boundary through the error state, not left to fall through the
dispatch with ``QS = -999`` feeding the energy balance.
"""

from __future__ import annotations

import copy
from importlib import import_module
import json

import pytest

import supy as sp

_run_rust = import_module("supy._run_rust")

pytestmark = [pytest.mark.api, pytest.mark.rust, pytest.mark.core]

# Error code registered in suews_ctrl_error.f95 for a storage-heat method
# with no scheme behind it.
UNSUPPORTED_STORAGE_HEAT_CODE = 106
# One hour at the sample 5-minute timestep: the guard fires on the first step.
N_STEPS = 12


@pytest.fixture(scope="module")
def bridge_inputs():
    sim = sp.SUEWSSimulation.from_sample_data()
    df_forcing = sim._df_forcing.iloc[:N_STEPS]
    return {
        "rust": _run_rust._check_rust_available(),
        "config_dict": sim.config.model_dump(exclude_none=True, mode="json"),
        "forcing_flat": _run_rust
        ._prepare_forcing_block(df_forcing)
        .ravel(order="C")
        .tolist(),
        "len_sim": len(df_forcing),
    }


def _run_with_storage_heat(inputs: dict, method: int):
    """Run one grid through the bridge with ``storage_heat`` set past the data model."""
    config = copy.deepcopy(inputs["config_dict"])
    config["model"]["physics"]["storage_heat"] = {"value": method}
    return inputs["rust"].run_suews_multi(
        [json.dumps(config)], inputs["forcing_flat"], inputs["len_sim"], 1
    )


@pytest.mark.parametrize("method", [4, 14], ids=["estm-removed", "never-a-scheme"])
def test_unsupported_storage_heat_is_refused_at_the_kernel(bridge_inputs, method):
    with pytest.raises(RuntimeError) as exc_info:
        _run_with_storage_heat(bridge_inputs, method)
    message = str(exc_info.value)
    assert f"(code {UNSUPPORTED_STORAGE_HEAT_CODE})" in message, message
    assert f"StorageHeatMethod {method}" in message, message
    assert "not available" in message, message


def test_supported_storage_heat_still_runs(bridge_inputs):
    """The guard is an ELSE branch: a method with a scheme must be unaffected."""
    results = _run_with_storage_heat(bridge_inputs, 1)
    assert len(results) == 1
    assert results[0][3] == bridge_inputs["len_sim"]
