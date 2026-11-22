"""The kernel refuses a momentum roughness method it has no scheme for (gh#2105).

The data model rejects ``roughness_length_momentum = 5`` (ALTERNATIVE) before
a run starts, but the Rust bridge can also be driven directly with a raw
configuration mapping that never passed through Pydantic validation. Inside
``SUEWS_cal_RoughnessParameters``, any unsupported ``RoughLenMomMethod`` (such
as 5 or 99) must fail fatally through the error state (code 113) on both
roughness-element grids (``Zh > 0``) and 100%-paved grids (``Zh == 0``),
instead of leaving ``z0m_zh``/``zdm_zh`` unassigned or silently falling back.
"""

from __future__ import annotations

import copy
from importlib import import_module
import json

import pytest

import supy as sp

_run_rust = import_module("supy._run_rust")

pytestmark = [
    pytest.mark.api,
    pytest.mark.rust,
    pytest.mark.core,
    pytest.mark.contract("C-VALID", "C-ERROR", "C-STRICT"),
]

# Error code registered in suews_ctrl_error.f95 for an unsupported
# RoughLenMomMethod with no scheme behind it.
UNSUPPORTED_MOMENTUM_ROUGHNESS_CODE = 113
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


def _make_config(inputs: dict, method: int, *, zero_zh: bool) -> dict:
    """Build a raw bridge config dict with ``roughness_length_momentum`` injected."""
    config = copy.deepcopy(inputs["config_dict"])
    config["model"]["physics"]["roughness_length_momentum"] = {"value": method}
    if zero_zh:
        land_cover = config["sites"][0]["properties"]["land_cover"]
        for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
            land_cover[surface]["sfr"] = {"value": 1.0 if surface == "paved" else 0.0}
    return config


def _run_with_roughness_method(inputs: dict, method: int, *, zero_zh: bool):
    """Run one grid through the bridge with ``roughness_length_momentum`` past Pydantic."""
    config = _make_config(inputs, method, zero_zh=zero_zh)
    return inputs["rust"].run_suews_multi(
        [json.dumps(config)], inputs["forcing_flat"], inputs["len_sim"], 1
    )


@pytest.mark.parametrize(
    "zero_zh",
    [False, True],
    ids=["zh-positive", "zh-zero-100pct-paved"],
)
@pytest.mark.parametrize("method", [5, 99], ids=["alternative-5", "unknown-99"])
def test_unsupported_momentum_roughness_is_refused_at_the_kernel(
    bridge_inputs, zero_zh, method
):
    with pytest.raises(RuntimeError) as exc_info:
        _run_with_roughness_method(bridge_inputs, method, zero_zh=zero_zh)
    message = str(exc_info.value)
    assert f"(code {UNSUPPORTED_MOMENTUM_ROUGHNESS_CODE})" in message, message
    assert "SUEWS_cal_RoughnessParameters" in message, message
    assert f"RoughLenMomMethod {method}" in message, message


@pytest.mark.parametrize(
    "zero_zh",
    [False, True],
    ids=["zh-positive", "zh-zero-100pct-paved"],
)
@pytest.mark.parametrize(
    "method", [1, 2, 3, 4], ids=["fixed", "variable", "macdonald", "lambdap"]
)
def test_supported_momentum_roughness_methods_still_run(bridge_inputs, zero_zh, method):
    """All four implemented momentum roughness methods remain unaffected."""
    results = _run_with_roughness_method(bridge_inputs, method, zero_zh=zero_zh)
    assert len(results) == 1
    assert results[0][3] == bridge_inputs["len_sim"]
