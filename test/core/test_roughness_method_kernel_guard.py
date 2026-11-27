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

# The tests use the named validation bypass on purpose.
# ruff: disable[import-private-name]
from supy.data_model.core.config import _assign_trusted

# ruff: enable[import-private-name]
from supy.data_model.core.model import MomentumRoughnessMethod

_run_rust = import_module("supy._run_rust")

pytestmark = [
    pytest.mark.native,
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


def _simulation_with_bypassed_method(method: int, *, n_grids: int):
    """Return a ``SUEWSSimulation`` whose momentum roughness method skipped validation.

    ``SUEWSConfig`` rejects method 5 at load and on assignment, so the method is
    written with the named ``_assign_trusted`` bypass after the configuration
    has been accepted. ``SUEWSSimulation.run()`` then hands it to the kernel
    through its own output-buffer bridge path, not ``run_suews_multi``.
    """
    sim = sp.SUEWSSimulation.from_sample_data()
    forcing = sim._df_forcing.iloc[:N_STEPS]
    config = sim.config.model_copy(deep=True)
    if n_grids > 1:
        sites = [config.sites[0]]
        for gridiv in range(2, n_grids + 1):
            site = config.sites[0].model_copy(deep=True)
            site.gridiv = gridiv
            site.name = f"{config.sites[0].name}_{gridiv}"
            sites.append(site)
        config.sites = sites
    sim.update_config(config, auto_load_forcing=False)
    sim.update_forcing(forcing)
    _set_method_unvalidated(sim, method)
    return sim, forcing


def _set_method_unvalidated(sim, method) -> None:
    """Write ``roughness_length_momentum`` past ``validate_assignment``."""
    physics = sim.config.model.physics
    current = physics.roughness_length_momentum
    if hasattr(current, "value"):
        _assign_trusted(current, "value", method)
    else:
        _assign_trusted(physics, "roughness_length_momentum", method)


@pytest.mark.filterwarnings("ignore:Pydantic serializer warnings:UserWarning")
@pytest.mark.parametrize(
    "n_jobs,n_grids",
    [(1, 1), (-1, 2)],
    ids=["serial-one-grid", "parallel-two-grids"],
)
def test_unsupported_momentum_roughness_is_refused_through_simulation_run(
    n_jobs, n_grids
):
    """``SUEWSSimulation.run()`` surfaces code 113 on its serial and parallel paths."""
    sim, forcing = _simulation_with_bypassed_method(5, n_grids=n_grids)
    run_options = {
        "start_date": forcing.index[0],
        "end_date": forcing.index[-1],
        "n_jobs": n_jobs,
    }

    with pytest.raises(RuntimeError) as exc_info:
        sim.run(**run_options)
    message = str(exc_info.value)
    assert f"(code {UNSUPPORTED_MOMENTUM_ROUGHNESS_CODE})" in message, message
    assert "SUEWS_cal_RoughnessParameters" in message, message
    assert "RoughLenMomMethod 5" in message, message

    # The fatal state does not leak into the next run on the same object.
    _set_method_unvalidated(sim, MomentumRoughnessMethod.FIXED)
    output = sim.run(**run_options)
    assert len(output.df.loc[1]) == len(forcing)
