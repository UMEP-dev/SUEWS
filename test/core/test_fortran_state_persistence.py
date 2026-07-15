"""Exercise the native simulation boundary in different in-process orders.

Fresh ``run_suews`` calls own a fresh model state.  Continuation is explicit and
uses ``run_suews_with_state`` with the state returned by an earlier call.  This
test protects that lifecycle contract without relying on pytest collection order.
"""

from copy import deepcopy
import json
from pathlib import Path

from conftest import SHORT_RUN_STEPS
import numpy as np
import pytest

from supy import SUEWSSimulation
from supy._run_rust import (  # noqa: PLC2701 - test the native boundary directly.
    _load_rust_module,
    _prepare_forcing_block,
)

pytestmark = pytest.mark.physics


@pytest.fixture(scope="module")
def native_run_cases():
    """Return two distinct inputs for repeated native-boundary calls."""
    path_config = (
        Path(__file__).resolve().parents[2]
        / "src"
        / "supy"
        / "sample_data"
        / "sample_config.yml"
    )
    simulation = SUEWSSimulation(str(path_config))
    dict_config_a = simulation.config.model_dump(exclude_none=True, mode="json")
    dict_config_b = deepcopy(dict_config_a)

    # Make B physically distinct from A while retaining the same dimensions and
    # forcing.  A large albedo contrast ensures the matrix is not a vacuous
    # comparison between two aliases of the same input.
    dict_config_b["sites"][0]["properties"]["land_cover"]["paved"]["alb"]["value"] = 0.8

    df_forcing = simulation.forcing.df.iloc[:SHORT_RUN_STEPS]
    forcing_flat = _prepare_forcing_block(df_forcing).ravel(order="C").tolist()
    return (
        _load_rust_module(),
        {
            "A": json.dumps(dict_config_a),
            "B": json.dumps(dict_config_b),
        },
        forcing_flat,
        len(df_forcing),
    )


def _run_native_case(native_run_cases, case_name):
    """Run one fresh native simulation and return output plus final state."""
    rust_module, dict_config_json, forcing_flat, len_sim = native_run_cases
    output_flat, state_json, actual_len = rust_module.run_suews(
        dict_config_json[case_name], forcing_flat, len_sim
    )
    assert actual_len == len_sim
    return np.asarray(output_flat), state_json


@pytest.mark.core
def test_native_state_isolated_across_order_matrix(native_run_cases):
    """Fresh calls produce their isolated baseline in A/B and repeat orders."""
    dict_baseline = {
        case_name: _run_native_case(native_run_cases, case_name)
        for case_name in ("A", "B")
    }
    output_a = dict_baseline["A"][0]
    output_b = dict_baseline["B"][0]
    finite_difference = (
        np.isfinite(output_a) & np.isfinite(output_b) & (output_a != output_b)
    )
    assert np.any(finite_difference), "A and B need a real finite output difference"

    for first_case, second_case in (("A", "B"), ("B", "A"), ("A", "A"), ("B", "B")):
        _run_native_case(native_run_cases, first_case)
        output_observed, state_observed = _run_native_case(
            native_run_cases, second_case
        )
        output_expected, state_expected = dict_baseline[second_case]

        np.testing.assert_array_equal(
            output_observed,
            output_expected,
            err_msg=f"native output leaked across {first_case}->{second_case}",
        )
        assert state_observed == state_expected, (
            f"native final state leaked across {first_case}->{second_case}"
        )
