"""Exercise the native simulation boundary in different in-process orders.

Each reference case runs in its own fresh Python process.  Every transition
still makes both native calls in one process, so retained Fortran state cannot
hide behind pytest process isolation or collection order.
"""

from copy import deepcopy
import json
import os
from pathlib import Path
import subprocess
import sys

from conftest import SHORT_RUN_STEPS
import numpy as np
import pytest

from supy import SUEWSSimulation
from supy._run_rust import (  # noqa: PLC2701 - test the native boundary directly.
    _prepare_forcing_block,
)

pytestmark = pytest.mark.physics

_RESULT_PREFIX = "__SUEWS_NATIVE_RESULT__"
_NATIVE_SEQUENCE_RUNNER = f"""
import json
import os
import sys
import uuid

from supy._run_rust import _load_rust_module

payload = json.load(sys.stdin)
rust_module = _load_rust_module()
results = []
for case_name in payload["sequence"]:
    output_flat, state_json, actual_len = rust_module.run_suews(
        payload["config_json_by_case"][case_name],
        payload["forcing_flat"],
        payload["len_sim"],
    )
    results.append(
        {{
            "actual_len": actual_len,
            "output_flat": output_flat,
            "state_json": state_json,
        }}
    )

result = {{
    "pid": os.getpid(),
    "process_token": uuid.uuid4().hex,
    "results": results,
}}
sys.stdout.write("{_RESULT_PREFIX}" + json.dumps(result))
"""


@pytest.fixture(scope="module")
def native_run_payload():
    """Return two physically distinct inputs for native-boundary calls."""
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

    # A large albedo contrast ensures A and B cannot be aliases whose identical
    # outputs would make the order matrix pass vacuously.
    dict_config_b["sites"][0]["properties"]["land_cover"]["paved"]["alb"]["value"] = 0.8

    df_forcing = simulation.forcing.df.iloc[:SHORT_RUN_STEPS]
    forcing_flat = _prepare_forcing_block(df_forcing).ravel(order="C").tolist()
    return {
        "config_json_by_case": {
            "A": json.dumps(dict_config_a),
            "B": json.dumps(dict_config_b),
        },
        "forcing_flat": forcing_flat,
        "len_sim": len(df_forcing),
    }


def _run_native_sequence(native_run_payload, sequence):
    """Run a sequence of native calls together in one fresh Python process."""
    dict_payload = {**native_run_payload, "sequence": list(sequence)}
    result = subprocess.run(
        [sys.executable, "-c", _NATIVE_SEQUENCE_RUNNER],
        input=json.dumps(dict_payload),
        text=True,
        capture_output=True,
        timeout=120,
        check=False,
        cwd=Path(__file__).resolve().parents[2],
    )
    if result.returncode != 0:
        pytest.fail(
            "native sequence subprocess failed: "
            f"stdout={result.stdout!r}, stderr={result.stderr!r}"
        )

    result_line = next(
        (
            line
            for line in reversed(result.stdout.splitlines())
            if line.startswith(_RESULT_PREFIX)
        ),
        None,
    )
    assert result_line is not None, result.stdout
    dict_result = json.loads(result_line.removeprefix(_RESULT_PREFIX))
    assert len(dict_result["results"]) == len(sequence)
    for dict_case_result in dict_result["results"]:
        assert dict_case_result["actual_len"] == native_run_payload["len_sim"]
    return dict_result


@pytest.fixture(scope="module")
def clean_native_references(native_run_payload):
    """Run A and B separately so neither can contaminate the other reference."""
    dict_reference = {}
    for case_name in ("A", "B"):
        dict_run = _run_native_sequence(native_run_payload, (case_name,))
        dict_case_result = dict_run["results"][0]
        dict_reference[case_name] = {
            "output": np.asarray(dict_case_result["output_flat"]),
            "state": dict_case_result["state_json"],
            "pid": dict_run["pid"],
            "process_token": dict_run["process_token"],
        }
    return dict_reference


@pytest.mark.core
def test_native_references_use_independent_processes(clean_native_references):
    """Reject the old A-then-B reference scheme in the pytest process."""
    list_pid = [dict_case["pid"] for dict_case in clean_native_references.values()]
    list_process_token = [
        dict_case["process_token"] for dict_case in clean_native_references.values()
    ]

    assert all(pid != os.getpid() for pid in list_pid)
    assert len(set(list_pid)) == len(list_pid)
    assert len(set(list_process_token)) == len(list_process_token)


@pytest.mark.core
def test_native_reference_cases_are_physically_distinct(clean_native_references):
    """A and B need at least one real finite output difference."""
    output_a = clean_native_references["A"]["output"]
    output_b = clean_native_references["B"]["output"]
    finite_difference = (
        np.isfinite(output_a) & np.isfinite(output_b) & (output_a != output_b)
    )
    assert np.any(finite_difference)


@pytest.mark.core
@pytest.mark.parametrize(
    ("first_case", "second_case"),
    (("A", "B"), ("B", "A"), ("A", "A"), ("B", "B")),
    ids=("A-to-B", "B-to-A", "A-to-A", "B-to-B"),
)
def test_native_state_isolated_across_order(
    native_run_payload,
    clean_native_references,
    first_case,
    second_case,
):
    """Both calls match clean references when run consecutively in one process."""
    tuple_sequence = (first_case, second_case)
    dict_run = _run_native_sequence(native_run_payload, tuple_sequence)
    set_reference_token = {
        dict_case["process_token"] for dict_case in clean_native_references.values()
    }
    assert dict_run["process_token"] not in set_reference_token

    for case_name, dict_observed in zip(tuple_sequence, dict_run["results"]):
        dict_expected = clean_native_references[case_name]
        np.testing.assert_array_equal(
            np.asarray(dict_observed["output_flat"]),
            dict_expected["output"],
            err_msg=f"native output leaked across {first_case}->{second_case}",
        )
        assert dict_observed["state_json"] == dict_expected["state"], (
            f"native final state leaked across {first_case}->{second_case}"
        )
