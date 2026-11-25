"""Ambiguous public checkpoint keys must fail with actionable diagnostics."""

import copy
import json
import re

import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy._run_rust import (  # ruff: ignore[import-private-name]
    iter_suews_rust_streamed,
    run_suews_rust_chunked,
    run_suews_rust_multi_with_state,
)
from supy.suews_checkpoint import SUEWSCheckpoint

pytestmark = [pytest.mark.api, pytest.mark.contract("C-RESTART", "C-ERROR")]


class _GridIdWrapper:
    """Hashable value wrapper exercising existing ``_normalise_grid_id`` conversion."""

    def __init__(self, value: int, *, mimic_int_repr: bool = False) -> None:
        self.value = value
        self._mimic_int_repr = mimic_int_repr

    def __repr__(self) -> str:
        if self._mimic_int_repr:
            return repr(self.value)
        return f"_GridIdWrapper({self.value!r})"


def _expected_duplicate_message(
    canonical: int, first_repr: str, second_repr: str
) -> str:
    return (
        f"Duplicate checkpoint grid ID after normalisation: {canonical} "
        f"(keys {first_repr} and {second_repr})"
    )


def _run_window(sim: SUEWSSimulation, df_forcing: pd.DataFrame) -> SUEWSSimulation:
    sim.update_forcing(df_forcing)
    sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1], n_jobs=1)
    return sim


@pytest.fixture(scope="module")
def two_real_states(short_sample_sim):
    """Produce two genuinely distinct SUEWS checkpoint states and continuation inputs."""
    sim_a = short_sample_sim()
    forcing = sim_a.forcing.df.iloc[:18].copy()
    _run_window(sim_a, forcing.iloc[:6])

    sim_b = short_sample_sim()
    _run_window(sim_b, forcing.iloc[:12])

    state_a = sim_a.checkpoint.grid_states[1]
    state_b = sim_b.checkpoint.grid_states[1]
    assert state_a != state_b

    return {
        "config": copy.deepcopy(sim_b.config),
        "forcing_continue": forcing.iloc[12:18].copy(),
        "checkpoint": sim_b.checkpoint,
        "state_a": state_a,
        "state_b": state_b,
        "obj_a": json.loads(state_a),
        "obj_b": json.loads(state_b),
    }


@pytest.mark.parametrize(
    ("keys", "canonical", "first_repr", "second_repr"),
    [
        ((7, "7"), 7, "7", "'7'"),
        (("7", 7), 7, "'7'", "7"),
        (("7", "007"), 7, "'7'", "'007'"),
        (("007", "7"), 7, "'007'", "'7'"),
        ((37, "37"), 37, "37", "'37'"),
        ((37, "037"), 37, "37", "'037'"),
        ((0, "-0"), 0, "0", "'-0'"),
        (("-0", 0), 0, "'-0'", "0"),
        (("-0", "+0"), 0, "'-0'", "'+0'"),
        (("+7", "7"), 7, "'+7'", "'7'"),
        (("-7", "-007"), -7, "'-7'", "'-007'"),
        ((-7, "-7"), -7, "-7", "'-7'"),
        ((" 7 ", "7"), 7, "' 7 '", "'7'"),
        (("7", "\u0667"), 7, "'7'", r"'\u0667'"),
    ],
)
@pytest.mark.parametrize("factory", [SUEWSCheckpoint, SUEWSCheckpoint.from_grid_states])
@pytest.mark.parametrize("equal_states", [False, True])
def test_checkpoint_constructor_rejects_normalised_aliases(
    two_real_states, keys, canonical, first_repr, second_repr, factory, equal_states
):
    """Report canonical grid and both original keys on constructor entry points."""
    state_first = two_real_states["state_a"]
    state_second = state_first if equal_states else two_real_states["state_b"]
    expected = _expected_duplicate_message(canonical, first_repr, second_repr)
    with pytest.raises(ValueError, match=f"^{re.escape(expected)}$"):
        factory({keys[0]: state_first, keys[1]: state_second})


@pytest.mark.parametrize(
    ("key_pair", "canonical", "first_repr", "second_repr"),
    [
        (("7", "007"), 7, "'7'", "'007'"),
        (("007", "7"), 7, "'007'", "'7'"),
        (("37", "037"), 37, "'37'", "'037'"),
        (("0", "-0"), 0, "'0'", "'-0'"),
        (("+7", "7"), 7, "'+7'", "'7'"),
        (("-7", "-007"), -7, "'-7'", "'-007'"),
    ],
)
@pytest.mark.parametrize("equal_states", [False, True])
def test_checkpoint_file_rejects_normalised_aliases_with_both_keys(
    tmp_path, two_real_states, key_pair, canonical, first_repr, second_repr, equal_states
):
    """Real from_file rejects colliding keys in both orders and for equal states."""
    payload = two_real_states["checkpoint"].to_dict()
    obj_first = two_real_states["obj_a"]
    obj_second = obj_first if equal_states else two_real_states["obj_b"]
    payload["grid_states"] = {key_pair[0]: obj_first, key_pair[1]: obj_second}
    path = tmp_path / "checkpoint.json"
    path.write_text(json.dumps(payload), encoding="utf-8")

    expected = _expected_duplicate_message(canonical, first_repr, second_repr)
    with pytest.raises(ValueError, match=f"^{re.escape(expected)}$"):
        SUEWSCheckpoint.from_file(path)


@pytest.mark.parametrize("literal_key", ["37", "7", "007"])
def test_checkpoint_file_rejects_literal_duplicate_json_members(
    tmp_path, two_real_states, literal_key
):
    """Preserve literal duplicate JSON member rejection before dict decoding."""
    obj_a = json.dumps(two_real_states["obj_a"])
    obj_b = json.dumps(two_real_states["obj_b"])
    members = f'"{literal_key}": {obj_a}, "{literal_key}": {obj_b}'
    path = tmp_path / "checkpoint_literal_dup.json"
    path.write_text('{"grid_states": {' + members + "}}", encoding="utf-8")

    expected = f"Duplicate checkpoint JSON key: {literal_key!r}"
    with pytest.raises(ValueError, match=f"^{re.escape(expected)}$"):
        SUEWSCheckpoint.from_file(path)


@pytest.mark.parametrize(
    ("keys", "canonical", "first_repr", "second_repr"),
    [
        ((37, "37"), 37, "37", "'37'"),
        (("7", 7), 7, "'7'", "7"),
        (("7", "007"), 7, "'7'", "'007'"),
        (("007", "7"), 7, "'007'", "'7'"),
    ],
)
def test_checkpoint_dict_rejects_duplicate_grid_keys(
    two_real_states, keys, canonical, first_repr, second_repr
):
    """The mapping entry point reports canonical grid and both original keys."""
    payload = two_real_states["checkpoint"].to_dict()
    payload["grid_states"] = {
        keys[0]: two_real_states["obj_a"],
        keys[1]: two_real_states["obj_b"],
    }
    expected = _expected_duplicate_message(canonical, first_repr, second_repr)
    with pytest.raises(ValueError, match=f"^{re.escape(expected)}$"):
        SUEWSCheckpoint.from_dict(payload)


@pytest.mark.parametrize(
    ("source_key", "canonical"),
    [
        ("37", 37),
        ("037", 37),
        ("+7", 7),
        ("-0", 0),
        ("-07", -7),
        (7, 7),
    ],
)
def test_single_alias_preserves_existing_meaning_state_bytes_and_format(
    tmp_path, two_real_states, source_key, canonical
):
    """Surviving solo aliases preserve sign, leading-zero conversion, bytes, and format."""
    state_json = two_real_states["state_b"]
    base = two_real_states["checkpoint"]
    checkpoint = SUEWSCheckpoint.from_grid_states(
        {source_key: state_json},
        supy_version=base.supy_version,
        checkpoint_schema_version=base.checkpoint_schema_version,
        state_schema_version=base.state_schema_version,
        created_at=base.created_at,
        last_timestamp=base.last_timestamp,
    )
    assert checkpoint.grid_states == {canonical: state_json}

    dumped = checkpoint.to_dict()
    assert list(dumped["grid_states"]) == [str(canonical)]
    assert dumped["grid_states"][str(canonical)] == two_real_states["obj_b"]
    assert dumped["checkpoint_schema_version"] == base.checkpoint_schema_version
    assert dumped["state_schema_version"] == base.state_schema_version

    path = checkpoint.to_file(tmp_path / "solo.json")
    reloaded = SUEWSCheckpoint.from_file(path)
    assert reloaded.to_dict() == dumped


def test_noncolliding_multi_grid_keys_preserve_distinct_states(
    tmp_path, two_real_states
):
    """Non-colliding plain and zero-padded keys preserve every state without loss."""
    payload = two_real_states["checkpoint"].to_dict()
    payload["grid_states"] = {
        "7": two_real_states["obj_a"],
        "001": two_real_states["obj_b"],
    }
    path = tmp_path / "noncolliding.json"
    path.write_text(json.dumps(payload), encoding="utf-8")

    loaded = SUEWSCheckpoint.from_file(path)
    assert set(loaded.grid_states) == {1, 7}
    assert json.loads(loaded.grid_states[7]) == two_real_states["obj_a"]
    assert json.loads(loaded.grid_states[1]) == two_real_states["obj_b"]
    assert list(loaded.to_dict()["grid_states"]) == ["1", "7"]


@pytest.mark.parametrize(
    ("key_pair", "first_repr", "second_repr"),
    [
        (("1", "001"), "'1'", "'001'"),
        (("001", "1"), "'001'", "'1'"),
    ],
)
@pytest.mark.parametrize("equal_states", [False, True])
@pytest.mark.parametrize("method", ["from_checkpoint", "continue_from"])
def test_simulation_restart_entry_points_report_both_keys(
    tmp_path, two_real_states, key_pair, first_repr, second_repr, equal_states, method
):
    """Real SUEWSSimulation continuation routes report both colliding keys."""
    payload = two_real_states["checkpoint"].to_dict()
    obj_first = two_real_states["obj_a"]
    obj_second = obj_first if equal_states else two_real_states["obj_b"]
    payload["grid_states"] = {key_pair[0]: obj_first, key_pair[1]: obj_second}
    path = tmp_path / "colliding_restart.json"
    path.write_text(json.dumps(payload), encoding="utf-8")

    expected = _expected_duplicate_message(1, first_repr, second_repr)
    with pytest.raises(ValueError, match=f"^{re.escape(expected)}$"):
        if method == "from_checkpoint":
            SUEWSSimulation.from_checkpoint(two_real_states["config"], path)
        else:
            sim = SUEWSSimulation()
            sim.update_config(
                copy.deepcopy(two_real_states["config"]), auto_load_forcing=False
            )
            sim.continue_from(path)


def test_simulation_restart_accepts_plain_and_zero_padded_valid_keys(
    tmp_path, two_real_states
):
    """Surviving plain and zero-padded checkpoint files produce identical restarts."""
    forcing_next = two_real_states["forcing_continue"]
    outputs = {}
    final_states = {}
    for label, key in [("plain", "1"), ("padded", "001")]:
        payload = two_real_states["checkpoint"].to_dict()
        payload["grid_states"] = {key: two_real_states["obj_b"]}
        path = tmp_path / f"valid_{label}.json"
        path.write_text(json.dumps(payload), encoding="utf-8")

        sim = SUEWSSimulation.from_checkpoint(two_real_states["config"], path)
        sim.update_forcing(forcing_next)
        out = sim.run(
            start_date=forcing_next.index[0],
            end_date=forcing_next.index[-1],
            n_jobs=1,
        )
        outputs[label] = out.df
        final_states[label] = sim.checkpoint.grid_states[1]

    pd.testing.assert_frame_equal(outputs["plain"], outputs["padded"], check_exact=True)
    assert final_states["plain"] == final_states["padded"]


@pytest.mark.parametrize("entry", ["multi", "chunked", "streamed"])
@pytest.mark.parametrize(
    ("keys", "first_repr", "second_repr"),
    [
        ((1, "1"), "1", "'1'"),
        (("1", 1), "'1'", "1"),
        (("1", "001"), "'1'", "'001'"),
        (("001", "1"), "'001'", "'1'"),
        ((1, "+001"), "1", "'+001'"),
    ],
)
@pytest.mark.parametrize("equal_states", [False, True])
def test_direct_restart_routes_reject_aliases_before_native_execution(
    two_real_states, entry, keys, first_repr, second_repr, equal_states
):
    """All direct restart routes report canonical grid and both original keys."""
    config = two_real_states["config"]
    forcing = two_real_states["forcing_continue"].iloc[:2]
    state_first = two_real_states["state_a"]
    state_second = state_first if equal_states else two_real_states["state_b"]
    states = {keys[0]: state_first, keys[1]: state_second}
    expected = _expected_duplicate_message(1, first_repr, second_repr)
    with pytest.raises(ValueError, match=f"^{re.escape(expected)}$"):
        if entry == "multi":
            run_suews_rust_multi_with_state(config, forcing, states)
        elif entry == "chunked":
            run_suews_rust_chunked(config, forcing, initial_state_json_by_grid=states)
        else:
            list(
                iter_suews_rust_streamed(
                    config,
                    forcing,
                    300,
                    1,
                    initial_state_json_by_grid=states,
                )
            )


@pytest.mark.parametrize("entry", ["multi", "chunked", "streamed"])
def test_direct_restart_routes_preserve_value_wrapper_and_disambiguate_same_repr(
    two_real_states, entry
):
    """Direct routes keep ``.value`` conversion and disambiguate identical reprs."""
    config = two_real_states["config"]
    forcing = two_real_states["forcing_continue"].iloc[:2]
    box = _GridIdWrapper(1, mimic_int_repr=True)
    states = {1: two_real_states["state_a"], box: two_real_states["state_b"]}
    expected = _expected_duplicate_message(1, "int(1)", "_GridIdWrapper(1)")
    with pytest.raises(ValueError, match=f"^{re.escape(expected)}$"):
        if entry == "multi":
            run_suews_rust_multi_with_state(config, forcing, states)
        elif entry == "chunked":
            run_suews_rust_chunked(config, forcing, initial_state_json_by_grid=states)
        else:
            list(
                iter_suews_rust_streamed(
                    config,
                    forcing,
                    300,
                    1,
                    initial_state_json_by_grid=states,
                )
            )


@pytest.mark.parametrize("entry", ["multi", "chunked", "streamed"])
def test_direct_restart_routes_accept_plain_padded_and_wrapper_valid_keys(
    two_real_states, entry
):
    """Non-colliding plain, zero-padded, and wrapped keys produce identical restarts."""
    config = two_real_states["config"]
    forcing = two_real_states["forcing_continue"]
    state_b = two_real_states["state_b"]

    def _run(states_map):
        if entry == "multi":
            df_out, dict_state, _ = run_suews_rust_multi_with_state(
                config, forcing, states_map, serial_mode=True
            )
            return df_out, dict_state[1]
        if entry == "chunked":
            df_out, dict_state, _ = run_suews_rust_chunked(
                config,
                forcing,
                serial_mode=True,
                initial_state_json_by_grid=states_map,
            )
            return df_out, dict_state[1]
        chunks = list(
            iter_suews_rust_streamed(
                config,
                forcing,
                300,
                1,
                serial_mode=True,
                initial_state_json_by_grid=states_map,
            )
        )
        assert len(chunks) == 1
        return chunks[0].df_output, chunks[0].dict_state_json[1]

    df_int, final_int = _run({1: state_b})
    df_plain, final_plain = _run({"1": state_b})
    df_padded, final_padded = _run({"001": state_b})
    df_wrapped, final_wrapped = _run({_GridIdWrapper(1): state_b})

    for df_other, final_other in [
        (df_plain, final_plain),
        (df_padded, final_padded),
        (df_wrapped, final_wrapped),
    ]:
        pd.testing.assert_frame_equal(df_int, df_other, check_exact=True)
        assert final_int == final_other
