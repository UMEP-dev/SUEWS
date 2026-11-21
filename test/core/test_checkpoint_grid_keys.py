"""Ambiguous public checkpoint keys must fail before state is discarded."""

import pytest

from supy import SUEWSSimulation
from supy._run_rust import (  # ruff: ignore[import-private-name]
    iter_suews_rust_streamed,
    run_suews_rust_chunked,
    run_suews_rust_multi_with_state,
)
from supy.suews_checkpoint import SUEWSCheckpoint

pytestmark = pytest.mark.api


@pytest.mark.parametrize("keys", [(37, "37"), (37, "037"), (0, "-0")])
@pytest.mark.parametrize("factory", [SUEWSCheckpoint, SUEWSCheckpoint.from_grid_states])
def test_checkpoint_constructor_rejects_normalised_aliases(keys, factory):
    """Keep established int coercion, but never discard another key's state."""
    with pytest.raises(ValueError, match="Duplicate checkpoint grid ID"):
        factory(dict.fromkeys(keys, "{}"))


@pytest.mark.parametrize("members", ['"37": {}, "37": {}', '"37": {}, "037": {}'])
def test_checkpoint_file_rejects_duplicate_grid_keys(tmp_path, members):
    """Catch duplicate literal JSON members as well as coerced grid aliases."""
    path = tmp_path / "checkpoint.json"
    path.write_text('{"grid_states": {' + members + '}}', encoding="utf-8")
    with pytest.raises(ValueError, match="Duplicate checkpoint"):
        SUEWSCheckpoint.from_file(path)


def test_checkpoint_dict_rejects_duplicate_grid_keys():
    """The mapping entry point uses the same checked normalisation."""
    with pytest.raises(ValueError, match="Duplicate checkpoint grid ID"):
        SUEWSCheckpoint.from_dict({"grid_states": {37: {}, "37": {}}})


def test_single_alias_still_has_its_existing_meaning():
    """An unambiguous numeric string is still an accepted public key."""
    checkpoint = SUEWSCheckpoint.from_grid_states({"037": "{}"})
    assert checkpoint.grid_states == {37: "{}"}
    assert checkpoint.to_dict()["grid_states"] == {"37": {}}


@pytest.mark.parametrize("entry", ["multi", "chunked", "streamed"])
def test_direct_restart_routes_reject_aliases_before_native_execution(entry):
    """All restart routes reject ambiguous public state keys consistently."""
    sim = SUEWSSimulation.from_sample_data()
    forcing = sim.forcing.df.iloc[:2]
    states = {1: "{}", "1": "{}"}
    with pytest.raises(ValueError, match="Duplicate checkpoint grid ID"):
        if entry == "multi":
            run_suews_rust_multi_with_state(sim.config, forcing, states)
        elif entry == "chunked":
            run_suews_rust_chunked(sim.config, forcing, initial_state_json_by_grid=states)
        else:
            list(iter_suews_rust_streamed(
                sim.config, forcing, 300, 1, initial_state_json_by_grid=states,
            ))
