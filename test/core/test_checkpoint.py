"""Tests for typed SUEWS restart checkpoints."""

import importlib
import json
from pathlib import Path

import pandas as pd
import pytest

try:
    from importlib.resources import files
except ImportError:
    from importlib_resources import files

from supy.suews_checkpoint import SUEWSCheckpoint
from supy.suews_sim import SUEWSSimulation

pytestmark = pytest.mark.api

SAMPLE_BUILDING_DYOHM_PROFILE_INDEX = 70
TIMESTEPS_PER_DAY = 288
CHECKPOINT_RTOL = 1e-12
CHECKPOINT_ATOL = 1e-12
TIMER_SENSITIVE_OUTPUTS = {
    ("SUEWS", "QS"),
    ("SUEWS", "T2"),
    ("debug", "dqndt"),
}


def _checkpoint_payload(state_schema_version=1):
    """Build the smallest version-2 envelope needed by API validation tests."""
    return {
        "checkpoint_schema_version": 2,
        "timer": {
            "dt_since_start": 600,
            "dt_since_start_prev": 300,
            "tstep": 300,
            "new_day": 0,
        },
        "state": {
            "schema_version": state_schema_version,
            "members": {},
        },
    }


def test_checkpoint_json_roundtrip(tmp_path):
    """Checkpoint files preserve typed Rust state by grid ID."""
    payload = _checkpoint_payload(state_schema_version=3)
    payload["state"]["members"] = {"demo_state": {"value": 1}}
    checkpoint = SUEWSCheckpoint.from_grid_states(
        {1: json.dumps(payload)},
        last_timestamp=pd.Timestamp("2012-01-01 00:55:00"),
    )

    path = checkpoint.to_file(tmp_path / "site_SUEWS_checkpoint.json")
    loaded = SUEWSCheckpoint.from_file(path)

    assert loaded.grid_states.keys() == {1}
    assert json.loads(loaded.grid_states[1]) == payload
    assert loaded.checkpoint_schema_version == 2
    assert loaded.state_schema_version == 3
    assert loaded.last_timestamp == "2012-01-01T00:55:00"


def test_run_stores_non_empty_checkpoint():
    """SUEWSSimulation.run() exposes Rust state_json as a checkpoint."""
    sim = SUEWSSimulation.from_sample_data()
    forcing = sim.forcing.df.iloc[:12]
    sim.update_forcing(forcing)

    output = sim.run()

    assert isinstance(sim.checkpoint, SUEWSCheckpoint)
    assert output.checkpoint == sim.checkpoint
    assert sim.checkpoint.grid_states
    assert sim.checkpoint.checkpoint_schema_version == 2
    assert sim.checkpoint.last_timestamp == forcing.index.max().isoformat()
    checkpoint_payload = json.loads(next(iter(sim.checkpoint.grid_states.values())))
    assert checkpoint_payload["timer"] == {
        "dt_since_start": 3900,
        "dt_since_start_prev": 0,
        "tstep": 300,
        "new_day": 0,
    }


def test_checkpoint_continuation_calls_rust_state_path(monkeypatch):
    """Continuation from checkpoint uses the Rust state bridge API."""
    rust_module = importlib.import_module("supy._run_rust")
    sim1 = SUEWSSimulation.from_sample_data()
    forcing = sim1.forcing.df.iloc[:24]
    sim1.update_forcing(forcing.iloc[:12])
    sim1.run()

    calls = {"count": 0}
    bridge_module = rust_module._check_rust_available()
    original = bridge_module.run_suews_with_state

    def wrapped_run_suews_with_state(*args, **kwargs):
        calls["count"] += 1
        return original(*args, **kwargs)

    monkeypatch.setattr(
        bridge_module,
        "run_suews_with_state",
        wrapped_run_suews_with_state,
    )

    sim2 = SUEWSSimulation.from_checkpoint(sim1.config, sim1.checkpoint)
    sim2.update_forcing(forcing.iloc[12:24])
    sim2.run()

    assert calls["count"] >= 1


def test_checkpoint_continuation_restores_legacy_null_marker():
    """A legacy null state value is restored as a NaN sentinel."""
    sim_first = SUEWSSimulation.from_sample_data()
    forcing = sim_first.forcing.df.iloc[:24]
    sim_first.update_forcing(forcing.iloc[:12])
    sim_first.run()

    dict_grid_states = {
        grid_id: json.loads(state_json)
        for grid_id, state_json in sim_first.checkpoint.grid_states.items()
    }
    first_grid_id = next(iter(dict_grid_states))
    dict_grid_states[first_grid_id]["state"]["members"]["heat_state"]["values"][
        SAMPLE_BUILDING_DYOHM_PROFILE_INDEX
    ] = None
    checkpoint = SUEWSCheckpoint.from_grid_states(
        dict_grid_states,
        last_timestamp=sim_first.checkpoint.last_timestamp,
    )

    sim_second = SUEWSSimulation.from_checkpoint(sim_first.config, checkpoint)
    sim_second.update_forcing(forcing.iloc[12:24])

    output = sim_second.run()

    assert not output.df.empty


def test_checkpoint_continuation_reports_invalid_value_path():
    """An unsupported state value reports its exact checkpoint path."""
    sim_first = SUEWSSimulation.from_sample_data()
    forcing = sim_first.forcing.df.iloc[:24]
    sim_first.update_forcing(forcing.iloc[:12])
    sim_first.run()

    dict_grid_states = {
        grid_id: json.loads(state_json)
        for grid_id, state_json in sim_first.checkpoint.grid_states.items()
    }
    first_grid_id = next(iter(dict_grid_states))
    dict_grid_states[first_grid_id]["state"]["members"]["atm_state"]["values"][0] = (
        "NaN"
    )
    checkpoint = SUEWSCheckpoint.from_grid_states(
        dict_grid_states,
        last_timestamp=sim_first.checkpoint.last_timestamp,
    )

    sim_second = SUEWSSimulation.from_checkpoint(sim_first.config, checkpoint)
    sim_second.update_forcing(forcing.iloc[12:24])

    with pytest.raises(
        RuntimeError,
        match=(
            r"invalid checkpoint state value at members\.atm_state\.values\[0\]: "
            r"expected a JSON number or null NaN marker, found string"
        ),
    ):
        sim_second.run()


def test_external_checkpoint_matches_continuous_run_across_day_boundary():
    """External continuation agrees to 1e-12 across a 24-hour boundary."""
    sim_full = SUEWSSimulation.from_sample_data()
    forcing = sim_full.forcing.df.iloc[:300]
    sim_full.update_forcing(forcing)
    output_full = sim_full.run()

    sim_first = SUEWSSimulation.from_sample_data()
    sim_first.update_forcing(forcing.iloc[:TIMESTEPS_PER_DAY])
    sim_first.run()

    sim_second = SUEWSSimulation.from_checkpoint(
        sim_first.config,
        sim_first.checkpoint,
    )
    sim_second.update_forcing(forcing.iloc[TIMESTEPS_PER_DAY:])
    output_second = sim_second.run()

    expected_second = output_full.df.loc[output_second.df.index]
    assert TIMER_SENSITIVE_OUTPUTS.issubset(output_second.df.columns)
    pd.testing.assert_frame_equal(
        output_second.df,
        expected_second,
        check_exact=False,
        rtol=CHECKPOINT_RTOL,
        atol=CHECKPOINT_ATOL,
    )


def test_chunked_run_matches_uninterrupted_run_across_day_boundary():
    """Internal one-day chunks agree to 1e-12 with an uninterrupted run."""
    sim_full = SUEWSSimulation.from_sample_data()
    forcing = sim_full.forcing.df.iloc[:300]
    sim_full.update_forcing(forcing)
    output_full = sim_full.run(chunk_day=3660, n_jobs=1)

    sim_chunked = SUEWSSimulation.from_sample_data()
    sim_chunked.update_forcing(forcing)
    output_chunked = sim_chunked.run(chunk_day=1, n_jobs=1)

    assert TIMER_SENSITIVE_OUTPUTS.issubset(output_chunked.df.columns)
    pd.testing.assert_frame_equal(
        output_chunked.df,
        output_full.df,
        check_exact=False,
        rtol=CHECKPOINT_RTOL,
        atol=CHECKPOINT_ATOL,
    )


def test_from_checkpoint_requires_config():
    """A checkpoint alone is not enough to continue a run."""
    checkpoint = SUEWSCheckpoint.from_grid_states({
        1: {"schema_version": 1, "members": {}}
    })

    with pytest.raises(ValueError, match="requires a YAML/SUEWSConfig"):
        SUEWSSimulation.from_checkpoint(None, checkpoint)

    with pytest.raises(RuntimeError, match="requires a loaded configuration"):
        SUEWSSimulation().continue_from(checkpoint)


def test_checkpoint_grid_ids_must_match_config():
    """Checkpoint continuation rejects missing and unexpected grid states."""
    config_path = files("supy").joinpath("sample_data/sample_config.yml")
    sim = SUEWSSimulation(str(config_path))
    forcing = sim.forcing.df.iloc[:12]
    sim.update_forcing(forcing)
    checkpoint = SUEWSCheckpoint.from_grid_states({2: _checkpoint_payload()})
    sim.continue_from(checkpoint)

    with pytest.raises(ValueError, match=r"missing checkpoint states.*unexpected"):
        sim.run()


def test_legacy_checkpoint_requires_timer_metadata():
    """Version-1 state-only checkpoints fail with an actionable migration error."""
    sim = SUEWSSimulation.from_sample_data()
    checkpoint = SUEWSCheckpoint.from_grid_states({
        1: {"schema_version": 1, "members": {}}
    })

    with pytest.raises(ValueError, match="has no elapsed timer metadata"):
        sim.continue_from(checkpoint)


def test_checkpoint_timestep_must_match_config():
    """Continuation rejects timer metadata from a different model timestep."""
    sim_first = SUEWSSimulation.from_sample_data()
    forcing = sim_first.forcing.df.iloc[:24]
    sim_first.update_forcing(forcing.iloc[:12])
    sim_first.run()

    grid_states = {
        grid_id: json.loads(state_json)
        for grid_id, state_json in sim_first.checkpoint.grid_states.items()
    }
    first_grid_id = next(iter(grid_states))
    grid_states[first_grid_id]["timer"]["tstep"] = 600
    checkpoint = SUEWSCheckpoint.from_grid_states(
        grid_states,
        last_timestamp=sim_first.checkpoint.last_timestamp,
    )
    sim_second = SUEWSSimulation.from_checkpoint(sim_first.config, checkpoint)
    sim_second.update_forcing(forcing.iloc[12:])

    with pytest.raises(RuntimeError, match="does not match configuration timestep"):
        sim_second.run()


def test_checkpoint_file_continuation_roundtrip(tmp_path):
    """Checkpoint JSON files can be read back into continuation runs."""
    config_path = files("supy").joinpath("sample_data/sample_config.yml")
    sim1 = SUEWSSimulation(str(config_path))
    forcing = sim1.forcing.df.iloc[:24]
    sim1.update_forcing(forcing.iloc[:12])
    sim1.run()

    checkpoint_path = sim1.checkpoint.to_file(tmp_path / "Kc_SUEWS_checkpoint.json")
    sim2 = SUEWSSimulation.from_checkpoint(config_path, checkpoint_path)
    sim2.update_forcing(forcing.iloc[12:24])
    output = sim2.run()

    assert output.checkpoint is not None
    assert not output.df.empty
    assert Path(checkpoint_path).exists()
