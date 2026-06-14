"""Tests for typed SUEWS restart checkpoints."""

import importlib
import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

try:
    from importlib.resources import files
except ImportError:
    from importlib_resources import files

from supy.suews_checkpoint import SUEWSCheckpoint
from supy.suews_sim import SUEWSSimulation

pytestmark = pytest.mark.api


def test_checkpoint_json_roundtrip(tmp_path):
    """Checkpoint files preserve typed Rust state by grid ID."""
    payload = {"schema_version": 3, "members": {"demo_state": {"value": 1}}}
    checkpoint = SUEWSCheckpoint.from_grid_states(
        {1: json.dumps(payload)},
        last_timestamp=pd.Timestamp("2012-01-01 00:55:00"),
    )

    path = checkpoint.to_file(tmp_path / "site_SUEWS_checkpoint.json")
    loaded = SUEWSCheckpoint.from_file(path)

    assert loaded.grid_states.keys() == {1}
    assert json.loads(loaded.grid_states[1]) == payload
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
    assert sim.checkpoint.last_timestamp == forcing.index.max().isoformat()


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


def test_split_run_matches_continuous_run_with_checkpoint():
    """A checkpoint continuation matches key continuous-run outputs."""
    sim_full = SUEWSSimulation.from_sample_data()
    forcing = sim_full.forcing.df.iloc[:48]
    sim_full.update_forcing(forcing)
    output_full = sim_full.run()

    sim_first = SUEWSSimulation.from_sample_data()
    sim_first.update_forcing(forcing.iloc[:24])
    sim_first.run()

    sim_second = SUEWSSimulation.from_checkpoint(
        sim_first.config,
        sim_first.checkpoint,
    )
    sim_second.update_forcing(forcing.iloc[24:48])
    output_second = sim_second.run()

    expected_second = output_full.df.loc[output_second.df.index]
    tolerances = {
        "QN": {"rtol": 0.008, "atol": 0.1},
        "QF": {"rtol": 0.008, "atol": 0.1},
        "QS": {"rtol": 0.010, "atol": 0.2},
        "QE": {"rtol": 0.010, "atol": 0.2},
        "QH": {"rtol": 0.010, "atol": 0.2},
        "T2": {"rtol": 0.005, "atol": 0.05},
        "RH2": {"rtol": 0.010, "atol": 0.5},
        "U10": {"rtol": 0.010, "atol": 0.05},
    }

    for var_name, tolerance in tolerances.items():
        column = ("SUEWS", var_name)
        if column not in output_second.df.columns:
            continue
        assert np.allclose(
            output_second.df[column],
            expected_second[column],
            rtol=tolerance["rtol"],
            atol=tolerance["atol"],
            equal_nan=True,
        ), f"{var_name} differs outside tolerance"


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
    checkpoint = SUEWSCheckpoint.from_grid_states({
        2: {"schema_version": 1, "members": {}}
    })
    sim.continue_from(checkpoint)

    with pytest.raises(ValueError, match=r"missing checkpoint states.*unexpected"):
        sim.run()


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
