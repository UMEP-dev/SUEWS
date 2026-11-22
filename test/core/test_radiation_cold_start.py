"""Fresh radiation-memory input and observable cold-start provenance."""

from copy import deepcopy
import json

import pandas as pd
from pydantic import ValidationError
import pytest
import yaml

from supy import SUEWSSimulation
from supy._run_rust import (
    _check_rust_available,
    _prepare_forcing_block,
    config_to_engine_yaml,
)
from supy.data_model import SUEWSConfig
from supy.data_model.core.state import InitialStates
from supy.suews_checkpoint import SUEWSCheckpoint
from supy.util.converter.yaml_upgrade import YamlUpgradeError, upgrade_yaml

pytestmark = [
    pytest.mark.api,
    pytest.mark.contract(
        "C-VALID", "C-MIGRATE", "C-WARMSTART", "C-RESTART", "C-SAVE", "C-SCHEMA"
    ),
]


@pytest.mark.parametrize("name", ["qn_surfs", "dqndt_surf"])
@pytest.mark.parametrize("position", range(7))
def test_explicit_memory_rejected_at_python_and_native_boundaries(
    short_sample_sim, tmp_path, name, position
):
    sim = short_sample_sim()
    values = [0.0] * 7
    values[position] = 82.0
    raw = sim.config.model_dump(mode="json")
    raw["sites"][0]["initial_states"][name] = values
    for construct in (SUEWSConfig.from_dict, SUEWSConfig.model_validate):
        with pytest.raises(ValueError, match=name):
            construct(deepcopy(raw))
    path = tmp_path / "explicit.yml"
    path.write_text(yaml.safe_dump(raw), encoding="utf-8")
    with pytest.raises(ValueError, match=name):
        SUEWSSimulation(path)
    with pytest.raises(ValueError, match=name):
        sim.update_config({"sites": raw["sites"]}, auto_load_forcing=False)
    with pytest.raises(ValueError, match=name):
        InitialStates(**{name: values})
    resolved = yaml.safe_load(config_to_engine_yaml(sim.config))
    resolved["sites"][0]["initial_states"][name] = values
    forcing = _prepare_forcing_block(sim.forcing.df.iloc[:2]).ravel().tolist()
    with pytest.raises(RuntimeError, match=name):
        _check_rust_available().run_suews(yaml.safe_dump(resolved), forcing, 2)


@pytest.mark.parametrize("name", ["qn_surfs", "dqndt_surf"])
def test_current_zero_and_null_memory_are_not_user_inputs(short_sample_sim, name):
    sim = short_sample_sim()
    forcing = _prepare_forcing_block(sim.forcing.df.iloc[:2]).ravel().tolist()
    for value in ([0.0] * 7, None):
        raw = sim.config.model_dump(mode="json")
        raw["sites"][0]["initial_states"][name] = value
        for validate in (True, False):
            with pytest.raises(ValueError, match=name):
                SUEWSConfig.from_dict(raw, use_conditional_validation=validate)
        resolved = yaml.safe_load(config_to_engine_yaml(sim.config))
        resolved["sites"][0]["initial_states"][name] = value
        with pytest.raises(RuntimeError, match=name):
            _check_rust_available().run_suews(yaml.safe_dump(resolved), forcing, 2)


def test_legacy_zero_migration_and_nonzero_refusal(short_sample_sim, tmp_path, capsys):
    raw = short_sample_sim().config.model_dump(mode="json")
    raw["schema_version"] = "2026.6.dev12"
    for name in ("qn_surfs", "dqndt_surf"):
        raw["sites"][0]["initial_states"][name] = [0.0] * 7
    config = SUEWSConfig.from_dict(deepcopy(raw))
    notice = capsys.readouterr().err
    assert "legacy zero placeholder" in notice
    assert "qn_surfs" in notice and "dqndt_surf" in notice
    for name in ("qn_surfs", "dqndt_surf"):
        assert name not in config.sites[0].initial_states.model_dump()
    src = tmp_path / "legacy.yml"
    dst = tmp_path / "current.yml"
    src.write_text(yaml.safe_dump(raw), encoding="utf-8")
    upgrade_yaml(src, dst)
    migrated = yaml.safe_load(dst.read_text(encoding="utf-8"))
    assert migrated["schema_version"] == "2026.6.dev13"
    assert "qn_surfs" not in migrated["sites"][0]["initial_states"]
    raw["sites"][0]["initial_states"]["qn_surfs"][0] = 1.0
    with pytest.raises(YamlUpgradeError, match="complete checkpoint"):
        SUEWSConfig.from_dict(raw)


def run(sim, forcing):
    sim.update_forcing(forcing)
    return sim.run(start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1)


def test_cold_metadata_all_rows_and_complete_checkpoint_continuation(
    short_sample_sim, tmp_path
):
    sim = short_sample_sim()
    sim.update_config(
        {"model": {"physics": {"storage_heat": 6}}}, auto_load_forcing=False
    )
    forcing = sim.forcing.df.iloc[:24]
    full = run(sim, forcing)
    assert full.df.index.get_level_values("datetime").equals(forcing.index)
    assert full.metadata["initialisation"] == {
        "origin": "cold_start",
        "radiation_memory": "internal",
        "warmup_completion": "not_assessed",
    }
    assert sim.output.metadata == full.metadata
    sim.save(tmp_path / "cold", format="txt", freq_s=300, output_groups=["SUEWS"])
    saved = json.loads((tmp_path / "cold/provenance.json").read_text(encoding="utf-8"))
    assert saved["run"]["initialisation"] == full.metadata["initialisation"]
    first = short_sample_sim()
    first.update_config(
        {"model": {"physics": {"storage_heat": 6}}}, auto_load_forcing=False
    )
    run(first, forcing.iloc[:12])
    payload = deepcopy(first.checkpoint.grid_states)
    restart = SUEWSSimulation.from_checkpoint(first.config, first.checkpoint)
    tail = run(restart, forcing.iloc[12:])
    pd.testing.assert_frame_equal(tail.df, full.df.loc[tail.df.index], check_exact=True)
    assert tail.checkpoint.grid_states == full.checkpoint.grid_states
    assert tail.metadata["initialisation"]["origin"] == "checkpoint"
    assert tail.metadata["initialisation"]["warmup_completion"] == "not_assessed"
    # Ensure the input checkpoint, including all 14 values and clock, is not edited.
    assert first.checkpoint.grid_states == payload
    again = run(first, forcing.iloc[12:])
    assert again.metadata["initialisation"]["origin"] == "checkpoint"
    assert again.checkpoint.grid_states == full.checkpoint.grid_states
    restart.save(tmp_path / "resume", format="txt", freq_s=300, output_groups=["SUEWS"])
    saved = json.loads(
        (tmp_path / "resume/provenance.json").read_text(encoding="utf-8")
    )
    assert saved["run"]["initialisation"]["origin"] == "checkpoint"


def test_checkpoint_keeps_distinct_all_fourteen_memories(short_sample_sim, tmp_path):
    sim = short_sample_sim()
    sim.update_config(
        {"model": {"physics": {"storage_heat": 6}}}, auto_load_forcing=False
    )
    forcing = sim.forcing.df.iloc[:24]
    run(sim, forcing.iloc[:12])
    (key,) = sim.checkpoint.grid_states
    payload = json.loads(sim.checkpoint.grid_states[key])
    values = [float(i + 1) for i in range(14)]
    payload["state"]["members"]["ohm_state"]["values"][2:16] = values
    checkpoint = SUEWSCheckpoint.from_grid_states(
        {key: payload}, last_timestamp=sim.checkpoint.last_timestamp
    )
    restored = SUEWSCheckpoint.from_file(
        checkpoint.to_file(tmp_path / "checkpoint.json")
    )
    actual = json.loads(restored.grid_states[key])
    assert actual["state"]["members"]["ohm_state"]["values"][2:16] == values
    assert actual["timer"] == payload["timer"]
    result = run(
        SUEWSSimulation.from_checkpoint(sim.config, restored), forcing.iloc[12:]
    )
    assert result.metadata["initialisation"]["origin"] == "checkpoint"


@pytest.mark.parametrize("name", ["qn_surfs", "dqndt_surf"])
def test_legacy_dataframe_only_accepts_zero_placeholders(short_sample_sim, name):
    frame = short_sample_sim().config.to_df_state()
    grid = frame.index[0]
    restored = InitialStates.from_df_state(frame, grid)
    assert name not in restored.model_dump()
    frame.loc[grid, (name, "(3,)")] = 1.0
    with pytest.raises(ValueError, match="complete checkpoint"):
        InitialStates.from_df_state(frame, grid)


def test_streamed_provenance_keeps_cold_start_origin(short_sample_sim, tmp_path):
    sim = short_sample_sim()
    forcing = sim.forcing.df.iloc[:24]
    sim.update_config(
        {
            "model": {
                "control": {
                    "output": {
                        "format": "txt",
                        "freq": 300,
                        "dir": str(tmp_path),
                        "groups": ["SUEWS"],
                        "stream_chunk": 1,
                    }
                }
            }
        },
        auto_load_forcing=False,
    )
    output = run(sim, forcing)
    saved = json.loads((tmp_path / "provenance.json").read_text(encoding="utf-8"))
    assert saved["run"]["initialisation"] == output.metadata["initialisation"]
    assert saved["run"]["initialisation"]["origin"] == "cold_start"
    assert saved["period"]["actual"]["n_timesteps"] == len(forcing)


@pytest.mark.parametrize("include_internal", [False, True])
def test_published_schema_rejects_memory_without_backdating_the_rule(include_internal):
    from jsonschema import Draft202012Validator, ValidationError
    from supy.data_model.configuration.publisher import generate_json_schema

    for version in ("2026.6.dev12", "2026.6.dev13"):
        schema = generate_json_schema(version, include_internal=include_internal)
        validator = Draft202012Validator({
            "$ref": "#/$defs/InitialStates",
            "$defs": schema["$defs"],
        })
        validator.validate({})
        for name in ("qn_surfs", "dqndt_surf"):
            for values in ([0.0] * 7, None):
                if version == "2026.6.dev13":
                    with pytest.raises(ValidationError):
                        validator.validate({name: values})
                else:
                    validator.validate({name: values})


@pytest.mark.parametrize("sites", [None, 17, "invalid", {"gridiv": 1}])
@pytest.mark.parametrize("version", [None, "2026.6.dev13", "2026.6.dev12"])
def test_invalid_sites_reaches_public_validation(sites, version, tmp_path):
    raw = {"sites": sites}
    if version is not None:
        raw["schema_version"] = version
    with pytest.raises(ValidationError) as error:
        SUEWSConfig.model_validate(deepcopy(raw))
    assert any(
        e["loc"] == ("sites",) and e["type"] == "list_type"
        for e in error.value.errors()
    )
    with pytest.raises(ValueError, match=r"(?s)sites.*list_type"):
        SUEWSConfig.from_dict(deepcopy(raw))
    path = tmp_path / "invalid.yml"
    path.write_text(yaml.safe_dump(raw), encoding="utf-8")
    with pytest.raises(ValueError, match=r"(?s)sites.*list_type"):
        SUEWSConfig.from_yaml(path)


@pytest.mark.parametrize("sites", [None, 17, "invalid", {"gridiv": 1}])
def test_legacy_converter_preserves_invalid_sites_for_validation(sites, tmp_path):
    # Conversion migrates owned fields; it must not repair malformed structure.
    src = tmp_path / "legacy.yml"
    dst = tmp_path / "converted.yml"
    src.write_text(
        yaml.safe_dump({"schema_version": "2026.6.dev12", "sites": sites}),
        encoding="utf-8",
    )
    upgrade_yaml(src, dst)
    converted = yaml.safe_load(dst.read_text(encoding="utf-8"))
    assert converted["sites"] == sites
    assert converted["schema_version"] == "2026.6.dev13"
    with pytest.raises(ValueError, match=r"(?s)sites.*list_type"):
        SUEWSConfig.from_yaml(dst)
