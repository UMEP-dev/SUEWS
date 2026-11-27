"""Fresh radiation-memory input and observable cold-start provenance."""

from copy import deepcopy
from dataclasses import dataclass
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
from supy.data_model.configuration import CURRENT_SCHEMA_VERSION
from supy.data_model.core.state import (
    InitialStateDectr,
    InitialStateEvetr,
    InitialStateGrass,
    InitialStates,
)
from supy.suews_checkpoint import SUEWSCheckpoint
from supy.util.converter.yaml_upgrade import YamlUpgradeError, upgrade_yaml

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract(
        "C-VALID", "C-MIGRATE", "C-WARMSTART", "C-RESTART", "C-SAVE", "C-SCHEMA"
    ),
]


@dataclass(frozen=True)
class _ShortSample:
    """Read-only artefacts of one short-sample simulation; copies on hand-out."""

    _sim: SUEWSSimulation
    _raw: dict
    _resolved: dict
    _forcing: tuple
    _df_state: pd.DataFrame

    def raw(self) -> dict:
        """A private copy of ``config.model_dump(mode="json")``."""
        return deepcopy(self._raw)

    def resolved(self) -> dict:
        """A private copy of the resolved engine YAML, parsed."""
        return deepcopy(self._resolved)

    def forcing(self) -> list:
        """The first two forcing rows as the flat list the bridge takes."""
        return list(self._forcing)

    def df_state(self) -> pd.DataFrame:
        """A private copy of ``config.to_df_state()``."""
        return self._df_state.copy()

    def sim(self) -> SUEWSSimulation:
        """A deep copy of the YAML-built simulation, free to mutate."""
        return deepcopy(self._sim)


@pytest.fixture(scope="module")
def short_sample(short_sample_sim):
    """Build the short sample's config views and two-step forcing once.

    The rejection and legacy tests below only read these views of the short
    sample, or call ``update_config`` on a simulation built from its YAML.
    Each case used to rebuild that simulation (YAML validation plus a month of
    forcing) and re-derive every view; they are now derived once from one
    ``short_sample_sim()`` and handed out as copies. Tests that run or
    continue a simulation still build their own through ``short_sample_sim``.
    """
    sim = short_sample_sim()
    config = sim.config
    return _ShortSample(
        sim,
        config.model_dump(mode="json"),
        yaml.safe_load(config_to_engine_yaml(config)),
        tuple(_prepare_forcing_block(sim.forcing.df.iloc[:2]).ravel().tolist()),
        config.to_df_state(),
    )


@pytest.mark.parametrize("name", ["qn_surfs", "dqndt_surf"])
@pytest.mark.parametrize("position", range(7))
def test_explicit_memory_rejected_at_python_and_native_boundaries(
    short_sample, tmp_path, name, position
):
    values = [0.0] * 7
    values[position] = 82.0
    raw = short_sample.raw()
    raw["sites"][0]["initial_states"][name] = values
    for construct in (SUEWSConfig.from_dict, SUEWSConfig.model_validate):
        with pytest.raises(ValueError, match=name):
            construct(deepcopy(raw))
    path = tmp_path / "explicit.yml"
    path.write_text(yaml.safe_dump(raw), encoding="utf-8")
    with pytest.raises(ValueError, match=name):
        SUEWSSimulation(path)
    sim = short_sample.sim()
    with pytest.raises(ValueError, match=name):
        sim.update_config({"sites": raw["sites"]}, auto_load_forcing=False)
    with pytest.raises(ValueError, match=name):
        InitialStates(**{name: values})
    resolved = short_sample.resolved()
    resolved["sites"][0]["initial_states"][name] = values
    with pytest.raises(RuntimeError, match=name):
        _check_rust_available().run_suews(
            yaml.safe_dump(resolved), short_sample.forcing(), 2
        )


@pytest.mark.parametrize("name", ["qn_surfs", "dqndt_surf"])
def test_current_zero_and_null_memory_are_not_user_inputs(short_sample, name):
    for value in ([0.0] * 7, None):
        raw = short_sample.raw()
        raw["sites"][0]["initial_states"][name] = value
        for validate in (True, False):
            with pytest.raises(ValueError, match=name):
                SUEWSConfig.from_dict(raw, use_conditional_validation=validate)
        resolved = short_sample.resolved()
        resolved["sites"][0]["initial_states"][name] = value
        with pytest.raises(RuntimeError, match=name):
            _check_rust_available().run_suews(
                yaml.safe_dump(resolved), short_sample.forcing(), 2
            )


def test_legacy_zero_migration_and_nonzero_refusal(short_sample, tmp_path, capsys):
    raw = short_sample.raw()
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
    assert migrated["schema_version"] == CURRENT_SCHEMA_VERSION
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
def test_legacy_dataframe_only_accepts_zero_placeholders(short_sample, name):
    frame = short_sample.df_state()
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

    for version in (
        "2026.6.dev12",
        "2026.6.dev13",
        "2026.6.dev14",
        "2026.6.dev15",
        "2026.6.dev16",
        "2026.6.dev17",
    ):
        schema = generate_json_schema(version, include_internal=include_internal)
        validator = Draft202012Validator({
            "$ref": "#/$defs/InitialStates",
            "$defs": schema["$defs"],
        })
        validator.validate({})
        for name in ("qn_surfs", "dqndt_surf"):
            for values in ([0.0] * 7, None):
                if version in (
                    "2026.6.dev13",
                    "2026.6.dev14",
                    "2026.6.dev15",
                    "2026.6.dev16",
                    "2026.6.dev17",
                ):
                    with pytest.raises(ValidationError):
                        validator.validate({name: values})
                else:
                    validator.validate({name: values})
        for name in (
            "dt_since_start",
            "tair_av",
            "tstep_prev",
            "l_mod",
            "ustar",
            "ra_h",
            "rb",
            "rs",
        ):
            for value in (0.0, None):
                if version in (
                    "2026.6.dev14",
                    "2026.6.dev15",
                    "2026.6.dev16",
                    "2026.6.dev17",
                ):
                    with pytest.raises(ValidationError):
                        validator.validate({name: value})
                else:
                    validator.validate({name: value})
        for def_name in ("InitialStateEvetr", "InitialStateDectr", "InitialStateGrass"):
            veg_validator = Draft202012Validator({
                "$ref": f"#/$defs/{def_name}",
                "$defs": schema["$defs"],
            })
            veg_validator.validate({})
            for value in ({"wu_total": 0.0, "wu_auto": 0.0, "wu_manual": 0.0}, None):
                if version in (
                    "2026.6.dev14",
                    "2026.6.dev15",
                    "2026.6.dev16",
                    "2026.6.dev17",
                ):
                    with pytest.raises(ValidationError):
                        veg_validator.validate({"wu": value})
                else:
                    veg_validator.validate({"wu": value})


@pytest.mark.parametrize("sites", [None, 17, "invalid", {"gridiv": 1}])
@pytest.mark.parametrize(
    "version",
    [
        None,
        "2026.6.dev17",
        "2026.6.dev16",
        "2026.6.dev15",
        "2026.6.dev14",
        "2026.6.dev13",
        "2026.6.dev12",
    ],
)
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
    assert converted["schema_version"] == CURRENT_SCHEMA_VERSION
    with pytest.raises(ValueError, match=r"(?s)sites.*list_type"):
        SUEWSConfig.from_yaml(dst)


@pytest.mark.parametrize(
    "name",
    ["dt_since_start", "tair_av", "tstep_prev", "l_mod", "ustar", "ra_h", "rb", "rs"],
)
def test_retired_scalar_continuation_state_rejected_at_python_and_native_boundaries(
    short_sample, tmp_path, name
):
    for value in (0.0, 12.5, None):
        raw = short_sample.raw()
        raw["sites"][0]["initial_states"][name] = value
        for construct in (SUEWSConfig.from_dict, SUEWSConfig.model_validate):
            with pytest.raises(ValueError, match=name):
                construct(deepcopy(raw))
        with pytest.raises(ValueError, match=name):
            InitialStates(**{name: value})
        resolved = short_sample.resolved()
        resolved["sites"][0]["initial_states"][name] = value
        with pytest.raises(RuntimeError, match=name):
            _check_rust_available().run_suews(
                yaml.safe_dump(resolved), short_sample.forcing(), 2
            )
    path = tmp_path / f"explicit_{name}.yml"
    raw = short_sample.raw()
    raw["sites"][0]["initial_states"][name] = 0.0
    path.write_text(yaml.safe_dump(raw), encoding="utf-8")
    with pytest.raises(ValueError, match=name):
        SUEWSSimulation(path)
    sim = short_sample.sim()
    with pytest.raises(ValueError, match=name):
        sim.update_config({"sites": raw["sites"]}, auto_load_forcing=False)


@pytest.mark.parametrize(
    ("veg", "veg_cls"),
    [
        ("evetr", InitialStateEvetr),
        ("dectr", InitialStateDectr),
        ("grass", InitialStateGrass),
    ],
)
def test_retired_veg_water_use_rejected_at_python_and_native_boundaries(
    short_sample, tmp_path, veg, veg_cls
):
    zero_wu = {
        "wu_total": {"value": 0.0},
        "wu_auto": {"value": 0.0},
        "wu_manual": {"value": 0.0},
    }
    nonzero_wu = {"wu_total": 5.0, "wu_auto": 3.0, "wu_manual": 2.0}
    for value in (zero_wu, nonzero_wu, None):
        raw = short_sample.raw()
        raw["sites"][0]["initial_states"][veg]["wu"] = deepcopy(value)
        for construct in (SUEWSConfig.from_dict, SUEWSConfig.model_validate):
            with pytest.raises(ValueError, match=rf"{veg}\.wu"):
                construct(deepcopy(raw))
        with pytest.raises(ValueError, match=rf"{veg}\.wu"):
            veg_cls(wu=deepcopy(value))
        resolved = short_sample.resolved()
        resolved["sites"][0]["initial_states"][veg]["wu"] = deepcopy(value)
        with pytest.raises(RuntimeError, match=rf"{veg}\.wu"):
            _check_rust_available().run_suews(
                yaml.safe_dump(resolved), short_sample.forcing(), 2
            )
    path = tmp_path / f"explicit_{veg}_wu.yml"
    raw = short_sample.raw()
    raw["sites"][0]["initial_states"][veg]["wu"] = zero_wu
    path.write_text(yaml.safe_dump(raw), encoding="utf-8")
    with pytest.raises(ValueError, match=rf"{veg}\.wu"):
        SUEWSSimulation(path)


def test_legacy_dev13_continuation_state_migration_and_refusal(
    short_sample, tmp_path, capsys
):
    raw = short_sample.raw()
    raw["schema_version"] = "2026.6.dev13"
    states = raw["sites"][0]["initial_states"]
    states["dt_since_start"] = 0.0
    states["tair_av"] = 273.15
    states["tstep_prev"] = raw["model"]["control"]["tstep"]
    for name in ("l_mod", "ustar", "ra_h", "rb", "rs"):
        states[name] = 0.0
    for veg in ("evetr", "dectr", "grass"):
        states[veg]["wu"] = {
            "wu_total": {"value": 0.0},
            "wu_auto": {"value": 0.0},
            "wu_manual": {"value": 0.0},
        }
    config = SUEWSConfig.from_dict(deepcopy(raw))
    notice = capsys.readouterr().err
    assert "sites[0].initial_states.evetr.wu" in notice
    assert "sites[0].initial_states.tair_av" in notice
    assert "sites[0].initial_states.tstep_prev" in notice
    dumped = config.sites[0].initial_states.model_dump()
    for name in (
        "dt_since_start",
        "tair_av",
        "tstep_prev",
        "l_mod",
        "ustar",
        "ra_h",
        "rb",
        "rs",
    ):
        assert name not in dumped
    for veg in ("evetr", "dectr", "grass"):
        assert "wu" not in dumped[veg]

    src = tmp_path / "legacy_dev13.yml"
    dst = tmp_path / "current_dev14.yml"
    src.write_text(yaml.safe_dump(raw), encoding="utf-8")
    upgrade_yaml(src, dst)
    migrated = yaml.safe_load(dst.read_text(encoding="utf-8"))
    assert migrated["schema_version"] == CURRENT_SCHEMA_VERSION
    assert "tstep_prev" not in migrated["sites"][0]["initial_states"]
    assert "wu" not in migrated["sites"][0]["initial_states"]["evetr"]

    # Non-placeholder legacy values must be refused rather than silently dropped,
    # including oversized integers that exceed float range.
    bad_cases = [
        ("dt_since_start", 300.0),
        ("tair_av", 20.0),
        ("tair_av", 10**400),
        ("tstep_prev", 600.0),
        ("tstep_prev", 10**400),
        ("l_mod", -50.0),
        ("ustar", 0.4),
        ("ra_h", 30.0),
        ("rb", 10.0),
        ("rs", 100.0),
    ]
    for name, bad_value in bad_cases:
        bad = deepcopy(raw)
        bad["sites"][0]["initial_states"][name] = bad_value
        with pytest.raises(YamlUpgradeError, match="complete checkpoint"):
            SUEWSConfig.from_dict(bad)

    bad_tstep_overflow = deepcopy(raw)
    bad_tstep_overflow["model"]["control"]["tstep"] = 10**400
    bad_tstep_overflow["sites"][0]["initial_states"]["tstep_prev"] = 300.0
    with pytest.raises(YamlUpgradeError, match="complete checkpoint"):
        SUEWSConfig.from_dict(bad_tstep_overflow)

    bad_wu = deepcopy(raw)
    bad_wu["sites"][0]["initial_states"]["grass"]["wu"]["wu_auto"] = {"value": 1.5}
    with pytest.raises(YamlUpgradeError, match="complete checkpoint"):
        SUEWSConfig.from_dict(bad_wu)

    # A signed dev13 file must still reject radiation memory even when all zeros.
    bad_rad = deepcopy(raw)
    bad_rad["sites"][0]["initial_states"]["qn_surfs"] = [0.0] * 7
    with pytest.raises(ValueError, match="qn_surfs"):
        SUEWSConfig.from_dict(bad_rad)


def test_legacy_dataframe_accepts_cold_start_placeholders_and_rejects_continuation_state(
    short_sample,
):
    frame = short_sample.df_state()
    grid = frame.index[0]
    # Simulate _load.py legacy DFState cold-start placeholders.
    frame.loc[grid, ("tair_av", "0")] = 273.15
    frame.loc[grid, ("tstep_prev", "0")] = frame.loc[grid, ("tstep", "0")]
    restored = InitialStates.from_df_state(frame, grid)
    dumped = restored.model_dump()
    assert "dt_since_start" not in dumped
    assert "tair_av" not in dumped
    assert "tstep_prev" not in dumped
    assert "wu" not in dumped["evetr"]

    for col, bad_value in [
        (("dt_since_start", "0"), 300.0),
        (("tair_av", "0"), 18.5),
        (("tstep_prev", "0"), 900.0),
        (("wuday_id", "(4,)"), 2.0),
        (("dt_since_start", "0"), float("nan")),
    ]:
        bad_frame = frame.copy()
        bad_frame.loc[grid, col] = bad_value
        with pytest.raises(ValueError, match="complete checkpoint"):
            InitialStates.from_df_state(bad_frame, grid)

    # Boolean columns (numpy.bool_) and string/object cells must not pass as zeros.
    for col, bad_series in [
        (("dt_since_start", "0"), pd.Series([False], index=frame.index, dtype=bool)),
        (("tair_av", "0"), pd.Series(["0"], index=frame.index, dtype=object)),
        (("tstep_prev", "0"), pd.Series([10**400], index=frame.index, dtype=object)),
        (("wuday_id", "(0,)"), pd.Series([False], index=frame.index, dtype=bool)),
    ]:
        bad_frame = frame.copy()
        bad_frame[col] = bad_series
        with pytest.raises(ValueError, match="complete checkpoint"):
            InitialStates.from_df_state(bad_frame, grid)
