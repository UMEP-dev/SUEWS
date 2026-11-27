"""Integer-only day-of-year (DOY) validation for irrigation and daylight saving fields.

Covers gh#2101 (linked to gh#2001 and gh#2002): ``ie_start``, ``ie_end``,
``startdls``, and ``enddls`` represent integer days of year in ``1..366``.
Fractional floats (such as ``20.5`` or ``100.1``), booleans, non-finite
floats (``NaN``/``Inf``), out-of-range integers (``0``, ``367``), and
malformed strings must be rejected across:

1. Direct Pydantic models (``IrrigationParams``, ``AnthropogenicEmissions``)
   and ``to_df_state`` / ``from_df_state`` round-trips.
2. Public configuration loading and updates (``SUEWSConfig.from_yaml``,
   ``SUEWSConfig(**mapping)``, ``SUEWSSimulation.update_config``,
   ``SUEWSConfig.validate_for_engine``).
3. Validation pipeline Phase B (both ``phase_b`` and ``phase_b_rules.other_rules``)
   and Phase C (``collect_phase_c_issues``).
4. Schema upgrader (``upgrade_yaml``), which normalises legacy whole-number
   floats (``86.0 -> 86``) and rejects fractional/invalid DOYs with a clear
   migration hint before writing output.
5. Field metadata descriptions and exported JSON Schema
   (``docs/source/inputs/tables/schema.json``).
"""

from __future__ import annotations

import copy
from importlib import import_module
from importlib.resources import as_file, files
import json
import os
from pathlib import Path
import subprocess
from types import SimpleNamespace
import warnings

from click.testing import CliRunner
import numpy as np
from pydantic import ValidationError
import pytest
import yaml

from supy import SUEWSSimulation
from supy.cmd.suews_cli import cli as suews_cli
from supy.cmd.validate_config import cli as validate_cli
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.human_activity import (
    AnthropogenicEmissions,
    IrrigationParams,
)
from supy.data_model.core.type import RefValue
from supy.data_model.validation.pipeline import phase_b
from supy.data_model.validation.pipeline.phase_b_rules import (
    other_rules as phase_b_other_rules,
)
from supy.data_model.validation.pipeline.phase_c import collect_phase_c_issues
from supy.util.converter.yaml_upgrade import YamlUpgradeError, upgrade_yaml

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-MIGRATE", "C-ROUNDTRIP", "C-SCHEMA"),
]

PROJECT_ROOT = Path(__file__).resolve().parents[2]
SAMPLE_CONFIG_PATH = PROJECT_ROOT / "src" / "supy" / "sample_data" / "sample_config.yml"


@pytest.fixture
def sample_config_dict() -> dict:
    with SAMPLE_CONFIG_PATH.open("r", encoding="utf-8") as fh:
        return yaml.safe_load(fh)


# ---------------------------------------------------------------------------
# 1. Direct Pydantic models: valid integers, whole-number floats, rejections
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("raw_val", "expected"),
    [
        (1, 1),
        (91, 91),
        (250, 250),
        (366, 366),
        (1.0, 1),
        (91.0, 91),
        (366.0, 366),
        (np.int64(250), 250),
        (np.float64(91.0), 91),
        ("91", 91),
    ],
)
def test_valid_integer_and_whole_float_doys_accepted_and_normalised(
    raw_val, expected: int
) -> None:
    irr_bare = IrrigationParams(ie_start=raw_val, ie_end=raw_val)
    assert irr_bare.ie_start == expected
    assert type(irr_bare.ie_start) is int
    assert irr_bare.ie_end == expected
    assert type(irr_bare.ie_end) is int

    irr_wrapped = IrrigationParams(
        ie_start={"value": raw_val},
        ie_end=RefValue(raw_val),
    )
    assert isinstance(irr_wrapped.ie_start, RefValue)
    assert irr_wrapped.ie_start.value == expected
    assert type(irr_wrapped.ie_start.value) is int
    assert isinstance(irr_wrapped.ie_end, RefValue)
    assert irr_wrapped.ie_end.value == expected
    assert type(irr_wrapped.ie_end.value) is int

    anthro = AnthropogenicEmissions(
        startdls={"value": raw_val},
        enddls=RefValue(raw_val),
    )
    assert anthro.startdls.value == expected
    assert type(anthro.startdls.value) is int
    assert anthro.enddls.value == expected
    assert type(anthro.enddls.value) is int

    # Dumping to JSON must emit plain ints with zero Pydantic serialization warnings.
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        dumped_irr = irr_wrapped.model_dump(mode="json")
        dumped_anthro = anthro.model_dump(mode="json")
    assert dumped_irr["ie_start"]["value"] == expected
    assert type(dumped_irr["ie_start"]["value"]) is int
    assert dumped_anthro["startdls"]["value"] == expected
    assert type(dumped_anthro["startdls"]["value"]) is int
    ser_warnings = [w for w in caught if "PydanticSerialization" in str(w.message)]
    assert not ser_warnings


@pytest.mark.parametrize(
    "bad_val",
    [
        20.5,
        100.1,
        86.5,
        303.75,
        np.float64(20.5),
        True,
        False,
        np.bool_(True),
        float("nan"),
        float("inf"),
        float("-inf"),
        0,
        0.0,
        -1,
        367,
        367.0,
        999,
        "20.5",
        "100.1",
        "abc",
        "",
        "+91",
    ],
)
@pytest.mark.parametrize(
    ("model_cls", "field_name"),
    [
        (IrrigationParams, "ie_start"),
        (IrrigationParams, "ie_end"),
        (AnthropogenicEmissions, "startdls"),
        (AnthropogenicEmissions, "enddls"),
    ],
)
def test_invalid_doys_rejected_bare_and_wrapped(
    model_cls, field_name: str, bad_val
) -> None:
    with pytest.raises(ValidationError):
        model_cls(**{field_name: bad_val})

    with pytest.raises(ValidationError):
        model_cls(**{field_name: {"value": bad_val}})


def test_fractional_doy_error_message_includes_integer_hint() -> None:
    for model_cls, field_name in (
        (IrrigationParams, "ie_start"),
        (IrrigationParams, "ie_end"),
        (AnthropogenicEmissions, "startdls"),
        (AnthropogenicEmissions, "enddls"),
    ):
        with pytest.raises(ValidationError) as exc_info:
            model_cls(**{field_name: 20.5})
        msg = str(exc_info.value)
        assert "integer day of year" in msg.lower()
        assert "1..366" in msg or "1 and 366" in msg


def test_df_state_roundtrip_for_irrigation_and_dls() -> None:
    # Disabled (None) round-trips cleanly via 0 sentinel -> None.
    irr_none = IrrigationParams(ie_start=None, ie_end=None)
    df_irr_none = irr_none.to_df_state(grid_id=1)
    assert df_irr_none.loc[1, ("ie_start", "0")] == 0
    assert df_irr_none.loc[1, ("ie_end", "0")] == 0
    restored_irr_none = IrrigationParams.from_df_state(df_irr_none, grid_id=1)
    assert restored_irr_none.ie_start is None
    assert restored_irr_none.ie_end is None

    # Active integer DOYs round-trip cleanly even when DataFrame stores float64.
    irr_active = IrrigationParams(ie_start=91, ie_end=250)
    df_irr = irr_active.to_df_state(grid_id=1)
    df_irr["ie_start", "0"] = df_irr["ie_start", "0"].astype(float)
    df_irr["ie_end", "0"] = df_irr["ie_end", "0"].astype(float)
    restored_irr = IrrigationParams.from_df_state(df_irr, grid_id=1)
    assert restored_irr.ie_start.value == 91
    assert type(restored_irr.ie_start.value) is int
    assert restored_irr.ie_end.value == 250
    assert type(restored_irr.ie_end.value) is int

    # Fractional DOY injected into df_state is rejected on reconstruction.
    df_irr_bad = df_irr.copy()
    df_irr_bad.loc[1, ("ie_start", "0")] = 20.5
    with pytest.raises(ValidationError, match="integer day of year"):
        IrrigationParams.from_df_state(df_irr_bad, grid_id=1)

    anthro = AnthropogenicEmissions(startdls=86, enddls=303)
    df_anthro = anthro.to_df_state(grid_id=1)
    df_anthro_bad = df_anthro.copy()
    df_anthro_bad["startdls", "0"] = df_anthro_bad["startdls", "0"].astype(float)
    df_anthro_bad.loc[1, ("startdls", "0")] = 86.5
    with pytest.raises(ValidationError, match="integer day of year"):
        AnthropogenicEmissions.from_df_state(df_anthro_bad, grid_id=1)


# ---------------------------------------------------------------------------
# 2. Public config loading, update_config, and live-mutation validate_for_engine
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("section", "field_name", "bad_val"),
    [
        ("irrigation", "ie_start", 20.5),
        ("irrigation", "ie_end", 100.1),
        ("irrigation", "ie_start", 0),
        ("irrigation", "ie_end", 367),
        ("anthropogenic_emissions", "startdls", 20.5),
        ("anthropogenic_emissions", "enddls", 303.75),
        ("anthropogenic_emissions", "startdls", 0),
        ("anthropogenic_emissions", "enddls", 367),
    ],
)
def test_suews_config_from_yaml_and_update_config_reject_invalid_doys(
    sample_config_dict: dict,
    tmp_path: Path,
    section: str,
    field_name: str,
    bad_val,
) -> None:
    bad_cfg = copy.deepcopy(sample_config_dict)
    if section == "irrigation":
        bad_cfg["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
    bad_cfg["sites"][0]["properties"][section][field_name] = {"value": bad_val}

    # Direct mapping construction (deepcopy so in-place enum coercion does not leak)
    with pytest.raises(ValidationError):
        SUEWSConfig(**copy.deepcopy(bad_cfg))

    # SUEWSConfig.from_yaml
    yaml_path = tmp_path / f"bad_{field_name}.yml"
    yaml_path.write_text(yaml.safe_dump(bad_cfg, sort_keys=False), encoding="utf-8")
    with pytest.raises(ValueError):
        SUEWSConfig.from_yaml(yaml_path)

    # SUEWSSimulation.update_config
    sim = SUEWSSimulation.from_sample_data()
    with pytest.raises((ValueError, ValidationError)):
        sim.update_config(copy.deepcopy(bad_cfg))


def test_refvalue_live_assignment_rejects_booleans_fractionals_and_out_of_range(
    sample_config_dict: dict,
) -> None:
    active_dict = copy.deepcopy(sample_config_dict)
    active_dict["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
    cfg = SUEWSConfig(**active_dict)
    irr = cfg.sites[0].properties.irrigation
    anthro = cfg.sites[0].properties.anthropogenic_emissions

    # Whole-number floats coerce cleanly to int on .value assignment.
    irr.ie_start.value = 1.0
    assert irr.ie_start.value == 1
    assert type(irr.ie_start.value) is int
    anthro.startdls.value = 86.0
    assert anthro.startdls.value == 86
    assert type(anthro.startdls.value) is int

    # Booleans, fractional floats, and out-of-range integers fail immediately on .value = ...
    with pytest.raises(ValueError, match="boolean"):
        irr.ie_start.value = True
    with pytest.raises(ValueError, match="boolean"):
        irr.ie_start.value = np.bool_(True)
    with pytest.raises(ValueError, match="boolean"):
        anthro.startdls.value = False
    with pytest.raises(ValueError, match="integer day of year"):
        irr.ie_end.value = 20.5
    with pytest.raises(ValueError, match="between 1 and 366"):
        anthro.enddls.value = 367
    with pytest.raises(ValueError, match="between 1 and 366"):
        irr.ie_start.value = 0

    # Replacing the RefValue wrapper preserves the bound assignment guard.
    irr.ie_start = RefValue(91.0)
    assert irr.ie_start.value == 91
    assert type(irr.ie_start.value) is int
    with pytest.raises(ValueError, match="boolean"):
        irr.ie_start.value = True
    with pytest.raises(ValueError, match="integer day of year"):
        irr.ie_end = 20.5  # type: ignore[assignment]

    # Deep-copied configs preserve the bound RefValue assignment guard.
    cfg_copy = copy.deepcopy(cfg)
    with pytest.raises(ValueError, match="boolean"):
        cfg_copy.sites[0].properties.irrigation.ie_start.value = True


def test_validate_for_engine_catches_live_mutated_invalid_doys(
    sample_config_dict: dict,
) -> None:
    active_dict = copy.deepcopy(sample_config_dict)
    active_dict["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
    cfg = SUEWSConfig(**copy.deepcopy(active_dict))
    cfg.sites[0].properties.irrigation.ie_start.__dict__["value"] = 0
    with pytest.raises(ValueError, match=r"irrigation\.ie_start"):
        cfg.validate_for_engine()

    cfg2 = SUEWSConfig(**copy.deepcopy(sample_config_dict))
    cfg2.sites[0].properties.anthropogenic_emissions.enddls.__dict__["value"] = 367
    with pytest.raises(ValueError, match=r"anthropogenic_emissions\.enddls"):
        cfg2.validate_for_engine()

    cfg3 = SUEWSConfig(**copy.deepcopy(active_dict))
    cfg3.sites[0].properties.irrigation.__dict__["ie_end"] = 20.5
    with pytest.raises(ValueError, match="integer day of year"):
        cfg3.validate_for_engine()

    cfg4 = SUEWSConfig(**copy.deepcopy(active_dict))
    cfg4.sites[0].properties.irrigation.ie_start.__dict__["value"] = True
    with pytest.raises(ValueError, match="boolean"):
        cfg4.validate_for_engine()


# ---------------------------------------------------------------------------
# 3. Validation pipeline Phase B & Phase C
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "validate_fn",
    [phase_b.validate_irrigation_doy, phase_b_other_rules.validate_irrigation_doy],
)
@pytest.mark.parametrize(
    "bad_val", [20.5, 100.1, True, False, float("nan"), "20.5", "abc"]
)
def test_phase_b_validate_irrigation_doy_rejects_non_integer(
    validate_fn, bad_val
) -> None:
    results = validate_fn(bad_val, 250, 51.5, 2023, "test_site")
    errors = [r for r in results if r.status == "ERROR"]
    assert errors
    assert any("integer day of year" in r.message.lower() for r in errors)


@pytest.mark.parametrize(
    ("start_val", "end_val", "bad_field", "expected_phrase"),
    [
        (86.5, 303, "startdls", "integer day of year"),
        (0, 303, "startdls", "between 1 and 366"),
        (86, 367, "enddls", "between 1 and 366"),
    ],
)
def test_phase_b_geography_rejects_fractional_and_out_of_range_dls(
    start_val, end_val, bad_field: str, expected_phrase: str
) -> None:
    yaml_data = {
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "lat": {"value": 51.5},
                    "lng": {"value": -0.12},
                    "timezone": {"value": 0},
                    "anthropogenic_emissions": {
                        "startdls": {"value": start_val},
                        "enddls": {"value": end_val},
                    },
                },
            }
        ]
    }
    ctx = SimpleNamespace(yaml_data=yaml_data)
    res_b = phase_b.validate_geographic_parameters(yaml_data)
    res_rule = phase_b_other_rules.validate_geographic_parameters(ctx)
    for res in (res_b, res_rule):
        errors = [r for r in res if r.status == "ERROR"]
        passes = [r for r in res if r.status == "PASS"]
        assert not passes
        assert any(
            r.parameter == f"anthropogenic_emissions.{bad_field}"
            and expected_phrase in r.message.lower()
            for r in errors
        )


def test_phase_c_reports_fractional_doy_issue(
    sample_config_dict: dict, tmp_path: Path
) -> None:
    bad_cfg = copy.deepcopy(sample_config_dict)
    bad_cfg["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
    bad_cfg["sites"][0]["properties"]["irrigation"]["ie_start"] = {"value": 20.5}
    yaml_path = tmp_path / "phase_c_bad_doy.yml"
    yaml_path.write_text(yaml.safe_dump(bad_cfg, sort_keys=False), encoding="utf-8")

    issues = collect_phase_c_issues(str(yaml_path))
    assert issues
    assert any(
        "ie_start" in (issue.yaml_path or "")
        and "integer day of year" in issue.message.lower()
        for issue in issues
    )


# ---------------------------------------------------------------------------
# 4. Converter / upgrader hints & whole-float normalisation
# ---------------------------------------------------------------------------


def test_yaml_upgrade_normalises_whole_float_doys_and_rejects_fractional(
    sample_config_dict: dict, tmp_path: Path
) -> None:
    in_ok = tmp_path / "legacy_whole_float.yml"
    out_ok = tmp_path / "upgraded_ok.yml"
    in_ok.write_text(
        yaml.safe_dump(sample_config_dict, sort_keys=False), encoding="utf-8"
    )

    upgrade_yaml(in_ok, out_ok)
    upgraded = yaml.safe_load(out_ok.read_text(encoding="utf-8"))
    props = upgraded["sites"][0]["properties"]
    assert props["irrigation"]["ie_start"]["value"] == 1
    assert type(props["irrigation"]["ie_start"]["value"]) is int
    assert props["irrigation"]["ie_end"]["value"] == 366
    assert type(props["irrigation"]["ie_end"]["value"]) is int
    assert props["anthropogenic_emissions"]["startdls"]["value"] == 86
    assert type(props["anthropogenic_emissions"]["startdls"]["value"]) is int
    assert props["anthropogenic_emissions"]["enddls"]["value"] == 303
    assert type(props["anthropogenic_emissions"]["enddls"]["value"]) is int

    # Fractional DOY fails loudly with migration hint and writes no output file.
    bad_cfg = copy.deepcopy(sample_config_dict)
    bad_cfg["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = {
        "value": 20.5
    }
    in_bad = tmp_path / "legacy_fractional.yml"
    out_bad = tmp_path / "upgraded_bad.yml"
    in_bad.write_text(yaml.safe_dump(bad_cfg, sort_keys=False), encoding="utf-8")

    with pytest.raises(YamlUpgradeError) as exc_info:
        upgrade_yaml(in_bad, out_bad)
    msg = str(exc_info.value)
    assert "startdls" in msg
    assert "integer day of year" in msg.lower()
    assert not out_bad.exists()


def test_yaml_upgrade_keeps_reference_only_doy_mappings_disabled(
    sample_config_dict: dict, tmp_path: Path
) -> None:
    """A ref-only DOY mapping is a disabled window, as the Rust loader reads it."""
    cfg = copy.deepcopy(sample_config_dict)
    props = cfg["sites"][0]["properties"]
    disabled = {"ref": {"desc": "disabled"}}
    props["irrigation"]["ie_start"] = copy.deepcopy(disabled)
    props["irrigation"]["ie_end"] = copy.deepcopy(disabled)
    props["anthropogenic_emissions"]["startdls"] = copy.deepcopy(disabled)
    props["anthropogenic_emissions"]["enddls"] = copy.deepcopy(disabled)
    path_in = tmp_path / "legacy_ref_only.yml"
    path_out = tmp_path / "upgraded_ref_only.yml"
    path_in.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")

    upgrade_yaml(path_in, path_out)

    upgraded = yaml.safe_load(path_out.read_text(encoding="utf-8"))["sites"][0][
        "properties"
    ]
    assert upgraded["irrigation"]["ie_start"] == disabled
    assert upgraded["irrigation"]["ie_end"] == disabled
    assert upgraded["anthropogenic_emissions"]["startdls"] == disabled
    assert upgraded["anthropogenic_emissions"]["enddls"] == disabled


# ---------------------------------------------------------------------------
# 4b. Public entry points: SUEWSSimulation.run(), suews validate, suews convert
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("section", "field"),
    [
        ("irrigation", "ie_start"),
        ("irrigation", "ie_end"),
        ("anthropogenic_emissions", "startdls"),
        ("anthropogenic_emissions", "enddls"),
    ],
)
@pytest.mark.parametrize(
    "bad_val", [0, 367, 20.5, True], ids=["zero", "367", "fractional", "bool"]
)
def test_simulation_run_rejects_live_mutated_invalid_doy(
    section, field, bad_val
) -> None:
    """``SUEWSSimulation.run()`` refuses a DOY mutated past field validation."""
    sim = SUEWSSimulation.from_sample_data()
    if section == "irrigation":
        sim.config.sites[0].properties.irrigation.daywat.monday = 1.0
    forcing = sim.forcing.df.iloc[:12]
    sim.update_forcing(forcing)
    ref_value = getattr(getattr(sim.config.sites[0].properties, section), field)
    ref_value.__dict__["value"] = bad_val

    with pytest.raises(ValueError, match=rf"{section}\.{field}"):
        sim.run(start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1)
    assert sim._df_output is None


def _write_config_with_doy(
    sample_config_dict: dict, tmp_path: Path, section: str, field: str, value
) -> Path:
    cfg = copy.deepcopy(sample_config_dict)
    if section == "irrigation":
        cfg["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
    cfg["sites"][0]["properties"][section][field] = {"value": value}
    path = tmp_path / f"{section}_{field}.yml"
    path.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")
    return path


@pytest.mark.parametrize(
    ("section", "field", "bad_val"),
    [
        ("irrigation", "ie_start", 20.5),
        ("anthropogenic_emissions", "enddls", 367),
    ],
)
def test_suews_validate_cli_rejects_invalid_doy(
    sample_config_dict: dict, tmp_path: Path, section, field, bad_val
) -> None:
    """``suews validate`` exits non-zero and names the offending DOY field."""
    path = _write_config_with_doy(sample_config_dict, tmp_path, section, field, bad_val)
    result = CliRunner().invoke(
        validate_cli,
        ["--pipeline", "ABC", "--dry-run", "--format", "json", str(path)],
    )

    assert result.exit_code != 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert field in json.dumps(envelope["errors"]), envelope["errors"]


@pytest.mark.parametrize(
    ("section", "field", "bad_val", "wrapped"),
    [
        ("irrigation", "ie_start", 20.5, True),
        ("irrigation", "ie_start", 0, False),
        ("irrigation", "ie_start", 0, True),
        ("irrigation", "ie_end", 0, False),
        ("anthropogenic_emissions", "startdls", 20.5, True),
        ("anthropogenic_emissions", "enddls", 367, True),
        ("anthropogenic_emissions", "startdls", {"unexpected": 1}, False),
    ],
)
def test_suews_validate_phase_b_cli_rejects_invalid_doy(
    sample_config_dict: dict, tmp_path: Path, section, field, bad_val, wrapped: bool
) -> None:
    """Non-dry-run ``suews validate --pipeline B`` rejects fractional/zero/out-of-range/bad DOYs."""
    cfg = copy.deepcopy(sample_config_dict)
    if section == "irrigation":
        cfg["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
    cfg["sites"][0]["properties"][section][field] = (
        {"value": bad_val} if wrapped else bad_val
    )
    if section == "irrigation" and bad_val == 0:
        other = "ie_end" if field == "ie_start" else "ie_start"
        cfg["sites"][0]["properties"]["irrigation"][other] = (
            {"value": 0} if wrapped else 0
        )
    path = tmp_path / f"phase_b_{section}_{field}_{int(wrapped)}.yml"
    path.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")

    result = CliRunner().invoke(
        validate_cli,
        ["--pipeline", "B", "--format", "json", str(path)],
    )
    assert result.exit_code != 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert field in json.dumps(envelope), envelope
    assert "0/None" not in json.dumps(envelope), envelope


@pytest.mark.parametrize(
    "disabled_mapping",
    [{"ref": {"desc": "disabled"}}, {}],
    ids=["ref-only", "empty-mapping"],
)
def test_suews_validate_phase_b_cli_accepts_disabled_dls_mapping(
    sample_config_dict: dict, tmp_path: Path, disabled_mapping: dict
) -> None:
    """Non-dry-run ``suews validate`` (B and ABC) and ``SUEWSConfig`` accept ref-only/empty DOYs."""
    cfg = copy.deepcopy(sample_config_dict)
    cfg["model"]["control"]["forcing"]["file"] = None
    anthro = cfg["sites"][0]["properties"]["anthropogenic_emissions"]
    anthro["startdls"] = copy.deepcopy(disabled_mapping)
    anthro["enddls"] = copy.deepcopy(disabled_mapping)
    irrigation = cfg["sites"][0]["properties"]["irrigation"]
    irrigation["ie_start"] = copy.deepcopy(disabled_mapping)
    irrigation["ie_end"] = copy.deepcopy(disabled_mapping)
    path = tmp_path / "phase_b_disabled_dls.yml"
    path.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")

    for pipeline in ("B", "ABC"):
        result = CliRunner().invoke(
            validate_cli,
            ["--pipeline", pipeline, "--format", "json", str(path)],
        )
        assert result.exit_code == 0, f"pipeline={pipeline}: {result.output}"
        envelope = json.loads(result.stdout)
        assert envelope["status"] != "error", envelope
        assert envelope["data"]["phases_run"] == list(pipeline), envelope

    loaded = SUEWSConfig.from_yaml(str(path))
    props = loaded.sites[0].properties
    for current in (
        props.irrigation.ie_start,
        props.irrigation.ie_end,
        props.anthropogenic_emissions.startdls,
        props.anthropogenic_emissions.enddls,
    ):
        if disabled_mapping:
            assert isinstance(current, RefValue)
            assert current.value is None
            assert current.ref is not None and current.ref.desc == "disabled"
        else:
            assert current is None

    # Live assignment accepts {} / ref-only mappings with preserved ref metadata.
    props.irrigation.ie_start = copy.deepcopy(disabled_mapping)
    props.anthropogenic_emissions.startdls = copy.deepcopy(disabled_mapping)
    if disabled_mapping:
        assert isinstance(props.irrigation.ie_start, RefValue)
        assert props.irrigation.ie_start.value is None
        assert props.irrigation.ie_start.ref.desc == "disabled"
        assert isinstance(props.anthropogenic_emissions.startdls, RefValue)
        assert props.anthropogenic_emissions.startdls.value is None
        assert props.anthropogenic_emissions.startdls.ref.desc == "disabled"
    else:
        assert props.irrigation.ie_start is None
        assert props.anthropogenic_emissions.startdls is None


_OMIT_DOY = object()


@pytest.mark.parametrize(
    ("start_entry", "end_entry"),
    [
        (None, None),
        ({}, {}),
        ({"ref": {"desc": "disabled"}}, {"ref": {"desc": "disabled"}}),
        (
            {"value": None, "ref": {"desc": "disabled"}},
            {"value": None, "ref": {"desc": "disabled"}},
        ),
        (_OMIT_DOY, _OMIT_DOY),
        (None, _OMIT_DOY),
        ({"ref": {"desc": "disabled"}}, _OMIT_DOY),
        (_OMIT_DOY, {}),
    ],
    ids=[
        "bare-null",
        "empty-mapping",
        "ref-only",
        "null-with-ref",
        "both-omitted",
        "null-and-omitted",
        "ref-only-and-omitted",
        "omitted-and-empty",
    ],
)
def test_suews_validate_science_fixes_apply_preserves_explicitly_disabled_dls(
    sample_config_dict: dict, tmp_path: Path, start_entry: object, end_entry: object
) -> None:
    """``suews validate --science-fixes apply`` preserves disabled DLS across all absent forms."""
    cfg = copy.deepcopy(sample_config_dict)
    cfg["model"]["control"]["forcing"]["file"] = None
    anthro = cfg["sites"][0]["properties"]["anthropogenic_emissions"]
    if start_entry is _OMIT_DOY:
        anthro.pop("startdls", None)
    else:
        anthro["startdls"] = copy.deepcopy(start_entry)
    if end_entry is _OMIT_DOY:
        anthro.pop("enddls", None)
    else:
        anthro["enddls"] = copy.deepcopy(end_entry)
    path = tmp_path / "science_fixes_disabled_dls.yml"
    path.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")

    for pipeline in ("B", "ABC"):
        result = CliRunner().invoke(
            validate_cli,
            [
                "--pipeline",
                pipeline,
                "--science-fixes",
                "apply",
                "--format",
                "json",
                str(path),
            ],
        )
        assert result.exit_code == 0, f"pipeline={pipeline}: {result.output}"
        envelope = json.loads(result.stdout)
        assert envelope["status"] != "error", envelope
        assert envelope["data"]["phases_run"] == list(pipeline), envelope
        # Disabled DLS pairs must not emit a misleading auto-calculation warning.
        assert "will be calculated automatically" not in json.dumps(envelope), envelope

        updated_path = Path(envelope["data"]["updated_yaml"])
        assert updated_path.is_file(), envelope
        emitted = yaml.safe_load(updated_path.read_text(encoding="utf-8"))
        emitted_anthro = emitted["sites"][0]["properties"]["anthropogenic_emissions"]
        for key, expected in (("startdls", start_entry), ("enddls", end_entry)):
            if expected is _OMIT_DOY:
                if pipeline == "B":
                    assert key not in emitted_anthro
                else:
                    assert emitted_anthro.get(key) in (None, {"value": None})
            else:
                assert emitted_anthro[key] == expected

        loaded = SUEWSConfig.from_yaml(str(updated_path))
        loaded_anthro = loaded.sites[0].properties.anthropogenic_emissions
        for attr, expected in (("startdls", start_entry), ("enddls", end_entry)):
            current = getattr(loaded_anthro, attr)
            if isinstance(expected, dict) and "ref" in expected:
                assert isinstance(current, RefValue)
                assert current.value is None
                assert current.ref is not None
                assert current.ref.desc == "disabled"
            elif expected is _OMIT_DOY and pipeline == "ABC":
                assert (
                    current.value if isinstance(current, RefValue) else current
                ) is None
            else:
                assert current is None


def test_cli_nonleap_2023_rejects_dls_366_under_science_fixes_apply(
    sample_config_dict: dict, tmp_path: Path
) -> None:
    """Non-leap-year (2023) startdls=366 is rejected in Phase B / --science-fixes apply."""
    cfg_2023 = copy.deepcopy(sample_config_dict)
    cfg_2023["model"]["control"]["start_time"] = "2023-01-01"
    cfg_2023["model"]["control"]["end_time"] = "2023-12-31"
    cfg_2023["model"]["control"]["forcing"]["file"] = None
    cfg_2023["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 366
    cfg_2023["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = 303
    path_2023 = tmp_path / "nonleap_2023_dls366.yml"
    path_2023.write_text(yaml.safe_dump(cfg_2023, sort_keys=False), encoding="utf-8")

    res_2023 = CliRunner().invoke(
        validate_cli,
        ["--pipeline", "ABC", "--science-fixes", "apply", "--format", "json", str(path_2023)],
    )
    assert res_2023.exit_code != 0, res_2023.output
    assert "365" in res_2023.output and "startdls" in res_2023.output


def test_suews_convert_cli_rejects_fractional_doy_and_accepts_ref_only(
    sample_config_dict: dict, tmp_path: Path
) -> None:
    """``suews convert`` fails on a fractional DOY and passes a ref-only one that loads."""
    runner = CliRunner()
    bad = _write_config_with_doy(
        sample_config_dict, tmp_path, "anthropogenic_emissions", "startdls", 20.5
    )
    bad_out = tmp_path / "converted_bad.yml"
    result = runner.invoke(suews_cli, ["convert", "-i", str(bad), "-o", str(bad_out)])
    assert result.exit_code != 0, result.output
    assert "startdls" in result.output
    assert "integer day of year" in result.output.lower()
    assert not bad_out.exists()

    cfg = copy.deepcopy(sample_config_dict)
    cfg["sites"][0]["properties"]["irrigation"]["ie_start"] = {
        "ref": {"desc": "disabled"}
    }
    cfg["sites"][0]["properties"]["irrigation"]["ie_end"] = {}
    cfg["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = {
        "ref": {"desc": "disabled"}
    }
    cfg["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = {}
    ok = tmp_path / "ref_only.yml"
    ok.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")
    ok_out = tmp_path / "converted_ok.yml"
    result = runner.invoke(suews_cli, ["convert", "-i", str(ok), "-o", str(ok_out)])
    assert result.exit_code == 0, result.output
    converted = yaml.safe_load(ok_out.read_text(encoding="utf-8"))
    assert converted["sites"][0]["properties"]["irrigation"]["ie_start"] == {
        "ref": {"desc": "disabled"}
    }
    reloaded = SUEWSConfig.from_yaml(str(ok_out))
    assert reloaded.sites[0].properties.irrigation.ie_start.value is None
    assert reloaded.sites[0].properties.irrigation.ie_start.ref.desc == "disabled"
    assert reloaded.sites[0].properties.irrigation.ie_end is None
    assert reloaded.sites[0].properties.anthropogenic_emissions.startdls.value is None
    assert reloaded.sites[0].properties.anthropogenic_emissions.enddls is None


def _locate_suews_engine() -> Path:
    """Return the bundled or locally built ``suews-engine`` binary."""
    binary_name = "suews-engine.exe" if os.name == "nt" else "suews-engine"
    bundled = files("supy").joinpath("bin").joinpath(binary_name)
    with as_file(bundled) as bundled_path:
        if bundled_path.exists():
            return Path(bundled_path)
    repo_root = Path(__file__).resolve().parents[2]
    dev_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / binary_name
    if dev_binary.exists():
        return dev_binary
    pytest.skip("Rust CLI binary not found; build with: make bridge")


def _run_suews_engine(run_dir: Path, mutate_config=None, mutate_resolved=None):
    """Run ``suews-engine run`` on the sample case cut to four forcing rows.

    ``mutate_config`` edits the YAML mapping before ``SUEWSConfig`` validates
    it; ``mutate_resolved`` edits the resolved engine YAML afterwards, for
    values the data model would refuse.
    """
    sample_dir = files("supy").joinpath("sample_data")
    cfg = yaml.safe_load(
        sample_dir.joinpath("sample_config.yml").read_text(encoding="utf-8")
    )
    control = cfg["model"]["control"]
    control["output"]["dir"] = str(run_dir)
    forcing_entry = control["forcing"]["file"]
    forcing_name = (
        forcing_entry["value"] if isinstance(forcing_entry, dict) else forcing_entry
    )
    if mutate_config is not None:
        mutate_config(cfg)
    run_rust = import_module("supy._run_rust")
    resolved = yaml.safe_load(
        run_rust.config_to_engine_yaml(SUEWSConfig.from_dict(cfg))
    )
    if mutate_resolved is not None:
        mutate_resolved(resolved)
    forcing_lines = [
        line
        for line in sample_dir
        .joinpath(forcing_name)
        .read_text(encoding="utf-8")
        .splitlines()
        if line.strip()
    ]
    (run_dir / forcing_name).write_text(
        "\n".join(forcing_lines[:5]) + "\n", encoding="utf-8"
    )
    config_path = run_dir / "engine_config.yml"
    config_path.write_text(yaml.safe_dump(resolved, sort_keys=False), encoding="utf-8")
    return subprocess.run(
        [str(_locate_suews_engine()), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
    )


@pytest.mark.parametrize(
    ("section", "field", "bad_val", "expected"),
    [
        ("irrigation", "ie_start", 20.5, "fractional part"),
        ("anthropogenic_emissions", "enddls", 367, "between 1 and 366"),
    ],
)
def test_suews_engine_cli_rejects_invalid_doy(
    tmp_path, section, field, bad_val, expected
):
    """Standalone ``suews-engine run`` refuses a DOY the data model never saw."""

    def set_doy(resolved):
        if section == "irrigation":
            resolved["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
        resolved["sites"][0]["properties"][section][field] = {"value": bad_val}

    result = _run_suews_engine(tmp_path, mutate_resolved=set_doy)

    assert result.returncode != 0, result.stderr
    assert f"{section}.{field}" in result.stderr, result.stderr
    assert expected in result.stderr, result.stderr
    assert not list(tmp_path.glob("*.arrow"))


# ---------------------------------------------------------------------------
# 5. Field metadata descriptions and committed JSON Schema
# ---------------------------------------------------------------------------


def test_doy_field_descriptions_and_schema_json_state_integer_day_of_year() -> None:
    from jsonschema import Draft202012Validator, ValidationError as JsonSchemaError

    for model_cls, field_name in (
        (IrrigationParams, "ie_start"),
        (IrrigationParams, "ie_end"),
        (AnthropogenicEmissions, "startdls"),
        (AnthropogenicEmissions, "enddls"),
    ):
        desc = (model_cls.model_fields[field_name].description or "").lower()
        assert "integer day of year" in desc
        assert "decimal day of year" not in desc

    schema_path = PROJECT_ROOT / "docs" / "source" / "inputs" / "tables" / "schema.json"
    schema = json.loads(schema_path.read_text(encoding="utf-8"))
    defs = schema["$defs"]
    for model_name, field_name in (
        ("IrrigationParams", "ie_start"),
        ("IrrigationParams", "ie_end"),
        ("AnthropogenicEmissions", "startdls"),
        ("AnthropogenicEmissions", "enddls"),
    ):
        prop = defs[model_name]["properties"][field_name]
        desc = prop["description"].lower()
        assert "integer day of year" in desc
        assert "decimal day of year" not in desc
        prop_json = json.dumps(prop)
        assert '"type": "integer"' in prop_json
        assert '"minimum": 1' in prop_json
        assert '"maximum": 366' in prop_json
        assert "RefValue_float_" not in prop_json

        validator = Draft202012Validator({"$defs": defs, **prop})
        for valid_val in (
            1,
            366,
            {"value": 1},
            {"value": 366, "ref": {"desc": "valid"}},
            None,
            {"value": None},
            {"value": None, "ref": {"desc": "disabled"}},
            {},
            {"ref": {"desc": "disabled"}},
        ):
            validator.validate(valid_val)
        for invalid_val in (
            0,
            -1,
            367,
            20.5,
            True,
            False,
            "86",
            {"value": 0},
            {"value": 367},
            {"value": 20.5},
            {"value": True},
            {"unexpected": 1},
            {"value": 86, "unexpected": 1},
        ):
            with pytest.raises(JsonSchemaError):
                validator.validate(invalid_val)


def test_schema_dev17_migration_and_historical_boundaries(
    sample_config_dict: dict, tmp_path: Path
) -> None:
    """Versioned dev16 -> dev17 migration enforces integer DOYs while historical targets preserve float DOYs."""
    from packaging.version import Version
    from supy.data_model.configuration import CURRENT_SCHEMA_VERSION
    from supy.data_model.configuration.migration import SchemaMigrator
    from supy.data_model.configuration.publisher import generate_json_schema
    from supy.util.converter.yaml_upgrade import SoilMigrationContext, YamlUpgradeError

    assert Version(CURRENT_SCHEMA_VERSION) >= Version("2026.6.dev17")
    migrator = SchemaMigrator()

    # 1. dev16 -> dev17 normalises whole-number floats and preserves ref metadata.
    whole = copy.deepcopy(sample_config_dict)
    whole["schema_version"] = "2026.6.dev16"
    whole["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = {
        "value": 86.0,
        "ref": {"desc": "spring"},
    }
    whole["sites"][0]["properties"]["irrigation"]["ie_start"] = 120.0
    whole["sites"][0]["properties"]["irrigation"]["ie_end"] = 250.0
    migrated = migrator.migrate(whole, to_version="2026.6.dev17")
    assert migrated["schema_version"] == "2026.6.dev17"
    assert migrated["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] == {
        "value": 86,
        "ref": {"desc": "spring"},
    }
    assert type(migrated["sites"][0]["properties"]["irrigation"]["ie_start"]) is int
    assert migrated["sites"][0]["properties"]["irrigation"]["ie_start"] == 120

    # Non-leap-year (2023) dev16->dev17, dev17->dev17, and upgrade_yaml reject startdls=366
    # (matching SUEWSConfig.validate_dls_parameters) while still accepting ie_end=366.
    nonleap_dev16 = copy.deepcopy(sample_config_dict)
    nonleap_dev16["schema_version"] = "2026.6.dev16"
    nonleap_dev16["model"]["control"]["start_time"] = "2023-01-01"
    nonleap_dev16["model"]["control"]["end_time"] = "2023-12-31"
    nonleap_dev16["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 366
    nonleap_dev16["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = 303
    with pytest.raises(YamlUpgradeError, match=r"startdls.*365"):
        migrator.migrate(nonleap_dev16, to_version="2026.6.dev17")
    nonleap_dev17 = copy.deepcopy(nonleap_dev16)
    nonleap_dev17["schema_version"] = "2026.6.dev17"
    with pytest.raises(YamlUpgradeError, match=r"startdls.*365"):
        migrator.migrate(nonleap_dev17, to_version="2026.6.dev17")
    for label, payload in (("dev16", nonleap_dev16), ("dev17", nonleap_dev17)):
        src_path = tmp_path / f"nonleap_{label}.yml"
        dst_path = tmp_path / f"nonleap_{label}_out.yml"
        src_path.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")
        with pytest.raises(YamlUpgradeError, match=r"startdls.*365"):
            upgrade_yaml(src_path, dst_path)
        assert not dst_path.exists()
    nonleap_ok = copy.deepcopy(nonleap_dev16)
    nonleap_ok["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 85.0
    migrated_2023 = migrator.migrate(nonleap_ok, to_version="2026.6.dev17")
    assert migrated_2023["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] == 85
    assert migrated_2023["sites"][0]["properties"]["irrigation"]["ie_end"] == {"value": 366}

    # 2. Historical target dev15 -> dev16 does NOT apply dev17 DOY rejection; dev17 does.
    frac = copy.deepcopy(sample_config_dict)
    frac["schema_version"] = "2026.6.dev15"
    frac["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 86.5
    hist = migrator.migrate(frac, to_version="2026.6.dev16")
    assert hist["schema_version"] == "2026.6.dev16"
    assert hist["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] == 86.5
    with pytest.raises(YamlUpgradeError, match=r"startdls.*fractional"):
        migrator.migrate(hist, to_version="2026.6.dev17")
    # Same-version dev17 check also rejects fractional DOYs.
    frac_dev17 = copy.deepcopy(frac)
    frac_dev17["schema_version"] = "2026.6.dev17"
    with pytest.raises(YamlUpgradeError, match=r"startdls.*fractional"):
        migrator.migrate(frac_dev17, to_version="2026.6.dev17")

    # 3. dev8 -> dev9 observed-soil migration does not leak dev17 DOY rejection onto dev9..dev16,
    #    including fractional floats and quoted whole/exponent/signed numeric strings.
    ctx = SoilMigrationContext(
        layer_bounds={0: (0, 0.2)},
        original_method=1,
        forcing_manifest={
            "completed": True,
            "original_method": 1,
            "source_unit": "m3 m-3",
            "input_sha256": "a" * 64,
            "output_sha256": "b" * 64,
        },
    )
    for legacy_doy in (
        86.5,
        "86.0",
        {"value": "86.0", "ref": {"desc": "legacy"}},
        "8.6e1",
        {"value": "+86", "ref": {"desc": "legacy"}},
    ):
        dev8 = copy.deepcopy(sample_config_dict)
        dev8["schema_version"] = "2026.6.dev8"
        dev8["model"]["physics"].pop("soil_moisture", None)
        dev8["model"]["physics"]["soil_moisture_deficit"] = 1
        for surf in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil"):
            dev8["sites"][0]["properties"]["land_cover"][surf].update(
                soil_depth=200.0,
                soil_store_capacity=80.0,
                soil_field_capacity=0.6,
            )
        dev8["sites"][0]["properties"]["soil_observation"] = {
            "depth": 200.0,
            "bulk_density": 1.25,
            "soil_not_rocks": 0.8,
        }
        dev8["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = copy.deepcopy(
            legacy_doy
        )
        for hist_target in ("2026.6.dev9", "2026.6.dev15", "2026.6.dev16"):
            out = migrator.migrate(dev8, to_version=hist_target, migration_context=ctx)
            assert out["schema_version"] == hist_target
            assert (
                out["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"]
                == legacy_doy
            )
        with pytest.raises(YamlUpgradeError, match="startdls"):
            migrator.migrate(dev8, to_version="2026.6.dev17", migration_context=ctx)

    # Pre-existing DLS bounds (ge=1, le=366) must STILL reject out-of-range,
    # non-finite, quoted out-of-range, and False values on historical targets
    # dev9..dev16, whereas historically unbounded irrigation DOYs keep passing
    # dev9..dev16 and fail at dev17.
    def _make_dev8_observed_soil() -> dict:
        cfg = copy.deepcopy(sample_config_dict)
        cfg["schema_version"] = "2026.6.dev8"
        cfg["model"]["physics"].pop("soil_moisture", None)
        cfg["model"]["physics"]["soil_moisture_deficit"] = 1
        for surf in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil"):
            cfg["sites"][0]["properties"]["land_cover"][surf].update(
                soil_depth=200.0,
                soil_store_capacity=80.0,
                soil_field_capacity=0.6,
            )
        cfg["sites"][0]["properties"]["soil_observation"] = {
            "depth": 200.0,
            "bulk_density": 1.25,
            "soil_not_rocks": 0.8,
        }
        return cfg

    for invalid_hist_dls in (0, -5, 367, 366.5, float("nan"), False, "0", "367", {"value": "367"}):
        dev8_bad_dls = _make_dev8_observed_soil()
        dev8_bad_dls["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = (
            copy.deepcopy(invalid_hist_dls)
        )
        for hist_target in ("2026.6.dev9", "2026.6.dev16"):
            with pytest.raises((ValidationError, YamlUpgradeError)):
                migrator.migrate(dev8_bad_dls, to_version=hist_target, migration_context=ctx)

    # Non-leap-year historical DLS bound (max 365) must also hold on dev9..dev16.
    for invalid_nonleap_dls in (365.5, 366, "366", "365.5", {"value": "366"}):
        dev8_2023 = _make_dev8_observed_soil()
        dev8_2023["model"]["control"]["start_time"] = "2023-01-01"
        dev8_2023["model"]["control"]["end_time"] = "2023-12-31"
        dev8_2023["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = (
            copy.deepcopy(invalid_nonleap_dls)
        )
        for hist_target in ("2026.6.dev9", "2026.6.dev16"):
            with pytest.raises((ValidationError, YamlUpgradeError)):
                migrator.migrate(dev8_2023, to_version=hist_target, migration_context=ctx)

    dev8_2023_ok = _make_dev8_observed_soil()
    dev8_2023_ok["model"]["control"]["start_time"] = "2023-01-01"
    dev8_2023_ok["model"]["control"]["end_time"] = "2023-12-31"
    dev8_2023_ok["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 364.5
    out_2023 = migrator.migrate(dev8_2023_ok, to_version="2026.6.dev16", migration_context=ctx)
    assert out_2023["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] == 364.5
    with pytest.raises(YamlUpgradeError, match="startdls"):
        migrator.migrate(dev8_2023_ok, to_version="2026.6.dev17", migration_context=ctx)

    for legacy_irr in (0, 367, 20.5, "367", False):
        dev8_irr = _make_dev8_observed_soil()
        dev8_irr["sites"][0]["properties"]["irrigation"]["ie_start"] = legacy_irr
        for hist_target in ("2026.6.dev9", "2026.6.dev16"):
            out = migrator.migrate(dev8_irr, to_version=hist_target, migration_context=ctx)
            assert out["schema_version"] == hist_target
            assert out["sites"][0]["properties"]["irrigation"]["ie_start"] == legacy_irr
        with pytest.raises(YamlUpgradeError, match="ie_start"):
            migrator.migrate(dev8_irr, to_version="2026.6.dev17", migration_context=ctx)

    # 4. Published historical JSON Schema (< dev17) preserves float DOY types and descriptions.
    s16 = generate_json_schema("2026.6.dev16")
    s17 = generate_json_schema("2026.6.dev17")
    for model_name, field_name in (
        ("IrrigationParams", "ie_start"),
        ("IrrigationParams", "ie_end"),
        ("AnthropogenicEmissions", "startdls"),
        ("AnthropogenicEmissions", "enddls"),
    ):
        p16 = json.dumps(s16["$defs"][model_name]["properties"][field_name])
        p17 = json.dumps(s17["$defs"][model_name]["properties"][field_name])
        assert "RefValue_float_" in p16 and '"type": "number"' in p16
        assert '"type": "integer"' in p17 and '"minimum": 1' in p17 and '"maximum": 366' in p17
