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
from importlib.resources import files
import json
from pathlib import Path
import subprocess
from types import SimpleNamespace
import warnings

from click.testing import CliRunner
import numpy as np
from pydantic import ValidationError
import pytest
import yaml

import supy
from supy import SUEWSSimulation
from supy.cmd.suews_cli import cli as suews_cli
from supy.cmd.validate_config import cli as validate_cli
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.human_activity import (
    AnthropogenicEmissions,
    IrrigationParams,
)
from supy.data_model.core.type import RefValue
from supy.data_model.validation.core.yaml_helpers import DLSCheck
from supy.data_model.validation.pipeline import phase_b
from supy.data_model.validation.pipeline.document import ConfigDocument
from supy.data_model.validation.pipeline.patches import plan_seasonal_patches
from supy.data_model.validation.pipeline.phase_b_rules import (
    other_rules as phase_b_other_rules,
)
from supy.data_model.validation.pipeline.phase_c import collect_phase_c_issues
from supy.util.converter.yaml_upgrade import (
    SoilMigrationContext,
    YamlUpgradeError,
    upgrade_yaml,
)

pytestmark = [
    pytest.mark.native,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-MIGRATE", "C-ROUNDTRIP", "C-SCHEMA"),
]

PROJECT_ROOT = Path(__file__).resolve().parents[2]
_KEEP_DOY = object()
_OMIT_DOY = object()
_SOIL_MIGRATION_CTX = SoilMigrationContext(
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


_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)


@pytest.fixture(scope="module")
def _sample_config_template() -> dict:
    """Parse the bundled sample YAML once per module via Traversable API."""
    resource = files("supy").joinpath("sample_data", "sample_config.yml")
    return yaml.load(resource.read_text(encoding="utf-8"), Loader=_YAML_LOADER)


@pytest.fixture
def sample_config_dict(_sample_config_template: dict) -> dict:
    return copy.deepcopy(_sample_config_template)


@pytest.fixture(scope="module")
def _schema_migrator():
    from supy.data_model.configuration.migration import SchemaMigrator

    return SchemaMigrator()


@pytest.fixture(scope="module")
def _dev8_observed_soil_template(_sample_config_template: dict) -> dict:
    """Base dev8 observed-soil config built once per module."""
    cfg = copy.deepcopy(_sample_config_template)
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


@pytest.fixture(scope="module")
def _resolved_engine_sample(sample_config_loaded: SUEWSConfig) -> tuple[dict, str, str]:
    """Resolve the engine YAML and 4-row forcing slice once per module."""
    sample_dir = files("supy").joinpath("sample_data")
    run_rust = import_module("supy._run_rust")
    resolved = _load_yaml(run_rust.config_to_engine_yaml(sample_config_loaded))
    forcing_entry = resolved["model"]["control"]["forcing"]["file"]
    forcing_name = (
        forcing_entry["value"] if isinstance(forcing_entry, dict) else forcing_entry
    )
    forcing_lines = [
        line
        for line in sample_dir.joinpath(forcing_name)
        .read_text(encoding="utf-8")
        .splitlines()
        if line.strip()
    ]
    return resolved, forcing_name, "\n".join(forcing_lines[:5]) + "\n"


def _load_yaml(text: str):
    return yaml.load(text, Loader=_YAML_LOADER)


def _write_yaml(path: Path, payload: dict) -> Path:
    path.write_text(
        yaml.dump(payload, Dumper=_YAML_DUMPER, sort_keys=False), encoding="utf-8"
    )
    return path


def _with_doy(
    base: dict,
    section: str,
    field: str,
    value: object,
    *,
    wrapped: bool = True,
    enable_irrigation: bool = True,
) -> dict:
    cfg = copy.deepcopy(base)
    if section == "irrigation" and enable_irrigation:
        cfg["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
    cfg["sites"][0]["properties"][section][field] = (
        {"value": value} if wrapped else copy.deepcopy(value)
    )
    return cfg


def _configure_site_dls(
    base: dict,
    *,
    lat: float | None = None,
    lng: float | None = None,
    timezone: float | None = None,
    year: int | None = None,
    clear_bounds: bool = False,
    startdls: object = _KEEP_DOY,
    enddls: object = _KEEP_DOY,
    wrap_doy: bool = True,
    disable_forcing: bool = True,
) -> dict:
    """Return a copy of *base* with site location, run year, and DLS fields set."""
    cfg = copy.deepcopy(base)
    control = cfg["model"]["control"]
    if clear_bounds:
        control["start_time"] = None
        control["end_time"] = None
    elif year is not None:
        control["start_time"] = f"{year}-01-01"
        control["end_time"] = f"{year}-12-31"
    if disable_forcing:
        control["forcing"]["file"] = None
    props = cfg["sites"][0]["properties"]
    if lat is not None:
        props["lat"] = {"value": lat}
    if lng is not None:
        props["lng"] = {"value": lng}
    if timezone is not None:
        props["timezone"] = {"value": timezone}
    anthro = props["anthropogenic_emissions"]
    for key, val in (("startdls", startdls), ("enddls", enddls)):
        if val is _KEEP_DOY:
            continue
        if val is _OMIT_DOY:
            anthro.pop(key, None)
        elif val is None or not wrap_doy or isinstance(val, dict):
            anthro[key] = copy.deepcopy(val)
        else:
            anthro[key] = {"value": val}
    return cfg


def _make_sample_sim(
    sample_config_loaded: SUEWSConfig, forcing=None
) -> SUEWSSimulation:
    """Build a fresh SUEWSSimulation from the cached config without reparsing forcing."""
    sim = SUEWSSimulation()
    sim.update_config(sample_config_loaded.model_copy(deep=True), auto_load_forcing=False)
    if forcing is not None:
        sim.update_forcing(forcing)
    return sim


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
    sample_config_loaded: SUEWSConfig,
    tmp_path: Path,
    section: str,
    field_name: str,
    bad_val,
) -> None:
    bad_cfg = _with_doy(sample_config_dict, section, field_name, bad_val)

    # Direct mapping construction (deepcopy so in-place enum coercion does not leak)
    with pytest.raises(ValidationError):
        SUEWSConfig(**copy.deepcopy(bad_cfg))

    # SUEWSConfig.from_yaml
    yaml_path = _write_yaml(tmp_path / f"bad_{field_name}.yml", bad_cfg)
    with pytest.raises(ValueError):
        SUEWSConfig.from_yaml(yaml_path)

    # SUEWSSimulation.update_config
    sim = _make_sample_sim(sample_config_loaded)
    with pytest.raises((ValueError, ValidationError)):
        sim.update_config(copy.deepcopy(bad_cfg))


def test_refvalue_live_assignment_rejects_booleans_fractionals_and_out_of_range(
    sample_config_loaded: SUEWSConfig,
) -> None:
    cfg = copy.deepcopy(sample_config_loaded)
    cfg.sites[0].properties.irrigation.daywat.monday = 1.0
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


@pytest.mark.parametrize(
    ("section", "field", "mutate_ref", "bad_val", "expected_match"),
    [
        ("irrigation", "ie_start", True, 0, r"irrigation\.ie_start"),
        ("anthropogenic_emissions", "enddls", True, 367, r"anthropogenic_emissions\.enddls"),
        ("irrigation", "ie_end", False, 20.5, "integer day of year"),
        ("irrigation", "ie_start", True, True, "boolean"),
    ],
    ids=["irr-zero", "dls-367", "irr-bare-fractional", "irr-bool"],
)
def test_validate_for_engine_catches_live_mutated_invalid_doys(
    sample_config_loaded: SUEWSConfig,
    section: str,
    field: str,
    mutate_ref: bool,
    bad_val,
    expected_match: str,
) -> None:
    cfg = copy.deepcopy(sample_config_loaded)
    if section == "irrigation":
        cfg.sites[0].properties.irrigation.daywat.monday = 1.0
    block = getattr(cfg.sites[0].properties, section)
    if mutate_ref:
        getattr(block, field).__dict__["value"] = bad_val
    else:
        block.__dict__[field] = bad_val
    with pytest.raises(ValueError, match=expected_match):
        cfg.validate_for_engine()


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
    upgraded = _load_yaml(out_ok.read_text(encoding="utf-8"))
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

    upgraded = _load_yaml(path_out.read_text(encoding="utf-8"))["sites"][0][
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
    sample_config_loaded: SUEWSConfig, sample_forcing_parsed, section, field, bad_val
) -> None:
    """``SUEWSSimulation.run()`` refuses a DOY mutated past field validation."""
    _, forcing = sample_forcing_parsed(12)
    sim = _make_sample_sim(sample_config_loaded, forcing)
    if section == "irrigation":
        sim.config.sites[0].properties.irrigation.daywat.monday = 1.0
    ref_value = getattr(getattr(sim.config.sites[0].properties, section), field)
    ref_value.__dict__["value"] = bad_val

    with pytest.raises(ValueError, match=rf"{section}\.{field}"):
        sim.run(start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1)
    assert sim._df_output is None


def _write_config_with_doy(
    sample_config_dict: dict, tmp_path: Path, section: str, field: str, value
) -> Path:
    cfg = _with_doy(sample_config_dict, section, field, value)
    return _write_yaml(tmp_path / f"{section}_{field}.yml", cfg)


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
    cfg = _configure_site_dls(
        sample_config_dict,
        startdls=disabled_mapping,
        enddls=disabled_mapping,
        wrap_doy=False,
    )
    irrigation = cfg["sites"][0]["properties"]["irrigation"]
    irrigation["ie_start"] = copy.deepcopy(disabled_mapping)
    irrigation["ie_end"] = copy.deepcopy(disabled_mapping)
    path = _write_yaml(tmp_path / "phase_b_disabled_dls.yml", cfg)

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
    cfg = _configure_site_dls(
        sample_config_dict,
        startdls=start_entry,
        enddls=end_entry,
        wrap_doy=False,
    )
    path = _write_yaml(tmp_path / "science_fixes_disabled_dls.yml", cfg)

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
        emitted = _load_yaml(updated_path.read_text(encoding="utf-8"))
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
    cfg_2023 = _configure_site_dls(
        sample_config_dict,
        year=2023,
        startdls=366,
        enddls=303,
        wrap_doy=False,
    )
    path_2023 = _write_yaml(tmp_path / "nonleap_2023_dls366.yml", cfg_2023)

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

    cfg = _configure_site_dls(
        sample_config_dict,
        startdls={"ref": {"desc": "disabled"}},
        enddls={},
        wrap_doy=False,
        disable_forcing=False,
    )
    cfg["sites"][0]["properties"]["irrigation"]["ie_start"] = {
        "ref": {"desc": "disabled"}
    }
    cfg["sites"][0]["properties"]["irrigation"]["ie_end"] = {}
    ok = _write_yaml(tmp_path / "ref_only.yml", cfg)
    ok_out = tmp_path / "converted_ok.yml"
    result = runner.invoke(suews_cli, ["convert", "-i", str(ok), "-o", str(ok_out)])
    assert result.exit_code == 0, result.output
    converted = _load_yaml(ok_out.read_text(encoding="utf-8"))
    assert converted["sites"][0]["properties"]["irrigation"]["ie_start"] == {
        "ref": {"desc": "disabled"}
    }
    reloaded = SUEWSConfig.from_yaml(str(ok_out))
    assert reloaded.sites[0].properties.irrigation.ie_start.value is None
    assert reloaded.sites[0].properties.irrigation.ie_start.ref.desc == "disabled"
    assert reloaded.sites[0].properties.irrigation.ie_end is None
    assert reloaded.sites[0].properties.anthropogenic_emissions.startdls.value is None
    assert reloaded.sites[0].properties.anthropogenic_emissions.enddls is None


@pytest.mark.e2e
@pytest.mark.parametrize(
    ("section", "field", "bad_val", "expected"),
    [
        ("irrigation", "ie_start", 20.5, "fractional part"),
        ("anthropogenic_emissions", "enddls", 367, "between 1 and 366"),
    ],
)
def test_suews_engine_cli_rejects_invalid_doy(
    tmp_path,
    suews_engine_bin: Path,
    _resolved_engine_sample: tuple[dict, str, str],
    section,
    field,
    bad_val,
    expected,
):
    """Standalone ``suews-engine run`` refuses a DOY the data model never saw."""
    resolved_template, forcing_name, forcing_slice = _resolved_engine_sample
    resolved = _with_doy(resolved_template, section, field, bad_val)
    resolved["model"]["control"]["output"]["dir"] = str(tmp_path)
    (tmp_path / forcing_name).write_text(forcing_slice, encoding="utf-8")
    config_path = _write_yaml(tmp_path / "engine_config.yml", resolved)

    result = subprocess.run(
        [str(suews_engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
    )

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


def test_schema_dev17_migration_normalises_and_enforces_nonleap(
    sample_config_dict: dict, _schema_migrator, tmp_path: Path
) -> None:
    """dev16 -> dev17 normalises whole floats and enforces non-leap-year DLS bounds."""
    from packaging.version import Version

    from supy.data_model.configuration import CURRENT_SCHEMA_VERSION

    assert Version(CURRENT_SCHEMA_VERSION) >= Version("2026.6.dev17")

    # 1. dev16 -> dev17 normalises whole-number floats and preserves ref metadata.
    whole = _configure_site_dls(
        sample_config_dict,
        startdls={"value": 86.0, "ref": {"desc": "spring"}},
        wrap_doy=False,
        disable_forcing=False,
    )
    whole["schema_version"] = "2026.6.dev16"
    whole["sites"][0]["properties"]["irrigation"]["ie_start"] = 120.0
    whole["sites"][0]["properties"]["irrigation"]["ie_end"] = 250.0
    migrated = _schema_migrator.migrate(whole, to_version="2026.6.dev17")
    assert migrated["schema_version"] == "2026.6.dev17"
    assert migrated["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] == {
        "value": 86,
        "ref": {"desc": "spring"},
    }
    assert type(migrated["sites"][0]["properties"]["irrigation"]["ie_start"]) is int
    assert migrated["sites"][0]["properties"]["irrigation"]["ie_start"] == 120

    # Non-leap-year (2023) dev16->dev17, dev17->dev17, and upgrade_yaml reject startdls=366
    # (matching SUEWSConfig.validate_dls_parameters) while still accepting ie_end=366.
    nonleap_dev16 = _configure_site_dls(
        sample_config_dict,
        year=2023,
        startdls=366,
        enddls=303,
        wrap_doy=False,
        disable_forcing=False,
    )
    nonleap_dev16["schema_version"] = "2026.6.dev16"
    with pytest.raises(YamlUpgradeError, match=r"startdls.*365"):
        _schema_migrator.migrate(nonleap_dev16, to_version="2026.6.dev17")
    nonleap_dev17 = copy.deepcopy(nonleap_dev16)
    nonleap_dev17["schema_version"] = "2026.6.dev17"
    with pytest.raises(YamlUpgradeError, match=r"startdls.*365"):
        _schema_migrator.migrate(nonleap_dev17, to_version="2026.6.dev17")
    for label, payload in (("dev16", nonleap_dev16), ("dev17", nonleap_dev17)):
        src_path = _write_yaml(tmp_path / f"nonleap_{label}.yml", payload)
        dst_path = tmp_path / f"nonleap_{label}_out.yml"
        with pytest.raises(YamlUpgradeError, match=r"startdls.*365"):
            upgrade_yaml(src_path, dst_path)
        assert not dst_path.exists()
    nonleap_ok = copy.deepcopy(nonleap_dev16)
    nonleap_ok["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 85.0
    migrated_2023 = _schema_migrator.migrate(nonleap_ok, to_version="2026.6.dev17")
    assert migrated_2023["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] == 85
    assert migrated_2023["sites"][0]["properties"]["irrigation"]["ie_end"] == {"value": 366}

    # Historical target dev15 -> dev16 does NOT apply dev17 DOY rejection; dev17 does.
    frac = _configure_site_dls(
        sample_config_dict, startdls=86.5, wrap_doy=False, disable_forcing=False
    )
    frac["schema_version"] = "2026.6.dev15"
    hist = _schema_migrator.migrate(frac, to_version="2026.6.dev16")
    assert hist["schema_version"] == "2026.6.dev16"
    assert hist["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] == 86.5
    with pytest.raises(YamlUpgradeError, match=r"startdls.*fractional"):
        _schema_migrator.migrate(hist, to_version="2026.6.dev17")
    frac_dev17 = copy.deepcopy(frac)
    frac_dev17["schema_version"] = "2026.6.dev17"
    with pytest.raises(YamlUpgradeError, match=r"startdls.*fractional"):
        _schema_migrator.migrate(frac_dev17, to_version="2026.6.dev17")


def test_schema_dev8_observed_soil_historical_doy_boundaries(
    _dev8_observed_soil_template: dict, _schema_migrator
) -> None:
    """dev8 -> dev9..dev16 preserves legacy float/string DOYs and pre-existing DLS bounds."""
    from supy.data_model.configuration.publisher import generate_json_schema

    def _dev8_with(section: str, field: str, value: object, *, year: int | None = None) -> dict:
        cfg = copy.deepcopy(_dev8_observed_soil_template)
        if year is not None:
            cfg["model"]["control"]["start_time"] = f"{year}-01-01"
            cfg["model"]["control"]["end_time"] = f"{year}-12-31"
        cfg["sites"][0]["properties"][section][field] = copy.deepcopy(value)
        return cfg

    # Legacy fractional floats and quoted strings survive dev9..dev16 and fail at dev17.
    for legacy_doy in (
        86.5,
        "86.0",
        {"value": "86.0", "ref": {"desc": "legacy"}},
        "8.6e1",
        {"value": "+86", "ref": {"desc": "legacy"}},
    ):
        dev8 = _dev8_with("anthropogenic_emissions", "startdls", legacy_doy)
        for hist_target in ("2026.6.dev9", "2026.6.dev15", "2026.6.dev16"):
            out = _schema_migrator.migrate(
                dev8, to_version=hist_target, migration_context=_SOIL_MIGRATION_CTX
            )
            assert out["schema_version"] == hist_target
            assert (
                out["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"]
                == legacy_doy
            )
        with pytest.raises(YamlUpgradeError, match="startdls"):
            _schema_migrator.migrate(
                dev8, to_version="2026.6.dev17", migration_context=_SOIL_MIGRATION_CTX
            )

    # Pre-existing DLS bounds (ge=1, le=366) must STILL reject invalid values on dev9..dev16.
    for invalid_hist_dls in (
        0, -5, 367, 366.5, float("nan"), False, "0", "367", {"value": "367"}
    ):
        dev8_bad_dls = _dev8_with("anthropogenic_emissions", "startdls", invalid_hist_dls)
        for hist_target in ("2026.6.dev9", "2026.6.dev16"):
            with pytest.raises((ValidationError, YamlUpgradeError)):
                _schema_migrator.migrate(
                    dev8_bad_dls,
                    to_version=hist_target,
                    migration_context=_SOIL_MIGRATION_CTX,
                )

    # Non-leap-year historical DLS bound (max 365) must also hold on dev9..dev16.
    for invalid_nonleap_dls in (365.5, 366, "366", "365.5", {"value": "366"}):
        dev8_2023 = _dev8_with(
            "anthropogenic_emissions", "startdls", invalid_nonleap_dls, year=2023
        )
        for hist_target in ("2026.6.dev9", "2026.6.dev16"):
            with pytest.raises((ValidationError, YamlUpgradeError)):
                _schema_migrator.migrate(
                    dev8_2023,
                    to_version=hist_target,
                    migration_context=_SOIL_MIGRATION_CTX,
                )

    dev8_2023_ok = _dev8_with("anthropogenic_emissions", "startdls", 364.5, year=2023)
    out_2023 = _schema_migrator.migrate(
        dev8_2023_ok, to_version="2026.6.dev16", migration_context=_SOIL_MIGRATION_CTX
    )
    assert out_2023["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] == 364.5
    with pytest.raises(YamlUpgradeError, match="startdls"):
        _schema_migrator.migrate(
            dev8_2023_ok, to_version="2026.6.dev17", migration_context=_SOIL_MIGRATION_CTX
        )

    # Historically unbounded irrigation DOYs keep passing dev9..dev16 and fail at dev17.
    for legacy_irr in (0, 367, 20.5, "367", False):
        dev8_irr = _dev8_with("irrigation", "ie_start", legacy_irr)
        for hist_target in ("2026.6.dev9", "2026.6.dev16"):
            out = _schema_migrator.migrate(
                dev8_irr, to_version=hist_target, migration_context=_SOIL_MIGRATION_CTX
            )
            assert out["schema_version"] == hist_target
            assert out["sites"][0]["properties"]["irrigation"]["ie_start"] == legacy_irr
        with pytest.raises(YamlUpgradeError, match="ie_start"):
            _schema_migrator.migrate(
                dev8_irr, to_version="2026.6.dev17", migration_context=_SOIL_MIGRATION_CTX
            )

    # Published historical JSON Schema (< dev17) preserves float DOY types and descriptions.
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


@pytest.mark.parametrize("science_fixes", ["apply", "suggest", "off"])
@pytest.mark.parametrize("has_dst", [True, False], ids=["site_with_dst", "site_without_dst"])
@pytest.mark.parametrize(
    ("present_key", "user_val", "omit_missing"),
    [
        ("startdls", 90, False),
        ("enddls", 295, True),
    ],
    ids=["only_startdls_null_enddls", "only_enddls_omitted_startdls"],
)
def test_half_specified_dst_policy_and_site_dst_matrix(
    sample_config_dict: dict,
    tmp_path: Path,
    science_fixes: str,
    has_dst: bool,
    present_key: str,
    user_val: int,
    omit_missing: bool,
) -> None:
    """Half-specified DST obeys ``apply``/``suggest``/``off`` x DST/non-DST sites."""
    lat, lng, tz = (51.5, -0.1, 0) if has_dst else (1.35, 103.82, 8)
    missing_key = "enddls" if present_key == "startdls" else "startdls"
    missing_val = _OMIT_DOY if omit_missing else None
    cfg = _configure_site_dls(
        sample_config_dict,
        lat=lat,
        lng=lng,
        timezone=tz,
        year=2012,
        startdls=user_val if present_key == "startdls" else missing_val,
        enddls=user_val if present_key == "enddls" else missing_val,
    )

    calc_start, calc_end, _ = DLSCheck(lat=lat, lng=lng, year=2012).compute_dst_transitions()
    if has_dst:
        assert calc_start and calc_end
        expected_missing = calc_end if missing_key == "enddls" else calc_start
    else:
        assert not (calc_start and calc_end)
        expected_missing = None

    report = supy.validate_config(
        cfg,
        pipeline="ABC",
        mode="public",
        forcing="off",
        science_fixes=science_fixes,
    )
    b_issues = [i for i in report.phases[1].issues if "startdls,enddls" in (i.yaml_path or "")]

    if has_dst and science_fixes == "apply":
        assert report.is_valid, report.to_text()
        assert not report.has_errors
        assert any(i.severity == "INFO" for i in b_issues)
        applied_params = {i.yaml_path for i in report.applied_fixes}
        assert f"anthropogenic_emissions.{missing_key}" in applied_params
        assert f"anthropogenic_emissions.{present_key}" not in applied_params

        cfg_path = _write_yaml(tmp_path / "half_dst.yml", cfg)
        cli_res = CliRunner().invoke(
            validate_cli,
            [
                "-p",
                "ABC",
                "--forcing",
                "off",
                "--science-fixes",
                "apply",
                str(cfg_path),
            ],
        )
        assert cli_res.exit_code == 0, cli_res.output
        updated_path = tmp_path / "updated_half_dst.yml"
        updated = _load_yaml(updated_path.read_text(encoding="utf-8"))
        updated_anthro = updated["sites"][0]["properties"]["anthropogenic_emissions"]
        assert updated_anthro[present_key] == {"value": user_val}
        expected_filled = (
            {"value": expected_missing} if omit_missing else expected_missing
        )
        assert updated_anthro[missing_key] == expected_filled
        reloaded = SUEWSConfig.from_yaml(str(updated_path))
        assert getattr(reloaded.sites[0].properties.anthropogenic_emissions, present_key).value == user_val
        missing_loaded = getattr(reloaded.sites[0].properties.anthropogenic_emissions, missing_key)
        assert (missing_loaded.value if isinstance(missing_loaded, RefValue) else missing_loaded) == expected_missing
    else:
        assert not report.is_valid
        assert report.has_errors
        dls_errors = [i for i in report.phases[1].issues if i.severity == "ERROR" and "startdls,enddls" in (i.yaml_path or "")]
        assert len(dls_errors) == 1
        if has_dst:
            assert dls_errors[0].suggested_value == expected_missing
        else:
            assert f"Remove {present_key}" in str(dls_errors[0].suggested_value)


def test_half_specified_dst_apply_without_year_still_errors(
    sample_config_dict: dict,
) -> None:
    """When simulation year cannot be resolved, half-specified DST still fails under apply."""
    cfg = _configure_site_dls(
        sample_config_dict, clear_bounds=True, startdls=90, enddls=None
    )

    report = supy.validate_config(
        cfg,
        pipeline="ABC",
        mode="public",
        forcing="off",
        science_fixes="apply",
    )
    assert not report.is_valid
    assert any(
        i.severity == "ERROR" and "startdls,enddls" in (i.yaml_path or "")
        for i in report.errors
    )


def test_plan_seasonal_patches_half_dst_and_full_pair_behaviour() -> None:
    """``plan_seasonal_patches`` fills only the missing half and keeps full-pair overwrite."""
    calc_start, calc_end, _ = DLSCheck(lat=51.5, lng=-0.1, year=2012).compute_dst_transitions()
    assert calc_start and calc_end

    def _make_site_cfg(lat: float, lng: float, tz: int, anthro: dict) -> dict:
        return {
            "sites": [
                {
                    "gridiv": 1,
                    "properties": {
                        "lat": lat,
                        "lng": lng,
                        "timezone": tz,
                        "anthropogenic_emissions": anthro,
                    },
                }
            ]
        }

    # 1. Half case: startdls kept at 90, enddls filled.
    only_start = _make_site_cfg(51.5, -0.1, 0, {"startdls": 90, "enddls": None})
    plan_s = plan_seasonal_patches(only_start, "2012-01-01", 2012)
    assert [a.parameter for a in plan_s.adjustments] == ["anthropogenic_emissions.enddls"]
    applied_s = ConfigDocument.from_dict(only_start, normalise=False).apply_patches(plan_s.patches).data
    assert applied_s["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] == 90
    assert applied_s["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] == calc_end

    # 2. Half case: enddls kept at 295, startdls filled even when key omitted.
    only_end = _make_site_cfg(51.5, -0.1, 0, {"enddls": 295})
    plan_e = plan_seasonal_patches(only_end, "2012-01-01", 2012)
    assert [a.parameter for a in plan_e.adjustments] == ["anthropogenic_emissions.startdls"]
    applied_e = ConfigDocument.from_dict(only_end, normalise=False).apply_patches(plan_e.patches).data
    assert applied_e["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] == 295
    assert applied_e["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] == {"value": calc_start}

    # 3. Full pair retains existing overwrite behaviour (deferred to h).
    both = _make_site_cfg(51.5, -0.1, 0, {"startdls": 90, "enddls": 295})
    plan_both = plan_seasonal_patches(both, "2012-01-01", 2012)
    assert [a.parameter for a in plan_both.adjustments] == [
        "anthropogenic_emissions.startdls",
        "anthropogenic_emissions.enddls",
    ]
    applied_both = ConfigDocument.from_dict(both, normalise=False).apply_patches(plan_both.patches).data
    assert applied_both["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] == calc_start
    assert applied_both["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] == calc_end

    # 4. Site without DST emits no DLS patch.
    no_dst = _make_site_cfg(1.35, 103.82, 8, {"startdls": 90, "enddls": None})
    plan_nodst = plan_seasonal_patches(no_dst, "2012-01-01", 2012)
    assert plan_nodst.patches == []


def test_half_dst_unavailable_timezone_lookup_does_not_claim_no_dst(
    sample_config_dict: dict, monkeypatch: pytest.MonkeyPatch
) -> None:
    """When DLSCheck returns (None, None, None), Phase B reports unavailable lookup rather than non-DST."""
    from supy.data_model.validation.pipeline.phase_b_rules import other_rules

    class _UnavailableDLSCheck:
        def __init__(self, **_kwargs):
            pass

        def compute_dst_transitions(self):
            return None, None, None

    monkeypatch.setattr(other_rules, "DLSCheck", _UnavailableDLSCheck)
    cfg = _configure_site_dls(
        sample_config_dict, startdls=90, enddls=None, disable_forcing=False
    )

    report = supy.validate_config(
        cfg,
        pipeline="B",
        mode="public",
        forcing="off",
        science_fixes="suggest",
    )
    assert not report.is_valid
    dls_errors = [
        i for i in report.errors if "startdls,enddls" in (i.yaml_path or "")
    ]
    assert len(dls_errors) == 1
    msg = dls_errors[0].message.lower()
    assert "no daylight saving transition" not in msg
    assert "unavailable" in msg
    assert "both" in msg


def test_half_dst_partial_lookup_and_first_of_month_transition(
    sample_config_dict: dict, monkeypatch: pytest.MonkeyPatch
) -> None:
    """First-of-month DST transitions are detected, and partial lookups never claim no-DST."""
    from supy.data_model.validation.pipeline import phase_b
    from supy.data_model.validation.pipeline.phase_b_rules import other_rules

    # 1. Australia/Sydney 2012 ended DST on April 1 (DOY 92) and started on October 7 (DOY 281).
    syd_start, syd_end, syd_tz = DLSCheck(
        lat=-33.87, lng=151.21, year=2012
    ).compute_dst_transitions()
    assert syd_start == 281
    assert syd_end == 92
    assert syd_tz == 10

    syd_cfg = _configure_site_dls(
        sample_config_dict,
        lat=-33.87,
        lng=151.21,
        timezone=10,
        startdls=281,
        enddls=None,
        disable_forcing=False,
    )
    syd_apply = supy.validate_config(
        syd_cfg, pipeline="ABC", mode="public", forcing="off", science_fixes="apply"
    )
    assert syd_apply.is_valid, syd_apply.to_text()
    assert any(
        "enddls" in (fix.yaml_path or "") and str(fix.suggested_value) == "92"
        for fix in syd_apply.applied_fixes
    ), syd_apply.applied_fixes

    # 2. Partial lookup (None, 92, 10): filling enddls succeeds under apply, while
    #    missing startdls reports unavailable lookup rather than claiming "no DST".
    class _PartialDLSCheck:
        def __init__(self, **_kwargs):
            pass

        def compute_dst_transitions(self):
            return None, 92, 10

    monkeypatch.setattr(other_rules, "DLSCheck", _PartialDLSCheck)
    monkeypatch.setattr(phase_b, "DLSCheck", _PartialDLSCheck)

    only_start = _configure_site_dls(
        sample_config_dict, startdls=281, enddls=None, disable_forcing=False
    )
    partial_ok = supy.validate_config(
        only_start, pipeline="ABC", mode="public", forcing="off", science_fixes="apply"
    )
    assert partial_ok.is_valid, partial_ok.to_text()
    assert any(
        "enddls" in (fix.yaml_path or "") and str(fix.suggested_value) == "92"
        for fix in partial_ok.applied_fixes
    ), partial_ok.applied_fixes

    only_end = _configure_site_dls(
        sample_config_dict, startdls=None, enddls=92, disable_forcing=False
    )
    partial_missing = supy.validate_config(
        only_end, pipeline="B", mode="public", forcing="off", science_fixes="apply"
    )
    assert not partial_missing.is_valid
    dls_errs = [
        i for i in partial_missing.errors if "startdls,enddls" in (i.yaml_path or "")
    ]
    assert len(dls_errs) == 1
    assert "no daylight saving transition" not in dls_errs[0].message.lower()
    assert "unavailable" in dls_errs[0].message.lower()


@pytest.mark.parametrize("science_fixes", ["suggest", "apply"])
def test_multi_period_dst_africa_casablanca_2012_public_api_and_cli(
    sample_config_dict: dict,
    tmp_path: Path,
    science_fixes: str,
) -> None:
    """Multi-period DST years (Africa/Casablanca 2012) never silently truncate or partially patch."""
    casablanca_lat = 33.5731
    casablanca_lng = -7.5898

    schedule = DLSCheck(
        lat=casablanca_lat, lng=casablanca_lng, year=2012
    ).inspect_dst_schedule()
    assert schedule.timezone == "Africa/Casablanca"
    assert schedule.has_multiple_periods is True
    assert schedule.starts == (120, 233)
    assert schedule.ends == (202, 274)
    assert DLSCheck(
        lat=casablanca_lat, lng=casablanca_lng, year=2012
    ).compute_dst_transitions() == (None, None, 0)

    def _casablanca_cfg(startdls: int | None, enddls: int | None) -> dict:
        return _configure_site_dls(
            sample_config_dict,
            lat=casablanca_lat,
            lng=casablanca_lng,
            timezone=0,
            year=2012,
            startdls=startdls,
            enddls=enddls,
        )

    # 1. Half-configured input remains blocking under both suggest and apply,
    #    with explicit unsupported-schedule diagnostic and zero DST patches.
    for start_val, end_val in ((120, None), (None, 202)):
        half_cfg = _casablanca_cfg(start_val, end_val)
        rep_half = supy.validate_config(
            half_cfg,
            pipeline="ABC",
            mode="public",
            forcing="off",
            science_fixes=science_fixes,
        )
        assert not rep_half.is_valid, (science_fixes, rep_half.to_text())
        dls_errs = [
            i for i in rep_half.errors if "startdls,enddls" in (i.yaml_path or "")
        ]
        assert len(dls_errs) == 1, rep_half.errors
        err_msg = dls_errs[0].message
        assert "Africa/Casablanca" in err_msg
        assert "multiple daylight saving periods" in err_msg.lower()
        assert "120, 233" in err_msg and "202, 274" in err_msg
        assert not any(
            "startdls" in (f.yaml_path or "") or "enddls" in (f.yaml_path or "")
            for f in rep_half.applied_fixes
        )

    half_dir = tmp_path / f"casablanca_half_{science_fixes}"
    half_dir.mkdir()
    half_path = _write_yaml(half_dir / "casablanca_half.yml", _casablanca_cfg(120, None))
    cli_half_dry = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--dry-run",
            "--forcing",
            "off",
            "--science-fixes",
            science_fixes,
            "--format",
            "json",
            str(half_path),
        ],
    )
    assert cli_half_dry.exit_code == 1, cli_half_dry.output
    half_env = json.loads(cli_half_dry.stdout)
    assert any(
        "Africa/Casablanca" in str(err.get("message", ""))
        and "multiple daylight saving periods" in str(err.get("message", "")).lower()
        for err in half_env["errors"]
    ), half_env["errors"]

    cli_half_staged = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--forcing",
            "off",
            "--science-fixes",
            science_fixes,
            str(half_path),
        ],
    )
    assert cli_half_staged.exit_code == 1, cli_half_staged.output
    updated_half_path = half_dir / "updated_casablanca_half.yml"
    if updated_half_path.exists():
        updated_half = _load_yaml(updated_half_path.read_text(encoding="utf-8"))
        updated_half_anthro = updated_half["sites"][0]["properties"][
            "anthropogenic_emissions"
        ]
        assert updated_half_anthro["startdls"] == {"value": 120}
        assert updated_half_anthro["enddls"] is None

    # 2. Already-paired input emits an explicit unsupported-schedule warning
    #    and preserves existing startdls/enddls bounds untouched under both suggest and apply.
    paired_cfg = _casablanca_cfg(120, 274)
    rep_paired = supy.validate_config(
        paired_cfg,
        pipeline="ABC",
        mode="public",
        forcing="off",
        science_fixes=science_fixes,
    )
    assert rep_paired.is_valid, rep_paired.to_text()
    dls_warns = [
        i for i in rep_paired.warnings if "startdls,enddls" in (i.yaml_path or "")
    ]
    assert len(dls_warns) == 1, rep_paired.warnings
    assert "Africa/Casablanca" in dls_warns[0].message
    assert "multiple daylight saving periods" in dls_warns[0].message.lower()
    assert not any(
        "startdls" in (i.yaml_path or "") or "enddls" in (i.yaml_path or "")
        for i in (*rep_paired.applied_fixes, *rep_paired.suggestions)
    )

    paired_dir = tmp_path / f"casablanca_paired_{science_fixes}"
    paired_dir.mkdir()
    paired_path = _write_yaml(paired_dir / "casablanca_paired.yml", paired_cfg)
    cli_paired_dry = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--dry-run",
            "--forcing",
            "off",
            "--science-fixes",
            science_fixes,
            "--format",
            "json",
            str(paired_path),
        ],
    )
    assert cli_paired_dry.exit_code == 0, cli_paired_dry.output
    paired_env = json.loads(cli_paired_dry.stdout)
    assert paired_env["data"]["is_valid"] is True
    assert any(
        "Africa/Casablanca" in str(w.get("message", ""))
        and "multiple daylight saving periods" in str(w.get("message", "")).lower()
        for w in paired_env["warnings"]
    ), paired_env["warnings"]

    cli_paired_staged = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--forcing",
            "off",
            "--science-fixes",
            science_fixes,
            str(paired_path),
        ],
    )
    assert cli_paired_staged.exit_code == 0, cli_paired_staged.output
    updated_paired = _load_yaml(
        (paired_dir / "updated_casablanca_paired.yml").read_text(encoding="utf-8")
    )
    updated_anthro = updated_paired["sites"][0]["properties"]["anthropogenic_emissions"]
    assert updated_anthro["startdls"] == {"value": 120}
    assert updated_anthro["enddls"] == {"value": 274}


def test_dst_base_offset_and_transitions_derived_together_for_casablanca_2019(
    sample_config_dict: dict,
) -> None:
    """Mid-year Ramadan drop (Africa/Casablanca 2019) derives base offset 0 with consistent DST bounds."""
    casablanca_lat = 33.5731
    casablanca_lng = -7.5898

    schedule = DLSCheck(
        lat=casablanca_lat, lng=casablanca_lng, year=2019
    ).inspect_dst_schedule()
    assert schedule.timezone == "Africa/Casablanca"
    assert schedule.scan_complete is True
    assert schedule.has_multiple_periods is False
    # In 2019 Morocco stayed at UTC+1 except May 5 (DOY 125) to Jun 9 (DOY 160) at UTC+0.
    # Deriving standard offset from the full year yields base 0 with DST wrap [160..125).
    assert schedule.utc_offset_hours == 0
    assert schedule.start == 160
    assert schedule.end == 125
    assert DLSCheck(
        lat=casablanca_lat, lng=casablanca_lng, year=2019
    ).compute_dst_transitions() == (160, 125, 0)

    cfg = _configure_site_dls(
        sample_config_dict,
        lat=casablanca_lat,
        lng=casablanca_lng,
        timezone=1,
        year=2019,
        startdls=160,
        enddls=None,
    )

    rep = supy.validate_config(
        cfg, pipeline="ABC", mode="public", forcing="off", science_fixes="apply"
    )
    assert rep.is_valid, rep.to_text()
    applied = {fix.yaml_path: str(fix.suggested_value) for fix in rep.applied_fixes}
    assert applied.get("anthropogenic_emissions.enddls") == "125"
    assert applied.get("timezone") == "0"


def test_dst_failed_daily_localisation_marks_scan_incomplete_and_blocks_auto_patch(
    sample_config_dict: dict, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Any daily localisation exception marks the DST scan incomplete and suppresses auto-repair."""
    import pytz

    from supy.data_model.validation.core import yaml_helpers

    real_tz = pytz.timezone("Europe/London")

    class _FlakyTZ:
        def localize(self, dt, is_dst=None):
            if dt.month == 3 and dt.day == 25:
                raise RuntimeError("simulated localisation failure on transition day")
            return real_tz.localize(dt, is_dst=is_dst)

    monkeypatch.setattr(yaml_helpers.pytz, "timezone", lambda _name: _FlakyTZ())

    schedule = DLSCheck(lat=51.5, lng=-0.1, year=2012).inspect_dst_schedule()
    assert schedule.scan_complete is False
    assert schedule.start is None
    assert schedule.end is None
    assert schedule.has_no_dst is False

    cfg = _configure_site_dls(
        sample_config_dict, startdls=85, enddls=None, disable_forcing=False
    )
    rep = supy.validate_config(
        cfg, pipeline="ABC", mode="public", forcing="off", science_fixes="apply"
    )
    assert not rep.is_valid
    assert not any(
        "startdls" in (f.yaml_path or "") or "enddls" in (f.yaml_path or "")
        for f in rep.applied_fixes
    )


def test_dst_timezone_lookup_exception_preserves_valid_pair_and_reports_half_pair_cleanly(
    sample_config_dict: dict, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Unknown timezone errors never invalidate complete DST pairs and stay actionable on half pairs."""
    import pytz

    from supy.data_model.validation.core import yaml_helpers

    def _raise_unknown(_name: str):
        raise pytz.UnknownTimeZoneError("Simulated/Unknown")

    monkeypatch.setattr(yaml_helpers.pytz, "timezone", _raise_unknown)

    # Complete valid pair stays valid across off, suggest, and apply.
    paired_cfg = _configure_site_dls(
        sample_config_dict, startdls=85, enddls=302, disable_forcing=False
    )
    for mode_fix in ("off", "suggest", "apply"):
        rep = supy.validate_config(
            paired_cfg,
            pipeline="ABC",
            mode="public",
            forcing="off",
            science_fixes=mode_fix,
        )
        assert rep.is_valid, (mode_fix, rep.to_text())
        assert not any(i.code.startswith("VALIDATOR.RULE_FAILED") for i in rep.issues)

    # Half-configured pair returns actionable GEOGRAPHY error, never a rule crash.
    half_cfg = _configure_site_dls(
        sample_config_dict, startdls=85, enddls=None, disable_forcing=False
    )
    rep_half = supy.validate_config(
        half_cfg, pipeline="ABC", mode="public", forcing="off", science_fixes="apply"
    )
    assert not rep_half.is_valid
    assert not any(i.code.startswith("VALIDATOR.RULE_FAILED") for i in rep_half.issues)
    assert any(
        "startdls,enddls" in (e.yaml_path or "") and "unavailable" in e.message.lower()
        for e in rep_half.errors
    ), rep_half.errors


def test_reordered_land_cover_keys_deduplicate_small_drift_warning_in_dry_run(
    sample_config_dict: dict, tmp_path: Path
) -> None:
    """Reordering land_cover YAML keys still deduplicates Phase B/C small-drift warnings in dry-run."""
    cfg = copy.deepcopy(sample_config_dict)
    lc = cfg["sites"][0]["properties"]["land_cover"]
    lc["paved"]["sfr"]["value"] = float(lc["paved"]["sfr"]["value"]) + 5e-5
    reordered_lc = {key: lc[key] for key in reversed(list(lc.keys()))}
    cfg["sites"][0]["properties"]["land_cover"] = reordered_lc

    cfg_path = _write_yaml(tmp_path / "reordered_lc_drift.yml", cfg)

    res = CliRunner().invoke(
        validate_cli,
        ["-p", "ABC", "--dry-run", "--forcing", "off", "--format", "json", str(cfg_path)],
    )
    assert res.exit_code == 0, res.output
    envelope = json.loads(res.stdout)
    drift_warnings = [
        w
        for w in envelope["warnings"]
        if isinstance(w, dict)
        and str(w.get("issue_code", "")).endswith(
            "LAND_COVER.SURFACE_FRACTIONS.SMALL_DRIFT"
        )
    ]
    assert len(drift_warnings) == 1, envelope["warnings"]


def test_non_hourly_dst_lord_howe_reports_explicit_diagnostic_without_auto_patch(
    sample_config_dict: dict,
) -> None:
    """Half-hour DST (Australia/Lord_Howe 2012) emits an explicit non-hourly diagnostic."""
    lord_howe_lat = -31.555
    lord_howe_lng = 159.082
    schedule = DLSCheck(
        lat=lord_howe_lat, lng=lord_howe_lng, year=2012
    ).inspect_dst_schedule()
    assert schedule.timezone == "Australia/Lord_Howe"
    assert schedule.has_non_hourly_dst is True
    assert schedule.offset_step_hours == pytest.approx(0.5)
    assert schedule.start is None
    assert schedule.end is None

    def _lh_cfg(startdls: int | None, enddls: int | None) -> dict:
        return _configure_site_dls(
            sample_config_dict,
            lat=lord_howe_lat,
            lng=lord_howe_lng,
            timezone=10.5,
            year=2012,
            startdls=startdls,
            enddls=enddls,
        )

    # Half-configured stays blocking with explicit non-hourly message (not unavailable).
    rep_half = supy.validate_config(
        _lh_cfg(281, None), pipeline="ABC", mode="public", forcing="off", science_fixes="apply"
    )
    assert not rep_half.is_valid
    assert any(
        "non-hourly daylight saving shift" in e.message.lower()
        and "unavailable" not in e.message.lower()
        for e in rep_half.errors
    ), rep_half.errors

    # Already-paired stays valid with explicit non-hourly warning and zero DST patches.
    rep_paired = supy.validate_config(
        _lh_cfg(281, 92), pipeline="ABC", mode="public", forcing="off", science_fixes="apply"
    )
    assert rep_paired.is_valid, rep_paired.to_text()
    assert any(
        "non-hourly daylight saving shift" in w.message.lower()
        for w in rep_paired.warnings
    ), rep_paired.warnings
    assert not any(
        "startdls" in (f.yaml_path or "") or "enddls" in (f.yaml_path or "")
        for f in rep_paired.applied_fixes
    )

    # Phase C-only public validation and direct SUEWSConfig.from_dict() also surface
    # the explicit non-hourly DST diagnostic rather than silently omitting it.
    rep_c_only = supy.validate_config(
        _lh_cfg(281, 92), pipeline="C", mode="public", forcing="off"
    )
    assert rep_c_only.is_valid, rep_c_only.to_text()
    assert any(
        "non-hourly daylight saving shift" in i.message.lower()
        for i in rep_c_only.issues
    ), rep_c_only.issues

    lh_model = SUEWSConfig.from_dict(_lh_cfg(281, 92))
    info_msgs = lh_model._validation_summary.get("info_messages", [])
    assert any(
        "non-hourly daylight saving shift" in msg.lower() for msg in info_msgs
    ), info_msgs

    # Jan 1 non-hourly boundary shift (Asia/Kathmandu 1986: UTC+5:30 -> UTC+5:45)
    # must also be classified as non-hourly and never auto-patch startdls=1.
    ktm_schedule = DLSCheck(
        lat=27.7172, lng=85.3240, year=1986
    ).inspect_dst_schedule()
    assert ktm_schedule.timezone == "Asia/Kathmandu"
    assert ktm_schedule.has_non_hourly_dst is True
    assert ktm_schedule.offset_step_hours == pytest.approx(0.25)
    assert ktm_schedule.start is None
    assert ktm_schedule.end is None

    ktm_cfg = _configure_site_dls(
        sample_config_dict,
        lat=27.7172,
        lng=85.3240,
        timezone=5.75,
        year=1986,
        startdls=None,
        enddls=295,
    )
    ktm_rep = supy.validate_config(
        ktm_cfg, pipeline="ABC", mode="public", forcing="off", science_fixes="apply"
    )
    assert not ktm_rep.is_valid
    assert any(
        "non-hourly daylight saving shift" in e.message.lower()
        for e in ktm_rep.errors
    ), ktm_rep.errors
    assert not any(
        "startdls" in (f.yaml_path or "") or "enddls" in (f.yaml_path or "")
        for f in ktm_rep.applied_fixes
    )
