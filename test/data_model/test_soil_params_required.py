"""Soil parameters are required on active non-water surfaces.

gh#2077 / gh#2076 / gh#2001 / gh#2002 (part of gh#1991): omitting or nulling
``soil_depth``, ``soil_store_capacity`` or ``saturated_hydraulic_conductivity``
on an active (``sfr > 0``) non-water surface used to pass validation and reach
the water-balance kernel as ``0.0`` via the Rust bridge. Active non-water
surfaces must now supply all three parameters; inactive surfaces (``sfr == 0``)
and ``water`` remain exempt. The 2026.6.dev11 migration requires the user to
supply ``soil_depth`` (reporting each affected site and surface without writing
output) when it is missing or null on an active surface with positive
``soil_store_capacity``, preserves supplied values and inactive/``water``
surfaces unchanged, and writes explicit ``0.0`` for other missing or null
active soil parameters.
"""

from __future__ import annotations

import copy
import json
from pathlib import Path

from click.testing import CliRunner
import pytest
import yaml

from supy import SUEWSSimulation
from supy._env import trv_supy_module
from supy._run_rust import run_suews_rust
from supy.cmd.suews_cli import cli
from supy.data_model.configuration import (
    CURRENT_SCHEMA_VERSION,
    SchemaMigrator,
    migrate_config_file,
)
from supy.data_model.core import SUEWSConfig
from supy.data_model.validation.required_fields import (
    NON_WATER_SURFACES,
    SOIL_REQUIRED,
    required_when,
)
from supy.util.converter.yaml_upgrade import YamlUpgradeError, upgrade_yaml

pytestmark = [pytest.mark.api, pytest.mark.cfg]

SOIL_FIELDS: tuple[str, ...] = tuple(SOIL_REQUIRED)


@pytest.fixture(scope="module")
def sample_dict() -> dict:
    """Return a parsed copy of the shipped sample configuration."""
    text = (trv_supy_module / "sample_data" / "sample_config.yml").read_text(
        encoding="utf-8"
    )
    return yaml.safe_load(text)


def _land_cover(data: dict) -> dict:
    return data["sites"][0]["properties"]["land_cover"]


def _with_soil_override(
    sample_dict: dict, surface: str, field: str, value: object
) -> dict:
    """Return a copy of the sample with one active surface soil field changed.

    If *surface* has ``sfr == 0`` in the shipped sample (``evetr``, ``bsoil``),
    0.05 is shifted from ``paved`` so the surface is genuinely active while
    preserving ``sum(sfr) == 1.0``. ``value`` is a scalar to wrap as
    ``{value: scalar}``, ``None`` for ``{value: null}``, or ``...`` to omit
    the key completely.
    """
    data = copy.deepcopy(sample_dict)
    lc = _land_cover(data)
    surf = lc[surface]
    if surf["sfr"]["value"] <= 0.0:
        lc["paved"]["sfr"]["value"] = round(lc["paved"]["sfr"]["value"] - 0.05, 6)
        surf["sfr"]["value"] = 0.05
    surf.pop(field, None)
    if value is not ...:
        surf[field] = {"value": value}
    return data


@pytest.mark.parametrize("surface", NON_WATER_SURFACES)
@pytest.mark.parametrize("field", SOIL_FIELDS)
@pytest.mark.parametrize("value", [..., None])
def test_active_non_water_surface_requires_soil_params(
    sample_dict: dict, surface: str, field: str, value: object
) -> None:
    """Active non-water surfaces reject omitted or null soil parameters."""
    # ARRANGE
    data = _with_soil_override(sample_dict, surface, field, value)

    # ACT / ASSERT
    with pytest.raises(
        ValueError,
        match=rf"land_cover\.{surface}: {field} is None but {surface}\.sfr=",
    ):
        SUEWSConfig.from_dict(data)


def test_from_yaml_rejects_missing_soil_param_and_annotates(
    sample_dict: dict, tmp_path: Path
) -> None:
    """Loading from YAML rejects omitted soil parameters and annotates them."""
    # ARRANGE
    data = _with_soil_override(sample_dict, "paved", "soil_depth", ...)
    config_path = tmp_path / "missing_soil.yml"
    config_path.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")

    # ACT / ASSERT
    with pytest.raises(ValueError, match=r"land_cover\.paved: soil_depth is None"):
        SUEWSConfig.from_yaml(str(config_path))


def test_inactive_non_water_surface_allows_omitted_soil_params(
    sample_dict: dict,
) -> None:
    """An inactive (sfr == 0) non-water surface may omit all soil parameters."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    lc = _land_cover(data)
    grass_sfr = lc["grass"]["sfr"]["value"]
    lc["paved"]["sfr"]["value"] = round(lc["paved"]["sfr"]["value"] + grass_sfr, 6)
    lc["grass"]["sfr"]["value"] = 0.0
    for surface in ("grass", "bsoil"):
        for field in SOIL_FIELDS:
            lc[surface].pop(field, None)

    # ACT
    config = SUEWSConfig.from_dict(data)

    # ASSERT
    for surface in ("grass", "bsoil"):
        surf = getattr(config.sites[0].properties.land_cover, surface)
        assert surf.sfr.value == pytest.approx(0.0)
        for field in SOIL_FIELDS:
            assert getattr(surf, field) is None


def test_water_surface_allows_omitted_soil_params(sample_dict: dict) -> None:
    """Water has no soil column and remains exempt even when sfr == 1.0."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    lc = _land_cover(data)
    for surface in NON_WATER_SURFACES:
        lc[surface]["sfr"] = {"value": 0.0}
        for field in SOIL_FIELDS:
            lc[surface].pop(field, None)
    lc["water"]["sfr"] = {"value": 1.0}
    for field in SOIL_FIELDS:
        lc["water"].pop(field, None)

    # ACT
    config = SUEWSConfig.from_dict(data)

    # ASSERT
    assert config.sites[0].properties.land_cover.water.sfr.value == pytest.approx(1.0)
    for field in SOIL_FIELDS:
        assert getattr(config.sites[0].properties.land_cover.water, field) is None


def test_water_docs_do_not_claim_soil_requirements() -> None:
    """WaterProperties does not advertise soil-parameter requirements."""
    for field in (*SOIL_FIELDS, "soil_field_capacity"):
        assert not required_when("WaterProperties", field)


@pytest.mark.parametrize(
    "source_schema", ["2026.6.dev10", "2026.6.dev9", "2026.6.dev8"]
)
def test_upgrade_writes_former_zero_when_capacity_is_zero_or_absent(
    sample_dict: dict, tmp_path: Path, source_schema: str
) -> None:
    """Zero or omitted capacity allows explicit 0.0 soil params while keeping refs."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    data["schema_version"] = source_schema
    if source_schema == "2026.6.dev8":
        data["model"]["physics"].pop("soil_moisture", None)
        data["model"]["physics"]["soil_moisture_deficit"] = {"value": 0}
    lc = _land_cover(data)
    lc["paved"].pop("soil_depth", None)
    lc["paved"]["soil_store_capacity"] = {
        "value": None,
        "ref": {"desc": "Legacy null placeholder"},
    }
    lc["paved"]["saturated_hydraulic_conductivity"] = {"value": 0.0}
    lc["bldgs"]["soil_store_capacity"] = {"value": 0.0}
    lc["bldgs"]["soil_depth"] = {
        "value": None,
        "ref": {"desc": "Zero-capacity null depth"},
    }
    lc["grass"]["soil_depth"] = {"value": 417.0, "ref": {"desc": "Measured depth"}}
    lc["grass"].pop("saturated_hydraulic_conductivity", None)
    lc["bldgs"]["sfr"]["value"] += lc["bsoil"]["sfr"]["value"]
    lc["bsoil"]["sfr"]["value"] = 0.0
    lc["evetr"]["sfr"] = {"value": 0.0}
    lc["evetr"]["soil_store_capacity"] = {"value": 150.0}
    lc["evetr"].pop("soil_depth", None)
    for surface in ("bsoil", "water"):
        for field in SOIL_FIELDS:
            lc[surface].pop(field, None)

    source = tmp_path / f"{source_schema}.yml"
    output = tmp_path / "upgraded.yml"
    source.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")

    # ACT
    upgrade_yaml(input_path=source, output_path=output)

    # ASSERT
    migrated = yaml.safe_load(output.read_text(encoding="utf-8"))
    assert migrated["schema_version"] == CURRENT_SCHEMA_VERSION
    migrated_lc = _land_cover(migrated)
    assert migrated_lc["paved"]["soil_depth"] == {"value": 0.0}
    assert migrated_lc["paved"]["soil_store_capacity"] == {
        "value": 0.0,
        "ref": {"desc": "Legacy null placeholder"},
    }
    assert migrated_lc["bldgs"]["soil_depth"] == {
        "value": 0.0,
        "ref": {"desc": "Zero-capacity null depth"},
    }
    assert migrated_lc["grass"]["soil_depth"] == {
        "value": 417.0,
        "ref": {"desc": "Measured depth"},
    }
    assert migrated_lc["grass"]["saturated_hydraulic_conductivity"] == {"value": 0.0}
    assert "soil_depth" not in migrated_lc["evetr"]
    for field in SOIL_FIELDS:
        assert field not in migrated_lc["bsoil"]
        assert field not in migrated_lc["water"]

    loaded_lc = SUEWSConfig.from_yaml(str(output)).sites[0].properties.land_cover
    assert loaded_lc.paved.soil_depth.value == pytest.approx(0.0)
    assert loaded_lc.paved.soil_store_capacity.value == pytest.approx(0.0)
    assert loaded_lc.bldgs.soil_depth.value == pytest.approx(0.0)
    assert loaded_lc.bldgs.soil_store_capacity.value == pytest.approx(0.0)
    assert loaded_lc.grass.soil_depth.value == pytest.approx(417.0)
    assert loaded_lc.grass.saturated_hydraulic_conductivity.value == pytest.approx(0.0)
    assert loaded_lc.evetr.soil_depth is None


def _legacy_soil_depth_doc(
    sample_dict: dict,
    *,
    source_schema: str,
    surface: str,
    form: str,
    second_site: bool = False,
) -> tuple[dict, dict]:
    """Build a legacy configuration targeting *surface* on site 0 or site 1."""
    doc = copy.deepcopy(sample_dict)
    doc["schema_version"] = source_schema
    if source_schema == "2026.6.dev8":
        physics = doc["model"]["physics"]
        physics.pop("soil_moisture", None)
        physics["soil_moisture_deficit"] = {"value": 0}
    if second_site:
        other = copy.deepcopy(doc["sites"][0])
        other["name"] = "Independent site B"
        other["gridiv"] = 42
        doc["sites"].append(other)
    site = doc["sites"][-1]
    fields = site["properties"]["land_cover"][surface]
    if form == "missing":
        fields.pop("soil_depth", None)
    elif form == "wrapped-null":
        fields["soil_depth"] = {"value": None}
    elif form == "bare-null":
        fields["soil_depth"] = None
    elif form == "legacy-alias-null":
        fields.pop("soil_depth", None)
        fields.pop("soil_store_capacity", None)
        fields["soildepth"] = {"value": None}
        fields["soilstorecap"] = {"value": 150.0}
    else:
        raise AssertionError(f"Unexpected form {form!r}")
    return doc, site


@pytest.mark.parametrize(
    ("surface", "form", "second_site"),
    [
        ("bldgs", "missing", False),
        ("paved", "wrapped-null", False),
        ("paved", "bare-null", True),
        ("grass", "legacy-alias-null", False),
    ],
)
@pytest.mark.parametrize(
    "source_schema", ["2026.6.dev10", "2026.6.dev9", "2026.6.dev8"]
)
def test_cli_convert_refuses_missing_or_null_soil_depth_on_active_positive_capacity(
    sample_dict: dict,
    tmp_path: Path,
    surface: str,
    form: str,
    second_site: bool,
    source_schema: str,
) -> None:
    """Public ``suews convert`` exits non-zero, writes no file, and reports site/surface."""
    # ARRANGE
    doc, site = _legacy_soil_depth_doc(
        sample_dict,
        source_schema=source_schema,
        surface=surface,
        form=form,
        second_site=second_site,
    )
    source = tmp_path / "old.yml"
    output = tmp_path / "converted.yml"
    source.write_text(yaml.safe_dump(doc, sort_keys=False), encoding="utf-8")

    # ACT
    result = CliRunner().invoke(
        cli,
        ["convert", "-i", str(source), "-o", str(output)],
        terminal_width=250,
        color=False,
    )

    # ASSERT
    assert result.exit_code == 1, result.output
    assert not output.exists()
    msg = " ".join(result.output.split())
    assert site["name"] in msg
    assert str(site["gridiv"]) in msg
    assert surface in msg
    assert "soil_depth" in msg


def test_cli_convert_json_and_migrator_refusal_are_transactional(
    sample_dict: dict, tmp_path: Path
) -> None:
    """JSON CLI, SchemaMigrator and migrate_config_file leave input and target untouched."""
    # ARRANGE
    doc, site = _legacy_soil_depth_doc(
        sample_dict,
        source_schema="2026.6.dev10",
        surface="paved",
        form="missing",
        second_site=True,
    )
    # Also remove soil_depth from bldgs on site 0 to verify multi-site reporting.
    doc["sites"][0]["properties"]["land_cover"]["bldgs"].pop("soil_depth", None)
    before = copy.deepcopy(doc)

    # ACT / ASSERT: in-memory migrator does not mutate caller dict
    with pytest.raises(YamlUpgradeError) as exc_info:
        SchemaMigrator().migrate(doc)
    assert doc == before
    err_msg = str(exc_info.value)
    assert doc["sites"][0]["name"] in err_msg
    assert "bldgs" in err_msg
    assert site["name"] in err_msg
    assert str(site["gridiv"]) in err_msg
    assert "paved" in err_msg
    assert "soil_depth" in err_msg

    # ACT / ASSERT: file helpers and CLI do not overwrite existing output
    source = tmp_path / "old.yml"
    dest = tmp_path / "existing.yml"
    source.write_text(yaml.safe_dump(doc, sort_keys=False), encoding="utf-8")
    dest.write_text("existing output", encoding="utf-8")

    with pytest.raises(YamlUpgradeError):
        migrate_config_file(str(source), str(dest))
    assert dest.read_text(encoding="utf-8") == "existing output"

    try:
        json_runner = CliRunner(mix_stderr=False)
    except TypeError:
        json_runner = CliRunner()
    result_json = json_runner.invoke(
        cli,
        ["convert", "-i", str(source), "-o", str(dest), "--format", "json"],
        terminal_width=250,
        color=False,
    )
    assert result_json.exit_code == 1, result_json.output
    assert dest.read_text(encoding="utf-8") == "existing output"
    envelope = json.loads(result_json.stdout)
    assert envelope["status"] == "error"
    error_text = " ".join(entry["message"] for entry in envelope["errors"])
    assert site["name"] in error_text
    assert str(site["gridiv"]) in error_text
    assert "paved" in error_text
    assert "bldgs" in error_text
    assert "soil_depth" in error_text

    # ACT / ASSERT: suews schema migrate writes no .migrated.yml file and reports the error
    schema_out = source.with_suffix(".migrated.yml")
    result_schema = CliRunner().invoke(
        cli,
        ["schema", "migrate", str(source)],
        terminal_width=250,
        color=False,
    )
    assert not schema_out.exists()
    schema_msg = " ".join(result_schema.output.split())
    assert site["name"] in schema_msg
    assert str(site["gridiv"]) in schema_msg
    assert "paved" in schema_msg
    assert "soil_depth" in schema_msg


@pytest.mark.parametrize("case", ["supplied", "inactive", "water", "zero-capacity"])
def test_cli_convert_positive_controls_and_reloads(
    sample_dict: dict, tmp_path: Path, case: str
) -> None:
    """Supplied soil_depth, inactive, water and zero-capacity cases migrate and reload."""
    # ARRANGE
    doc = copy.deepcopy(sample_dict)
    doc["schema_version"] = "2026.6.dev10"
    lc = _land_cover(doc)
    if case == "supplied":
        lc["paved"]["soil_depth"] = {
            "value": 417.0,
            "ref": {"desc": "Site borehole survey"},
        }
    elif case == "inactive":
        assert lc["evetr"]["sfr"]["value"] == pytest.approx(0.0)
        lc["evetr"]["soil_store_capacity"] = {"value": 150.0}
        lc["evetr"].pop("soil_depth", None)
    elif case == "water":
        lc["water"].pop("soil_depth", None)
    elif case == "zero-capacity":
        lc["paved"]["soil_store_capacity"] = {"value": 0.0}
        lc["paved"].pop("soil_depth", None)

    source = tmp_path / f"{case}.yml"
    output = tmp_path / f"{case}_converted.yml"
    source.write_text(yaml.safe_dump(doc, sort_keys=False), encoding="utf-8")

    # ACT
    result = CliRunner().invoke(
        cli,
        ["convert", "-i", str(source), "-o", str(output)],
        terminal_width=250,
        color=False,
    )

    # ASSERT
    assert result.exit_code == 0, result.output
    assert output.is_file()
    migrated = yaml.safe_load(output.read_text(encoding="utf-8"))
    assert migrated["schema_version"] == CURRENT_SCHEMA_VERSION
    migrated_lc = _land_cover(migrated)
    loaded_lc = SUEWSConfig.from_yaml(str(output)).sites[0].properties.land_cover
    if case == "supplied":
        assert migrated_lc["paved"]["soil_depth"] == {
            "value": 417.0,
            "ref": {"desc": "Site borehole survey"},
        }
        assert loaded_lc.paved.soil_depth.value == pytest.approx(417.0)
    elif case == "inactive":
        assert "soil_depth" not in migrated_lc["evetr"]
        assert loaded_lc.evetr.soil_depth is None
    elif case == "water":
        assert "soil_depth" not in migrated_lc["water"]
        assert loaded_lc.water.soil_depth is None
    elif case == "zero-capacity":
        assert migrated_lc["paved"]["soil_depth"] == {"value": 0.0}
        assert loaded_lc.paved.soil_depth.value == pytest.approx(0.0)
        assert loaded_lc.paved.soil_store_capacity.value == pytest.approx(0.0)


def test_rust_bridge_rejects_missing_soil_param_on_programmatic_config() -> None:
    """Even if a programmatic config drops a soil field, the Rust bridge refuses it."""
    # ARRANGE
    sim = SUEWSSimulation.from_sample_data()
    cfg = sim.config.model_copy(deep=True)
    cfg.sites[0].properties.land_cover.paved.soil_depth = None
    forcing_slice = sim.forcing.df.iloc[:2]

    # ACT / ASSERT
    with pytest.raises(
        RuntimeError,
        match=r"properties\.land_cover\.paved\.soil_depth.*is missing",
    ):
        run_suews_rust(cfg, forcing_slice, grid_id=cfg.sites[0].gridiv)
