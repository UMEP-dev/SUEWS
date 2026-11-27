"""Explicit zero soil_depth is rejected on active positive-capacity non-water surfaces.

Dated software-maintainer policy (2026-11-20), following #2077 / #2080:
horizontal soil water redistribution divides by ``soil_depth`` whenever a
non-water surface is active (``sfr > 0``) and has positive soil storage
capacity (``soil_store_capacity > 0``). Explicit zero ``soil_depth`` under
those conditions must fail across all four public routes:

1. Public config loading (``SUEWSConfig.from_yaml``, ``from_dict``, ``SUEWSConfig(**...)``).
2. Schema migration / conversion prior to any destination write
   (``suews convert``, ``suews schema migrate``, ``SchemaMigrator``,
   ``migrate_config_file``, ``upgrade_yaml``).
3. Post-construction live mutation before or after ``SUEWSSimulation.update_config``
   at ``update_config``, ``sim.run``, ``config_to_engine_yaml``,
   ``_grid_config_jsons``, ``to_df_state``, and Rust runner entry points.
4. Direct resolved native Rust engine (``suews_bridge.run_suews``).

Zero storage capacity, inactive surfaces (``sfr == 0``), ``water``, and
positive supplied ``soil_depth`` values remain permitted.

Cost shape (gh#2349). The sample YAML, resolved engine mapping, and two-step
native Rust forcing block are prepared once per module with libyaml when
available. Live-mutation and direct Rust engine tests reuse
``sample_config_loaded`` and a two-step forcing slice instead of re-parsing the
full-year forcing file. The 12-case ``suews convert`` refusal matrix uses a
compact legacy YAML payload while the transactional multi-API, two-grid,
release-fixture, and positive-control migration tests continue to exercise the
full sample configuration.
"""

from __future__ import annotations

import copy
import json
from pathlib import Path
import warnings

from click.testing import CliRunner
import pytest
import yaml

from supy import SUEWSSimulation
from supy._run_rust import (
    _check_rust_available,
    _grid_config_jsons,
    _prepare_forcing_block,
    _run_grid_into,
    config_to_engine_yaml,
    run_suews_rust,
    run_suews_rust_multi,
)
from supy.cmd.suews_cli import cli
from supy.data_model.configuration import (
    CURRENT_SCHEMA_VERSION,
    SchemaMigrator,
    migrate_config_file,
)
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.type import RefValue
from supy.data_model.validation.required_fields import NON_WATER_SURFACES
from supy.util.converter.yaml_upgrade import YamlUpgradeError, upgrade_yaml

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-STRICT", "C-MIGRATE"),
]

_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)
_HANDOFF_STEPS = 2


def _yaml_load(text: str):
    return yaml.load(text, Loader=_YAML_LOADER)


def _yaml_dump(payload: dict) -> str:
    return yaml.dump(payload, Dumper=_YAML_DUMPER, sort_keys=False)


@pytest.fixture(scope="module")
def sample_dict(sample_yaml_path: Path) -> dict:
    """Return a parsed copy of the shipped sample configuration. READ-ONLY."""
    return _yaml_load(sample_yaml_path.read_text(encoding="utf-8"))


@pytest.fixture(scope="module")
def resolved_sample_dict(sample_config_loaded: SUEWSConfig) -> dict:
    """Parsed resolved engine YAML for the sample config. READ-ONLY."""
    return _yaml_load(config_to_engine_yaml(sample_config_loaded))


@pytest.fixture(scope="module")
def short_forcing(sample_forcing_parsed):
    """Two forcing timesteps. READ-ONLY: tests take ``.copy()``."""
    _, forcing = sample_forcing_parsed(_HANDOFF_STEPS)
    return forcing


@pytest.fixture(scope="module")
def rust_engine_context(sample_config_loaded: SUEWSConfig, short_forcing):
    """Prepared native Rust module and two-step forcing block. READ-ONLY."""
    rust_module = _check_rust_available()
    forcing_block = _prepare_forcing_block(
        short_forcing.copy(),
        soil_moisture_method=int(
            sample_config_loaded.model.physics.soil_moisture.value.value
        ),
    )
    return rust_module, forcing_block, len(short_forcing)


def _sim_from_sample(
    sample_config_loaded: SUEWSConfig, short_forcing
) -> SUEWSSimulation:
    """Build a fresh ``SUEWSSimulation`` from cached sample config and two-step forcing."""
    sim = SUEWSSimulation()
    sim.update_config(
        sample_config_loaded.model_copy(deep=True), auto_load_forcing=False
    )
    sim.update_forcing(short_forcing.copy())
    return sim


def _land_cover(data: dict, site_index: int = 0) -> dict:
    return data["sites"][site_index]["properties"]["land_cover"]


def _activate_surface(lc: dict, surface: str) -> None:
    """Ensure *surface* has positive ``sfr`` while preserving ``sum(sfr) == 1``."""
    raw = lc[surface]["sfr"]
    sfr = raw["value"] if isinstance(raw, dict) else raw
    if sfr <= 0.0:
        paved_raw = lc["paved"]["sfr"]
        paved_sfr = paved_raw["value"] if isinstance(paved_raw, dict) else paved_raw
        lc["paved"]["sfr"] = {"value": round(paved_sfr - 0.05, 6)}
        lc[surface]["sfr"] = {"value": 0.05}


def _with_zero_soil_depth(
    sample_dict: dict,
    surface: str,
    depth_value: object,
    *,
    legacy_keys: bool = False,
    capacity: float = 150.0,
) -> dict:
    """Return a sample copy with *surface* active, positive capacity and zero depth."""
    data = copy.deepcopy(sample_dict)
    lc = _land_cover(data)
    _activate_surface(lc, surface)
    surf = lc[surface]
    surf.pop("soil_depth", None)
    surf.pop("soildepth", None)
    surf.pop("soil_store_capacity", None)
    surf.pop("soilstorecap", None)
    if legacy_keys:
        surf["soildepth"] = depth_value
        surf["soilstorecap"] = {"value": capacity}
    else:
        surf["soil_depth"] = depth_value
        surf["soil_store_capacity"] = {"value": capacity}
    return data


def _separate_stream_runner() -> CliRunner:
    """Return a Click CliRunner with separated stdout and stderr across Click versions."""
    try:
        return CliRunner(mix_stderr=False)
    except TypeError:
        return CliRunner()


def _normalise_rich_table_text(output: str) -> str:
    """Join wrapped Rich table cell text regardless of terminal width or box glyphs."""
    parts: list[str] = []
    for line in output.splitlines():
        cleaned = "".join(
            " " if (ch in "|+" or "\u2500" <= ch <= "\u257f") else ch for ch in line
        ).strip()
        if cleaned and not set(cleaned) <= {"-", "="}:
            parts.append(cleaned)
    return " ".join(" ".join(parts).split())


def _assert_zero_depth_error_tokens(
    message: str,
    *,
    site_index: int,
    site_name: str,
    gridiv: int,
    surface: str,
) -> None:
    """Assert that *message* identifies the site index, name, gridiv, surface and path."""
    assert f"sites[{site_index}]" in message, message
    assert site_name in message, message
    assert f"gridiv={gridiv}" in message, message
    assert f"properties.land_cover.{surface}.soil_depth" in message, message


# ---------------------------------------------------------------------------
# Route (a): Public config loading (from_yaml, from_dict, SUEWSConfig(**...))
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("surface", NON_WATER_SURFACES)
@pytest.mark.parametrize(
    "depth_literal",
    [
        0,
        0.0,
        -0.0,
        {"value": 0},
        {"value": 0.0},
        {"value": 0.0, "ref": {"desc": "Explicit zero"}},
    ],
)
def test_from_dict_rejects_zero_soil_depth_on_all_active_non_water_surfaces(
    sample_dict: dict, surface: str, depth_literal: object
) -> None:
    """All 6 active non-water surfaces reject explicit zero soil_depth with positive capacity."""
    # ARRANGE
    data = _with_zero_soil_depth(sample_dict, surface, depth_literal)
    site = data["sites"][0]

    # ACT / ASSERT
    with pytest.raises(ValueError) as exc_info:
        SUEWSConfig.from_dict(data)
    _assert_zero_depth_error_tokens(
        str(exc_info.value),
        site_index=0,
        site_name=site["name"],
        gridiv=site["gridiv"],
        surface=surface,
    )


@pytest.mark.parametrize("legacy_keys", [False, True])
def test_public_loading_routes_reject_zero_soil_depth(
    sample_dict: dict, tmp_path: Path, legacy_keys: bool
) -> None:
    """from_yaml, from_dict and SUEWSConfig(**...) reject canonical and legacy zero soil_depth."""
    # ARRANGE
    data = _with_zero_soil_depth(
        sample_dict,
        "grass",
        {"value": 0.0},
        legacy_keys=legacy_keys,
    )
    site = data["sites"][0]
    yaml_path = tmp_path / ("legacy_zero.yml" if legacy_keys else "canonical_zero.yml")
    yaml_path.write_text(_yaml_dump(data), encoding="utf-8")

    # ACT / ASSERT: from_yaml (including auto_generate_annotated=True)
    with warnings.catch_warnings():
        if legacy_keys:
            warnings.simplefilter("ignore", DeprecationWarning)
        with pytest.raises(ValueError) as yaml_exc:
            SUEWSConfig.from_yaml(str(yaml_path), auto_generate_annotated=True)
    _assert_zero_depth_error_tokens(
        str(yaml_exc.value),
        site_index=0,
        site_name=site["name"],
        gridiv=site["gridiv"],
        surface="grass",
    )
    annotated = yaml_path.with_name(f"{yaml_path.stem}_annotated.yml")
    assert annotated.is_file()
    assert "soil_depth" in annotated.read_text(encoding="utf-8")

    # ACT / ASSERT: from_dict
    with warnings.catch_warnings():
        if legacy_keys:
            warnings.simplefilter("ignore", DeprecationWarning)
        with pytest.raises(ValueError) as dict_exc:
            SUEWSConfig.from_dict(copy.deepcopy(data))
    _assert_zero_depth_error_tokens(
        str(dict_exc.value),
        site_index=0,
        site_name=site["name"],
        gridiv=site["gridiv"],
        surface="grass",
    )

    # ACT / ASSERT: direct SUEWSConfig(**...)
    with warnings.catch_warnings():
        if legacy_keys:
            warnings.simplefilter("ignore", DeprecationWarning)
        with pytest.raises(ValueError) as ctor_exc:
            SUEWSConfig(**copy.deepcopy(data))
    _assert_zero_depth_error_tokens(
        str(ctor_exc.value),
        site_index=0,
        site_name=site["name"],
        gridiv=site["gridiv"],
        surface="grass",
    )


def test_direct_constructor_with_model_instances_rejects_zero_soil_depth(
    sample_config_loaded: SUEWSConfig,
) -> None:
    """SUEWSConfig(model=..., sites=[Site(...)]) rejects zero soil_depth without _yaml_raw."""
    # ARRANGE
    site = sample_config_loaded.sites[0].model_copy(deep=True)
    site.name = "DirectSite"
    site.gridiv = 77
    site.properties.land_cover.bldgs.soil_depth = RefValue(0.0)

    # ACT / ASSERT
    with pytest.raises(ValueError) as exc_info:
        SUEWSConfig(
            model=sample_config_loaded.model.model_copy(deep=True), sites=[site]
        )
    _assert_zero_depth_error_tokens(
        str(exc_info.value),
        site_index=0,
        site_name="DirectSite",
        gridiv=77,
        surface="bldgs",
    )


def test_loading_positive_controls_preserve_valid_zero_and_positive_cases(
    sample_dict: dict,
) -> None:
    """Zero capacity, inactive surfaces, water and positive depths all load cleanly."""
    # Zero capacity on active non-water surface
    zero_cap = _with_zero_soil_depth(sample_dict, "paved", {"value": 0.0}, capacity=0.0)
    cfg_zero_cap = SUEWSConfig.from_dict(zero_cap)
    assert cfg_zero_cap.sites[
        0
    ].properties.land_cover.paved.soil_depth.value == pytest.approx(0.0)
    assert cfg_zero_cap.sites[
        0
    ].properties.land_cover.paved.soil_store_capacity.value == pytest.approx(0.0)

    # Inactive non-water surface with positive capacity and zero soil_depth
    inactive = copy.deepcopy(sample_dict)
    lc_inactive = _land_cover(inactive)
    assert lc_inactive["evetr"]["sfr"]["value"] == pytest.approx(0.0)
    lc_inactive["evetr"]["soil_store_capacity"] = {"value": 150.0}
    lc_inactive["evetr"]["soil_depth"] = {"value": 0.0}
    cfg_inactive = SUEWSConfig.from_dict(inactive)
    assert cfg_inactive.sites[
        0
    ].properties.land_cover.evetr.soil_depth.value == pytest.approx(0.0)

    # Water surface with sfr == 1.0, positive capacity and zero soil_depth
    water_only = copy.deepcopy(sample_dict)
    lc_water = _land_cover(water_only)
    for surface in NON_WATER_SURFACES:
        lc_water[surface]["sfr"] = {"value": 0.0}
    lc_water["water"]["sfr"] = {"value": 1.0}
    lc_water["water"]["soil_store_capacity"] = {"value": 150.0}
    lc_water["water"]["soil_depth"] = {"value": 0.0}
    cfg_water = SUEWSConfig.from_dict(water_only)
    assert cfg_water.sites[
        0
    ].properties.land_cover.water.soil_depth.value == pytest.approx(0.0)

    # Positive supplied soil_depth
    positive = _with_zero_soil_depth(
        sample_dict, "dectr", {"value": 417.0}, capacity=150.0
    )
    cfg_pos = SUEWSConfig.from_dict(positive)
    assert cfg_pos.sites[
        0
    ].properties.land_cover.dectr.soil_depth.value == pytest.approx(417.0)


# ---------------------------------------------------------------------------
# Route (b): Transactional migration / conversion prior to destination write
# ---------------------------------------------------------------------------


def _legacy_zero_soil_depth_doc(
    sample_dict: dict,
    *,
    source_schema: str,
    surface: str,
    form: str,
    second_site: bool = False,
) -> tuple[dict, dict]:
    """Build a legacy configuration with explicit zero soil_depth on site 0 or 1."""
    doc = copy.deepcopy(sample_dict)
    doc["schema_version"] = source_schema
    if source_schema == "2026.6.dev8":
        physics = doc["model"]["physics"]
        physics.pop("soil_moisture", None)
        physics["soil_moisture_deficit"] = {"value": 0}
    if second_site:
        other = copy.deepcopy(doc["sites"][0])
        other["name"] = "ZeroDepthSiteB"
        other["gridiv"] = 42
        doc["sites"].append(other)
    site = doc["sites"][-1]
    lc = site["properties"]["land_cover"]
    _activate_surface(lc, surface)
    fields = lc[surface]
    fields.pop("soil_depth", None)
    fields.pop("soildepth", None)
    if form == "bare-zero":
        fields["soil_depth"] = 0
        fields["soil_store_capacity"] = {"value": 150.0}
    elif form == "wrapped-zero":
        fields["soil_depth"] = {"value": 0.0}
        fields["soil_store_capacity"] = {"value": 150.0}
    elif form == "legacy-alias-bare-zero":
        fields.pop("soil_store_capacity", None)
        fields["soildepth"] = 0.0
        fields["soilstorecap"] = 150.0
    elif form == "legacy-alias-wrapped-zero":
        fields.pop("soil_store_capacity", None)
        fields["soildepth"] = {"value": 0.0}
        fields["soilstorecap"] = {"value": 150.0}
    else:
        raise AssertionError(f"Unexpected form {form!r}")
    return doc, site


def _compact_legacy_zero_soil_depth_doc(
    *,
    source_schema: str,
    surface: str,
    form: str,
    second_site: bool = False,
) -> tuple[dict, dict]:
    """Build a compact legacy config with explicit zero soil_depth on site 0 or 1."""

    def _make_site(name: str, gridiv: int) -> dict:
        lc: dict[str, dict] = {
            "paved": {
                "sfr": {"value": 0.45},
                "soil_depth": {"value": 350.0},
                "soil_store_capacity": {"value": 150.0},
                "saturated_hydraulic_conductivity": {"value": 0.0005},
            },
            "bldgs": {
                "sfr": {"value": 0.35},
                "soil_depth": {"value": 350.0},
                "soil_store_capacity": {"value": 150.0},
                "saturated_hydraulic_conductivity": {"value": 0.0005},
            },
            "evetr": {
                "sfr": {"value": 0.0},
                "soil_depth": {"value": 350.0},
                "soil_store_capacity": {"value": 150.0},
                "saturated_hydraulic_conductivity": {"value": 0.0005},
            },
            "dectr": {
                "sfr": {"value": 0.05},
                "soil_depth": {"value": 350.0},
                "soil_store_capacity": {"value": 150.0},
                "saturated_hydraulic_conductivity": {"value": 0.0005},
            },
            "grass": {
                "sfr": {"value": 0.15},
                "soil_depth": {"value": 350.0},
                "soil_store_capacity": {"value": 150.0},
                "saturated_hydraulic_conductivity": {"value": 0.0005},
            },
            "bsoil": {
                "sfr": {"value": 0.0},
                "soil_depth": {"value": 350.0},
                "soil_store_capacity": {"value": 150.0},
                "saturated_hydraulic_conductivity": {"value": 0.0005},
            },
            "water": {"sfr": {"value": 0.0}},
        }
        return {"name": name, "gridiv": gridiv, "properties": {"land_cover": lc}}

    physics: dict[str, dict] = (
        {"soil_moisture_deficit": {"value": 0}}
        if source_schema == "2026.6.dev8"
        else {"soil_moisture": {"value": 0}}
    )
    sites = [_make_site("ZeroDepthSiteA", 1)]
    if second_site:
        sites.append(_make_site("ZeroDepthSiteB", 42))
    doc = {
        "name": "compact_zero_soil_depth",
        "schema_version": source_schema,
        "model": {"physics": physics},
        "sites": sites,
    }
    site = doc["sites"][-1]
    lc = site["properties"]["land_cover"]
    _activate_surface(lc, surface)
    fields = lc[surface]
    fields.pop("soil_depth", None)
    fields.pop("soildepth", None)
    if form == "bare-zero":
        fields["soil_depth"] = 0
        fields["soil_store_capacity"] = {"value": 150.0}
    elif form == "wrapped-zero":
        fields["soil_depth"] = {"value": 0.0}
        fields["soil_store_capacity"] = {"value": 150.0}
    elif form == "legacy-alias-bare-zero":
        fields.pop("soil_store_capacity", None)
        fields["soildepth"] = 0.0
        fields["soilstorecap"] = 150.0
    elif form == "legacy-alias-wrapped-zero":
        fields.pop("soil_store_capacity", None)
        fields["soildepth"] = {"value": 0.0}
        fields["soilstorecap"] = {"value": 150.0}
    else:
        raise AssertionError(f"Unexpected form {form!r}")
    return doc, site


@pytest.mark.parametrize(
    ("surface", "form", "second_site"),
    [
        ("paved", "bare-zero", False),
        ("bldgs", "wrapped-zero", True),
        ("evetr", "legacy-alias-bare-zero", False),
        ("grass", "legacy-alias-wrapped-zero", False),
    ],
)
@pytest.mark.parametrize(
    "source_schema", ["2026.6.dev10", "2026.6.dev9", "2026.6.dev8"]
)
def test_cli_convert_refuses_zero_soil_depth_without_writing_output(
    tmp_path: Path,
    surface: str,
    form: str,
    second_site: bool,
    source_schema: str,
) -> None:
    """suews convert exits 1, writes no file, and reports site/surface/soil_depth."""
    # ARRANGE
    doc, site = _compact_legacy_zero_soil_depth_doc(
        source_schema=source_schema,
        surface=surface,
        form=form,
        second_site=second_site,
    )
    source = tmp_path / "old_zero.yml"
    output = tmp_path / "converted.yml"
    source.write_text(_yaml_dump(doc), encoding="utf-8")

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
    site_idx = 1 if second_site else 0
    _assert_zero_depth_error_tokens(
        msg,
        site_index=site_idx,
        site_name=site["name"],
        gridiv=site["gridiv"],
        surface=surface,
    )


def test_release_2026_4_old_names_zero_soildepth_refused_before_write(
    tmp_path: Path,
) -> None:
    """Upgrading a pre-2026.5 YAML with legacy soildepth: 0 fails before writing output."""
    # ARRANGE
    fixture_2026_4 = (
        Path(__file__).resolve().parents[1]
        / "fixtures"
        / "release_configs"
        / "2026.4.3.yml"
    )
    doc = _yaml_load(fixture_2026_4.read_text(encoding="utf-8"))
    site = doc["sites"][0]
    paved = site["properties"]["land_cover"]["paved"]
    assert "soildepth" in paved and "soilstorecap" in paved
    paved["soildepth"] = {"value": 0.0}
    paved["soilstorecap"] = {"value": 150.0}

    source = tmp_path / "legacy_2026_4.yml"
    output = tmp_path / "upgraded_2026_4.yml"
    source.write_text(_yaml_dump(doc), encoding="utf-8")

    # ACT
    res = CliRunner().invoke(
        cli,
        ["convert", "-i", str(source), "-o", str(output), "-f", "2026.4.3"],
        terminal_width=250,
        color=False,
    )

    # ASSERT
    assert res.exit_code == 1, res.output
    assert not output.exists()
    msg = " ".join(res.output.split())
    _assert_zero_depth_error_tokens(
        msg,
        site_index=0,
        site_name=site["name"],
        gridiv=site["gridiv"],
        surface="paved",
    )


def test_current_schema_convert_and_migrator_refuse_zero_soil_depth(
    sample_dict: dict, tmp_path: Path
) -> None:
    """Even at CURRENT_SCHEMA_VERSION, convert/upgrade_yaml/SchemaMigrator/migrate_config_file refuse zero soil_depth."""
    # ARRANGE
    doc = _with_zero_soil_depth(
        sample_dict, "bsoil", {"value": 0.0}, legacy_keys=True, capacity=150.0
    )
    doc["schema_version"] = CURRENT_SCHEMA_VERSION
    before = copy.deepcopy(doc)
    source = tmp_path / "current_zero.yml"
    dest = tmp_path / "existing.yml"
    source.write_text(_yaml_dump(doc), encoding="utf-8")
    dest.write_text("existing output", encoding="utf-8")

    # ACT / ASSERT: SchemaMigrator
    with pytest.raises(YamlUpgradeError) as mig_exc:
        SchemaMigrator().migrate(doc)
    assert doc == before
    _assert_zero_depth_error_tokens(
        str(mig_exc.value),
        site_index=0,
        site_name=doc["sites"][0]["name"],
        gridiv=doc["sites"][0]["gridiv"],
        surface="bsoil",
    )

    # ACT / ASSERT: upgrade_yaml and migrate_config_file leave existing destination untouched
    with pytest.raises(YamlUpgradeError):
        upgrade_yaml(source, dest)
    assert dest.read_text(encoding="utf-8") == "existing output"

    with pytest.raises(YamlUpgradeError):
        migrate_config_file(str(source), str(dest))
    assert dest.read_text(encoding="utf-8") == "existing output"

    # ACT / ASSERT: suews convert CLI exits 1 without overwriting destination
    res = CliRunner().invoke(
        cli,
        ["convert", "-i", str(source), "-o", str(dest)],
        terminal_width=250,
        color=False,
    )
    assert res.exit_code == 1, res.output
    assert dest.read_text(encoding="utf-8") == "existing output"


def test_migration_refusal_is_transactional_across_all_migration_apis(
    sample_dict: dict, tmp_path: Path
) -> None:
    """SchemaMigrator, migrate_config_file, convert JSON, and schema migrate are transactional."""
    # ARRANGE
    doc, site1 = _legacy_zero_soil_depth_doc(
        sample_dict,
        source_schema="2026.6.dev10",
        surface="paved",
        form="legacy-alias-wrapped-zero",
        second_site=True,
    )
    site0 = doc["sites"][0]
    site0["properties"]["land_cover"]["bldgs"]["soil_depth"] = {"value": 0.0}
    before = copy.deepcopy(doc)

    # ACT / ASSERT: SchemaMigrator does not mutate caller dict and reports both sites
    with pytest.raises(YamlUpgradeError) as exc_info:
        SchemaMigrator().migrate(doc)
    assert doc == before
    err_msg = str(exc_info.value)
    _assert_zero_depth_error_tokens(
        err_msg,
        site_index=0,
        site_name=site0["name"],
        gridiv=site0["gridiv"],
        surface="bldgs",
    )
    _assert_zero_depth_error_tokens(
        err_msg,
        site_index=1,
        site_name=site1["name"],
        gridiv=site1["gridiv"],
        surface="paved",
    )

    source = tmp_path / "old_multi.yml"
    dest = tmp_path / "existing.yml"
    source.write_text(_yaml_dump(doc), encoding="utf-8")
    dest.write_text("existing output", encoding="utf-8")

    with pytest.raises(YamlUpgradeError):
        migrate_config_file(str(source), str(dest))
    assert dest.read_text(encoding="utf-8") == "existing output"

    # suews convert --format json
    res_json = _separate_stream_runner().invoke(
        cli,
        ["convert", "-i", str(source), "-o", str(dest), "--format", "json"],
        terminal_width=80,
        color=False,
    )
    assert res_json.exit_code == 1, res_json.output
    assert dest.read_text(encoding="utf-8") == "existing output"
    envelope = json.loads(res_json.stdout)
    assert envelope["status"] == "error"
    json_msg = " ".join(entry["message"] for entry in envelope["errors"])
    _assert_zero_depth_error_tokens(
        json_msg,
        site_index=0,
        site_name=site0["name"],
        gridiv=site0["gridiv"],
        surface="bldgs",
    )
    _assert_zero_depth_error_tokens(
        json_msg,
        site_index=1,
        site_name=site1["name"],
        gridiv=site1["gridiv"],
        surface="paved",
    )

    # suews schema --quiet migrate
    schema_out = source.with_suffix(".migrated.yml")
    res_quiet = _separate_stream_runner().invoke(
        cli,
        ["schema", "--quiet", "migrate", str(source)],
        env={"COLUMNS": "80"},
        terminal_width=80,
        color=False,
    )
    assert res_quiet.exit_code == 1, res_quiet.output
    assert not schema_out.exists()
    assert not res_quiet.stdout
    quiet_msg = " ".join(res_quiet.stderr.split())
    _assert_zero_depth_error_tokens(
        quiet_msg,
        site_index=1,
        site_name=site1["name"],
        gridiv=site1["gridiv"],
        surface="paved",
    )

    # suews schema migrate (narrow table)
    res_table = CliRunner().invoke(
        cli,
        ["schema", "migrate", str(source)],
        env={"COLUMNS": "80"},
        terminal_width=80,
        color=False,
    )
    assert res_table.exit_code == 1, res_table.output
    assert not schema_out.exists()
    table_msg = _normalise_rich_table_text(res_table.output)
    _assert_zero_depth_error_tokens(
        table_msg,
        site_index=1,
        site_name=site1["name"],
        gridiv=site1["gridiv"],
        surface="paved",
    )


def _build_two_grid_zero_depth_doc(
    sample_dict: dict, *, active_first: bool, fix_active_depth: bool
) -> dict:
    """Build a two-grid dev10 config pairing inactive grass (depth 0) with active grass."""
    doc = copy.deepcopy(sample_dict)
    doc["schema_version"] = "2026.6.dev10"

    inactive_site = copy.deepcopy(doc["sites"][0])
    inactive_site["name"] = "GridInactive"
    inactive_site["gridiv"] = 101
    inactive_lc = inactive_site["properties"]["land_cover"]
    grass_sfr = inactive_lc["grass"]["sfr"]["value"]
    inactive_lc["paved"]["sfr"]["value"] = round(
        inactive_lc["paved"]["sfr"]["value"] + grass_sfr, 6
    )
    inactive_lc["grass"]["sfr"]["value"] = 0.0
    inactive_lc["grass"]["soil_store_capacity"] = {"value": 150.0}
    inactive_lc["grass"]["soil_depth"] = {"value": 0.0}

    active_site = copy.deepcopy(doc["sites"][0])
    active_site["name"] = "GridActive"
    active_site["gridiv"] = 202
    active_lc = active_site["properties"]["land_cover"]
    assert active_lc["grass"]["sfr"]["value"] > 0.0
    assert active_lc["grass"]["soil_store_capacity"]["value"] > 0.0
    active_lc["grass"]["soil_depth"] = {"value": 380.0 if fix_active_depth else 0.0}

    doc["sites"] = (
        [active_site, inactive_site] if active_first else [inactive_site, active_site]
    )
    return doc


@pytest.mark.parametrize("active_first", [False, True])
def test_two_grid_zero_depth_migration_both_orders(
    sample_dict: dict, tmp_path: Path, active_first: bool
) -> None:
    """Two-grid migration blames only active grid with zero soil_depth in both orders."""
    # ARRANGE (error control)
    broken_doc = _build_two_grid_zero_depth_doc(
        sample_dict, active_first=active_first, fix_active_depth=False
    )
    broken_in = tmp_path / "two_grid_zero.yml"
    broken_out = tmp_path / "two_grid_zero_out.yml"
    schema_out = broken_in.with_suffix(".migrated.yml")
    broken_in.write_text(_yaml_dump(broken_doc), encoding="utf-8")

    # ACT / ASSERT: convert & schema migrate refuse and blame only GridActive
    res_convert = CliRunner().invoke(
        cli,
        ["convert", "-i", str(broken_in), "-o", str(broken_out)],
        env={"COLUMNS": "80"},
        terminal_width=80,
        color=False,
    )
    assert res_convert.exit_code == 1, res_convert.output
    assert not broken_out.exists()

    res_schema = _separate_stream_runner().invoke(
        cli,
        ["schema", "--quiet", "migrate", str(broken_in)],
        env={"COLUMNS": "80"},
        terminal_width=80,
        color=False,
    )
    assert res_schema.exit_code == 1, res_schema.output
    assert not schema_out.exists()

    active_idx, inactive_idx = (0, 1) if active_first else (1, 0)
    for msg in (
        " ".join(res_convert.output.split()),
        " ".join(res_schema.stderr.split()),
    ):
        _assert_zero_depth_error_tokens(
            msg,
            site_index=active_idx,
            site_name="GridActive",
            gridiv=202,
            surface="grass",
        )
        assert "GridInactive" not in msg
        assert "gridiv=101" not in msg

    # ARRANGE / ACT / ASSERT (positive control): fix GridActive, keep GridInactive at 0.0
    fixed_doc = _build_two_grid_zero_depth_doc(
        sample_dict, active_first=active_first, fix_active_depth=True
    )
    fixed_in = tmp_path / "two_grid_fixed.yml"
    fixed_out = tmp_path / "two_grid_fixed_out.yml"
    fixed_in.write_text(_yaml_dump(fixed_doc), encoding="utf-8")

    ok_res = CliRunner().invoke(
        cli,
        ["convert", "-i", str(fixed_in), "-o", str(fixed_out)],
        terminal_width=250,
        color=False,
    )
    assert ok_res.exit_code == 0, ok_res.output
    assert fixed_out.is_file()
    loaded = SUEWSConfig.from_yaml(str(fixed_out))
    assert loaded.sites[
        active_idx
    ].properties.land_cover.grass.soil_depth.value == pytest.approx(380.0)
    assert loaded.sites[
        inactive_idx
    ].properties.land_cover.grass.soil_depth.value == pytest.approx(0.0)


@pytest.mark.parametrize("case", ["zero-capacity", "inactive", "water", "positive"])
def test_cli_convert_positive_controls_with_zero_or_positive_depth(
    sample_dict: dict, tmp_path: Path, case: str
) -> None:
    """Zero capacity, inactive, water and positive depths migrate and reload cleanly."""
    # ARRANGE
    doc = copy.deepcopy(sample_dict)
    doc["schema_version"] = "2026.6.dev10"
    lc = _land_cover(doc)
    if case == "zero-capacity":
        lc["paved"]["soil_store_capacity"] = {"value": 0.0}
        lc["paved"]["soil_depth"] = {"value": 0.0}
    elif case == "inactive":
        lc["evetr"]["sfr"] = {"value": 0.0}
        lc["evetr"]["soil_store_capacity"] = {"value": 150.0}
        lc["evetr"]["soil_depth"] = {"value": 0.0}
    elif case == "water":
        lc["water"]["soil_store_capacity"] = {"value": 150.0}
        lc["water"]["soil_depth"] = {"value": 0.0}
    elif case == "positive":
        lc["paved"]["soil_depth"] = {"value": 417.0, "ref": {"desc": "Borehole"}}

    source = tmp_path / f"{case}.yml"
    output = tmp_path / f"{case}_out.yml"
    source.write_text(_yaml_dump(doc), encoding="utf-8")

    # ACT
    res = CliRunner().invoke(
        cli,
        ["convert", "-i", str(source), "-o", str(output)],
        terminal_width=250,
        color=False,
    )

    # ASSERT
    assert res.exit_code == 0, res.output
    assert output.is_file()
    loaded = SUEWSConfig.from_yaml(str(output))
    loaded_lc = loaded.sites[0].properties.land_cover
    if case == "zero-capacity":
        assert loaded_lc.paved.soil_depth.value == pytest.approx(0.0)
        assert loaded_lc.paved.soil_store_capacity.value == pytest.approx(0.0)
    elif case == "inactive":
        assert loaded_lc.evetr.soil_depth.value == pytest.approx(0.0)
    elif case == "water":
        assert loaded_lc.water.soil_depth.value == pytest.approx(0.0)
    elif case == "positive":
        assert loaded_lc.paved.soil_depth.value == pytest.approx(417.0)


# ---------------------------------------------------------------------------
# Route (c): Post-construction live mutation at execution / export
# ---------------------------------------------------------------------------


def _apply_live_zero_mutation(surface: object, mode: str) -> None:
    """Mutate *surface*.soil_depth to zero using *mode*."""
    if mode == "inplace-value":
        surface.soil_depth.value = 0.0
    elif mode == "refvalue":
        surface.soil_depth = RefValue(0.0)
    elif mode == "scalar":
        surface.soil_depth = 0.0
    elif mode == "dict":
        surface.soil_depth = {"value": 0.0}
    else:
        raise AssertionError(f"Unknown mutation mode {mode!r}")


@pytest.mark.parametrize("mode", ["inplace-value", "refvalue", "scalar", "dict"])
def test_live_mutation_before_update_config_rejected_atomically(
    sample_config_loaded: SUEWSConfig, mode: str
) -> None:
    """Mutating soil_depth to 0 before update_config fails without replacing existing config."""
    # ARRANGE
    sim = SUEWSSimulation()
    original_cfg = sample_config_loaded.model_copy(deep=True)
    sim.update_config(original_cfg, auto_load_forcing=False)

    mutated_cfg = original_cfg.model_copy(deep=True)
    _apply_live_zero_mutation(mutated_cfg.sites[0].properties.land_cover.paved, mode)

    # ACT / ASSERT
    with pytest.raises(ValueError) as exc_info:
        sim.update_config(mutated_cfg, auto_load_forcing=False)
    _assert_zero_depth_error_tokens(
        str(exc_info.value),
        site_index=0,
        site_name=original_cfg.sites[0].name,
        gridiv=original_cfg.sites[0].gridiv,
        surface="paved",
    )
    # Existing simulation config stays intact on refusal.
    assert sim.config is original_cfg


def test_partial_dict_update_config_rejects_zero_soil_depth_atomically(
    sample_config_loaded: SUEWSConfig,
) -> None:
    """Partial dict update setting soil_depth to 0 fails without mutating sim.config."""
    # ARRANGE
    sim = SUEWSSimulation()
    original_cfg = sample_config_loaded.model_copy(deep=True)
    sim.update_config(original_cfg, auto_load_forcing=False)
    original_depth = sim.config.sites[0].properties.land_cover.paved.soil_depth.value

    # ACT / ASSERT
    with pytest.raises(ValueError) as exc_info:
        sim.update_config(
            {
                "sites": {
                    0: {"properties": {"land_cover": {"paved": {"soil_depth": 0.0}}}}
                }
            },
            auto_load_forcing=False,
        )
    _assert_zero_depth_error_tokens(
        str(exc_info.value),
        site_index=0,
        site_name=original_cfg.sites[0].name,
        gridiv=original_cfg.sites[0].gridiv,
        surface="paved",
    )
    assert sim.config.sites[
        0
    ].properties.land_cover.paved.soil_depth.value == pytest.approx(original_depth)


@pytest.mark.parametrize("mode", ["inplace-value", "refvalue", "scalar", "dict"])
def test_live_mutation_after_update_config_rejected_at_all_handoffs(
    sample_config_loaded: SUEWSConfig, short_forcing, mode: str
) -> None:
    """Mutating sim.config soil_depth to 0 fails at run, export, grid JSON, to_df_state and runners."""
    # ARRANGE
    sim = _sim_from_sample(sample_config_loaded, short_forcing)
    site = sim.config.sites[0]
    forcing_slice = sim.forcing.df
    _apply_live_zero_mutation(site.properties.land_cover.bldgs, mode)

    # ACT / ASSERT: every handoff fails cleanly with ValueError before the engine
    handoffs = [
        lambda: sim.run(end_date=forcing_slice.index[-1]),
        lambda: config_to_engine_yaml(sim.config),
        lambda: _grid_config_jsons(sim.config),
        sim.config.to_df_state,
        lambda: run_suews_rust(sim.config, forcing_slice, grid_id=site.gridiv),
        lambda: run_suews_rust_multi(sim.config, forcing_slice),
    ]
    for call in handoffs:
        with pytest.raises(ValueError) as exc_info:
            call()
        _assert_zero_depth_error_tokens(
            str(exc_info.value),
            site_index=0,
            site_name=site.name,
            gridiv=site.gridiv,
            surface="bldgs",
        )


def test_cross_field_live_mutations_to_active_or_positive_capacity_rejected(
    sample_dict: dict,
) -> None:
    """Activating sfr or raising soil_store_capacity above 0 when soil_depth is 0 fails."""
    # Case 1: start with zero-capacity active paved (soil_depth == 0.0), then raise capacity
    zero_cap_data = _with_zero_soil_depth(
        sample_dict, "paved", {"value": 0.0}, capacity=0.0
    )
    cfg1 = SUEWSConfig.from_dict(zero_cap_data)
    cfg1.sites[0].properties.land_cover.paved.soil_store_capacity.value = 150.0
    for call in (
        cfg1.to_df_state,
        lambda: config_to_engine_yaml(cfg1),
        lambda: _grid_config_jsons(cfg1),
    ):
        with pytest.raises(
            ValueError, match=r"properties\.land_cover\.paved\.soil_depth"
        ):
            call()

    # Case 2: start with inactive evetr (sfr == 0, soil_depth == 0.0, capacity == 150), then activate
    inactive_data = copy.deepcopy(sample_dict)
    lc = _land_cover(inactive_data)
    lc["evetr"]["sfr"] = {"value": 0.0}
    lc["evetr"]["soil_store_capacity"] = {"value": 150.0}
    lc["evetr"]["soil_depth"] = {"value": 0.0}
    cfg2 = SUEWSConfig.from_dict(inactive_data)
    lc2 = cfg2.sites[0].properties.land_cover
    lc2.paved.sfr.value = round(lc2.paved.sfr.value - 0.05, 6)
    lc2.evetr.sfr.value = 0.05
    for call in (
        cfg2.to_df_state,
        lambda: config_to_engine_yaml(cfg2),
        lambda: _grid_config_jsons(cfg2),
    ):
        with pytest.raises(
            ValueError, match=r"properties\.land_cover\.evetr\.soil_depth"
        ):
            call()


def test_live_mutation_positive_controls_run_cleanly(
    sample_config_loaded: SUEWSConfig, short_forcing
) -> None:
    """Valid live edits (positive soil_depth, or zero depth with zero capacity) run cleanly."""
    # Positive soil_depth mutation
    sim_pos = _sim_from_sample(sample_config_loaded, short_forcing)
    sim_pos.config.sites[0].properties.land_cover.paved.soil_depth.value = 250.0
    forcing_slice = sim_pos.forcing.df
    assert sim_pos.config.to_df_state() is not None
    assert "soil_depth" in config_to_engine_yaml(sim_pos.config)
    grid_ids, jsons = _grid_config_jsons(sim_pos.config)
    assert len(grid_ids) == 1 and len(jsons) == 1
    out_pos = sim_pos.run(end_date=forcing_slice.index[-1])
    assert not out_pos.df.empty

    # Zero soil_depth paired with zero soil_store_capacity (and matching 0 initial soilstore)
    sim_zero_cap = _sim_from_sample(sample_config_loaded, short_forcing)
    paved = sim_zero_cap.config.sites[0].properties.land_cover.paved
    paved.soil_depth.value = 0.0
    paved.soil_store_capacity.value = 0.0
    sim_zero_cap.config.sites[0].initial_states.paved.soilstore.value = 0.0
    assert sim_zero_cap.config.to_df_state() is not None
    out_zero_cap = sim_zero_cap.run(end_date=forcing_slice.index[-1])
    assert not out_zero_cap.df.empty


def test_validate_for_engine_aggregates_refvalue_null_and_zero_soil_depth(
    sample_dict: dict,
    sample_config_loaded: SUEWSConfig,
) -> None:
    """Verify ``validate_for_engine()`` and ``to_df_state()`` run both guards."""
    # ARRANGE: active biogenic CO2 mutated to null min_res_bioco2
    data_co2 = copy.deepcopy(sample_dict)
    data_co2["model"]["physics"]["emissions"] = {"value": 22}
    cfg_co2 = SUEWSConfig.from_dict(data_co2)
    cfg_co2.sites[0].properties.land_cover.grass.min_res_bioco2.value = None

    # ACT / ASSERT: validate_for_engine and all handoffs (including to_df_state) reject it
    with pytest.raises(ValueError, match="min_res_bioco2"):
        cfg_co2.validate_for_engine()
    with pytest.raises(ValueError, match="min_res_bioco2"):
        cfg_co2.to_df_state()

    # ARRANGE: mutated zero soil_depth on active positive-capacity surface
    cfg_soil = sample_config_loaded.model_copy(deep=True)
    cfg_soil.sites[0].properties.land_cover.paved.soil_depth.value = 0.0

    # ACT / ASSERT
    with pytest.raises(ValueError, match=r"properties\.land_cover\.paved\.soil_depth"):
        cfg_soil.validate_for_engine()
    with pytest.raises(ValueError, match=r"properties\.land_cover\.paved\.soil_depth"):
        cfg_soil.to_df_state()


# ---------------------------------------------------------------------------
# Route (d): Direct resolved native Rust engine rejection and positive controls
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("surface", NON_WATER_SURFACES)
@pytest.mark.parametrize("legacy_keys", [False, True])
def test_native_rust_engine_directly_rejects_zero_soil_depth(
    resolved_sample_dict: dict,
    rust_engine_context,
    surface: str,
    legacy_keys: bool,
) -> None:
    """Direct Rust engine call rejects zero soil_depth on active positive-capacity surfaces."""
    # ARRANGE: bypass Python validation by patching the resolved YAML handed straight to Rust
    rust_module, forcing_block, n_steps = rust_engine_context
    resolved_dict = copy.deepcopy(resolved_sample_dict)
    lc = resolved_dict["sites"][0]["properties"]["land_cover"]
    _activate_surface(lc, surface)
    lc[surface].pop("soil_depth", None)
    lc[surface].pop("soil_store_capacity", None)
    if legacy_keys:
        lc[surface]["soilstorecap"] = {"value": 150.0}
        lc[surface]["soildepth"] = {"value": 0.0}
    else:
        lc[surface]["soil_store_capacity"] = {"value": 150.0}
        lc[surface]["soil_depth"] = {"value": 0.0}
    patched_yaml = _yaml_dump(resolved_dict)

    # ACT / ASSERT
    with pytest.raises(
        RuntimeError,
        match=rf"sites\[gridiv=1\]\.properties\.land_cover\.{surface}\.soil_depth.*must not be 0",
    ):
        _run_grid_into(
            rust_module,
            patched_yaml,
            forcing_block,
            n_steps,
            grid_id=1,
        )


def test_native_rust_engine_positive_controls_allow_zero_capacity_inactive_and_water(
    resolved_sample_dict: dict,
    rust_engine_context,
) -> None:
    """Direct Rust engine call accepts zero soil_depth when capacity is 0, inactive or water."""
    # ARRANGE
    rust_module, forcing_block, n_steps = rust_engine_context
    resolved_dict = copy.deepcopy(resolved_sample_dict)
    lc = resolved_dict["sites"][0]["properties"]["land_cover"]

    # Zero capacity on active paved (with initial soilstore within capacity)
    lc["paved"]["soil_store_capacity"] = {"value": 0.0}
    lc["paved"]["soil_depth"] = {"value": 0.0}
    resolved_dict["sites"][0]["initial_states"]["paved"]["soilstore"] = {"value": 0.0}
    # Inactive evetr with positive capacity
    lc["evetr"]["sfr"] = {"value": 0.0}
    lc["evetr"]["soil_store_capacity"] = {"value": 150.0}
    lc["evetr"]["soil_depth"] = {"value": 0.0}
    # Water with positive capacity
    lc["water"]["soil_store_capacity"] = {"value": 150.0}
    lc["water"]["soil_depth"] = {"value": 0.0}
    patched_yaml = _yaml_dump(resolved_dict)

    # ACT
    df_output, _, _ = _run_grid_into(
        rust_module,
        patched_yaml,
        forcing_block,
        n_steps,
        grid_id=1,
    )

    # ASSERT
    assert not df_output.empty
