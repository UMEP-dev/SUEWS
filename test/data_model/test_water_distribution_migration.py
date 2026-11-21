"""Water-distribution schema dev12 migration boundaries (gh#2117)."""

from copy import deepcopy
import json
from pathlib import Path

from click.testing import CliRunner
from packaging.version import Version
import pytest
import yaml

from supy.cmd.suews_cli import cli
from supy.data_model.configuration import CURRENT_SCHEMA_VERSION, SchemaMigrator
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.hydro import WaterDistribution
from supy.data_model.core.type import SurfaceType
from supy.util.converter import yaml_upgrade as upgrade

pytestmark = [pytest.mark.api, pytest.mark.cfg, pytest.mark.contract("C-MIGRATE", "C-VALID", "C-SCHEMA")]

SURFACES = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil")


def _partial(surface, dist, sfr=0):
    return {
        "schema_version": "2026.6.dev11",
        "sites": [
            {
                "name": "Routing site",
                "gridiv": 7,
                "properties": {
                    "land_cover": {surface: {"sfr": sfr, "waterdist": dist}}
                },
            }
        ],
    }


def _dist(cfg, surface="paved", site=0):
    return cfg["sites"][site]["properties"]["land_cover"][surface]["waterdist"]


@pytest.mark.parametrize("surface", SURFACES)
@pytest.mark.parametrize(
    "zero",
    [
        0,
        0.0,
        -0.0,
        {"value": 0},
        {
            "value": -0.0,
            "ref": {"desc": "Legacy zero"},
        },
    ],
)
def test_only_inapplicable_numeric_zeros_are_removed(surface, zero, capsys):
    """Partial migrations preserve legitimate zero destinations and references."""
    terminal = "to_soilstore" if surface in {"paved", "bldgs"} else "to_runoff"
    active_terminal = "to_runoff" if surface in {"paved", "bldgs"} else "to_soilstore"
    dist = {
        f"to_{surface}": zero,
        terminal: zero,
        "to_water": {"value": 0, "ref": {"desc": "Keep this reference"}},
        active_terminal: 0,
        "unrelated": {"value": 0},
    }
    cfg = _partial(surface, dist)
    original = deepcopy(cfg)

    result = SchemaMigrator().migrate(cfg)

    assert _dist(result, surface) == {
        "to_water": dist["to_water"],
        active_terminal: 0,
        "unrelated": {"value": 0},
    }
    assert cfg == original
    assert result["schema_version"] == "2026.6.dev12"
    log = capsys.readouterr().err
    assert f"waterdist.to_{surface}" in log
    assert f"waterdist.{terminal}" in log
    assert "inapplicable destination" in log
    assert "gridiv=7" in log


@pytest.mark.parametrize(
    "entry",
    [
        None,
        {"value": None},
        {
            "value": None,
            "ref": {"desc": "Null retained"},
        },
        "0",
        {"value": "0", "ref": {"desc": "String retained"}},
    ],
)
def test_existing_null_and_string_parsing_is_preserved(entry):
    """Accepted non-numeric representations are never silently removed."""
    parsed = WaterDistribution.model_validate({"to_paved": entry})
    parsed.validate_distribution(
        SurfaceType.PAVED, require_complete=False, normalise_zero_disallowed=True
    )
    cfg = _partial("paved", {"to_paved": entry})
    assert _dist(SchemaMigrator().migrate(cfg)) == {"to_paved": entry}


@pytest.mark.parametrize(
    "entry",
    [
        0.2,
        -1,
        float("inf"),
        float("nan"),
        True,
        False,
        {"value": 0.2},
        {"value": False},
        {"value": float("nan")},
        "0.2",
        {"value": "0.2"},
        {"value": 0, "metadata": "invalid wrapper"},
        {"ref": {"desc": "Missing value"}},
        [0],
        {"alternate": 0},
    ],
)
@pytest.mark.parametrize("sfr", [0, 0.5])
def test_inapplicable_errors_match_public_validation(entry, sfr, tmp_path):
    """Neither zero area nor migration may hide malformed inapplicable data."""
    with pytest.raises(ValueError):
        parsed = WaterDistribution.model_validate({"to_paved": entry})
        parsed.validate_distribution(
            SurfaceType.PAVED, require_complete=False, normalise_zero_disallowed=True
        )
    cfg = _partial("paved", {"to_paved": entry}, sfr=sfr)
    source = tmp_path / "source.yml"
    output = tmp_path / "destination.yml"
    source.write_text(yaml.safe_dump(cfg), encoding="utf-8")
    output.write_text("existing destination\n", encoding="utf-8")

    with pytest.raises(
        upgrade.YamlUpgradeError, match=r"sites\[0\].*gridiv=7.*waterdist.to_paved"
    ):
        upgrade.upgrade_yaml(source, output)

    assert output.read_text(encoding="utf-8") == "existing destination\n"


def test_multisite_delta_leaves_other_surfaces_and_missing_blocks_untouched():
    cfg = _partial("paved", {"to_paved": 0})
    second = _partial("grass", {"to_grass": -0.0, "to_soilstore": 1})["sites"][0]
    second["gridiv"] = 8
    cfg["sites"].append(second)
    lc = cfg["sites"][0]["properties"]["land_cover"]
    lc.update({
        "water": {"waterdist": {"to_water": 0}},
        "unknown": {"waterdist": {"to_unknown": 0}},
        "bldgs": {"sfr": 0},
    })
    original = deepcopy(cfg)

    result = SchemaMigrator().migrate(cfg)

    assert _dist(result) == {}
    assert _dist(result, "grass", 1) == {"to_soilstore": 1}
    for surface in ("water", "unknown", "bldgs"):
        assert result["sites"][0]["properties"]["land_cover"][surface] == lc[surface]
    assert cfg == original


@pytest.mark.parametrize(
    "target",
    ["2026.6.dev8", "2026.6.dev9", "2026.6.dev10", "2026.6.dev11", "2026.6.dev12"],
)
def test_historical_targets_pin_water_delta(target):
    cfg = _partial("paved", {"to_paved": 0})
    cfg["schema_version"] = "2026.6.dev7"
    cfg["model"] = {"physics": {"soil_moisture_deficit": 0}}
    original = deepcopy(cfg)

    result = SchemaMigrator().migrate(cfg, to_version=target)

    assert _dist(result) == ({} if target == "2026.6.dev12" else {"to_paved": 0})
    assert result["schema_version"] == target
    assert cfg == original


def test_current_routes_forward_context_and_apply_water_delta_once(
    monkeypatch, valid_two_site_config
):
    """Every old current-target edge retains the context-aware soil route."""
    context = upgrade.SoilMigrationContext(
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
    water_calls = []
    original = upgrade._migrate_2026_6_dev11_to_2026_6_dev12

    def water(cfg):
        water_calls.append(True)
        return original(cfg)

    monkeypatch.setattr(upgrade, "_migrate_2026_6_dev11_to_2026_6_dev12", water)
    sources = [
        source
        for source, target in upgrade._HANDLERS
        if target == CURRENT_SCHEMA_VERSION and Version(source) < Version("2026.6.dev9")
    ]
    assert len(sources) >= 9
    for source in sources:
        water_calls.clear()
        cfg = deepcopy(valid_two_site_config)
        cfg["sites"] = cfg["sites"][:1]
        cfg["schema_version"] = source
        cfg["model"]["physics"].pop("soil_moisture", None)
        cfg["model"]["physics"]["soil_moisture_deficit"] = 1
        for surface in SURFACES:
            cfg["sites"][0]["properties"]["land_cover"][surface].update(
                soil_depth=200.0,
                soil_store_capacity=80.0,
                soil_field_capacity=0.6,
            )
        cfg["sites"][0]["properties"]["soil_observation"] = {
            "depth": 200,
            "smcap": 0.9,
            "bulk_density": 1.25,
        }
        before = deepcopy(cfg)
        result = SchemaMigrator().migrate(cfg, migration_context=context)
        assert water_calls == [True], source
        assert "to_paved" not in _dist(result)
        observation = result["sites"][0]["properties"]["soil_observation"]
        assert observation["layer_depth"] == pytest.approx(0.2), source
        assert observation["bulk_density"] == pytest.approx(1250.0), source
        assert "smcap" not in observation, source
        assert cfg == before


@pytest.fixture
def valid_two_site_config():
    path = (
        Path(__file__).resolve().parents[2] / "src/supy/sample_data/sample_config.yml"
    )
    cfg = yaml.safe_load(path.read_text(encoding="utf-8"))
    cfg["schema_version"] = "2026.6.dev11"
    cfg["sites"].append(deepcopy(cfg["sites"][0]))
    cfg["sites"][1]["gridiv"] = 999
    for site in cfg["sites"]:
        site["properties"]["land_cover"]["paved"]["waterdist"]["to_paved"] = {
            "value": -0.0
        }
    return cfg


@pytest.mark.parametrize("path", ["convert", "unsigned-convert", "schema"])
@pytest.mark.parametrize("malformed", [False, True])
def test_public_cli_migration_on_real_multisite_input(
    valid_two_site_config, tmp_path, path, malformed
):
    """Both public commands migrate valid inputs and preserve outputs on refusal."""
    cfg = valid_two_site_config
    if malformed:
        _dist(cfg, site=1)["to_soilstore"] = {"value": False}
    else:
        SUEWSConfig.model_validate(deepcopy(cfg))
    if path == "unsigned-convert":
        del cfg["schema_version"]
    source = tmp_path / "source.yml"
    source.write_text(yaml.safe_dump(cfg), encoding="utf-8")
    source_bytes = source.read_bytes()
    if path == "schema":
        output = source.with_suffix(".migrated.yml")
        args = ["schema", "--quiet", "migrate", str(source)]
    else:
        output = tmp_path / "output.yml"
        args = ["convert", "-i", str(source), "-o", str(output)]
        if path == "unsigned-convert":
            args += ["--from", "2026.6.dev11"]
    output.write_text("existing destination\n", encoding="utf-8")

    result = CliRunner().invoke(cli, args)

    assert source.read_bytes() == source_bytes
    if malformed:
        assert result.exit_code != 0, result.output
        assert "to_soilstore" in result.output
        assert "999" in result.output
        assert output.read_text(encoding="utf-8") == "existing destination\n"
    else:
        assert result.exit_code == 0, result.output
        migrated = yaml.safe_load(output.read_text(encoding="utf-8"))
        assert migrated["schema_version"] == "2026.6.dev12"
        assert all("to_paved" not in _dist(migrated, site=i) for i in range(2))
        SUEWSConfig.from_yaml(str(output))


def test_pinned_schema_export_keeps_dev11_water_help(tmp_path):
    runner = CliRunner()
    for version in ("2026.6.dev12", "2026.6.dev11", "2026.6.dev12"):
        path = tmp_path / "schema.json"
        result = runner.invoke(
            cli, ["schema", "export", "--version", version, "-o", str(path)]
        )
        assert result.exit_code == 0, result.output
        schema = json.loads(path.read_text(encoding="utf-8"))
        assert schema["version"] == schema["examples"][0]["schema_version"] == version
        definitions = schema["$defs"]
        routing = definitions["WaterDistribution"]
        building = definitions["BldgsProperties"]["properties"]["waterdist"]
        historical = version == "2026.6.dev11"
        if historical:
            assert routing["description"] == "Water routing between surface types."
            assert "description" not in building
        else:
            assert "sfr > 0" in routing["description"]
            assert "sfr > 0" in building["description"]
        for name, field in routing["properties"].items():
            if name.startswith("to_"):
                assert ("sfr > 0" in field["description"]) is not historical
        for surface in ("Bsoil", "Dectr", "Evetr", "Grass", "Paved", "Water"):
            waterdist = definitions[f"{surface}Properties"]["properties"]["waterdist"]
            assert ("sfr > 0" in waterdist["description"]) is not historical
