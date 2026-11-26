"""Vegetation senescence migration preserves evidence for every grid and surface."""

from copy import deepcopy
from pathlib import Path

from click.testing import CliRunner
import numpy as np
import pytest
import yaml

from supy._run_rust import config_to_engine_yaml  # noqa: PLC2701
from supy.cmd.suews_cli import cli
from supy.data_model.configuration import CURRENT_SCHEMA_VERSION
from supy.data_model.configuration.migration import SchemaMigrator
from supy.data_model.configuration.publisher import generate_json_schema
from supy.data_model.core import SUEWSConfig
from supy.util.converter.yaml_upgrade import (
    SoilMigrationContext,
    YamlUpgradeError,
    upgrade_yaml,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-MIGRATE", "C-SCHEMA", "C-ERROR", "C-ROUNDTRIP"),
]

DEV15 = "2026.6.dev15"
DEV16 = "2026.6.dev16"
SURFACES = ("evetr", "dectr", "grass")


@pytest.fixture(params=["portable", "resolved"])
def migration_input(request, tmp_path, sample_config_loaded):
    if request.param == "portable":
        path = tmp_path / "portable.yml"
        sample_config_loaded.to_yaml(str(path), include_internal=False)
        data = yaml.safe_load(path.read_text(encoding="utf-8"))
    else:
        data = yaml.safe_load(config_to_engine_yaml(sample_config_loaded))
    data["schema_version"] = DEV15
    data["sites"].append(deepcopy(data["sites"][0]))
    data["sites"][1]["gridiv"] = 82
    return data


def test_migration_preserves_each_field_and_source_representation(migration_input):
    data = migration_input
    first, second = [site["initial_states"] for site in data["sites"]]
    del first["evetr"]["sdd_id"]
    first["dectr"]["sdd_id"] = 0
    first["grass"]["sdd_id"] = -450.0
    second["evetr"]["sdd_id"] = {"value": 0, "ref": {"desc": "spring restart"}}
    second["dectr"]["sdd_id"] = {"value": 12.5, "ref": {"desc": "site record"}}
    second["grass"]["sdd_id"] = -17
    original = deepcopy(data)
    migrated = SchemaMigrator().migrate(data, to_version=DEV16)
    assert data == original
    assert migrated == {**original, "schema_version": DEV16}
    assert all("sdd_id" not in states["water"] for states in (first, second))


@pytest.mark.parametrize("version", [DEV15, DEV16])
@pytest.mark.parametrize("command", ["schema", "schema-dry-run", "convert"])
def test_migration_errors_name_all_sites_and_leave_files_intact(
    migration_input, version, command, tmp_path
):
    data = migration_input
    data["schema_version"] = version
    source = tmp_path / "site.yml"
    target = source.with_suffix(".migrated.yml")
    if command == "convert":
        args = ["convert", "-i", str(source), "-o", str(target)]
    else:
        args = ["schema", "--quiet", "migrate", str(source)]
        if command == "schema-dry-run":
            args.append("--dry-run")
    target.write_bytes(b"existing output\n")
    # Check a valid run through each chosen command before planting its errors.
    source.write_text(yaml.safe_dump(data), encoding="utf-8")
    runner = CliRunner()
    good = runner.invoke(cli, args, terminal_width=240)
    assert good.exit_code == 0, good.output
    if command == "convert" or (version != CURRENT_SCHEMA_VERSION and command == "schema"):
        output = yaml.safe_load(target.read_text(encoding="utf-8"))
        assert output["schema_version"] == CURRENT_SCHEMA_VERSION
        assert output["sites"] == data["sites"]
    else:
        assert target.read_bytes() == b"existing output\n"
    target_before = target.read_bytes()
    data["sites"][0]["initial_states"]["grass"]["sdd_id"] = float("nan")
    states = data["sites"][1]["initial_states"]
    states["dectr"]["sdd_id"] = {"value": -float("inf")}
    states["evetr"]["sdd_id"] = {"value": float("inf"), "ref": {"desc": "restart"}}
    source.write_text(yaml.safe_dump(data), encoding="utf-8")
    source_before = source.read_bytes()
    result = runner.invoke(cli, args, terminal_width=240)
    assert result.exit_code != 0, result.output
    grid = data["sites"][0]["gridiv"]
    for expected in (
        f"sites[0] (gridiv={grid}).initial_states.grass.sdd_id",
        "sites[1] (gridiv=82).initial_states.dectr.sdd_id",
        "sites[1] (gridiv=82).initial_states.evetr.sdd_id",
        "site or restart record",
    ):
        assert expected in result.output
    assert source.read_bytes() == source_before
    assert target.read_bytes() == target_before


def test_historical_migration_target_retains_dev15_contract(migration_input):
    migration_input["schema_version"] = "2026.6.dev14"
    migration_input["sites"][1]["initial_states"]["grass"]["sdd_id"] = float("nan")
    migrator = SchemaMigrator()
    old = migrator.migrate(migration_input, to_version=DEV15)
    assert np.isnan(old["sites"][1]["initial_states"]["grass"]["sdd_id"])
    for source in (old, migration_input):
        with pytest.raises(YamlUpgradeError, match=r"sites\[1\].*grass\.sdd_id"):
            migrator.migrate(source, to_version=DEV16)


@pytest.mark.parametrize(
    "target",
    [
        "2026.6.dev9",
        "2026.6.dev10",
        "2026.6.dev11",
        "2026.6.dev12",
        "2026.6.dev13",
        "2026.6.dev14",
        DEV15,
    ],
)
@pytest.mark.parametrize(
    "legacy_sdd",
    [
        float("nan"),
        np.float32("nan"),
        np.longdouble("nan"),
        np.longdouble("inf"),
        {"value": np.longdouble("-inf"), "ref": {"desc": "legacy"}},
        True,
        {"value": True, "ref": {"desc": "legacy"}},
    ],
    ids=[
        "nan",
        "np_float32_nan",
        "np_longdouble_nan",
        "np_longdouble_inf",
        "np_longdouble_neg_inf_wrapped",
        "bool_bare",
        "bool_wrapped",
    ],
)
def test_dev8_observed_soil_historical_targets_preserve_nonfinite_sdd_and_dev16_rejects(
    sample_config_loaded, target, legacy_sdd
):
    """dev8 -> dev9 observed-soil validation must not leak dev16 SDD checks onto historical targets."""
    data = yaml.safe_load(config_to_engine_yaml(sample_config_loaded))
    data["schema_version"] = "2026.6.dev8"
    data["model"]["physics"].pop("soil_moisture", None)
    data["model"]["physics"]["soil_moisture_deficit"] = 1
    for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil"):
        data["sites"][0]["properties"]["land_cover"][surface].update(
            soil_depth=200.0,
            soil_store_capacity=80.0,
            soil_field_capacity=0.6,
        )
    data["sites"][0]["properties"]["soil_observation"] = {
        "depth": 200.0,
        "bulk_density": 1.25,
        "soil_not_rocks": 0.8,
    }
    data["sites"][0]["initial_states"]["grass"]["sdd_id"] = deepcopy(legacy_sdd)
    context = SoilMigrationContext(
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
    migrator = SchemaMigrator()
    historical = migrator.migrate(data, to_version=target, migration_context=context)
    assert historical["schema_version"] == target
    preserved = historical["sites"][0]["initial_states"]["grass"]["sdd_id"]
    if isinstance(legacy_sdd, dict):
        assert preserved["ref"] == legacy_sdd["ref"]
        val = preserved["value"]
        if isinstance(legacy_sdd["value"], bool):
            assert val is legacy_sdd["value"]
        else:
            assert not np.isfinite(val)
    elif isinstance(legacy_sdd, bool):
        assert preserved is legacy_sdd
    else:
        assert not np.isfinite(preserved)
    with pytest.raises(YamlUpgradeError, match=r"sites\[0\].*grass\.sdd_id"):
        migrator.migrate(data, to_version=DEV16, migration_context=context)


@pytest.mark.parametrize("source_version", [DEV15, DEV16])
def test_migration_rejects_nested_value_mapping_on_sdd_id(
    migration_input, source_version
):
    """Nested ``{value: {value: 12.5}}`` wrappers must not bypass migration checks."""
    data = migration_input
    data["schema_version"] = source_version
    data["sites"][0]["initial_states"]["dectr"]["sdd_id"] = {"value": {"value": 12.5}}
    with pytest.raises(YamlUpgradeError, match=r"sites\[0\].*dectr\.sdd_id"):
        SchemaMigrator().migrate(data, to_version=DEV16)


@pytest.mark.parametrize("release", ["2026.4.3", "2026.6.5"])
def test_released_input_requires_an_explicit_repair(release, tmp_path):
    fixture = (
        Path(__file__).resolve().parents[1]
        / "fixtures"
        / "release_configs"
        / f"{release}.yml"
    )
    source = tmp_path / "original.yml"
    target = tmp_path / "upgraded.yml"
    source.write_bytes(fixture.read_bytes())
    upgrade_yaml(source, target, from_ver=release)
    good = target.read_bytes()
    exported = yaml.safe_load(good)
    assert exported["schema_version"] == CURRENT_SCHEMA_VERSION
    initial = exported["sites"][0]["initial_states"]
    assert [initial[name]["sdd_id"]["value"] for name in SURFACES] == [-450] * 3

    previous = yaml.safe_load(source.read_text(encoding="utf-8"))
    grid = previous["sites"][0]["gridiv"]
    previous["sites"][0]["initial_states"]["dectr"]["sdd_id"]["value"] = float("nan")
    source.write_text(yaml.safe_dump(previous), encoding="utf-8")
    before = source.read_bytes()
    with pytest.raises(YamlUpgradeError) as error:
        upgrade_yaml(source, target, from_ver=release)
    assert f"sites[0] (gridiv={grid}).initial_states.dectr.sdd_id" in str(error.value)
    assert target.read_bytes() == good
    assert source.read_bytes() == before


@pytest.mark.parametrize("include_internal", [True, False])
def test_published_senescence_description_begins_at_dev16(include_internal):
    for version in (
        "2025.12",
        "2026.5",
        "2026.6.dev11",
        "2026.6.dev12",
        "2026.6.dev13",
        "2026.6.dev14",
        DEV15,
        DEV16,
        "2026.6.dev17",
        "2026.6.dev18",
        "2026.6.dev19",
        "2026.6.dev20",
        "2026.6.dev21",
    ):
        schema = generate_json_schema(version, include_internal=include_internal)
        for name in ("InitialStateEvetr", "InitialStateDectr", "InitialStateGrass"):
            field = schema["$defs"][name]["properties"]["sdd_id"]
            assert field["default"] == 0
            if version in (
                DEV16,
                "2026.6.dev17",
                "2026.6.dev18",
                "2026.6.dev19",
                "2026.6.dev20",
                "2026.6.dev21",
            ):
                assert "Must be finite" in field["description"]
            else:
                assert (
                    field["description"]
                    == "Senescence degree days at the start of the model run"
                )
        assert "sdd_id" not in schema["$defs"]["InitialStateWater"]["properties"]


def test_dataframe_assigns_senescence_to_three_vegetation_kinds(sample_config_loaded):
    frame = sample_config_loaded.to_df_state()
    labels = [column for column in frame.columns if column[0] == "sdd_id"]
    assert set(labels) == {("sdd_id", "(0,)"), ("sdd_id", "(1,)"), ("sdd_id", "(2,)")}
    grid = frame.index[0]
    for i, value in enumerate((-17.0, 0.0, 12.5)):
        frame.loc[grid, ("sdd_id", f"({i},)")] = value
    restored = SUEWSConfig.from_df_state(frame).sites[0].initial_states
    assert [getattr(restored, name).sdd_id.value for name in SURFACES] == [-17, 0, 12.5]
    assert "sdd_id" not in type(restored.water).model_fields


@pytest.mark.parametrize("index,surface", enumerate(SURFACES))
def test_full_dataframe_requires_each_physical_senescence_column(
    sample_config_loaded, index, surface
):
    frame = sample_config_loaded.to_df_state()
    column = ("sdd_id", f"({index},)")
    grid = frame.index[0]
    with pytest.raises(KeyError) as error:
        SUEWSConfig.from_df_state(frame.drop(columns=[column]))
    assert f"site[gridiv={grid}].initial_states.{surface}.sdd_id" in str(error.value)
    assert repr(column) in str(error.value)
