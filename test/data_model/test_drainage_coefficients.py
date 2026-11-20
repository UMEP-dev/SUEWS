"""Consumed drainage coefficients must not acquire implicit zero values."""

from copy import deepcopy
from importlib.resources import files
from pathlib import Path
import re

import pytest
import yaml

from supy import SUEWSSimulation
from supy._run_rust import config_to_engine_yaml
from supy.data_model.core.config import SUEWSConfig
from supy.util.converter.yaml_upgrade import (
    YamlUpgradeError,
    _migrate_2026_6_dev11_to_2026_6_dev12,
)

pytestmark = [pytest.mark.api, pytest.mark.cfg]


@pytest.fixture
def data():
    return yaml.safe_load(
        files("supy")
        .joinpath("sample_data/sample_config.yml")
        .read_text(encoding="utf-8")
    )


def storage(data, surface="paved", site=0):
    return data["sites"][site]["properties"]["land_cover"][surface][
        "storage_drain_params"
    ]


@pytest.mark.parametrize("field", ["drain_coef_1", "drain_coef_2"])
@pytest.mark.parametrize("equation", [1, 2, 3])
@pytest.mark.parametrize("absence", ["missing", "null", "wrapped_null"])
@pytest.mark.parametrize("entry", ["direct", "dict", "file"])
def test_load_requires_consumed_coefficient(
    data, tmp_path, field, equation, absence, entry
):
    params = storage(data)
    params["drain_eq"] = equation
    if absence == "missing":
        params.pop(field)
    else:
        params[field] = None if absence == "null" else {"value": None}
    path = f"sites[0].properties.land_cover.paved.storage_drain_params.{field}"
    with pytest.raises(ValueError, match=re.escape(path)):
        if entry == "direct":
            SUEWSConfig(**data)
        elif entry == "dict":
            SUEWSConfig.from_dict(data)
        else:
            config_path = tmp_path / "drainage.yml"
            config_path.write_text(yaml.safe_dump(data), encoding="utf-8")
            SUEWSSimulation(config_path)


@pytest.mark.parametrize("field", ["drain_coef_1", "drain_coef_2"])
@pytest.mark.parametrize(
    "surface", ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil"]
)
@pytest.mark.parametrize("mutation", ["field", "value"])
def test_mutable_handoff_checks_current_surface_values(data, field, surface, mutation):
    config = SUEWSConfig(**data)
    land = getattr(config.sites[0].properties.land_cover, surface)
    land.sfr.value = 0.1
    land.storage_drain_params.drain_eq.value = 3
    if mutation == "field":
        setattr(land.storage_drain_params, field, None)
    else:
        getattr(land.storage_drain_params, field).value = None
    path = f"sites[0].properties.land_cover.{surface}.storage_drain_params.{field}"
    for handoff in (config_to_engine_yaml, lambda cfg: cfg.to_df_state()):
        with pytest.raises(ValueError, match=re.escape(path)):
            handoff(config)


@pytest.mark.parametrize("field", ["drain_coef_1", "drain_coef_2"])
def test_mutable_public_run_rejects_omission(field):
    sim = SUEWSSimulation.from_sample_data()
    setattr(
        sim.config.sites[0].properties.land_cover.paved.storage_drain_params,
        field,
        None,
    )
    with pytest.raises(
        ValueError,
        match=re.escape(
            f"sites[0].properties.land_cover.paved.storage_drain_params.{field}"
        ),
    ):
        sim.run()


def test_error_identifies_second_site(data):
    data["sites"].append(deepcopy(data["sites"][0]))
    storage(data, site=1).pop("drain_coef_2")
    with pytest.raises(
        ValueError,
        match=re.escape(
            "sites[1].properties.land_cover.paved.storage_drain_params.drain_coef_2"
        ),
    ):
        SUEWSConfig(**data)


@pytest.mark.parametrize("equation", [1, 2, 3])
@pytest.mark.parametrize("legacy", [False, True])
def test_zero_and_reference_survive_handoff(data, equation, legacy):
    params = storage(data)
    params["drain_eq"] = equation
    params["drain_coef_1"] = {"value": 1.25, "ref": {"desc": "Coefficient source"}}
    params["drain_coef_2"] = 0.0
    if legacy:
        paved = data["sites"][0]["properties"]["land_cover"]["paved"]
        paved["storedrainprm"] = paved.pop("storage_drain_params")
    cfg = SUEWSConfig(**data)
    emitted = storage(yaml.safe_load(config_to_engine_yaml(cfg)))
    assert emitted["drain_coef_1"]["value"] == 1.25
    assert emitted["drain_coef_1"]["ref"]["desc"] == "Coefficient source"
    # An unannotated FlexibleRefValue may serialise as a bare scalar. Keep the
    # referenced coefficient above as a separate control and inspect this key.
    assert emitted["drain_coef_2"] == 0.0
    state = cfg.to_df_state()
    assert state.loc[1, ("storedrainprm", "(2, 0)")] == 1.25
    assert state.loc[1, ("storedrainprm", "(3, 0)")] == 0.0


@pytest.mark.parametrize("surface", ["evetr", "water"])
def test_ignored_coefficients_may_be_absent(data, surface):
    # The shipped evergreen area is zero; water is excluded even when present.
    params = storage(data, surface)
    params.pop("drain_coef_1")
    params["drain_coef_2"] = None
    cfg = SUEWSConfig(**data)
    config_to_engine_yaml(cfg)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), "bad", True])
def test_mutated_supplied_coefficient_must_be_finite_numeric(data, value):
    cfg = SUEWSConfig(**data)
    # Deliberate unchecked mutation tests defence at the execution boundary.
    object.__setattr__(
        cfg.sites[0].properties.land_cover.paved.storage_drain_params,
        "drain_coef_1",
        value,
    )
    with pytest.raises(ValueError, match="drain_coef_1"):
        config_to_engine_yaml(cfg)


@pytest.mark.parametrize("legacy", [False, True])
def test_migration_refuses_to_invent_coefficients(data, legacy):
    params = storage(data)
    params.pop("drain_coef_1")
    if legacy:
        paved = data["sites"][0]["properties"]["land_cover"]["paved"]
        paved["storedrainprm"] = paved.pop("storage_drain_params")
    original = deepcopy(data)
    with pytest.raises(
        YamlUpgradeError, match="land_cover.paved.storage_drain_params.drain_coef_1"
    ):
        _migrate_2026_6_dev11_to_2026_6_dev12(data)
    assert data == original


def test_migration_preserves_zero_references_and_ignored_absence(data):
    storage(data)["drain_coef_1"] = {"value": 0.0, "ref": {"desc": "Declared zero"}}
    storage(data, "water").pop("drain_coef_1")
    storage(data, "evetr").pop("drain_coef_2")
    assert _migrate_2026_6_dev11_to_2026_6_dev12(data) == data


@pytest.mark.parametrize("field", ["drain_coef_1", "drain_coef_2"])
def test_partial_update_revalidates_coefficients(field):
    sim = SUEWSSimulation.from_sample_data()
    patch = {
        "sites": {
            0: {
                "properties": {
                    "land_cover": {
                        "paved": {"storage_drain_params": {field: {"value": None}}}
                    }
                }
            }
        }
    }
    with pytest.raises(ValueError, match=re.escape(f"storage_drain_params.{field}")):
        sim.update_config(patch, auto_load_forcing=False)


@pytest.mark.parametrize("missing", [False, True])
def test_registered_migration_keeps_source_and_checks_coefficients(
    data, tmp_path, missing
):
    from supy.data_model.configuration import CURRENT_SCHEMA_VERSION
    from supy.util.converter.yaml_upgrade import upgrade_yaml

    data["schema_version"] = "2026.6.dev11"
    if missing:
        storage(data).pop("drain_coef_1")
    else:
        storage(data)["drain_coef_1"] = {"value": 0.0, "ref": {"desc": "Declared zero"}}
    source = tmp_path / "old.yml"
    output = tmp_path / "new.yml"
    source.write_text(yaml.safe_dump(data), encoding="utf-8")
    original = source.read_bytes()
    if missing:
        with pytest.raises(YamlUpgradeError, match="drain_coef_1"):
            upgrade_yaml(source, output)
        assert not output.exists()
    else:
        upgrade_yaml(source, output)
        upgraded = yaml.safe_load(output.read_text(encoding="utf-8"))
        assert upgraded["schema_version"] == CURRENT_SCHEMA_VERSION
        assert storage(upgraded) == storage(data)
    assert source.read_bytes() == original


@pytest.mark.parametrize("equation", [1.5, 2.5, 3.5, 1.0000000001])
def test_fractional_selector_cannot_bypass_mutable_handoff(data, equation):
    cfg = SUEWSConfig(**data)
    params = cfg.sites[0].properties.land_cover.paved.storage_drain_params
    object.__setattr__(params, "drain_eq", equation)
    params.drain_coef_1 = None
    path = "sites[0].properties.land_cover.paved.storage_drain_params.drain_eq"
    with pytest.raises(ValueError, match=re.escape(path)):
        config_to_engine_yaml(cfg)


@pytest.mark.parametrize("entry", ["direct", "dict", "file"])
def test_missing_active_storage_block_is_diagnosed(data, tmp_path, entry):
    del data["sites"][0]["properties"]["land_cover"]["paved"]["storage_drain_params"]
    path = "sites[0].properties.land_cover.paved.storage_drain_params"
    with pytest.raises(ValueError, match=re.escape(path)):
        if entry == "direct":
            SUEWSConfig(**data)
        elif entry == "dict":
            SUEWSConfig.from_dict(data)
        else:
            file = tmp_path / "absent-block.yml"
            file.write_text(yaml.safe_dump(data), encoding="utf-8")
            SUEWSSimulation(file)


def test_missing_block_on_mutable_run_reports_full_path():
    sim = SUEWSSimulation.from_sample_data()
    object.__setattr__(
        sim.config.sites[0].properties.land_cover.paved, "storage_drain_params", None
    )
    with pytest.raises(
        ValueError,
        match=re.escape("sites[0].properties.land_cover.paved.storage_drain_params"),
    ):
        sim.run()


@pytest.mark.parametrize("surface", ["evetr", "water"])
def test_missing_ignored_block_remains_accepted(data, surface):
    del data["sites"][0]["properties"]["land_cover"][surface]["storage_drain_params"]
    config_to_engine_yaml(SUEWSConfig(**data))


@pytest.mark.parametrize("equation", [0, -999, 4])
def test_unknown_integer_selector_policy_is_unchanged(data, equation):
    params = storage(data)
    params["drain_eq"] = equation
    params.pop("drain_coef_1")
    params.pop("drain_coef_2")
    cfg = SUEWSConfig(**data)
    emitted = storage(yaml.safe_load(config_to_engine_yaml(cfg)))
    # This is a transport check, not a claim that a wet run is defined.
    assert emitted["drain_eq"] == equation


@pytest.mark.parametrize("source_version", ["2026.6.dev9", "2026.6.dev10"])
@pytest.mark.parametrize("missing", ["none", "coefficient", "block"])
def test_before_dev11_migration_applies_drainage_delta(
    data, tmp_path, source_version, missing
):
    from supy.util.converter.yaml_upgrade import upgrade_yaml

    data["schema_version"] = source_version
    storage(data)["drain_coef_1"] = {"value": 0.0, "ref": {"desc": "User coefficient"}}
    if missing == "coefficient":
        storage(data).pop("drain_coef_2")
    elif missing == "block":
        del data["sites"][0]["properties"]["land_cover"]["paved"][
            "storage_drain_params"
        ]
    source, target = tmp_path / "source.yml", tmp_path / "upgraded.yml"
    source.write_text(yaml.safe_dump(data), encoding="utf-8")
    original = source.read_bytes()
    if missing == "none":
        upgrade_yaml(source, target)
        upgraded = yaml.safe_load(target.read_text(encoding="utf-8"))
        assert storage(upgraded) == storage(data)
        SUEWSConfig.from_dict(upgraded)
    else:
        with pytest.raises(
            YamlUpgradeError, match="land_cover.paved.storage_drain_params"
        ):
            upgrade_yaml(source, target)
        assert not target.exists()
    assert source.read_bytes() == original


@pytest.mark.parametrize("release", ["2025.11.20", "2026.4.3", "2026.6.5"])
@pytest.mark.parametrize("missing", [False, True])
def test_released_legacy_migration_preserves_or_refuses_coefficients(
    tmp_path, release, missing
):
    from supy.util.converter.yaml_upgrade import upgrade_yaml

    fixture = (
        Path(__file__).parents[1] / "fixtures" / "release_configs" / f"{release}.yml"
    )
    payload = yaml.safe_load(fixture.read_text(encoding="utf-8"))
    paved = payload["sites"][0]["properties"]["land_cover"]["paved"]
    params = paved.get("storage_drain_params", paved.get("storedrainprm"))
    params["drain_coef_1"] = {
        "value": 0.0,
        "ref": {"desc": "Legacy coefficient source"},
    }
    if missing:
        params.pop("drain_coef_2")
    source, target = tmp_path / "source.yml", tmp_path / "upgraded.yml"
    source.write_text(yaml.safe_dump(payload), encoding="utf-8")
    original = source.read_bytes()
    if missing:
        with pytest.raises(YamlUpgradeError, match="drain_coef_2"):
            upgrade_yaml(source, target, from_ver=release)
        assert not target.exists()
    else:
        upgrade_yaml(source, target, from_ver=release)
        upgraded = yaml.safe_load(target.read_text(encoding="utf-8"))
        assert storage(upgraded)["drain_coef_1"] == params["drain_coef_1"]
        assert storage(upgraded)["drain_coef_2"] == params["drain_coef_2"]
        SUEWSConfig.from_dict(upgraded)
    assert source.read_bytes() == original


def test_historical_dev11_target_does_not_apply_dev12_policy(data):
    from supy.util.converter.yaml_upgrade import _HANDLERS, apply_migration_handler

    storage(data).pop("drain_coef_1")
    upgraded = apply_migration_handler(
        _HANDLERS[("2026.6.dev10", "2026.6.dev11")], data
    )
    assert "drain_coef_1" not in storage(upgraded)
