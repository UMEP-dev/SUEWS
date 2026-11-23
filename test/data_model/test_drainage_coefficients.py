"""Consumed drainage coefficients must not acquire implicit zero values."""

from copy import deepcopy
from importlib.resources import files
from pathlib import Path
import re

import pytest
import yaml

from supy import SUEWSSimulation
from supy._run_rust import config_to_engine_yaml
from supy.data_model.configuration.publisher import generate_json_schema
from supy.data_model.core.config import SUEWSConfig
from supy.util.converter.yaml_upgrade import (
    YamlUpgradeError,
    _migrate_2026_6_dev14_to_2026_6_dev15,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract(
        "C-VALID", "C-STRICT", "C-MIGRATE", "C-SCHEMA", "C-ERROR", "C-BIND"
    ),
]


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
        _migrate_2026_6_dev14_to_2026_6_dev15(data)
    assert data == original


def test_migration_preserves_zero_references_and_ignored_absence(data):
    storage(data)["drain_coef_1"] = {"value": 0.0, "ref": {"desc": "Declared zero"}}
    storage(data, "water").pop("drain_coef_1")
    storage(data, "evetr").pop("drain_coef_2")
    assert _migrate_2026_6_dev14_to_2026_6_dev15(data) == data


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
def test_missing_active_storage_block_is_diagnosed_at_execution(data, tmp_path, entry):
    # Omission is supported during partial configuration work. Loading a
    # configuration is not proof that it is ready for physical execution.
    del data["sites"][0]["properties"]["land_cover"]["paved"]["storage_drain_params"]
    if entry == "direct":
        cfg = SUEWSConfig(**data)
    elif entry == "dict":
        cfg = SUEWSConfig.from_dict(data)
    else:
        file = tmp_path / "absent-block.yml"
        file.write_text(yaml.safe_dump(data), encoding="utf-8")
        cfg = SUEWSSimulation(file).config
    path = "sites[0].properties.land_cover.paved.storage_drain_params"
    with pytest.raises(ValueError, match=re.escape(path)):
        config_to_engine_yaml(cfg)


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
    if missing != "coefficient":
        upgrade_yaml(source, target)
        upgraded = yaml.safe_load(target.read_text(encoding="utf-8"))
        if missing == "none":
            assert storage(upgraded) == storage(data)
            SUEWSConfig.from_dict(upgraded)
        else:
            assert "storage_drain_params" not in upgraded["sites"][0]["properties"]["land_cover"]["paved"]
            cfg = SUEWSConfig.from_dict(upgraded)
            with pytest.raises(ValueError, match="land_cover.paved.storage_drain_params"):
                config_to_engine_yaml(cfg)
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


@pytest.mark.parametrize(
    "target", ["2026.6.dev11", "2026.6.dev12", "2026.6.dev13", "2026.6.dev14"]
)
def test_historical_targets_do_not_apply_dev15_drainage(data, target):
    from supy.util.converter.yaml_upgrade import _HANDLERS, apply_migration_handler

    storage(data).pop("drain_coef_1")
    upgraded = apply_migration_handler(
        _HANDLERS[("2026.6.dev10", target)], data
    )
    assert "drain_coef_1" not in storage(upgraded)


@pytest.mark.parametrize("entry", ["direct", "dict", "yaml", "object"])
def test_public_sparse_configuration_can_be_updated_but_not_run(data, tmp_path, entry):
    """The default public API remains usable without a physical drainage setup."""
    from supy._run_rust import _grid_config_jsons

    payload = {
        "name": "partial configuration",
        "sites": [{"name": "survey", "gridiv": 7,
                   "properties": {"lat": 51.5, "lng": 0.1}}],
    }
    if entry == "direct":
        # A SUEWSConfig with land cover present and all drainage blocks omitted
        # passes update_config() so callers can inspect or patch before run().
        lc = data["sites"][0]["properties"]["land_cover"]
        for surf in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
            lc[surf].pop("storage_drain_params", None)
        cfg = SUEWSConfig(**data)
        sim = SUEWSSimulation(cfg)
    elif entry == "dict":
        sim = SUEWSSimulation(payload)
    elif entry == "yaml":
        source = tmp_path / "partial.yml"
        source.write_text(yaml.safe_dump(payload), encoding="utf-8")
        sim = SUEWSSimulation(source)
    else:
        # Includes the true default component factories, without fixture values.
        cfg = SUEWSConfig(sites=[{}])
        state = cfg.to_df_state()
        assert state["storedrainprm"].isna().all().all()
        restored = SUEWSConfig.from_df_state(state)
        assert all(
            getattr(
                restored.sites[0].properties.land_cover, surf
            ).storage_drain_params.is_placeholder()
            for surf in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil")
        )
        for candidate in (cfg, restored):
            for export in (config_to_engine_yaml, _grid_config_jsons):
                with pytest.raises(
                    ValueError, match=r"sites\[0\].*paved.storage_drain_params"
                ):
                    export(candidate)
        return
    sim.update_config({"model": {"control": {"tstep": 600}}}, auto_load_forcing=False)
    sim.update_config(
        {"model": {"control": {"tstep": {"ref": {"desc": "survey interval"}}}}},
        auto_load_forcing=False,
    )
    assert sim.config.model.control.tstep.value == 600
    assert sim.config.model.control.tstep.ref.desc == "survey interval"
    state = sim.config.to_df_state()
    assert state["storedrainprm"].isna().all().all()
    restored = SUEWSConfig.from_df_state(state)
    # Neither public conversion nor reconstruction can invent coefficients.
    assert all(
        getattr(
            restored.sites[0].properties.land_cover, surf
        ).storage_drain_params.is_placeholder()
        for surf in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil")
    )
    if entry == "direct":
        assert restored.to_df_state()["storedrainprm"].isna().all().all()
    for cfg in (sim.config, restored):
        for export in (config_to_engine_yaml, _grid_config_jsons):
            with pytest.raises(ValueError, match=r"sites\[0\].*paved.storage_drain_params"):
                export(cfg)
    # Provide forcing so the refusal tests physical completeness, not the
    # separate, earlier missing-forcing diagnostic.
    sim.update_config({"model": {"control": {"tstep": 300}}}, auto_load_forcing=False)
    sim.update_forcing(SUEWSSimulation.from_sample_data().forcing.df.iloc[:2])
    with pytest.raises(ValueError, match=r"sites\[0\].*paved.storage_drain_params"):
        sim.run()


def test_omitted_block_stays_missing_through_yaml_and_dataframe(data, tmp_path):
    """Neither serialisation may promote a factory selector into supplied input."""
    del data["sites"][0]["properties"]["land_cover"]["paved"]["storage_drain_params"]
    cfg = SUEWSConfig.from_dict(data)
    saved = tmp_path / "partial.yml"
    cfg.to_yaml(str(saved))
    assert storage(yaml.safe_load(saved.read_text(encoding="utf-8"))) == {}
    restored_yaml = SUEWSConfig.from_yaml(str(saved))
    restored_df = SUEWSConfig.from_df_state(cfg.to_df_state())
    for restored in (restored_yaml, restored_df):
        with pytest.raises(ValueError, match="paved.storage_drain_params"):
            restored.validate_for_engine()


@pytest.mark.parametrize("equation", [1, 2, 3])
@pytest.mark.parametrize("field_index", [2, 3])
def test_dataframe_missing_coefficient_cannot_become_zero(data, equation, field_index):
    from supy._run_rust import _grid_config_jsons

    storage(data)["drain_eq"] = equation
    state = SUEWSConfig(**data).to_df_state()
    state.loc[1, ("storedrainprm", f"({field_index}, 0)")] = float("nan")
    cfg = SUEWSConfig.from_df_state(state)
    field = f"drain_coef_{field_index - 1}"
    assert getattr(cfg.sites[0].properties.land_cover.paved.storage_drain_params, field) is None
    for handoff in (config_to_engine_yaml, _grid_config_jsons, lambda c: c.to_df_state()):
        with pytest.raises(ValueError, match=field):
            handoff(cfg)


def test_drainage_and_existing_errors_share_annotated_diagnostics(data, tmp_path):
    """A new drainage defect must not mask unrelated primary input defects."""
    storage(data).pop("drain_coef_1")
    data["sites"][0]["properties"]["land_cover"]["bldgs"].pop("bldgh")
    source = tmp_path / "invalid.yml"
    source.write_text(yaml.safe_dump(data), encoding="utf-8")
    with pytest.raises(ValueError) as error:
        SUEWSConfig.from_yaml(str(source), auto_generate_annotated=True)
    message = str(error.value)
    assert "bldgh" in message
    assert "paved.storage_drain_params.drain_coef_1" in message
    assert message.index("bldgh") < message.index("paved.storage_drain_params.drain_coef_1")
    annotated = source.with_name("invalid_annotated.yml").read_text(encoding="utf-8")
    assert "bldgh:" in annotated
    assert "drain_coef_1:" in annotated


@pytest.mark.parametrize("defect", ["null", "zero_depth"])
@pytest.mark.parametrize("handoff", ["yaml", "json", "dataframe", "update", "run"])
def test_master_safety_checks_survive_drainage_integration(defect, handoff):
    from supy._run_rust import _grid_config_jsons

    sim = SUEWSSimulation.from_sample_data()
    cfg = sim.config
    paved = cfg.sites[0].properties.land_cover.paved
    from supy.data_model.core.hydro import StorageDrainParams

    # Remove the drainage block too: the existing safety diagnostic must still
    # run, regardless of whether this is a partial or an engine boundary.
    paved.storage_drain_params = StorageDrainParams()
    if defect == "null":
        # Deliberately bypass assignment validation to exercise the live guard.
        object.__setattr__(paved.emis, "value", None)
        message = "emis"
    else:
        assert paved.soil_store_capacity.value > 0
        paved.soil_depth.value = 0.0
        message = "soil_depth"
    with pytest.raises(ValueError, match=message):
        if handoff == "yaml":
            config_to_engine_yaml(cfg)
        elif handoff == "json":
            _grid_config_jsons(cfg)
        elif handoff == "dataframe":
            cfg.to_df_state()
        elif handoff == "update":
            sim.update_config(cfg, auto_load_forcing=False)
        else:
            sim.run()


@pytest.mark.parametrize("unused", ["omitted", "null", "wrapped_null"])
def test_valid_water_distribution_edits_preserve_drainage_values(data, unused):
    """Accepted water-routing edits preserve independently validated drainage inputs."""
    waterdist = data["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"]
    if unused == "omitted":
        waterdist.pop("to_soilstore", None)
    else:
        waterdist["to_soilstore"] = None if unused == "null" else {"value": None}
    cfg = SUEWSConfig(**data)
    paved = cfg.sites[0].properties.land_cover.paved
    # Valid successive edits, with the sum restored before handoff.
    runoff = paved.waterdist.to_runoff.value
    grass = paved.waterdist.to_grass.value
    transfer = min(runoff / 2, 0.01)
    paved.waterdist.to_runoff.value = runoff - transfer
    paved.waterdist.to_grass.value = grass + transfer
    paved.emis.value = 0.90
    expected = storage(yaml.safe_load(config_to_engine_yaml(cfg)))
    sim = SUEWSSimulation(cfg)
    sim.update_config({"model": {"control": {"tstep": 600}}}, auto_load_forcing=False)
    assert storage(yaml.safe_load(config_to_engine_yaml(sim.config))) == expected
    assert sim.config.sites[0].properties.land_cover.paved.emis.value == 0.90


@pytest.mark.parametrize(
    "provided",
    [
        {},
        {"ref": {"desc": "survey"}},
        {"store_cap": None},
        {"store_cap": {"value": None, "ref": {"desc": "survey"}}},
        {"drain_coef_1": {"value": None, "ref": {"desc": "survey"}}},
    ],
)
def test_empty_drainage_remains_partial_without_inventing_values(
    data, provided, tmp_path
):
    from supy._run_rust import _grid_config_jsons

    paved = data["sites"][0]["properties"]["land_cover"]["paved"]
    paved["storage_drain_params"] = provided
    cfg = SUEWSConfig.from_dict(data)
    params = cfg.sites[0].properties.land_cover.paved.storage_drain_params
    assert params.is_placeholder()
    saved = params.model_dump(mode="json", exclude_none=True)
    if "ref" in provided:
        assert saved["ref"]["desc"] == "survey"
    for field in ("store_cap", "drain_coef_1"):
        if isinstance(provided.get(field), dict):
            assert saved[field]["ref"]["desc"] == "survey"
            assert saved[field].get("value") is None
    assert not set(saved).difference(provided)
    assert params.to_df_state(1, 0).isna().all().all()
    path = tmp_path / "partial.yml"
    cfg.to_yaml(path)
    restored = SUEWSConfig.from_yaml(path)
    reloaded = restored.sites[0].properties.land_cover.paved.storage_drain_params
    assert reloaded.model_dump(mode="json", exclude_none=True) == saved
    for candidate in (cfg, restored):
        for export in (config_to_engine_yaml, _grid_config_jsons):
            with pytest.raises(ValueError, match=r"paved\.storage_drain_params"):
                export(candidate)


@pytest.mark.parametrize("equation", [0, 4])
def test_placeholder_detection_preserves_supplied_selector(equation):
    """Explicit integer codes keep their transport, including an explicit zero."""
    from supy.data_model.core.hydro import StorageDrainParams

    params = StorageDrainParams()
    params.drain_eq = equation
    assert not params.is_placeholder()
    assert params.model_dump(mode="json")["drain_eq"] == equation
    state = params.to_df_state(1, 0)
    assert state.loc[1, ("storedrainprm", "(1, 0)")] == equation


@pytest.mark.parametrize("equation", [1, 2, 3])
def test_inactive_missing_coefficients_survive_dataframe_roundtrip(data, equation):
    storage(data, "evetr")["drain_eq"] = equation
    storage(data, "evetr").pop("drain_coef_1")
    storage(data, "evetr")["drain_coef_2"] = 0.0
    cfg = SUEWSConfig(**data)
    restored = SUEWSConfig.from_df_state(cfg.to_df_state())
    evergreen = restored.sites[0].properties.land_cover.evetr
    assert evergreen.storage_drain_params.drain_coef_1 is None
    assert evergreen.storage_drain_params.drain_coef_2.value == 0.0
    config_to_engine_yaml(restored)
    # Activating the surface cannot turn the retained absence into zero.
    evergreen.sfr.value = 0.1
    with pytest.raises(ValueError, match="evetr.storage_drain_params.drain_coef_1"):
        config_to_engine_yaml(restored)


def test_water_dev12_radiation_dev13_continuation_dev14_and_drainage_dev15_are_separate_migrations(
    data,
):
    from supy.data_model.configuration import SchemaMigrator

    data["schema_version"] = "2026.6.dev11"
    paved = data["sites"][0]["properties"]["land_cover"]["paved"]
    paved["waterdist"]["to_soilstore"] = {"value": 0.0}
    data["sites"][0]["initial_states"]["qn_surfs"] = [0.0] * 7
    data["sites"][0]["initial_states"]["dt_since_start"] = 0.0
    reference = {"value": 0.0, "ref": {"desc": "Explicit zero"}}
    storage(data)["drain_coef_1"] = reference
    source = deepcopy(data)
    migrator = SchemaMigrator()
    water = migrator.migrate(data, to_version="2026.6.dev12")
    water_paved = water["sites"][0]["properties"]["land_cover"]["paved"]
    assert "to_soilstore" not in water_paved["waterdist"]
    assert water["sites"][0]["initial_states"]["qn_surfs"] == [0.0] * 7
    assert water["sites"][0]["initial_states"]["dt_since_start"] == 0.0
    assert storage(water)["drain_coef_1"] == reference
    radiation = migrator.migrate(water, to_version="2026.6.dev13")
    assert "qn_surfs" not in radiation["sites"][0]["initial_states"]
    assert radiation["sites"][0]["initial_states"]["dt_since_start"] == 0.0
    assert migrator.migrate(data, to_version="2026.6.dev13") == radiation
    assert storage(radiation)["drain_coef_1"] == reference
    continuation = migrator.migrate(radiation, to_version="2026.6.dev14")
    assert "dt_since_start" not in continuation["sites"][0]["initial_states"]
    assert migrator.migrate(data, to_version="2026.6.dev14") == continuation
    assert migrator.migrate(water, to_version="2026.6.dev14") == continuation
    assert storage(continuation)["drain_coef_1"] == reference
    sequential = migrator.migrate(continuation, to_version="2026.6.dev15")
    assert migrator.migrate(data, to_version="2026.6.dev15") == sequential
    assert migrator.migrate(water, to_version="2026.6.dev15") == sequential
    assert migrator.migrate(radiation, to_version="2026.6.dev15") == sequential
    assert storage(sequential)["drain_coef_1"] == reference
    assert data == source

    del storage(data)["drain_coef_1"]
    missing_dev12 = migrator.migrate(data, to_version="2026.6.dev12")
    assert "drain_coef_1" not in storage(missing_dev12)
    missing_dev13 = migrator.migrate(missing_dev12, to_version="2026.6.dev13")
    assert "drain_coef_1" not in storage(missing_dev13)
    assert "qn_surfs" not in missing_dev13["sites"][0]["initial_states"]
    missing_dev14 = migrator.migrate(missing_dev13, to_version="2026.6.dev14")
    assert "drain_coef_1" not in storage(missing_dev14)
    assert "dt_since_start" not in missing_dev14["sites"][0]["initial_states"]
    for earlier in (data, missing_dev12, missing_dev13, missing_dev14):
        with pytest.raises(
            YamlUpgradeError, match=r"schema 2026\.6\.dev15.*drain_coef_1"
        ):
            migrator.migrate(earlier, to_version="2026.6.dev15")

    data["sites"][0]["initial_states"]["qn_surfs"] = [1.0] + [0.0] * 6
    for target in ("2026.6.dev13", "2026.6.dev14", "2026.6.dev15"):
        with pytest.raises(YamlUpgradeError, match=r"qn_surfs"):
            migrator.migrate(data, to_version=target)

    paved["waterdist"]["to_soilstore"] = 0.5
    for target in ("2026.6.dev12", "2026.6.dev13", "2026.6.dev14", "2026.6.dev15"):
        with pytest.raises(
            YamlUpgradeError, match=r"schema 2026\.6\.dev12.*waterdist\.to_soilstore"
        ):
            migrator.migrate(data, to_version=target)


@pytest.mark.parametrize("command", ["schema", "convert"])
@pytest.mark.parametrize("missing", [False, True])
def test_public_commands_compose_water_radiation_then_drainage(
    data, tmp_path, command, missing
):
    from click.testing import CliRunner

    from supy.cmd.suews_cli import cli

    data["schema_version"] = "2026.6.dev11"
    paved = data["sites"][0]["properties"]["land_cover"]["paved"]
    paved["waterdist"]["to_soilstore"] = {"value": 0.0}
    data["sites"][0]["initial_states"]["qn_surfs"] = [0.0] * 7
    if missing:
        del storage(data)["drain_coef_1"]
    else:
        storage(data)["drain_coef_1"] = {"value": 0.0, "ref": {"desc": "Survey"}}
    source = tmp_path / "legacy.yml"
    source.write_text(yaml.safe_dump(data), encoding="utf-8")
    original = source.read_bytes()
    if command == "schema":
        output = source.with_suffix(".migrated.yml")
        args = ["schema", "--quiet", "migrate", str(source)]
    else:
        output = tmp_path / "converted.yml"
        args = ["convert", "-i", str(source), "-o", str(output)]
    output.write_text("existing file\n", encoding="utf-8")

    result = CliRunner().invoke(cli, args, terminal_width=220)

    assert source.read_bytes() == original
    if missing:
        assert result.exit_code != 0, result.output
        assert output.read_text(encoding="utf-8") == "existing file\n"
    else:
        assert result.exit_code == 0, result.output
        migrated = yaml.safe_load(output.read_text(encoding="utf-8"))
        assert migrated["schema_version"] == "2026.6.dev15"
        water = migrated["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"]
        assert "to_soilstore" not in water
        assert "qn_surfs" not in migrated["sites"][0]["initial_states"]
        assert storage(migrated)["drain_coef_1"] == storage(data)["drain_coef_1"]


@pytest.mark.parametrize("include_internal", [False, True])
def test_schema_export_keeps_water_radiation_and_drainage_version_boundaries(
    include_internal,
):
    for version, requires_routing, rejects_radiation, requires_coefficients in (
        ("2026.6.dev15", True, True, True),
        ("2026.6.dev10", False, False, False),
        ("2026.6.dev11", False, False, False),
        ("2026.6.dev12", True, False, False),
        ("2026.6.dev13", True, True, False),
        ("2026.6.dev14", True, True, False),
        ("2026.6.dev15", True, True, True),
    ):
        schema = generate_json_schema(version, include_internal=include_internal)
        assert schema["version"] == schema["examples"][0]["schema_version"] == version
        routing = schema["$defs"]["WaterDistribution"]["properties"]["to_runoff"]
        assert ("sfr > 0" in routing["description"]) is requires_routing
        assert ("not" in schema["$defs"]["InitialStates"]) is rejects_radiation
        fields = schema["$defs"]["StorageDrainParams"]["properties"]
        for name in ("drain_coef_1", "drain_coef_2"):
            mentions_requirement = "Required and finite" in fields[name]["description"]
            assert mentions_requirement is requires_coefficients
