"""The EHC surface-coupling option: default, readable names and df_state round trip."""

import copy

import pytest
import yaml

from supy.data_model import SUEWSConfig
from supy.data_model.core.model import (
    EHCStabilityControl,
    EHCSurfaceCoupling,
    ModelPhysics,
)

pytestmark = [
    pytest.mark.unit,
    pytest.mark.contract("C-VALID", "C-MIGRATE", "C-ROUNDTRIP", "C-SCHEMA", "C-BIND"),
]


def _code(value):
    while hasattr(value, "value"):
        value = value.value
    return value


def test_ehc_surface_coupling_defaults_to_implicit():
    assert _code(ModelPhysics().ehc_surface_coupling) == EHCSurfaceCoupling.IMPLICIT.value


@pytest.mark.parametrize(
    ("name", "member"),
    [("explicit", EHCSurfaceCoupling.EXPLICIT), ("implicit", EHCSurfaceCoupling.IMPLICIT)],
)
def test_ehc_surface_coupling_accepts_readable_names(name, member):
    assert _code(ModelPhysics(ehc_surface_coupling=name).ehc_surface_coupling) == member.value


@pytest.mark.parametrize("wrapped", [False, True])
@pytest.mark.parametrize("value,code", [("explicit", 0), ("implicit", 1), (0, 0), (1, 1), (0.0, 0), (1.0, 1)])
def test_ehc_coupling_accepts_native_selector_domain(value, code, wrapped):
    """Use the same scalar and RefValue cases as the native YAML parser."""
    selector = {"value": value} if wrapped else value
    assert _code(ModelPhysics(ehc_surface_coupling=selector).ehc_surface_coupling) == code


@pytest.mark.parametrize("wrapped", [False, True])
@pytest.mark.parametrize("value", ["unknown", -999, -1, 2, 0.5, 1.5, 1.0000000001, float("nan"), float("inf")])
def test_ehc_coupling_rejects_invalid_native_selectors(value, wrapped):
    """Invalid native YAML selectors must also fail Python validation."""
    selector = {"value": value} if wrapped else value
    with pytest.raises(ValueError):
        ModelPhysics(ehc_surface_coupling=selector)


@pytest.mark.parametrize("member", list(EHCSurfaceCoupling))
def test_ehc_surface_coupling_roundtrips_through_df_state(member):
    state = ModelPhysics(ehc_surface_coupling=member).to_df_state(grid_id=1)
    restored = ModelPhysics.from_df_state(state, grid_id=1)

    assert state.loc[1, ("ehc_surface_coupling", "0")] == member.value
    assert _code(restored.ehc_surface_coupling) == member.value


def test_ehc_surface_coupling_missing_from_legacy_df_state_defaults_to_implicit():
    state = ModelPhysics(ehc_surface_coupling="implicit").to_df_state(grid_id=1)
    state = state.drop(columns=[("ehc_surface_coupling", "0")])

    restored = ModelPhysics.from_df_state(state, grid_id=1)

    assert _code(restored.ehc_surface_coupling) == EHCSurfaceCoupling.IMPLICIT.value


@pytest.mark.parametrize("field,default", [
    ("control_ehc_initial_profile", True),
    ("resistance_ehc_indoor_roof", 0.10),
    ("resistance_ehc_indoor_wall", 0.13),
])
def test_indoor_options_defaults_and_legacy_df_state(field, default):
    physics = ModelPhysics()
    assert _code(getattr(physics, field)) == default
    state = physics.to_df_state(grid_id=1).drop(columns=[(field, "0")])
    assert _code(getattr(ModelPhysics.from_df_state(state, 1), field)) == default


@pytest.mark.parametrize("field", ["resistance_ehc_indoor_roof", "resistance_ehc_indoor_wall"])
@pytest.mark.parametrize("value", [-1.0, -999.0, float("nan"), float("inf"), True, False])
def test_indoor_resistance_rejects_invalid_values(field, value):
    with pytest.raises(ValueError):
        ModelPhysics(**{field: value})


@pytest.mark.parametrize("value", [0.0, 0.25])
def test_indoor_options_roundtrip_through_df_state(value):
    physics = ModelPhysics(control_ehc_initial_profile=False,
                           resistance_ehc_indoor_roof=value,
                           resistance_ehc_indoor_wall=value)
    restored = ModelPhysics.from_df_state(physics.to_df_state(grid_id=1), 1)
    assert _code(restored.control_ehc_initial_profile) is False
    assert _code(restored.resistance_ehc_indoor_roof) == value
    assert _code(restored.resistance_ehc_indoor_wall) == value


@pytest.mark.parametrize("member", list(EHCStabilityControl))
def test_stability_control_names_and_df_state(member):
    physics = ModelPhysics(control_ehc_stability=member.name.lower())
    state = physics.to_df_state(grid_id=1)
    assert _code(physics.control_ehc_stability) == member.value
    assert _code(ModelPhysics.from_df_state(state, 1).control_ehc_stability) == member.value
    state = state.drop(columns=[("control_ehc_stability", "0")])
    assert _code(ModelPhysics.from_df_state(state, 1).control_ehc_stability) == 0


@pytest.mark.parametrize("value", ["unknown", -999, -1, 3, 1.5, float("nan"), float("inf")])
def test_stability_control_rejects_invalid_values(value):
    with pytest.raises(ValueError):
        ModelPhysics(control_ehc_stability=value)


@pytest.mark.parametrize("name", ["off", "relax", "secant"])
def test_stability_control_yaml_and_mapping_inputs(sample_yaml_path, tmp_path, name):
    mapping = yaml.safe_load(sample_yaml_path.read_text(encoding="utf-8"))
    mapping["model"]["physics"]["control_ehc_stability"] = {"value": name}
    mapping["model"]["control"]["forcing"]["file"] = str(sample_yaml_path.parent / "Kc_2012_data_60.txt")
    direct = SUEWSConfig.model_validate(copy.deepcopy(mapping))
    path = tmp_path / "control.yml"
    path.write_text(yaml.safe_dump(mapping), encoding="utf-8")
    loaded = SUEWSConfig.from_yaml(str(path))
    assert _code(loaded.model.physics.control_ehc_stability) == _code(direct.model.physics.control_ehc_stability)


@pytest.mark.parametrize("coupling", [None, "explicit", "implicit"])
def test_optional_ehc_fields_load_from_yaml_and_mapping(sample_yaml_path, tmp_path, coupling):
    mapping = yaml.safe_load(sample_yaml_path.read_text(encoding="utf-8"))
    physics = mapping["model"]["physics"]
    for field in (
        "ehc_surface_coupling", "control_ehc_stability", "control_ehc_initial_profile",
        "resistance_ehc_indoor_roof", "resistance_ehc_indoor_wall",
    ):
        physics.pop(field, None)
    physics["storage_heat"] = 5
    if coupling is not None:
        physics["ehc_surface_coupling"] = coupling
    mapping["model"]["control"]["forcing"]["file"] = str(sample_yaml_path.parent / "Kc_2012_data_60.txt")
    direct = SUEWSConfig.model_validate(copy.deepcopy(mapping))
    path = tmp_path / "ehc.yml"
    path.write_text(yaml.safe_dump(mapping), encoding="utf-8")
    loaded = SUEWSConfig.from_yaml(str(path))
    for config in (direct, loaded):
        assert _code(config.model.physics.ehc_surface_coupling) == (0 if coupling == "explicit" else 1)
        assert _code(config.model.physics.control_ehc_stability) == 0
        assert _code(config.model.physics.control_ehc_initial_profile) is True
        assert _code(config.model.physics.resistance_ehc_indoor_roof) == pytest.approx(0.10)
        assert _code(config.model.physics.resistance_ehc_indoor_wall) == pytest.approx(0.13)


@pytest.mark.parametrize("property_name", ["dz", "k", "rho_cp"])
@pytest.mark.parametrize("invalid", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_thermal_properties_rejected_in_yaml_and_mapping(
    sample_yaml_path, tmp_path, property_name, invalid
):
    mapping = yaml.safe_load(sample_yaml_path.read_text(encoding="utf-8"))
    mapping["model"]["physics"]["storage_heat"] = 5
    mapping["model"]["control"]["forcing"]["file"] = str(sample_yaml_path.parent / "Kc_2012_data_60.txt")
    thermal = mapping["sites"][0]["properties"]["land_cover"]["paved"]["thermal_layers"]
    thermal[property_name]["value"][1] = invalid
    with pytest.raises(ValueError, match="thermal layer properties must be finite"):
        SUEWSConfig.model_validate(copy.deepcopy(mapping))
    path = tmp_path / "invalid-thermal.yml"
    path.write_text(yaml.safe_dump(mapping), encoding="utf-8")
    with pytest.raises(ValueError, match="thermal layer properties must be finite"):
        SUEWSConfig.from_yaml(str(path))


def test_historical_dev17_schema_omits_dev18_ehc_controls():
    from supy.data_model.configuration.publisher import generate_json_schema

    dev17 = generate_json_schema("2026.6.dev17")
    dev18 = generate_json_schema("2026.6.dev18")
    physics17 = dev17["$defs"]["ModelPhysics"]["properties"]
    physics18 = dev18["$defs"]["ModelPhysics"]["properties"]
    for field_name in (
        "ehc_surface_coupling",
        "control_ehc_stability",
        "control_ehc_initial_profile",
        "resistance_ehc_indoor_roof",
        "resistance_ehc_indoor_wall",
    ):
        assert field_name not in physics17
        assert field_name in physics18
    for def_name in (
        "EHCSurfaceCoupling",
        "EHCStabilityControl",
        "RefValue_EHCSurfaceCoupling_",
        "RefValue_EHCStabilityControl_",
    ):
        assert def_name not in dev17["$defs"]


def test_dev17_to_dev18_migration_logs_default_coupling_change(sample_yaml_path, capsys):
    from supy.data_model.configuration.migration import SchemaMigrator

    mapping = yaml.safe_load(sample_yaml_path.read_text(encoding="utf-8"))
    mapping["schema_version"] = "2026.6.dev17"
    mapping["model"]["physics"]["storage_heat"] = 5
    mapping["model"]["physics"].pop("ehc_surface_coupling", None)
    migrated = SchemaMigrator().migrate(mapping, to_version="2026.6.dev18")
    err = capsys.readouterr().err
    assert migrated["schema_version"] == "2026.6.dev18"
    assert "ehc_surface_coupling" not in migrated["model"]["physics"]
    assert "ehc_surface_coupling omitted with storage_heat=5" in err
