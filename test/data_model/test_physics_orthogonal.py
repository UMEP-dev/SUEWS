"""Tests for orthogonal physics input forms."""

from __future__ import annotations

from pathlib import Path

from pydantic import ValidationError
import pytest
import yaml

from supy.data_model.core.field_renames import read_physics_key
from supy.data_model.core.model import ModelPhysics
from supy.data_model.core.physics_families import flatten_physics_in_config
from supy.data_model.core.physics_orthogonal import coerce_orthogonal_to_flat

pytestmark = pytest.mark.api


def _unwrap(v):
    return v.value if hasattr(v, "value") else v


@pytest.mark.parametrize(
    "payload, expected",
    [
        ({"scheme": "forcing"}, 0),
        ({"scheme": "narp", "ldown": "observed"}, 1),
        ({"scheme": "narp", "ldown": "cloud"}, 2),
        ({"scheme": "narp", "ldown": "air"}, 3),
        ({"scheme": "narp", "ldown": "observed", "variant": "surface"}, 11),
        ({"scheme": "narp", "ldown": "cloud", "variant": "surface"}, 12),
        ({"scheme": "narp", "ldown": "air", "variant": "surface"}, 13),
        ({"scheme": "narp", "ldown": "observed", "variant": "zenith"}, 100),
        ({"scheme": "narp", "ldown": "cloud", "variant": "zenith"}, 200),
        ({"scheme": "narp", "ldown": "air", "variant": "zenith"}, 300),
        ({"scheme": "spartacus", "ldown": "observed"}, 1001),
        ({"scheme": "spartacus", "ldown": "cloud"}, 1002),
        ({"scheme": "spartacus", "ldown": "air"}, 1003),
    ],
)
def test_orthogonal_net_radiation_collapses(payload, expected):
    assert coerce_orthogonal_to_flat("net_radiation", payload) == {"value": expected}


def test_orthogonal_form_preserves_ref():
    result = coerce_orthogonal_to_flat(
        "net_radiation",
        {"scheme": "narp", "ldown": "air", "ref": {"doi": "10.example/ref"}},
    )
    assert result == {"value": 3, "ref": {"doi": "10.example/ref"}}


def test_orthogonal_forcing_scheme_preserves_ref():
    # gh#1495 review: the `forcing` scheme (code 0) has its own branch that
    # carries no ldown/variant; confirm a `ref` survives on that path too.
    result = coerce_orthogonal_to_flat(
        "net_radiation",
        {"scheme": "forcing", "ref": {"doi": "10.example/forcing"}},
    )
    assert result == {"value": 0, "ref": {"doi": "10.example/forcing"}}


def test_orthogonal_net_radiation_accepts_scheme_scoped_options():
    assert coerce_orthogonal_to_flat("net_radiation", {"narp": {"ldown": "air"}}) == {
        "value": 3
    }


def test_non_orthogonal_shapes_pass_through_for_existing_normalisers():
    family = {"spartacus": {"value": 1001}}
    flat = {"value": 3}

    assert coerce_orthogonal_to_flat("net_radiation", family) is family
    assert coerce_orthogonal_to_flat("net_radiation", flat) is flat
    assert coerce_orthogonal_to_flat("storage_heat", {"scheme": "narp"}) == {
        "scheme": "narp"
    }


def test_model_physics_accepts_orthogonal_narp_default_variant():
    phys = ModelPhysics(net_radiation={"scheme": "narp", "ldown": "air"})
    assert int(_unwrap(phys.net_radiation)) == 3


def test_sample_config_prefers_nested_readable_physics_defaults():
    path = Path(__file__).resolve().parents[2] / "src/supy/sample_data/sample_config.yml"
    physics = yaml.safe_load(path.read_text(encoding="utf-8"))["model"]["physics"]

    assert physics["net_radiation"] == {"narp": {"ldown": "air"}}
    assert physics["kdown_split_method"] == "constant"
    assert physics["emissions"] == {
        "heat": "J11",
        "co2": {"anthropogenic": "none", "biogenic": "none"},
    }
    assert physics["storage_heat"] == {"ohm": {"include_qf": False}}
    assert {
        key: physics[key]
        for key in (
            "roughness_length_momentum",
            "roughness_length_heat",
            "stability",
            "soil_moisture_deficit",
            "water_use",
            "roughness_sublayer",
            "frontal_area_index",
            "roughness_sublayer_level",
            "surface_conductance",
            "leaf_area_index",
            "snow",
        )
    } == {
        "roughness_length_momentum": "fixed",
        "roughness_length_heat": "K09",
        "stability": "CN98",
        "soil_moisture_deficit": "modelled",
        "water_use": "modelled",
        "roughness_sublayer": "variable",
        "frontal_area_index": "observed",
        "roughness_sublayer_level": "basic",
        "surface_conductance": "W16",
        "leaf_area_index": "modelled",
        "snow": "disabled",
    }
    assert physics["stebbs"] == {
        "enabled": False,
        "parameter_source": "default",
        "capacitance": "default",
        "setpoint": "constant",
        "same_albedo_wall": "disabled",
        "same_albedo_roof": "disabled",
        "same_emissivity_wall": "disabled",
        "same_emissivity_roof": "disabled",
    }

    parsed = ModelPhysics.model_validate(physics)
    assert {
        "net_radiation": int(_unwrap(parsed.net_radiation)),
        "kdown_split_method": int(_unwrap(parsed.kdown_split_method)),
        "emissions": int(_unwrap(parsed.emissions)),
        "storage_heat": int(_unwrap(parsed.storage_heat)),
        "ohm_inc_qf": int(_unwrap(parsed.ohm_inc_qf)),
        "roughness_length_momentum": int(_unwrap(parsed.roughness_length_momentum)),
        "roughness_length_heat": int(_unwrap(parsed.roughness_length_heat)),
        "stability": int(_unwrap(parsed.stability)),
        "soil_moisture_deficit": int(_unwrap(parsed.soil_moisture_deficit)),
        "water_use": int(_unwrap(parsed.water_use)),
        "roughness_sublayer": int(_unwrap(parsed.roughness_sublayer)),
        "frontal_area_index": int(_unwrap(parsed.frontal_area_index)),
        "roughness_sublayer_level": int(_unwrap(parsed.roughness_sublayer_level)),
        "surface_conductance": int(_unwrap(parsed.surface_conductance)),
        "laimethod": int(_unwrap(parsed.laimethod)),
        "snow_use": int(_unwrap(parsed.snow_use)),
        "stebbs.enabled": bool(_unwrap(parsed.stebbs.enabled)),
        "stebbs.parameters": int(_unwrap(parsed.stebbs.parameters)),
        "stebbs.capacitance": int(_unwrap(parsed.stebbs.capacitance)),
        "stebbs.setpoint": int(_unwrap(parsed.stebbs.setpoint)),
        "stebbs.same_albedo_wall": int(_unwrap(parsed.stebbs.same_albedo_wall)),
        "stebbs.same_albedo_roof": int(_unwrap(parsed.stebbs.same_albedo_roof)),
        "stebbs.same_emissivity_wall": int(
            _unwrap(parsed.stebbs.same_emissivity_wall)
        ),
        "stebbs.same_emissivity_roof": int(
            _unwrap(parsed.stebbs.same_emissivity_roof)
        ),
    } == {
        "net_radiation": 3,
        "kdown_split_method": 2,
        "emissions": 2,
        "storage_heat": 1,
        "ohm_inc_qf": 0,
        "roughness_length_momentum": 1,
        "roughness_length_heat": 2,
        "stability": 3,
        "soil_moisture_deficit": 0,
        "water_use": 0,
        "roughness_sublayer": 2,
        "frontal_area_index": 0,
        "roughness_sublayer_level": 1,
        "surface_conductance": 2,
        "laimethod": 1,
        "snow_use": 0,
        "stebbs.enabled": False,
        "stebbs.parameters": 1,
        "stebbs.capacitance": 0,
        "stebbs.setpoint": 0,
        "stebbs.same_albedo_wall": 0,
        "stebbs.same_albedo_roof": 0,
        "stebbs.same_emissivity_wall": 0,
        "stebbs.same_emissivity_roof": 0,
    }


def test_storage_heat_owns_ohm_inc_qf_nested_axis():
    phys = ModelPhysics(storage_heat={"ohm": {"include_qf": True}})

    assert int(_unwrap(phys.storage_heat)) == 1
    assert int(_unwrap(phys.ohm_inc_qf)) == 1


def test_storage_heat_scheme_scoped_include_qf_accepts_yes_no():
    phys = ModelPhysics(storage_heat={"ohm": {"include_qf": "no"}})

    assert int(_unwrap(phys.storage_heat)) == 1
    assert int(_unwrap(phys.ohm_inc_qf)) == 0


def test_storage_heat_legacy_nested_ohm_inc_qf_stays_accepted():
    phys = ModelPhysics(storage_heat={"scheme": "ohm", "ohm_inc_qf": "include"})

    assert int(_unwrap(phys.storage_heat)) == 1
    assert int(_unwrap(phys.ohm_inc_qf)) == 1


def test_storage_heat_nested_ohm_inc_qf_rejects_flat_duplicate():
    with pytest.raises(ValidationError, match="storage_heat\\.ohm\\.include_qf"):
        ModelPhysics(
            storage_heat={"ohm": {"include_qf": False}},
            ohm_inc_qf="include",
        )


def test_storage_heat_scheme_scoped_qf_rejects_sibling_scheme():
    with pytest.raises(
        ValidationError,
        match="storage_heat\\.ohm.*cannot be combined",
    ):
        ModelPhysics(storage_heat={"ohm": {"include_qf": False}, "anohm": {}})


def test_storage_heat_invalid_nested_qf_is_not_partially_folded():
    data = {
        "model": {
            "physics": {
                "storage_heat": {
                    "ohm": {
                        "include_qf": False,
                        "unexpected": True,
                    },
                }
            }
        }
    }
    expected = yaml.safe_load(yaml.safe_dump(data))

    flatten_physics_in_config(data)

    assert data == expected


def test_storage_heat_value_plus_nested_qf_is_not_partially_folded():
    data = {
        "model": {
            "physics": {
                "storage_heat": {
                    "ohm": {
                        "value": 1,
                        "include_qf": False,
                    },
                }
            }
        }
    }
    expected = yaml.safe_load(yaml.safe_dump(data))

    flatten_physics_in_config(data)

    assert data == expected


def test_storage_heat_value_plus_nested_qf_rejected_by_model():
    with pytest.raises(ValidationError, match="storage_heat\\.ohm.*inner keys"):
        ModelPhysics(storage_heat={"ohm": {"value": 1, "include_qf": False}})


def test_model_physics_accepts_orthogonal_spartacus():
    phys = ModelPhysics(net_radiation={"scheme": "spartacus", "ldown": "cloud"})
    assert int(_unwrap(phys.net_radiation)) == 1002


def test_model_physics_orthogonal_dumps_to_flat():
    phys = ModelPhysics(net_radiation={"scheme": "narp", "ldown": "air"})
    dumped = yaml.safe_dump(phys.model_dump(mode="json"))

    assert "scheme" not in dumped
    assert "ldown" not in dumped
    assert phys.model_dump(mode="json")["net_radiation"]["value"] == 3


@pytest.mark.parametrize(
    "payload, message",
    [
        ({"scheme": "forcing", "ldown": "air"}, "sibling keys"),
        ({"scheme": "narp"}, "requires 'ldown'"),
        ({"scheme": "narp", "ldown": "snow"}, "does not support"),
        (
            {"scheme": "narp", "ldown": "air", "variant": "street_canyon"},
            "does not support",
        ),
        (
            {"scheme": "spartacus", "ldown": "air", "variant": "zenith"},
            "sibling keys",
        ),
        ({"scheme": "beers", "ldown": "air"}, "must be one of"),
        ({"scheme": "narp", "ldown": 3}, "non-empty string token"),
    ],
)
def test_orthogonal_net_radiation_rejects_invalid_combinations(payload, message):
    with pytest.raises(ValueError, match=message):
        coerce_orthogonal_to_flat("net_radiation", payload)


def test_model_physics_reports_orthogonal_errors():
    with pytest.raises(ValidationError) as exc:
        ModelPhysics(net_radiation={"scheme": "narp", "ldown": "snow"})
    assert "does not support" in str(exc.value)


@pytest.mark.parametrize(
    "payload, expected",
    [
        ({"heat": "observed"}, 0),
        ({"heat": "l11"}, 1),
        ({"heat": "j11"}, 2),
        ({"heat": "l11_updated"}, 3),
        *[
            (
                {
                    "heat": heat,
                    "co2": {
                        "anthropogenic": anthropogenic,
                        "biogenic": biogenic,
                    },
                },
                offset + heat_code + anthropogenic_offset,
            )
            for biogenic, offset in [
                ("rectangular", 10),
                ("bellucco_local", 20),
                ("bellucco_general", 30),
                ("conductance", 40),
            ]
            for anthropogenic, anthropogenic_offset in [
                ("qf_linked", 0),
                ("detailed", 3),
            ]
            for heat, heat_code in [
                ("l11", 1),
                ("j11", 2),
                ("l11_updated", 3),
            ]
        ],
    ],
)
def test_orthogonal_emissions_collapses(payload, expected):
    assert coerce_orthogonal_to_flat("emissions", payload) == {"value": expected}


def test_orthogonal_emissions_preserves_ref():
    result = coerce_orthogonal_to_flat(
        "emissions",
        {
            "heat": "j11",
            "co2": {"anthropogenic": "detailed", "biogenic": "conductance"},
            "ref": {"doi": "10.example/emissions"},
        },
    )
    assert result == {"value": 45, "ref": {"doi": "10.example/emissions"}}


def test_model_physics_accepts_orthogonal_emissions_and_dumps_flat():
    phys = ModelPhysics(
        emissions={
            "heat": "l11_updated",
            "co2": {"anthropogenic": "detailed", "biogenic": "bellucco_general"},
        }
    )

    assert int(_unwrap(phys.emissions)) == 36
    dumped = yaml.safe_dump(phys.model_dump(mode="json"))
    assert "anthropogenic" not in dumped
    assert "biogenic" not in dumped
    assert phys.model_dump(mode="json")["emissions"]["value"] == 36


def test_read_physics_key_accepts_orthogonal_emissions_legacy_key():
    physics = {
        "emissionsmethod": {
            "heat": "j11",
            "co2": {"anthropogenic": "detailed", "biogenic": "conductance"},
        }
    }

    assert read_physics_key(physics, "emissions") == 45


@pytest.mark.parametrize(
    "payload, message",
    [
        ({"co2": {"anthropogenic": "qf_linked"}}, "requires 'heat'"),
        ({"heat": "observed", "co2": {"biogenic": "rectangular"}}, "observed"),
        (
            {"heat": "j11", "co2": {"anthropogenic": "qf_linked"}},
            "requires a biogenic",
        ),
        (
            {"heat": "j11", "co2": {"biogenic": "rectangular"}},
            "also calculate anthropogenic",
        ),
        ({"heat": "beers"}, "must be one of"),
        (
            {"heat": "j11", "co2": {"anthropogenic": "inventory"}},
            "must be one of",
        ),
        (
            {
                "heat": "j11",
                "co2": {"anthropogenic": "qf_linked", "biogenic": "urban_veg"},
            },
            "must be one of",
        ),
        ({"heat": "j11", "co2": "none"}, "must be a mapping"),
        ({"heat": "j11", "scheme": "simple"}, "sibling keys"),
        (
            {"heat": "j11", "co2": {"biogenic": "rectangular", "source": "site"}},
            "sibling keys",
        ),
    ],
)
def test_orthogonal_emissions_rejects_invalid_combinations(payload, message):
    with pytest.raises(ValueError, match=message):
        coerce_orthogonal_to_flat("emissions", payload)


def test_model_physics_reports_orthogonal_emissions_errors():
    with pytest.raises(ValidationError) as exc:
        ModelPhysics(
            emissions={"heat": "j11", "co2": {"anthropogenic": "qf_linked"}}
        )
    assert "requires a biogenic" in str(exc.value)
