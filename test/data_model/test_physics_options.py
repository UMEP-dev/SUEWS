"""Physics-option input forms on ModelPhysics and the coercion helpers.

One file for the whole physics-option input surface:

* nested family-tag form (gh#972), e.g. ``net_radiation: {narp: {value: 3}}``
  -- integration tests on ``ModelPhysics`` plus YAML fixture round-trips
  (formerly ``test_nested_physics.py``);
* the ``physics_families`` registry and ``coerce_nested_to_flat``, including
  readable scheme names and enum parity
  (formerly ``test_physics_families.py``);
* the orthogonal form, e.g. ``{scheme: narp, ldown: air}`` via
  ``coerce_orthogonal_to_flat``
  (formerly ``test_physics_orthogonal.py``).
"""

from __future__ import annotations

from pathlib import Path

from pydantic import ValidationError
import pytest
import yaml

from supy.data_model.core.field_renames import read_physics_key
from supy.data_model.core.model import ModelPhysics, NetRadiationMethod
from supy.data_model.core.physics_families import (
    MODEL_PHYSICS_ENUM_FIELDS,
    PHYSICS_ENUM_FIELDS,
    PHYSICS_FAMILIES,
    STEBBS_PHYSICS_ENUM_FIELDS,
    coerce_nested_to_flat,
    flatten_physics_in_config,
)
from supy.data_model.core.physics_orthogonal import coerce_orthogonal_to_flat

pytestmark = pytest.mark.api


_FIXTURES = Path(__file__).parent.parent / "fixtures" / "nested_physics"


def _unwrap(v):
    return v.value if hasattr(v, "value") else v


# ===========================================================================
# Nested family-tag form (gh#972) -- formerly test_nested_physics.py
# ===========================================================================

class TestModelPhysicsNested:
    def test_flat_form_still_works(self):
        phys = ModelPhysics(net_radiation={"value": 3})
        assert NetRadiationMethod(int(_unwrap(phys.net_radiation))) is NetRadiationMethod.LDOWN_AIR

    def test_nested_spartacus_form_accepted(self):
        phys = ModelPhysics(net_radiation={"spartacus": {"value": 1001}})
        assert int(_unwrap(phys.net_radiation)) == 1001

    def test_nested_storage_heat_ehc(self):
        phys = ModelPhysics(storage_heat={"ehc": {"value": 5}})
        assert int(_unwrap(phys.storage_heat)) == 5

    def test_nested_emissions_simple(self):
        phys = ModelPhysics(emissions={"simple": {"value": 2}})
        assert int(_unwrap(phys.emissions)) == 2

    def test_wrong_family_rejected_at_validation(self):
        with pytest.raises(ValidationError) as exc:
            ModelPhysics(net_radiation={"narp": {"value": 1001}})
        assert "expects one of" in str(exc.value)

    def test_non_integral_nested_code_rejected(self):
        with pytest.raises(ValidationError) as exc:
            ModelPhysics(net_radiation={"narp": {"value": 3.7}})
        assert "must be an integer code" in str(exc.value)

    def test_stringified_nested_code_rejected(self):
        with pytest.raises(ValidationError) as exc:
            ModelPhysics(net_radiation={"narp": {"value": "3"}})
        assert "must be an integer code" in str(exc.value)

    def test_non_registered_field_unaffected(self):
        phys = ModelPhysics(snow_use={"value": 1})
        assert int(_unwrap(phys.snow_use)) == 1


class TestYamlRoundTrip:
    def test_flat_yaml_loads(self):
        cfg = yaml.safe_load((_FIXTURES / "flat.yml").read_text(encoding="utf-8"))
        phys = ModelPhysics(**cfg["model"]["physics"])
        assert int(_unwrap(phys.net_radiation)) == 1001

    def test_nested_yaml_loads_to_same_internal_state(self):
        flat_cfg = yaml.safe_load((_FIXTURES / "flat.yml").read_text(encoding="utf-8"))
        nested_cfg = yaml.safe_load((_FIXTURES / "nested.yml").read_text(encoding="utf-8"))

        flat = ModelPhysics(**flat_cfg["model"]["physics"])
        nested = ModelPhysics(**nested_cfg["model"]["physics"])

        # Family tag is accept-only — internal representation must match.
        assert flat.model_dump(mode="json") == nested.model_dump(mode="json")

    def test_nested_yaml_dumps_to_flat(self):
        nested_cfg = yaml.safe_load((_FIXTURES / "nested.yml").read_text(encoding="utf-8"))
        phys = ModelPhysics(**nested_cfg["model"]["physics"])

        dumped_text = yaml.safe_dump(phys.model_dump(mode="json"))
        # Round-trip is flat: no family tags in the dump.
        assert "spartacus" not in dumped_text
        assert "ehc" not in dumped_text
        assert "simple:" not in dumped_text

    def test_mixed_reject_yaml_raises(self):
        cfg = yaml.safe_load((_FIXTURES / "mixed_reject.yml").read_text(encoding="utf-8"))
        with pytest.raises(ValidationError) as exc:
            ModelPhysics(**cfg["model"]["physics"])
        assert "expects one of" in str(exc.value)

# ===========================================================================
# physics_families registry and coerce_nested_to_flat -- formerly
# test_physics_families.py
# ===========================================================================

class TestRegistryShape:
    def test_exemplar_fields_present(self):
        assert set(PHYSICS_FAMILIES) >= {"net_radiation", "storage_heat", "emissions"}

    def test_families_disjoint(self):
        for field_name, fams in PHYSICS_FAMILIES.items():
            seen: dict[int, str] = {}
            for fam, codes in fams.items():
                for c in codes:
                    assert c not in seen, (
                        f"{field_name}: code {c} in both {seen[c]!r} and {fam!r}"
                    )
                    seen[c] = fam


class TestCoerceFlatPassthrough:
    def test_flat_refvalue_dict_passes_through(self):
        assert coerce_nested_to_flat("net_radiation", {"value": 3}) == {"value": 3}

    def test_bare_int_passes_through(self):
        assert coerce_nested_to_flat("net_radiation", 3) == 3

    def test_none_passes_through(self):
        assert coerce_nested_to_flat("net_radiation", None) is None

    def test_unknown_field_passes_through(self):
        assert coerce_nested_to_flat("snow_use", {"narp": {"value": 1}}) == {
            "narp": {"value": 1}
        }

    def test_flat_with_ref_preserved(self):
        payload = {"value": 3, "ref": {"DOI": "10.x/abc"}}
        assert coerce_nested_to_flat("net_radiation", payload) == payload


class TestCoerceNestedHappyPath:
    def test_narp_family_collapses(self):
        result = coerce_nested_to_flat("net_radiation", {"narp": {"value": 3}})
        assert result == {"value": 3}

    def test_spartacus_family_collapses(self):
        result = coerce_nested_to_flat("net_radiation", {"spartacus": {"value": 1001}})
        assert result == {"value": 1001}

    def test_forcing_family_collapses(self):
        result = coerce_nested_to_flat("net_radiation", {"forcing": {"value": 0}})
        assert result == {"value": 0}

    def test_inner_ref_preserved(self):
        result = coerce_nested_to_flat(
            "net_radiation",
            {"spartacus": {"value": 1001, "ref": {"DOI": "10.x/abc"}}},
        )
        assert result == {"value": 1001, "ref": {"DOI": "10.x/abc"}}

    def test_storage_heat_ehc(self):
        assert coerce_nested_to_flat("storage_heat", {"ehc": {"value": 5}}) == {
            "value": 5
        }

    def test_emissions_simple_hidden_l11_updated_detailed(self):
        assert coerce_nested_to_flat("emissions", {"simple": {"value": 6}}) == {
            "value": 6
        }

    def test_emissions_biogenic_rectangular(self):
        assert coerce_nested_to_flat(
            "emissions", {"biogenic_rectangular": {"value": 16}}
        ) == {"value": 16}

    def test_integral_float_matches_flat_acceptance(self):
        assert coerce_nested_to_flat("net_radiation", {"narp": {"value": 3.0}}) == {
            "value": 3
        }


class TestCoerceErrorPaths:
    def test_multiple_family_tags_rejected(self):
        with pytest.raises(ValueError, match="multiple family tags"):
            coerce_nested_to_flat(
                "net_radiation",
                {"narp": {"value": 3}, "spartacus": {"value": 1001}},
            )

    def test_family_with_sibling_key_rejected(self):
        with pytest.raises(ValueError, match="sibling keys"):
            coerce_nested_to_flat(
                "net_radiation",
                {"narp": {"value": 3}, "value": 1001},
            )

    def test_inner_missing_value_rejected(self):
        with pytest.raises(ValueError, match="must be a mapping with a 'value' key"):
            coerce_nested_to_flat("net_radiation", {"narp": {}})

    def test_inner_not_a_mapping_rejected(self):
        with pytest.raises(ValueError, match="must be a mapping with a 'value' key"):
            coerce_nested_to_flat("net_radiation", {"narp": 3})

    def test_inner_value_is_dict_rejected(self):
        with pytest.raises(ValueError, match="must be a scalar numeric code"):
            coerce_nested_to_flat("net_radiation", {"narp": {"value": {"nested": 3}}})

    def test_inner_foreign_key_rejected(self):
        with pytest.raises(ValueError, match="inner keys"):
            coerce_nested_to_flat(
                "storage_heat",
                {"ohm": {"value": 1, "include_qf": False}},
            )

    def test_non_string_inner_foreign_key_rejected(self):
        with pytest.raises(ValueError, match="inner keys"):
            coerce_nested_to_flat("storage_heat", {"ohm": {"value": 1, 42: False}})

    def test_inner_value_not_integerlike_rejected(self):
        with pytest.raises(ValueError, match="must be an integer code"):
            coerce_nested_to_flat("net_radiation", {"narp": {"value": "abc"}})

    def test_stringified_integer_rejected(self):
        with pytest.raises(ValueError, match="must be an integer code"):
            coerce_nested_to_flat("net_radiation", {"narp": {"value": "3"}})

    def test_non_integral_float_rejected(self):
        with pytest.raises(ValueError, match="must be an integer code"):
            coerce_nested_to_flat("net_radiation", {"narp": {"value": 3.7}})

    def test_code_wrong_family_rejected(self):
        with pytest.raises(ValueError, match="expects one of"):
            coerce_nested_to_flat("net_radiation", {"narp": {"value": 1001}})

    def test_unknown_family_tag_falls_through(self):
        result = coerce_nested_to_flat("net_radiation", {"bogus": {"value": 3}})
        assert result == {"bogus": {"value": 3}}


class TestCoerceScalarNames:
    """Human-readable scalar names and bibliographic codes."""

    def test_family_tag_name_resolves(self):
        assert coerce_nested_to_flat("storage_heat", "ohm") == {"value": 1}
        assert coerce_nested_to_flat("storage_heat", "ehc") == {"value": 5}
        assert coerce_nested_to_flat("storage_heat", "dyohm_building") == {
            "value": 8
        }

    def test_enum_member_name_resolves(self):
        assert coerce_nested_to_flat("storage_heat", "ohm_without_qf") == {"value": 1}
        assert coerce_nested_to_flat("stability", "campbell_norman") == {"value": 3}

    def test_bibliographic_code_resolves(self):
        assert coerce_nested_to_flat("stability", "cn98") == {"value": 3}
        assert coerce_nested_to_flat("roughness_length_heat", "k09") == {"value": 2}
        assert coerce_nested_to_flat("roughness_length_momentum", "m98") == {"value": 3}

    def test_name_is_case_insensitive(self):
        assert coerce_nested_to_flat("storage_heat", "OHM") == {"value": 1}
        assert coerce_nested_to_flat("stability", "CN98") == {"value": 3}

    def test_refvalue_string_resolves_and_preserves_ref(self):
        payload = {"value": "provided", "ref": {"DOI": "10.x/ref"}}
        assert coerce_nested_to_flat("capacitance", payload) == {
            "value": 1,
            "ref": {"DOI": "10.x/ref"},
        }

    def test_nested_stebbs_names_resolve(self):
        assert coerce_nested_to_flat("parameters", "provided") == {"value": 2}
        assert coerce_nested_to_flat("capacitance", "parameterise") == {"value": 2}
        assert coerce_nested_to_flat("same_albedo_wall", "enabled") == {"value": 1}

    def test_emissions_current_hidden_and_biogenic_names_resolve(self):
        assert coerce_nested_to_flat("emissions", "l11_updated_detailed") == {
            "value": 6
        }
        assert coerce_nested_to_flat(
            "emissions", "biogen_bellucco_general_l11_updated_detailed"
        ) == {"value": 36}

    def test_net_radiation_enum_name_scalar_resolves(self):
        assert coerce_nested_to_flat("net_radiation", "ldown_air") == {"value": 3}
        assert coerce_nested_to_flat("net_radiation", "forcing") == {"value": 0}

    def test_source_choice_names_are_consistent(self):
        assert coerce_nested_to_flat("laimethod", "modelled") == {"value": 1}
        assert coerce_nested_to_flat("frontal_area_index", "observed") == {
            "value": 0
        }
        assert coerce_nested_to_flat("frontal_area_index", "modelled") == {
            "value": 1
        }
        assert coerce_nested_to_flat("water_use", "modelled") == {"value": 0}
        assert coerce_nested_to_flat("soil_moisture_deficit", "modelled") == {
            "value": 0
        }
        assert coerce_nested_to_flat("soil_moisture_deficit", "observed") == {
            "value": 1
        }

    def test_lai_calculated_name_is_not_public(self):
        with pytest.raises(ValueError, match="calculated"):
            coerce_nested_to_flat("laimethod", "calculated")

    def test_source_choice_model_name_is_not_public(self):
        with pytest.raises(ValueError, match="modelled"):
            coerce_nested_to_flat("laimethod", "model")

    @pytest.mark.parametrize("name", ["observed_volumetric", "observed_gravimetric"])
    def test_smd_unit_specific_names_are_not_public(self, name):
        with pytest.raises(ValueError, match="observed"):
            coerce_nested_to_flat("soil_moisture_deficit", name)

    def test_unknown_scalar_name_rejected(self):
        with pytest.raises(ValueError, match="unknown scheme name"):
            coerce_nested_to_flat("storage_heat", "not_a_scheme")

    def test_unknown_scalar_name_lists_valid_names(self):
        with pytest.raises(ValueError, match="campbell_norman"):
            coerce_nested_to_flat("stability", "bogus")

class TestReadableNamesInModels:
    def test_model_physics_accepts_top_level_readable_names(self):
        from supy.data_model.core.model import ModelPhysics

        physics = ModelPhysics(
            storage_heat="ohm",
            stability="cn98",
            snow="enabled",
            leaf_area_index="modelled",
            emissions="biogen_conductance_j11_detailed",
        )

        assert physics.storage_heat.value.value == 1
        assert physics.stability.value.value == 3
        assert physics.snow_use.value.value == 1
        assert physics.laimethod.value.value == 1
        assert physics.emissions.value.value == 45

    def test_public_physics_key_aliases_reject_duplicates(self):
        from supy.data_model.core.model import ModelPhysics

        with pytest.raises(ValueError, match="leaf_area_index"):
            ModelPhysics(leaf_area_index="modelled", laimethod="modelled")
        with pytest.raises(ValueError, match="snow"):
            ModelPhysics(snow="disabled", snow_use="disabled")

    def test_model_physics_accepts_nested_stebbs_readable_names(self):
        from supy.data_model.core.model import ModelPhysics

        physics = ModelPhysics(
            stebbs={
                "enabled": True,
                "parameter_source": "provided",
                "capacitance": "parameterise",
                "setpoint": {"value": "scheduled"},
                "same_albedo_wall": "enabled",
            }
        )

        assert physics.stebbs.parameters.value.value == 2
        assert physics.stebbs.capacitance.value.value == 2
        assert physics.stebbs.setpoint.value.value == 2
        assert physics.stebbs.same_albedo_wall.value.value == 1

    def test_nested_stebbs_public_alias_rejects_duplicate(self):
        from supy.data_model.core.model import ModelPhysics

        with pytest.raises(ValueError, match="parameter_source"):
            ModelPhysics(
                stebbs={"parameter_source": "default", "parameters": "default"}
            )


class TestRegistryEnumParity:
    """The hardcoded readable-name codes must match the live Enum definitions."""

    def test_codes_match_enum_values(self):
        from supy.data_model.core import model as m, physics_families as pf

        field_to_enum = {
            "net_radiation": m.NetRadiationMethod,
            "kdown_split_method": m.KdownSplitMethod,
            "emissions": m.EmissionsMethod,
            "storage_heat": m.StorageHeatMethod,
            "ohm_inc_qf": m.OhmIncQf,
            "roughness_length_momentum": m.MomentumRoughnessMethod,
            "roughness_length_heat": m.HeatRoughnessMethod,
            "stability": m.StabilityMethod,
            "soil_moisture_deficit": m.SMDMethod,
            "water_use": m.WaterUseMethod,
            "laimethod": m.LAIMethod,
            "roughness_sublayer": m.RSLMethod,
            "frontal_area_index": m.FAIMethod,
            "roughness_sublayer_level": m.RSLLevel,
            "surface_conductance": m.GSModel,
            "snow_use": m.SnowUse,
            "stebbs": m.StebbsMethod,
            "parameters": m.StebbsParameterSource,
            "capacitance": m.RCMethod,
            "setpoint": m.SetpointMethod,
            "same_albedo_wall": m.SameAlbedoWall,
            "same_albedo_roof": m.SameAlbedoRoof,
            "same_emissivity_wall": m.SameEmissivityWall,
            "same_emissivity_roof": m.SameEmissivityRoof,
        }

        assert set(field_to_enum) == set(PHYSICS_ENUM_FIELDS)
        assert set(MODEL_PHYSICS_ENUM_FIELDS).issubset(PHYSICS_ENUM_FIELDS)
        assert set(STEBBS_PHYSICS_ENUM_FIELDS).issubset(PHYSICS_ENUM_FIELDS)

        # gh#1422/#1447: SMD's historical code 2 remains a numeric
        # compatibility path while the public readable surface converges on a
        # single observed soil-moisture source.
        public_name_coverage_exceptions = {"soil_moisture_deficit": {2}}

        for field, enum_cls in field_to_enum.items():
            valid = {member.value for member in enum_cls}
            for code in pf._ALIAS_TO_CODE[field].values():
                assert code in valid, f"{field}: code {code} not in {enum_cls}"
            assert (
                set(pf._CODE_TO_CANONICAL[field])
                == valid - public_name_coverage_exceptions.get(field, set())
            ), (
                f"{field}: canonical names do not cover every enum value"
            )

# ===========================================================================
# Orthogonal input form -- formerly test_physics_orthogonal.py
# ===========================================================================

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
    assert physics["kdown_split_method"] == {
        "constant": {"sw_dn_direct_frac": 0.45}
    }
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
    assert float(_unwrap(parsed.sw_dn_direct_frac)) == 0.45
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


def test_flatten_physics_accepts_kdown_constant_direct_fraction():
    data = {
        "model": {
            "physics": {
                "kdown_split_method": {
                    "constant": {"sw_dn_direct_frac": 0.45}
                }
            }
        }
    }

    flatten_physics_in_config(data)

    assert data["model"]["physics"] == {"kdown_split_method": {"value": 2}}


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
