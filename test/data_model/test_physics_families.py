"""Unit tests for physics_families registry and coerce_nested_to_flat."""

from __future__ import annotations

import pytest

from supy.data_model.core.physics_families import (
    MODEL_PHYSICS_ENUM_FIELDS,
    PHYSICS_ENUM_FIELDS,
    PHYSICS_FAMILIES,
    STEBBS_PHYSICS_ENUM_FIELDS,
    coerce_nested_to_flat,
)

pytestmark = pytest.mark.api


class TestRegistryShape:
    def test_exemplar_fields_present(self):
        assert set(PHYSICS_FAMILIES) >= {"net_radiation", "storage_heat", "emissions"}

    def test_net_radiation_families(self):
        fams = PHYSICS_FAMILIES["net_radiation"]
        assert set(fams) == {"forcing", "narp", "spartacus"}
        assert 0 in fams["forcing"]
        assert 3 in fams["narp"]
        assert 1001 in fams["spartacus"]

    def test_storage_heat_families(self):
        fams = PHYSICS_FAMILIES["storage_heat"]
        assert set(fams) == {
            "observed",
            "ohm",
            "anohm",
            "estm",
            "ehc",
            "dyohm",
            "stebbs",
        }
        assert fams["ohm"] == frozenset({1})
        assert fams["ehc"] == frozenset({5})

    def test_emissions_families(self):
        fams = PHYSICS_FAMILIES["emissions"]
        assert set(fams) == {
            "observed",
            "simple",
            "biogenic_rectangular",
            "biogenic_bellucco_local",
            "biogenic_bellucco_general",
            "biogenic_conductance",
        }
        assert fams["simple"] == frozenset({1, 2, 3, 4, 5, 6})
        assert fams["biogenic_rectangular"] == frozenset(range(11, 17))
        assert fams["biogenic_bellucco_general"] == frozenset(range(31, 37))

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
        assert coerce_nested_to_flat("laimethod", "model") == {"value": 1}
        assert coerce_nested_to_flat("frontal_area_index", "observed") == {
            "value": 0
        }
        assert coerce_nested_to_flat("frontal_area_index", "model") == {"value": 1}
        assert coerce_nested_to_flat("water_use", "model") == {"value": 0}
        assert coerce_nested_to_flat("soil_moisture_deficit", "model") == {
            "value": 0
        }

    def test_lai_calculated_name_is_not_public(self):
        with pytest.raises(ValueError, match="calculated"):
            coerce_nested_to_flat("laimethod", "calculated")

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
            leaf_area_index="model",
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
            ModelPhysics(leaf_area_index="model", laimethod="model")
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

        for field, enum_cls in field_to_enum.items():
            valid = {member.value for member in enum_cls}
            for code in pf._ALIAS_TO_CODE[field].values():
                assert code in valid, f"{field}: code {code} not in {enum_cls}"
            assert set(pf._CODE_TO_CANONICAL[field]) == valid, (
                f"{field}: canonical names do not cover every enum value"
            )
