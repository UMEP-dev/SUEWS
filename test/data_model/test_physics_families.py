"""Unit tests for physics_families registry and coerce_nested_to_flat."""

from __future__ import annotations

import pytest

from supy.data_model.core.physics_families import (
    PHYSICS_FAMILIES,
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
        assert set(fams) == {"observed", "ohm", "anohm", "estm", "ehc", "dyohm", "stebbs"}
        assert fams["ohm"] == frozenset({1})
        assert fams["ehc"] == frozenset({5})

    def test_emissions_families(self):
        fams = PHYSICS_FAMILIES["emissions"]
        assert set(fams) == {"observed", "simple"}
        assert fams["simple"] == frozenset({1, 2, 3, 4, 5})

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
        assert coerce_nested_to_flat(
            "snow_use", {"narp": {"value": 1}}
        ) == {"narp": {"value": 1}}

    def test_flat_with_ref_preserved(self):
        payload = {"value": 3, "ref": {"DOI": "10.x/abc"}}
        assert coerce_nested_to_flat("net_radiation", payload) == payload


class TestCoerceNestedHappyPath:
    def test_narp_family_collapses(self):
        result = coerce_nested_to_flat(
            "net_radiation", {"narp": {"value": 3}}
        )
        assert result == {"value": 3}

    def test_spartacus_family_collapses(self):
        result = coerce_nested_to_flat(
            "net_radiation", {"spartacus": {"value": 1001}}
        )
        assert result == {"value": 1001}

    def test_forcing_family_collapses(self):
        result = coerce_nested_to_flat(
            "net_radiation", {"forcing": {"value": 0}}
        )
        assert result == {"value": 0}

    def test_inner_ref_preserved(self):
        result = coerce_nested_to_flat(
            "net_radiation",
            {"spartacus": {"value": 1001, "ref": {"DOI": "10.x/abc"}}},
        )
        assert result == {"value": 1001, "ref": {"DOI": "10.x/abc"}}

    def test_storage_heat_ehc(self):
        assert coerce_nested_to_flat(
            "storage_heat", {"ehc": {"value": 5}}
        ) == {"value": 5}

    def test_emissions_simple_j11(self):
        assert coerce_nested_to_flat(
            "emissions", {"simple": {"value": 2}}
        ) == {"value": 2}


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
            coerce_nested_to_flat(
                "net_radiation", {"narp": {"value": {"nested": 3}}}
            )

    def test_inner_value_not_integerlike_rejected(self):
        with pytest.raises(ValueError, match="must be an integer code"):
            coerce_nested_to_flat(
                "net_radiation", {"narp": {"value": "abc"}}
            )

    def test_code_wrong_family_rejected(self):
        with pytest.raises(ValueError, match="expects one of"):
            coerce_nested_to_flat(
                "net_radiation", {"narp": {"value": 1001}}
            )

    def test_unknown_family_tag_falls_through(self):
        result = coerce_nested_to_flat(
            "net_radiation", {"bogus": {"value": 3}}
        )
        assert result == {"bogus": {"value": 3}}
