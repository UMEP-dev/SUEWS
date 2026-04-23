"""Unit tests for physics_families registry and coerce_nested_to_flat."""

from __future__ import annotations

import pytest

from supy.data_model.core.physics_families import (
    PHYSICS_FAMILIES,
    coerce_nested_to_flat,
)


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
        assert set(fams) == {
            "observed", "simple", "biogen_rect", "biogen_nrect", "biogen_cond",
        }

    def test_families_disjoint(self):
        for field_name, fams in PHYSICS_FAMILIES.items():
            seen: dict[int, str] = {}
            for fam, codes in fams.items():
                for c in codes:
                    assert c not in seen, (
                        f"{field_name}: code {c} in both {seen[c]!r} and {fam!r}"
                    )
                    seen[c] = fam
