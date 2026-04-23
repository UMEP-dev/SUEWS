"""Integration tests: nested physics sub-options on ModelPhysics (gh#972)."""

from __future__ import annotations

import pytest
from pydantic import ValidationError

from supy.data_model.core.model import ModelPhysics, NetRadiationMethod


def _unwrap(v):
    return v.value if hasattr(v, "value") else v


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

    def test_non_registered_field_unaffected(self):
        phys = ModelPhysics(snow_use={"value": 1})
        assert int(_unwrap(phys.snow_use)) == 1
