"""Integration tests: nested physics sub-options on ModelPhysics (gh#972)."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from supy.data_model.core.model import ModelPhysics, NetRadiationMethod

pytestmark = pytest.mark.api


_FIXTURES = Path(__file__).parent.parent / "fixtures" / "nested_physics"


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


class TestYamlRoundTrip:
    def test_flat_yaml_loads(self):
        cfg = yaml.safe_load((_FIXTURES / "flat.yml").read_text())
        phys = ModelPhysics(**cfg["model"]["physics"])
        assert int(_unwrap(phys.net_radiation)) == 1001

    def test_nested_yaml_loads_to_same_internal_state(self):
        flat_cfg = yaml.safe_load((_FIXTURES / "flat.yml").read_text())
        nested_cfg = yaml.safe_load((_FIXTURES / "nested.yml").read_text())

        flat = ModelPhysics(**flat_cfg["model"]["physics"])
        nested = ModelPhysics(**nested_cfg["model"]["physics"])

        # Family tag is accept-only — internal representation must match.
        assert flat.model_dump(mode="json") == nested.model_dump(mode="json")

    def test_nested_yaml_dumps_to_flat(self):
        nested_cfg = yaml.safe_load((_FIXTURES / "nested.yml").read_text())
        phys = ModelPhysics(**nested_cfg["model"]["physics"])

        dumped_text = yaml.safe_dump(phys.model_dump(mode="json"))
        # Round-trip is flat: no family tags in the dump.
        assert "spartacus" not in dumped_text
        assert "ehc" not in dumped_text
        assert "simple:" not in dumped_text

    def test_mixed_reject_yaml_raises(self):
        cfg = yaml.safe_load((_FIXTURES / "mixed_reject.yml").read_text())
        with pytest.raises(ValidationError) as exc:
            ModelPhysics(**cfg["model"]["physics"])
        assert "expects one of" in str(exc.value)
