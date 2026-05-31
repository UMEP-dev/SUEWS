"""Tests for orthogonal physics input forms."""

from __future__ import annotations

from pydantic import ValidationError
import pytest
import yaml

from supy.data_model.core.model import ModelPhysics
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
