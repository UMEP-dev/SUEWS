"""Established kernel bounds apply to scalar and reference-wrapped inputs."""

from pydantic import ValidationError
import pytest

from supy.data_model.core.model import ModelControl
from supy.data_model.core.surface import VerticalLayers
from supy.data_model.core.type import NULL_REJECTED_FIELDS, RefValue

pytestmark = pytest.mark.api


@pytest.mark.parametrize(
    ("value", "message"),
    [
        (None, "tstep"),
        ({"value": None}, "tstep needs a number; null is not accepted"),
        (RefValue[int](value=None), "tstep needs a number; null is not accepted"),
    ],
    ids=["plain-null", "wrapped-null", "refvalue-null"],
)
def test_null_tstep_is_a_validation_error(value, message):
    """Null rejection must precede the arithmetic validator."""
    with pytest.raises(ValidationError, match=message):
        ModelControl(tstep=value)


def test_tstep_is_registered_as_null_rejected():
    """The validation pipeline must not insert null placeholders for tstep."""
    assert "tstep" in NULL_REJECTED_FIELDS


@pytest.mark.parametrize("wrapped", [False, True])
@pytest.mark.parametrize("value", [-999, -1, 0, 7, 3601, 7200])
def test_invalid_tstep(value, wrapped):
    """Reject timesteps before output-frequency modulo or native arithmetic."""
    with pytest.raises(ValidationError, match="tstep must be positive"):
        ModelControl(tstep={"value": value} if wrapped else value)


@pytest.mark.parametrize("wrapped", [False, True])
@pytest.mark.parametrize("value", [1, 60, 300, 900, 3600])
def test_valid_tstep(value, wrapped):
    """Keep supported timesteps unchanged."""
    control = ModelControl(tstep={"value": value} if wrapped else value)
    assert getattr(control.tstep, "value", control.tstep) == value


@pytest.mark.parametrize("wrapped", [False, True])
@pytest.mark.parametrize("value", [-999, -1, 0, 16, 4294967295])
def test_invalid_nlayer(value, wrapped):
    """The Python boundary already enforces the native layer limit."""
    with pytest.raises(ValidationError):
        VerticalLayers(nlayer={"value": value} if wrapped else value)


@pytest.mark.parametrize("value", [1, 15])
def test_valid_nlayer(value):
    """Both endpoints remain representable by the layer model."""
    layers = VerticalLayers(nlayer=value)
    assert getattr(layers.nlayer, "value", layers.nlayer) == value
