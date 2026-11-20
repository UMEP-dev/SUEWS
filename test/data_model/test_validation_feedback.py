"""Public feedback reports expose every error through Pydantic's API."""

import json

from pydantic import ValidationError
import pytest

from supy.data_model.validation.core.feedback import ValidationLevel, ValidationReport

pytestmark = [pytest.mark.api, pytest.mark.cfg]


@pytest.mark.parametrize("level", [None, ValidationLevel.WARNING, ValidationLevel.INFO])
def test_report_without_errors_returns(level):
    """Valid and warning-only reports must not raise."""
    report = ValidationReport()
    if level is not None:
        report.add_issue(level, "model.physics", "physics", "Review this choice")
    assert report.raise_if_errors() is None


def test_report_raises_all_errors_without_invented_inputs():
    """Messages and opaque report locations survive errors() and json()."""
    report = ValidationReport()
    expected = [
        ("model.physics", "Select a supported scheme"),
        ("sites[gridiv=7].properties.lat", "Latitude is outside its range"),
    ]
    report.add_issue(ValidationLevel.WARNING, "model", "model", "Review defaults")
    for location, message in expected:
        report.add_issue(ValidationLevel.ERROR, location, "parameter", message)

    with pytest.raises(ValidationError) as caught:
        report.raise_if_errors()

    error = caught.value
    assert error.error_count() == len(expected)
    assert [(item["loc"], item["msg"]) for item in error.errors()] == [
        ((location,), message) for location, message in expected
    ]
    # None means unavailable here, not that a null input was validated/rejected.
    assert all(item["input"] is None for item in error.errors())
    assert all(item["input"] is None for item in json.loads(error.json()))
    assert all("input" not in item for item in error.errors(include_input=False))
    assert [item["msg"] for item in json.loads(error.json())] == [
        message for _, message in expected
    ]
    assert "Review defaults" not in str(error)
    assert "input_value" not in str(error)
