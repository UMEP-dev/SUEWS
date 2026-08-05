"""Tests for forcing/output version ownership."""

import pytest

from supy.data_model.forcing.version import (
    CURRENT_FORCING_VERSION,
    FORCING_VERSIONS,
)
from supy.data_model.output.version import CURRENT_OUTPUT_VERSION, OUTPUT_VERSIONS

pytestmark = pytest.mark.api


def test_forcing_and_output_versions_start_unpublished() -> None:
    assert CURRENT_FORCING_VERSION is None
    assert CURRENT_OUTPUT_VERSION is None
    assert FORCING_VERSIONS == {}
    assert OUTPUT_VERSIONS == {}


def test_forcing_and_output_own_separate_version_histories() -> None:
    assert FORCING_VERSIONS is not OUTPUT_VERSIONS
