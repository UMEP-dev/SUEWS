"""Tests for forcing/output version ownership."""

import pytest

from supy.data_model.forcing.version import (
    CURRENT_FORCING_VERSION,
    FORCING_VERSIONS,
)
from supy.data_model.output.version import CURRENT_OUTPUT_VERSION, OUTPUT_VERSIONS

pytestmark = pytest.mark.api


def test_forcing_and_output_have_initial_releases() -> None:
    assert CURRENT_FORCING_VERSION == "1.2.0"
    assert list(FORCING_VERSIONS) == ["1.0.0", "1.1.0", "1.2.0"]
    assert all(digest.startswith("sha256:") for digest in FORCING_VERSIONS.values())
    assert CURRENT_OUTPUT_VERSION == "1.1.0"
    assert list(OUTPUT_VERSIONS) == ["1.0.0", "1.1.0"]
    assert OUTPUT_VERSIONS["1.0.0"].startswith("sha256:")
    assert OUTPUT_VERSIONS["1.1.0"].startswith("sha256:")


def test_forcing_and_output_own_separate_version_histories() -> None:
    assert FORCING_VERSIONS is not OUTPUT_VERSIONS
