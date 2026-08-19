"""The Phase-2 seams are interfaces today: one realised reader, sketched rest."""

import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from reference_source import ReferenceSource, LocalBenchmarkRef, ExternalLegacyRef  # noqa: E402
from engine_provider import EngineProvider, PinnedVenvProvider, RustBridgeProvider  # noqa: E402

INDEX = {"versions": {"2026.6.5": {"full": {"QH": {"MAE": 39.5}}}}}


def test_local_reference_resolves_a_block():
    ref = LocalBenchmarkRef(INDEX)
    assert isinstance(ref, ReferenceSource)
    assert ref.stats_for("2026.6.5")["full"]["QH"]["MAE"] == 39.5


def test_external_legacy_is_not_yet_implemented():
    with pytest.raises(NotImplementedError):
        ExternalLegacyRef("cases/London_KCL/v2020a").stats_for("2020a")


def test_engine_providers_are_phase2_stubs():
    assert issubclass(PinnedVenvProvider, EngineProvider)
    assert issubclass(RustBridgeProvider, EngineProvider)
    with pytest.raises(NotImplementedError):
        PinnedVenvProvider().run("2026.4.3", config=None, forcing=None)
    with pytest.raises(NotImplementedError):
        RustBridgeProvider().run("2026.4.3", config=None, forcing=None)
