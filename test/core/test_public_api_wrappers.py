"""Compatibility tests for the one retained procedural forwarding shim."""

from __future__ import annotations

from pathlib import Path
import warnings

import pandas as pd
import pytest

import supy._supy_module as supy_module

pytestmark = pytest.mark.api


class _ForcingResult:
    def __init__(self, df_forcing):
        self._df_forcing = df_forcing

    def to_dataframe(self, include_extras=False):
        assert include_extras is True
        return self._df_forcing


def test_load_forcing_grid_warns_and_forwards_to_simulation(monkeypatch):
    """The UMEP shim delegates YAML loading to ``SUEWSSimulation``."""
    df_forcing = pd.DataFrame({"Tair": [10.0]})
    calls = []

    class FakeSimulation:
        def __init__(self, path_init):
            calls.append(path_init)
            self.forcing = _ForcingResult(df_forcing)

    monkeypatch.setattr("supy.suews_sim.SUEWSSimulation", FakeSimulation)

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        result = supy_module.load_forcing_grid(
            "sample.yml",
            grid=0,
            df_state_init=object(),
        )

    assert result is df_forcing
    assert calls == [Path("sample.yml")]
    assert [item.category for item in caught] == [FutureWarning]
    assert "SUEWSSimulation(path).forcing" in str(caught[0].message)


def test_load_forcing_grid_rejects_namelist_input():
    """The public shim is deliberately limited to the supported YAML workflow."""
    with pytest.warns(FutureWarning):
        with pytest.raises(ValueError, match="YAML configuration"):
            supy_module.load_forcing_grid("RunControl.nml", grid=0)
