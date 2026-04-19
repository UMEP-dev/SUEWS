"""Regression tests for the moisture-aware FLUXNET diagnostic harness."""

from __future__ import annotations

import importlib.util
import sys
import types
from pathlib import Path

import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]
SCRIPT_PATH = REPO_ROOT / "scripts" / "verify" / "moisture_phenology_site.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "moisture_phenology_site", SCRIPT_PATH
)
moisture_phenology_site = importlib.util.module_from_spec(SCRIPT_SPEC)
assert SCRIPT_SPEC.loader is not None
SCRIPT_SPEC.loader.exec_module(moisture_phenology_site)


def test_load_forcing_preserves_subhourly_index_and_filters_year(monkeypatch) -> None:
    """load_forcing() should rely on SuPy parsing and keep minute resolution intact."""

    idx = pd.DatetimeIndex(
        ["2010-01-01 00:00:00", "2010-01-01 00:30:00", "2011-01-01 00:00:00"],
        name="datetime",
    )
    fake_df = pd.DataFrame({"lai": [1.0, 1.2, 0.8]}, index=idx)

    fake_util = types.ModuleType("supy.util")
    fake_util.read_forcing = lambda path, tstep_mod=300: fake_df
    fake_supy = types.ModuleType("supy")
    fake_supy.util = fake_util

    monkeypatch.setitem(sys.modules, "supy", fake_supy)
    monkeypatch.setitem(sys.modules, "supy.util", fake_util)

    loaded = moisture_phenology_site.load_forcing(Path("dummy.txt"), year=2010)

    assert loaded.index.tolist() == idx[:2].tolist()
    assert loaded.index[1].minute == 30


def test_run_scenario_uses_requested_forcing_and_dates(monkeypatch) -> None:
    """run_scenario() must pass the caller's forcing and run window through to SuPy."""

    calls: dict[str, object] = {}
    forcing = pd.DataFrame(
        {"lai": [1.0, 1.1]},
        index=pd.DatetimeIndex(
            ["2012-07-01 00:00:00", "2012-07-01 00:05:00"], name="datetime"
        ),
    )
    state_columns = pd.MultiIndex.from_tuples(moisture_phenology_site.LAI_TYPE_COLS)

    class FakeScenario:
        def __init__(self, state_init: pd.DataFrame | None = None) -> None:
            self.state_init = (
                state_init
                if state_init is not None
                else pd.DataFrame([[0, 0, 0]], columns=state_columns)
            )

        @classmethod
        def from_sample_data(cls):
            return cls()

        @classmethod
        def from_state(cls, state: pd.DataFrame):
            calls["state"] = state.copy()
            return cls(state_init=state.copy())

        def update_forcing(self, forcing_data: pd.DataFrame):
            calls["forcing"] = forcing_data.copy()
            return self

        def run(self, start_date=None, end_date=None):
            calls["start_date"] = start_date
            calls["end_date"] = end_date
            output_index = pd.MultiIndex.from_product(
                [[1], pd.DatetimeIndex(["2012-07-01 00:00:00"])],
                names=["grid", "datetime"],
            )
            return types.SimpleNamespace(
                SUEWS=pd.DataFrame({"LAI": [1.5]}, index=output_index)
            )

    fake_supy = types.ModuleType("supy")
    fake_supy.SUEWSSimulation = FakeScenario
    monkeypatch.setitem(sys.modules, "supy", fake_supy)

    start_date = forcing.index.min()
    end_date = forcing.index.max()
    result = moisture_phenology_site.run_scenario(
        2, forcing, start_date=start_date, end_date=end_date
    )

    assert calls["forcing"].equals(forcing)
    assert calls["start_date"] == start_date
    assert calls["end_date"] == end_date
    assert (calls["state"].loc[:, moisture_phenology_site.LAI_TYPE_COLS] == 2).all().all()
    assert list(result.columns) == ["LAI"]
