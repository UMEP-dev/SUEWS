"""Wet-day water-balance conservation regressions for waterdist validation (gh#2086).

Impervious surfaces (``paved``, ``bldgs``) route remaining drainage to
``to_runoff`` and pervious surfaces (``evetr``, ``dectr``, ``grass``, ``bsoil``)
route it to ``to_soilstore``. Both destinations map onto slot 8 of the kernel's
``WaterDist`` matrix. Previously, supplying a disallowed ``to_soilstore`` entry
(even ``0.0`` or ``0.5``) alongside valid ``to_runoff`` on ``paved`` passed
``SUEWSConfig.from_yaml()`` and shadowed ``to_runoff`` in the native YAML
reader, breaking wet-day water-balance closure by several millimetres.

These tests exercise the public YAML -> ``SUEWSSimulation`` path on the first
wet day of the bundled sample (2012-01-01, Rain > 10 mm) to verify that every
configuration either refuses disallowed/incomplete water distributions up front
or conserves water and reproduces the intact control runoff.
"""

from __future__ import annotations

from copy import deepcopy
from importlib.resources import files
from pathlib import Path

from conftest import TIMESTEPS_PER_DAY, load_sample_frames
import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation

pytestmark = pytest.mark.physics

GRID = 1
WATER_CLOSURE_TOLERANCE_MM = 1e-4


def _load_sample_raw() -> dict:
    return yaml.safe_load(
        (files("supy") / "sample_data" / "sample_config.yml").read_text(
            encoding="utf-8"
        )
    )


def _run_wet_day(
    raw: dict, df_forcing: pd.DataFrame, path_yaml: Path
) -> tuple[SUEWSSimulation, pd.DataFrame]:
    """Write ``raw`` to ``path_yaml`` and run one wet day via ``SUEWSSimulation``."""
    path_yaml.write_text(yaml.safe_dump(raw), encoding="utf-8")
    sim = SUEWSSimulation()
    sim.update_config(path_yaml, auto_load_forcing=False)
    sim.update_forcing(df_forcing)
    output = sim.run(end_date=df_forcing.index[-1])
    return sim, output.df


def _water_balance_residual_mm(
    sim: SUEWSSimulation, df_output: pd.DataFrame
) -> pd.Series:
    """Per-timestep water-balance residual (dTotalStore - (Rain + Irr - Evap - RO)) [mm]."""
    suews = df_output.loc[GRID, "SUEWS"]
    df_soil_store = df_output.loc[GRID, "debug"].filter(regex=r"^ss_.*_next$")
    soil_store = df_soil_store.dot(sim.state_init.sfr_surf.iloc[0].values)
    total_store = soil_store + suews["State"]
    net_input = suews["Rain"] + suews["Irr"] - suews["Evap"] - suews["RO"]
    return (total_store.diff() - net_input).dropna()


@pytest.fixture(scope="module")
def one_day_forcing() -> pd.DataFrame:
    """First wet day (2012-01-01) from the bundled sample forcing."""
    _, df_forcing = load_sample_frames()
    return df_forcing.iloc[:TIMESTEPS_PER_DAY].copy()


@pytest.fixture(scope="module")
def control_run(
    one_day_forcing: pd.DataFrame, tmp_path_factory: pytest.TempPathFactory
) -> dict[str, object]:
    """Intact sample run over the wet day used as the conservation control."""
    raw = _load_sample_raw()
    path_yaml = tmp_path_factory.mktemp("waterdist_control") / "control.yml"
    sim, df_output = _run_wet_day(raw, one_day_forcing, path_yaml)
    residual = _water_balance_residual_mm(sim, df_output)
    suews = df_output.loc[GRID, "SUEWS"]
    return {
        "residual": residual,
        "rain_mm": float(suews["Rain"].sum()),
        "runoff": suews["RO"].copy(),
    }


@pytest.mark.core
def test_intact_sample_closes_wet_day_water_balance(control_run: dict) -> None:
    """Control guard: 2012-01-01 is genuinely wet and closes water balance."""
    # ASSERT
    assert control_run["rain_mm"] > 10.0, "control window must have substantial rain"
    assert float(control_run["runoff"].sum()) > 1.0, (
        "control window must produce runoff"
    )
    residual = control_run["residual"]
    assert float(residual.abs().max()) < WATER_CLOSURE_TOLERANCE_MM, (
        f"intact control residual {float(residual.abs().max()):.3e} mm "
        f"(signed sum {float(residual.sum()):.3e} mm)"
    )


@pytest.mark.core
@pytest.mark.parametrize(
    ("case", "payload", "should_refuse"),
    [
        ("omitted", None, False),
        ("null", None, False),
        ("wrapped-null", {"value": None}, False),
        ("zero", {"value": 0.0}, True),
        ("half", {"value": 0.5}, True),
    ],
)
def test_paved_terminal_alias_conserves_water_or_is_refused(
    case: str,
    payload: object,
    should_refuse: bool,
    one_day_forcing: pd.DataFrame,
    control_run: dict,
    tmp_path: Path,
) -> None:
    """Exercise all five paved to_soilstore terminal-alias forms against the wet-day control."""
    # ARRANGE
    raw = deepcopy(_load_sample_raw())
    dist = raw["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"]
    assert "to_soilstore" not in dist
    assert dist["to_runoff"]["value"] == pytest.approx(0.98)
    if case != "omitted":
        dist["to_soilstore"] = payload

    # ACT / ASSERT
    if should_refuse:
        with pytest.raises(ValueError, match="to_soilstore"):
            _run_wet_day(raw, one_day_forcing, tmp_path / f"{case}.yml")
        return

    sim, df_output = _run_wet_day(raw, one_day_forcing, tmp_path / f"{case}.yml")
    residual = _water_balance_residual_mm(sim, df_output)
    runoff = df_output.loc[GRID, "SUEWS"]["RO"]
    assert float(residual.abs().max()) < WATER_CLOSURE_TOLERANCE_MM
    np.testing.assert_allclose(
        runoff.to_numpy(),
        control_run["runoff"].to_numpy(),
        rtol=0.0,
        atol=WATER_CLOSURE_TOLERANCE_MM,
    )


@pytest.mark.core
@pytest.mark.parametrize(
    ("case", "surface", "mutation", "expected_pattern"),
    [
        (
            "paved-omitted-runoff",
            "paved",
            ("delete", "to_runoff"),
            "Missing required field to_runoff for paved",
        ),
        (
            "paved-null-runoff",
            "paved",
            ("set", "to_runoff", {"value": None}),
            "Missing required field to_runoff for paved",
        ),
        (
            "paved-overallocated",
            "paved",
            ("set", "to_grass", {"value": 0.5}),
            r"Water distribution sum must be 1\.0",
        ),
        (
            "paved-self-routing",
            "paved",
            ("set", "to_paved", {"value": 0.5}),
            "Disallowed water distribution field to_paved for paved",
        ),
        (
            "grass-runoff-zero",
            "grass",
            ("set", "to_runoff", {"value": 0.0}),
            "Disallowed water distribution field to_runoff for grass",
        ),
        (
            "grass-runoff-half",
            "grass",
            ("set", "to_runoff", {"value": 0.5}),
            "Disallowed water distribution field to_runoff for grass",
        ),
    ],
)
def test_invalid_water_distribution_refused_before_run(
    case: str,
    surface: str,
    mutation: tuple,
    expected_pattern: str,
    one_day_forcing: pd.DataFrame,
    tmp_path: Path,
) -> None:
    """Incomplete, non-unit, self-routed, and wrong-terminal distributions fail at load."""
    # ARRANGE
    raw = deepcopy(_load_sample_raw())
    dist = raw["sites"][0]["properties"]["land_cover"][surface]["waterdist"]
    if mutation[0] == "delete":
        del dist[mutation[1]]
    else:
        dist[mutation[1]] = mutation[2]

    # ACT / ASSERT
    with pytest.raises(ValueError, match=expected_pattern):
        _run_wet_day(raw, one_day_forcing, tmp_path / f"{case}.yml")
