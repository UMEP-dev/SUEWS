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
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.type import RefValue
import supy.suews_sim as suews_sim_module

pytestmark = pytest.mark.physics

GRID = 1
WATER_CLOSURE_TOLERANCE_MM = 1e-4
RUNOFF_MATCH_TOLERANCE_MM = 1e-12


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
    ("case", "payload", "should_refuse", "expect_absent"),
    [
        ("omitted", None, False, False),
        ("null", None, False, False),
        ("wrapped-null", {"value": None}, False, False),
        ("zero-int", 0, False, True),
        ("zero-float", 0.0, False, True),
        ("neg-zero", -0.0, False, True),
        ("wrapped-zero", {"value": 0.0}, False, True),
        ("half", {"value": 0.5}, True, False),
    ],
)
def test_paved_terminal_alias_conserves_water_or_is_refused(
    case: str,
    payload: object,
    should_refuse: bool,
    expect_absent: bool,
    one_day_forcing: pd.DataFrame,
    control_run: dict,
    tmp_path: Path,
) -> None:
    """Zero inapplicable terminal aliases normalise to None at load; non-zero fails."""
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
    loaded_dist = sim.config.sites[0].properties.land_cover.paved.waterdist
    if expect_absent:
        assert loaded_dist.to_soilstore is None
        assert "to_soilstore" not in loaded_dist.model_fields_set
    assert loaded_dist.to_runoff.value == pytest.approx(0.98)
    assert loaded_dist.to_grass.value == pytest.approx(0.02)

    residual = _water_balance_residual_mm(sim, df_output)
    runoff = df_output.loc[GRID, "SUEWS"]["RO"]
    assert float(residual.abs().max()) < WATER_CLOSURE_TOLERANCE_MM
    np.testing.assert_allclose(
        runoff.to_numpy(),
        control_run["runoff"].to_numpy(),
        rtol=0.0,
        atol=RUNOFF_MATCH_TOLERANCE_MM,
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
            "paved-soilstore-bool-false",
            "paved",
            ("set", "to_soilstore", {"value": False}),
            "to_soilstore",
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
    """Incomplete, non-unit, non-zero inapplicable, and bool distributions fail at load."""
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


@pytest.mark.core
@pytest.mark.parametrize("field_name", ["to_soilstore", "to_paved"])
@pytest.mark.parametrize("when", ["before-update-config", "after-update-config"])
def test_mutable_disallowed_destination_refused_at_handoff(
    field_name: str,
    when: str,
    one_day_forcing: pd.DataFrame,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Post-load mutation adding paved.to_soilstore=0.0 or to_paved=0.0 is refused."""
    # ARRANGE
    cfg = SUEWSConfig.from_dict(deepcopy(_load_sample_raw()))

    # ACT / ASSERT
    pattern = (
        r"sites\[0\] \(KCL, gridiv=1\): Invalid surface configuration for paved: "
        rf"Disallowed water distribution field {field_name} for paved"
    )
    if when == "before-update-config":
        setattr(
            cfg.sites[0].properties.land_cover.paved.waterdist,
            field_name,
            RefValue(value=0.0),
        )
        sim = SUEWSSimulation()
        with pytest.raises(ValueError, match=pattern):
            sim.update_config(cfg, auto_load_forcing=False)
        assert sim.config is None
        with pytest.raises(ValueError, match=pattern):
            SUEWSSimulation(cfg)
        return

    sim = SUEWSSimulation()
    sim.update_config(cfg, auto_load_forcing=False)
    setattr(
        sim.config.sites[0].properties.land_cover.paved.waterdist,
        field_name,
        RefValue(value=0.0),
    )
    sim.update_forcing(one_day_forcing.copy())

    def _forbid_rust_runner(*_args, **_kwargs):
        raise AssertionError(
            "SUEWSSimulation.run() must reject invalid waterdist before "
            "invoking run_suews_rust_chunked"
        )

    monkeypatch.setattr(suews_sim_module, "run_suews_rust_chunked", _forbid_rust_runner)
    with pytest.raises(ValueError, match=pattern):
        sim.run(end_date=one_day_forcing.index[-1])


@pytest.mark.core
def test_inactive_surface_omitted_waterdist_runs_and_activation_fails(
    one_day_forcing: pd.DataFrame,
    control_run: dict,
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Inactive surfaces (sfr == 0) may omit owning destinations; activating sfr > 0 fails."""
    # ARRANGE: evetr and bsoil have sfr == 0 in sample_config.yml.
    raw_omitted_dest = deepcopy(_load_sample_raw())
    lc = raw_omitted_dest["sites"][0]["properties"]["land_cover"]
    assert lc["evetr"]["sfr"]["value"] == pytest.approx(0.0)
    assert lc["bsoil"]["sfr"]["value"] == pytest.approx(0.0)
    del lc["evetr"]["waterdist"]["to_soilstore"]
    lc["bsoil"]["waterdist"] = {}

    raw_omitted_block = deepcopy(_load_sample_raw())
    del raw_omitted_block["sites"][0]["properties"]["land_cover"]["evetr"]["waterdist"]

    # ACT / ASSERT (a): both inactive omission forms load, close water balance,
    # and match the intact control runoff identically.
    for label, raw_case in (
        ("omitted-dest", raw_omitted_dest),
        ("omitted-block", raw_omitted_block),
    ):
        sim, df_output = _run_wet_day(
            raw_case, one_day_forcing, tmp_path / f"inactive_{label}.yml"
        )
        residual = _water_balance_residual_mm(sim, df_output)
        runoff = df_output.loc[GRID, "SUEWS"]["RO"]
        assert float(residual.abs().max()) < WATER_CLOSURE_TOLERANCE_MM
        np.testing.assert_allclose(
            runoff.to_numpy(),
            control_run["runoff"].to_numpy(),
            rtol=0.0,
            atol=RUNOFF_MATCH_TOLERANCE_MM,
        )

    # ACT / ASSERT (b): activating evetr from sfr=0 to sfr=0.05 (keeping sum(sfr)=1)
    # must fail at both update_config() and sim.run() naming site, surface, and field.
    expected = (
        r"sites\[0\] \(KCL, gridiv=1\): Invalid surface configuration for evetr: "
        r"Missing required field to_soilstore for evetr"
    )
    cfg_before = SUEWSConfig.from_dict(deepcopy(raw_omitted_dest))
    cfg_before.sites[0].properties.land_cover.evetr.sfr = RefValue(value=0.05)
    cfg_before.sites[0].properties.land_cover.paved.sfr = RefValue(value=0.38)
    sim_before = SUEWSSimulation()
    with pytest.raises(ValueError, match=expected):
        sim_before.update_config(cfg_before, auto_load_forcing=False)
    assert sim_before.config is None

    cfg_after = SUEWSConfig.from_dict(deepcopy(raw_omitted_dest))
    sim_after = SUEWSSimulation()
    sim_after.update_config(cfg_after, auto_load_forcing=False)
    sim_after.update_forcing(one_day_forcing.copy())
    sim_after.config.sites[0].properties.land_cover.evetr.sfr = RefValue(value=0.05)
    sim_after.config.sites[0].properties.land_cover.paved.sfr = RefValue(value=0.38)

    def _forbid_rust_runner(*_args, **_kwargs):
        raise AssertionError(
            "SUEWSSimulation.run() must reject activated incomplete surface "
            "before invoking run_suews_rust_chunked"
        )

    monkeypatch.setattr(suews_sim_module, "run_suews_rust_chunked", _forbid_rust_runner)
    with pytest.raises(ValueError, match=expected):
        sim_after.run(end_date=one_day_forcing.index[-1])


@pytest.mark.core
@pytest.mark.parametrize("when", ["before-update-config", "after-update-config"])
def test_mutable_valid_multi_step_edit_succeeds_and_conserves_water(
    when: str,
    one_day_forcing: pd.DataFrame,
    control_run: dict,
) -> None:
    """Two-step valid in-place fraction shift still runs and closes water balance."""
    # ARRANGE
    cfg = SUEWSConfig.from_dict(deepcopy(_load_sample_raw()))
    sim = SUEWSSimulation()

    def _shift_paved_runoff_to_grass(target_cfg: SUEWSConfig) -> None:
        dist = target_cfg.sites[0].properties.land_cover.paved.waterdist
        # Intermediate sum is 0.98 after step 1, restored to 1.0 after step 2.
        dist.to_runoff = RefValue(value=0.96)
        dist.to_grass = RefValue(value=0.04)

    # ACT
    if when == "before-update-config":
        _shift_paved_runoff_to_grass(cfg)
        sim.update_config(cfg, auto_load_forcing=False)
    else:
        sim.update_config(cfg, auto_load_forcing=False)
        _shift_paved_runoff_to_grass(sim.config)

    sim.update_forcing(one_day_forcing.copy())
    output = sim.run(end_date=one_day_forcing.index[-1])

    # ASSERT
    residual = _water_balance_residual_mm(sim, output.df)
    assert float(residual.abs().max()) < WATER_CLOSURE_TOLERANCE_MM
    shifted_runoff_mm = float(output.df.loc[GRID, "SUEWS"]["RO"].sum())
    control_runoff_mm = float(control_run["runoff"].sum())
    assert shifted_runoff_mm < control_runoff_mm
