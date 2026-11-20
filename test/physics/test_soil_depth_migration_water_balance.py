"""Wet-day water-balance controls for legacy soil_depth migration (gh#2077 / #2080).

Migrating a legacy (pre-2026.6.dev11) YAML with missing or null ``soil_depth``
on an active non-water surface with positive ``soil_store_capacity`` must stop
without writing output and report the affected site and surface, rather than
substituting ``0.0`` (which divides by zero in horizontal soil water
redistribution and breaks wet-day water balance). Once the user supplies
``soil_depth`` -- or when soil store capacity is zero -- the migrated YAML
reloads cleanly and closes the wet-day grid water budget.
"""

from __future__ import annotations

from copy import deepcopy
from pathlib import Path

from click.testing import CliRunner
from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation
from supy.cmd.suews_cli import cli
from supy.data_model.core import SUEWSConfig

pytestmark = [pytest.mark.physics, pytest.mark.cfg]

WATER_CLOSURE_TOLERANCE_MM = 1e-4
GRID_ID = 1
SURFACE_TAGS: dict[str, str] = {
    "paved": "Paved",
    "bldgs": "Bldgs",
    "evetr": "EveTr",
    "dectr": "DecTr",
    "grass": "Grass",
    "bsoil": "BSoil",
    "water": "Water",
}


def _unwrap(item: object) -> float:
    return float(item["value"] if isinstance(item, dict) else item)


@pytest.fixture(scope="module")
def sample_raw(sample_yaml_path: Path) -> dict:
    return yaml.safe_load(sample_yaml_path.read_text(encoding="utf-8"))


@pytest.fixture(scope="module")
def one_day_forcing(sample_forcing_parsed):
    _, df_forcing = sample_forcing_parsed(TIMESTEPS_PER_DAY)
    return df_forcing


def _wet_day_residual_mm(
    config_path: Path, df_forcing: pd.DataFrame, raw_cfg: dict
) -> pd.Series:
    """Run a one-day wet simulation and return the per-step water-balance residual (mm)."""
    sim = SUEWSSimulation()
    sim.update_config(str(config_path), auto_load_forcing=False)
    sim.update_forcing(df_forcing.copy())
    output = sim.run(end_date=df_forcing.index[-1])

    assert int(sim.config.model.physics.snow_use.value) == 0, "snow must be off"
    frame = output.df.xs(GRID_ID, level="grid")
    land = raw_cfg["sites"][0]["properties"]["land_cover"]
    weights = pd.Series({surf: _unwrap(land[surf]["sfr"]) for surf in SURFACE_TAGS})
    assert abs(weights.sum() - 1.0) < 1e-10
    soil = pd.DataFrame(
        {surf: frame["debug", f"ss_{tag}_next"] for surf, tag in SURFACE_TAGS.items()}
    )
    suews = frame["SUEWS"]
    assert suews["Rain"].sum() > 0, "dry window: water balance would hold trivially"
    assert (suews[["FlowCh", "AddWater"]] == 0).all().all()
    store = soil.dot(weights) + suews["State"]
    net = suews["Rain"] + suews["Irr"] - suews["Evap"] - suews["RO"]
    residual = (store.diff() - net).iloc[1:]
    assert len(residual) == TIMESTEPS_PER_DAY - 1
    assert np.isfinite(residual).all()
    return residual


@pytest.mark.core
@pytest.mark.parametrize("surface", ["paved", "bldgs"])
def test_migrated_supplied_soil_depth_closes_wet_day_water_balance(
    sample_raw: dict,
    one_day_forcing: pd.DataFrame,
    tmp_path: Path,
    surface: str,
) -> None:
    """Missing active soil_depth refuses migration; supplying it migrates and closes budget."""
    # ARRANGE: legacy 2026.6.dev10 config missing soil_depth on an active positive-capacity surface
    legacy_missing = deepcopy(sample_raw)
    legacy_missing["schema_version"] = "2026.6.dev10"
    land = legacy_missing["sites"][0]["properties"]["land_cover"]
    assert _unwrap(land[surface]["sfr"]) > 0.0
    assert _unwrap(land[surface]["soil_store_capacity"]) > 0.0
    original_depth = _unwrap(land[surface]["soil_depth"])
    assert original_depth > 0.0
    land[surface].pop("soil_depth", None)

    broken_in = tmp_path / f"{surface}_missing.yml"
    broken_out = tmp_path / f"{surface}_missing_out.yml"
    broken_in.write_text(
        yaml.safe_dump(legacy_missing, sort_keys=False), encoding="utf-8"
    )

    # ACT / ASSERT (no-write error control)
    fail_result = CliRunner().invoke(
        cli,
        ["convert", "-i", str(broken_in), "-o", str(broken_out)],
        terminal_width=250,
        color=False,
    )
    assert fail_result.exit_code == 1, fail_result.output
    assert not broken_out.exists()
    fail_msg = " ".join(fail_result.output.split())
    assert legacy_missing["sites"][0]["name"] in fail_msg
    assert str(legacy_missing["sites"][0]["gridiv"]) in fail_msg
    assert surface in fail_msg
    assert "soil_depth" in fail_msg

    # ARRANGE (corrected-input positive control): user supplies soil_depth
    legacy_fixed = deepcopy(legacy_missing)
    legacy_fixed["sites"][0]["properties"]["land_cover"][surface]["soil_depth"] = {
        "value": original_depth
    }
    fixed_in = tmp_path / f"{surface}_fixed.yml"
    fixed_out = tmp_path / f"{surface}_migrated.yml"
    fixed_in.write_text(yaml.safe_dump(legacy_fixed, sort_keys=False), encoding="utf-8")

    # ACT: migrate via public CLI, reload via SUEWSConfig.from_yaml, and run wet-day simulation
    ok_result = CliRunner().invoke(
        cli,
        ["convert", "-i", str(fixed_in), "-o", str(fixed_out)],
        terminal_width=250,
        color=False,
    )
    assert ok_result.exit_code == 0, ok_result.output
    assert fixed_out.is_file()
    loaded = SUEWSConfig.from_yaml(str(fixed_out))
    assert (
        getattr(loaded.sites[0].properties.land_cover, surface).soil_depth.value
        == original_depth
    )
    migrated_raw = yaml.safe_load(fixed_out.read_text(encoding="utf-8"))
    residual = _wet_day_residual_mm(fixed_out, one_day_forcing, migrated_raw)

    # ASSERT
    assert residual.abs().max() < WATER_CLOSURE_TOLERANCE_MM, (
        f"{surface} supplied soil_depth: max residual {residual.abs().max():.6g} mm, "
        f"signed sum {residual.sum():.6g} mm"
    )


@pytest.mark.core
def test_migrated_inactive_and_water_missing_soil_depth_closes_wet_day_water_balance(
    sample_raw: dict,
    one_day_forcing: pd.DataFrame,
    tmp_path: Path,
) -> None:
    """Inactive and water surfaces missing soil_depth migrate cleanly and close budget."""
    # ARRANGE
    legacy_doc = deepcopy(sample_raw)
    legacy_doc["schema_version"] = "2026.6.dev10"
    land = legacy_doc["sites"][0]["properties"]["land_cover"]
    assert _unwrap(land["evetr"]["sfr"]) == pytest.approx(0.0)
    land["evetr"]["soil_store_capacity"] = {"value": 150.0}
    land["evetr"].pop("soil_depth", None)
    land["water"].pop("soil_depth", None)

    source = tmp_path / "inactive_water.yml"
    output = tmp_path / "inactive_water_migrated.yml"
    source.write_text(yaml.safe_dump(legacy_doc, sort_keys=False), encoding="utf-8")

    # ACT
    result = CliRunner().invoke(
        cli,
        ["convert", "-i", str(source), "-o", str(output)],
        terminal_width=250,
        color=False,
    )
    assert result.exit_code == 0, result.output
    assert output.is_file()
    loaded = SUEWSConfig.from_yaml(str(output))
    assert loaded.sites[0].properties.land_cover.evetr.soil_depth is None
    assert loaded.sites[0].properties.land_cover.water.soil_depth is None

    migrated_raw = yaml.safe_load(output.read_text(encoding="utf-8"))
    residual = _wet_day_residual_mm(output, one_day_forcing, migrated_raw)

    # ASSERT
    assert residual.abs().max() < WATER_CLOSURE_TOLERANCE_MM, (
        f"inactive/water missing soil_depth: max residual {residual.abs().max():.6g} mm, "
        f"signed sum {residual.sum():.6g} mm"
    )


@pytest.mark.core
def test_migrated_zero_capacity_surface_closes_wet_day_water_balance(
    sample_raw: dict,
    one_day_forcing: pd.DataFrame,
    tmp_path: Path,
) -> None:
    """Zero-capacity active surface missing soil_depth migrates to 0.0 and closes wet-day budget."""
    # ARRANGE: active dectr (sfr=0.01) with zero soil_store_capacity and omitted soil_depth
    legacy_zero_cap = deepcopy(sample_raw)
    legacy_zero_cap["schema_version"] = "2026.6.dev10"
    dectr = legacy_zero_cap["sites"][0]["properties"]["land_cover"]["dectr"]
    assert _unwrap(dectr["sfr"]) > 0.0
    dectr["soil_store_capacity"] = {"value": 0.0}
    dectr.pop("soil_depth", None)
    legacy_zero_cap["sites"][0]["initial_states"]["dectr"]["soilstore"] = {
        "value": 10.0
    }

    source = tmp_path / "zero_cap.yml"
    output = tmp_path / "zero_cap_migrated.yml"
    source.write_text(
        yaml.safe_dump(legacy_zero_cap, sort_keys=False), encoding="utf-8"
    )

    # ACT
    result = CliRunner().invoke(
        cli,
        ["convert", "-i", str(source), "-o", str(output)],
        terminal_width=250,
        color=False,
    )
    assert result.exit_code == 0, result.output
    assert output.is_file()
    loaded = SUEWSConfig.from_yaml(str(output))
    loaded_dectr = loaded.sites[0].properties.land_cover.dectr
    assert loaded_dectr.soil_depth.value == pytest.approx(0.0)
    assert loaded_dectr.soil_store_capacity.value == pytest.approx(0.0)

    migrated_raw = yaml.safe_load(output.read_text(encoding="utf-8"))
    residual = _wet_day_residual_mm(output, one_day_forcing, migrated_raw)

    # ASSERT
    assert residual.abs().max() < WATER_CLOSURE_TOLERANCE_MM, (
        f"zero-capacity dectr: max residual {residual.abs().max():.6g} mm, "
        f"signed sum {residual.sum():.6g} mm"
    )
