"""Opt-in 48-hour restart matrix for active physics (F-21c, #2100).

No tolerance: identical execution must preserve every floating-point bit,
except that NaN payloads are immaterial. Sentinels are compared exactly.
There is no spin-up row: the public API has no spin-up-to-main-run hand-over
distinct from from_checkpoint, which the checkpoint row already covers, and the
pure-cover get_spinup_state algorithm is a separate route not tested here.
"""

import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSCheckpoint, SUEWSSimulation
from supy.data_model import SUEWSConfig

pytestmark = [
    pytest.mark.physics,
    pytest.mark.slow(
        reason="Opt-in active-physics continuation matrix requested by #2100 F-21c"
    ),
    pytest.mark.contract("C-RESTART", "C-STEBBS-RESTART", "C-EHC-RESTART"),
]

STEPS = 576
# 12:20 on the first day: daytime (shortwave and photosynthesis active) and
# away from midnight and the hourly forcing knots.
SPLIT = 147


class ContinuationDifference(AssertionError):
    """An exact comparison failed, with the first variable/time and magnitude."""


def _compare(actual, expected):
    """Compare all rows/columns without alignment that could hide missing data."""
    pd.testing.assert_index_equal(actual.index, expected.index)
    pd.testing.assert_index_equal(actual.columns, expected.columns)
    assert actual.dtypes.equals(expected.dtypes), "Output dtypes changed"
    a, b = actual.to_numpy(), expected.to_numpy()
    equal = (a.view(np.uint64) == b.view(np.uint64)) | (np.isnan(a) & np.isnan(b))
    if not equal.all():
        row, col = np.argwhere(~equal)[0]
        raise ContinuationDifference(
            f"{actual.columns[col]} at {actual.index[row]}: "
            f"resumed={a[row, col]!r}, continuous={b[row, col]!r}, "
            f"abs_diff={abs(a[row, col] - b[row, col])!r}; "
            f"differing_columns={list(actual.columns[(~equal).any(axis=0)])!r}"
        )


def _run(sim, forcing, **kwargs):
    sim.update_forcing(forcing.copy())
    output = sim.run(
        start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1, **kwargs
    ).df
    assert output.index.is_unique, "Duplicate grid/timestamp output rows"
    pd.testing.assert_index_equal(
        output.index.get_level_values("datetime"), forcing.index, check_names=False
    )
    return output


def _simulation(config):
    sim = SUEWSSimulation()
    sim.update_config(config, auto_load_forcing=False)
    return sim


def _active(frame, mode):
    """Require a finite live signal; irrigation may deliver a constant rate."""
    if mode == "default":
        series = frame["SUEWS", "QN"]
    elif mode == "snow":
        series = frame["SUEWS", "SWE"]
        assert (series > 0).any(), "Snow mode has no snow pack"
    elif mode == "irrigation":
        series = frame["SUEWS", "Irr"]
        assert (series > 0).any(), "Irrigation mode delivers no water"
    elif mode == "co2":
        series = frame["SUEWS", "FcPhoto"]
        assert (series != 0).any(), "Biogenic CO2 is inactive"
    elif mode in {"ehc", "ehc_explicit"}:
        series = frame["SUEWS", "QS"]
    elif mode == "stebbs":
        series = frame["STEBBS", "Tair_ind"]
    else:
        series = frame["SPARTACUS", "KNet_Grnd"]
    assert np.isfinite(series).all(), f"Non-finite {mode} activity witness"
    assert series.ne(-999).all(), f"Sentinel {mode} activity witness"
    if mode != "irrigation":
        assert series.nunique() > 1, f"Inactive {mode}"


def _configuration(sample_config_loaded, sample_data_loaded, mode):
    cfg = sample_config_loaded.model_copy(deep=True)
    forcing = sample_data_loaded[1].iloc[:STEPS].copy()
    assert len(forcing) == STEPS
    assert forcing.index[-1] - forcing.index[0] == pd.Timedelta(hours=48, minutes=-5)
    physics = cfg.model.physics
    if mode == "snow":
        physics.snow_use = 1
        forcing["Tair"] = -5.0
        forcing["rain"] = 0.1
        forcing["RH"], forcing["U"], forcing["pres"] = 80.0, 3.0, 1013.0
    elif mode == "spartacus":
        physics.net_radiation = 1003
        physics.storage_heat = 5
        layers = cfg.sites[0].properties.vertical_layers
        layers.veg_scale.value[2] = 0.0
        layers.veg_frac.value[2] = 0.0
    elif mode == "stebbs":
        path = (
            Path(__file__).parents[1]
            / "fixtures/data_test/stebbs_test/sample_config.yml"
        )
        template = SUEWSConfig.from_yaml(path)
        physics.stebbs = template.model.physics.stebbs.model_copy(deep=True)
        # Uncoupled mode exposes lost buildings through output rather than code 110.
        cfg.sites[0].properties.stebbs = template.sites[0].properties.stebbs.model_copy(
            deep=True
        )
        cfg.sites[0].properties.building_archetype = template.sites[
            0
        ].properties.building_archetype.model_copy(deep=True)
    elif mode == "ehc":
        physics.storage_heat = 5
    elif mode == "ehc_explicit":
        physics.storage_heat = 5
        physics.ehc_surface_coupling = 0
    elif mode == "irrigation":
        props = cfg.sites[0].properties
        irr = props.irrigation
        irr.h_maintain.value, irr.faut.value = -999.0, 0.5
        irr.ie_start.value, irr.ie_end.value = 1.0, 366.0
        irr.internalwateruse_h.value = 0.0
        for day in (
            "monday",
            "tuesday",
            "wednesday",
            "thursday",
            "friday",
            "saturday",
            "sunday",
        ):
            setattr(irr.daywat, day, 1.0)
            setattr(irr.daywatper, day, 1.0)
        for profile in (irr.wuprofa_24hr, irr.wuprofm_24hr):
            for day in (profile.working_day, profile.holiday):
                for hour in day:
                    day[hour] = 1 / 24
        for name, coefficient in (("evetr", 4.0), ("dectr", 0.0), ("grass", 0.0)):
            surface = getattr(props.land_cover, name)
            surface.ie_a.value = surface.ie_m.value = coefficient
            surface.irrigation_fraction.value = float(name == "grass")
        forcing["rain"] = 0.0
    elif mode == "co2":
        physics.emissions = 32
    return cfg, forcing


@pytest.mark.parametrize(
    "mode",
    [
        "default",
        "snow",
        "spartacus",
        "stebbs",
        "ehc",
        "ehc_explicit",
        "irrigation",
        "co2",
    ],
)
@pytest.mark.parametrize("mechanism", ["chunk", "checkpoint", "stream"])
def test_continuation_matrix(
    sample_config_loaded, sample_data_loaded, tmp_path, mode, mechanism
):
    """All active output groups survive the chosen state hand-over exactly."""
    cfg, forcing = _configuration(sample_config_loaded, sample_data_loaded, mode)
    # Round-trip through YAML so overrides take the public parser/engine path.
    path = tmp_path / "case.yml"
    cfg.to_yaml(path)
    whole = _simulation(path)
    expected = _run(whole, forcing, chunk_day=3660)
    _active(expected, mode)
    if mechanism == "chunk":
        actual = _run(_simulation(path), forcing, chunk_day=1)
    elif mechanism == "checkpoint":
        first = _simulation(path)
        head = _run(first, forcing.iloc[:SPLIT])
        second = SUEWSSimulation.from_checkpoint(
            path,
            first.checkpoint.to_file(tmp_path / "checkpoint.json"),
        )
        actual = pd.concat([head, _run(second, forcing.iloc[SPLIT:])])
    else:
        groups = list(expected.columns.get_level_values(0).unique())
        # BL has no kernel writer; omit only that inactive, all-NaN group.
        groups = [group for group in groups if group != "BL"]
        output = {
            "format": "parquet",
            "freq": 300,
            "dir": str(tmp_path / "stream"),
            "groups": groups,
            "include_provisional": True,
            "stream_chunk": 1,
        }
        streamed = _simulation(path)
        streamed.update_config(
            {"model": {"control": {"output": output}}}, auto_load_forcing=False
        )
        streamed.update_forcing(forcing)
        streamed.run(start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1)
        (saved,) = (tmp_path / "stream").glob("*SUEWS_output*.parquet")
        actual = pd.read_parquet(saved).sort_index()
        assert actual.index.is_unique, "Streamed output repeats a timestamp"
        pd.testing.assert_index_equal(
            actual.index.get_level_values("datetime"), forcing.index, check_names=False
        )
        # save() converts accumulated native quantities (e.g. Evap mm/step)
        # to declared output units (mm/s). Compare two persisted representations,
        # as test_stream_chunk does, never persisted values against native units.
        whole.config.model.control.output.freq = 300
        whole.config.model.control.output.groups = groups
        whole.save(tmp_path / "whole", format="parquet", include_provisional=True)
        (saved,) = (tmp_path / "whole").glob("*SUEWS_output*.parquet")
        expected = pd.read_parquet(saved).sort_index()
    _compare(actual, expected)
    _active(actual, mode)


def test_checkpoint_perturbation_is_detected(sample_config_loaded, sample_data_loaded):
    """Changing evolved surface water at the hand-over must fail exact comparison."""
    forcing = sample_data_loaded[1].iloc[:STEPS].copy()
    first = _simulation(sample_config_loaded)
    _run(first, forcing.iloc[:SPLIT])
    checkpoint = first.checkpoint
    reference = SUEWSSimulation.from_checkpoint(first.config, checkpoint)
    expected = _run(reference, forcing.iloc[SPLIT:])
    states = {grid: json.loads(state) for grid, state in checkpoint.grid_states.items()}
    # hydro_state_pack writes seven soil stores, then state_surf(Paved) at 7.
    for state in states.values():
        state["state"]["members"]["hydro_state"]["values"][7] += 10.0
    altered = SUEWSCheckpoint.from_grid_states(
        states, last_timestamp=checkpoint.last_timestamp
    )
    resumed = SUEWSSimulation.from_checkpoint(first.config, altered)
    actual = _run(resumed, forcing.iloc[SPLIT:])
    with pytest.raises(ContinuationDifference, match="abs_diff="):
        _compare(actual, expected)
