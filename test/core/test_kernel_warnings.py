"""Regression tests for Fortran kernel warnings reaching the user (GH#1737).

Before this fix every non-fatal warning raised inside the Fortran kernel was
discarded: ``add_supy_warning`` was a no-op stub, the per-grid
``modState%errorstate`` log was reset at the end of every timestep, and the
Rust bridge exported only the fatal error code. Physics fallbacks such as
SPARTACUS flat-tile substitution or EHC leaving QS at zero therefore ran in
silence.

Two layers are covered here:

* ``KernelWarningLog`` (pure Python): timestamp decoding, accumulation across
  grids and chunks, deduplicated summaries, and explicit reporting when the
  kernel's bounded log has dropped entries.
* End-to-end through the real bridge: a deliberately unstable DyOHM thermal
  layer trips the ``cal_tsfc_dyohm`` stability guard (a formerly stubbed
  site) on every timestep, and the warning is visible on the simulation with
  grid and timestep context.
"""

from __future__ import annotations

import logging

import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy._run_rust import KernelWarningLog, _kernel_timestamp, _unpack_run_result

pytestmark = [pytest.mark.physics, pytest.mark.core]


# ---------------------------------------------------------------------------
# KernelWarningLog (pure Python)
# ---------------------------------------------------------------------------


def _entry(iy, id_, it, imin, location, message):
    return (iy, id_, it, imin, location, message)


def test_kernel_timestamp_decodes_fortran_stamp_and_flags_unstamped():
    assert _kernel_timestamp(2012, 1, 0, 5) == pd.Timestamp("2012-01-01 00:05")
    assert _kernel_timestamp(2012, 60, 23, 55) == pd.Timestamp("2012-02-29 23:55")
    assert _kernel_timestamp(0, 0, 0, 0) is pd.NaT


def test_unpack_run_result_accepts_three_and_four_element_tuples():
    assert _unpack_run_result((b"", "s", 3)) == (b"", "s", 3, None)
    payload = (2, [_entry(2012, 1, 0, 5, "EHC", "x")])
    assert _unpack_run_result((b"", "s", 3, payload)) == (b"", "s", 3, payload)


def test_kernel_warning_log_frame_and_summary_dedup_per_grid():
    log = KernelWarningLog()
    log.add(
        1,
        (
            3,
            [
                _entry(
                    2012,
                    1,
                    0,
                    5,
                    "SPARTACUS",
                    "LW full NaN detected -- using flat-tile fallback",
                ),
                _entry(
                    2012,
                    1,
                    0,
                    10,
                    "SPARTACUS",
                    "LW full NaN detected -- using flat-tile fallback",
                ),
                _entry(
                    2012,
                    1,
                    1,
                    0,
                    "EHC",
                    "lumped slab: invalid aggregated heat capacity; QS remains zero",
                ),
            ],
        ),
    )
    log.add(
        2,
        (
            1,
            [
                _entry(
                    2012,
                    2,
                    12,
                    0,
                    "interp_z",
                    "z_x above array maximum, using boundary value",
                )
            ],
        ),
    )

    df = log.to_frame()
    assert list(df.columns) == ["grid", "datetime", "location", "message"]
    assert len(df) == 4
    assert df["grid"].tolist() == [1, 1, 1, 2]
    assert df["datetime"].iloc[0] == pd.Timestamp("2012-01-01 00:05")
    assert (
        df["datetime"].is_monotonic_increasing
        or df
        .groupby("grid")["datetime"]
        .apply(lambda s: s.is_monotonic_increasing)
        .all()
    )
    assert log.totals == {1: 3, 2: 1}

    lines = log.summary_lines()
    assert len(lines) == 3, lines
    spartacus = next(line for line in lines if "SPARTACUS" in line)
    assert "(grid 1)" in spartacus
    assert "[2 timestep(s), from 2012-01-01 00:05 to 2012-01-01 00:10]" in spartacus
    ehc = next(line for line in lines if "EHC" in line)
    assert "[1 timestep(s), at 2012-01-01 01:00]" in ehc
    assert any("(grid 2)" in line and "interp_z" in line for line in lines)
    # No cap line: every raised warning was recorded
    assert not any("raised in total" in line for line in lines)


def test_kernel_warning_log_reports_capped_entries_explicitly():
    log = KernelWarningLog()
    # Kernel raised 700 warnings but its bounded log kept 2 of them
    log.add(
        1,
        (700, [_entry(2012, 1, 0, 5, "EHC", "m"), _entry(2012, 1, 0, 10, "EHC", "m")]),
    )
    lines = log.summary_lines()
    assert any("700 raised in total, 2 recorded" in line for line in lines), lines


def test_kernel_warning_log_accumulates_across_chunks_and_unstamped_entries():
    first = KernelWarningLog()
    first.add(1, (1, [_entry(2012, 1, 0, 5, "EHC", "m")]))
    second = KernelWarningLog()
    second.add(
        1,
        (
            2,
            [
                _entry(
                    0,
                    0,
                    0,
                    0,
                    "gen_building",
                    "unrecognised rcmethod value, defaulting to 0.5",
                )
            ],
        ),
    )
    second.add(1, (0, []))  # a clean chunk contributes nothing
    first.extend(second)
    assert first.totals == {1: 3}
    df = first.to_frame()
    assert len(df) == 2
    assert df["datetime"].isna().sum() == 1
    lines = first.summary_lines()
    assert any("timestep unknown" in line for line in lines), lines
    assert any("3 raised in total, 2 recorded" in line for line in lines), lines


def test_kernel_warning_log_empty_is_falsy_and_silent():
    log = KernelWarningLog()
    assert not log
    assert log.to_frame().empty
    assert log.summary_lines() == []
    log.add(1, None)
    assert not log


# ---------------------------------------------------------------------------
# End to end through the real bridge
# ---------------------------------------------------------------------------

DYOHM = 6
STEPS_PER_HOUR = 12  # sample forcing is at 5-minute resolution


def _dyohm_sim_with_unstable_paved_layer(sample_yaml_path, n_steps):
    """Sample run under DyOHM with a paved first layer that violates the
    explicit-scheme stability limit (alpha*dt/dz_min**2 > 0.5), so the
    ``cal_tsfc_dyohm`` guard fires deterministically on every timestep."""
    sim = SUEWSSimulation(str(sample_yaml_path))
    config = sim.config.model_copy(deep=True)
    config.model.physics.storage_heat = DYOHM
    paved = config.sites[0].properties.land_cover.paved.thermal_layers
    # alpha = k / rho_cp; with the driver's fixed dz_min of 0.03 m and a
    # 300 s timestep, rho_cp = 2e5 J m-3 K-1 gives alpha*dt/dz**2 ~ 1.8
    paved.rho_cp.value[0] = 2.0e5
    sim.update_config(config)
    sim.update_forcing(sim.forcing.df.iloc[:n_steps])
    return sim


@pytest.mark.smoke
def test_stubbed_site_warning_reaches_user_with_grid_and_timestep(
    sample_yaml_path, caplog
):
    sim = _dyohm_sim_with_unstable_paved_layer(sample_yaml_path, STEPS_PER_HOUR)

    with caplog.at_level(logging.WARNING, logger="SuPy"):
        output = sim.run()

    df = sim.kernel_warnings
    assert not df.empty, "DyOHM stability warning was not surfaced"
    hits = df[(df["location"] == "cal_tsfc_dyohm")]
    assert len(hits) == STEPS_PER_HOUR, df["message"].value_counts()
    assert set(hits["grid"]) == {1}
    assert hits["message"].str.contains("time step may be too large").all()
    forcing_index = sim.forcing.df.index
    assert hits["datetime"].min() >= forcing_index.min()
    assert hits["datetime"].max() <= forcing_index.max()
    assert hits["datetime"].is_unique

    # Same table on the output object, and a deduplicated summary in the log
    pd.testing.assert_frame_equal(output.kernel_warnings, df)
    summary = [
        rec.getMessage()
        for rec in caplog.records
        if "Kernel warning" in rec.getMessage()
    ]
    assert len(summary) == 1, summary
    assert "(grid 1)" in summary[0]
    assert "cal_tsfc_dyohm" in summary[0]
    assert f"[{STEPS_PER_HOUR} timestep(s)" in summary[0]


def test_clean_sample_run_has_no_kernel_warnings(sample_yaml_path):
    sim = SUEWSSimulation(str(sample_yaml_path))
    sim.update_forcing(sim.forcing.df.iloc[:STEPS_PER_HOUR])
    output = sim.run()
    assert sim.kernel_warnings.empty
    assert output.kernel_warnings.empty


def test_kernel_log_cap_is_reported_not_silent(sample_yaml_path):
    # Two days of 5-minute steps raise 576 warnings; the kernel keeps 512
    n_steps = 2 * 24 * STEPS_PER_HOUR
    sim = _dyohm_sim_with_unstable_paved_layer(sample_yaml_path, n_steps)
    sim.run()
    df = sim.kernel_warnings
    assert len(df) == 512
    assert sim._kernel_warnings.totals == {1: n_steps}
    lines = sim._kernel_warnings.summary_lines()
    assert any(f"{n_steps} raised in total, 512 recorded" in line for line in lines), (
        lines
    )
