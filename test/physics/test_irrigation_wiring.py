"""Regression tests for modelled-irrigation propagation (issue #1436, PR #1437).

Issue #1436 reported that the YAML -> Fortran bridge in ``SUEWSSimulation``
never copied the ``properties.irrigation`` block (and the per-vegetated-
surface ``ie_a`` / ``ie_m``) into the Rust ``SuewsSite::irrigation`` struct.
Any run with ``water_use=0`` (modelled irrigation) silently received default
all-zero irrigation parameters and produced ``WUDay_id = 0`` for every day,
regardless of what the user wrote in their YAML.

The bug went unnoticed because every shipped fixture runs with effectively
no irrigation (``faut=0`` or ``ie_a=-999``). These end-to-end tests pin the
contract that the new ``SUEWSSimulation`` API enforces:

  * when the user configures a non-default ``irrigation`` block AND switches
    ``model.physics.water_use`` to ``MODELLED``, the modelled water-use
    pathway actually runs and produces non-zero daily water use; and
  * with the sample-default irrigation block (sentinel ``-999`` / ``faut=0``)
    the same code path produces zero water use, so configuration changes are
    necessary AND sufficient to drive the output.

A failure in either direction indicates a regression in the YAML -> Rust
struct wiring (``yaml_config.rs::apply_irrigation_overrides``), in the Rust
-> Fortran handoff, or in the Fortran ``update_WaterUse`` routine itself.
"""

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy.data_model.core.model import WaterUseMethod

from conftest import TIMESTEPS_PER_DAY

pytestmark = pytest.mark.physics


def _evening_profile() -> dict[str, float]:
    """24-hour profile with five evening hours of 0.2 (sums to 1.0).

    Hour keys are 1..24 per the SUEWS YAML convention.
    """
    return {str(h): (0.2 if 19 <= h <= 23 else 0.0) for h in range(1, 25)}


def _enable_modelled_irrigation(sim: SUEWSSimulation) -> None:
    """Overlay a non-trivial irrigation block on ``sim._config`` in place.

    Three polynomial coefficients ``a0 + a1*Tair + a2*days_since_rain`` are
    stored, by SUEWS convention, as one scalar per vegetated land-cover
    block (``evetr`` -> a0, ``dectr`` -> a1, ``grass`` -> a2). Only the
    constant a0 is set here; that suffices to drive non-zero water use
    every day when combined with ``faut > 0`` and a non-zero daily profile.
    """
    cfg = sim._config

    cfg.model.physics.water_use = WaterUseMethod.MODELLED

    site = cfg.sites[0].properties
    irrigation = site.irrigation
    irrigation.h_maintain.value = 10.0
    irrigation.faut.value = 0.2
    irrigation.ie_start.value = 1.0
    irrigation.ie_end.value = 366.0
    irrigation.internalwateruse_h.value = 0.0

    for day in (
        "monday",
        "tuesday",
        "wednesday",
        "thursday",
        "friday",
        "saturday",
        "sunday",
    ):
        setattr(irrigation.daywat, day, 1.0)
        setattr(irrigation.daywatper, day, 1.0)

    evening = _evening_profile()
    irrigation.wuprofa_24hr.working_day = evening
    irrigation.wuprofa_24hr.holiday = evening
    irrigation.wuprofm_24hr.working_day = evening
    irrigation.wuprofm_24hr.holiday = evening

    # Polynomial constant term (a0) — sourced from the EveTr ie_a slot.
    # The Tair and days-since-rain coefficients (a1, a2) come from the
    # DecTr and Grass slots respectively; leaving them at zero keeps the
    # daily water use constant and makes the assertion below independent
    # of forcing variability.
    site.land_cover.evetr.ie_a.value = 3.674
    site.land_cover.evetr.ie_m.value = 1.102

    # Sample fixtures ship every surface with ``irrigation_fraction = 0``.
    # The Fortran timestep delivery multiplies the daily water-use total
    # by this fraction (``wu_surf = wu_surf * IrrFrac`` in
    # ``suews_phys_waterdist.f95``), so without a non-zero fraction the
    # surface output stays zero even when the daily coefficient is set.
    # Turn the grass surface fully irrigated so the timestep ``WUGrass``
    # column actually carries the modelled signal.
    site.land_cover.grass.irrigation_fraction.value = 1.0

    sim._df_state_init = cfg.to_df_state()


_WATER_USE_COLUMNS = ["Irr", "WUInt", "WUEveTr", "WUDecTr", "WUGrass"]


class TestIrrigationConfigPropagation:
    """End-to-end guard for the YAML -> Fortran irrigation wiring.

    The SUEWS-group columns ``Irr``, ``WUInt``, ``WUEveTr``, ``WUDecTr``,
    and ``WUGrass`` are the user-facing irrigation outputs: they are the
    actual water-use amounts delivered to each surface at every timestep.
    Under #1436 every one of these was identically zero whenever the user
    configured a modelled irrigation block, because the Rust YAML bridge
    silently dropped ``properties.irrigation`` before it ever reached the
    Fortran ``IRRIGATION_PRM`` struct.

    These outputs flow through a different code path from the DailyState
    ``WU_Grass{1,2,3}`` daily coefficients (which the Fortran computes
    locally inside ``update_WaterUse``), so they are the right signal to
    guard end-to-end propagation of the user's YAML.
    """

    N_DAYS = 30

    def _slice_forcing(self, sim: SUEWSSimulation) -> pd.DataFrame:
        """Trim the sample forcing to ``N_DAYS`` days for a fast run."""
        end_index = TIMESTEPS_PER_DAY * self.N_DAYS - 1
        return sim.forcing.df.copy().iloc[: end_index + 1].copy()

    def _suews_water_use(self, results: pd.DataFrame) -> pd.DataFrame:
        """Extract the SUEWS-group water-use columns for grid 1."""
        return (
            results.xs("SUEWS", level="group", axis=1)
            .xs(1, level="grid")[_WATER_USE_COLUMNS]
        )

    @pytest.mark.core
    def test_modelled_irrigation_delivers_water_use_to_outputs(self):
        """Configured ``properties.irrigation`` block must reach the surface.

        Regression for #1436: prior to PR #1437 the Rust YAML bridge in
        ``apply_site_overrides`` had no ``apply_irrigation_overrides``
        applier, so every irrigation parameter (``h_maintain``, ``faut``,
        ``ie_a``, ``ie_m``, ``daywat``, ``daywatper``, ``wuprofa_24hr``,
        ``wuprofm_24hr``) was silently dropped and the bridge handed
        ``IrrigationPrm::default()`` (all zeros) to the Fortran physics.
        As a result every timestep produced zero ``Irr`` / ``WUGrass`` /
        ``WUEveTr`` / ``WUDecTr`` regardless of the user's YAML.
        """
        sim = SUEWSSimulation.from_sample_data()
        _enable_modelled_irrigation(sim)

        df_forcing = self._slice_forcing(sim)
        sim.update_forcing(df_forcing)
        results = sim.run(end_date=df_forcing.index[-1])

        df_water_use = self._suews_water_use(results)
        ser_total = df_water_use.sum()

        assert ser_total["Irr"] > 0.0, (
            "SUEWS 'Irr' is exactly zero across the run — the irrigation "
            "configuration is being dropped on the YAML -> Fortran path "
            "(regression of gh#1436). Column totals:\n"
            f"{ser_total.to_dict()}"
        )
        assert ser_total["WUGrass"] > 0.0, (
            "SUEWS 'WUGrass' is exactly zero across the run — modelled "
            "irrigation is not reaching the grass surface water budget. "
            f"Column totals:\n{ser_total.to_dict()}"
        )

    @pytest.mark.core
    def test_default_irrigation_block_produces_zero_water_use(self):
        """Sample-default irrigation (faut=0, ie_a=-999) yields zero water use.

        This is the masking baseline that hid #1436 for so long: every
        fixture ships with an effectively-off irrigation block. Asserting
        the baseline pins it so a future change that accidentally turned
        irrigation on by default would surface immediately and not silently
        invalidate the positive test above.
        """
        sim = SUEWSSimulation.from_sample_data()
        sim._config.model.physics.water_use = WaterUseMethod.MODELLED
        sim._df_state_init = sim._config.to_df_state()

        df_forcing = self._slice_forcing(sim)
        sim.update_forcing(df_forcing)
        results = sim.run(end_date=df_forcing.index[-1])

        df_water_use = self._suews_water_use(results)

        np.testing.assert_allclose(
            df_water_use.to_numpy(),
            0.0,
            atol=1e-9,
            err_msg=(
                "Sample-default irrigation block should produce zero "
                "Irr/WUInt/WUEveTr/WUDecTr/WUGrass, but non-zero values "
                "appeared. If this is intentional (e.g. sample defaults "
                "changed), update the positive test alongside this baseline."
            ),
        )
