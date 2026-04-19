"""GH-1292 PR2 end-to-end contract for moisture-aware LAI phenology.

The ``LAIType = 2`` branch implements Design C (see
``dev-ref/design-notes/gh1292-moisture-phenology.md``): a Jarvis
water-stress factor on ``delta_GDD`` combined with a CLM5-style
persistence latch on the tau_w-day running mean of relative soil
water. Two invariants define correctness:

1. **Graceful degradation at well-watered sites.** When the soil
   stays above ``w_opt`` and ``wbar_id`` stays above ``w_off`` at all
   times, Jarvis returns 1.0 and the latch never flips, so the
   trajectory must equal the ``LAIType = 0`` baseline.
2. **Engaged moisture gate under aggressive thresholds.** When the
   Jarvis interval or persistence thresholds are tightened, the gate
   must strictly reduce simulated LAI.

Under PR1 the branch was a documented no-op; PR2 activates the
numerics and the bit-identity assertion now holds only because the
bundled London sample is well-watered.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation

LAI_TYPE_COLS = [
    ("laitype", "(0,)"),
    ("laitype", "(1,)"),
    ("laitype", "(2,)"),
]
MOISTURE_PARAM_COLS = {
    "w_wilt": [("w_wilt", f"({i},)") for i in range(3)],
    "w_opt": [("w_opt", f"({i},)") for i in range(3)],
    "w_on": [("w_on", f"({i},)") for i in range(3)],
    "w_off": [("w_off", f"({i},)") for i in range(3)],
}


def _run_with_state_override(laitype: int, moisture_overrides: dict | None = None) -> pd.DataFrame:
    """Return the simulated LAI DataFrame with all veg surfaces set to ``laitype``.

    ``moisture_overrides`` is an optional dict mapping parameter name
    (``w_wilt``, ``w_opt``, ``w_on``, ``w_off``) to a scalar value that
    is broadcast across all three vegetation surfaces.
    """

    sim = SUEWSSimulation.from_sample_data()
    state = sim.state_init.copy()
    for col in LAI_TYPE_COLS:
        state.loc[:, col] = laitype
    if moisture_overrides:
        for param, value in moisture_overrides.items():
            for col in MOISTURE_PARAM_COLS[param]:
                state.loc[:, col] = value

    scenario = SUEWSSimulation.from_state(state)
    scenario.update_forcing(sim.forcing)
    output = scenario.run()
    return output.SUEWS[["LAI"]].copy()


@pytest.fixture(scope="module")
def lai_baseline_0() -> pd.DataFrame:
    return _run_with_state_override(0)


def test_laitype_2_matches_laitype_0_in_well_watered_regime(
    lai_baseline_0: pd.DataFrame,
) -> None:
    """Default moisture thresholds + well-watered London sample -> bit-identical to LAIType=0."""

    lai_variant_2 = _run_with_state_override(2)
    assert lai_baseline_0.shape == lai_variant_2.shape
    np.testing.assert_array_equal(lai_baseline_0.values, lai_variant_2.values)


def test_laitype_2_reduces_lai_under_aggressive_moisture_thresholds(
    lai_baseline_0: pd.DataFrame,
) -> None:
    """Aggressive Jarvis thresholds force f_w < 1 for part of the year -> lower mean LAI."""

    aggressive = {
        "w_wilt": 0.80,
        "w_opt": 0.95,
        "w_on": 0.90,
        "w_off": 0.70,
    }
    lai_aggressive = _run_with_state_override(2, aggressive)

    baseline_mean = float(lai_baseline_0.mean().iloc[0])
    aggressive_mean = float(lai_aggressive.mean().iloc[0])

    assert aggressive_mean < baseline_mean, (
        f"aggressive LAIType=2 mean ({aggressive_mean:.4f}) should be strictly "
        f"below LAIType=0 baseline ({baseline_mean:.4f})"
    )
    # Must still run to completion without NaN and within physical LAI bounds.
    assert not lai_aggressive.isna().any().any()
    assert (lai_aggressive >= 0.0).all().all()


def test_sample_run_laitype_1_completes_without_nan() -> None:
    """Legacy laitype=1 path must still run to completion without NaNs."""

    sim = SUEWSSimulation.from_sample_data()
    output = sim.run()
    lai = output.SUEWS[["LAI"]]
    assert len(lai) > 0
    assert not lai.isna().any().any()


def test_unsupported_positive_laitype_falls_back_to_legacy_nonzero_branch() -> None:
    """Unsupported positive laitype values should retain the pre-GH-1292 fallback semantics."""

    lai_type_1 = _run_with_state_override(1)
    lai_type_3 = _run_with_state_override(3)

    assert lai_type_1.shape == lai_type_3.shape
    np.testing.assert_array_equal(lai_type_1.values, lai_type_3.values)


def test_laitype_2_dry_start_delays_spring_green_up() -> None:
    """Zeroing the vegetation soil store at init forces the latch off; spring green-up must lag.

    Design C expectation (CLM5 stress-deciduous): a dry initial root-zone keeps
    ``leaf_on_permitted`` false until the tau_w-day running mean of relative soil
    water climbs above ``w_on``. Cumulative LAI over the first months of the
    sample year is therefore strictly lower than the LAIType=0 baseline.
    """

    sim = SUEWSSimulation.from_sample_data()
    state_baseline = sim.state_init.copy()
    for col in LAI_TYPE_COLS:
        state_baseline.loc[:, col] = 0

    state_dry = sim.state_init.copy()
    for col in LAI_TYPE_COLS:
        state_dry.loc[:, col] = 2
    # Deplete the soil store on the vegetation surfaces (indices 2, 3, 4) so initial SMD is
    # close to capacity; the data model enforces soilstore >= 10 mm, so we cannot use exactly
    # zero, but 10 mm gives w ~ 0.07, well below the default w_wilt (0.15) and w_off (0.20).
    for veg_idx in (2, 3, 4):
        state_dry.loc[:, ("soilstore_surf", f"({veg_idx},)")] = 10.0

    def _run(state):
        scenario = SUEWSSimulation.from_state(state)
        scenario.update_forcing(sim.forcing)
        return scenario.run().SUEWS[["LAI"]]

    lai_baseline = _run(state_baseline)
    lai_dry = _run(state_dry)

    # Compare cumulative LAI over the first 60 days; the dry-start variant should integrate
    # to a strictly smaller total because the moisture latch blocks spring GDD accumulation.
    baseline_total = float(lai_baseline.iloc[: 60 * 288].sum().iloc[0])
    dry_total = float(lai_dry.iloc[: 60 * 288].sum().iloc[0])
    assert dry_total < baseline_total, (
        f"dry-start LAIType=2 cumulative LAI ({dry_total:.1f}) should lag LAIType=0 "
        f"baseline ({baseline_total:.1f}) when the latch is forced off on day 1"
    )
    assert not lai_dry.isna().any().any()
