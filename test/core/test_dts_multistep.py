"""Multi-timestep DTS parity test.

This test verifies that when we properly update timer and forcing between timesteps,
the DTS interface produces identical output to traditional run.

Key insight from Fortran suews_cal_multitsteps (lines 5205-5268):
1. timer.dt_since_start must be updated AFTER each timestep: += tstep
2. Timer datetime values (iy, id, it, imin) must be set for each timestep
3. Forcing values must be updated from forcing dataframe for each timestep
"""

import numpy as np
import pandas as pd
import pytest

from supy import load_SampleData, run_supy
from supy.data_model import SUEWSConfig
from supy.supy_driver import module_ctrl_type as dts
from supy.supy_driver import suews_driver as drv
from supy.dts._core import (
    create_suews_config, create_suews_site, create_suews_state,
    create_suews_forcing, create_suews_timer
)
from supy.dts._populate import (
    populate_config_from_pydantic, populate_site_from_pydantic,
    populate_state_from_pydantic, populate_storedrainprm,
    populate_forcing_from_row, populate_timer_from_datetime,
    populate_roughnessstate, populate_atmstate, populate_ohmstate_defaults
)


def run_multi_timestep_comparison(n_timesteps: int = 48, verbose: bool = True):
    """Run multi-timestep comparison between DTS and traditional run.

    Parameters
    ----------
    n_timesteps : int
        Number of timesteps to compare (default 48 = 4 hours at 5-min intervals)
    verbose : bool
        Whether to print detailed output

    Returns
    -------
    dict
        Comparison results with max differences for each variable
    """
    # Load sample data
    df_state_init, df_forcing = load_SampleData()
    grid_id = df_state_init.index[0]
    df_forcing_subset = df_forcing.head(n_timesteps)

    # Run traditional with debug_mode to get ground truth output
    df_output, df_state_final, df_debug, dict_dts = run_supy(
        df_forcing_subset, df_state_init, debug_mode=True
    )

    # Create Pydantic config
    config = SUEWSConfig.from_df_state(df_state_init.loc[[grid_id]])
    site = config.sites[0]
    model = config.model

    nlayer_val = site.properties.vertical_layers.nlayer
    nlayer = int(nlayer_val.value if hasattr(nlayer_val, 'value') else nlayer_val)
    ndepth = 5
    tstep = 300  # 5-minute timesteps

    # Create and populate config
    config_dts = create_suews_config()
    populate_config_from_pydantic(config_dts, model)

    # Create and populate site
    site_dts = create_suews_site(nlayer=nlayer, ndepth=ndepth)
    populate_site_from_pydantic(site_dts, site, model)
    site_dts.cal_surf(config_dts)

    # Create and populate state (ONCE - will be modified in-place)
    state_dts = create_suews_state(nlayer=nlayer, ndepth=ndepth)
    populate_state_from_pydantic(
        state_dts, site.initial_states, nlayer=nlayer, ndepth=ndepth,
        land_cover=site.properties.land_cover
    )
    populate_storedrainprm(state_dts, site.properties.land_cover)
    populate_roughnessstate(state_dts, site_dts)
    populate_ohmstate_defaults(state_dts)

    # Create forcing and timer objects (will be updated each timestep)
    forcing_dts = create_suews_forcing()
    timer_dts = create_suews_timer()

    # Output storage
    dts_outputs = {
        'QN': [], 'QF': [], 'QS': [], 'QH': [], 'QE': []
    }
    trad_outputs = {
        'QN': [], 'QF': [], 'QS': [], 'QH': [], 'QE': []
    }

    # Output indices in dataoutlinesuews
    output_indices = {'QN': 10, 'QF': 11, 'QS': 12, 'QH': 13, 'QE': 14}

    if verbose:
        print(f"\n{'='*70}")
        print(f"Running {n_timesteps} timesteps comparison")
        print(f"{'='*70}")

    # Run timestep by timestep
    for i in range(n_timesteps):
        dt = df_forcing_subset.index[i]
        row = df_forcing_subset.iloc[i]

        # Update forcing for this timestep
        populate_forcing_from_row(forcing_dts, row)

        # CRITICAL: For first timestep, populate atmstate from forcing
        # This sets the atmospheric state and dyohm temperatures
        if i == 0:
            populate_atmstate(state_dts, forcing_dts)

        # Update timer for this timestep
        # dt_since_start should be 0 for first timestep, then accumulate
        dt_since_start = i * tstep
        # Pass DLS parameters from site for correct daylight saving calculation
        startdls = int(site_dts.anthroemis.startdls)
        enddls = int(site_dts.anthroemis.enddls)
        populate_timer_from_datetime(timer_dts, dt, tstep, dt_since_start, startdls, enddls)

        # Run DTS simulation for this timestep
        output_line = drv.suews_cal_main(
            timer_dts,
            forcing_dts,
            config_dts,
            site_dts,
            state_dts  # Modified in-place!
        )

        # Store DTS outputs
        out_arr = output_line.dataoutlinesuews
        for var, idx in output_indices.items():
            dts_outputs[var].append(out_arr[idx])

        # Store traditional outputs
        df_out_row = df_output.iloc[i]
        for var in output_indices.keys():
            trad_outputs[var].append(df_out_row[('SUEWS', var)])

        # NOTE: timer.dt_since_start is updated by suews_cal_main,
        # but we handle it via dt_since_start parameter

    # Compare results
    results = {'n_timesteps': n_timesteps, 'max_diff': {}, 'first_diverge': {}}

    if verbose:
        print(f"\n{'Variable':8} {'Max Diff':15} {'First Diverge Step':20} {'Status'}")
        print("-" * 60)

    all_passed = True
    for var in output_indices.keys():
        dts_arr = np.array(dts_outputs[var])
        trad_arr = np.array(trad_outputs[var])
        diff = np.abs(dts_arr - trad_arr)
        max_diff = np.max(diff)
        results['max_diff'][var] = max_diff

        # Find first timestep where difference exceeds tolerance
        tolerance = 1e-10
        diverge_mask = diff > tolerance
        if np.any(diverge_mask):
            first_diverge = np.argmax(diverge_mask)
            results['first_diverge'][var] = int(first_diverge)
            all_passed = False
            status = f"FAIL at step {first_diverge}"
            if verbose:
                print(f"{var:8} {max_diff:15.6e} {first_diverge:20} {status}")
                # Show values at divergence point
                print(f"         DTS={dts_arr[first_diverge]:.10f}, Trad={trad_arr[first_diverge]:.10f}")
        else:
            results['first_diverge'][var] = None
            status = "PASS"
            if verbose:
                print(f"{var:8} {max_diff:15.6e} {'N/A':20} {status}")

    results['all_passed'] = all_passed

    if verbose:
        print(f"\n{'='*70}")
        if all_passed:
            print("SUCCESS: All outputs identical across all timesteps!")
        else:
            print("FAIL: Some outputs diverge")
        print(f"{'='*70}")

    return results


@pytest.mark.core
class TestDTSMultiTimestep:
    """Multi-timestep DTS parity tests."""

    def test_48_timesteps_parity(self):
        """Test 48 timesteps (4 hours) produce identical output."""
        results = run_multi_timestep_comparison(n_timesteps=48, verbose=True)
        assert results['all_passed'], \
            f"Parity failed. Max diffs: {results['max_diff']}, First diverge: {results['first_diverge']}"

    def test_single_timestep_parity(self):
        """Sanity check: single timestep should definitely work."""
        results = run_multi_timestep_comparison(n_timesteps=1, verbose=True)
        assert results['all_passed'], f"Single timestep failed: {results['max_diff']}"

    def test_two_timesteps_parity(self):
        """Test second timestep specifically (where divergence was observed)."""
        results = run_multi_timestep_comparison(n_timesteps=2, verbose=True)
        assert results['all_passed'], \
            f"Two timesteps failed. Diverge at: {results['first_diverge']}"


if __name__ == "__main__":
    print("Running multi-timestep DTS parity comparison...")
    results = run_multi_timestep_comparison(n_timesteps=48, verbose=True)

    if not results['all_passed']:
        print("\nDetailed divergence analysis:")
        for var, step in results['first_diverge'].items():
            if step is not None:
                print(f"  {var}: first diverges at step {step}, max_diff = {results['max_diff'][var]:.6e}")
