"""Comprehensive DTS parity test to find exact source of differences."""
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


def compare_values(name, our_val, gt_val, rtol=1e-10, atol=1e-12):
    """Compare values and return difference info."""
    if isinstance(our_val, np.ndarray):
        if our_val.shape != gt_val.shape:
            return f"{name}: SHAPE MISMATCH {our_val.shape} vs {gt_val.shape}"
        if not np.allclose(our_val, gt_val, rtol=rtol, atol=atol, equal_nan=True):
            diff = np.abs(our_val - gt_val)
            max_diff = np.nanmax(diff)
            max_idx = np.unravel_index(np.nanargmax(diff), diff.shape)
            return f"{name}: max_diff={max_diff:.2e} at {max_idx}, our={our_val.flat[np.nanargmax(diff)]:.6f}, gt={gt_val.flat[np.nanargmax(diff)]:.6f}"
    else:
        if our_val != gt_val:
            diff = abs(our_val - gt_val) if isinstance(our_val, (int, float)) else None
            return f"{name}: our={our_val}, gt={gt_val}, diff={diff}"
    return None


def get_nested_attrs(obj, prefix=""):
    """Get all numeric attributes from a DTS object recursively."""
    attrs = {}
    for attr_name in dir(obj):
        if attr_name.startswith('_'):
            continue
        try:
            val = getattr(obj, attr_name)
            full_name = f"{prefix}.{attr_name}" if prefix else attr_name

            if isinstance(val, (int, float, np.integer, np.floating)):
                attrs[full_name] = val
            elif isinstance(val, np.ndarray):
                attrs[full_name] = val.copy()
            elif hasattr(val, '_handle'):
                # Nested DTS object
                nested = get_nested_attrs(val, full_name)
                attrs.update(nested)
        except Exception:
            pass
    return attrs


@pytest.fixture(scope="module")
def ground_truth_and_our_dts():
    """Set up both traditional run and our DTS objects for comparison."""
    # Load sample data
    df_state_init, df_forcing = load_SampleData()
    grid_id = df_state_init.index[0]
    df_forcing_short = df_forcing.head(2)

    # Run traditional with debug_mode to get ground truth
    df_output, df_state_final, df_debug, dict_dts = run_supy(
        df_forcing_short, df_state_init, debug_mode=True
    )

    # Get ground truth state
    gt_state = dict_dts[grid_id].block[0]

    # Create our DTS objects
    config = SUEWSConfig.from_df_state(df_state_init.loc[[grid_id]])
    site = config.sites[0]
    model = config.model

    nlayer_val = site.properties.vertical_layers.nlayer
    nlayer = int(nlayer_val.value if hasattr(nlayer_val, 'value') else nlayer_val)
    ndepth = 5

    # Create and populate config
    config_dts = create_suews_config()
    populate_config_from_pydantic(config_dts, model)

    # Create and populate site
    site_dts = create_suews_site(nlayer=nlayer, ndepth=ndepth)
    populate_site_from_pydantic(site_dts, site, model)
    site_dts.cal_surf(config_dts)

    # Create and populate state
    state_dts = create_suews_state(nlayer=nlayer, ndepth=ndepth)
    populate_state_from_pydantic(state_dts, site.initial_states, nlayer=nlayer, ndepth=ndepth,
                                  land_cover=site.properties.land_cover)
    populate_storedrainprm(state_dts, site.properties.land_cover)
    populate_roughnessstate(state_dts, site_dts)
    populate_ohmstate_defaults(state_dts)

    # Create forcing and timer
    forcing_dts = create_suews_forcing()
    dt = df_forcing_short.index[0]
    row = df_forcing_short.iloc[0]
    populate_forcing_from_row(forcing_dts, row)
    populate_atmstate(state_dts, forcing_dts)

    timer_dts = create_suews_timer()
    populate_timer_from_datetime(timer_dts, dt, 300, 0)

    return {
        'gt_state': gt_state,
        'our_state': state_dts,
        'config_dts': config_dts,
        'site_dts': site_dts,
        'forcing_dts': forcing_dts,
        'timer_dts': timer_dts,
        'df_output': df_output,
        'nlayer': nlayer,
        'ndepth': ndepth,
    }


class TestDTSParity:
    """Tests to find exact differences between DTS and traditional."""

    def test_atmstate_parity(self, ground_truth_and_our_dts):
        """Compare atmstate values."""
        gt = ground_truth_and_our_dts['gt_state'].atmstate
        our = ground_truth_and_our_dts['our_state'].atmstate

        attrs = ['tair_av', 'avcp', 'avdens', 'lv_j_kg', 'es_hpa', 'ea_hpa',
                 'vpd_hpa', 'vpd_pa', 'dq', 'dens_dry', 'fcld']

        diffs = []
        for attr in attrs:
            try:
                our_val = getattr(our, attr)
                gt_val = getattr(gt, attr)
                diff = compare_values(f"atmstate.{attr}", our_val, gt_val)
                if diff:
                    diffs.append(diff)
            except Exception as e:
                diffs.append(f"atmstate.{attr}: ERROR {e}")

        assert not diffs, f"Differences found:\n" + "\n".join(diffs)

    def test_roughnessstate_parity(self, ground_truth_and_our_dts):
        """Compare roughnessstate values."""
        gt = ground_truth_and_our_dts['gt_state'].roughnessstate
        our = ground_truth_and_our_dts['our_state'].roughnessstate

        attrs = ['z0m', 'zdm', 'fai', 'zh', 'zzd']

        diffs = []
        for attr in attrs:
            try:
                our_val = getattr(our, attr)
                gt_val = getattr(gt, attr)
                diff = compare_values(f"roughnessstate.{attr}", our_val, gt_val)
                if diff:
                    diffs.append(diff)
            except Exception as e:
                diffs.append(f"roughnessstate.{attr}: ERROR {e}")

        assert not diffs, f"Differences found:\n" + "\n".join(diffs)

    def test_heatstate_parity(self, ground_truth_and_our_dts):
        """Compare heatstate values."""
        gt = ground_truth_and_our_dts['gt_state'].heatstate
        our = ground_truth_and_our_dts['our_state'].heatstate

        attrs = ['temp_surf', 'tsfc_surf', 'temp_roof', 'tsfc_roof',
                 'temp_wall', 'tsfc_wall']

        diffs = []
        for attr in attrs:
            try:
                our_val = getattr(our, attr)
                gt_val = getattr(gt, attr)
                diff = compare_values(f"heatstate.{attr}", our_val, gt_val)
                if diff:
                    diffs.append(diff)
            except Exception as e:
                diffs.append(f"heatstate.{attr}: ERROR {e}")

        assert not diffs, f"Differences found:\n" + "\n".join(diffs)

    def test_hydrostate_parity(self, ground_truth_and_our_dts):
        """Compare hydrostate values."""
        gt = ground_truth_and_our_dts['gt_state'].hydrostate
        our = ground_truth_and_our_dts['our_state'].hydrostate

        attrs = ['soilstore_surf', 'state_surf']

        diffs = []
        for attr in attrs:
            try:
                our_val = getattr(our, attr)
                gt_val = getattr(gt, attr)
                diff = compare_values(f"hydrostate.{attr}", our_val, gt_val)
                if diff:
                    diffs.append(diff)
            except Exception as e:
                diffs.append(f"hydrostate.{attr}: ERROR {e}")

        assert not diffs, f"Differences found:\n" + "\n".join(diffs)

    def test_phenstate_parity(self, ground_truth_and_our_dts):
        """Compare phenstate values."""
        gt = ground_truth_and_our_dts['gt_state'].phenstate
        our = ground_truth_and_our_dts['our_state'].phenstate

        attrs = ['lai_id', 'gdd_id', 'sdd_id', 'alb', 'decidcap_id', 'porosity_id']

        diffs = []
        for attr in attrs:
            try:
                our_val = getattr(our, attr)
                gt_val = getattr(gt, attr)
                diff = compare_values(f"phenstate.{attr}", our_val, gt_val)
                if diff:
                    diffs.append(diff)
            except Exception as e:
                diffs.append(f"phenstate.{attr}: ERROR {e}")

        assert not diffs, f"Differences found:\n" + "\n".join(diffs)

    def test_anthroemisstate_parity(self, ground_truth_and_our_dts):
        """Compare anthroemisstate values."""
        gt = ground_truth_and_our_dts['gt_state'].anthroemisstate
        our = ground_truth_and_our_dts['our_state'].anthroemisstate

        attrs = ['hdd_id']

        diffs = []
        for attr in attrs:
            try:
                our_val = getattr(our, attr)
                gt_val = getattr(gt, attr)
                diff = compare_values(f"anthroemisstate.{attr}", our_val, gt_val)
                if diff:
                    diffs.append(diff)
            except Exception as e:
                diffs.append(f"anthroemisstate.{attr}: ERROR {e}")

        assert not diffs, f"Differences found:\n" + "\n".join(diffs)

    def test_ohmstate_parity(self, ground_truth_and_our_dts):
        """Compare ohmstate values."""
        gt = ground_truth_and_our_dts['gt_state'].ohmstate
        our = ground_truth_and_our_dts['our_state'].ohmstate

        attrs = ['qn_av', 'dqndt', 'a1', 'a2', 'a3']

        diffs = []
        for attr in attrs:
            try:
                our_val = getattr(our, attr)
                gt_val = getattr(gt, attr)
                diff = compare_values(f"ohmstate.{attr}", our_val, gt_val)
                if diff:
                    diffs.append(diff)
            except Exception as e:
                diffs.append(f"ohmstate.{attr}: ERROR {e}")

        assert not diffs, f"Differences found:\n" + "\n".join(diffs)

    def test_snowstate_parity(self, ground_truth_and_our_dts):
        """Compare snowstate values."""
        gt = ground_truth_and_our_dts['gt_state'].snowstate
        our = ground_truth_and_our_dts['our_state'].snowstate

        attrs = ['snowpack', 'snowfrac', 'snowdens', 'snowalb', 'icefrac', 'snowwater']

        diffs = []
        for attr in attrs:
            try:
                our_val = getattr(our, attr)
                gt_val = getattr(gt, attr)
                diff = compare_values(f"snowstate.{attr}", our_val, gt_val)
                if diff:
                    diffs.append(diff)
            except Exception as e:
                diffs.append(f"snowstate.{attr}: ERROR {e}")

        assert not diffs, f"Differences found:\n" + "\n".join(diffs)

    def test_full_simulation_parity(self, ground_truth_and_our_dts):
        """Run simulation and compare outputs exactly."""
        data = ground_truth_and_our_dts

        # Run DTS simulation
        output_line = drv.suews_cal_main(
            data['timer_dts'],
            data['forcing_dts'],
            data['config_dts'],
            data['site_dts'],
            data['our_state']
        )

        # Get outputs
        out_arr = output_line.dataoutlinesuews
        df_out = data['df_output'].iloc[0]

        # Compare key outputs - indices: QN=10, QF=11, QS=12, QH=13, QE=14
        vars_to_check = [
            ('QN', 10, ('SUEWS', 'QN')),
            ('QF', 11, ('SUEWS', 'QF')),
            ('QS', 12, ('SUEWS', 'QS')),
            ('QH', 13, ('SUEWS', 'QH')),
            ('QE', 14, ('SUEWS', 'QE')),
        ]

        diffs = []
        for name, idx, col in vars_to_check:
            our_val = out_arr[idx]
            gt_val = df_out[col]
            diff = abs(our_val - gt_val)
            if diff > 1e-10:
                diffs.append(f"{name}: our={our_val:.10f}, gt={gt_val:.10f}, diff={diff:.2e}")

        assert not diffs, f"Output differences found:\n" + "\n".join(diffs)


if __name__ == "__main__":
    # Run as script for debugging
    import faulthandler
    faulthandler.enable()

    print("Setting up comparison...")
    data = ground_truth_and_our_dts.__wrapped__()  # Get fixture result directly

    print("\n=== Comparing state objects ===")

    # Compare all nested objects
    gt_state = data['gt_state']
    our_state = data['our_state']

    print("\n--- atmstate ---")
    for attr in ['tair_av', 'avcp', 'avdens', 'lv_j_kg', 'es_hpa', 'ea_hpa',
                 'vpd_hpa', 'vpd_pa', 'dq', 'dens_dry', 'fcld']:
        our = getattr(our_state.atmstate, attr)
        gt = getattr(gt_state.atmstate, attr)
        match = "OK" if np.allclose(our, gt, rtol=1e-10) else "DIFF"
        print(f"  {attr}: our={our:.6f} gt={gt:.6f} [{match}]")

    print("\n--- roughnessstate ---")
    for attr in ['z0m', 'zdm', 'fai', 'zh', 'zzd']:
        our = getattr(our_state.roughnessstate, attr)
        gt = getattr(gt_state.roughnessstate, attr)
        match = "OK" if np.allclose(our, gt, rtol=1e-10) else "DIFF"
        print(f"  {attr}: our={our:.6f} gt={gt:.6f} [{match}]")

    print("\n--- heatstate ---")
    for attr in ['temp_surf', 'tsfc_surf']:
        our = getattr(our_state.heatstate, attr)
        gt = getattr(gt_state.heatstate, attr)
        match = "OK" if np.allclose(our, gt, rtol=1e-10, equal_nan=True) else "DIFF"
        if match == "DIFF":
            print(f"  {attr}: DIFF")
            print(f"    our: {our}")
            print(f"    gt:  {gt}")
        else:
            print(f"  {attr}: OK")

    print("\n--- phenstate ---")
    for attr in ['lai_id', 'gdd_id', 'sdd_id', 'alb', 'decidcap_id', 'porosity_id']:
        try:
            our = getattr(our_state.phenstate, attr)
            gt = getattr(gt_state.phenstate, attr)
            match = "OK" if np.allclose(our, gt, rtol=1e-10, equal_nan=True) else "DIFF"
            if match == "DIFF":
                print(f"  {attr}: DIFF")
                print(f"    our: {our}")
                print(f"    gt:  {gt}")
            else:
                print(f"  {attr}: OK")
        except Exception as e:
            print(f"  {attr}: ERROR {e}")

    print("\n--- ohmstate ---")
    for attr in ['qn_av', 'dqndt', 'a1', 'a2', 'a3']:
        try:
            our = getattr(our_state.ohmstate, attr)
            gt = getattr(gt_state.ohmstate, attr)
            if isinstance(our, np.ndarray):
                match = "OK" if np.allclose(our, gt, rtol=1e-10, equal_nan=True) else "DIFF"
            else:
                match = "OK" if abs(our - gt) < 1e-10 else "DIFF"
            if match == "DIFF":
                print(f"  {attr}: DIFF our={our} gt={gt}")
            else:
                print(f"  {attr}: OK")
        except Exception as e:
            print(f"  {attr}: ERROR {e}")

    print("\n=== Running simulation ===")
    output_line = drv.suews_cal_main(
        data['timer_dts'],
        data['forcing_dts'],
        data['config_dts'],
        data['site_dts'],
        data['our_state']
    )

    out_arr = output_line.dataoutlinesuews
    df_out = data['df_output'].iloc[0]

    print("\nVariable          DTS             Traditional      Diff")
    print("-" * 60)
    for name, idx, col in [('QN', 10, ('SUEWS', 'QN')), ('QF', 11, ('SUEWS', 'QF')),
                            ('QS', 12, ('SUEWS', 'QS')), ('QH', 13, ('SUEWS', 'QH')),
                            ('QE', 14, ('SUEWS', 'QE'))]:
        our = out_arr[idx]
        gt = df_out[col]
        diff = our - gt
        print(f"{name:8} {our:18.10f} {gt:18.10f} {diff:12.2e}")
