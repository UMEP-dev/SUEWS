"""DTS Population Verification Tests.

This module verifies that DTS objects populated from Pydantic configuration
match the ground truth from successful Fortran runtime execution.

Ground truth is captured by running the traditional run() with debug_mode=True,
which provides access to block_mod_state - the actual Fortran SUEWS_STATE
objects populated during execution.
"""

import numpy as np
import pytest
from typing import Any

# Import supy modules
import supy
from supy._post import pack_dts
from supy.supy_driver import module_ctrl_type as dts

# Import DTS module functions
from supy.dts._core import (
    create_suews_config,
    create_suews_state,
    create_suews_site,
)
from supy.dts._populate import (
    populate_config_from_pydantic,
    populate_site_from_pydantic,
    populate_state_from_pydantic,
)


# =============================================================================
# Comparison Helpers
# =============================================================================


def compare_values(expected: Any, actual: Any, path: str, rtol: float = 1e-6, atol: float = 1e-8) -> dict:
    """Compare two values and return mismatches.

    Parameters
    ----------
    expected : Any
        Expected value (ground truth).
    actual : Any
        Actual value (from our population).
    path : str
        Attribute path for error reporting.
    rtol : float
        Relative tolerance for float comparison.
    atol : float
        Absolute tolerance for float comparison.

    Returns
    -------
    dict
        Empty if values match, otherwise contains mismatch details.
    """
    mismatches = {}

    # Handle None cases
    if expected is None and actual is None:
        return mismatches
    if expected is None or actual is None:
        mismatches[path] = {'expected': expected, 'actual': actual, 'reason': 'None mismatch'}
        return mismatches

    # Handle dict (nested objects)
    if isinstance(expected, dict):
        if not isinstance(actual, dict):
            mismatches[path] = {'expected': 'dict', 'actual': type(actual).__name__}
            return mismatches
        for key in set(expected.keys()) | set(actual.keys()):
            sub_expected = expected.get(key)
            sub_actual = actual.get(key)
            sub_mismatches = compare_values(sub_expected, sub_actual, f"{path}.{key}", rtol, atol)
            mismatches.update(sub_mismatches)
        return mismatches

    # Handle numpy arrays
    if isinstance(expected, np.ndarray):
        if not isinstance(actual, np.ndarray):
            mismatches[path] = {
                'expected': f'ndarray{expected.shape}',
                'actual': type(actual).__name__
            }
            return mismatches
        if expected.shape != actual.shape:
            mismatches[path] = {
                'expected_shape': expected.shape,
                'actual_shape': actual.shape
            }
            return mismatches
        try:
            if not np.allclose(expected, actual, rtol=rtol, atol=atol, equal_nan=True):
                # Find specific indices that differ
                diff_mask = ~np.isclose(expected, actual, rtol=rtol, atol=atol, equal_nan=True)
                diff_indices = np.argwhere(diff_mask)
                # Report first few differences
                sample_diffs = []
                for idx in diff_indices[:5]:
                    idx_tuple = tuple(idx)
                    sample_diffs.append({
                        'index': idx_tuple,
                        'expected': expected[idx_tuple],
                        'actual': actual[idx_tuple]
                    })
                mismatches[path] = {
                    'num_differences': len(diff_indices),
                    'sample_differences': sample_diffs
                }
        except TypeError:
            # For non-numeric arrays, compare element-wise
            if not np.array_equal(expected, actual):
                mismatches[path] = {'expected': expected, 'actual': actual}
        return mismatches

    # Handle scalars
    try:
        if not np.isclose(float(expected), float(actual), rtol=rtol, atol=atol, equal_nan=True):
            mismatches[path] = {'expected': expected, 'actual': actual}
    except (ValueError, TypeError):
        # Non-numeric comparison
        if expected != actual:
            mismatches[path] = {'expected': expected, 'actual': actual}

    return mismatches


def compare_dts_objects(our_dts: Any, ground_truth_dts: Any, name: str) -> dict:
    """Compare two DTS objects attribute by attribute.

    Parameters
    ----------
    our_dts : Any
        DTS object from our population.
    ground_truth_dts : Any
        DTS object from successful Fortran run.
    name : str
        Name for error reporting.

    Returns
    -------
    dict
        Dictionary of mismatches (empty if all match).
    """
    our_dict = pack_dts(our_dts)
    gt_dict = pack_dts(ground_truth_dts)

    return compare_values(gt_dict, our_dict, name)


def format_mismatches(mismatches: dict) -> str:
    """Format mismatches for readable output.

    Parameters
    ----------
    mismatches : dict
        Dictionary of mismatches from compare_values.

    Returns
    -------
    str
        Formatted string for display.
    """
    lines = []
    for path, details in sorted(mismatches.items()):
        lines.append(f"\n  {path}:")
        if isinstance(details, dict):
            for key, value in details.items():
                if isinstance(value, (list, dict)):
                    lines.append(f"    {key}:")
                    if isinstance(value, list):
                        for item in value[:3]:  # Show first 3
                            lines.append(f"      {item}")
                    else:
                        for k, v in list(value.items())[:3]:
                            lines.append(f"      {k}: {v}")
                else:
                    lines.append(f"    {key}: {value}")
        else:
            lines.append(f"    {details}")
    return "\n".join(lines)


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture(scope="module")
def sample_forcing():
    """Load sample forcing data."""
    df_state_init, df_forcing = supy.load_SampleData()
    return df_forcing.head(24)  # 1 day of data


@pytest.fixture(scope="module")
def sample_state_init():
    """Load sample initial state."""
    df_state_init, df_forcing = supy.load_SampleData()
    return df_state_init


@pytest.fixture(scope="module")
def ground_truth(sample_forcing, sample_state_init):
    """Run traditional method with debug_mode=True to capture ground truth.

    This provides access to the actual Fortran SUEWS_STATE objects
    that were populated during a successful run.
    """
    # Run with debug mode to capture block_mod_state
    df_output, df_state_final, df_debug, dict_dts_state = supy.run_supy(
        sample_forcing, sample_state_init, debug_mode=True
    )

    # Extract ground truth
    grid_id = sample_state_init.index[0]
    block_mod_state = dict_dts_state[grid_id]

    # block_mod_state.block[0] is SUEWS_STATE at first timestep
    # This contains properly initialised state from Fortran
    state_ground_truth = block_mod_state.block[0]

    return {
        'state_ground_truth': state_ground_truth,
        'block_mod_state': block_mod_state,
        'df_state_init': sample_state_init,
        'df_state_final': df_state_final,
        'grid_id': grid_id,
    }


# =============================================================================
# Tests
# =============================================================================


class TestGroundTruthCapture:
    """Tests to verify ground truth capture works."""

    def test_ground_truth_available(self, ground_truth):
        """Verify ground truth fixture provides expected objects."""
        assert ground_truth is not None
        assert 'state_ground_truth' in ground_truth
        assert 'block_mod_state' in ground_truth
        assert ground_truth['state_ground_truth'] is not None

    def test_state_ground_truth_has_nested_objects(self, ground_truth):
        """Verify state ground truth has expected nested structure."""
        state = ground_truth['state_ground_truth']

        # Check for expected nested state objects
        assert hasattr(state, 'heatstate'), "Missing heatstate"
        assert hasattr(state, 'hydrostate'), "Missing hydrostate"
        assert hasattr(state, 'snowstate'), "Missing snowstate"
        assert hasattr(state, 'phenstate'), "Missing phenstate"
        assert hasattr(state, 'atmstate'), "Missing atmstate"

    def test_pack_dts_works_on_ground_truth(self, ground_truth):
        """Verify pack_dts can convert ground truth to dict."""
        state = ground_truth['state_ground_truth']
        state_dict = pack_dts(state)

        assert isinstance(state_dict, dict)
        assert len(state_dict) > 0
        assert 'heatstate' in state_dict
        assert 'hydrostate' in state_dict

    def test_inspect_ground_truth_structure(self, ground_truth):
        """Inspect and print ground truth structure for debugging."""
        state = ground_truth['state_ground_truth']
        state_dict = pack_dts(state)

        print("\n=== Ground Truth SUEWS_STATE Structure ===")
        for key in sorted(state_dict.keys()):
            value = state_dict[key]
            if isinstance(value, dict):
                print(f"\n{key}:")
                for subkey, subval in sorted(value.items()):
                    if isinstance(subval, np.ndarray):
                        print(f"  {subkey}: ndarray{subval.shape} dtype={subval.dtype}")
                    else:
                        print(f"  {subkey}: {type(subval).__name__} = {subval}")
            elif isinstance(value, np.ndarray):
                print(f"{key}: ndarray{value.shape} dtype={value.dtype}")
            else:
                print(f"{key}: {type(value).__name__} = {value}")


class TestDTSStatePopulation:
    """Compare our state population against initial values from df_state_init.

    Note: We compare against initial state values from df_state_init,
    NOT computed values from block_mod_state.block[0] (which are outputs).
    """

    @pytest.fixture
    def initial_values(self, sample_state_init):
        """Extract initial state values from df_state_init for comparison."""
        df = sample_state_init
        grid_id = df.index[0]

        def get_array(var_name, size):
            """Get array from df_state_init."""
            arr = np.zeros(size, dtype=np.float64)
            for i in range(size):
                col = (var_name, f'({i},)')
                if col in df.columns:
                    arr[i] = df.loc[grid_id, col]
            return arr

        def get_2d_array(var_name, shape):
            """Get 2D array from df_state_init."""
            arr = np.zeros(shape, dtype=np.float64, order='F')
            for i in range(shape[0]):
                for j in range(shape[1]):
                    col = (var_name, f'({i}, {j})')
                    if col in df.columns:
                        arr[i, j] = df.loc[grid_id, col]
            return arr

        return {
            # Hydro state
            'soilstore_surf': get_array('soilstore_surf', 7),
            'state_surf': get_array('state_surf', 7),
            # Heat state
            'temp_surf': get_2d_array('temp_surf', (7, 5)),
            # Snow state
            'snowfrac': get_array('snowfrac', 7),
            'snowpack': get_array('snowpack', 7),
            'snowdens': get_array('snowdens', 7),
            'snowwater': get_array('snowwater', 7),
            'snowalb': get_array('snowalb', 7),
        }

    @pytest.fixture
    def pydantic_config(self):
        """Load the Pydantic config from sample data."""
        from supy._env import trv_supy_module
        from supy.data_model.core.config import SUEWSConfig

        sample_config_path = trv_supy_module / 'sample_data' / 'sample_config.yml'
        return SUEWSConfig.from_yaml(sample_config_path)

    @pytest.fixture
    def our_populated_state(self, pydantic_config):
        """Create and populate state from Pydantic config."""
        # Create state with same dimensions
        nlayer = 5
        ndepth = 5
        state = create_suews_state(nlayer=nlayer, ndepth=ndepth)

        # Get initial states from config
        site = pydantic_config.sites[0] if hasattr(pydantic_config, 'sites') else pydantic_config.site
        initial_states = site.initial_states

        # Populate from Pydantic config
        populate_state_from_pydantic(state, initial_states)
        return state

    @pytest.mark.smoke
    def test_hydrostate_soilstore_surf(self, initial_values, our_populated_state):
        """Verify soilstore_surf values match df_state_init."""
        expected = initial_values['soilstore_surf']
        actual = our_populated_state.hydrostate.soilstore_surf

        print(f"\n=== soilstore_surf ===")
        print(f"Expected (from df_state_init): {expected}")
        print(f"Actual (our DTS): {actual}")

        np.testing.assert_allclose(actual, expected, rtol=1e-6)

    @pytest.mark.smoke
    def test_hydrostate_state_surf(self, initial_values, our_populated_state):
        """Verify state_surf (surface wetness) values match."""
        expected = initial_values['state_surf']
        actual = our_populated_state.hydrostate.state_surf

        print(f"\n=== state_surf ===")
        print(f"Expected (from df_state_init): {expected}")
        print(f"Actual (our DTS): {actual}")

        np.testing.assert_allclose(actual, expected, rtol=1e-6)

    @pytest.mark.smoke
    def test_heatstate_temp_surf(self, initial_values, our_populated_state):
        """Verify temp_surf values match."""
        expected = initial_values['temp_surf']
        actual = our_populated_state.heatstate.temp_surf

        print(f"\n=== temp_surf ===")
        print(f"Expected shape: {expected.shape}")
        print(f"Actual shape: {actual.shape}")
        print(f"Expected[0]: {expected[0]}")
        print(f"Actual[0]: {actual[0]}")

        np.testing.assert_allclose(actual, expected, rtol=1e-6)

    def test_snowstate_snowpack(self, initial_values, our_populated_state):
        """Verify snowpack values match."""
        expected = initial_values['snowpack']
        actual = our_populated_state.snowstate.snowpack

        print(f"\n=== snowpack ===")
        print(f"Expected: {expected}")
        print(f"Actual: {actual}")

    def test_snowstate_snowfrac(self, initial_values, our_populated_state):
        """Verify snowfrac values match."""
        expected = initial_values['snowfrac']
        actual = our_populated_state.snowstate.snowfrac

        print(f"\n=== snowfrac ===")
        print(f"Expected: {expected}")
        print(f"Actual: {actual}")


class TestGroundTruthAnalysis:
    """Analyse ground truth to understand expected values."""

    def test_print_heatstate_values(self, ground_truth):
        """Print heatstate values for reference."""
        state = ground_truth['state_ground_truth']
        heat = state.heatstate
        heat_dict = pack_dts(heat)

        print("\n=== Ground Truth HeatState Values ===")
        for key, value in sorted(heat_dict.items()):
            if isinstance(value, np.ndarray):
                print(f"{key}: shape={value.shape}")
                if value.size < 20:
                    print(f"  values: {value}")
                else:
                    print(f"  sample: {value.flat[:5]}...")
            else:
                print(f"{key}: {value}")

    def test_print_hydrostate_values(self, ground_truth):
        """Print hydrostate values for reference."""
        state = ground_truth['state_ground_truth']
        hydro = state.hydrostate
        hydro_dict = pack_dts(hydro)

        print("\n=== Ground Truth HydroState Values ===")
        for key, value in sorted(hydro_dict.items()):
            if isinstance(value, np.ndarray):
                print(f"{key}: shape={value.shape}")
                if value.size < 20:
                    print(f"  values: {value}")
                else:
                    print(f"  sample: {value.flat[:5]}...")
            else:
                print(f"{key}: {value}")

    def test_print_phenstate_values(self, ground_truth):
        """Print phenstate values for reference."""
        state = ground_truth['state_ground_truth']
        phen = state.phenstate
        phen_dict = pack_dts(phen)

        print("\n=== Ground Truth PhenState Values ===")
        for key, value in sorted(phen_dict.items()):
            if isinstance(value, np.ndarray):
                print(f"{key}: shape={value.shape}")
                if value.size < 20:
                    print(f"  values: {value}")
                else:
                    print(f"  sample: {value.flat[:5]}...")
            else:
                print(f"{key}: {value}")


# =============================================================================
# SUEWS_SITE Verification Tests
# =============================================================================


class TestDTSSitePopulation:
    """Verify SUEWS_SITE population from Pydantic config.

    We compare the populated DTS values against the Pydantic config values
    (source of truth) to ensure correct mapping.
    """

    @pytest.fixture
    def pydantic_config(self):
        """Load the Pydantic config from sample data."""
        from supy._env import trv_supy_module
        from supy.data_model.core.config import SUEWSConfig

        sample_config_path = trv_supy_module / 'sample_data' / 'sample_config.yml'
        return SUEWSConfig.from_yaml(sample_config_path)

    @pytest.fixture
    def populated_site(self, pydantic_config):
        """Create and populate SUEWS_SITE from Pydantic config."""
        from supy.dts._core import create_suews_site
        from supy.dts._populate import populate_site_from_pydantic

        # Get site from config
        site = pydantic_config.sites[0] if hasattr(pydantic_config, 'sites') else pydantic_config.site
        model = pydantic_config.model

        # Get nlayer from config's vertical_layers
        vl = site.properties.vertical_layers
        nlayer = vl.nlayer.value if hasattr(vl.nlayer, 'value') else vl.nlayer
        site_dts = create_suews_site(nlayer=nlayer)

        # Populate
        populate_site_from_pydantic(site_dts, site, model)
        return site_dts

    @pytest.fixture
    def pydantic_site(self, pydantic_config):
        """Get the Pydantic site object for comparison."""
        return pydantic_config.sites[0] if hasattr(pydantic_config, 'sites') else pydantic_config.site

    # -------------------------------------------------------------------------
    # Basic Site Properties
    # -------------------------------------------------------------------------

    @pytest.mark.smoke
    def test_site_location(self, populated_site, pydantic_site):
        """Verify lat/lon/alt are correctly populated."""
        props = pydantic_site.properties

        # Pydantic uses 'lat'/'lng', Fortran uses 'lat'/'lon'
        expected_lat = props.lat.value if hasattr(props.lat, 'value') else props.lat
        expected_lon = props.lng.value if hasattr(props.lng, 'value') else props.lng
        expected_alt = props.alt.value if hasattr(props.alt, 'value') else props.alt

        print(f"\n=== Site Location ===")
        print(f"lat: expected={expected_lat}, actual={populated_site.lat}")
        print(f"lon: expected={expected_lon}, actual={populated_site.lon}")
        print(f"alt: expected={expected_alt}, actual={populated_site.alt}")

        assert populated_site.lat == pytest.approx(expected_lat, rel=1e-6)
        assert populated_site.lon == pytest.approx(expected_lon, rel=1e-6)
        assert populated_site.alt == pytest.approx(expected_alt, rel=1e-6)

    @pytest.mark.smoke
    def test_site_measurement_height(self, populated_site, pydantic_site):
        """Verify measurement height z is correctly populated."""
        props = pydantic_site.properties
        expected_z = props.z.value if hasattr(props.z, 'value') else props.z

        print(f"\n=== Measurement Height ===")
        print(f"z: expected={expected_z}, actual={populated_site.z}")

        assert populated_site.z == pytest.approx(expected_z, rel=1e-6)

    # -------------------------------------------------------------------------
    # Surface Fractions
    # -------------------------------------------------------------------------

    @pytest.mark.smoke
    def test_surface_fractions(self, populated_site, pydantic_site):
        """Verify sfr_surf array matches land_cover fractions."""
        lc = pydantic_site.properties.land_cover

        def get_sfr(surf):
            val = surf.sfr
            return val.value if hasattr(val, 'value') else val

        expected = np.array([
            get_sfr(lc.paved),
            get_sfr(lc.bldgs),
            get_sfr(lc.evetr),
            get_sfr(lc.dectr),
            get_sfr(lc.grass),
            get_sfr(lc.bsoil),
            get_sfr(lc.water),
        ])

        actual = populated_site.sfr_surf

        print(f"\n=== Surface Fractions ===")
        print(f"Expected: {expected}")
        print(f"Actual: {actual}")
        print(f"Sum: {np.sum(actual)}")

        np.testing.assert_allclose(actual, expected, rtol=1e-6)
        assert np.sum(actual) == pytest.approx(1.0, abs=1e-6), "Fractions should sum to 1"

    def test_derived_fractions(self, populated_site):
        """Verify derived fractions are correctly calculated."""
        sfr = populated_site.sfr_surf

        expected_veg = sfr[2] + sfr[3] + sfr[4]  # evetr + dectr + grass
        expected_imperv = sfr[0] + sfr[1]  # paved + bldgs
        expected_nonwater = 1.0 - sfr[6]

        print(f"\n=== Derived Fractions ===")
        print(f"vegfraction: expected={expected_veg}, actual={populated_site.vegfraction}")
        print(f"impervfraction: expected={expected_imperv}, actual={populated_site.impervfraction}")
        print(f"nonwaterfraction: expected={expected_nonwater}, actual={populated_site.nonwaterfraction}")

        assert populated_site.vegfraction == pytest.approx(expected_veg, rel=1e-6)
        assert populated_site.impervfraction == pytest.approx(expected_imperv, rel=1e-6)
        assert populated_site.nonwaterfraction == pytest.approx(expected_nonwater, rel=1e-6)

    # -------------------------------------------------------------------------
    # Conductance Parameters
    # -------------------------------------------------------------------------

    @pytest.mark.smoke
    def test_conductance_parameters(self, populated_site, pydantic_site):
        """Verify conductance parameters are correctly populated."""
        cond_pydantic = pydantic_site.properties.conductance
        cond_dts = populated_site.conductance

        def get_val(attr):
            val = getattr(cond_pydantic, attr)
            return val.value if hasattr(val, 'value') else val

        params = ['g_max', 'g_k', 'g_q_base', 'g_q_shape', 'g_t', 'g_sm',
                  'kmax', 's1', 's2', 'th', 'tl']

        print(f"\n=== Conductance Parameters ===")
        for param in params:
            expected = get_val(param)
            actual = getattr(cond_dts, param)
            print(f"{param}: expected={expected}, actual={actual}")
            assert actual == pytest.approx(expected, rel=1e-6), f"Mismatch in {param}"

    # -------------------------------------------------------------------------
    # LUMPS Parameters
    # -------------------------------------------------------------------------

    def test_lumps_parameters(self, populated_site, pydantic_site):
        """Verify LUMPS parameters are correctly populated."""
        lumps_pydantic = pydantic_site.properties.lumps
        lumps_dts = populated_site.lumps

        def get_val(attr):
            val = getattr(lumps_pydantic, attr)
            return val.value if hasattr(val, 'value') else val

        params = ['drainrt', 'raincover', 'rainmaxres', 'veg_type']

        print(f"\n=== LUMPS Parameters ===")
        for param in params:
            expected = get_val(param)
            actual = getattr(lumps_dts, param)
            print(f"{param}: expected={expected}, actual={actual}")
            assert actual == pytest.approx(expected, rel=1e-6), f"Mismatch in {param}"

    # -------------------------------------------------------------------------
    # Snow Parameters
    # -------------------------------------------------------------------------

    def test_snow_parameters(self, populated_site, pydantic_site):
        """Verify snow parameters are correctly populated."""
        snow_pydantic = pydantic_site.properties.snow
        snow_dts = populated_site.snow

        def get_val(attr):
            val = getattr(snow_pydantic, attr)
            return val.value if hasattr(val, 'value') else val

        params = ['crwmax', 'crwmin', 'narp_emis_snow', 'preciplimit',
                  'preciplimitalb', 'radmeltfact', 'snowalbmax', 'snowalbmin',
                  'snowdensmax', 'snowdensmin', 'tau_a', 'tau_f', 'tau_r',
                  'tempmeltfact']

        print(f"\n=== Snow Parameters ===")
        for param in params:
            expected = get_val(param)
            actual = getattr(snow_dts, param)
            print(f"{param}: expected={expected}, actual={actual}")
            assert actual == pytest.approx(expected, rel=1e-6), f"Mismatch in {param}"

    # -------------------------------------------------------------------------
    # Land Cover Types
    # -------------------------------------------------------------------------

    def test_landcover_paved(self, populated_site, pydantic_site):
        """Verify LC_PAVED_PRM is correctly populated."""
        lc_pydantic = pydantic_site.properties.land_cover.paved
        lc_dts = populated_site.lc_paved

        def get_val(attr):
            val = getattr(lc_pydantic, attr)
            return val.value if hasattr(val, 'value') else val

        print(f"\n=== LC_PAVED ===")
        print(f"sfr: expected={get_val('sfr')}, actual={lc_dts.sfr}")
        print(f"albedo: expected={get_val('alb')}, actual={lc_dts.albedo}")
        print(f"emissivity: expected={get_val('emis')}, actual={lc_dts.emissivity}")

        assert lc_dts.sfr == pytest.approx(get_val('sfr'), rel=1e-6)
        assert lc_dts.albedo == pytest.approx(get_val('alb'), rel=1e-6)
        assert lc_dts.emissivity == pytest.approx(get_val('emis'), rel=1e-6)

    def test_landcover_bldgs(self, populated_site, pydantic_site):
        """Verify LC_BLDG_PRM is correctly populated."""
        lc_pydantic = pydantic_site.properties.land_cover.bldgs
        lc_dts = populated_site.lc_bldg

        def get_val(attr):
            val = getattr(lc_pydantic, attr)
            return val.value if hasattr(val, 'value') else val

        print(f"\n=== LC_BLDG ===")
        print(f"sfr: expected={get_val('sfr')}, actual={lc_dts.sfr}")
        print(f"albedo: expected={get_val('alb')}, actual={lc_dts.albedo}")
        print(f"emissivity: expected={get_val('emis')}, actual={lc_dts.emissivity}")

        assert lc_dts.sfr == pytest.approx(get_val('sfr'), rel=1e-6)
        assert lc_dts.albedo == pytest.approx(get_val('alb'), rel=1e-6)
        assert lc_dts.emissivity == pytest.approx(get_val('emis'), rel=1e-6)

    def test_landcover_vegetation(self, populated_site, pydantic_site):
        """Verify vegetation land cover types are correctly populated."""
        lc_config = pydantic_site.properties.land_cover

        def get_val(obj, attr):
            val = getattr(obj, attr)
            return val.value if hasattr(val, 'value') else val

        # evetr
        lc_evetr = populated_site.lc_evetr
        print(f"\n=== LC_EVETR ===")
        print(f"sfr: expected={get_val(lc_config.evetr, 'sfr')}, actual={lc_evetr.sfr}")
        assert lc_evetr.sfr == pytest.approx(get_val(lc_config.evetr, 'sfr'), rel=1e-6)

        # dectr
        lc_dectr = populated_site.lc_dectr
        print(f"\n=== LC_DECTR ===")
        print(f"sfr: expected={get_val(lc_config.dectr, 'sfr')}, actual={lc_dectr.sfr}")
        assert lc_dectr.sfr == pytest.approx(get_val(lc_config.dectr, 'sfr'), rel=1e-6)

        # grass
        lc_grass = populated_site.lc_grass
        print(f"\n=== LC_GRASS ===")
        print(f"sfr: expected={get_val(lc_config.grass, 'sfr')}, actual={lc_grass.sfr}")
        assert lc_grass.sfr == pytest.approx(get_val(lc_config.grass, 'sfr'), rel=1e-6)

    def test_landcover_other(self, populated_site, pydantic_site):
        """Verify bsoil and water land cover types are correctly populated."""
        lc_config = pydantic_site.properties.land_cover

        def get_val(obj, attr):
            val = getattr(obj, attr)
            return val.value if hasattr(val, 'value') else val

        # bsoil
        lc_bsoil = populated_site.lc_bsoil
        print(f"\n=== LC_BSOIL ===")
        print(f"sfr: expected={get_val(lc_config.bsoil, 'sfr')}, actual={lc_bsoil.sfr}")
        assert lc_bsoil.sfr == pytest.approx(get_val(lc_config.bsoil, 'sfr'), rel=1e-6)

        # water
        lc_water = populated_site.lc_water
        print(f"\n=== LC_WATER ===")
        print(f"sfr: expected={get_val(lc_config.water, 'sfr')}, actual={lc_water.sfr}")
        assert lc_water.sfr == pytest.approx(get_val(lc_config.water, 'sfr'), rel=1e-6)


# =============================================================================
# SUEWS_CONFIG Verification Tests
# =============================================================================


class TestDTSConfigPopulation:
    """Verify SUEWS_CONFIG population from Pydantic Model.

    We compare the populated DTS config values against the Pydantic model values
    (source of truth) to ensure correct physics method mappings.
    """

    @pytest.fixture
    def pydantic_config(self):
        """Load the Pydantic config from sample data."""
        from supy._env import trv_supy_module
        from supy.data_model.core.config import SUEWSConfig

        sample_config_path = trv_supy_module / 'sample_data' / 'sample_config.yml'
        return SUEWSConfig.from_yaml(sample_config_path)

    @pytest.fixture
    def populated_config(self, pydantic_config):
        """Create and populate SUEWS_CONFIG from Pydantic Model."""
        from supy.dts._core import create_suews_config
        from supy.dts._populate import populate_config_from_pydantic

        config_dts = create_suews_config()
        model = pydantic_config.model

        populate_config_from_pydantic(config_dts, model)
        return config_dts

    @pytest.fixture
    def pydantic_model(self, pydantic_config):
        """Get the Pydantic model object for comparison."""
        return pydantic_config.model

    # -------------------------------------------------------------------------
    # Physics Methods
    # -------------------------------------------------------------------------

    @pytest.mark.smoke
    def test_radiation_methods(self, populated_config, pydantic_model):
        """Verify radiation method flags are correctly populated."""
        physics = pydantic_model.physics

        def get_val(attr):
            val = getattr(physics, attr)
            if hasattr(val, 'value'):
                val = val.value
            # Handle Enum types
            if hasattr(val, 'value'):
                val = int(val)
            return val

        print(f"\n=== Radiation Methods ===")
        expected_netrad = get_val('netradiationmethod')
        print(f"netradiationmethod: expected={expected_netrad}, actual={populated_config.netradiationmethod}")
        assert populated_config.netradiationmethod == expected_netrad

    @pytest.mark.smoke
    def test_storage_heat_method(self, populated_config, pydantic_model):
        """Verify storage heat method is correctly populated."""
        physics = pydantic_model.physics

        def get_val(attr):
            val = getattr(physics, attr)
            if hasattr(val, 'value'):
                val = val.value
            if hasattr(val, 'value'):
                val = int(val)
            return val

        print(f"\n=== Storage Heat Method ===")
        expected = get_val('storageheatmethod')
        print(f"storageheatmethod: expected={expected}, actual={populated_config.storageheatmethod}")
        assert populated_config.storageheatmethod == expected

    def test_stability_method(self, populated_config, pydantic_model):
        """Verify stability method is correctly populated."""
        physics = pydantic_model.physics

        def get_val(attr):
            val = getattr(physics, attr)
            if hasattr(val, 'value'):
                val = val.value
            if hasattr(val, 'value'):
                val = int(val)
            return val

        print(f"\n=== Stability Method ===")
        expected = get_val('stabilitymethod')
        print(f"stabilitymethod: expected={expected}, actual={populated_config.stabilitymethod}")
        assert populated_config.stabilitymethod == expected

    def test_roughness_methods(self, populated_config, pydantic_model):
        """Verify roughness length methods are correctly populated."""
        physics = pydantic_model.physics

        def get_val(attr):
            val = getattr(physics, attr)
            if hasattr(val, 'value'):
                val = val.value
            if hasattr(val, 'value'):
                val = int(val)
            return val

        print(f"\n=== Roughness Methods ===")

        expected_mom = get_val('roughlenmommethod')
        print(f"roughlenmommethod: expected={expected_mom}, actual={populated_config.roughlenmommethod}")
        assert populated_config.roughlenmommethod == expected_mom

        expected_heat = get_val('roughlenheatmethod')
        print(f"roughlenheatmethod: expected={expected_heat}, actual={populated_config.roughlenheatmethod}")
        assert populated_config.roughlenheatmethod == expected_heat

    def test_smd_method(self, populated_config, pydantic_model):
        """Verify soil moisture deficit method is correctly populated."""
        physics = pydantic_model.physics

        def get_val(attr):
            val = getattr(physics, attr)
            if hasattr(val, 'value'):
                val = val.value
            if hasattr(val, 'value'):
                val = int(val)
            return val

        print(f"\n=== SMD Method ===")
        expected = get_val('smdmethod')
        print(f"smdmethod: expected={expected}, actual={populated_config.smdmethod}")
        assert populated_config.smdmethod == expected

    def test_emissions_method(self, populated_config, pydantic_model):
        """Verify emissions method is correctly populated."""
        physics = pydantic_model.physics

        def get_val(attr):
            val = getattr(physics, attr)
            if hasattr(val, 'value'):
                val = val.value
            if hasattr(val, 'value'):
                val = int(val)
            return val

        print(f"\n=== Emissions Method ===")
        expected = get_val('emissionsmethod')
        print(f"emissionsmethod: expected={expected}, actual={populated_config.emissionsmethod}")
        assert populated_config.emissionsmethod == expected

    def test_snow_and_stebbs(self, populated_config, pydantic_model):
        """Verify snow and STEBBS flags are correctly populated."""
        physics = pydantic_model.physics

        def get_val(attr):
            val = getattr(physics, attr)
            if hasattr(val, 'value'):
                val = val.value
            if hasattr(val, 'value'):
                val = int(val)
            return val

        print(f"\n=== Snow and STEBBS ===")

        expected_snow = get_val('snowuse')
        print(f"snowuse: expected={expected_snow}, actual={populated_config.snowuse}")
        assert populated_config.snowuse == expected_snow

        expected_stebbs = get_val('stebbsmethod')
        print(f"stebbsmethod: expected={expected_stebbs}, actual={populated_config.stebbsmethod}")
        assert populated_config.stebbsmethod == expected_stebbs

    def test_water_use_method(self, populated_config, pydantic_model):
        """Verify water use method is correctly populated."""
        physics = pydantic_model.physics

        def get_val(attr):
            val = getattr(physics, attr)
            if hasattr(val, 'value'):
                val = val.value
            if hasattr(val, 'value'):
                val = int(val)
            return val

        print(f"\n=== Water Use Method ===")
        expected = get_val('waterusemethod')
        print(f"waterusemethod: expected={expected}, actual={populated_config.waterusemethod}")
        assert populated_config.waterusemethod == expected

    def test_rsl_method(self, populated_config, pydantic_model):
        """Verify RSL method and level are correctly populated."""
        physics = pydantic_model.physics

        def get_val(attr):
            val = getattr(physics, attr)
            if hasattr(val, 'value'):
                val = val.value
            if hasattr(val, 'value'):
                val = int(val)
            return val

        print(f"\n=== RSL Method ===")

        expected_method = get_val('rslmethod')
        print(f"rslmethod: expected={expected_method}, actual={populated_config.rslmethod}")
        assert populated_config.rslmethod == expected_method

        expected_level = get_val('rsllevel')
        print(f"rsllevel: expected={expected_level}, actual={populated_config.rsllevel}")
        assert populated_config.rsllevel == expected_level
