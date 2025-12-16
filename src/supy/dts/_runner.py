"""DTS simulation runner functions.

Contains functions for running SUEWS simulations using DTS objects
and a test function for validating the interface.
"""

from typing import Any, Dict, Tuple

from ..supy_driver import suews_driver as sd
from ..supy_driver import module_ctrl_type as dts
from .. import _state_accessors as acc
from ._core import create_suews_config, create_suews_state, create_suews_forcing
from ._extract import extract_heat_state


def run_supy_dts_tstep(
    timer: dts.SUEWS_TIMER,
    config: dts.SUEWS_CONFIG,
    site: dts.SUEWS_SITE,
    state: dts.SUEWS_STATE,
    forcing: dts.SUEWS_FORCING,
    debug: bool = False,
) -> Tuple[dts.SUEWS_STATE, Dict[str, Any]]:
    """Run a single SUEWS timestep using DTS objects.

    Parameters
    ----------
    timer : dts.SUEWS_TIMER
        Timer with temporal information for this timestep
    config : dts.SUEWS_CONFIG
        Model configuration
    site : dts.SUEWS_SITE
        Site parameters
    state : dts.SUEWS_STATE
        Model state (modified in place)
    forcing : dts.SUEWS_FORCING
        Meteorological forcing
    debug : bool
        Whether to return debug information. When True, creates and
        initialises a SUEWS_DEBUG object for detailed diagnostics.

    Returns
    -------
    tuple
        (updated_state, output_dict) where output_dict contains
        simulation results for this timestep
    """
    # Create and initialise debug state if requested
    # Note: f90wrap handles None for optional Fortran arguments, but we
    # create an uninitialised debug object as a fallback for robustness
    if debug:
        debug_state = dts.SUEWS_DEBUG()
        dts.init_suews_debug(debug_state)
    else:
        # Create minimal debug state - f90wrap may not handle None correctly
        # for all Fortran compilers, so provide an uninitialised object
        debug_state = dts.SUEWS_DEBUG()

    # Call the main calculation
    # Note: timer is the first positional argument (appears as 'self' in signature)
    output_line = sd.suews_cal_main(timer, forcing, config, site, state, debug_state)

    # Extract output values
    output_dict = {}

    # Extract main SUEWS outputs from dataoutlinesuews array
    if output_line is not None:
        suews_data = output_line.dataoutlinesuews
        if suews_data is not None and len(suews_data) > 0:
            output_dict["dataoutlinesuews"] = suews_data.copy()

        # Extract datetime components
        dt_line = output_line.datetimeline
        if dt_line is not None and len(dt_line) > 0:
            output_dict["datetimeline"] = dt_line.copy()

    # Also extract key state values
    output_dict.update(extract_heat_state(state))

    return state, output_dict


def test_dts_interface() -> bool:
    """Test the DTS interface by running basic operations.

    Returns True if all tests pass.
    """
    try:
        # Create DTS objects
        config = create_suews_config()
        state = create_suews_state(nlayer=5, ndepth=5)
        forcing = create_suews_forcing()

        # Test dimension getters
        nlayer, ndepth, nsurf = acc.get_heat_state_dims(state)
        assert nlayer == 5, f"Expected nlayer=5, got {nlayer}"
        assert ndepth == 5, f"Expected ndepth=5, got {ndepth}"
        assert nsurf == 7, f"Expected nsurf=7, got {nsurf}"

        # Test scalar getters
        qh, qe, qs, qn, qf, tsurf = acc.get_heat_state_scalars(state)
        assert qh == 0.0, f"Expected qh=0.0, got {qh}"

        # Test scalar setters
        acc.set_heat_state_scalars(state, 100.0, 50.0, 30.0, 200.0, 10.0, 25.0)
        qh, qe, qs, qn, qf, tsurf = acc.get_heat_state_scalars(state)
        assert qh == 100.0, f"Expected qh=100.0, got {qh}"
        assert tsurf == 25.0, f"Expected tsurf=25.0, got {tsurf}"

        print("All DTS interface tests passed!")
        return True

    except Exception as e:
        print(f"DTS interface test failed: {e}")
        return False


if __name__ == "__main__":
    test_dts_interface()
