"""DTS factory functions for creating SUEWS Fortran objects.

This module provides factory functions that create and initialise
SUEWS Derived Type Structures (DTS) for use with the Fortran kernel.
Uses direct f90wrap access - no custom accessor modules needed.
"""

from ..supy_driver import module_ctrl_type as dts


def create_suews_config() -> dts.SUEWS_CONFIG:
    """Create a new SUEWS_CONFIG object with default values.

    Returns
    -------
    dts.SUEWS_CONFIG
        Configuration object for physics method flags.
    """
    return dts.SUEWS_CONFIG()


def create_suews_state(nlayer: int = 5, ndepth: int = 5, nbtypes: int = 1) -> dts.SUEWS_STATE:
    """Create and allocate a new SUEWS_STATE object.

    Parameters
    ----------
    nlayer : int, optional
        Number of vertical layers (roof/wall facets), by default 5.
    ndepth : int, optional
        Number of substrate depth levels, by default 5.
    nbtypes : int, optional
        Number of building types for STEBBS, by default 1.

    Returns
    -------
    dts.SUEWS_STATE
        Allocated state object with all nested arrays ready for use.

    Notes
    -----
    The state object contains 12 nested state types (heatState, hydroState,
    snowState, etc.). After allocation, arrays like `state.heatstate.temp_surf`
    are accessible with shape (nsurf, ndepth) where nsurf=7.

    The stebbsState.buildings array must be allocated even when STEBBS is
    disabled because Fortran MERGE evaluates both arguments. The traditional
    code allocates with nbtypes=1 by default.
    """
    state = dts.SUEWS_STATE()
    state.allocate(nlayer, ndepth)

    # CRITICAL: Allocate stebbsState.buildings even when STEBBS is disabled
    # Fortran MERGE evaluates both arguments, so buildings(1)%Textroof_C
    # is accessed even when storageheatmethod != 7
    state.stebbsstate.allocate(nbtypes, nlayer)

    # Also allocate the inner arrays of each building
    # buildings[0] corresponds to Fortran buildings(1)
    for i in range(nbtypes):
        state.stebbsstate.buildings[i].allocate(nlayer)

    return state


def create_suews_site(nlayer: int = 5, ndepth: int = 5, nsurf: int = 7) -> dts.SUEWS_SITE:
    """Create and allocate a new SUEWS_SITE object.

    Parameters
    ----------
    nlayer : int, optional
        Number of vertical layers for roof/wall facets, by default 5.
    ndepth : int, optional
        Number of substrate depth levels, by default 5.
    nsurf : int, optional
        Number of surface types (always 7 for SUEWS), by default 7.

    Returns
    -------
    dts.SUEWS_SITE
        Allocated site object ready for population.

    Notes
    -----
    This also allocates the EHC (Element Heat Capacity) arrays which are
    needed for heat storage calculations but not allocated by site.allocate().
    The EHC arrays use:
    - (nsurf, ndepth) for surface arrays (dz_surf, k_surf, cp_surf, tin_surf)
    - (nlayer, ndepth) for roof/wall arrays (dz_roof, k_roof, etc.)

    Note: The Fortran EHC allocate uses nlayer for ALL arrays, but surf arrays
    need nsurf. We work around this by allocating with nsurf (which is typically
    larger than nlayer), accepting that roof/wall arrays will be oversized.
    """
    site = dts.SUEWS_SITE()
    site.allocate(nlayer)

    # CRITICAL: Fortran allocate doesn't set nlayer - we must set it explicitly
    site.nlayer = nlayer

    # Allocate nested parameter objects that have allocate methods
    # EHC: Element Heat Capacity arrays for heat storage calculations
    # The Fortran allocate uses same size for ALL arrays, but:
    # - surf arrays need (nsurf, ndepth)
    # - roof/wall arrays need (nlayer, ndepth)
    # We allocate with nsurf (larger) to ensure surf arrays are correct size.
    # Roof/wall arrays will be (nsurf, ndepth) but we only use (nlayer, ndepth) portion.
    site.ehc.allocate(nsurf, ndepth)

    # SPARTACUS layer parameters
    site.spartacus_layer.allocate(nlayer)

    # SPARTACUS parameters - allocate height array (nlayer+1 levels)
    site.spartacus.allocate(nlayer)

    return site


def create_suews_forcing() -> dts.SUEWS_FORCING:
    """Create a new SUEWS_FORCING object.

    Returns
    -------
    dts.SUEWS_FORCING
        Forcing object for meteorological input data.
    """
    return dts.SUEWS_FORCING()


def create_suews_timer() -> dts.SUEWS_TIMER:
    """Create a new SUEWS_TIMER object.

    Returns
    -------
    dts.SUEWS_TIMER
        Timer object for temporal information.
    """
    return dts.SUEWS_TIMER()


def create_output_line() -> dts.output_line:
    """Create a new output_line object.

    Returns
    -------
    dts.output_line
        Output line object for single timestep results.
    """
    line = dts.output_line()
    dts.output_line_init(line)
    return line
