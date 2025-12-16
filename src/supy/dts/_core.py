"""Core DTS creation functions and configuration.

Contains DTSConfig dataclass and factory functions for creating
SUEWS DTS objects.
"""

from dataclasses import dataclass

from ..supy_driver import module_ctrl_type as dts


@dataclass
class DTSConfig:
    """Configuration for DTS-based simulation.

    Maps Python/YAML configuration to Fortran DTS objects.
    """

    nlayer: int = 5
    ndepth: int = 5
    nsurf: int = 7  # Standard SUEWS surfaces


def create_suews_config() -> dts.SUEWS_CONFIG:
    """Create a SUEWS_CONFIG DTS object with default values.

    Returns
    -------
    dts.SUEWS_CONFIG
        Initialised config object with default method flags.
    """
    config = dts.SUEWS_CONFIG()
    # Defaults are already set to 0 by Fortran initialisation
    return config


def create_suews_state(nlayer: int = 5, ndepth: int = 5) -> dts.SUEWS_STATE:
    """Create and allocate a SUEWS_STATE DTS object.

    Parameters
    ----------
    nlayer : int
        Number of urban canopy layers
    ndepth : int
        Number of substrate depth levels

    Returns
    -------
    dts.SUEWS_STATE
        Allocated state object ready for simulation
    """
    state = dts.SUEWS_STATE()
    state.allocate(nlayer=nlayer, ndepth=ndepth)
    return state


def create_suews_forcing() -> dts.SUEWS_FORCING:
    """Create a SUEWS_FORCING DTS object.

    Returns
    -------
    dts.SUEWS_FORCING
        Forcing object to be populated with meteorological data
    """
    return dts.SUEWS_FORCING()


def create_suews_timer() -> dts.SUEWS_TIMER:
    """Create a SUEWS_TIMER DTS object.

    Returns
    -------
    dts.SUEWS_TIMER
        Timer object to be populated with temporal info
    """
    return dts.SUEWS_TIMER()
