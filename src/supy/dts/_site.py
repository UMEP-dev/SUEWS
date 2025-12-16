"""DTS site functions.

Functions for creating, populating, and extracting SUEWS_SITE objects.
"""

from typing import Any, Dict

import numpy as np

from ..supy_driver import module_ctrl_type as dts


def create_suews_site(nlayer: int = 5) -> dts.SUEWS_SITE:
    """Create and allocate a SUEWS_SITE DTS object.

    Parameters
    ----------
    nlayer : int
        Number of urban canopy layers

    Returns
    -------
    dts.SUEWS_SITE
        Allocated site object ready for configuration
    """
    site = dts.SUEWS_SITE()
    site.nlayer = nlayer
    site.allocate(nlayer=nlayer)
    return site


def populate_site_from_dict(
    site: dts.SUEWS_SITE, params: Dict[str, Any], nsurf: int = 7
) -> None:
    """Populate SUEWS_SITE from a parameter dictionary.

    This populates the directly accessible site parameters. Nested parameter
    types (spartacus, lumps, ehc, etc.) are not accessible through f90wrap
    and require separate accessor functions.

    Parameters
    ----------
    site : dts.SUEWS_SITE
        Site object to populate
    params : dict
        Dictionary of site parameters. Keys should match SUEWS_SITE attributes:
        - lat, lon, alt: Location coordinates
        - timezone: Time zone offset from UTC
        - gridiv: Grid identifier
        - surfacearea: Grid area [m2]
        - z: Measurement height [m]
        - z0m_in, zdm_in: Roughness parameters
        - pipecapacity, runofftowater: Water infrastructure
        - narp_trans_site: Atmospheric transmissivity
        - co2pointsource: CO2 emission factor
        - flowchange: Water flow change
        - n_buildings, h_std, lambda_c: Building parameters
        - sfr_surf: Surface fractions (7 surfaces)
        - sfr_roof, sfr_wall: Building facet fractions
    nsurf : int
        Number of surfaces (default 7)
    """
    # Scalar parameters mapping
    scalar_attrs = [
        "lat",
        "lon",
        "alt",
        "timezone",
        "gridiv",
        "surfacearea",
        "z",
        "z0m_in",
        "zdm_in",
        "pipecapacity",
        "runofftowater",
        "narp_trans_site",
        "co2pointsource",
        "flowchange",
        "n_buildings",
        "h_std",
        "lambda_c",
    ]

    for attr in scalar_attrs:
        if attr in params:
            value = params[attr]
            # Handle RefValue wrappers from Pydantic config
            if hasattr(value, "value"):
                value = value.value
            setattr(site, attr, float(value))

    # Derived fractions (calculated from sfr_surf if not provided)
    if "sfr_surf" in params:
        sfr = np.array(params["sfr_surf"], dtype=np.float64)
        if len(sfr) == nsurf:
            site.sfr_surf = sfr
            # Calculate derived fractions
            # SUEWS surfaces: 0=Paved, 1=Bldg, 2=EveTr, 3=DecTr, 4=Grass, 5=BSoil, 6=Water
            site.vegfraction = float(np.sum(sfr[2:5]))  # EveTr + DecTr + Grass
            site.impervfraction = float(np.sum(sfr[0:2]))  # Paved + Bldg
            site.pervfraction = float(np.sum(sfr[2:6]))  # EveTr + DecTr + Grass + BSoil
            site.nonwaterfraction = float(np.sum(sfr[0:6]))  # All except Water

    # Override derived fractions if explicitly provided
    for frac_attr in [
        "vegfraction",
        "impervfraction",
        "pervfraction",
        "nonwaterfraction",
    ]:
        if frac_attr in params:
            value = params[frac_attr]
            if hasattr(value, "value"):
                value = value.value
            setattr(site, frac_attr, float(value))

    # Building facet fractions (need nlayer-sized arrays)
    nlayer = site.nlayer
    if "sfr_roof" in params:
        sfr_roof = np.array(params["sfr_roof"], dtype=np.float64)
        if len(sfr_roof) == nlayer:
            site.sfr_roof = sfr_roof
    if "sfr_wall" in params:
        sfr_wall = np.array(params["sfr_wall"], dtype=np.float64)
        if len(sfr_wall) == nlayer:
            site.sfr_wall = sfr_wall


def extract_site_params(site: dts.SUEWS_SITE) -> Dict[str, Any]:
    """Extract site parameters from DTS object to dictionary.

    Parameters
    ----------
    site : dts.SUEWS_SITE
        Site object to extract from

    Returns
    -------
    dict
        Dictionary of site parameters
    """
    result = {}

    # Scalar parameters
    scalar_attrs = [
        "lat",
        "lon",
        "alt",
        "timezone",
        "gridiv",
        "surfacearea",
        "z",
        "z0m_in",
        "zdm_in",
        "pipecapacity",
        "runofftowater",
        "narp_trans_site",
        "co2pointsource",
        "flowchange",
        "n_buildings",
        "h_std",
        "lambda_c",
        "nlayer",
    ]
    for attr in scalar_attrs:
        result[attr] = getattr(site, attr)

    # Derived fractions
    result["vegfraction"] = site.vegfraction
    result["impervfraction"] = site.impervfraction
    result["pervfraction"] = site.pervfraction
    result["nonwaterfraction"] = site.nonwaterfraction

    # Array parameters
    result["sfr_surf"] = np.array(site.sfr_surf).copy()
    result["sfr_roof"] = np.array(site.sfr_roof).copy()
    result["sfr_wall"] = np.array(site.sfr_wall).copy()

    return result
