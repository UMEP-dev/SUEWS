"""Water distribution utility functions.

Python ports of physics routines from suews_phys_waterdist.f95.
"""

import numpy as np

# SUEWS surface type indices (0-based, matching Fortran ConifSurf=3..GrassSurf=5 minus 1)
_CONIF_SURF = 2
_GRASS_SURF = 5  # exclusive upper bound for Python slicing


def cal_smd_veg(soilstorecap, soilstore_id, sfr_surf):
    """Calculate area-weighted soil moisture deficit for vegetated surfaces.

    Mirrors the Fortran function ``cal_smd_veg`` in ``suews_phys_waterdist.f95``.

    Parameters
    ----------
    soilstorecap : array_like, shape (7,)
        Soil moisture storage capacity for each surface type [mm].
    soilstore_id : array_like, shape (7,)
        Current soil moisture store for each surface type [mm].
    sfr_surf : array_like, shape (7,)
        Surface area fractions [-].

    Returns
    -------
    float
        Weighted soil moisture deficit across the three vegetated surfaces
        (coniferous, deciduous, grass).
    """
    smd_surf = np.asarray(soilstorecap, dtype=np.float64) - np.asarray(
        soilstore_id, dtype=np.float64
    )
    smd_veg = smd_surf[_CONIF_SURF:_GRASS_SURF]
    surf_veg = np.asarray(sfr_surf, dtype=np.float64)[_CONIF_SURF:_GRASS_SURF].copy()
    surf_veg /= surf_veg.sum()
    return float(np.dot(smd_veg, surf_veg))
