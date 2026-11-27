"""Shared physical, mathematical and unit-conversion constants for SuPy.

Values are taken from the same authoritative sources as the Fortran kernel
(``src/suews/src/suews_ctrl_const.f95``, modules ``module_ctrl_const_mathconst``
and ``module_ctrl_const_physconst``) so both layers agree to double precision.
Cross-layer agreement is checked by ``test/core/test_supy_constants.py``.
"""

from __future__ import annotations

import math
from typing import Final

__all__ = [
    "ARCSEC_PER_DEG",
    "C2K",
    "CP_AIR",
    "CP_AIR_ANOHM",
    "DEG2RAD",
    "EPS_FP",
    "HPA_TO_PA",
    "J_TO_UMOL_PAR",
    "KDN_TO_PAR",
    "LV_ANOHM",
    "LV_BOLTON",
    "LV_WETBULB",
    "MIN_PER_DAY",
    "PI",
    "P_REF_PA",
    "RAD2DEG",
    "SEC_PER_DAY",
    "SEC_PER_HOUR",
    "STEFAN_BOLTZMANN",
]

# ---------------------------------------------------------------------------
# Mathematical and angle constants (Fortran: module_ctrl_const_mathconst)
# ---------------------------------------------------------------------------

#: Circle circumference to diameter ratio [-] (Fortran: ``pi``).
PI: Final[float] = math.pi

#: Degrees-to-radians conversion factor [rad degree-1] (Fortran: ``DEG2RAD``).
DEG2RAD: Final[float] = PI / 180.0

#: Radians-to-degrees conversion factor [degree rad-1] (Fortran: ``RAD2DEG``).
RAD2DEG: Final[float] = 180.0 / PI

# ---------------------------------------------------------------------------
# SI time and angular unit conversions (Fortran: module_ctrl_const_physconst)
# ---------------------------------------------------------------------------

#: Seconds per hour [s h-1] (Fortran: ``sec_per_hour``).
SEC_PER_HOUR: Final[int] = 3600

#: Seconds per day [s d-1] (Fortran: ``sec_per_day``).
SEC_PER_DAY: Final[int] = 86400

#: Minutes per day [min d-1] (Fortran: ``min_per_day``).
MIN_PER_DAY: Final[int] = 1440

#: Arcseconds per degree [arcsec degree-1] (Fortran: ``arcsec_per_deg``).
ARCSEC_PER_DEG: Final[int] = 3600

# ---------------------------------------------------------------------------
# Thermodynamic, radiative, pressure and numerical constants
# (Fortran: module_ctrl_const_physconst)
# ---------------------------------------------------------------------------

#: SI Celsius-to-Kelvin offset [K] (Fortran: ``C2K``).
C2K: Final[float] = 273.15

#: CODATA 2018 Stefan-Boltzmann constant [W m-2 K-4] (Fortran: ``SBConst``).
STEFAN_BOLTZMANN: Final[float] = 5.670374419e-8

#: Conversion factor from PAR in W m-2 to umol m-2 s-1 (Fortran: ``JtoumolPAR``).
J_TO_UMOL_PAR: Final[float] = 4.6

#: Conversion factor from incoming shortwave radiation to PAR [-]
#: (Tsubo and Walker 2005; Bellucco et al. 2017; Fortran: ``KdntoPAR``).
KDN_TO_PAR: Final[float] = 0.46

#: Specific heat capacity of dry air [J kg-1 K-1] (dry-air intercept in
#: meteorological formula; Fortran: ``cp_air``).
CP_AIR: Final[float] = 1005.0

#: AnOHM air heat capacity [J kg-1 K-1]; differs from ``CP_AIR``
#: (Fortran: ``cp_air_anohm``).
CP_AIR_ANOHM: Final[float] = 1006.38

#: Bolton (1980) latent heat of vaporisation intercept [J kg-1]; distinct from
#: AnOHM and wet-bulb formulae (Fortran: ``lv_bolton``).
LV_BOLTON: Final[float] = 2501000.0

#: Wet-bulb latent heat of vaporisation intercept [kJ kg-1]; differs from
#: ``LV_BOLTON`` after unit conversion (Fortran: ``lv_wetbulb``).
LV_WETBULB: Final[float] = 2500.25

#: AnOHM latent heat of vaporisation [J kg-1]; differs from ``LV_BOLTON`` and
#: ``LV_WETBULB`` (Fortran: ``lv_anohm``).
LV_ANOHM: Final[float] = 2264705.0

#: SI hecto prefix: Pa per hPa [Pa hPa-1] (Fortran: ``HPA_TO_PA``).
HPA_TO_PA: Final[float] = 100.0

#: SI standard atmosphere [Pa] (Fortran: ``p_ref_pa_d0``).
P_REF_PA: Final[float] = 101325.0

#: Epsilon for floating-point near-zero comparisons [-] (Fortran: ``eps_fp``).
EPS_FP: Final[float] = 1.0e-12
