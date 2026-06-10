"""Thermodynamic helper functions.

All inputs in SI units (K, Pa, kg kg-1) unless stated otherwise;
relative humidity in per cent to match the SUEWS forcing convention.
"""

import numpy as np

from .constants import EPS, GRAV, KAPPA, P0, R_D


def exner(p):
    """Exner function (p / p0) ** kappa."""
    return (np.asarray(p, dtype=float) / P0) ** KAPPA


def theta_from_t(t, p):
    """Potential temperature [K] from temperature [K] and pressure [Pa]."""
    return np.asarray(t, dtype=float) / exner(p)


def t_from_theta(theta, p):
    """Temperature [K] from potential temperature [K] and pressure [Pa]."""
    return np.asarray(theta, dtype=float) * exner(p)


def esat_pa(t):
    """Saturation vapour pressure over water [Pa].

    Magnus formula with the Alduchov & Eskridge (1996) coefficients,
    accurate to ~0.4 % between -40 and +50 degC.
    """
    tc = np.asarray(t, dtype=float) - 273.15
    return 610.94 * np.exp(17.625 * tc / (tc + 243.04))


def qsat(t, p):
    """Saturation specific humidity [kg kg-1] at temperature t [K], pressure p [Pa]."""
    es = esat_pa(t)
    return EPS * es / (p - (1.0 - EPS) * es)


def q_from_rh(rh, t, p):
    """Specific humidity [kg kg-1] from relative humidity [%]."""
    e = np.asarray(rh, dtype=float) / 100.0 * esat_pa(t)
    return EPS * e / (p - (1.0 - EPS) * e)


def rh_from_q(q, t, p):
    """Relative humidity [%] from specific humidity [kg kg-1]."""
    q = np.asarray(q, dtype=float)
    e = q * p / (EPS + (1.0 - EPS) * q)
    return 100.0 * e / esat_pa(t)


def virtual_temperature(t, q):
    """Virtual (or virtual potential) temperature [K]."""
    return np.asarray(t, dtype=float) * (1.0 + 0.61 * np.asarray(q, dtype=float))


def rho_air(t, p, q):
    """Moist air density [kg m-3] via the virtual temperature."""
    return np.asarray(p, dtype=float) / (R_D * virtual_temperature(t, q))


def pressure_profile(p_sfc, t_ref, z):
    """Hydrostatic pressure [Pa] at heights z [m] above the surface.

    Uses a constant scale height H = R_d * t_ref / g, adequate for the
    lowest few kilometres resolved by the column. ``t_ref`` should be a
    representative (e.g. layer-mean) virtual temperature [K].
    """
    h_scale = R_D * float(t_ref) / GRAV
    return float(p_sfc) * np.exp(-np.asarray(z, dtype=float) / h_scale)
