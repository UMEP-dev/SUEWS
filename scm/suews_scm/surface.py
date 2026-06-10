"""Monin-Obukhov surface-layer scheme for idealised (uncoupled) cases.

In coupled mode the surface fluxes come from SUEWS and this module is
bypassed. For idealised cases (e.g. GABLS1) the surface boundary
condition is a prescribed surface temperature, and the kinematic fluxes
are computed here with standard Monin-Obukhov similarity:

- unstable: Paulson (1970) / Dyer (1974) psi functions;
- stable: log-linear, psi = -5 zeta.
"""

import numpy as np

from .constants import GRAV, VONK


def _psi_m_unstable(zeta):
    x = (1.0 - 16.0 * zeta) ** 0.25
    return (
        2.0 * np.log((1.0 + x) / 2.0)
        + np.log((1.0 + x * x) / 2.0)
        - 2.0 * np.arctan(x)
        + np.pi / 2.0
    )


def _psi_h_unstable(zeta):
    x = (1.0 - 16.0 * zeta) ** 0.25
    return 2.0 * np.log((1.0 + x * x) / 2.0)


def psi_m(zeta):
    """Integrated stability function for momentum."""
    zeta = np.clip(zeta, -10.0, 5.0)
    return np.where(zeta < 0.0, _psi_m_unstable(np.minimum(zeta, -1e-12)), -5.0 * zeta)


def psi_h(zeta):
    """Integrated stability function for heat."""
    zeta = np.clip(zeta, -10.0, 5.0)
    return np.where(zeta < 0.0, _psi_h_unstable(np.minimum(zeta, -1e-12)), -5.0 * zeta)


def most_fluxes(wind_speed, thv_air, thv_sfc, z_ref, z0m, z0h=None, n_iter=12):
    """Surface fluxes from Monin-Obukhov similarity theory.

    Parameters
    ----------
    wind_speed : float
        Wind speed at the reference height [m s-1].
    thv_air : float
        Virtual potential temperature at the reference height [K].
    thv_sfc : float
        Virtual potential temperature of the surface [K].
    z_ref : float
        Reference height [m] (first model level).
    z0m : float
        Aerodynamic roughness length [m].
    z0h : float, optional
        Thermal roughness length [m]; defaults to ``z0m``.
    n_iter : int
        Fixed-point iterations for the Obukhov length.

    Returns
    -------
    dict
        ``ustar`` [m s-1], ``wthv`` kinematic virtual heat flux
        [K m s-1], ``obukhov`` length [m].
    """
    if z0h is None:
        z0h = z0m
    u = max(float(wind_speed), 0.1)
    dthv = float(thv_air) - float(thv_sfc)
    ln_m = np.log(z_ref / z0m)
    ln_h = np.log(z_ref / z0h)

    # neutral first guess
    ustar = VONK * u / ln_m
    tstar = VONK * dthv / ln_h

    obukhov = 1.0e6
    for _ in range(n_iter):
        if abs(tstar) < 1.0e-9:
            obukhov = 1.0e6
        else:
            obukhov = ustar**2 * thv_air / (VONK * GRAV * tstar)
            # keep |L| away from zero for numerical safety
            obukhov = np.sign(obukhov) * max(abs(obukhov), 1.0)
        zeta = z_ref / obukhov
        ustar = VONK * u / (ln_m - psi_m(zeta) + psi_m(z0m / obukhov))
        ustar = max(float(ustar), 0.01)
        tstar = VONK * dthv / (ln_h - psi_h(zeta) + psi_h(z0h / obukhov))

    return dict(ustar=float(ustar), wthv=-float(ustar) * float(tstar), obukhov=float(obukhov))
