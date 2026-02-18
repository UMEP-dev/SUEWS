"""
Physical calculations for attribution analysis.

Includes gamma calculations, effective resistance, profile components,
and flux budget decomposition.
"""

import logging

import numpy as np

from .._atm import cal_cp_with_rh, cal_dens_air, cal_lat_vap, cal_psi_mom, cal_qa

# Module logger
_logger = logging.getLogger(__name__)


# =============================================================================
# Gamma (scale factor) calculations
# =============================================================================


def cal_gamma_heat(
    temp_C: np.ndarray,
    rh_pct: np.ndarray,
    press_hPa: np.ndarray,
) -> np.ndarray:
    """
    Calculate scale factor gamma = 1 / (rho * c_p) for heat flux.

    Parameters
    ----------
    temp_C : array-like
        Air temperature (degC)
    rh_pct : array-like
        Relative humidity (%)
    press_hPa : array-like
        Air pressure (hPa)

    Returns
    -------
    gamma : array-like
        Scale factor (K*m^3/J) - combines with r[s/m]*Q_H[W/m^2] to give
        temperature change [K]. Equivalent to 1/(rho*cp).
    """
    rho = cal_dens_air(press_hPa, temp_C)
    cp = cal_cp_with_rh(temp_C, rh_pct, press_hPa)
    with np.errstate(divide="ignore", invalid="ignore"):
        gamma = 1.0 / (rho * cp)
    return gamma


def cal_gamma_humidity(
    temp_C: np.ndarray,
    rh_pct: np.ndarray,
    press_hPa: np.ndarray,
) -> np.ndarray:
    """
    Calculate scale factor gamma = 1 / (rho * L_v) for latent heat flux.

    Parameters
    ----------
    temp_C : array-like
        Air temperature (degC)
    rh_pct : array-like
        Relative humidity (%)
    press_hPa : array-like
        Air pressure (hPa)

    Returns
    -------
    gamma : array-like
        Scale factor (m^3/J) - combines with r[s/m]*Q_E[W/m^2] to give
        humidity change [kg/kg]. Equivalent to 1/(rho*Lv).
    """
    # Calculate air density
    rho = cal_dens_air(press_hPa, temp_C)

    # Calculate specific humidity from RH for latent heat calculation
    theta_K = temp_C + 273.15  # Celsius to Kelvin
    qa = cal_qa(rh_pct, theta_K, press_hPa)

    # Calculate latent heat of vaporisation
    Lv = cal_lat_vap(qa, theta_K, press_hPa)

    # Scale factor: delta_q = gamma * QE (where QE in W/m2)
    with np.errstate(divide="ignore", invalid="ignore"):
        gamma = 1.0 / (rho * Lv)
    return gamma


# =============================================================================
# Effective resistance calculations
# =============================================================================


def _cal_r_eff(
    var: np.ndarray,
    var_ref: np.ndarray,
    flux: np.ndarray,
    gamma: np.ndarray,
    min_flux: float,
    flux_name: str,
) -> np.ndarray:
    """Shared effective resistance back-calculation for heat and humidity."""
    # Avoid division by near-zero flux
    denom = flux * gamma
    low_flux_mask = np.abs(denom) < min_flux * np.abs(gamma)
    n_low_flux = int(np.sum(low_flux_mask))

    if n_low_flux > 0:
        _logger.debug(
            "Setting %d timesteps to NaN due to %s below threshold (%.1f W/m2)",
            n_low_flux,
            flux_name,
            min_flux,
        )

    denom = np.where(low_flux_mask, np.nan, denom)

    with np.errstate(divide="ignore", invalid="ignore"):
        r_eff = (var - var_ref) / denom
    return r_eff


def cal_r_eff_heat(
    T2: np.ndarray,
    T_ref: np.ndarray,
    QH: np.ndarray,
    gamma: np.ndarray,
    min_flux: float = 0.1,
) -> np.ndarray:
    """
    Back-calculate effective heat resistance from T2 profile.

    From T2 = T_ref + r * QH * gamma, we get:
        r = (T2 - T_ref) / (QH * gamma)

    Parameters
    ----------
    T2 : array-like
        2m air temperature (degC)
    T_ref : array-like
        Reference temperature at forcing height (degC)
    QH : array-like
        Sensible heat flux (W/m2)
    gamma : array-like
        Scale factor 1/(rho*cp)
    min_flux : float, optional
        Minimum flux threshold to avoid division issues, by default 0.1 W/m2

    Returns
    -------
    r_eff : array-like
        Effective heat resistance (s/m). NaN for timesteps with |Q_H| < min_flux.
    """
    return _cal_r_eff(T2, T_ref, QH, gamma, min_flux=min_flux, flux_name="flux")


def cal_r_eff_humidity(
    q2: np.ndarray,
    q_ref: np.ndarray,
    QE: np.ndarray,
    gamma: np.ndarray,
    min_flux: float = 0.1,
) -> np.ndarray:
    """
    Back-calculate effective moisture resistance from q2 profile.

    From q2 = q_ref + r * QE * gamma, we get:
        r = (q2 - q_ref) / (QE * gamma)

    Parameters
    ----------
    q2 : array-like
        2m specific humidity (kg/kg)
    q_ref : array-like
        Reference specific humidity at forcing height (kg/kg)
    QE : array-like
        Latent heat flux (W/m2)
    gamma : array-like
        Scale factor 1/(rho*Lv)
    min_flux : float, optional
        Minimum flux threshold to avoid division issues, by default 0.1 W/m2

    Returns
    -------
    r_eff : array-like
        Effective moisture resistance (s/m). NaN for timesteps with |Q_E| < min_flux.
    """
    return _cal_r_eff(
        q2, q_ref, QE, gamma, min_flux=min_flux, flux_name="latent flux"
    )


# =============================================================================
# Flux budget decomposition
# =============================================================================


def decompose_flux_budget(
    flux_A: np.ndarray,
    flux_B: np.ndarray,
    components_A: dict[str, np.ndarray],
    components_B: dict[str, np.ndarray],
    total_contribution: np.ndarray,
) -> dict[str, np.ndarray]:
    """
    Hierarchical decomposition of flux contribution into budget components.

    For sensible heat flux: Q_H = Q* - Q_E - dQ_S + Q_F

    The total flux contribution (Phi_flux) is allocated to each budget
    component proportionally to their contribution to the flux change.

    Parameters
    ----------
    flux_A, flux_B : array-like
        Total flux in states A and B
    components_A, components_B : dict
        Dictionary of flux budget components in each state
    total_contribution : array-like
        Total Shapley contribution from flux

    Returns
    -------
    dict
        Contributions from each budget component. NaN for timesteps where
        the total flux change is near-zero (< 1e-10 W/m2).

    Notes
    -----
    The threshold of 1e-10 W/m2 is chosen to be well below machine precision
    for typical flux magnitudes (1-1000 W/m2), ensuring numerical stability
    while capturing essentially all physically meaningful flux changes.
    """
    # Calculate change in each component
    d_components = {
        name: components_B[name] - components_A[name] for name in components_A
    }

    # Total flux change
    d_flux = flux_B - flux_A

    # Allocate total contribution proportionally
    # Threshold: 1e-10 W/m2 is well below machine precision for typical flux values
    # This prevents division-by-zero while preserving all meaningful decompositions
    FLUX_CHANGE_THRESHOLD = 1e-10  # W/m2
    d_flux_safe = np.where(np.abs(d_flux) < FLUX_CHANGE_THRESHOLD, np.nan, d_flux)

    with np.errstate(divide="ignore", invalid="ignore"):
        contributions = {}
        for name, d_comp in d_components.items():
            weight = d_comp / d_flux_safe
            contributions[name] = weight * total_contribution

    return contributions


# =============================================================================
# U10 profile calculations
# =============================================================================


def cal_u10_profile_components(
    ustar: np.ndarray,
    z0m: np.ndarray,
    zd: np.ndarray,
    Lob: np.ndarray,
    z_ref: float = 10.0,
    k: float = 0.4,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Calculate wind profile components for Shapley decomposition.

    The logarithmic wind profile is:
        U(z) = (u*/k) * [ln((z-d)/z0m) - psi_m(zeta)]

    This decomposes into:
        F = u*/k  (forcing term - surface stress)
        R = ln((z-d)/z0m)  (roughness term - geometric)
        S = -psi_m(zeta)  (stability correction)

    Parameters
    ----------
    ustar : array-like
        Friction velocity (m/s)
    z0m : array-like
        Roughness length for momentum (m)
    zd : array-like
        Displacement height (m)
    Lob : array-like
        Obukhov length (m). Positive = stable, negative = unstable.
    z_ref : float, optional
        Reference height for wind speed (m). Default 10.0 m.
    k : float, optional
        von Karman constant. Default 0.4.

    Returns
    -------
    F, R, S : tuple of array-like
        Forcing, roughness, and stability profile components
    """
    # Forcing term: F = u*/k
    F = ustar / k

    # Effective height above displacement
    z_eff = z_ref - zd

    # Roughness term: R = ln((z-d)/z0m)
    # Handle edge cases where z_eff <= 0 or z0m <= 0
    with np.errstate(divide="ignore", invalid="ignore"):
        R = np.log(z_eff / z0m)
        # Set to NaN where calculation is invalid
        R = np.where((z_eff <= 0) | (z0m <= 0), np.nan, R)

    # Stability parameter: zeta = (z-d)/L
    with np.errstate(divide="ignore", invalid="ignore"):
        zeta = z_eff / Lob
        # Limit extreme values
        zeta = np.clip(zeta, -5, 5)

    # Stability correction: S = -psi_m(zeta)
    # Using the stability function from _atm module
    psi_m = cal_psi_mom(zeta)
    S = -psi_m

    return F, R, S


def cal_u10_from_components(
    F: np.ndarray,
    R: np.ndarray,
    S: np.ndarray,
) -> np.ndarray:
    """
    Calculate wind speed from profile components.

    U10 = F * (R + S) = (u*/k) * [ln((z-d)/z0m) - psi_m]

    Parameters
    ----------
    F : array-like
        Forcing term (u*/k)
    R : array-like
        Roughness term (ln((z-d)/z0m))
    S : array-like
        Stability term (-psi_m)

    Returns
    -------
    U10 : array-like
        Wind speed at reference height (m/s)
    """
    return F * (R + S)


def back_calculate_z0m(
    U10: np.ndarray,
    ustar: np.ndarray,
    zd: np.ndarray,
    Lob: np.ndarray,
    z_ref: float = 10.0,
    k: float = 0.4,
    min_ustar: float = 0.01,
) -> np.ndarray:
    """
    Back-calculate roughness length from U10, u*, displacement and stability.

    From U10 = (u*/k) * [ln((z-d)/z0m) - psi_m], we get:
        ln((z-d)/z0m) = k * U10 / u* + psi_m
        z0m = (z-d) / exp(k * U10 / u* + psi_m)

    Parameters
    ----------
    U10 : array-like
        Wind speed at reference height (m/s)
    ustar : array-like
        Friction velocity (m/s)
    zd : array-like
        Displacement height (m)
    Lob : array-like
        Obukhov length (m)
    z_ref : float, optional
        Reference height (m). Default 10.0.
    k : float, optional
        von Karman constant. Default 0.4.
    min_ustar : float, optional
        Minimum friction velocity threshold. Default 0.01 m/s.

    Returns
    -------
    z0m : array-like
        Roughness length for momentum (m). NaN for low u*.
    """
    # Effective height
    z_eff = z_ref - zd

    # Stability parameter and correction
    with np.errstate(divide="ignore", invalid="ignore"):
        zeta = z_eff / Lob
        zeta = np.clip(zeta, -5, 5)
    psi_m = cal_psi_mom(zeta)

    # Back-calculate z0m
    # Avoid division by near-zero u*
    low_ustar_mask = np.abs(ustar) < min_ustar
    ustar_safe = np.where(low_ustar_mask, np.nan, ustar)

    with np.errstate(divide="ignore", invalid="ignore"):
        log_term = k * U10 / ustar_safe + psi_m
        z0m = z_eff / np.exp(log_term)

    return z0m
