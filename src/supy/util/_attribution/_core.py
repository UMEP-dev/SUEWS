"""
Core Shapley decomposition functions for attribution analysis.

These functions provide exact decomposition of changes in products
of multiple factors, ensuring perfect closure (sum of contributions
equals total change).
"""

import numpy as np


def shapley_triple_product(
    x_A: np.ndarray,
    x_B: np.ndarray,
    y_A: np.ndarray,
    y_B: np.ndarray,
    z_A: np.ndarray,
    z_B: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Exact Shapley decomposition for f = x * y * z.

    Given two states A and B, decompose the change in f = xyz into
    contributions from each factor such that:
        Phi_x + Phi_y + Phi_z = f_B - f_A (exactly)

    Parameters
    ----------
    x_A, x_B : array-like
        Factor x in states A and B
    y_A, y_B : array-like
        Factor y in states A and B
    z_A, z_B : array-like
        Factor z in states A and B

    Returns
    -------
    Phi_x, Phi_y, Phi_z : array-like
        Shapley value contributions from each factor

    Notes
    -----
    The Shapley value assigns each factor's contribution based on its average
    marginal contribution across all coalition orderings. For f = xyz with
    3! = 6 orderings, each factor appears first/middle/last in 2 orderings each.

    The resulting decomposition formulas:
        Phi_x = (dx/6) * [2*y_A*z_A + 2*y_B*z_B + y_A*z_B + y_B*z_A]
        Phi_y = (dy/6) * [2*x_A*z_A + 2*x_B*z_B + x_A*z_B + x_B*z_A]
        Phi_z = (dz/6) * [2*x_A*y_A + 2*x_B*y_B + x_A*y_B + x_B*y_A]

    This ensures exact closure by construction: Phi_x + Phi_y + Phi_z = f_B - f_A.

    NaN values in any input will propagate to outputs. This is intentional
    for timesteps where physical calculations are undefined (e.g., near-zero flux).

    References
    ----------
    Owen, G. (1972). Multilinear extensions of games. Management Science, 18(5), 64-79.
    """
    dx = x_B - x_A
    dy = y_B - y_A
    dz = z_B - z_A

    Phi_x = (dx / 6) * (2 * y_A * z_A + 2 * y_B * z_B + y_A * z_B + y_B * z_A)
    Phi_y = (dy / 6) * (2 * x_A * z_A + 2 * x_B * z_B + x_A * z_B + x_B * z_A)
    Phi_z = (dz / 6) * (2 * x_A * y_A + 2 * x_B * y_B + x_A * y_B + x_B * y_A)

    return Phi_x, Phi_y, Phi_z


def shapley_binary_product(
    x_A: np.ndarray,
    x_B: np.ndarray,
    y_A: np.ndarray,
    y_B: np.ndarray,
) -> tuple[np.ndarray, np.ndarray]:
    """
    Shapley decomposition for f = x * y (binary product).

    Parameters
    ----------
    x_A, x_B : array-like
        Factor x in states A and B
    y_A, y_B : array-like
        Factor y in states A and B

    Returns
    -------
    Phi_x, Phi_y : array-like
        Shapley value contributions from each factor
    """
    dx = x_B - x_A
    dy = y_B - y_A

    # Shapley for binary: Phi_x = dx * (y_A + y_B) / 2
    Phi_x = dx * (y_A + y_B) / 2
    Phi_y = dy * (x_A + x_B) / 2

    return Phi_x, Phi_y


def shapley_forcing_profile(
    F_A: np.ndarray,
    F_B: np.ndarray,
    R_A: np.ndarray,
    R_B: np.ndarray,
    S_A: np.ndarray,
    S_B: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Shapley decomposition for f = F * (R + S) structure.

    This handles the wind profile equation structure where:
    - F is a forcing term (u*/k for wind)
    - R is the roughness term (logarithmic height/roughness)
    - S is the stability correction term (-psi_m)

    The decomposition ensures exact closure:
        Phi_F + Phi_R + Phi_S = f_B - f_A

    Parameters
    ----------
    F_A, F_B : array-like
        Forcing factor in states A and B
    R_A, R_B : array-like
        Roughness factor in states A and B
    S_A, S_B : array-like
        Stability factor in states A and B

    Returns
    -------
    Phi_F, Phi_R, Phi_S : array-like
        Shapley value contributions from forcing, roughness, and stability

    Notes
    -----
    For f = F * (R + S), the Shapley values are:
        Phi_F = dF * (P_A + P_B) / 2,  where P = R + S
        Phi_R = dR * (F_A + F_B) / 2
        Phi_S = dS * (F_A + F_B) / 2

    Proof of closure:
        Phi_F + Phi_R + Phi_S
        = dF(P_A + P_B)/2 + (dR + dS)(F_A + F_B)/2
        = dF(P_A + P_B)/2 + dP(F_A + F_B)/2
        = [two-factor Shapley for F * P]
        = F_B * P_B - F_A * P_A
        = df
    """
    # Changes in each factor
    dF = F_B - F_A
    dR = R_B - R_A
    dS = S_B - S_A

    # Profile shape: P = R + S
    P_A = R_A + S_A
    P_B = R_B + S_B

    # Shapley contributions
    Phi_F = dF * (P_A + P_B) / 2
    Phi_R = dR * (F_A + F_B) / 2
    Phi_S = dS * (F_A + F_B) / 2

    return Phi_F, Phi_R, Phi_S
