"""NumPy reference implementation of FvCB + Medlyn A-gs equations.

Mirrors the equations in ``src/suews/src/suews_phys_farquhar.f95`` (and, once it
lands, ``suews_phys_medlyn.f95``). Exists for two purposes:

1. Immediate Tier A textbook unit tests that do not depend on the Rust/Fortran
   bridge being built or exposing a Python-callable photosynthesis entry point.
2. Once the Fortran path is exposed to Python (via the ags_solver wrapper in
   Week 4), this module becomes an independent cross-check oracle: two
   implementations agreeing is strong evidence both are correct.

Parameters match the Fortran module constants verbatim. Units follow the
Fortran conventions: CO2 mole fractions in ppm (= umol mol^-1), PAR in
umol m^-2 s^-1, temperatures in degC, enzyme kinetics as J mol^-1.

References
----------
Farquhar, G. D., von Caemmerer, S., & Berry, J. A. (1980). *Planta*, 149.
Medlyn, B. E. et al. (2002). *Plant, Cell and Environment*, 25(9).
Bernacchi, C. J. et al. (2001). *Plant, Cell and Environment*, 24(2).
Collatz, G. J., Ribas-Carbo, M., & Berry, J. A. (1992). *Aust J Plant Phys*.
Sharkey, T. D. et al. (2007). *Plant, Cell and Environment*, 30(9).

.. note::
    This module is a private implementation detail (underscore prefix). It is
    not part of the public ``supy.util`` API and its signatures may change.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

# Physical constants — keep in sync with `module_phys_farquhar`.
R_GAS = 8.314  # J mol^-1 K^-1
T_REF_K = 298.15  # K
O_AIR = 210_000.0  # ppm (approx. 21 kPa O2 at surface)

# C3 peaked-Arrhenius parameters (Bernacchi 2001 + Medlyn 2002).
EA_VCMAX = 65_330.0
EA_JMAX = 43_540.0
EA_KC = 79_430.0
EA_KO = 36_380.0
EA_GAMMA_STAR = 37_830.0
EA_RD = 46_390.0
HD_VCMAX = 149_250.0
HD_JMAX = 152_044.0
DS_VCMAX = 485.0
DS_JMAX = 495.0
KC_25 = 404.9
KO_25 = 278_400.0
GAMMA_STAR_25 = 42.75
ALPHA_PSII = 0.3
THETA_J = 0.7

# C4 Collatz 1992 parameters.
KP_25 = 0.7
EA_KP = 46_390.0
ALPHA_C4 = 0.067


def arrhenius_response(k25: float, ea: float, t_leaf_c: float) -> float:
    """Compute the simple Arrhenius temperature response.

    Used for K_c, K_o, Gamma*, R_d.

    Parameters
    ----------
    k25 : float
        Parameter value at 25 C.
    ea : float
        Activation energy [J mol^-1].
    t_leaf_c : float
        Leaf temperature [degC].

    Returns
    -------
    float
        Parameter value at ``t_leaf_c``.
    """
    t_k = t_leaf_c + 273.15
    return k25 * np.exp(ea * (t_k - T_REF_K) / (T_REF_K * R_GAS * t_k))


def peaked_arrhenius_response(
    k25: float,
    ea: float,
    hd: float,
    ds: float,
    t_leaf_c: float,
) -> float:
    """Peaked Arrhenius with high-T deactivation (Medlyn 2002).

    Used for V_cmax and J_max. Peak occurs above the growth-temperature window;
    above the peak, enzyme denaturation causes a rapid fall-off that drives
    the heat-stress stomatal-closure signal the A-gs work is meant to capture.

    Parameters
    ----------
    k25 : float
        Parameter value at 25 C.
    ea : float
        Activation energy [J mol^-1].
    hd : float
        Deactivation enthalpy [J mol^-1].
    ds : float
        Entropy term [J mol^-1 K^-1].
    t_leaf_c : float
        Leaf temperature [degC].

    Returns
    -------
    float
        Parameter value at ``t_leaf_c``.
    """
    t_k = t_leaf_c + 273.15
    num = 1.0 + np.exp((T_REF_K * ds - hd) / (T_REF_K * R_GAS))
    den = 1.0 + np.exp((t_k * ds - hd) / (t_k * R_GAS))
    return arrhenius_response(k25, ea, t_leaf_c) * num / den


@dataclass(frozen=True)
class FvcbC3Result:
    """Return container for :func:`farquhar_c3`."""

    a_c: float  # Rubisco-limited gross photosynthesis [umol m^-2 s^-1]
    a_j: float  # RuBP-regeneration-limited gross photosynthesis
    a_n: float  # Net (min(a_c, a_j) - R_d)


def farquhar_c3(
    par_umol: float,
    c_i_ppm: float,
    t_leaf_c: float,
    vcmax25: float,
    jmax25: float,
    rd25: float,
) -> FvcbC3Result:
    """C3 leaf-level net photosynthesis.

    Parameters
    ----------
    par_umol : float
        Absorbed photosynthetically active radiation [umol photon m^-2 s^-1].
    c_i_ppm : float
        Intercellular CO2 mole fraction [umol mol^-1, == ppm].
    t_leaf_c : float
        Leaf temperature [degC].
    vcmax25 : float
        Maximum Rubisco carboxylation rate at 25 C [umol m^-2 s^-1].
    jmax25 : float
        Maximum electron transport rate at 25 C [umol m^-2 s^-1].
    rd25 : float
        Dark respiration rate at 25 C [umol m^-2 s^-1].

    Returns
    -------
    FvcbC3Result
        Rubisco-limited, RuBP-limited, and net photosynthesis rates.

    Raises
    ------
    ValueError
        If Vcmax25 or Jmax25 is non-positive, or Rd25 is negative.
    """
    if vcmax25 <= 0.0 or jmax25 <= 0.0 or rd25 < 0.0:
        msg = "farquhar_c3: Vcmax25, Jmax25 > 0 and Rd25 >= 0 required"
        raise ValueError(msg)

    vcmax_t = peaked_arrhenius_response(vcmax25, EA_VCMAX, HD_VCMAX, DS_VCMAX, t_leaf_c)
    jmax_t = peaked_arrhenius_response(jmax25, EA_JMAX, HD_JMAX, DS_JMAX, t_leaf_c)
    rd_t = arrhenius_response(rd25, EA_RD, t_leaf_c)
    kc_t = arrhenius_response(KC_25, EA_KC, t_leaf_c)
    ko_t = arrhenius_response(KO_25, EA_KO, t_leaf_c)
    gamma_star_t = arrhenius_response(GAMMA_STAR_25, EA_GAMMA_STAR, t_leaf_c)

    denom_c = c_i_ppm + kc_t * (1.0 + O_AIR / ko_t)
    a_c = 0.0 if denom_c <= 0.0 else vcmax_t * (c_i_ppm - gamma_star_t) / denom_c
    a_c = max(a_c, 0.0)

    i_abs = ALPHA_PSII * max(par_umol, 0.0)
    disc = (i_abs + jmax_t) ** 2 - 4.0 * THETA_J * i_abs * jmax_t
    disc = max(disc, 0.0)
    j_t = (i_abs + jmax_t - np.sqrt(disc)) / (2.0 * THETA_J)

    denom_j = 4.0 * (c_i_ppm + 2.0 * gamma_star_t)
    a_j = 0.0 if denom_j <= 0.0 else j_t * (c_i_ppm - gamma_star_t) / denom_j
    a_j = max(a_j, 0.0)

    a_n = min(a_c, a_j) - rd_t
    return FvcbC3Result(a_c=float(a_c), a_j=float(a_j), a_n=float(a_n))


@dataclass(frozen=True)
class FvcbC4Result:
    """Return container for :func:`farquhar_c4`."""

    a_c: float  # Rubisco-limited (saturated via CCM)
    a_i: float  # Light-limited (quantum)
    a_p: float  # PEP-carboxylase / low-CO2 limited
    a_n: float  # Net (min(a_c, a_i, a_p) - R_d)


def farquhar_c4(
    par_umol: float,
    c_i_ppm: float,
    t_leaf_c: float,
    vcmax25: float,
    rd25: float,
) -> FvcbC4Result:
    """C4 leaf-level net photosynthesis (Collatz 1992, simplified).

    The CO2-concentrating mechanism makes C4 essentially CO2-saturated at the
    Rubisco site, so the Rubisco limit is simply ``V_cmax(T)``. The quantum
    and PEP-carboxylase limits capture low-light and low-CO2 behaviour.

    Parameters
    ----------
    par_umol : float
        Absorbed photosynthetically active radiation [umol photon m^-2 s^-1].
    c_i_ppm : float
        Intercellular CO2 mole fraction [ppm].
    t_leaf_c : float
        Leaf temperature [degC].
    vcmax25 : float
        Maximum Rubisco carboxylation rate at 25 C [umol m^-2 s^-1].
    rd25 : float
        Dark respiration rate at 25 C [umol m^-2 s^-1].

    Returns
    -------
    FvcbC4Result
        Rubisco-, light-, and PEP-limited rates plus net photosynthesis.

    Raises
    ------
    ValueError
        If Vcmax25 is non-positive or Rd25 is negative.
    """
    if vcmax25 <= 0.0 or rd25 < 0.0:
        msg = "farquhar_c4: Vcmax25 > 0 and Rd25 >= 0 required"
        raise ValueError(msg)

    vcmax_t = peaked_arrhenius_response(vcmax25, EA_VCMAX, HD_VCMAX, DS_VCMAX, t_leaf_c)
    rd_t = arrhenius_response(rd25, EA_RD, t_leaf_c)
    kp_t = arrhenius_response(KP_25, EA_KP, t_leaf_c)

    a_c = vcmax_t
    a_i = ALPHA_C4 * max(par_umol, 0.0)
    a_p = kp_t * max(c_i_ppm, 0.0)

    a_n = min(a_c, a_i, a_p) - rd_t
    return FvcbC4Result(
        a_c=float(a_c), a_i=float(a_i), a_p=float(a_p), a_n=float(a_n)
    )


# --- Medlyn stomatal conductance -------------------------------------------

H2O_CO2_DIFF_RATIO = 1.6
VPD_MIN_KPA = 0.05


def medlyn_gs(
    a_n_umol: float,
    c_s_ppm: float,
    vpd_kpa: float,
    g_0: float,
    g_1: float,
) -> float:
    """Compute Medlyn optimal stomatal conductance.

    Mirrors ``module_phys_medlyn.medlyn_gs``.

    Parameters
    ----------
    a_n_umol : float
        Net photosynthesis [umol m^-2 s^-1].
    c_s_ppm : float
        CO2 mole fraction at leaf surface [ppm].
    vpd_kpa : float
        Leaf-to-air vapour pressure deficit [kPa].
    g_0 : float
        Residual stomatal conductance [mol H2O m^-2 s^-1].
    g_1 : float
        Medlyn slope parameter [kPa^0.5].

    Returns
    -------
    float
        Stomatal conductance to H2O [mol m^-2 s^-1].

    Raises
    ------
    ValueError
        If g_0 < 0, g_1 < 0, or c_s_ppm <= 0.
    """
    if g_0 < 0.0 or g_1 < 0.0 or c_s_ppm <= 0.0:
        msg = "medlyn_gs: g_0 >= 0, g_1 >= 0, c_s > 0 required"
        raise ValueError(msg)

    if a_n_umol <= 0.0:
        return float(g_0)

    vpd_eff = max(vpd_kpa, VPD_MIN_KPA)
    extra = H2O_CO2_DIFF_RATIO * (1.0 + g_1 / np.sqrt(vpd_eff)) * (a_n_umol / c_s_ppm)
    return float(g_0 + extra)


def ci_from_gs(
    a_n_umol: float,
    c_s_ppm: float,
    g_s_mol: float,
) -> float:
    """Invert Fickian diffusion to recover c_i from gas-exchange quantities.

    Clamped to [0.05 c_s, 0.99 c_s] so the Newton solver does not wander into
    unphysical territory.
    """
    if g_s_mol <= 0.0 or c_s_ppm <= 0.0:
        return 0.7 * c_s_ppm
    c_i_raw = c_s_ppm - H2O_CO2_DIFF_RATIO * a_n_umol / g_s_mol
    c_i_lo = 0.05 * c_s_ppm
    c_i_hi = 0.99 * c_s_ppm
    return float(max(c_i_lo, min(c_i_hi, c_i_raw)))


# --- Leaf energy balance ---------------------------------------------------

LAMBDA_V = 2.45e6  # J kg^-1
CP_AIR = 1005.0  # J kg^-1 K^-1
M_AIR = 28.97  # g mol^-1
EPS_H2O = 0.622  # M_H2O / M_AIR
TLEAF_TOL_K = 1e-3
TLEAF_MAX_ITER = 15


def saturation_vapour_pressure(t_c: float) -> float:
    """Compute saturation vapour pressure [kPa] via the Tetens-Magnus form."""
    return 0.6108 * np.exp(17.27 * t_c / (t_c + 237.3))


def _svp_slope(t_c: float) -> float:
    """Compute the slope of the saturation vapour pressure curve [kPa K^-1]."""
    es = saturation_vapour_pressure(t_c)
    return 4098.0 * es / ((t_c + 237.3) ** 2)


@dataclass(frozen=True)
class LeafEbResult:
    """Return container for :func:`leaf_energy_balance`."""

    t_leaf_c: float
    le_w_m2: float
    h_w_m2: float
    converged: bool


def leaf_energy_balance(
    r_n_leaf_w_m2: float,
    t_air_c: float,
    vpd_kpa: float,
    pressure_kpa: float,
    g_s_mol: float,
    g_bw_mol: float,
    g_bh_mol: float,
) -> LeafEbResult:
    """Solve leaf energy balance for T_leaf.

    Mirrors ``module_phys_leafeb.leaf_energy_balance``. See the Fortran module
    header for the balance equations and references.

    Returns
    -------
    LeafEbResult
        Leaf temperature and fluxes at the converged state.
    """
    if pressure_kpa <= 0.0 or g_bh_mol <= 0.0:
        msg = "leaf_energy_balance: pressure > 0 and g_bh > 0 required"
        raise ValueError(msg)

    g_v_mol = 0.0 if g_s_mol + g_bw_mol <= 0.0 else g_s_mol * g_bw_mol / (g_s_mol + g_bw_mol)

    p_pa = pressure_kpa * 1000.0
    mass_factor = EPS_H2O / p_pa

    e_air = max(saturation_vapour_pressure(t_air_c) - vpd_kpa, 0.0)
    t_leaf = t_air_c
    converged = False
    le_w_m2 = 0.0
    h_w_m2 = 0.0

    for _ in range(TLEAF_MAX_ITER):
        es_leaf = saturation_vapour_pressure(t_leaf)
        delta_leaf = _svp_slope(t_leaf)

        h_w_m2 = CP_AIR * g_bh_mol * M_AIR * 1e-3 * (t_leaf - t_air_c)
        le_w_m2 = (
            LAMBDA_V
            * g_v_mol
            * mass_factor
            * M_AIR
            * 1e-3
            * (es_leaf * 1000.0 - e_air * 1000.0)
        )

        f = r_n_leaf_w_m2 - h_w_m2 - le_w_m2
        df = -CP_AIR * g_bh_mol * M_AIR * 1e-3 - (
            LAMBDA_V * g_v_mol * mass_factor * M_AIR * 1e-3 * delta_leaf * 1000.0
        )
        if abs(df) < 1e-12:
            break
        dt = -f / df
        dt = max(min(dt, 10.0), -10.0)
        t_leaf += dt
        if abs(dt) < TLEAF_TOL_K:
            converged = True
            break

    es_leaf = saturation_vapour_pressure(t_leaf)
    h_w_m2 = CP_AIR * g_bh_mol * M_AIR * 1e-3 * (t_leaf - t_air_c)
    le_w_m2 = (
        LAMBDA_V
        * g_v_mol
        * mass_factor
        * M_AIR
        * 1e-3
        * (es_leaf * 1000.0 - e_air * 1000.0)
    )
    return LeafEbResult(
        t_leaf_c=float(t_leaf),
        le_w_m2=float(le_w_m2),
        h_w_m2=float(h_w_m2),
        converged=bool(converged),
    )


# --- Coupled A-gs solver ---------------------------------------------------

CI_TOL_PPM = 0.1
AGS_TOL_K = 0.01
AGS_RELAX_CI = 0.5
AGS_MAX_ITER = 15


@dataclass(frozen=True)
class AgsResult:
    """Return container for :func:`ags_solve_leaf`."""

    a_n_umol: float
    g_s_mol: float
    c_i_ppm: float
    t_leaf_c: float
    iwue_umol_per_mol: float
    converged: bool
    iterations: int


def ags_solve_leaf(
    *,
    c3c4_flag: int,
    par_umol: float,
    c_s_ppm: float,
    t_air_c: float,
    vpd_kpa: float,
    pressure_kpa: float,
    r_n_leaf_w_m2: float,
    g_bw_mol: float,
    g_bh_mol: float,
    vcmax25: float,
    jmax25: float,
    rd25: float,
    g_0: float,
    g_1: float,
) -> AgsResult:
    """Solve the coupled A-gs problem for a single leaf.

    Mirrors ``module_phys_ags_solver.ags_solve_leaf``. Returns the converged
    (or best-effort) (A_n, g_s, c_i, T_leaf) together with an intrinsic WUE
    diagnostic ``A_n * 1.6 / g_s`` [umol CO2 / mol H2O].
    """
    if c_s_ppm <= 0.0 or g_bh_mol <= 0.0 or g_bw_mol <= 0.0:
        msg = "ags_solve_leaf: c_s > 0, g_bw > 0, g_bh > 0 required"
        raise ValueError(msg)

    c_i = 0.4 * c_s_ppm if c3c4_flag == 2 else 0.7 * c_s_ppm
    t_leaf = t_air_c
    a_n = 0.0
    g_s = g_0
    converged = False
    iter_count = 0

    for i in range(1, AGS_MAX_ITER + 1):
        iter_count = i

        if c3c4_flag == 2:
            a_n = farquhar_c4(par_umol, c_i, t_leaf, vcmax25, rd25).a_n
        else:
            a_n = farquhar_c3(par_umol, c_i, t_leaf, vcmax25, jmax25, rd25).a_n

        g_s = medlyn_gs(a_n, c_s_ppm, vpd_kpa, g_0, g_1)

        leaf = leaf_energy_balance(
            r_n_leaf_w_m2,
            t_air_c,
            vpd_kpa,
            pressure_kpa,
            g_s,
            g_bw_mol,
            g_bh_mol,
        )
        t_leaf_new = leaf.t_leaf_c

        c_i_raw = ci_from_gs(a_n, c_s_ppm, g_s)
        c_i_new = AGS_RELAX_CI * c_i_raw + (1.0 - AGS_RELAX_CI) * c_i

        if abs(c_i_new - c_i) < CI_TOL_PPM and abs(t_leaf_new - t_leaf) < AGS_TOL_K:
            c_i = c_i_new
            t_leaf = t_leaf_new
            converged = True
            break

        c_i = c_i_new
        t_leaf = t_leaf_new

    iwue = a_n * H2O_CO2_DIFF_RATIO / g_s if g_s > 0.0 else 0.0

    return AgsResult(
        a_n_umol=float(a_n),
        g_s_mol=float(g_s),
        c_i_ppm=float(c_i),
        t_leaf_c=float(t_leaf),
        iwue_umol_per_mol=float(iwue),
        converged=converged,
        iterations=iter_count,
    )
