"""Tier A textbook tests for the NumPy FvCB reference implementation.

These tests exercise :mod:`supy.util._ags_reference`, which mirrors the
forthcoming Fortran module ``module_phys_farquhar`` (already compiled into
``libsuewsdriver.a`` but not yet Python-callable — the bridge wrapper lands
in Week 4). The reference serves as:

1. A runnable Tier A contract pinning the FvCB/Collatz equations at the
   magnitudes reported in Sharkey et al. (2007), Medlyn et al. (2002), and
   Keeling et al. (2017).
2. An independent cross-check oracle for the Fortran path once exposed to
   Python.

Tests here deliberately avoid bit-level numeric checks — the physics
predictions (signs, monotonicity, temperature-peak window, heat-stress
collapse) are what matter and what the companion paper will be evaluated
against.
"""

from __future__ import annotations

import numpy as np
import pytest

from supy.util._ags_reference import (  # noqa: PLC2701 - reference is a private util by design
    ALPHA_C4,
    DS_VCMAX,
    EA_VCMAX,
    GAMMA_STAR_25,
    H2O_CO2_DIFF_RATIO,
    HD_VCMAX,
    KC_25,
    KO_25,
    ags_solve_leaf,
    arrhenius_response,
    ci_from_gs,
    farquhar_c3,
    farquhar_c4,
    leaf_energy_balance,
    medlyn_gs,
    peaked_arrhenius_response,
    saturation_vapour_pressure,
)

# --- Arrhenius / temperature response ---------------------------------------


def test_arrhenius_at_reference_returns_k25():
    assert arrhenius_response(404.9, 79_430.0, 25.0) == pytest.approx(404.9)


def test_peaked_arrhenius_at_reference_returns_k25():
    # At 25 C the peaked form must reduce to k25 (the numerator/denominator
    # cancel because T = T_REF).
    value = peaked_arrhenius_response(60.0, EA_VCMAX, HD_VCMAX, DS_VCMAX, 25.0)
    assert value == pytest.approx(60.0)


def test_peaked_arrhenius_has_peak_between_30_and_40C():
    # Medlyn 2002: V_cmax for C3 temperate plants peaks in the 30-40 C window.
    temps = np.linspace(5.0, 55.0, 101)
    values = np.array(
        [peaked_arrhenius_response(60.0, EA_VCMAX, HD_VCMAX, DS_VCMAX, t) for t in temps]
    )
    peak_t = temps[values.argmax()]
    assert 30.0 <= peak_t <= 40.0

    # Well below and well above the peak, V_cmax must be below its peak value.
    peak_value = values.max()
    assert peaked_arrhenius_response(60.0, EA_VCMAX, HD_VCMAX, DS_VCMAX, 10.0) < peak_value
    assert peaked_arrhenius_response(60.0, EA_VCMAX, HD_VCMAX, DS_VCMAX, 50.0) < peak_value


# --- C3 net photosynthesis --------------------------------------------------


def _c3_midday_standard():
    """A canonical 'healthy C3 leaf at noon under ambient CO2'."""
    return farquhar_c3(
        par_umol=1500.0,
        c_i_ppm=280.0,
        t_leaf_c=25.0,
        vcmax25=60.0,
        jmax25=100.0,
        rd25=0.9,
    )


def test_c3_midday_magnitude_matches_textbook():
    # Sharkey et al. (2007) Table 1 / Fig 2: A_n for temperate C3 leaves in
    # the 10-20 umol m-2 s-1 band at PAR=1500, c_i=280, T=25.
    r = _c3_midday_standard()
    assert 10.0 <= r.a_n <= 20.0
    # Decomposition: both rates positive, Rubisco and RuBP limits bracket A_n.
    assert r.a_c > 0.0
    assert r.a_j > 0.0
    assert r.a_n == pytest.approx(min(r.a_c, r.a_j) - 0.9, rel=1e-6)


def test_c3_is_light_limited_at_low_par():
    # At low PAR the RuBP-regeneration limit (A_j) should be the binding one.
    r = farquhar_c3(
        par_umol=50.0,
        c_i_ppm=280.0,
        t_leaf_c=25.0,
        vcmax25=60.0,
        jmax25=100.0,
        rd25=0.9,
    )
    assert r.a_j < r.a_c


def test_c3_saturates_with_par():
    # Increasing PAR beyond saturation cannot push A_n above min(A_c, A_j)-R_d.
    hi = farquhar_c3(2000.0, 280.0, 25.0, 60.0, 100.0, 0.9)
    saturating = farquhar_c3(3000.0, 280.0, 25.0, 60.0, 100.0, 0.9)
    assert saturating.a_n == pytest.approx(hi.a_n, rel=5e-3)


def test_c3_increases_monotonically_with_ci():
    # CO2 fertilisation: A_n should rise monotonically with c_i over a
    # realistic range (100-800 ppm).
    cis = [100.0, 200.0, 280.0, 400.0, 600.0, 800.0]
    ans = [
        farquhar_c3(1500.0, ci, 25.0, 60.0, 100.0, 0.9).a_n for ci in cis
    ]
    assert all(ans[i] < ans[i + 1] for i in range(len(ans) - 1))


def test_c3_heat_stress_drives_an_negative():
    # Above the peaked V_cmax window, A_n collapses and can turn negative
    # as respiration exceeds gross photosynthesis. This is the heat-stress
    # signal the A-gs work is intended to capture (Grossiord et al. 2020).
    cool = farquhar_c3(1500.0, 280.0, 25.0, 60.0, 100.0, 0.9)
    hot = farquhar_c3(1500.0, 280.0, 45.0, 60.0, 100.0, 0.9)
    assert hot.a_n < cool.a_n
    assert hot.a_n < 0.0


def test_c3_co2_compensation_point_makes_an_negative():
    # Below Gamma* the leaf loses carbon: gross photosynthesis is smaller
    # than dark respiration, so A_n must be negative.
    r = farquhar_c3(1500.0, 20.0, 25.0, 60.0, 100.0, 0.9)
    assert r.a_n < 0.0


def test_c3_rejects_nonpositive_vcmax():
    with pytest.raises(ValueError, match="Vcmax25"):
        farquhar_c3(1500.0, 280.0, 25.0, 0.0, 100.0, 0.9)


def test_c3_rejects_nonpositive_jmax():
    with pytest.raises(ValueError, match="Jmax25"):
        farquhar_c3(1500.0, 280.0, 25.0, 60.0, -1.0, 0.9)


# --- C4 net photosynthesis --------------------------------------------------


def test_c4_rubisco_is_vcmax_at_reference_temperature():
    # At 25 C the peaked V_cmax equals V_cmax25; Collatz 1992 makes the
    # Rubisco limit saturate at V_cmax(T) thanks to the CCM.
    r = farquhar_c4(1500.0, 150.0, 25.0, vcmax25=40.0, rd25=0.5)
    assert r.a_c == pytest.approx(40.0)


def test_c4_light_limit_scales_with_par():
    lo = farquhar_c4(200.0, 150.0, 25.0, 40.0, 0.5)
    hi = farquhar_c4(2000.0, 150.0, 25.0, 40.0, 0.5)
    assert hi.a_i == pytest.approx(ALPHA_C4 * 2000.0)
    assert lo.a_i == pytest.approx(ALPHA_C4 * 200.0)
    assert lo.a_i < hi.a_i


def test_c4_reject_nonpositive_vcmax():
    with pytest.raises(ValueError, match="Vcmax25"):
        farquhar_c4(1500.0, 150.0, 30.0, 0.0, 0.5)


# --- Parameter provenance --------------------------------------------------


def test_reference_values_match_bernacchi_2001():
    # The K_c, K_o, and Gamma_star references at 25 C should match
    # Bernacchi et al. (2001). This guards against accidental parameter
    # drift during the Fortran/NumPy refactors.
    assert pytest.approx(404.9) == KC_25
    assert pytest.approx(278_400.0) == KO_25
    assert pytest.approx(42.75) == GAMMA_STAR_25


# --- Medlyn stomatal conductance -------------------------------------------


def test_medlyn_returns_g0_when_photosynthesis_collapses():
    # Heat stress / drought drives A_n below zero; Medlyn's optimisation
    # collapses to the residual conductance g_0. This is the floor that
    # prevents runaway transpiration under stress.
    g_s = medlyn_gs(a_n_umol=-2.0, c_s_ppm=410.0, vpd_kpa=2.5, g_0=0.01, g_1=3.8)
    assert g_s == pytest.approx(0.01)


def test_medlyn_rises_with_photosynthesis():
    low = medlyn_gs(2.0, 410.0, 1.5, 0.01, 3.8)
    hi = medlyn_gs(15.0, 410.0, 1.5, 0.01, 3.8)
    assert low < hi


def test_medlyn_falls_with_vpd():
    # The sqrt(VPD) scaling (Medlyn 2011 theory) means drier air shrinks
    # g_s even for the same A_n. Monotonicity in VPD is the theory's
    # falsifiable prediction.
    humid = medlyn_gs(10.0, 410.0, 0.5, 0.01, 3.8)
    moderate = medlyn_gs(10.0, 410.0, 1.5, 0.01, 3.8)
    dry = medlyn_gs(10.0, 410.0, 3.0, 0.01, 3.8)
    assert humid > moderate > dry


def test_medlyn_co2_structural_sensitivity():
    # At fixed A_n, doubling c_s halves the A/c_s term, so g_s falls. This
    # is the structural mechanism by which the coupled solver gets its
    # CO2 fertilisation signal.
    low_ca = medlyn_gs(10.0, 410.0, 1.5, 0.01, 3.8)
    high_ca = medlyn_gs(10.0, 820.0, 1.5, 0.01, 3.8)
    assert high_ca < low_ca


def test_medlyn_rejects_nonpositive_c_s():
    with pytest.raises(ValueError, match="c_s"):
        medlyn_gs(10.0, 0.0, 1.5, 0.01, 3.8)


def test_ci_from_gs_inverts_fickian_diffusion():
    # A_n = (g_s / 1.6) * (c_s - c_i) -> c_i = c_s - 1.6 * A_n / g_s.
    c_i = ci_from_gs(a_n_umol=10.0, c_s_ppm=410.0, g_s_mol=0.2)
    assert c_i == pytest.approx(410.0 - H2O_CO2_DIFF_RATIO * 10.0 / 0.2)


# --- Leaf energy balance ---------------------------------------------------


def test_svp_at_25c_matches_reference():
    # Tetens-Magnus: e_sat(25 C) approx 3.17 kPa.
    es = saturation_vapour_pressure(25.0)
    assert 3.1 <= es <= 3.2


def test_leaf_eb_closed_stomata_drives_high_h():
    # With stomata closed (g_s small), almost all absorbed R_n must leave
    # as sensible heat H -> leaf overheats. This is the stress-feedback
    # channel we want physically represented.
    result = leaf_energy_balance(
        r_n_leaf_w_m2=400.0,
        t_air_c=25.0,
        vpd_kpa=1.5,
        pressure_kpa=101.325,
        g_s_mol=0.01,
        g_bw_mol=1.0,
        g_bh_mol=1.0,
    )
    assert result.converged
    assert result.t_leaf_c > 30.0  # leaf much hotter than air
    assert result.h_w_m2 > 5.0 * result.le_w_m2


def test_leaf_eb_open_stomata_leaves_leaf_near_air():
    # Conversely, open stomata convert absorbed R_n into LE, keeping the
    # leaf close to air temperature.
    result = leaf_energy_balance(
        r_n_leaf_w_m2=400.0,
        t_air_c=25.0,
        vpd_kpa=1.5,
        pressure_kpa=101.325,
        g_s_mol=0.3,
        g_bw_mol=1.0,
        g_bh_mol=1.0,
    )
    assert result.converged
    assert result.t_leaf_c - 25.0 < 7.0
    assert result.le_w_m2 > result.h_w_m2


def test_leaf_eb_rejects_nonpositive_pressure():
    with pytest.raises(ValueError, match="pressure"):
        leaf_energy_balance(400.0, 25.0, 1.5, 0.0, 0.3, 1.0, 1.0)


# --- Coupled A-gs solver ---------------------------------------------------


def _ags_midday_standard(**overrides):
    kwargs = {
        "c3c4_flag": 1,
        "par_umol": 1500.0,
        "c_s_ppm": 410.0,
        "t_air_c": 25.0,
        "vpd_kpa": 1.5,
        "pressure_kpa": 101.325,
        "r_n_leaf_w_m2": 400.0,
        "g_bw_mol": 1.0,
        "g_bh_mol": 1.0,
        "vcmax25": 60.0,
        "jmax25": 100.0,
        "rd25": 0.9,
        "g_0": 0.01,
        "g_1": 3.8,
    }
    kwargs.update(overrides)
    return ags_solve_leaf(**kwargs)


def test_ags_solver_converges_at_standard_conditions():
    r = _ags_midday_standard()
    assert r.converged
    assert r.iterations <= 15
    assert 5.0 <= r.a_n_umol <= 30.0
    # C3 typical c_i / c_a ratio in [0.6, 0.85].
    assert 0.6 * 410.0 <= r.c_i_ppm <= 0.85 * 410.0


def test_ags_solver_iwue_rises_with_co2():
    low = _ags_midday_standard(c_s_ppm=410.0)
    hi = _ags_midday_standard(c_s_ppm=800.0)
    assert hi.a_n_umol > low.a_n_umol  # CO2 fertilisation
    assert hi.g_s_mol < low.g_s_mol  # stomata close slightly
    assert hi.iwue_umol_per_mol > low.iwue_umol_per_mol  # iWUE rises


def test_ags_solver_collapses_under_heat_stress():
    # 42 C and 4 kPa VPD: V_cmax is denaturing, VPD starves the Medlyn
    # numerator -> stomata close to g_0 and A_n turns negative.
    r = _ags_midday_standard(t_air_c=42.0, vpd_kpa=4.0)
    assert r.a_n_umol < 0.0
    assert r.g_s_mol == pytest.approx(0.01)  # Medlyn floor
    # Leaf overheats because transpirational cooling collapsed.
    assert r.t_leaf_c > 45.0


def test_ags_solver_darkness_gives_respiration_only():
    # PAR = 0 -> A is respiration-dominated (negative), g_s = g_0.
    r = _ags_midday_standard(par_umol=0.0)
    assert r.a_n_umol < 0.0
    assert r.g_s_mol == pytest.approx(0.01)


def test_ags_solver_rejects_nonpositive_boundary_conductance():
    with pytest.raises(ValueError, match="g_bw > 0"):
        _ags_midday_standard(g_bw_mol=0.0)


def test_ags_solver_c4_runs_and_converges():
    # Smoke-check the C4 branch: should converge at standard midday.
    r = _ags_midday_standard(
        c3c4_flag=2,
        vcmax25=40.0,
        g_1=1.6,
        rd25=0.5,
    )
    assert r.converged
    assert r.a_n_umol > 0.0
