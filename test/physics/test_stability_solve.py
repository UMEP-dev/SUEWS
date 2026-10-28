"""The (z-d)/L solve in cal_Stab, through the compiled Fortran (#1962).

cal_Stab used to iterate zL <- clip(f(zL)) and stop once a step changed zL by
0.001 or less. Near the stable fold f lies just above the identity, so the
loop crawled and stopped short of its solution; inputs 1e-10 W m-2 apart
returned 0.357 or 0.5. It now brackets the residual g(zL) = f(zL) - zL and
takes the root nearest neutral, or the clip when no root lies inside it.
"""

import gzip
import os
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]

STABILITY_METHODS = (2, 3, 4)  # W16, K75 (Campbell & Norman), B71
ZL_STABLE_MAX, ZL_UNSTABLE_MIN = 0.5, -2.0  # clips in cal_Stab [-]
RESIDUAL_TOL = 1e-9  # well above the solver's 1e-12, far below the old 1e-3 step
# 2013-07-23 00:20, London benchmark (stability method 3): cal_Stab inputs
# recovered from the run (zzd from zL*L, rho*cp from QH/(T* u*)).
PROBE_SITE = dict(zzd=25.8, z0m=1.9, zdm=14.2, u=2.43, tair=23.39, rhocp=1201.696378861587)
# Fed QH on consecutive outer passes there; the old loop returned 0.357 for
# the first two and 0.5 for the others.
PROBE_QH = (-14.718630457399422, -14.718630459294966, -14.718630459900247, -14.718630460531088)

PROBE = """program probe
use module_phys_atmmoiststab, only: cal_Stab, stab_psi_mom, stability_scales, relax_zl, k, grav
use module_phys_resist, only: AerodynamicResistance
use module_ctrl_error_state, only: get_supy_error, reset_supy_error
implicit none
integer :: mode, n, i, meth, code
character(len=512) :: message
real(kind(1d0)) :: zzd, z0m, zdm, u, t, rhocp, x, qh, l, ts, us, zl, h, lm, expected, ra, z0v
read(*, *) mode, n, meth, zzd, z0m, zdm, u, t, rhocp
do i = 1, n
  read(*, *) x, qh
  if (mode == 1) then
    ! qh is the sensible heat flux [W m-2]; x is unused
    call cal_Stab(meth, zzd, z0m, zdm, u, t, qh, rhocp, 1d0, l, ts, us, zl)
    write(*, '(ES26.17E3)') zl
  else if (mode == 3) then
    ! Fatal code and every output sentinel, then clear the thread-local store.
    call reset_supy_error()
    call cal_Stab(meth, zzd, z0m, zdm, u, t, qh, rhocp, 1d0, l, ts, us, zl)
    call get_supy_error(code, message)
    write(*, '(5ES26.17E3)') real(code, kind(1d0)), l, ts, us, zl
    call reset_supy_error()
  else if (mode == 4) then
    ! Relax from x towards 0.5, then check the momentum and heat identities.
    zl = relax_zl(x, 0.5d0, 2)
    h = qh/rhocp
    call stability_scales(meth, zzd, z0m, u, h, zl, l, us, ts)
    expected = max(1d-12, k*u/(log(zzd/z0m) - stab_psi_mom(meth, zl) &
                 + stab_psi_mom(meth, zl*z0m/zzd)))
    call AerodynamicResistance(zzd, z0m, u, l, us, 0d0, 2, meth, 2, ra, z0v)
    write(*, '(7ES26.17E3)') zl, l, us, ts, expected, ra, zzd/l
  else
    ! residual f(z) - z at z = x for heat flux qh
    h = qh/rhocp
    lm = zzd/x
    us = max(1d-12, k*u/(log(zzd/z0m) - stab_psi_mom(meth, x) + stab_psi_mom(meth, z0m/lm)))
    write(*, '(ES26.17E3)') -zzd*((grav/(t + 273.15))*k)*h/us**3 - x
  end if
end do
end program
"""


def _probe_unavailable(reason):
    if os.environ.get("SUEWS_REQUIRE_FORTRAN_PROBES") == "1":
        pytest.fail(reason)
    pytest.skip(reason)


@pytest.fixture(scope="module")
def probe(tmp_path_factory):
    """Compile a probe against the source-build libraries; no copied physics."""
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        _probe_unavailable("Native probe needs gfortran and source-build libraries; run tests cover wheels")
    try:
        with gzip.open(suews / "mod" / "module_phys_atmmoiststab.mod", "rb") as stream:
            is_gnu = stream.readline().startswith(b"GFORTRAN")
    except (gzip.BadGzipFile, FileNotFoundError):
        is_gnu = False
    if not is_gnu:
        _probe_unavailable("Native probe needs GNU Fortran module files")
    directory = tmp_path_factory.mktemp("stability_solve")
    source = directory / "probe.f95"
    source.write_text(PROBE, encoding="ascii")
    executable = directory / "probe"
    compiled = subprocess.run(
        [compiler, str(source), "-I", str(suews / "mod"),
         "-L", str(suews / "lib"), "-lsuewsphys", "-lsuewsutil",
         "-lspartacus", "-fopenmp", "-o", str(executable)],
        check=False, capture_output=True, text=True,
    )
    assert compiled.returncode == 0, compiled.stderr

    def run(mode, method, zl, qh, site=PROBE_SITE):
        zl, qh = np.broadcast_arrays(np.asarray(zl, dtype=float), np.asarray(qh, dtype=float))
        header = [mode, zl.size, method, *site.values()]
        lines = [" ".join(repr(v) for v in header)]
        lines += [f"{float(z)!r} {float(q)!r}" for z, q in zip(zl.ravel(), qh.ravel())]
        result = subprocess.run(
            [str(executable)], input="\n".join(lines) + "\n",
            check=True, capture_output=True, text=True,
        )
        return np.array([float(v) for v in result.stdout.split()])

    return run


def test_probed_fold_case_returns_the_clip(probe):
    """No consistent (z-d)/L lies inside the stable clip at the probed step,
    so every fed flux there returns the clip, not wherever a crawl stopped."""
    # ARRANGE: the residual is positive all the way up to the clip
    grid = np.linspace(1e-4, ZL_STABLE_MAX, 2001)
    residual = probe(2, 3, grid, PROBE_QH[0])

    # ACT
    zl = probe(1, 3, 0.0, PROBE_QH)

    # ASSERT
    assert residual.min() > 0.0
    np.testing.assert_array_equal(zl, ZL_STABLE_MAX)


@pytest.mark.parametrize("method", STABILITY_METHODS)
def test_returned_zl_is_a_root_or_the_clip(probe, method):
    # ARRANGE: downward and upward fluxes, through the stable fold
    qh = np.r_[-np.geomspace(0.01, 60.0, 400), np.geomspace(0.01, 600.0, 400)]

    # ACT
    zl = probe(1, method, 0.0, qh)
    g = probe(2, method, zl, qh)

    # ASSERT: sign follows the flux and the clips hold
    assert np.all(zl[qh < 0] > 0.0) and np.all(zl[qh > 0] < 0.0)
    assert np.all((zl >= ZL_UNSTABLE_MIN) & (zl <= ZL_STABLE_MAX))
    at_stable_clip = zl == ZL_STABLE_MAX
    at_unstable_clip = zl == ZL_UNSTABLE_MIN
    interior = ~(at_stable_clip | at_unstable_clip)
    # at a clip the flux implies more stability (or instability) than the clip
    assert np.all(g[at_stable_clip] > 0.0) and np.all(g[at_unstable_clip] < 0.0)
    # A positive endpoint alone can hide a pair of interior roots. Check all
    # clipped cases against a fine grid on the entire corresponding branch.
    for clipped, limit, sign in ((at_stable_clip, ZL_STABLE_MAX, 1),
                                 (at_unstable_clip, ZL_UNSTABLE_MIN, -1)):
        grid = np.linspace(limit / 10000, limit, 10000)
        residual = probe(2, method, grid[:, None], qh[clipped][None, :])
        assert np.all(sign * residual > 0.0)
    # inside the clips the returned value is consistent with the flux
    assert np.all(np.abs(g[interior]) <= RESIDUAL_TOL * np.maximum(1.0, np.abs(zl[interior])))


@pytest.mark.parametrize("method", STABILITY_METHODS)
def test_stable_zl_is_monotone_in_the_flux(probe, method):
    """On the branch continuous with neutral, more downward flux never gives
    less stability, and tiny changes in the flux cannot flip the answer back."""
    qh = -np.linspace(0.01, 60.0, 3000)
    zl = probe(1, method, 0.0, qh)
    assert np.all(np.diff(zl) >= 0.0)


@pytest.mark.parametrize("field", ["qh", "u", "tair", "zzd", "z0m"])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
def test_non_finite_input_contract(probe, field, value):
    site = PROBE_SITE.copy()
    qh = -14.7
    if field == "qh":
        qh = value
    else:
        site[field] = value
    result = probe(3, 3, 0.0, qh, site)
    np.testing.assert_array_equal(result, [110, -999, -999, -999, -999])


def test_finite_inputs_leave_no_error(probe):
    result = probe(3, 3, 0.0, [-14.7, 0.0, 50.0]).reshape(-1, 5)
    np.testing.assert_array_equal(result[:, 0], 0)
    assert np.isfinite(result).all()
    assert not (result[:, 1:] == -999).any()


@pytest.mark.parametrize("qh", [-12.65377713054404, -12.654])
def test_two_roots_between_scan_points_select_nearest_neutral(probe, qh):
    # Both crossings sit inside the old scan cell, whose endpoints are positive.
    grid = np.linspace(0.1836629282, 0.2564509649, 10001)
    residual = probe(2, 4, grid, qh)
    crossings = np.flatnonzero(residual[:-1] * residual[1:] < 0)
    assert len(crossings) == 2
    assert residual[0] > 0 and residual[-1] > 0
    zl = probe(1, 4, 0.0, qh)[0]
    first = crossings[0]
    assert grid[first] <= zl <= grid[first + 1]
    assert abs(probe(2, 4, zl, qh)[0]) <= RESIDUAL_TOL


@pytest.mark.parametrize("method", STABILITY_METHODS)
@pytest.mark.parametrize("previous", [0.1, -0.5, -3 / 14])
@pytest.mark.parametrize("wind", [2.43, 0.0])
def test_relaxed_scales_and_neutral_cancellation(probe, method, previous, wind):
    site = {**PROBE_SITE, "u": wind}
    zl, length, us, ts, expected, ra, ratio = probe(4, method, previous, -12.0, site)
    assert np.isfinite([zl, length, us, ts, ra, ratio]).all()
    assert us >= 1e-12
    assert us == pytest.approx(expected, rel=1e-14)
    assert ts * us == pytest.approx(12.0 / site["rhocp"], rel=1e-14)
    if abs(zl) <= 1e-12:
        # neutral: z/L of about 1e-12 and a finite RSL Lc/(Lc/L) round trip
        assert abs(ratio) == pytest.approx(1e-12, rel=1e-12)
        for lc in (0.1, 3.0, 100.0):
            assert np.isfinite(lc / (lc / length))
    else:
        assert ratio == pytest.approx(zl, rel=1e-14)
