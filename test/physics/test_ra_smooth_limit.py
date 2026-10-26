"""Smooth upper limit on aerodynamic resistance, through the compiled Fortran.

The former hard cap MIN(RA, 120) has a slope that jumps from 1 to 0, a kink the
surface-temperature iteration could two-cycle across on stable nights.
`limit_ra_upper` replaces it: identity up to a knee, then a tanh blend that
approaches the same 120 s m-1 from below.
"""

import gzip
import os
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]

RA_MAX = 120.0  # asymptote [s m-1]
RA_KNEE = 90.0  # identity up to here [s m-1]
WIDTH = RA_MAX - RA_KNEE
# Neutral method: RA = ln(zzd/z0m)**2 / (k2 * U); k2 is a single-precision 0.16
# in the Fortran, so a double-precision reconstruction agrees to ~1e-8 only.
K2 = 0.16
NEUTRAL_RTOL = 1e-6

PROBE = """program probe
use module_phys_resist, only: limit_ra_upper, AerodynamicResistance
use module_ctrl_type, only: SUEWS_STATE
use module_ctrl_const_datain, only: SuppressWarnings
implicit none
integer :: mode, n, i
real(kind(1d0)) :: x, zzd, z0m, u, ra, z0v
type(SUEWS_STATE) :: state
SuppressWarnings = 0
read(*, *) mode, n
do i = 1, n
  if (mode == 1) then
    read(*, *) x
    write(*, '(ES25.17)') limit_ra_upper(x)
  else
    read(*, *) zzd, z0m, u
    ! neutral method (1); stability inputs are unused by it
    call state%errorstate%clear_log()
    call AerodynamicResistance(zzd, z0m, u, -999.0d0, 0.3d0, 0.0d0, 1, 3, 2, ra, z0v, state)
    if (mode == 3) then
      write(*, '(I0)') state%errorstate%count
    else
      write(*, '(ES25.17)') ra
    end if
  end if
end do
end program
"""


def _probe_unavailable(reason):
    if os.environ.get("SUEWS_REQUIRE_FORTRAN_PROBES") == "1":
        pytest.fail(reason)
    pytest.skip(reason)


@pytest.fixture(scope="module")
def ra_probe(tmp_path_factory):
    """Compile a probe against the source-build libraries; no copied physics."""
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        _probe_unavailable("Native probe needs gfortran and source-build libraries; run tests cover wheels")
    try:
        with gzip.open(suews / "mod" / "module_phys_resist.mod", "rb") as stream:
            is_gnu = stream.readline().startswith(b"GFORTRAN")
    except (gzip.BadGzipFile, FileNotFoundError):
        is_gnu = False
    if not is_gnu:
        _probe_unavailable("Native probe needs GNU Fortran module files")
    directory = tmp_path_factory.mktemp("ra_limit")
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

    def run(mode, rows):
        lines = [f"{mode} {len(rows)}"] + [" ".join(repr(float(v)) for v in row) for row in rows]
        result = subprocess.run(
            [str(executable)], input="\n".join(lines) + "\n",
            check=True, capture_output=True, text=True,
        )
        return np.array([float(v) for v in result.stdout.split()])

    return run


@pytest.fixture(scope="module")
def limit(ra_probe):
    return lambda x: ra_probe(1, [[v] for v in np.asarray(x, dtype=float)])


def slope_jumps(x, y):
    """Largest change in forward-difference slope between neighbouring cells."""
    slope = np.diff(y) / np.diff(x)
    return np.abs(np.diff(slope)).max()


def test_identity_below_knee(limit):
    x = np.concatenate([np.linspace(0.0, RA_KNEE, 9001), [10.0, 50.0, 89.999999]])
    np.testing.assert_array_equal(limit(x), x)


def test_value_and_slope_continuous_across_knee_and_former_cap(limit):
    # ARRANGE: fine grid spanning the knee and the former cap
    h = 1e-3
    x = np.arange(80.0, 130.0 + h / 2, h)

    # ACT
    y = limit(x)

    # ASSERT: value continuity -- no step larger than the slope bound (1) allows
    assert np.abs(np.diff(y)).max() <= h * (1 + 1e-9)
    # slope continuity -- tanh curvature is at most 0.77/WIDTH, so neighbouring
    # slopes differ by O(h); a kink would show a jump of order 1
    assert slope_jumps(x, y) < 1e-3
    # the same detector does see the former hard cap, whose slope jumps by 1
    assert slope_jumps(x, np.minimum(x, RA_MAX)) > 0.9
    # one-sided slopes at the knee and the former cap match the analytic form
    slope = np.diff(y) / h
    at = lambda v: slope[np.searchsorted(x, v)]
    assert at(RA_KNEE) == pytest.approx(1.0, abs=1e-4)
    assert at(RA_MAX) == pytest.approx(1 / np.cosh(1.0) ** 2, abs=1e-4)


def test_monotone_and_bounded_by_asymptote(limit):
    x = np.linspace(0.0, 1000.0, 100001)
    y = limit(x)
    assert np.all(np.diff(y) >= 0.0)
    # strictly increasing wherever tanh has not yet saturated in double precision
    assert np.all(np.diff(y[x <= 300.0]) > 0.0)
    # below 120 wherever tanh is below 1 in double precision (argument < ~19,
    # raw RA < ~650); beyond that the limit equals 120 to round-off
    assert np.all(y[x <= 600.0] < RA_MAX)
    assert np.all(y <= RA_MAX)


def test_asymptote_and_departure_from_former_cap(limit):
    far = limit([200.0, 1e3, 1e6, 1e300])
    assert np.all(far <= RA_MAX)
    np.testing.assert_allclose(far[1:], RA_MAX, rtol=0, atol=1e-9)
    # largest departure from MIN(x, 120) is WIDTH*(1 - tanh 1), at x = 120
    x = np.linspace(0.0, 400.0, 40001)
    departure = np.minimum(x, RA_MAX) - limit(x)
    assert departure.min() >= 0.0
    assert departure.max() == pytest.approx(WIDTH * (1 - np.tanh(1.0)), rel=1e-12)
    assert x[departure.argmax()] == pytest.approx(RA_MAX)


def test_aerodynamic_resistance_applies_the_limit(ra_probe, limit):
    # ARRANGE: neutral RA = ln(10)**2/(0.16 U) sweeps through the knee and cap
    zzd, z0m = 10.0, 1.0
    wind = np.geomspace(0.05, 2.0, 400)
    raw = np.log(zzd / z0m) ** 2 / (K2 * wind)

    # ACT
    ra = ra_probe(2, [[zzd, z0m, u] for u in wind])

    # ASSERT: the routine returns the limiter applied to its raw value
    assert raw.max() > 3 * RA_MAX and raw.min() < RA_KNEE
    np.testing.assert_allclose(ra, limit(raw), rtol=NEUTRAL_RTOL)
    assert np.all(ra <= RA_MAX)
    np.testing.assert_allclose(ra[raw < RA_KNEE], raw[raw < RA_KNEE], rtol=NEUTRAL_RTOL)


def test_lower_floor_and_warning_threshold_are_preserved(ra_probe):
    # Avoid round-off at the boundaries when reconstructing the neutral wind.
    raw = np.array([5.0, 10.1, 89.9, 90.1, 100.0, 119.9, 120.1, 200.0])
    wind = np.log(10.0) ** 2 / (K2 * raw)
    rows = [[10.0, 1.0, u] for u in wind]

    ra = ra_probe(2, rows)
    warnings = ra_probe(3, rows)

    assert ra[0] == pytest.approx(10.0, abs=0.0)
    assert ra[1] == pytest.approx(raw[1], rel=NEUTRAL_RTOL)
    # The smooth section below the old cap is quiet; both original warnings remain.
    np.testing.assert_array_equal(warnings, [1, 0, 0, 0, 0, 0, 1, 1])
