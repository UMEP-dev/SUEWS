"""Aerodynamic resistance without an upper cap, and the (z-d)/L relaxation.

The hard cap MIN(RA, 120) and the u* floor of 0.001 m s-1 are gone: the
(z-d)/L clip in `cal_Stab` is the only physical bound on stability. Without
the cap the outer surface-temperature loop could two-cycle on near-neutral
nights (one pass unstable, the next on the stable clip), so (z-d)/L is
under-relaxed between passes by `relax_zl`.
"""

import gzip
import os
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pandas as pd
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]

RA_FORMER_CAP = 120.0  # removed hard cap [s m-1]
RA_FLOOR = 10.0  # retained lower limit [s m-1]
ZL_RELAX = 0.3  # relaxation factor in module_phys_atmmoiststab
MAX_OUTER_PASSES = 60  # max_iter_default in the driver
# Neutral method: RA = ln(zzd/z0m)**2 / (k2 * U); k2 is a single-precision 0.16
# in the Fortran, so a double-precision reconstruction agrees to ~1e-8 only.
K2 = 0.16
NEUTRAL_RTOL = 1e-6
# Sample-year steps where the plain (unrelaxed) iteration two-cycled once the
# cap was removed, and where the capped code also failed to converge.
SIGN_FLIP_WINDOW = ("2012-04-13 20:25", "2012-04-13 21:05")

PROBE = """program probe
use module_phys_resist, only: AerodynamicResistance
use module_phys_atmmoiststab, only: relax_zl
use module_ctrl_type, only: SUEWS_STATE
use module_ctrl_const_datain, only: SuppressWarnings
implicit none
integer :: mode, n, i
real(kind(1d0)) :: zzd, z0m, u, ra, z0v, a, b
type(SUEWS_STATE) :: state
SuppressWarnings = 0
read(*, *) mode, n
do i = 1, n
  if (mode == 1) then
    read(*, *) a, b
    write(*, '(ES25.17)') relax_zl(a, b)
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
def probe(tmp_path_factory):
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
    directory = tmp_path_factory.mktemp("stability_probe")
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


def test_aerodynamic_resistance_has_no_upper_cap(probe):
    # ARRANGE: neutral RA = ln(10)**2/(0.16 U) sweeps well past the former cap
    zzd, z0m = 10.0, 1.0
    wind = np.geomspace(0.05, 2.0, 400)
    raw = np.log(zzd / z0m) ** 2 / (K2 * wind)

    # ACT
    ra = probe(2, [[zzd, z0m, u] for u in wind])

    # ASSERT: the routine returns its raw value above the former cap
    assert raw.max() > 3 * RA_FORMER_CAP and raw.min() > RA_FLOOR
    np.testing.assert_allclose(ra, raw, rtol=NEUTRAL_RTOL)


def test_lower_floor_and_its_warning_are_preserved(probe):
    # Avoid round-off at the boundaries when reconstructing the neutral wind.
    raw = np.array([5.0, 10.1, 100.0, 119.9, 120.1, 200.0, 1000.0])
    wind = np.log(10.0) ** 2 / (K2 * raw)
    rows = [[10.0, 1.0, u] for u in wind]

    ra = probe(2, rows)
    warnings = probe(3, rows)

    assert ra[0] == pytest.approx(RA_FLOOR, abs=0.0)
    np.testing.assert_allclose(ra[1:], raw[1:], rtol=NEUTRAL_RTOL)
    # Only the lower limit warns; large resistance is no longer clipped or flagged.
    np.testing.assert_array_equal(warnings, [1, 0, 0, 0, 0, 0, 0])


def test_relax_zl_keeps_fixed_points_and_moves_part_way(probe):
    # ARRANGE: previous and new (z-d)/L across both stability regimes and the clips
    prev = np.array([-2.0, -0.18, 0.0, 0.1, 0.5, 0.3])
    new = np.array([-2.0, 0.5, -0.47, 0.1, -0.18, 0.5])

    # ACT
    out = probe(1, np.column_stack([prev, new]))

    # ASSERT: a converged pass is left exactly where it is, and otherwise the
    # step is the stated fraction of the change, so it stays inside the clips
    fixed = prev == new
    np.testing.assert_array_equal(out[fixed], prev[fixed])
    np.testing.assert_allclose(out, prev + ZL_RELAX * (new - prev), rtol=0, atol=1e-15)
    assert np.all(out >= np.minimum(prev, new)) and np.all(out <= np.maximum(prev, new))


def test_former_sign_flip_steps_converge_without_the_cap(sample_run_cached):
    """On 13 April 2012 the uncapped plain iteration flipped (z-d)/L between
    unstable and the stable clip on every pass and stopped at the pass limit;
    the capped code also failed there. With relaxation the whole sample year
    converges, while the published RA exceeds the former cap elsewhere."""
    # ARRANGE / ACT
    df_output, _ = sample_run_cached(None)
    passes = df_output[("debug", "iter")]
    ra = df_output[("SUEWS", "RA")]
    window = pd.IndexSlice[:, slice(*SIGN_FLIP_WINDOW)]

    # ASSERT
    assert len(passes.loc[window]) == 9
    assert passes.loc[window].max() < MAX_OUTER_PASSES
    assert (passes < MAX_OUTER_PASSES).all()
    assert ra.max() > RA_FORMER_CAP
