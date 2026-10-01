"""Numerical edge cases for the Fortran splitter in a source build."""

import gzip
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

pytestmark = pytest.mark.physics


@pytest.fixture(scope="module")
def split_shortwave(tmp_path_factory):
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        pytest.skip("Scalar Fortran checks require gfortran and source-build libraries; driver tests cover installed wheels")
    module = suews / "mod" / "module_phys_spartacus.mod"
    if module.exists():
        try:
            with gzip.open(module, "rb") as stream:
                is_gnu = stream.readline().startswith(b"GFORTRAN")
        except gzip.BadGzipFile:
            is_gnu = False
        if not is_gnu:
            pytest.skip("Scalar harness uses gfortran and cannot read modules from a non-GNU source build")
    directory = tmp_path_factory.mktemp("reindl")
    source = directory / "split.f95"
    source.write_text(
        """program split
use module_phys_spartacus, only: split_shortwave_reindl
implicit none
integer :: doy, status
real(kind(1d0)) :: global, zenith, temperature, humidity, direct
do
  read(*, *, iostat=status) global, doy, zenith, temperature, humidity
  if (status /= 0) exit
  call split_shortwave_reindl(global, doy, zenith, temperature, humidity, direct)
  write(*, '(ES24.16)') direct
end do
end program
""",
        encoding="utf-8",
    )
    executable = directory / "split"
    compiled = subprocess.run(
        [compiler, str(source), "-I", str(suews / "mod"),
         "-L", str(suews / "lib"), "-lsuewsphys", "-lsuewsutil",
         "-lspartacus", "-fopenmp", "-o", str(executable)],
        check=False, capture_output=True, text=True,
    )
    assert compiled.returncode == 0, compiled.stderr

    def run(*rows):
        result = subprocess.run(
            [str(executable)], input="\n".join(rows) + "\n",
            check=True, capture_output=True, text=True,
        )
        return np.fromstring(result.stdout, sep=" ")

    return run


def test_reindl_reference_regimes(split_shortwave):
    # Day 1, zenith=60 degrees: extraterrestrial horizontal=709.02239328.
    # Hand-evaluated Reindl diffuse fractions for T=20 C, RH=50%:
    # kt=0.2 -> 0.96166; kt=0.5 -> 0.58610; kt=0.9 -> 0.36190.
    direct = split_shortwave(
        "141.804478656 1 60 20 50",
        "354.51119664 1 60 20 50",
        "638.120153952 1 60 20 50",
    )
    np.testing.assert_allclose(
        direct, [5.43678371167, 146.7321842893, 407.184470237],
        rtol=0, atol=1e-4,  # BEERS coefficients are legacy single precision.
    )


def test_reindl_night_and_nonpositive_global(split_shortwave):
    direct = split_shortwave(
        "0 1 60 20 50", "-999 1 60 20 50",
        "100 1 90 20 50", "100 1 100 20 50",
        "100 1 89.95 20 50",
    )
    np.testing.assert_allclose(direct, 0, rtol=0, atol=1e-12)


def test_reindl_missing_meteorology_uses_reduced_correlation(split_shortwave):
    direct = split_shortwave(
        "354.51119664 1 60 -999 50", "354.51119664 1 60 20 -999",
        "354.51119664 1 60 NaN 50", "354.51119664 1 60 20 NaN",
    )
    # At kt=0.5 the reduced correlation gives diffuse fraction 0.615.
    np.testing.assert_allclose(direct, 136.4868107064, rtol=0, atol=1e-4)


def test_reindl_low_sun_and_extreme_clearness_are_bounded(split_shortwave):
    direct = split_shortwave(
        "100 1 89.8 20 50", "100 1 89.5 20 50", "2000 1 60 20 50",
    )
    assert np.isfinite(direct).all()
    assert (direct >= 0).all()
    assert (direct <= [100, 100, 2000]).all()


def test_reindl_nonfinite_radiation_or_zenith_has_no_direct_beam(split_shortwave):
    direct = split_shortwave(
        "NaN 1 60 20 50", "Inf 1 60 20 50", "100 1 NaN 20 50",
    )
    np.testing.assert_allclose(direct, 0, rtol=0, atol=1e-12)
