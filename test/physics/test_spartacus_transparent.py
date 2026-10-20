"""Analytical transparent-layer limits of the SPARTACUS matrix solvers."""

import ctypes
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]


@pytest.fixture(scope="module")
def matrix_solvers(tmp_path_factory):
    compiler = shutil.which("gfortran")
    if compiler is None:
        pytest.skip("Native matrix checks require gfortran; model tests cover installed wheels")
    source_dir = (
        Path(__file__).resolve().parents[2]
        / "src/suews/ext_lib/spartacus-surface"
    )
    directory = tmp_path_factory.mktemp("spartacus_matrices")
    wrapper = directory / "wrapper.f90"
    wrapper.write_text(
        """subroutine matrices(band, n, rates, results) bind(C)
  use iso_c_binding, only: c_int, c_double
  use radtool_calc_matrices_lw_eig, only: calc_matrices_lw_eig
  use radtool_calc_matrices_sw_eig, only: calc_matrices_sw_eig
  implicit none
  integer(c_int), value :: band, n
  real(c_double), intent(in) :: rates(2)
  real(c_double), intent(out) :: results(2,n,n,8)
  real(c_double) :: g0(2,n,n), g1(2,n,n), g2(2,n,n), g3(2,n,n)
  real(c_double) :: emiss(2,n), source(2,n), int_source(2,n)
  integer :: j
  g0 = 0.0_c_double
  g1 = 0.0_c_double
  g2 = 0.0_c_double
  g3 = 0.0_c_double
  results = 0.0_c_double
  do j = 1,n
    g0(:,j,j) = -2.0_c_double*rates
    g1(:,j,j) = -rates
    emiss(:,j) = 300.0_c_double*rates
  end do
  if (band == 0) then
    call calc_matrices_lw_eig(2,n,4.0_c_double,g1,g2,emiss, &
      results(:,:,:,1),results(:,:,:,2),source,results(:,:,:,3),int_source)
    results(:,:,1,4) = source
    results(:,:,1,5) = int_source
  else
    call calc_matrices_sw_eig(2,n,n,4.0_c_double,0.5_c_double,g0,g1,g2,g3, &
      results(:,:,:,1),results(:,:,:,2),results(:,:,:,3),results(:,:,:,4), &
      results(:,:,:,5),results(:,:,:,6),results(:,:,:,7),results(:,:,:,8))
  end if
end subroutine matrices
""",
        encoding="ascii",
    )
    sources = [
        "utilities/parkind1.F90",
        "utilities/yomhook.F90",
        "radtool/radtool_matrix.F90",
        "radtool/radtool_eigen_decomposition.F90",
        "radtool/radtool_schur.F90",
        "radtool/radtool_calc_matrices_lw_eig.F90",
        "radtool/radtool_calc_matrices_sw_eig.F90",
    ]
    library = directory / "matrices.so"
    subprocess.run(
        [compiler, "-shared", "-fPIC", "-O0", "-fcheck=all",
         *[str(source_dir / source) for source in sources],
         str(wrapper), "-o", str(library)],
        cwd=directory, check=True, capture_output=True, text=True,
    )
    native = ctypes.CDLL(str(library)).matrices
    array_arg = np.ctypeslib.ndpointer(dtype=np.float64, flags="F_CONTIGUOUS")
    native.argtypes = [ctypes.c_int, ctypes.c_int, array_arg, array_arg]
    native.restype = None

    def run(band, n, rates):
        result = np.empty((2, n, n, 8), order="F")
        native(band, n, np.asfortranarray(rates, dtype=float), result)
        assert np.isfinite(result).all()
        return result

    return run


@pytest.mark.parametrize("n", [1, 4])
@pytest.mark.parametrize("rates", [[0.0, 0.0], [0.0, 0.3], [0.3, 0.0]])
@pytest.mark.parametrize("band", [0, 1], ids=["longwave", "shortwave"])
def test_transparent_and_absorbing_bands(matrix_solvers, band, n, rates):
    """Vacuum transmits all streams; adjacent absorbing bands obey Beer law."""
    actual = matrix_solvers(band, n, rates)
    expected = np.zeros_like(actual)
    for spectral, rate in enumerate(rates):
        transmission = np.exp(-rate * 4.0)
        expected[spectral, :, :, 1] = np.eye(n) * transmission
        if band == 0:
            integral = (1 - transmission) / rate if rate else 4.0
            expected[spectral, :, :, 2] = np.eye(n) * integral
            expected[spectral, :, 0, 3] = 300.0 * (1 - transmission)
            expected[spectral, :, 0, 4] = 600.0 * (4.0 - integral) if rate else 0.0
        else:
            expected[spectral, :, :, 4] = np.eye(n) * np.exp(-2 * rate * 4.0)
            if rate:
                expected[spectral, :, :, 5] = np.eye(n) / (2 * rate)
                expected[spectral, :, :, 6] = np.eye(n) / rate
    np.testing.assert_allclose(actual, expected, rtol=1e-12, atol=1e-12)
