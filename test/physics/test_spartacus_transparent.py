"""Transparent transport, SW zero-absorption convention and band isolation."""

import ctypes
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]


@pytest.fixture(scope="module", params=["release", "checked"])
def matrix_solvers(tmp_path_factory, request):
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
        """subroutine matrices(band, nb, nd, ns, g0, g1, g2, g3, emiss, results) bind(C)
  use iso_c_binding, only: c_int, c_double
  use radtool_calc_matrices_lw_eig, only: calc_matrices_lw_eig
  use radtool_calc_matrices_sw_eig, only: calc_matrices_sw_eig
  implicit none
  integer(c_int), value :: band, nb, nd, ns
  real(c_double), intent(in) :: g0(nb,ns,ns), g1(nb,nd,nd), g2(nb,nd,nd)
  real(c_double), intent(in) :: g3(nb,nd,ns), emiss(nb,nd)
  real(c_double), intent(out) :: results(nb,max(nd,ns),max(nd,ns),8)
  real(c_double) :: source(nb,nd), int_source(nb,nd)
  results = 0.0_c_double
  if (band == 0) then
    call calc_matrices_lw_eig(nb,nd,4.0_c_double,g1,g2,emiss, &
      results(:,1:nd,1:nd,1),results(:,1:nd,1:nd,2),source, &
      results(:,1:nd,1:nd,3),int_source)
    results(:,1:nd,1,4) = source
    results(:,1:nd,1,5) = int_source
  else
    call calc_matrices_sw_eig(nb,nd,ns,4.0_c_double,0.5_c_double,g0,g1,g2,g3, &
      results(:,1:nd,1:nd,1),results(:,1:nd,1:nd,2), &
      results(:,1:nd,1:ns,3),results(:,1:nd,1:ns,4), &
      results(:,1:ns,1:ns,5),results(:,1:ns,1:ns,6), &
      results(:,1:nd,1:nd,7),results(:,1:nd,1:ns,8))
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
    # Match the numerical flags in Makefile.gfortran for each build profile.
    profile_flags = ["-O3"] if request.param == "release" else ["-O0", "-fcheck=all"]
    subprocess.run(
        [compiler, "-shared", "-fPIC", "-cpp", "-frecursive", "-fopenmp",
         "-fconvert=big-endian", "-ffree-line-length-none", "-finit-real=zero",
         *profile_flags, *[str(source_dir / source) for source in sources],
         str(wrapper), "-o", str(library)],
        cwd=directory, check=True, capture_output=True, text=True,
    )
    native = ctypes.CDLL(str(library)).matrices
    array_arg = np.ctypeslib.ndpointer(dtype=np.float64, flags="F_CONTIGUOUS")
    native.argtypes = [ctypes.c_int] * 4 + [array_arg] * 6
    native.restype = None

    def run(band, g0, g1, g2, g3, emission):
        nb, nd, _ = g1.shape
        ns = g0.shape[1]
        result = np.empty((nb, max(nd, ns), max(nd, ns), 8), order="F")
        inputs = [np.asfortranarray(a, dtype=float) for a in (g0, g1, g2, g3, emission)]
        native(band, nb, nd, ns, *inputs, result)
        assert np.isfinite(result).all()
        return result

    return run


def _exchange_matrices(rates, ndiff, ndir, scattering=False):
    """Build attenuating bands with optional reflection and stream coupling."""
    rates = np.asarray(rates)
    g0 = -rates[:, None, None] * np.diag(np.linspace(0.7, 0.9, ndir))
    g1 = -rates[:, None, None] * np.diag(np.linspace(0.3, 0.5, ndiff))
    g2 = np.zeros_like(g1)
    g3 = np.zeros((len(rates), ndiff, ndir))
    if scattering:
        coupling = np.full((ndiff, ndiff), 0.01)
        np.fill_diagonal(coupling, 0.0)
        g1 += rates[:, None, None] * coupling
        g2 = rates[:, None, None] * (0.04 * np.eye(ndiff) + 0.01)
        g3 = rates[:, None, None] * np.full((ndiff, ndir), 0.03 / ndiff)
    emission = np.broadcast_to(90.0 * rates[:, None], (len(rates), ndiff)).copy()
    return g0, g1, g2, g3, emission


@pytest.mark.parametrize("n", [1, 4])
@pytest.mark.parametrize("rates", [[0.0, 0.0], [0.0, 0.3], [0.3, 0.0]])
@pytest.mark.parametrize("band", [0, 1], ids=["longwave", "shortwave"])
def test_transparent_and_absorbing_bands(matrix_solvers, band, n, rates):
    """Check Beer law and vacuum transport, with the SW absorption convention."""
    rates_array = np.asarray(rates)
    g1 = -rates_array[:, None, None] * np.eye(n)
    emission = np.broadcast_to(300.0 * rates_array[:, None], (2, n))
    actual = matrix_solvers(band, 2 * g1, g1, np.zeros_like(g1), np.zeros_like(g1), emission)
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
            # At zero extinction the SW coefficients stay zero for absorption;
            # they do not represent the analytical integrated-flux limit.
    np.testing.assert_allclose(actual, expected, rtol=1e-12, atol=1e-12)


@pytest.mark.parametrize("ndiff,ndir", [(1, 1), (2, 3), (4, 2)])
@pytest.mark.parametrize("scattering", [False, True], ids=["absorbing", "scattering"])
@pytest.mark.parametrize("band", [0, 1], ids=["longwave", "shortwave"])
def test_ordinary_bands_independent_of_transparent_neighbours(
    matrix_solvers, band, ndiff, ndir, scattering
):
    """Vacuum preserves single-band bits and ordinary batching to round-off."""
    ordinary = _exchange_matrices([1.0, 1.4], ndiff, ndir, scattering)
    batched = matrix_solvers(band, *ordinary)
    single_results = []
    for index in range(2):
        alone = matrix_solvers(band, *(a[index:index + 1] for a in ordinary))
        single_results.append(alone[0])
    single_results = np.asarray(single_results)
    # Production -O3 gives different last bits for ordinary multi-band and
    # single-band execution. Allow double-precision round-off in that comparison,
    # while requiring bit identity against single-band execution below.
    np.testing.assert_allclose(single_results, batched, rtol=1e-13, atol=1e-13)
    for position in (0, 1, 2):
        mixed_inputs = [np.insert(a, position, 0.0, axis=0) for a in ordinary]
        mixed = matrix_solvers(band, *mixed_inputs)
        ordinary_results = np.delete(mixed, position, axis=0)
        # Byte comparison deliberately includes signed zeros and uses no tolerance.
        assert ordinary_results.tobytes() == single_results.tobytes()
        np.testing.assert_allclose(ordinary_results, batched, rtol=1e-13, atol=1e-13)
        vacuum = np.zeros_like(mixed[position])
        vacuum[:ndiff, :ndiff, 1] = np.eye(ndiff)
        if band == 0:
            vacuum[:ndiff, :ndiff, 2] = 4.0 * np.eye(ndiff)
        else:
            vacuum[:ndir, :ndir, 4] = np.eye(ndir)
        np.testing.assert_array_equal(mixed[position], vacuum)
    if scattering:
        assert np.any(batched[:, :ndiff, :ndiff, 0] > 0)
        if band == 1:
            assert np.any(batched[:, :ndiff, :ndir, 2:4] > 0)


@pytest.mark.parametrize("n", [1, 4])
def test_transparent_longwave_with_independent_emission(matrix_solvers, n):
    """A constant source gives b*dz emerging and b*dz**2 integrated flux."""
    inputs = list(_exchange_matrices([0.0, 1.0], n, n))
    # Deliberately independent of extinction, including unequal stream sources.
    inputs[-1][:] = np.arange(1, n + 1) * 7.0
    actual = matrix_solvers(0, *inputs)
    expected = np.zeros_like(actual[0])
    expected[:, :, 1] = np.eye(n)
    expected[:, :, 2] = 4.0 * np.eye(n)
    expected[:, 0, 3] = inputs[-1][0] * 4.0
    expected[:, 0, 4] = inputs[-1][0] * 16.0
    np.testing.assert_allclose(actual[0], expected, rtol=1e-12, atol=1e-12)
