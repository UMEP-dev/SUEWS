"""Fatal native radiation failures must return to their host without using outputs."""

from concurrent.futures import ThreadPoolExecutor
import ctypes
from pathlib import Path
import shutil
import subprocess
from threading import Barrier

import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]


@pytest.fixture(scope="module")
def fatal_probe(tmp_path_factory):
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libspartacus.a").exists():
        pytest.skip("Native fatal-error probe requires gfortran and source-build libraries")
    directory = tmp_path_factory.mktemp("fatal_radiation")
    source = directory / "probe.f90"
    source.write_text(
        """program probe
use iso_c_binding, only: c_int
use radiation_io, only: radiation_abort
use radsurf_config, only: config_type
use radsurf_overlap, only: calc_overlap_matrices_urban
use radsurf_canopy_flux, only: canopy_flux_type
use radsurf_canopy_properties, only: canopy_properties_type
use radsurf_lw_spectral_properties, only: lw_spectral_properties_type
use radsurf_simple_spectrum, only: calc_simple_spectrum_lw
use radsurf_sw_spectral_properties, only: sw_spectral_properties_type
use radsurf_boundary_conds_out, only: boundary_conds_out_type
use radsurf_interface, only: radsurf
implicit none
interface
  subroutine reset_error() bind(C, name="suews_tls_error_reset")
  end subroutine
  function error_code() result(code) bind(C, name="suews_tls_error_code")
    import c_int
    integer(c_int) :: code
  end function
end interface
type(config_type) :: config
type(sw_spectral_properties_type) :: sw
type(boundary_conds_out_type) :: boundary
type(canopy_flux_type) :: lw_internal, lw_norm
type(canopy_flux_type) :: flux, other, unallocated_flux
type(canopy_properties_type) :: canopy
type(lw_spectral_properties_type) :: spectral
real(8) :: u(1,2,3), v(2,1,3)
character(24) :: mode
call get_command_argument(1, mode)
call reset_error()
select case(trim(mode))
case('abort')
  call radiation_abort('native host survives')
case('read')
  call config%read()
case('streams')
  config%n_stream_sw_urban = 0
  call config%consolidate()
case('regions')
  config%do_vegetation = .true.
  config%n_vegetation_region_urban = 3
  call config%consolidate()
case('overlap')
  call calc_overlap_matrices_urban(2, 1, reshape([0.8d0, 0.5d0], [1,2]), u, v, 1d-12)
case('scale')
  call flux%allocate(config, 1, 1, 1)
  flux%ground_dn = 123d0
  call flux%scale([1], reshape([1d0, 2d0], [2,1]))
  if (any(flux%ground_dn /= 123d0)) stop 2
case('sum')
  call unallocated_flux%sum(flux, other)
  if (allocated(unallocated_flux%ground_dn)) stop 3
case('radsurf')
  config%do_sw = .false.
  config%do_vegetation = .false.
  call config%consolidate()
  call canopy%allocate(config, 2, 4, [2,2])
  canopy%i_representation = 2
  canopy%istartlay = [1,3]
  canopy%nlay = 2
  canopy%building_fraction = [0.2d0, 0.5d0, 0.2d0, 0.1d0]
  call sw%allocate(config, 2, 4, 1, [2,2])
  call spectral%allocate(config, 1, 2, 4, [2,2])
  call boundary%allocate(2, 1, 1)
  boundary%lw_emissivity = 123d0
  boundary%lw_emission = 123d0
  call lw_internal%allocate(config, 2, 4, 1)
  call lw_norm%allocate(config, 2, 4, 1)
  lw_internal%ground_dn = 123d0
  call radsurf(config, canopy, sw, spectral, boundary, 1, 2, flux, other, lw_internal, lw_norm)
  ! The failed column must not write boundary results; the next column must
  ! not even start (its sentinel flux would otherwise be zeroed).
  if (any(boundary%lw_emissivity /= 123d0)) stop 7
  if (any(boundary%lw_emission /= 123d0)) stop 8
  if (any(lw_internal%ground_dn(:,2) /= 123d0)) stop 9
case('spectrum')
  call canopy%allocate(config, 1, 1)
  canopy%istartlay = 1
  canopy%nlay = 1
  call spectral%allocate(config, 2, 1, 1)
  spectral%ground_emission = 123d0
  call calc_simple_spectrum_lw(config, canopy, spectral, 1, 1)
  if (any(spectral%ground_emission /= 123d0)) stop 4
end select
if (error_code() /= 111) stop 5
call reset_error()
if (error_code() /= 0) stop 6
write(*,'(a)') 'host survived; TLS reset'
end program
""",
        encoding="ascii",
    )
    executable = directory / "probe"
    # Deliberately link only the vendor archive: it must own its TLS dependency.
    result = subprocess.run(
        [compiler, str(source), "-I", str(suews / "mod"), "-L", str(suews / "lib"),
         "-lspartacus", "-o", str(executable)], capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stderr
    return executable


@pytest.mark.parametrize("mode", ["abort", "read", "streams", "regions", "overlap", "scale", "sum", "spectrum", "radsurf"])
def test_native_fatal_returns_and_can_be_reset(fatal_probe, mode):
    result = subprocess.run([str(fatal_probe), mode], capture_output=True, text=True, check=False)
    assert result.returncode == 0, result.stdout + result.stderr
    assert "host survived; TLS reset" in result.stdout


def test_surface_resistance_fatal_initialises_outputs(tmp_path):
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        pytest.skip("Native resistance check requires gfortran and source-build libraries")
    source = tmp_path / "resistance.f90"
    source.write_text(
        """program resistance
use module_phys_resist, only: SurfaceResistance
use module_ctrl_error_state, only: reset_supy_error, get_supy_error
implicit none
real(8) :: gk, gq, gt, gsm, glai, gf, gs, rs
integer :: code
character(512) :: message
call reset_supy_error()
call SurfaceResistance(id=1, it=12, SMDMethod=0, SnowFrac=spread(0d0,1,7), &
  sfr_surf=spread(1d0/7d0,1,7), avkdn=500d0, Tair=20d0, dq=1d0, xsmd=1d0, vsmd=1d0, &
  MaxConductance=[1d0,1d0,1d0], LAIMax=[3d0,3d0,3d0], LAI_id=[1d0,1d0,1d0], &
  gsModel=-999, Kmax=1000d0, G_max=1d0, G_k=1d0, g_q_base=1d0, g_q_shape=1d0, &
  G_t=1d0, G_sm=1d0, TH=40d0, TL=0d0, S1=1d0, S2=1d0, &
  g_kdown=gk, g_dq=gq, g_ta=gt, g_smd=gsm, g_lai=glai, gfunc=gf, gsc=gs, RS=rs)
call get_supy_error(code, message)
if (code /= 71) stop 1
if (rs /= -999d0 .or. gs /= -999d0 .or. gf /= -999d0) stop 2
write(*,'(a)') 'resistance returned safe outputs'
end program
""", encoding="ascii",
    )
    executable = tmp_path / "resistance"
    subprocess.run(
        [compiler, str(source), "-I", str(suews / "mod"), "-L", str(suews / "lib"),
         "-lsuewsphys", "-lsuewsutil", "-lspartacus", "-o", str(executable)],
        check=True, capture_output=True, text=True,
    )
    result = subprocess.run([str(executable)], capture_output=True, text=True, check=False)
    assert result.returncode == 0, result.stdout + result.stderr
    assert "resistance returned safe outputs" in result.stdout


def test_radiation_tls_isolated_between_native_threads(tmp_path):
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libspartacus.a").exists():
        pytest.skip("Native TLS check requires gfortran and source-build libraries")
    source = tmp_path / "threads.f90"
    source.write_text(
        """subroutine native_abort() bind(C)
use radiation_io, only: radiation_abort
call radiation_abort('thread-local radiation failure')
end subroutine
""", encoding="ascii",
    )
    library_path = tmp_path / "threads.so"
    subprocess.run(
        [compiler, "-shared", "-fPIC", str(source), "-I", str(suews / "mod"),
         "-L", str(suews / "lib"), "-lspartacus", "-o", str(library_path)],
        check=True, capture_output=True, text=True,
    )
    library = ctypes.CDLL(str(library_path))
    library.native_abort.restype = None
    library.suews_tls_error_reset.restype = None
    library.suews_tls_error_code.restype = ctypes.c_int
    barrier = Barrier(2)

    def run(fail):
        library.suews_tls_error_reset()
        if fail:
            library.native_abort()
        barrier.wait(timeout=10)
        result = library.suews_tls_error_code()
        barrier.wait(timeout=10)
        library.suews_tls_error_reset()
        return result

    with ThreadPoolExecutor(max_workers=2) as pool:
        assert list(pool.map(run, [True, False])) == [111, 0]
