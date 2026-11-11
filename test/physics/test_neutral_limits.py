"""Analytical neutral limits of the native stability, resistance and MOST routines."""

import gzip
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]


@pytest.fixture(scope="module")
def neutral_probe(tmp_path_factory):
    """Link the real kernel, trapping invalid operations and division by zero."""
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        pytest.skip("Native neutral checks require gfortran and source-build libraries")
    try:
        with gzip.open(suews / "mod" / "module_phys_atmmoiststab.mod", "rb") as stream:
            is_gnu = stream.readline().startswith(b"GFORTRAN")
    except gzip.BadGzipFile:
        is_gnu = False
    if not is_gnu:
        pytest.skip("Native neutral harness needs GNU Fortran modules")
    directory = tmp_path_factory.mktemp("neutral")
    source = directory / "probe.f95"
    source.write_text(
        """program probe
use module_phys_atmmoiststab, only: cal_Stab
use module_phys_resist, only: AerodynamicResistance
use module_phys_rslprof, only: RSLProfile
use module_ctrl_const_physconst, only: eps_fp
use module_ctrl_const_allocate, only: ncolumnsDataOutRSL
use module_ctrl_type, only: SUEWS_CONFIG, SUEWS_TIMER, SUEWS_FORCING, SUEWS_SITE, SUEWS_STATE
use module_ctrl_error_state, only: reset_supy_error, get_supy_error
implicit none
type(SUEWS_CONFIG) :: config
type(SUEWS_TIMER) :: timer
type(SUEWS_FORCING) :: forcing
type(SUEWS_SITE) :: site
type(SUEWS_STATE) :: state
integer :: selector, method, option, code
real(kind(1d0)) :: factor, wind, length, tstar, ustar, zl, ra, z0v
real(kind(1d0)) :: profile(ncolumnsDataOutRSL - 5)
character(len=512) :: message
read(*, *) selector, method, option, factor, wind
call reset_supy_error()
select case(selector)
case(1)
  ! factor scales the kinematic heat-flux threshold used by cal_Stab.
  call cal_Stab(method, 20d0, 1d0, 2d0, wind, 20d0, &
                factor*eps_fp*1200d0, 1.2d0, 1000d0, length, tstar, ustar, zl)
  write(*, '(*(ES24.16,1X))') length, tstar, ustar, zl
case(2)
  call AerodynamicResistance(20d0, 1d0, wind, factor*eps_fp, 0.4d0, &
                             0.5d0, option, method, 1, ra, z0v)
  write(*, '(*(ES24.16,1X))') ra, z0v
case(3)
  config%StabilityMethod = method
  config%RSLMethod = option
  site%z = 22d0
  site%z0m_in = 1d0
  site%zdm_in = 2d0
  forcing%U = wind
  forcing%Temp_C = 20d0
  forcing%RH = 50d0
  forcing%pres = 1013d0
  state%roughnessState%z0m = 1d0
  state%roughnessState%zdm = 2d0
  state%roughnessState%z0v = 0.1d0
  state%roughnessState%Zh = 1d0
  state%roughnessState%PAI = 0d0
  state%roughnessState%FAI = 0d0
  state%atmState%L_MOD = factor*eps_fp
  state%atmState%RA_h = 50d0
  state%atmState%av_density = 1.2d0
  state%atmState%av_cp = 1000d0
  state%atmState%lv_J_kg = 2.45d6
  state%heatState%qh = 20d0
  state%heatState%qe = 40d0
  call RSLProfile(timer, config, forcing, site, state, profile)
  write(*, '(*(ES24.16,1X))') profile
end select
call get_supy_error(code, message)
write(*, '(I0)') code
write(*, '(A)') trim(message)
end program
""",
        encoding="ascii",
    )
    executable = directory / "probe"
    subprocess.run(
        [compiler, str(source), "-I", str(suews / "mod"),
         "-L", str(suews / "lib"), "-lsuewsphys", "-lsuewsutil",
         "-lspartacus", "-fopenmp", "-ffpe-trap=invalid,zero,overflow",
         "-o", str(executable)],
        check=True, capture_output=True, text=True,
    )

    def run(selector, method, option, factor, wind=3.0):
        result = subprocess.run(
            [str(executable)], input=f"{selector} {method} {option} {factor} {wind}\n",
            check=True, capture_output=True, text=True,
        )
        lines = result.stdout.splitlines()
        assert int(lines[1]) == 0, result.stdout
        values = np.fromstring(lines[0], sep=" ")
        assert np.isfinite(values).all()
        return values

    return run


@pytest.mark.parametrize("method", [2, 3, 4, 5])
@pytest.mark.parametrize("factor", [0.0, -0.5, 0.5, -1.0, 1.0])
@pytest.mark.parametrize("wind", [0.001, 3.0])
def test_stability_neutral_log_law(neutral_probe, method, factor, wind):
    actual = neutral_probe(1, method, 0, factor, wind)
    # Legacy k is a default-real literal promoted to double; retain its value.
    kappa = float(np.float32(0.4))
    expected = [0.0, 0.0, max(float(np.float32(0.001)), kappa * wind / np.log(20)), 0.0]
    np.testing.assert_allclose(actual, expected, rtol=2e-14, atol=0)


@pytest.mark.parametrize("factor", [-1e30, 1e30])
def test_stability_near_zero_computed_length(neutral_probe, factor):
    """Synthetic large heat flux exercises the small computed-length guard."""
    actual = neutral_probe(1, 3, 0, factor)
    eps_fp = float(np.float32(1e-12))
    kappa = float(np.float32(0.4))
    gravity = float(np.float32(9.80665))
    kelvin_offset = float(np.float32(273.15))
    ustar = kappa * 3 / np.log(20)
    tstar = -(factor * eps_fp) / ustar
    length = ustar**2 / ((gravity / (20 + kelvin_offset)) * kappa * tstar)
    assert 0 < abs(actual[0]) <= eps_fp
    np.testing.assert_allclose(actual, [length, tstar, ustar, 0], rtol=2e-14, atol=0)


@pytest.mark.parametrize("method", [2, 3, 4, 5])
@pytest.mark.parametrize("factor", [0.0, -0.5, 0.5, -1.0, 1.0])
@pytest.mark.parametrize("option", [1, 2, 3])
def test_resistance_neutral_limit(neutral_probe, method, factor, option):
    ra, z0v = neutral_probe(2, method, option, factor)
    k2 = float(np.float32(0.16))
    expected = {
        1: np.log(20)**2 / (k2 * 3),
        2: np.log(20) * np.log(20 / z0v) / (k2 * 3),
        3: float(np.float32(4.72)) * np.log(20)**2 / (1 + float(np.float32(0.54)) * 3),
    }[option]
    np.testing.assert_allclose(ra, np.clip(expected, 10, 120), rtol=2e-14, atol=0)


@pytest.mark.parametrize("method", [2, 3, 4, 5])
@pytest.mark.parametrize("factor", [0.0, -0.5, 0.5, -1.0, 1.0])
@pytest.mark.parametrize("option", [0, 2])
@pytest.mark.parametrize("wind_speed", [0.1, 3.0])
def test_most_profile_neutral_limit(neutral_probe, method, factor, option, wind_speed):
    """Both forced MOST and automatic MOST fallback obey the log-law profiles."""
    profile = neutral_probe(3, method, option, factor, wind=wind_speed)
    z, wind, temperature, humidity = profile[:120].reshape(4, 30)
    kappa = float(np.float32(0.4))
    expected_wind = wind_speed * np.log((z - 2) / 1) / np.log(20)
    ustar_heat = np.log(20 / 0.1) / (kappa * 50)
    scale = np.log((z - 2) / 20) / kappa
    expected_temperature = 20 - scale * (20 / 1200) / ustar_heat
    expected_humidity_delta = -scale * (40 / (2.45e6 * 1.2)) / ustar_heat * 1000
    # Floating-point algebra only: no observational tolerance is needed.
    np.testing.assert_allclose(wind, expected_wind, rtol=2e-14, atol=1e-14)
    np.testing.assert_allclose(temperature, expected_temperature, rtol=2e-14, atol=1e-14)
    np.testing.assert_allclose(humidity - humidity[-1], expected_humidity_delta, rtol=2e-13, atol=2e-14)
