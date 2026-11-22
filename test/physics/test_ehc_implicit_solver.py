"""Analytic checks of the implicit EHC conduction column (heatcond1d_seb_implicit).

The column's top face is closed by a linearised surface energy balance on a
zero-capacity surface node and is advanced by backward Euler. These tests call
the compiled routine through a small Fortran probe and compare it with closed
forms: a layered slab at steady state, an insulated slab whose heat content must
equal the energy that entered it, and a semi-infinite slab under a diurnal
sinusoid, refined in time and in space to measure the order of accuracy.
"""

import gzip
import os
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.contract("C-BALANCE", "C-RESPONSE", "C-NATIVE-ERROR"),
]

ROOT = Path(__file__).resolve().parents[2]


def _probe_unavailable(reason):
    if os.environ.get("SUEWS_REQUIRE_FORTRAN_PROBES") == "1":
        pytest.fail(reason)
    pytest.skip(reason)


@pytest.fixture(scope="module")
def probe(tmp_path_factory):
    """Compile the probe against the source-build physics library."""
    compiler = shutil.which("gfortran")
    suews = ROOT / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        _probe_unavailable("EHC solver probe requires gfortran and source-build libraries")
    try:
        with gzip.open(suews / "mod" / "module_phys_ehc_heatflux.mod", "rb") as stream:
            is_gnu = stream.readline().startswith(b"GFORTRAN")
    except (gzip.BadGzipFile, FileNotFoundError):
        is_gnu = False
    if not is_gnu:
        _probe_unavailable("EHC solver probe requires GNU Fortran module files")
    executable = tmp_path_factory.mktemp("ehc_implicit") / "probe"
    compiled = subprocess.run(
        [
            compiler, str(ROOT / "test" / "fixtures" / "ehc_implicit_probe.f95"),
            "-I", str(suews / "mod"), "-L", str(suews / "lib"),
            "-lsuewsphys", "-lsuewsutil", "-fopenmp", "-o", str(executable),
        ],
        capture_output=True, text=True, check=False,
    )
    assert compiled.returncode == 0, compiled.stderr

    def run(scenario, *params):
        stdin = f"{scenario}\n{' '.join(map(str, params))}\n"
        result = subprocess.run(
            [str(executable)], input=stdin, capture_output=True, text=True, check=False
        )
        assert result.returncode == 0, result.stdout + result.stderr
        values = {}
        for line in result.stdout.splitlines():
            key, value = line.split()
            values[key] = float(value)
        return values

    return run


def test_steady_layered_slab_carries_the_series_flux(probe):
    """At steady state q_top = q_bottom = the series-resistance flux, dU/dt = 0."""
    out = probe("steady", 7)
    assert out["q_top"] == pytest.approx(out["q_exact"], rel=1e-10)
    assert out["q_bottom"] == pytest.approx(out["q_exact"], rel=1e-10)
    assert abs(out["dudt"]) < 1e-9 * abs(out["q_exact"])
    assert out["ts"] == pytest.approx(out["ts_exact"], rel=1e-10)


def test_insulated_slab_stores_exactly_what_enters(probe):
    """With an insulated lower face, q_top = dU/dt every step and nothing leaks."""
    out = probe("insulated", 6, 2000)
    assert out["max_abs_qtop_minus_dudt"] < 1e-9
    assert out["max_abs_qbottom"] == 0.0
    # each surface node closes its linearised energy balance exactly
    assert out["max_abs_seb_residual"] < 1e-9
    # equal to accumulated round-off over 2000 steps
    assert out["energy_stored"] == pytest.approx(out["energy_in"], rel=1e-10)


def _errors(probe, n, dt, mode, h=20.0, depth=1.5, periods=2):
    out = probe("sine", n, depth, dt, periods, mode, h)
    return out["rel_rms_err_flux"], out["rel_rms_err_ts"]


@pytest.mark.parametrize("mode", [0, 1], ids=["surface-temperature", "air-temperature"])
def test_diurnal_wave_converges_first_order_in_time(probe, mode):
    """Backward Euler: halving dt halves the flux error once space is resolved."""
    dts = [1200.0, 600.0, 300.0, 150.0]
    errs = [_errors(probe, 400, dt, mode)[0] for dt in dts]
    orders = np.log2(np.array(errs[:-1]) / np.array(errs[1:]))
    assert np.all(orders > 0.85) and np.all(orders < 1.15), (errs, orders)
    # the SUEWS default step already resolves the diurnal wave to a few per cent
    assert errs[2] < 0.03


@pytest.mark.parametrize("mode", [0, 1], ids=["surface-temperature", "air-temperature"])
def test_diurnal_wave_converges_in_space(probe, mode):
    """Refining the layers reduces the flux error at second order (dt negligible)."""
    ns = [15, 30, 60]
    errs = [_errors(probe, n, 5.0, mode)[0] for n in ns]
    orders = np.log2(np.array(errs[:-1]) / np.array(errs[1:]))
    assert np.all(orders > 1.6), (errs, orders)


def test_water_keeps_crank_nicolson_column_in_implicit_coupling(probe):
    """Water is not given the implicit column: EHC runs heatcond1d_CN for it
    in both couplings and leaves its surface temperature to the driver."""
    out = probe("water")
    assert out["water_qs_implicit"] == out["water_qs_explicit"]
    assert out["water_qs_implicit"] == out["water_qs_cn"]
    assert out["water_temp_maxdiff_explicit_implicit"] == 0.0
    assert out["water_temp_maxdiff_cn"] == 0.0
    assert out["water_tsfc_implicit"] == out["water_tsfc_in"]
    # the other covered surface is solved implicitly: its Ts moves
    assert out["paved_tsfc_implicit"] != out["water_tsfc_in"]


@pytest.mark.parametrize("rsi", [0.0, 0.10, 0.13, 0.25])
@pytest.mark.parametrize("outer,inner", [(0.0, 20.0), (30.0, 20.0), (5.0, 5.0)])
def test_initial_profile_is_linear_in_series_resistance(probe, rsi, outer, inner):
    """Layer resistances are 0.10, 0.20, 0.02; cell centres at 0.05, 0.20, 0.31."""
    out = probe("profile", rsi, outer, inner)
    expected = outer + (inner - outer) * np.array([0.05, 0.20, 0.31]) / (0.32 + rsi)
    np.testing.assert_allclose([out[f"t{i}"] for i in (1, 2, 3)], expected, atol=1e-13, rtol=1e-13)


@pytest.mark.parametrize("mode", [0, 1], ids=["facets", "shared-slab-building"])
@pytest.mark.parametrize("roof,wall", [(0.10, 0.13), (0.0, 0.0), (0.25, 0.30)])
def test_indoor_robin_flux_matches_hand_calculated_steady_column(probe, mode, roof, wall):
    """20 K across exterior film 0.1, material 0.5 and the indoor film."""
    out = probe("indoor", mode, roof, wall)
    qr = 20.0 / (0.6 + roof)
    qw = 20.0 / (0.6 + wall)
    qi = qr if mode else 0.4 * qr + 0.6 * qw
    assert out["q_indoor"] == pytest.approx(qi, abs=1e-10)
    assert out["q_bottom"] == pytest.approx(-qi, abs=1e-10)
    assert abs(out["dudt"]) < 1e-9
    assert out["temp_change"] < 1e-12
    if not mode:
        assert out["qs_roof"] == pytest.approx(-qr, abs=1e-10)
        assert out["qs_wall"] == pytest.approx(-qw, abs=1e-10)


@pytest.mark.parametrize("mode", range(1, 9))
def test_invalid_solver_inputs_report_error_and_preserve_temperatures(probe, mode):
    out = probe("invalid", mode)
    assert out["code"] == 115
    assert out["temperature_change"] == 0.0
    assert out["unsafe_outputs"] == 0.0
