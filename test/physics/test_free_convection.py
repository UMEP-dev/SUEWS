"""Free-convection velocity in the aerodynamic resistance.

On the unstable branch, r_a uses a free-convection velocity
w_f = (g/T H zzd)**(1/3) from the pass heat flux: the momentum and stability
parts use the effective wind U_e = sqrt(U**2 + w_f**2), and the interfacial
part ln(z0m/z0v)/(k u*) uses u*_I = sqrt(u*_e**2 + w_f**2) with
u*_e = u* U_e/U. Stable and neutral steps are unchanged, and u* passed to the
rest of the model is unchanged. Between outer passes w_f is under-relaxed with
the (z-d)/L schedule. The previous pass's value is a local of the timestep
call that owns the outer loop, reset to zero at the start of every timestep:
it is never in a saved local, a module variable or the carried state, so
grids run together give the same output as each grid alone.
"""

import copy
import gzip
from importlib import import_module
import json
import os
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

import supy

pytestmark = [pytest.mark.physics, pytest.mark.core]

_run_rust = import_module("supy._run_rust")

GRAV = float(np.float32(9.80665))  # single-precision literal in the Fortran
C2K = float(np.float32(273.15))  # single-precision literal in the Fortran
K2 = float(np.float32(0.16))  # single-precision literal in the Fortran
ZL_RELAX = 0.3  # relax_zl factor for the first passes
ZL_RELAX_PASSES = 10  # passes at the full factor
ZL_RELAX_DECAY = 0.7  # per-pass decay after that
MAX_OUTER_PASSES = 60  # max_iter_default in the driver
RA_FLOOR = 10.0  # retained lower limit [s m-1]
ROUNDOFF_RTOL = 1e-12

# A rough urban site (the sample's geometry, rounded).
ZZD, Z0M, VEG_FRACTION = 25.8, 1.9, 0.3
ROUGH_LEN_HEAT_METHOD = 2  # Kawai et al. (2007), the sample's setting

PROBE = """program probe
use module_phys_resist, only: AerodynamicResistance
use module_phys_atmmoiststab, only: free_convection_velocity, relax_free_convection, &
                                    stab_psi_mom, stab_psi_heat
use module_ctrl_type, only: SUEWS_STATE
use module_ctrl_const_datain, only: SuppressWarnings
implicit none
integer :: mode, n, i, meth, rlh
real(kind(1d0)) :: a, b, c, d, e, f, g, ra, z0v, psim, psih
type(SUEWS_STATE) :: state
SuppressWarnings = 0
read(*, *) mode, n
do i = 1, n
  if (mode == 1) then
    ! kinematic heat flux, air temperature, zzd
    read(*, *) a, b, c
    write(*, '(ES26.17E3)') free_convection_velocity(a, b, c)
  else if (mode == 2) then
    ! previous w_f, new w_f, pass number, relaxation flag (1 or 0)
    read(*, *) a, b, c, d
    write(*, '(ES26.17E3)') relax_free_convection(a, b, nint(c), d > 0.5d0)
  else
    ! zzd, z0m, U, L, u*, w_f, VegFraction, stability method, roughness-length method
    read(*, *) a, b, c, d, e, f, g, meth, rlh
    call AerodynamicResistance(a, b, c, d, e, f, g, 2, meth, rlh, ra, z0v, state)
    psim = stab_psi_mom(meth, a/d) - stab_psi_mom(meth, b/d)
    psih = stab_psi_heat(meth, a/d) - stab_psi_heat(meth, z0v/d)
    write(*, '(4ES26.17E3)') ra, z0v, psim, psih
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
        _probe_unavailable(
            "Native probe needs gfortran and source-build libraries; run tests cover wheels"
        )
    try:
        with gzip.open(suews / "mod" / "module_phys_resist.mod", "rb") as stream:
            is_gnu = stream.readline().startswith(b"GFORTRAN")
    except (gzip.BadGzipFile, FileNotFoundError):
        is_gnu = False
    if not is_gnu:
        _probe_unavailable("Native probe needs GNU Fortran module files")
    directory = tmp_path_factory.mktemp("free_convection_probe")
    source = directory / "probe.f95"
    source.write_text(PROBE, encoding="ascii")
    executable = directory / "probe"
    compiled = subprocess.run(
        [
            compiler,
            str(source),
            "-I",
            str(suews / "mod"),
            "-L",
            str(suews / "lib"),
            "-lsuewsphys",
            "-lsuewsutil",
            "-lspartacus",
            "-fopenmp",
            "-o",
            str(executable),
        ],
        check=False,
        capture_output=True,
        text=True,
    )
    assert compiled.returncode == 0, compiled.stderr

    def run(mode, rows):
        lines = [f"{mode} {len(rows)}"]
        for row in rows:
            lines.append(
                " ".join(str(v) if isinstance(v, int) else repr(float(v)) for v in row)
            )
        result = subprocess.run(
            [str(executable)],
            input="\n".join(lines) + "\n",
            check=True,
            capture_output=True,
            text=True,
        )
        values = np.array([float(v) for v in result.stdout.split()])
        return values if mode != 3 else values.reshape(-1, 4)

    return run


def _resistance_cases(z_over_l, w_f, method):
    """Rows for probe mode 3 over wind speeds; u* from the log profile."""
    rows = []
    for wind in (0.3, 0.8, 1.5):
        ustar = 0.4 * wind / np.log(ZZD / Z0M)
        for zl in z_over_l:
            for wf in w_f:
                rows.append([
                    ZZD,
                    Z0M,
                    wind,
                    ZZD / zl,
                    ustar,
                    wf,
                    VEG_FRACTION,
                    method,
                    ROUGH_LEN_HEAT_METHOD,
                ])
    return np.array(rows, dtype=object)


def _pr_resistance(rows, out):
    """The resistance without free convection, from the probe's z0v and psi."""
    wind = rows[:, 2].astype(float)
    z0v, psim, psih = out[:, 1], out[:, 2], out[:, 3]
    return (np.log(ZZD / Z0M) - psim) * (np.log(ZZD / z0v) - psih) / (K2 * wind)


@pytest.mark.parametrize("method", [2, 3, 4])
def test_zero_free_convection_gives_the_formula_without_it(probe, method):
    # ARRANGE: unstable and stable (z-d)/L, including the near-neutral +-1e-12
    rows = _resistance_cases(
        [-2.0, -0.5, -0.05, -1e-12, 1e-12, 0.1, 0.5], [0.0], method
    )

    # ACT
    out = probe(3, rows)

    # ASSERT
    assert out[:, 0].min() > RA_FLOOR
    np.testing.assert_allclose(
        out[:, 0], _pr_resistance(rows, out), rtol=ROUNDOFF_RTOL, atol=0
    )


@pytest.mark.parametrize("method", [2, 3, 4])
def test_free_convection_term_follows_its_equation(probe, method):
    # ARRANGE
    rows = _resistance_cases([-2.0, -0.5, -0.05], [1e-9, 0.05, 0.3, 1.0], method)

    # ACT
    out = probe(3, rows)

    # ASSERT: momentum and stability parts scale by U/U_e, the interfacial
    # part by u*/u*_I, and z0v keeps the original u*
    wind = rows[:, 2].astype(float)
    ustar = rows[:, 4].astype(float)
    w_f = rows[:, 5].astype(float)
    z0v, psim = out[:, 1], out[:, 2]
    wind_eff = np.sqrt(wind**2 + w_f**2)
    ustar_int = np.sqrt((ustar * wind_eff / wind) ** 2 + w_f**2)
    interfacial = np.log(Z0M / z0v) * (np.log(ZZD / Z0M) - psim) / (K2 * wind)
    expected = _pr_resistance(rows, out) * wind / wind_eff - interfacial * (
        wind / wind_eff - ustar / ustar_int
    )
    np.testing.assert_allclose(
        out[:, 0], np.maximum(expected, RA_FLOOR), rtol=ROUNDOFF_RTOL, atol=0
    )


@pytest.mark.parametrize("method", [2, 3, 4])
def test_free_convection_lowers_unstable_resistance_and_vanishes_smoothly(
    probe, method
):
    # ARRANGE: w_f increasing within each (wind, (z-d)/L) block
    w_f = [0.0, 1e-9, 0.05, 0.3, 1.0]
    rows = _resistance_cases([-2.0, -0.5, -0.05, -1e-12], w_f, method)

    # ACT
    ra = probe(3, rows)[:, 0].reshape(-1, len(w_f))

    # ASSERT: never above the resistance without it, non-increasing in w_f,
    # and continuous as w_f -> 0 (the change is second order in w_f); the
    # tolerance admits round-off only
    roundoff = 1e-14 * ra[:, :1]
    assert np.all(ra <= ra[:, :1] + roundoff)
    assert np.all(np.diff(ra, axis=1) <= roundoff)
    np.testing.assert_allclose(ra[:, 1], ra[:, 0], rtol=1e-14, atol=0)
    above_floor = ra[:, 3] > RA_FLOOR
    assert above_floor.sum() >= len(ra) // 2
    assert np.all(ra[above_floor, 3] < ra[above_floor, 0])


@pytest.mark.parametrize("method", [2, 3, 4])
def test_stable_and_neutral_resistance_ignore_free_convection(probe, method):
    # ARRANGE: stable (z-d)/L, and the neutral equation (u* = 0)
    stable = _resistance_cases([1e-12, 0.1, 0.5], [0.0, 0.3, 1.0], method)
    neutral = stable.copy()
    neutral[:, 4] = 0.0

    # ACT / ASSERT: bit-identical whatever w_f is
    for rows in (stable, neutral):
        ra = probe(3, rows)[:, 0].reshape(-1, 3)
        np.testing.assert_array_equal(ra, np.repeat(ra[:, :1], 3, axis=1))


def test_free_convection_velocity(probe):
    # ARRANGE: kinematic heat flux [K m s-1], air temperature [degC], zzd [m]
    rows = np.array([
        [0.1, 20.0, 25.8],
        [0.002, 5.0, 10.0],
        [0.3, 35.0, 40.0],
        [0.0, 20.0, 25.8],
        [-0.05, 20.0, 25.8],
    ])

    # ACT
    w_f = probe(1, rows)

    # ASSERT: (g/T H zzd)**(1/3) for upward flux, zero otherwise
    expected = np.cbrt(
        np.maximum(0.0, GRAV / (rows[:, 1] + C2K) * rows[:, 0] * rows[:, 2])
    )
    np.testing.assert_allclose(w_f, expected, rtol=ROUNDOFF_RTOL, atol=0)
    np.testing.assert_array_equal(w_f[3:], [0.0, 0.0])


def test_relaxed_free_convection_follows_the_stability_schedule(probe):
    # ARRANGE: a unit step from w_f = 0 to 1 at every pass number
    passes = np.arange(1, MAX_OUTER_PASSES + 1, dtype=float)
    relaxed = np.column_stack([
        np.zeros_like(passes),
        np.ones_like(passes),
        passes,
        np.ones_like(passes),
    ])
    unrelaxed = relaxed.copy()
    unrelaxed[:, 3] = 0.0

    # ACT
    step = probe(2, relaxed)
    step_unrelaxed = probe(2, unrelaxed)

    # ASSERT: pass 1 and the undamped call take the new value; later passes
    # move by the (z-d)/L factor, which decays after ZL_RELAX_PASSES
    factor = np.where(
        passes <= ZL_RELAX_PASSES,
        ZL_RELAX,
        ZL_RELAX * ZL_RELAX_DECAY ** (passes - ZL_RELAX_PASSES),
    )
    expected = np.where(passes == 1, 1.0, factor)
    np.testing.assert_allclose(step, expected, rtol=1e-14, atol=0)
    np.testing.assert_array_equal(step_unrelaxed, np.ones_like(passes))


def test_relaxed_free_convection_keeps_fixed_points(probe):
    # ARRANGE: previous and new values in both directions, and an equal pair
    rows = np.array(
        [[0.4, 0.1, 2, 1], [0.1, 0.4, 5, 1], [0.25, 0.25, 30, 1], [0.0, 0.7, 12, 1]],
        dtype=float,
    )

    # ACT
    out = probe(2, rows)

    # ASSERT
    np.testing.assert_array_equal(out[2], 0.25)
    assert np.all(out >= np.minimum(rows[:, 0], rows[:, 1]))
    assert np.all(out <= np.maximum(rows[:, 0], rows[:, 1]))


# --- model level ------------------------------------------------------------

LEAK_STEPS = 288 * 7  # one week at the sample 5-minute timestep
# Measurement heights that make the grids differ, so a grid reading another
# grid's w_f would depart from its own run.
LEAK_GRID_Z = [40.0, 45.0, 50.0, 55.0]
LEAK_REPEATS = 5


@pytest.fixture(scope="module")
def leak_inputs():
    sim = supy.SUEWSSimulation.from_sample_data()
    df_forcing = sim._df_forcing.iloc[:LEAK_STEPS]
    return {
        "rust": _run_rust._check_rust_available(),
        "config_dict": sim.config.model_dump(exclude_none=True, mode="json"),
        "forcing_flat": _run_rust
        ._prepare_forcing_block(df_forcing)
        .ravel(order="C")
        .tolist(),
        "len_sim": len(df_forcing),
    }


def _grid_config(config_dict, gridiv, z):
    site = copy.deepcopy(config_dict["sites"][0])
    site["gridiv"] = gridiv
    site["properties"]["z"] = {"value": z}
    grid_dict = dict(config_dict)
    grid_dict["sites"] = [site]
    return json.dumps(grid_dict)


def _run_grids(inputs, grids, max_workers):
    """Run (gridiv, z) grids together; output blocks keyed by grid id."""
    configs = [_grid_config(inputs["config_dict"], gridiv, z) for gridiv, z in grids]
    results = inputs["rust"].run_suews_multi(
        configs, inputs["forcing_flat"], inputs["len_sim"], max_workers
    )
    # each result starts with the grid's position in the input list
    return {grids[result[0]][0]: bytes(result[1]) for result in results}


def test_free_convection_carry_does_not_leak_between_grids(leak_inputs):
    """Each grid in a run of four, in parallel and serially and in either
    order, gives the output of that grid run alone: the previous-pass w_f is
    local to each grid's timestep call, so no grid can read another's."""
    # ARRANGE: every grid on its own
    grids = list(enumerate(LEAK_GRID_Z, start=1))
    alone = {}
    for grid in grids:
        alone.update(_run_grids(leak_inputs, [grid], max_workers=1))
    assert len(set(alone.values())) == len(alone), "grids gave identical output"

    # ACT / ASSERT
    for max_workers, repeats in ((1, 1), (4, LEAK_REPEATS)):
        for _ in range(repeats):
            for order in (grids, grids[::-1]):
                assert _run_grids(leak_inputs, order, max_workers) == alone


def test_sample_year_converges_with_free_convection(sample_run_cached):
    """The w_f feedback (QH -> w_f -> r_a -> QE -> QH) two-cycles across
    QH = 0 when undamped; relaxed, no sample step reaches the pass limit."""
    # ARRANGE / ACT
    df_output, _ = sample_run_cached(None)

    # ASSERT
    assert (df_output["debug", "iter"] < MAX_OUTER_PASSES).all()
    assert (df_output["SUEWS", "QH"] * df_output["SUEWS", "zL"] <= 0).all()
