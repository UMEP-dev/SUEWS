"""Ground absorption and building residuals through the compiled SPARTACUS routine."""

import gzip
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]

GROUND = [0, 2, 3, 4, 5, 6]
MIXED = np.array([0.25, 0.25, 0.15, 0.05, 0.2, 0.05, 0.05])
ALBEDO = np.array([0.1, 0.2, 0.35, 0.3, 0.2, 0.25, 0.05])
# Algebraic budgets at fixed temperatures, rather than observational comparisons.
FLUX_ATOL = 1e-8


@pytest.fixture(scope="module")
def ground_radiation(tmp_path_factory):
    """Compile a one-layer call to the full routine, with no copied physics."""
    compiler = shutil.which("gfortran")
    root = Path(__file__).resolve().parents[2]
    suews = root / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        pytest.skip("Native radiation probe requires gfortran and source-build libraries")
    try:
        with gzip.open(suews / "mod" / "module_phys_spartacus.mod", "rb") as stream:
            is_gnu = stream.readline().startswith(b"GFORTRAN")
    except gzip.BadGzipFile:
        is_gnu = False
    if not is_gnu:
        pytest.skip("Native radiation probe requires GNU Fortran module files")
    executable = tmp_path_factory.mktemp("ground_budget") / "probe"
    compiled = subprocess.run(
        [
            compiler, str(root / "test" / "fixtures" / "spartacus_ground_budget.f95"),
            "-I", str(suews / "mod"), "-L", str(suews / "lib"),
            "-lsuewsphys", "-lsuewsutil", "-lspartacus", "-fopenmp",
            "-o", str(executable),
        ],
        capture_output=True, text=True, check=False,
    )
    assert compiled.returncode == 0, compiled.stderr

    def run(fractions, albedo=ALBEDO, incoming=500.0, direct_fraction=0.6,
            direct_albedo=False, multiplier=1.0):
        rows = [
            " ".join(map(str, fractions)), " ".join(map(str, albedo)),
            f"{incoming} {direct_fraction} {int(direct_albedo)} {multiplier}",
        ]
        result = subprocess.run(
            [str(executable)], input="\n".join(rows) + "\n",
            capture_output=True, text=True, check=False,
        )
        if (
            fractions[1:4].sum() == 0
            and result.returncode != 0
            and "Program received signal SIGSEGV" in result.stderr
            and "__module_phys_spartacus_MOD_spartacus" in result.stderr
        ):
            pytest.xfail(
                "Flat-tile canopy-depth allocation segfault precedes radiation; "
                "fixed separately by #1854. Run the budget assertions when reachable."
            )
        assert result.returncode == 0, result.stdout + result.stderr
        values = np.fromstring(result.stdout, sep=" ")
        assert values.size == 14, result.stdout
        assert np.isfinite(values).all()
        return values

    return run


def test_uniform_flat_ground_surface_matches_bulk(ground_radiation):
    fractions = np.array([0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0])
    values = ground_radiation(fractions, albedo=np.full(7, 0.2))
    np.testing.assert_allclose(values[4], values[7], rtol=0, atol=FLUX_ATOL)
    np.testing.assert_allclose(values[8], 400.0, rtol=0, atol=FLUX_ATOL)


@pytest.mark.parametrize("direct_fraction", [0.0, 0.6, 1.0])
@pytest.mark.parametrize("direct_albedo,multiplier", [(False, 1.0), (True, 1.4)])
def test_mixed_ground_absorption_conserves_solver_budget(
    ground_radiation, direct_fraction, direct_albedo, multiplier
):
    values = ground_radiation(
        MIXED, direct_fraction=direct_fraction,
        direct_albedo=direct_albedo, multiplier=multiplier,
    )
    surface_qn, ground_sw, ground_lw = values[:7], values[8], values[9]
    # Equal surface temperatures/emissivities make the ground LW uniform.
    surface_sw = surface_qn[GROUND] - ground_lw / (1 - MIXED[1])
    np.testing.assert_allclose(
        surface_sw @ MIXED[GROUND], ground_sw, rtol=0, atol=FLUX_ATOL
    )
    # Ratios also retain each surface's direct/diffuse absorptivity, even
    # though the solver excludes trees when forming its effective albedo.
    direct, total = values[10:12]
    direct_alb = ALBEDO[GROUND] * (multiplier if direct_albedo else 1.0)
    absorbed = direct * (1 - direct_alb) + (total - direct) * (1 - ALBEDO[GROUND])
    np.testing.assert_allclose(
        surface_sw / surface_sw[0], absorbed / absorbed[0],
        rtol=1e-12, atol=1e-12,
    )


def test_building_residual_has_no_ground_shortwave_deficit(ground_radiation):
    values = ground_radiation(MIXED)
    expected_building = (values[7] - values[8] - values[9]) / MIXED[1]
    np.testing.assert_allclose(values[1], expected_building, rtol=0, atol=FLUX_ATOL)


@pytest.mark.parametrize("incoming,albedo", [(0.0, 0.2), (500.0, 0.0), (500.0, 1.0)])
def test_zero_shortwave_black_and_reflecting_ground(ground_radiation, incoming, albedo):
    values = ground_radiation(MIXED, albedo=np.full(7, albedo), incoming=incoming)
    ground_qn = values[GROUND] @ MIXED[GROUND]
    np.testing.assert_allclose(ground_qn, values[8] + values[9], rtol=0, atol=FLUX_ATOL)
