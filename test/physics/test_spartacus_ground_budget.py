"""Ground absorption and building residuals through the compiled SPARTACUS routine."""

import gzip
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]

GROUND = [0, 4, 5, 6]
TREES = [2, 3]
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
            direct_albedo=False, multiplier=1.0, expected_error=0, expected_warnings=0,
            allocation_only=False, ground_budget=0.0, roof_multiplier=1.0):
        rows = [
            " ".join(map(str, fractions)), " ".join(map(str, albedo)),
            f"{incoming} {direct_fraction} {int(direct_albedo)} {multiplier} {roof_multiplier}",
            f"{int(allocation_only)} {ground_budget}",
        ]
        result = subprocess.run(
            [str(executable)], input="\n".join(rows) + "\n",
            capture_output=True, text=True, check=False,
        )
        if (
            fractions[1:4].sum() == 0
            and result.returncode != 0
            and "Program received signal SIGSEGV" in result.stderr
        ):
            pytest.xfail(
                "Flat-tile canopy-depth allocation segfault precedes radiation; "
                "fixed separately by #1854. Run the budget assertions when reachable."
            )
        assert result.returncode == 0, result.stdout + result.stderr
        lines = result.stdout.splitlines()
        values = np.fromstring(lines[0], sep=" ")
        error_flag, error_code, warning_count = map(int, lines[1].split())
        assert error_flag == bool(expected_error), result.stdout
        assert error_code == expected_error, result.stdout
        assert warning_count == expected_warnings, result.stdout
        if expected_error:
            assert "SPARTACUS: effective direct" in lines[2]
            np.testing.assert_array_equal(values[:14], -999.0)
        if expected_warnings:
            assert "ground SW" in result.stdout
        assert values.size == 21, result.stdout
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
    # Ground downwelling is per grid area. The ground material mixture also
    # represents the ground beneath crowns, so divide by non-tree area.
    direct, total = values[10:12]
    direct_alb = ALBEDO[GROUND] * (multiplier if direct_albedo else 1.0)
    expected = (direct * (1 - direct_alb) + (total - direct) * (1 - ALBEDO[GROUND])) / MIXED[GROUND].sum()
    np.testing.assert_allclose(surface_sw, expected, rtol=0, atol=FLUX_ATOL)
    tree_sw = surface_qn[TREES] - ground_lw / (1 - MIXED[1])
    np.testing.assert_allclose(tree_sw, values[14] / MIXED[TREES].sum(), rtol=0, atol=FLUX_ATOL)


def test_building_residual_has_no_ground_shortwave_deficit(ground_radiation):
    values = ground_radiation(MIXED)
    expected_building = (values[7] - values[8] - values[9] - values[14]) / MIXED[1]
    np.testing.assert_allclose(values[1], expected_building, rtol=0, atol=FLUX_ATOL)


@pytest.mark.parametrize("incoming,albedo", [(0.0, 0.2), (500.0, 0.0), (500.0, 1.0)])
def test_zero_shortwave_black_and_reflecting_ground(ground_radiation, incoming, albedo):
    values = ground_radiation(MIXED, albedo=np.full(7, albedo), incoming=incoming)
    surface_sw = values[GROUND] - values[9] / (1 - MIXED[1])
    expected = values[11] / MIXED[GROUND].sum() if incoming > 0 and albedo == 0 else 0.0
    np.testing.assert_allclose(surface_sw, expected, rtol=0, atol=FLUX_ATOL)
    np.testing.assert_allclose(values[8], values[11] if albedo == 0 else 0.0, rtol=0, atol=FLUX_ATOL)
    if incoming == 0:
        np.testing.assert_allclose(values[14], 0.0, rtol=0, atol=FLUX_ATOL)


def test_tree_free_building_matches_independent_facet_budget(ground_radiation):
    fractions = np.array([0.45, 0.25, 0.0, 0.0, 0.2, 0.05, 0.05])
    values = ground_radiation(fractions)
    # Roof area equals building plan area; wall area is 4*h*f_b/scale
    # with height=scale=10 m. No trees or absorbing air remain above ground.
    facet_budget = fractions[1] * values[12] + 4 * fractions[1] * values[13]
    # Native LW integration leaves a measured 0.00211 W m-2 closure
    # residual in this fixed-temperature case. Allow 0.01 W m-2 of grid
    # (under five times that residual); test SW separately at round-off.
    np.testing.assert_allclose(values[1] * fractions[1], facet_budget, rtol=0, atol=0.01)
    np.testing.assert_allclose(
        values[15] - values[8], fractions[1] * values[17] + 4 * fractions[1] * values[18],
        rtol=0, atol=FLUX_ATOL,
    )
    np.testing.assert_allclose(values[19:21], 0.0, rtol=0, atol=FLUX_ATOL)


def test_tree_dominated_ground_and_canopy_are_distinct(ground_radiation):
    fractions = np.array([0.05, 0.25, 0.4, 0.3, 0.0, 0.0, 0.0])
    albedo = np.array([0.0, 0.2, 1.0, 1.0, 0.2, 0.3, 0.1])
    values = ground_radiation(fractions, albedo=albedo)
    lw = values[9] / 0.75
    # Black paving represents ALL physical ground, including beneath trees.
    # Its effective SUEWS area is 0.05, not the physical ground area 0.75.
    np.testing.assert_allclose(values[0] - lw, values[11] / 0.05, rtol=0, atol=FLUX_ATOL)
    np.testing.assert_allclose(values[TREES] - lw, values[14] / 0.7, rtol=0, atol=FLUX_ATOL)
    assert values[14] > 0  # tree surface albedo 1 does not replace leaf SSA
    changed = albedo.copy()
    changed[TREES] = 0.0
    comparison = ground_radiation(fractions, albedo=changed)
    np.testing.assert_allclose(values, comparison, rtol=0, atol=FLUX_ATOL)
    np.testing.assert_allclose(values[:7] @ fractions, values[7], rtol=0, atol=FLUX_ATOL)


def test_invalid_individual_direct_albedo_rejected_before_solver(ground_radiation):
    albedo = np.zeros(7)
    albedo[4] = 1.0
    assert albedo[GROUND] @ MIXED[GROUND] * 1.4 / MIXED[GROUND].sum() < 1
    ground_radiation(MIXED, albedo=albedo, direct_albedo=True, multiplier=1.4, expected_error=109)


@pytest.mark.parametrize("multiplier", [float("nan"), float("inf"), -1.0])
def test_nonphysical_direct_albedo_is_fatal(ground_radiation, multiplier):
    ground_radiation(MIXED, direct_albedo=True, multiplier=multiplier, expected_error=109)


@pytest.mark.parametrize("albedo,ground_budget,warnings", [(1.0, 0.0, 0), (1.0, 2.0, 1), (1.1, 2.0, 1)])
def test_unresolved_absorption_is_diagnosed(ground_radiation, albedo, ground_budget, warnings):
    # Exercise the mapping with a deliberately inconsistent solver budget;
    # physical reflecting ground must take the silent zero branch instead.
    values = ground_radiation(MIXED, albedo=np.full(7, albedo), allocation_only=True,
                             ground_budget=ground_budget, expected_warnings=warnings)
    np.testing.assert_allclose(values[GROUND], ground_budget / MIXED[GROUND].sum(), rtol=0, atol=FLUX_ATOL)


def test_pure_forest_understorey_mapping_warns(ground_radiation):
    fractions = np.array([0.0, 0.0, 0.4, 0.6, 0.0, 0.0, 0.0])
    values = ground_radiation(fractions, allocation_only=True, ground_budget=20.0, expected_warnings=1)
    np.testing.assert_allclose(values[TREES], 20.0, rtol=0, atol=FLUX_ATOL)


def test_invalid_direct_roof_albedo_is_fatal(ground_radiation):
    ground_radiation(MIXED, direct_albedo=True, roof_multiplier=6.0, expected_error=109)


def test_tree_albedo_is_not_a_direct_ground_albedo(ground_radiation):
    albedo = np.zeros(7)
    albedo[TREES] = 1.0
    values = ground_radiation(MIXED, albedo=albedo, direct_albedo=True, multiplier=1.4)
    tree_sw = values[TREES] - values[9] / (1 - MIXED[1])
    np.testing.assert_allclose(tree_sw, values[14] / MIXED[TREES].sum(), rtol=0, atol=FLUX_ATOL)
    assert np.all(tree_sw > 0)
