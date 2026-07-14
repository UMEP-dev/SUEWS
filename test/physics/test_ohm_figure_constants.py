"""Guard the OHM documentation figure against silent drift.

``docs/plot_ohm_transitions.py`` must depict what ``suews_phys_ohm.f95`` actually
does. The three transition widths are declared ``PARAMETER, PRIVATE`` in the
Fortran, so Python cannot import them and the script has to restate them. That
restatement is exactly what rots: if anyone retunes a width, the published figure
becomes wrong and nothing says so.

This test parses the PARAMETERs out of the Fortran and asserts the script's
module-level constants still agree. A figure that misrepresents the physics is
worse than no figure, so this failing is the point.
"""

import re
from pathlib import Path

import pytest

pytestmark = pytest.mark.physics

REPO_ROOT = Path(__file__).resolve().parents[2]
FORTRAN = REPO_ROOT / "src" / "suews" / "src" / "suews_phys_ohm.f95"
FIGURE_SCRIPT = REPO_ROOT / "docs" / "plot_ohm_transitions.py"

# Fortran PARAMETER name -> constant name in the figure script
CONSTANT_PAIRS = {
    "OHM_TEMP_TRANSITION_HALF_WIDTH": "TEMP_HALF_WIDTH",
    "OHM_SOIL_TRANSITION_HALF_WIDTH": "SOIL_HALF_WIDTH",
    "OHM_SURFACE_WETNESS_TRANSITION_WIDTH": "SURFACE_WETNESS_WIDTH",
}


def _fortran_parameter(name: str) -> float:
    """Read a REAL PARAMETER's value out of the OHM Fortran source."""
    pattern = rf"{name}\s*=\s*([0-9.]+)D0"
    match = re.search(pattern, FORTRAN.read_text(encoding="utf-8"))
    assert match is not None, f"PARAMETER {name} not found in {FORTRAN.name}"
    return float(match.group(1))


def _script_constant(name: str) -> float:
    """Read a module-level float constant out of the figure script."""
    pattern = rf"^{name}\s*=\s*([0-9.]+)"
    match = re.search(
        pattern, FIGURE_SCRIPT.read_text(encoding="utf-8"), flags=re.MULTILINE
    )
    assert match is not None, f"constant {name} not found in {FIGURE_SCRIPT.name}"
    return float(match.group(1))


@pytest.mark.parametrize(("fortran_name", "script_name"), CONSTANT_PAIRS.items())
def test_figure_constants_match_fortran(fortran_name, script_name):
    """The documentation figure must use the transition widths the model uses."""
    fortran_value = _fortran_parameter(fortran_name)
    script_value = _script_constant(script_name)
    assert script_value == pytest.approx(fortran_value), (
        f"{FIGURE_SCRIPT.name} uses {script_name}={script_value} but "
        f"{FORTRAN.name} declares {fortran_name}={fortran_value}. "
        "The published OHM transition figure now misrepresents the model. "
        "Update the script and regenerate docs/source/assets/img/ohm_coefficient_transitions.png."
    )
