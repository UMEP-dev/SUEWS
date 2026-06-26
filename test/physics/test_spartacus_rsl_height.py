"""Regression tests for SPARTACUS vegetation RSL sampling heights."""

from pathlib import Path
import re

import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]

SPARTACUS_SOURCE = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "suews"
    / "src"
    / "suews_phys_spartacus.f95"
)


def _fortran_logical_lines(text: str) -> list[str]:
    """Return Fortran statements with continuation markers folded away."""
    statements: list[str] = []
    current: list[str] = []

    for raw_line in text.splitlines():
        line = raw_line.split("!", maxsplit=1)[0].strip()
        if not line:
            continue

        if line.startswith("&"):
            line = line[1:].lstrip()

        continued = line.endswith("&")
        if continued:
            line = line[:-1].rstrip()

        current.append(line)

        if not continued:
            statements.append(" ".join(current))
            current = []

    if current:
        statements.append(" ".join(current))

    return statements


def _mixed_tree_veg_air_height_expression() -> str:
    source = SPARTACUS_SOURCE.read_text(encoding="utf-8")
    statements = _fortran_logical_lines(source)

    for idx, statement in enumerate(statements):
        if re.fullmatch(r"IF\s*\(\s*use_evetr\s*\.AND\.\s*use_dectr\s*\)\s*THEN", statement, re.IGNORECASE):
            assignment = statements[idx + 1]
            break
    else:
        pytest.fail("Could not find the mixed evergreen/deciduous SPARTACUS branch")

    match = re.fullmatch(r"veg_air_height\s*=\s*(?P<expr>.+)", assignment, re.IGNORECASE)
    if not match:
        pytest.fail("Mixed-tree branch does not assign veg_air_height first")

    return match.group("expr")


def _eval_fortran_expr(expr: str, **values: float) -> float:
    python_expr = re.sub(r"(?<=\d)D(?=[+-]?\d)", "e", expr, flags=re.IGNORECASE)
    return eval(python_expr, {"__builtins__": {}}, values)


@pytest.mark.parametrize(
    (
        "sfr_evetr_use",
        "sfr_dectr_use",
        "EveTreeH_use",
        "DecTreeH_use",
        "expected_height",
    ),
    [
        (0.4, 1.0e-6, 12.0, 12.0, 6.0),
        (0.25, 0.75, 10.0, 20.0, 8.75),
    ],
)
def test_mixed_tree_rsl_height_uses_weighted_half_tree_height(
    sfr_evetr_use,
    sfr_dectr_use,
    EveTreeH_use,
    DecTreeH_use,
    expected_height,
):
    expr = _mixed_tree_veg_air_height_expression()

    veg_air_height = _eval_fortran_expr(
        expr,
        sfr_evetr_use=sfr_evetr_use,
        sfr_dectr_use=sfr_dectr_use,
        EveTreeH_use=EveTreeH_use,
        DecTreeH_use=DecTreeH_use,
        tree_frac_sum=sfr_evetr_use + sfr_dectr_use,
    )

    assert veg_air_height == pytest.approx(expected_height)
