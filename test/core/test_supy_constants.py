"""Tests for supy.constants and cross-layer parity with suews_ctrl_const.f95."""

from __future__ import annotations

import math
from pathlib import Path
import re

import pytest

from supy import constants
from supy.units import VOCABULARY

pytestmark = [pytest.mark.unit, pytest.mark.contract("C-RESPONSE")]

REPO_ROOT = Path(__file__).resolve().parents[2]
FORTRAN_CONST_PATH = REPO_ROOT / "src" / "suews" / "src" / "suews_ctrl_const.f95"

# Exact 1:1 mapping from Fortran PARAMETER names in module_ctrl_const_mathconst
# and module_ctrl_const_physconst to their Python counterparts in supy.constants.
FORTRAN_TO_PYTHON: dict[str, str] = {
    "pi": "PI",
    "DEG2RAD": "DEG2RAD",
    "RAD2DEG": "RAD2DEG",
    "sec_per_hour": "SEC_PER_HOUR",
    "sec_per_day": "SEC_PER_DAY",
    "min_per_day": "MIN_PER_DAY",
    "arcsec_per_deg": "ARCSEC_PER_DEG",
    "cp_air_anohm": "CP_AIR_ANOHM",
    "lv_anohm": "LV_ANOHM",
    "C2K": "C2K",
    "SBConst": "STEFAN_BOLTZMANN",
    "JtoumolPAR": "J_TO_UMOL_PAR",
    "KdntoPAR": "KDN_TO_PAR",
    "lv_bolton": "LV_BOLTON",
    "lv_wetbulb": "LV_WETBULB",
    "cp_air": "CP_AIR",
    "HPA_TO_PA": "HPA_TO_PA",
    "p_ref_pa_d0": "P_REF_PA",
    "eps_fp": "EPS_FP",
}

_MODULE_RE = re.compile(
    r"^\s*MODULE\s+(module_ctrl_const_(?:math|phys)const)\b(.*?)^\s*END\s+MODULE\b",
    re.IGNORECASE | re.MULTILINE | re.DOTALL,
)
_PARAM_LINE_RE = re.compile(
    r"^\s*REAL\s*\(\s*KIND\s*\(\s*1D0\s*\)\s*\)\s*,\s*PARAMETER\s*::\s*(.+)$",
    re.IGNORECASE,
)
_FORTRAN_FLOAT_RE = re.compile(r"^[+-]?(?:\d+\.\d*|\.\d+|\d+)(?:[dDeE][+-]?\d+)?$")


def _eval_fortran_rhs(expr: str, known: dict[str, float]) -> float:
    """Parse a Fortran double-precision literal or simple ratio referencing ``pi``."""
    token = expr.strip()
    if _FORTRAN_FLOAT_RE.fullmatch(token):
        return float(token.replace("D", "e").replace("d", "e"))
    if "/" in token:
        lhs_text, rhs_text = (part.strip() for part in token.split("/", 1))
        lhs = (
            known[lhs_text] if lhs_text in known else _eval_fortran_rhs(lhs_text, known)
        )
        rhs = (
            known[rhs_text] if rhs_text in known else _eval_fortran_rhs(rhs_text, known)
        )
        return lhs / rhs
    raise AssertionError(f"Unsupported Fortran constant expression: {expr!r}")


def _parse_fortran_constants(path: Path) -> dict[str, float]:
    """Extract every REAL(KIND(1D0)), PARAMETER from mathconst and physconst."""
    text = path.read_text(encoding="utf-8")
    parsed: dict[str, float] = {}
    matches = list(_MODULE_RE.finditer(text))
    assert len(matches) == 2, (
        "Expected module_ctrl_const_mathconst and module_ctrl_const_physconst "
        f"in {path}"
    )
    for match in matches:
        for raw_line in match.group(2).splitlines():
            code = raw_line.split("!", 1)[0].strip()
            if not code:
                continue
            param_match = _PARAM_LINE_RE.match(code)
            if param_match is None:
                continue
            for item in param_match.group(1).split(","):
                name, rhs = (part.strip() for part in item.split("=", 1))
                parsed[name] = _eval_fortran_rhs(rhs, parsed)
    return parsed


def test_exports_are_complete_and_finite() -> None:
    """Every exported constant is finite and exact time counts stay integers."""
    assert set(constants.__all__) == set(FORTRAN_TO_PYTHON.values())
    int_names = {"SEC_PER_HOUR", "SEC_PER_DAY", "MIN_PER_DAY", "ARCSEC_PER_DEG"}
    for name in constants.__all__:
        value = getattr(constants, name)
        assert math.isfinite(value), f"{name} is not finite: {value!r}"
        if name in int_names:
            assert type(value) is int, f"{name} should be int, got {type(value)}"
        else:
            assert type(value) is float, f"{name} should be float, got {type(value)}"


def test_python_constants_match_fortran_kernel() -> None:
    """Every constant in module_ctrl_const_{math,phys}const matches supy.constants."""
    fortran_values = _parse_fortran_constants(FORTRAN_CONST_PATH)

    assert set(fortran_values) == set(FORTRAN_TO_PYTHON), (
        "Constant inventory drifted between suews_ctrl_const.f95 and "
        "supy.constants: "
        f"missing_in_python={sorted(set(fortran_values) - set(FORTRAN_TO_PYTHON))}, "
        f"extra_in_python={sorted(set(FORTRAN_TO_PYTHON) - set(fortran_values))}"
    )

    for fortran_name, python_name in FORTRAN_TO_PYTHON.items():
        py_val = float(getattr(constants, python_name))
        f_val = fortran_values[fortran_name]
        assert py_val == f_val, (
            f"Mismatch between {fortran_name}={f_val!r} in suews_ctrl_const.f95 "
            f"and {python_name}={py_val!r} in supy.constants"
        )


def test_units_vocabulary_matches_constants() -> None:
    """Unit scales and offsets in supy.units match supy.constants."""
    assert VOCABULARY["degC"][1] == constants.C2K
    assert math.isclose(
        VOCABULARY["h"][0], constants.SEC_PER_HOUR, rel_tol=0.0, abs_tol=0.0
    )
    assert math.isclose(
        VOCABULARY["d"][0], constants.SEC_PER_DAY, rel_tol=0.0, abs_tol=0.0
    )
    assert VOCABULARY["degree"][0] == constants.DEG2RAD
