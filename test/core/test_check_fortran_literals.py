"""Tests for scripts/lint/check_fortran_literals.py.

The lint is a ratchet, so both directions are exercised: a count above the
baseline must fail, and so must a count below it (a stale baseline would let
literals come back). The counter itself is checked against the spellings in
use in the kernel and against the near misses it must not count. The live
source tree is checked too, so a new literal in ``src/suews/src`` fails here
as well as in CI.
"""

from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import sys

import pytest

pytestmark = pytest.mark.api


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "lint" / "check_fortran_literals.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "check_fortran_literals", SCRIPT_PATH
)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
check_fortran_literals = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = check_fortran_literals
SCRIPT_SPEC.loader.exec_module(check_fortran_literals)


SOURCE = """\
MODULE module_phys_example
   IMPLICIT NONE
   REAL(KIND(1D0)), PARAMETER :: SBConst = 5.67051E-8 ! Stefan-Boltzmann
CONTAINS
   SUBROUTINE example(temp_c, tk, lw, ang, sec, flag)
      REAL(KIND(1D0)) :: temp_c, tk, lw, ang, sec
      LOGICAL :: flag
      tk = temp_c + 273.15
      tk = temp_c + 273.15D0
      lw = 5.67E-8*tk**4
      ang = ang*3.14159265359/180.0
      ang = ang*pi/180.
      sec = 24*3600
      sec = sec/3600.
      flag = tk > 1.AND.flag
      tk = 1273.15 + x273
      lw = lw*1000 ! factor 1000 is reported, not gated; 273.15 in a comment
      CALL errorHint(1, 'temperature 273.15 in a string', tk, tk, 1)
   END SUBROUTINE example
END MODULE module_phys_example
"""


def _make_repo(tmp_path: Path, source: str, baseline: dict | None = None) -> Path:
    """Build a throwaway repo root with one Fortran file and a baseline."""
    path = tmp_path / "src" / "suews" / "src" / "suews_phys_example.f95"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(source, encoding="utf-8")
    baseline_path = tmp_path / check_fortran_literals.BASELINE_PATH
    baseline_path.parent.mkdir(parents=True, exist_ok=True)
    if baseline is not None:
        payload = {"files": {"src/suews/src/suews_phys_example.f95": baseline}}
        baseline_path.write_text(json.dumps(payload), encoding="utf-8")
    return tmp_path


def _run(repo_root: Path, *flags: str) -> int:
    return check_fortran_literals.main([
        "check_fortran_literals",
        str(repo_root),
        *flags,
    ])


EXPECTED = {
    "degree_radian": 1,
    "kelvin_offset": 2,
    "pi": 1,
    "seconds_per_day": 1,
    "seconds_per_hour": 1,
    "stefan_boltzmann": 2,
}


def test_counts_spellings_and_skips_near_misses():
    """Kind suffixes and exponent markers count; comments, strings, longer
    numbers, identifiers and dotted operators do not; a 24*3600 product is
    seconds per day, not an extra seconds-per-hour literal."""
    counts = check_fortran_literals.count_kinds(
        SOURCE, check_fortran_literals.GATED_KINDS
    )

    assert {kind: n for kind, n in counts.items() if n} == EXPECTED


def test_reported_kinds_are_counted_but_not_gated(tmp_path):
    """The factor-1000 bucket is visible in the report and absent from the
    baseline, so adding one does not fail the check."""
    counts = check_fortran_literals.count_kinds(
        SOURCE, check_fortran_literals.REPORTED_KINDS
    )
    assert counts["factor_1000"] == 1

    more_factors = SOURCE.replace("lw = lw*1000", "lw = lw*1000*1000")
    repo = _make_repo(tmp_path, more_factors, EXPECTED)

    assert _run(repo) == 0


@pytest.mark.parametrize(
    "source, expected",
    [
        ("flag = tk.GT.273.15.AND.tk.LT.273.16", {"kelvin_offset": 2}),
        ("tk = 273.&\n&15D0", {"kelvin_offset": 1}),
        ("sec = 24 * & ! one day\n! continued below\n  & 3600", {"seconds_per_day": 1}),
        ("ang = pi / &\n  180.0", {"degree_radian": 1}),
        ("CALL hint('value &\n! comment between continuation lines\n&273.15')", {}),
        ('CALL hint("value &\n&273.15")\ntk = 273.15', {"kelvin_offset": 1}),
        ("CALL hint('it''s 273.15!')\ntk = 273.15_real64", {"kelvin_offset": 1}),
        ("tk = .27315E3 + 27315D-2_8", {"kelvin_offset": 2}),
        ("sec = 24.0_dp * 60D0 * 60_8", {"seconds_per_day": 1}),
        ("ang = 180.0_real64 / PI", {"degree_radian": 1}),
        ('tk = 273.15 + &\n#include "constants.inc"\n& 273.16', {"kelvin_offset": 2}),
    ],
)
def test_fortran_lexical_boundaries(source, expected):
    """Count code across continuations, excluding continued character data."""
    counts = check_fortran_literals.count_kinds(
        source, check_fortran_literals.GATED_KINDS
    )

    assert {kind: n for kind, n in counts.items() if n} == expected


def test_matching_baseline_passes(tmp_path):
    repo = _make_repo(tmp_path, SOURCE, EXPECTED)

    assert _run(repo) == 0


def test_new_literal_fails(tmp_path, capsys):
    """A count above the baseline fails and names the file and kind."""
    baseline = {**EXPECTED, "kelvin_offset": 1}
    repo = _make_repo(tmp_path, SOURCE, baseline)

    assert _run(repo) == 1
    report = capsys.readouterr().err
    assert "suews_phys_example.f95: kelvin_offset 1 -> 2" in report


def test_new_file_with_literal_fails(tmp_path, capsys):
    """A file absent from the baseline is allowed zero literals."""
    repo = _make_repo(tmp_path, SOURCE, EXPECTED)
    extra = repo / "src" / "suews" / "src" / "suews_phys_new.f95"
    extra.write_text("x = y + 273.15\n", encoding="utf-8")

    assert _run(repo) == 1
    assert "suews_phys_new.f95: kelvin_offset 0 -> 1" in capsys.readouterr().err


def test_constants_module_is_not_counted(tmp_path):
    """Defining a named constant in the constants module is the remedy, so
    literals there do not count against the baseline."""
    repo = _make_repo(tmp_path, SOURCE, EXPECTED)
    const = repo / "src" / "suews" / "src" / "suews_ctrl_const.f95"
    const.write_text(
        "REAL(KIND(1D0)), PARAMETER :: C2K_D = 273.15D0\n", encoding="utf-8"
    )

    assert _run(repo) == 0


def test_stale_baseline_fails(tmp_path, capsys):
    """A count below the baseline fails until the baseline is lowered."""
    baseline = {**EXPECTED, "pi": 3}
    repo = _make_repo(tmp_path, SOURCE, baseline)

    assert _run(repo) == 1
    assert "pi 3 -> 1" in capsys.readouterr().err


def test_write_baseline_round_trips(tmp_path):
    """--write-baseline records the tree, after which the check passes."""
    repo = _make_repo(tmp_path, SOURCE)

    assert _run(repo, "--write-baseline") == 0
    payload = json.loads(
        (repo / check_fortran_literals.BASELINE_PATH).read_text(encoding="utf-8")
    )
    assert payload["files"] == {"src/suews/src/suews_phys_example.f95": EXPECTED}
    assert _run(repo) == 0


def test_source_tree_matches_baseline():
    """The committed baseline equals the shipped Fortran's counts."""
    assert _run(REPO_ROOT) == 0


def _constants_repo(tmp_path, constants, physics=""):
    repo = _make_repo(tmp_path, physics, {})
    path = repo / "src/suews/src/suews_ctrl_const.f95"
    path.write_text(constants, encoding="utf-8")
    return repo


@pytest.mark.parametrize("literal", ["273.15", "273.15E0", "273.15_real64"])
def test_shared_definition_requires_d_literal(tmp_path, literal):
    repo = _constants_repo(tmp_path, f"REAL(KIND(1D0)), PARAMETER :: C2K = {literal}")
    assert _run(repo) == 1


def test_double_literal_in_single_real_is_rejected(tmp_path):
    repo = _constants_repo(tmp_path, "REAL, PARAMETER :: C2K = 273.15D0")
    assert _run(repo) == 1


@pytest.mark.parametrize(
    "definitions",
    [
        "a=273.15D0, b=273.16D0",
        "a=5.67D-8, b=5.670374419D-8",
        "a=3.14159D0, b=3.141592653589793D0",
        "a=1013.0D0, b=101325.0D0",
    ],
)
def test_conflicting_shared_quantities_fail(tmp_path, definitions):
    repo = _constants_repo(tmp_path, "REAL(KIND(1D0)), PARAMETER :: " + definitions)
    assert any(
        "conflicts" in e for e in check_fortran_literals.check_shared_constants(repo)
    )


@pytest.mark.parametrize(
    "declaration",
    [
        "REAL(KIND(1D0)), PARAMETER :: SIGMA = SBConst",
        "REAL(KIND(1D0)) :: SIGMA = SBConst",
        "REAL(KIND(1D0)) :: C2K",
        "REAL(KIND(1D0)) :: sigma\nsigma = SBConst",
        "REAL(KIND(1D0)), PARAMETER :: a = 1, &\n & sigma = SBConst",
        "USE module_ctrl_const_physconst, ONLY: sb => SBConst\nREAL(KIND(1D0)) :: sigma = sb",
        "REAL(KIND(1D0)) :: sigma; sigma = SBConst",
    ],
)
def test_physics_cannot_redeclare_or_copy_shared_constant(tmp_path, declaration):
    repo = _constants_repo(
        tmp_path,
        "REAL(KIND(1D0)), PARAMETER :: C2K = 273.15D0, SBConst = 5.670374419D-8",
        declaration,
    )
    assert check_fortran_literals.check_shared_constants(repo)
    assert _run(repo, "--write-baseline") == 1


def test_use_renaming_and_derived_expressions_are_allowed(tmp_path):
    repo = _constants_repo(
        tmp_path,
        "REAL(KIND(1D0)), PARAMETER :: SBConst=5.670374419D-8, pi=3.141592653589793D0\n"
        "REAL(KIND(1D0)), PARAMETER :: DEG2RAD=pi/180.0D0, RAD2DEG=180.0D0/pi",
        "USE module_ctrl_const_physconst, ONLY: sigma => SBConst\n"
        "REAL(KIND(1D0)) :: lw\nlw = sigma * temp**4\n"
        "! REAL :: SBConst\nCALL hint('REAL :: pi = 3.14')",
    )
    assert _run(repo) == 0
