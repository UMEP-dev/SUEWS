"""Tests for scripts/lint/check_saved_fortran_locals.py (gh#1741).

A lint that has only been seen to pass is not tested, so both directions are
exercised: a procedure local with a declaration initialiser must fail, and
the forms that are not implicit SAVE (PARAMETER constants, derived-type
component defaults, runtime assignment) must pass. The live source tree is
checked too, so a reintroduction in ``src/suews/src`` fails here as well as
in CI.
"""

from __future__ import annotations

import importlib.util
from pathlib import Path
import sys

import pytest

pytestmark = pytest.mark.api


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "lint" / "check_saved_fortran_locals.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "check_saved_fortran_locals", SCRIPT_PATH
)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
check_saved_fortran_locals = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = check_saved_fortran_locals
SCRIPT_SPEC.loader.exec_module(check_saved_fortran_locals)


CLEAN_SOURCE = """\
MODULE module_phys_example
   IMPLICIT NONE
   INTEGER :: module_counter = 0 ! module-level: out of scope for this lint

   TYPE :: dts_example
      REAL(KIND(1D0)) :: albedo = 0.1D0 ! component default, not SAVE
   END TYPE dts_example

CONTAINS

   FUNCTION lat_vap(temp_c) RESULT(lv)
      REAL(KIND(1D0)), INTENT(IN) :: temp_c
      REAL(KIND(1D0)) :: lv
      LOGICAL :: switch1, switch2
      INTEGER, PARAMETER :: from = 2
      CHARACTER(len=*), PARAMETER :: label = 'lat_vap'
      REAL(KIND(1D0)), DIMENSION(2) :: pair ! no initialiser

      switch1 = .FALSE.
      switch2 = .FALSE.
      pair = (/1.0D0, 2.0D0/)
      IF (from == 2) lv = temp_c
   END FUNCTION lat_vap

END MODULE module_phys_example
"""

INITIALISED_LOCAL = """\
MODULE module_util_example
   IMPLICIT NONE
CONTAINS
   FUNCTION lat_vap(temp_c) RESULT(lv)
      REAL(KIND(1D0)) :: temp_c, lv
      LOGICAL :: switch1 = .FALSE., switch2 = .FALSE.
      INTEGER :: ii, from = 2
      lv = temp_c
   END FUNCTION lat_vap
END MODULE module_util_example
"""


def _make_repo(tmp_path: Path, files: dict[str, str]) -> Path:
    """Build a throwaway repo root containing ``src/suews/src`` with ``files``."""
    for name, source in files.items():
        path = tmp_path / "src" / "suews" / "src" / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(source, encoding="utf-8")
    return tmp_path


def _run(repo_root: Path) -> int:
    return check_saved_fortran_locals.main(
        ["check_saved_fortran_locals", str(repo_root)]
    )


def test_clean_tree_passes(tmp_path):
    """PARAMETERs, component defaults, module variables and runtime
    assignment are not implicit SAVE of a procedure local."""
    repo = _make_repo(tmp_path, {"suews_phys_example.f95": CLEAN_SOURCE})

    assert _run(repo) == 0


def test_initialised_local_fails(tmp_path, capsys):
    """Every initialised entity is named, including the one sharing a line
    with an uninitialised local."""
    repo = _make_repo(tmp_path, {"suews_util_example.f95": INITIALISED_LOCAL})

    assert _run(repo) == 1
    report = capsys.readouterr().err
    assert "suews_util_example.f95:6: switch1" in report
    assert "suews_util_example.f95:6: switch2" in report
    assert "suews_util_example.f95:7: from" in report
    assert ": ii " not in report


@pytest.mark.parametrize(
    "declaration",
    [
        "INTEGER, SAVE :: calls",
        "REAL(KIND(1D0)), POINTER :: p => NULL()",
        "REAL(KIND(1D0)) :: &\n         a, &\n         b = 1.0D0",
    ],
    ids=["save-attribute", "pointer-initialiser", "continued-declaration"],
)
def test_other_static_forms_fail(tmp_path, declaration):
    source = (
        "SUBROUTINE example()\n"
        "   IMPLICIT NONE\n"
        f"   {declaration}\n"
        "END SUBROUTINE example\n"
    )
    repo = _make_repo(tmp_path, {"suews_phys_example.f95": source})

    assert _run(repo) == 1


def test_save_statement_fails(tmp_path):
    source = (
        "SUBROUTINE example()\n"
        "   IMPLICIT NONE\n"
        "   INTEGER :: calls\n"
        "   SAVE\n"
        "END SUBROUTINE example\n"
    )
    repo = _make_repo(tmp_path, {"suews_phys_example.f95": source})

    assert _run(repo) == 1


def test_source_tree_is_clean():
    """The shipped Fortran carries no implicitly SAVEd procedure local
    beyond the documented allowlist."""
    assert _run(REPO_ROOT) == 0
