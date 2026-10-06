"""Tests for scripts/suews/gen_fortran_deps.py (gh#1790).

The Fortran build runs ``make -j``, which is only safe because
``src/suews/Makefile.deps`` orders every object after the objects whose
modules it uses. A ``USE`` added without regenerating that file can pass a
serial build and fail a parallel one intermittently, so the committed file is
checked against a fresh generation here.
"""

from __future__ import annotations

import importlib.util
from pathlib import Path
import sys

import pytest

pytestmark = pytest.mark.api


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "suews" / "gen_fortran_deps.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location("gen_fortran_deps", SCRIPT_PATH)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
gen_fortran_deps = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = gen_fortran_deps
SCRIPT_SPEC.loader.exec_module(gen_fortran_deps)


def test_committed_deps_are_up_to_date():
    assert gen_fortran_deps.main(["--check"]) == 0, (
        "src/suews/Makefile.deps is stale; run python scripts/suews/gen_fortran_deps.py"
    )


def test_scan_reads_module_and_use_statements(tmp_path):
    source = tmp_path / "example.f95"
    source.write_text(
        """\
MODULE module_example ! trailing comment
   USE module_ctrl_const, ONLY: pi
   use :: module_ctrl_type
   USE, INTRINSIC :: iso_c_binding
   USE, NON_INTRINSIC :: module_ctrl_error
   IMPLICIT NONE
   INTEGER :: useful = 1
   INTERFACE generic
      MODULE PROCEDURE specific
   END INTERFACE generic
END MODULE module_example
""",
        encoding="utf-8",
    )
    defined, used = gen_fortran_deps.scan(source)
    assert defined == {"module_example"}
    assert used == {"module_ctrl_const", "module_ctrl_type", "module_ctrl_error"}


def test_edges_skip_modules_defined_outside_the_build(tmp_path, monkeypatch):
    monkeypatch.setattr(gen_fortran_deps, "SUEWS_DIR", tmp_path)
    (tmp_path / "src").mkdir()
    base = tmp_path / "src" / "base.f95"
    base.write_text("MODULE base_mod\nEND MODULE base_mod\n", encoding="utf-8")
    user = tmp_path / "src" / "user.f95"
    user.write_text(
        "MODULE user_mod\n USE base_mod\n USE module_wrf_only\n"
        " USE user_mod\nEND MODULE user_mod\n",
        encoding="utf-8",
    )
    edges = gen_fortran_deps.build_edges([base, user])
    assert edges == {"src/user.o": {"src/base.o"}}


def test_every_used_suews_module_has_an_owner():
    """Each module a built SUEWS file uses resolves to a built object.

    Guards the generator itself: a regex miss on a ``MODULE`` line would drop
    every edge into that file without failing the up-to-date check.
    """
    sources = sorted((gen_fortran_deps.SUEWS_DIR / "src").glob("*.f95"))
    defined: set[str] = set().union(*gen_fortran_deps.GENERATED_SOURCES.values())
    used: set[str] = set()
    for source in sources:
        d, u = gen_fortran_deps.scan(source)
        defined |= d
        used |= u
    for spartacus in gen_fortran_deps.spartacus_sources(
        (gen_fortran_deps.SUEWS_DIR / "Makefile").read_text(encoding="utf-8")
    ):
        defined |= gen_fortran_deps.scan(spartacus)[0]
    intrinsic = {"iso_c_binding", "iso_fortran_env", "ieee_arithmetic"}
    # Modules referenced only from WRF-coupled code paths (#ifdef wrf).
    unresolved = used - defined - intrinsic
    assert all("wrf" in m for m in unresolved), sorted(unresolved)
