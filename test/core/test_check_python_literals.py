"""Tests for scripts/lint/check_python_literals.py.

The lint is a ratchet, so both directions are exercised: a count above the
baseline must fail, and so must a count below it (a stale baseline would let
literals come back). The AST counter is checked against physical/time literals
and multiplicative/divisive expressions, and against comments, docstrings,
strings, version strings, booleans, bare integer enum codes, and near misses
that it must ignore. The live source tree is checked too, so a new literal in
``src/supy`` fails here as well as in CI.
"""

from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import sys

import pytest

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-TOOLING")]


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "lint" / "check_python_literals.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "check_python_literals", SCRIPT_PATH
)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
check_python_literals = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = check_python_literals
SCRIPT_SPEC.loader.exec_module(check_python_literals)


SOURCE = '''\
"""Example module mentioning 273.15, 5.67e-8, 3600, 86400, and pi / 180."""

from enum import Enum
import math


class ErrorCode(Enum):
    """Machine-readable error codes."""

    PHYSICS_INCOMPATIBLE = 1005


def example(temp_c: float, ang: float, sec: float) -> float:
    """Docstring with 273.15, 1013.25, and 24 * 3600."""
    tk = temp_c + 273.15
    tk_neg = -273.15 + tk
    lw = 5.67e-8 * tk**4
    lw_codata = 5.670374419e-8 * tk_neg**4
    rad_from_lit = ang * 3.14159265359 / 180.0
    rad_from_pi = ang * math.pi / 180.0
    sec_day = 24 * 3600
    sec_hr = sec / 3600
    near_miss = 1273.15 + 3.14 + 180.0
    x273 = 1000  # 273.15 in comment; factor 1000 is reported, not gated
    label = "DailyState_1440 temperature 273.15 version 3.14159"
    flag = True
    return (
        lw
        + lw_codata
        + rad_from_lit
        + rad_from_pi
        + sec_day
        + sec_hr
        + near_miss
        + x273
        + len(label)
        + int(flag)
    )
'''

EXPECTED = {
    "degree_radian": 1,
    "kelvin_offset": 2,
    "pi": 1,
    "seconds_per_day": 1,
    "seconds_per_hour": 1,
    "stefan_boltzmann": 2,
}


def _make_repo(
    tmp_path: Path, source: str, baseline: dict[str, int] | None = None
) -> Path:
    """Build a throwaway repo root with one SuPy module and optional baseline."""
    path = tmp_path / "src" / "supy" / "example.py"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(source, encoding="utf-8")
    baseline_path = tmp_path / check_python_literals.BASELINE_PATH
    baseline_path.parent.mkdir(parents=True, exist_ok=True)
    if baseline is not None:
        payload = {"files": {"src/supy/example.py": baseline}}
        baseline_path.write_text(json.dumps(payload), encoding="utf-8")
    return tmp_path


def _run(repo_root: Path, *flags: str) -> int:
    return check_python_literals.main([
        "check_python_literals",
        str(repo_root),
        *flags,
    ])


def test_counts_literals_and_skips_non_code():
    """Numeric literals and expressions count; docstrings, comments, strings,
    booleans, integer enum codes, and near misses do not; ``24 * 3600`` is
    seconds per day rather than an extra seconds-per-hour literal."""
    counts = check_python_literals.count_kinds(
        SOURCE, check_python_literals.GATED_KINDS
    )

    assert {kind: n for kind, n in counts.items() if n} == EXPECTED


def test_reported_kinds_are_counted_but_not_gated(tmp_path):
    """The factor-1000 bucket is visible in the report and absent from the
    baseline, so adding one does not fail the check."""
    counts = check_python_literals.count_kinds(
        SOURCE, check_python_literals.REPORTED_KINDS
    )
    assert counts["factor_1000"] == 1

    more_factors = SOURCE.replace("x273 = 1000", "x273 = 1000 * 1000 + 1e6")
    repo = _make_repo(tmp_path, more_factors, EXPECTED)

    assert _run(repo) == 0


@pytest.mark.parametrize(
    "source, expected",
    [
        ("x = 273.15 + 273.16", {"kelvin_offset": 2}),
        ("x = -273.15", {"kelvin_offset": 1}),
        ("x = 5.67e-8 + 5.670374419e-8 + 5.67051e-8", {"stefan_boltzmann": 3}),
        ("x = 3.141592653589793", {"pi": 1}),
        ("x = 0.017453292519943295 + 57.29577951308232", {"degree_radian": 2}),
        ("x = math.pi / 180", {"degree_radian": 1}),
        ("x = np.pi / 180.0", {"degree_radian": 1}),
        ("x = 180 / math.pi", {"degree_radian": 1}),
        ("x = (angle * np.pi) / 180", {"degree_radian": 1}),
        ("x = angle / 180.0 * PI", {"degree_radian": 1}),
        ("x = -(angle * np.pi) / 180", {"degree_radian": 1}),
        ("x = angle * (180 / constants.PI)", {"degree_radian": 1}),
        ("x = 24 * 60 * 60", {"seconds_per_day": 1}),
        ("x = days * 24 * 3600", {"seconds_per_day": 1}),
        ("x = sec / 24 / 60 / 60", {"seconds_per_day": 1}),
        ("x = sec / (24.0 * 3600.0)", {"seconds_per_day": 1}),
        (
            "x = 86400 + 1440 + 3600",
            {"seconds_per_day": 1, "minutes_per_day": 1, "seconds_per_hour": 1},
        ),
        ("x = 1013.25 + 101325.0 + 101.325 + 1013", {"reference_pressure": 4}),
        ("x = 1005.0 + 1006.38", {"specific_heat_air": 2}),
        ("x = 2264705.0 + 2.501e6 + 2500.25", {"latent_heat_vaporisation": 3}),
        ("x = f'{temp + 273.15}'", {"kelvin_offset": 1}),
        ("x = foo(273.15) * np.pi / 180", {"degree_radian": 1, "kelvin_offset": 1}),
        # Enum methods and float members are still checked; bare integer codes are not.
        ("class E(Enum):\n    CODE = 1005\n", {}),
        ("class E(Enum):\n    CP = 1005.0\n", {"specific_heat_air": 1}),
        (
            "class Tz(float, Enum):\n"
            "    UTC = 0.0\n"
            "    def seconds(self):\n"
            "        return int(self.value * 3600)\n",
            {"seconds_per_hour": 1},
        ),
        # Non-matches: strings, comments, versions, unrelated math, standalone math.pi
        ("x = '273.15 86400 3600 5.67e-8'", {}),
        ("VERSION = '2026.6.dev9'\n# 273.15 86400 3600", {}),
        ("x = math.pi * r**2", {}),
        ("x = (180 + offset) / math.pi", {}),
        ("x = 180 * math.pi", {}),
        ("x = 24 / 3600", {"seconds_per_hour": 1}),
        ("x = 24 * (60 + 60)", {}),
        ("x = 10**400", {}),
        (
            "def f(lat_bounds=(-90.0, 90.0), lng_bounds=(-180.0, 180.0)): return True",
            {},
        ),
    ],
)
def test_ast_patterns_and_exclusions(source, expected):
    """AST matching handles literals, expressions, f-strings, and safe exclusions."""
    counts = check_python_literals.count_kinds(
        source, check_python_literals.GATED_KINDS
    )

    assert {kind: n for kind, n in counts.items() if n} == expected


def test_find_occurrences_reports_line_numbers():
    """find_occurrences returns sorted (lineno, kind) pairs."""
    snippet = "a = 1\nb = a + 273.15\nc = b * math.pi / 180\n"
    occurrences = check_python_literals.find_occurrences(
        snippet, check_python_literals.GATED_KINDS
    )

    assert occurrences == [(2, "kelvin_offset"), (3, "degree_radian")]


def test_matching_baseline_passes(tmp_path):
    repo = _make_repo(tmp_path, SOURCE, EXPECTED)

    assert _run(repo) == 0


def test_clean_code_with_empty_baseline_passes(tmp_path):
    """Clean Python importing supy.constants passes against an empty baseline."""
    clean = (
        "from supy.constants import C2K, DEG2RAD, SEC_PER_DAY, SEC_PER_HOUR\n"
        "def f(t_c, deg):\n"
        "    return (t_c + C2K, deg * DEG2RAD, SEC_PER_DAY // SEC_PER_HOUR)\n"
    )
    repo = _make_repo(tmp_path, clean)
    baseline_path = tmp_path / check_python_literals.BASELINE_PATH
    baseline_path.write_text(json.dumps({"files": {}}), encoding="utf-8")

    assert _run(repo) == 0


def test_new_literal_fails(tmp_path, capsys):
    """A count above the baseline fails and names the file and kind."""
    baseline = {**EXPECTED, "kelvin_offset": 1}
    repo = _make_repo(tmp_path, SOURCE, baseline)

    assert _run(repo) == 1
    report = capsys.readouterr().err
    assert "src/supy/example.py: kelvin_offset 1 -> 2" in report


def test_new_file_with_literal_fails(tmp_path, capsys):
    """A file absent from the baseline is allowed zero literals."""
    repo = _make_repo(tmp_path, SOURCE, EXPECTED)
    extra = repo / "src" / "supy" / "new_module.py"
    extra.write_text("x = y + 273.15\n", encoding="utf-8")

    assert _run(repo) == 1
    assert "src/supy/new_module.py: kelvin_offset 0 -> 1" in capsys.readouterr().err


def test_constants_module_is_exempt(tmp_path):
    """Defining named constants in src/supy/constants.py is exempt from the lint."""
    repo = _make_repo(tmp_path, SOURCE, EXPECTED)
    const = repo / "src" / "supy" / "constants.py"
    const.write_text(
        "C2K = 273.15\nSTEFAN_BOLTZMANN = 5.670374419e-8\nSEC_PER_DAY = 86400\n",
        encoding="utf-8",
    )

    assert _run(repo) == 0


def test_nested_constants_module_is_not_exempt(tmp_path, capsys):
    """Only canonical src/supy/constants.py is exempt, not arbitrary constants.py."""
    repo = _make_repo(tmp_path, SOURCE, EXPECTED)
    nested = repo / "src" / "supy" / "subpkg" / "constants.py"
    nested.parent.mkdir(parents=True, exist_ok=True)
    nested.write_text("C2K = 273.15\n", encoding="utf-8")

    assert _run(repo) == 1
    assert (
        "src/supy/subpkg/constants.py: kelvin_offset 0 -> 1" in capsys.readouterr().err
    )


def test_generated_files_are_exempt(tmp_path):
    """Generated _version.py and _suews_driver.py files are excluded."""
    repo = _make_repo(tmp_path, SOURCE, EXPECTED)
    (repo / "src" / "supy" / "_version.py").write_text(
        "__version_tuple__ = (2026, 6, 3600)\n", encoding="utf-8"
    )
    (repo / "src" / "supy" / "_suews_driver.py").write_text(
        "offset = 273.15\n", encoding="utf-8"
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
        (repo / check_python_literals.BASELINE_PATH).read_text(encoding="utf-8")
    )
    assert payload["files"] == {"src/supy/example.py": EXPECTED}
    assert _run(repo) == 0


def test_report_mode_exits_zero(tmp_path, capsys):
    """--report prints gated and reported totals without requiring a baseline."""
    repo = _make_repo(tmp_path, SOURCE)

    assert _run(repo, "--report") == 0
    out = capsys.readouterr().out
    assert "kelvin_offset" in out
    assert "factor_1000" in out


def test_missing_source_dir_and_missing_baseline_fail(tmp_path, capsys):
    """CLI exits 1 when src/supy or the baseline file is missing."""
    assert _run(tmp_path) == 1
    assert "src/supy not found" in capsys.readouterr().err

    repo = _make_repo(tmp_path, SOURCE)
    assert _run(repo) == 1
    assert "not found" in capsys.readouterr().err


def test_malformed_baseline_fails_cleanly(tmp_path, capsys):
    """Invalid baseline JSON or schema fails with exit 1."""
    repo = _make_repo(tmp_path, SOURCE)
    baseline_path = repo / check_python_literals.BASELINE_PATH
    baseline_path.write_text("{not valid json", encoding="utf-8")
    assert _run(repo) == 1
    assert "failed to read baseline" in capsys.readouterr().err

    baseline_path.write_text(json.dumps({"files": []}), encoding="utf-8")
    assert _run(repo) == 1
    assert "failed to read baseline" in capsys.readouterr().err


def test_syntax_error_fails_loudly(tmp_path, capsys):
    """Unparseable Python under src/supy fails the lint rather than being skipped."""
    repo = _make_repo(tmp_path, "def broken(:\n", {})

    assert _run(repo) == 1
    assert "failed to scan Python sources" in capsys.readouterr().err


def test_source_tree_matches_baseline():
    """The committed baseline equals the shipped SuPy Python counts."""
    assert _run(REPO_ROOT) == 0
