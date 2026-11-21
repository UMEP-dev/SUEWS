#!/usr/bin/env python3
"""Run optional-dependency, release-compat, and standalone SPARTACUS tests with zero silent skips.

Used by ``.github/workflows/optional-extras-tests.yml`` (gh#2104 F-17b) to
execute test nodes that are skipped on standard wheel CI lanes (``cf_units``,
``pvlib`` via ``.[tmy]``, release sample compatibility, and the standalone
NetCDF/OpenMP ``spartacus_surface`` binary) and fail loudly if any targeted
node silently skips.
"""

from __future__ import annotations

import argparse
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import xml.etree.ElementTree as ET

REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_SPARTACUS_BIN = (
    REPO_ROOT
    / "src"
    / "suews"
    / "ext_lib"
    / "spartacus-surface"
    / "bin"
    / "spartacus_surface"
)

# Policy-permitted dev-cycle skip in test_release_compat.py:
# .claude/rules/python/schema-versioning.md reserves test/fixtures/release_configs/
# for formal releases (never .devN bumps), so test_last_release_sample_parses has
# an empty current-schema fixture set during .devN development cycles. Pytest
# emits a single placeholder testcase with exact name and skip message below.
# Any real fixture parameter, unrelated skip reason, or formal release fails.
_DEV_CYCLE_RELEASE_TEST_BASE = "test_last_release_sample_parses"
_DEV_CYCLE_RELEASE_SKIP_CASE_NAME = "test_last_release_sample_parses[NOTSET]"
_DEV_CYCLE_RELEASE_SKIP_REASON = "got empty parameter set for (yaml_path)"


@dataclass(frozen=True)
class OptionalTestSuite:
    """Definition of one targeted optional-dependency or binary test suite."""

    name: str
    targets: tuple[str, ...]
    min_expected_passed: int
    allow_dev_release_skip: bool = False


OPTIONAL_TEST_SUITES: tuple[OptionalTestSuite, ...] = (
    OptionalTestSuite(
        name="cf_units",
        targets=(
            "test/core/test_units.py::test_every_symbol_is_valid_udunits_with_the_same_scale",
            "test/core/test_units.py::test_compound_units_agree_with_udunits",
            "test/data_model/test_unit_metadata.py::test_unit_metadata_cf_units_when_available",
        ),
        min_expected_passed=37,
    ),
    OptionalTestSuite(
        name="pvlib",
        targets=("test/io_tests/test_gen_epw_resample.py::TestGenEpwMultiIndexInput",),
        min_expected_passed=5,
    ),
    OptionalTestSuite(
        name="standalone_spartacus",
        targets=("test/physics/test_standalone_spartacus_errors.py",),
        min_expected_passed=3,
    ),
    OptionalTestSuite(
        name="release_compat",
        targets=("test/data_model/test_release_compat.py",),
        min_expected_passed=12,
        allow_dev_release_skip=True,
    ),
)

SUITE_BY_NAME: dict[str, OptionalTestSuite] = {
    suite.name: suite for suite in OPTIONAL_TEST_SUITES
}


@dataclass(frozen=True)
class SuiteCounts:
    """Parsed JUnit XML outcome counts for one suite run."""

    total: int
    passed: int
    failed: int
    errors: int
    skipped: int
    skipped_cases: tuple[tuple[str, str], ...]
    failed_cases: tuple[tuple[str, str], ...] = ()
    case_names: tuple[str, ...] = ()


@dataclass(frozen=True)
class SuiteRunResult:
    """Execution and zero-skip verification outcome for one suite."""

    suite: OptionalTestSuite
    returncode: int
    elapsed_seconds: float
    counts: SuiteCounts | None
    violations: tuple[str, ...]
    stdout: str
    stderr: str

    @property
    def passed(self) -> bool:
        """Whether pytest exited cleanly and all zero-skip invariants held."""
        return self.returncode == 0 and not self.violations


def parse_junit_xml(xml_path: Path) -> SuiteCounts:
    """Parse a pytest JUnit XML report into test counts and skip details.

    Parameters
    ----------
    xml_path : Path
        Path to the JUnit XML file written by ``pytest --junitxml``.

    Returns
    -------
    SuiteCounts
        Aggregated pass, failure, error, and skip counts plus skipped cases.
    """
    tree = ET.parse(xml_path)
    root = tree.getroot()
    cases = list(root.iter("testcase"))

    passed = 0
    failed = 0
    errors = 0
    skipped_cases: list[tuple[str, str]] = []
    failed_cases: list[tuple[str, str]] = []
    case_names: list[str] = []

    for case in cases:
        case_name = case.attrib.get("name", "<unknown>")
        case_names.append(case_name)
        failure = case.find("failure")
        error = case.find("error")
        skipped = case.find("skipped")
        if failure is not None:
            failed += 1
            msg = failure.attrib.get("message") or (failure.text or "").strip()
            failed_cases.append((case_name, msg.splitlines()[0] if msg else ""))
        elif error is not None:
            errors += 1
            msg = error.attrib.get("message") or (error.text or "").strip()
            failed_cases.append((case_name, msg.splitlines()[0] if msg else ""))
        elif skipped is not None:
            reason = (
                skipped.attrib.get("message", "").strip()
                or (skipped.text or "").strip()
            )
            skipped_cases.append((case_name, reason))
        else:
            passed += 1

    return SuiteCounts(
        total=len(cases),
        passed=passed,
        failed=failed,
        errors=errors,
        skipped=len(skipped_cases),
        skipped_cases=tuple(skipped_cases),
        failed_cases=tuple(failed_cases),
        case_names=tuple(case_names),
    )


def evaluate_suite_junit(
    suite: OptionalTestSuite,
    counts: SuiteCounts,
    *,
    current_schema_version: str,
) -> list[str]:
    """Validate JUnit XML counts against strict zero-silent-skip invariants.

    Parameters
    ----------
    suite : OptionalTestSuite
        Suite specification being checked.
    counts : SuiteCounts
        Parsed JUnit XML counts from pytest.
    current_schema_version : str
        Live ``CURRENT_SCHEMA_VERSION`` string (used to distinguish ``.devN``
        cycles from formal releases for ``release_compat``).

    Returns
    -------
    list[str]
        Human-readable violation strings; empty when every invariant passed.
    """
    violations: list[str] = []
    if counts.failed > 0 or counts.errors > 0:
        violations.append(
            f"{suite.name}: {counts.failed} failed, {counts.errors} errored"
        )
    if counts.passed < suite.min_expected_passed:
        violations.append(
            f"{suite.name}: expected at least {suite.min_expected_passed} passed "
            f"tests, observed {counts.passed}"
        )

    is_dev_schema = ".dev" in current_schema_version
    real_release_cases = [
        name
        for name in counts.case_names
        if (
            name == _DEV_CYCLE_RELEASE_TEST_BASE
            or name.startswith(f"{_DEV_CYCLE_RELEASE_TEST_BASE}[")
        )
        and name != _DEV_CYCLE_RELEASE_SKIP_CASE_NAME
    ]
    unexpected_skips: list[tuple[str, str]] = []
    dev_release_skips = 0
    for case_name, reason in counts.skipped_cases:
        if (
            suite.allow_dev_release_skip
            and is_dev_schema
            and not real_release_cases
            and case_name == _DEV_CYCLE_RELEASE_SKIP_CASE_NAME
            and reason == _DEV_CYCLE_RELEASE_SKIP_REASON
        ):
            dev_release_skips += 1
            continue
        unexpected_skips.append((case_name, reason))

    if dev_release_skips > 1:
        violations.append(
            f"{suite.name}: expected exactly 1 dev-cycle empty-parameter skip for "
            f"{_DEV_CYCLE_RELEASE_SKIP_CASE_NAME}, observed {dev_release_skips}"
        )

    for case_name, reason in unexpected_skips:
        violations.append(
            f"{suite.name}: unexpected skipped test {case_name!r} ({reason})"
        )

    return violations


def collect_optional_provenance(
    python_executable: str = sys.executable,
    *,
    repo_root: Path = REPO_ROOT,
    env: Mapping[str, str] | None = None,
) -> dict[str, str]:
    """Inspect imported optional-extras provenance and parse shipped sample config.

    Runs inside an isolated temporary working directory so local source trees
    cannot shadow the installed virtual environment under test.

    Parameters
    ----------
    python_executable : str, optional
        Python interpreter used to run the provenance probe.
    repo_root : Path, optional
        Repository root containing ``src/supy/sample_data/sample_config.yml``.
    env : Mapping[str, str] | None, optional
        Optional environment override for the probe subprocess.

    Returns
    -------
    dict[str, str]
        Mapping of interpreter, ``supy``, ``suews_bridge``, ``cf_units``,
        ``pvlib``, ``metpy``, and schema provenance fields.

    Raises
    ------
    RuntimeError
        If any required package fails to import or the shipped sample config
        fails to parse under ``CURRENT_SCHEMA_VERSION``.
    """
    sample_config = (
        repo_root.resolve() / "src" / "supy" / "sample_data" / "sample_config.yml"
    )
    probe = (
        "import json, sys\n"
        "from pathlib import Path\n"
        "import cf_units, metpy, pvlib, supy\n"
        "import supy.suews_bridge as sb\n"
        "from supy.data_model.configuration import CURRENT_SCHEMA_VERSION\n"
        "from supy.data_model.core.config import SUEWSConfig\n"
        "sample_path = Path(sys.argv[1])\n"
        "cfg = SUEWSConfig.from_yaml(str(sample_path))\n"
        "if cfg.schema_version != CURRENT_SCHEMA_VERSION:\n"
        "    raise SystemExit(\n"
        "        f'sample_config.yml schema_version {cfg.schema_version!r} '\n"
        "        f'!= CURRENT_SCHEMA_VERSION {CURRENT_SCHEMA_VERSION!r}'\n"
        "    )\n"
        "print(json.dumps({\n"
        "    'python': sys.executable,\n"
        "    'supy_version': str(getattr(supy, '__version__', '<unknown>')),\n"
        "    'supy_file': str(getattr(supy, '__file__', '<unknown>')),\n"
        "    'suews_bridge_file': str(getattr(sb, '__file__', '<unknown>')),\n"
        "    'cf_units_version': str(getattr(cf_units, '__version__', '<unknown>')),\n"
        "    'pvlib_version': str(getattr(pvlib, '__version__', '<unknown>')),\n"
        "    'metpy_version': str(getattr(metpy, '__version__', '<unknown>')),\n"
        "    'current_schema_version': str(CURRENT_SCHEMA_VERSION),\n"
        "    'sample_config_schema_version': str(cfg.schema_version),\n"
        "}))\n"
    )
    run_env = dict(os.environ if env is None else env)
    run_env["MPLBACKEND"] = "Agg"
    run_env["PYTHONUTF8"] = "1"

    with tempfile.TemporaryDirectory(prefix="suews-optional-prov-") as tmp_dir:
        proc = subprocess.run(
            [python_executable, "-c", probe, str(sample_config)],
            cwd=tmp_dir,
            env=run_env,
            capture_output=True,
            text=True,
            check=False,
        )
    if proc.returncode != 0:
        detail = proc.stderr.strip() or proc.stdout.strip()
        raise RuntimeError(
            f"Optional-extras provenance probe failed (exit {proc.returncode}): {detail}"
        )
    lines = [line.strip() for line in proc.stdout.splitlines() if line.strip()]
    if not lines:
        raise RuntimeError("Optional-extras provenance probe emitted no output.")
    return json.loads(lines[-1])


def verify_provenance_prefix(
    provenance: Mapping[str, str],
    require_prefix: Path,
) -> None:
    """Ensure ``supy`` and ``supy.suews_bridge`` were loaded inside ``require_prefix``.

    Parameters
    ----------
    provenance : Mapping[str, str]
        Output from :func:`collect_optional_provenance`.
    require_prefix : Path
        Expected virtual environment prefix directory.

    Raises
    ------
    RuntimeError
        If either ``supy_file`` or ``suews_bridge_file`` resolves outside
        ``require_prefix``.
    """
    expected_root = require_prefix.resolve()
    for key in ("supy_file", "suews_bridge_file"):
        raw = provenance.get(key, "")
        if not raw or raw == "<unknown>":
            raise RuntimeError(f"Package provenance missing {key}: {provenance}")
        resolved = Path(raw).resolve()
        if not resolved.is_relative_to(expected_root):
            raise RuntimeError(
                f"Expected {key} inside {expected_root}, got stale/external path: {resolved}"
            )


def run_suite(
    suite: OptionalTestSuite,
    *,
    repo_root: Path,
    current_schema_version: str,
    spartacus_bin: Path,
    python_executable: str = sys.executable,
) -> SuiteRunResult:
    """Run one pytest suite with JUnit XML capture and enforce zero silent skips.

    Parameters
    ----------
    suite : OptionalTestSuite
        Suite definition to execute.
    repo_root : Path
        Repository root directory.
    current_schema_version : str
        Live ``CURRENT_SCHEMA_VERSION`` string.
    spartacus_bin : Path
        Path to the standalone ``spartacus_surface`` executable.
    python_executable : str, optional
        Python interpreter used to invoke pytest.

    Returns
    -------
    SuiteRunResult
        Execution outcome, parsed counts, and any zero-skip violations.
    """
    env = dict(os.environ)
    py_bin_dir = str(Path(os.path.abspath(python_executable)).parent)
    existing_path = env.get("PATH", "")
    env["PATH"] = (
        f"{py_bin_dir}{os.pathsep}{existing_path}" if existing_path else py_bin_dir
    )
    env["MPLBACKEND"] = "Agg"
    env["PYTHONUTF8"] = "1"
    env["SUEWS_STANDALONE_SPARTACUS"] = str(spartacus_bin.resolve())

    start = time.monotonic()
    with tempfile.TemporaryDirectory(prefix=f"suews-opt-{suite.name}-") as tmp_dir:
        junit_path = Path(tmp_dir) / f"{suite.name}.xml"
        cmd = [
            python_executable,
            "-m",
            "pytest",
            "-v",
            f"--junitxml={junit_path}",
            *suite.targets,
        ]
        proc = subprocess.run(
            cmd,
            cwd=str(repo_root.resolve()),
            env=env,
            capture_output=True,
            text=True,
            check=False,
        )
        elapsed = time.monotonic() - start
        if not junit_path.is_file():
            return SuiteRunResult(
                suite=suite,
                returncode=proc.returncode or 1,
                elapsed_seconds=elapsed,
                counts=None,
                violations=(f"{suite.name}: pytest did not write JUnit XML output",),
                stdout=proc.stdout,
                stderr=proc.stderr,
            )
        counts = parse_junit_xml(junit_path)
        violations = evaluate_suite_junit(
            suite,
            counts,
            current_schema_version=current_schema_version,
        )
        return SuiteRunResult(
            suite=suite,
            returncode=proc.returncode,
            elapsed_seconds=elapsed,
            counts=counts,
            violations=tuple(violations),
            stdout=proc.stdout,
            stderr=proc.stderr,
        )


def format_markdown_summary(
    results: Sequence[SuiteRunResult],
    *,
    current_schema_version: str,
) -> str:
    """Format a compact Markdown table summarising optional-extras suite outcomes.

    Parameters
    ----------
    results : Sequence[SuiteRunResult]
        Completed suite results.
    current_schema_version : str
        Live ``CURRENT_SCHEMA_VERSION`` string.

    Returns
    -------
    str
        ASCII-only Markdown summary suitable for ``$GITHUB_STEP_SUMMARY``.
    """
    lines = [
        "## Optional extras and standalone SPARTACUS test summary",
        "",
        f"Schema version: `{current_schema_version}`",
        "",
        "| Suite | Passed | Skipped | Failed | Elapsed (s) | Status |",
        "| --- | ---: | ---: | ---: | ---: | --- |",
    ]
    for res in results:
        if res.counts is None:
            lines.append(
                f"| `{res.suite.name}` | 0 | 0 | 1 | {res.elapsed_seconds:.2f} | FAIL |"
            )
        else:
            status = "PASS" if res.passed else "FAIL"
            failed = res.counts.failed + res.counts.errors
            lines.append(
                f"| `{res.suite.name}` | {res.counts.passed} | {res.counts.skipped} "
                f"| {failed} | {res.elapsed_seconds:.2f} | {status} |"
            )
    lines.append("")
    return "\n".join(lines)


def build_parser() -> argparse.ArgumentParser:
    """Create the CLI argument parser.

    Returns
    -------
    argparse.ArgumentParser
        Configured CLI parser.
    """
    parser = argparse.ArgumentParser(
        description=(
            "Run optional-dependency, release-compat, and standalone SPARTACUS "
            "tests with strict zero-silent-skip enforcement."
        )
    )
    parser.add_argument(
        "--suite",
        action="append",
        dest="suites",
        choices=tuple(SUITE_BY_NAME),
        default=[],
        help="Specific suite to execute (may be repeated; defaults to all suites).",
    )
    parser.add_argument(
        "--repo-root",
        type=Path,
        default=REPO_ROOT,
        help="Repository root directory (default: %(default)s).",
    )
    parser.add_argument(
        "--spartacus-bin",
        type=Path,
        default=None,
        help=(
            "Path to standalone spartacus_surface executable "
            "(defaults to SUEWS_STANDALONE_SPARTACUS or vendor bin path)."
        ),
    )
    parser.add_argument(
        "--require-prefix",
        type=Path,
        default=None,
        help="Require imported supy and supy.suews_bridge files to reside under this directory.",
    )
    parser.add_argument(
        "--summary-md",
        type=Path,
        default=None,
        help="Optional path to append a Markdown summary table (e.g. $GITHUB_STEP_SUMMARY).",
    )
    parser.add_argument(
        "--list",
        action="store_true",
        help="Print configured suites and targets and exit without running.",
    )
    return parser


def _resolve_spartacus_binary(explicit: Path | None, repo_root: Path) -> Path:
    """Resolve the standalone SPARTACUS executable path."""
    if explicit is not None:
        return explicit.resolve()
    env_bin = os.environ.get("SUEWS_STANDALONE_SPARTACUS")
    if env_bin:
        return Path(env_bin).resolve()
    return (
        repo_root.resolve()
        / "src"
        / "suews"
        / "ext_lib"
        / "spartacus-surface"
        / "bin"
        / "spartacus_surface"
    )


def main(argv: Sequence[str] | None = None) -> int:
    """Run the optional-extras zero-skip verification CLI.

    Parameters
    ----------
    argv : Sequence[str] | None, optional
        Explicit CLI arguments; defaults to ``sys.argv[1:]``.

    Returns
    -------
    int
        Exit status (``0`` if every suite passed without unexpected skips, ``1`` otherwise).
    """
    args = build_parser().parse_args(argv)
    selected = (
        [SUITE_BY_NAME[name] for name in args.suites]
        if args.suites
        else list(OPTIONAL_TEST_SUITES)
    )

    if args.list:
        for suite in selected:
            print(f"{suite.name} (min_expected={suite.min_expected_passed}):")
            for target in suite.targets:
                print(f"  - {target}")
        return 0

    spartacus_bin = _resolve_spartacus_binary(args.spartacus_bin, args.repo_root)
    try:
        if any(s.name == "standalone_spartacus" for s in selected) and not (
            spartacus_bin.is_file() and os.access(spartacus_bin, os.X_OK)
        ):
            raise RuntimeError(
                f"Standalone SPARTACUS executable missing or not executable: {spartacus_bin}"
            )
        provenance = collect_optional_provenance(
            sys.executable, repo_root=args.repo_root
        )
        if args.require_prefix is not None:
            verify_provenance_prefix(provenance, args.require_prefix)
    except RuntimeError as exc:
        print(f"[X] {exc}", file=sys.stderr)
        return 1

    print("Optional-extras environment provenance:")
    print(f"  python:                 {provenance['python']}")
    print(f"  supy.__version__:       {provenance['supy_version']}")
    print(f"  supy.__file__:          {provenance['supy_file']}")
    print(f"  suews_bridge:           {provenance['suews_bridge_file']}")
    print(f"  cf_units.__version__:   {provenance['cf_units_version']}")
    print(f"  pvlib.__version__:      {provenance['pvlib_version']}")
    print(f"  metpy.__version__:      {provenance['metpy_version']}")
    print(f"  CURRENT_SCHEMA_VERSION: {provenance['current_schema_version']}")
    print(f"  spartacus_surface:      {spartacus_bin}")

    results: list[SuiteRunResult] = []
    failed_suites: list[SuiteRunResult] = []
    for suite in selected:
        res = run_suite(
            suite,
            repo_root=args.repo_root,
            current_schema_version=provenance["current_schema_version"],
            spartacus_bin=spartacus_bin,
        )
        results.append(res)
        if res.passed and res.counts is not None:
            print(
                f"[OK] {suite.name}: {res.counts.passed} passed, "
                f"{res.counts.skipped} skipped ({res.elapsed_seconds:.2f}s)"
            )
        else:
            failed_suites.append(res)
            print(
                f"[X] {suite.name} failed (exit {res.returncode}, {res.elapsed_seconds:.2f}s)",
                file=sys.stderr,
            )
            for msg in res.violations:
                print(f"  - {msg}", file=sys.stderr)
            if res.counts is not None:
                for case_name, detail in res.counts.failed_cases[:10]:
                    print(f"  - failure {case_name}: {detail}", file=sys.stderr)
            if res.stdout.strip():
                tail = "\n".join(res.stdout.strip().splitlines()[-40:])
                print(f"--- stdout ({suite.name}) ---\n{tail}", file=sys.stderr)
            if res.stderr.strip():
                tail = "\n".join(res.stderr.strip().splitlines()[-30:])
                print(f"--- stderr ({suite.name}) ---\n{tail}", file=sys.stderr)

    if args.summary_md is not None:
        summary_text = format_markdown_summary(
            results,
            current_schema_version=provenance["current_schema_version"],
        )
        with args.summary_md.open("a", encoding="utf-8") as fh:
            fh.write(summary_text)

    total_passed = sum(r.counts.passed for r in results if r.counts is not None)
    total_skipped = sum(r.counts.skipped for r in results if r.counts is not None)
    print(
        f"Optional-extras summary: {len(results) - len(failed_suites)}/{len(results)} "
        f"suites passed ({total_passed} passed, {total_skipped} skipped)."
    )
    return 1 if failed_suites else 0


if __name__ == "__main__":
    raise SystemExit(main())
