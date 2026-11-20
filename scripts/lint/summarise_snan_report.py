#!/usr/bin/env python3
"""Summarise pytest JUnit XML from the nightly no-trap -finit-real=snan pilot.

Parses the JUnit XML written by ``pytest -m physics`` under a no-trap
``SUEWS_INIT_REAL=snan`` build, flags failures whose traceback mentions NaN or
non-finite values, writes an ASCII Markdown summary for ``$GITHUB_STEP_SUMMARY``
(and report artefacts), and optionally emits GitHub Actions annotations.

Always exits 1 (even in default advisory pilot mode) if:

- the JUnit XML is missing, malformed, or recorded zero tests;
- zero non-skipped test bodies executed (for example an all-skipped run or a
  collection/import error before any test ran);
- any JUnit ``<error>`` occurred (collection failure, fixture setup/teardown
  error, or pytest-xdist worker crash even in a mixed pass/error run);
- ``--pytest-exit-code`` is outside ``{0, 1}`` (pytest exit codes 2, 3, 4, 5,
  or signal exits such as SIGSEGV/SIGFPE), or ``1`` with zero recorded failures
  and zero recorded errors; or
- any JUnit ``<failure>`` is outside the expected-failures allowlist (preventing
  new regressions from hiding silently in nightly runs).

Only failures explicitly listed in the expected-failures allowlist (currently
known Wave 4 ``#2105``) stay advisory (exit 0) in default pilot mode; passing
``--strict`` exits 1 on every failure including allowlisted entries.
"""

from __future__ import annotations

import argparse
from collections.abc import Mapping
from dataclasses import dataclass
import json
from pathlib import Path
import re
import sys
import xml.etree.ElementTree as ET

DEFAULT_EXPECTED_FAILURES_PATH = (
    Path(__file__).resolve().with_name("fortran_snan_expected_failures.json")
)

_NAN_SIGNAL_RE = re.compile(
    r"\b(?:nan|snan|qnan|non-finite|not finite|isfinite)\b|code 110",
    re.I,
)
_COLLECTION_ERROR_RE = re.compile(r"collection failure", re.I)


@dataclass(frozen=True)
class ExpectedFailure:
    """One tracked physics test failure allowlisted while its Wave 4 fix is open."""

    node: str
    issue: str
    reason: str = ""


@dataclass(frozen=True)
class FailedCase:
    """One failing or errored pytest case from JUnit XML."""

    node: str
    kind: str
    nan_related: bool
    is_collection_error: bool
    summary: str


@dataclass(frozen=True)
class SuiteSummary:
    """Aggregate counts and failures parsed from a pytest JUnit XML report."""

    tests: int
    passed: int
    failures: int
    errors: int
    skipped: int
    elapsed_s: float
    failed_cases: tuple[FailedCase, ...]

    @property
    def executed(self) -> int:
        """Number of non-skipped test bodies that actually ran."""
        return self.passed + self.failures

    @property
    def has_collection_error(self) -> bool:
        """Whether any recorded JUnit error represents a pytest collection failure."""
        return any(case.is_collection_error for case in self.failed_cases)

    @property
    def has_any_error(self) -> bool:
        """Whether any JUnit error occurred (collection, setup/teardown, xdist crash)."""
        return self.errors > 0 or any(
            case.kind == "error" for case in self.failed_cases
        )


def load_expected_failures(path: Path) -> dict[str, ExpectedFailure]:
    """Load the allowlisted known-failure map keyed by JUnit ``classname::name``."""
    payload = json.loads(path.read_text(encoding="utf-8"))
    raw_entries = payload.get("entries")
    if not isinstance(raw_entries, list):
        raise ValueError(
            f"Expected-failures file {path} must contain an 'entries' list"
        )
    expected: dict[str, ExpectedFailure] = {}
    for item in raw_entries:
        if not isinstance(item, dict) or not item.get("node") or not item.get("issue"):
            raise ValueError(
                f"Each entry in {path} must define non-empty 'node' and 'issue' strings"
            )
        entry = ExpectedFailure(
            node=str(item["node"]),
            issue=str(item["issue"]),
            reason=str(item.get("reason", "")),
        )
        expected[entry.node] = entry
    return expected


def _first_line(text: str, limit: int = 200) -> str:
    """Return a single ASCII-safe summary line from a failure message."""
    for raw in text.splitlines():
        stripped = raw.strip()
        if stripped:
            ascii_text = stripped.encode("ascii", errors="replace").decode("ascii")
            return ascii_text[:limit]
    return "(no failure message)"


def _extract_failed_cases(suite: ET.Element) -> list[FailedCase]:
    """Return every failing or errored testcase inside one ``<testsuite>``."""
    cases: list[FailedCase] = []
    for case in suite.iter("testcase"):
        for tag in ("failure", "error"):
            element = case.find(tag)
            if element is None:
                continue
            classname = case.attrib.get("classname", "")
            raw_name = case.attrib.get("name", "")
            name = raw_name or "<collection>"
            node = f"{classname}::{name}" if classname else name
            message = element.attrib.get("message", "")
            body = element.text or ""
            is_collection = tag == "error" and (
                not raw_name or bool(_COLLECTION_ERROR_RE.search(message))
            )
            cases.append(
                FailedCase(
                    node=node,
                    kind=tag,
                    nan_related=bool(_NAN_SIGNAL_RE.search(f"{message}\n{body}")),
                    is_collection_error=is_collection,
                    summary=_first_line(message or body),
                )
            )
    return cases


def parse_junit_xml(path: Path) -> SuiteSummary:
    """Parse pytest JUnit XML into aggregate counts and failed cases."""
    tree = ET.parse(path)
    root = tree.getroot()
    suites = [root] if root.tag == "testsuite" else list(root.iter("testsuite"))
    if not suites:
        raise ValueError(f"No <testsuite> elements found in {path}")

    tests = 0
    failures = 0
    errors = 0
    skipped = 0
    elapsed_s = 0.0
    failed_cases: list[FailedCase] = []

    for suite in suites:
        tests += int(suite.attrib.get("tests", "0"))
        failures += int(suite.attrib.get("failures", "0"))
        errors += int(suite.attrib.get("errors", "0"))
        skipped += int(suite.attrib.get("skipped", "0"))
        elapsed_s += float(suite.attrib.get("time", "0") or 0.0)
        failed_cases.extend(_extract_failed_cases(suite))

    passed = max(0, tests - failures - errors - skipped)
    return SuiteSummary(
        tests=tests,
        passed=passed,
        failures=failures,
        errors=errors,
        skipped=skipped,
        elapsed_s=elapsed_s,
        failed_cases=tuple(failed_cases),
    )


def classify_failures(
    summary: SuiteSummary,
    expected_failures: Mapping[str, ExpectedFailure],
) -> tuple[list[FailedCase], list[FailedCase], list[ExpectedFailure]]:
    """Split JUnit ``<failure>`` cases into ``(allowlisted, unexpected, resolved)``."""
    failure_cases = [case for case in summary.failed_cases if case.kind == "failure"]
    allowlisted = [case for case in failure_cases if case.node in expected_failures]
    unexpected = [case for case in failure_cases if case.node not in expected_failures]
    seen_nodes = {case.node for case in failure_cases}
    resolved = [
        entry for node, entry in expected_failures.items() if node not in seen_nodes
    ]
    return allowlisted, unexpected, resolved


def render_markdown(
    summary: SuiteSummary,
    *,
    compiler: str = "",
    build_profile: str = "release",
    pytest_exit_code: int | None = None,
    expected_failures: Mapping[str, ExpectedFailure] | None = None,
) -> str:
    """Render an ASCII Markdown report suitable for ``$GITHUB_STEP_SUMMARY``."""
    expected_map = {} if expected_failures is None else expected_failures
    allowlisted, unexpected, resolved = classify_failures(summary, expected_map)
    nan_count = sum(1 for case in summary.failed_cases if case.nan_related)
    lines = [
        "## Fortran no-trap `-finit-real=snan` physics pilot",
        "",
        f"- **Build profile:** `{build_profile}` with `SUEWS_INIT_REAL=snan` (no `-ffpe-trap=invalid`)",
    ]
    if compiler:
        lines.append(f"- **Compiler:** `{compiler}`")
    if pytest_exit_code is not None:
        lines.append(f"- **Pytest exit code:** `{pytest_exit_code}`")
    lines.extend([
        (
            f"- **Results:** {summary.tests} recorded -> "
            f"{summary.passed} passed, {summary.failures} failed "
            f"({len(allowlisted)} allowlisted, {len(unexpected)} unexpected), "
            f"{summary.errors} errors, {summary.skipped} skipped/xfailed "
            f"({summary.elapsed_s:.1f}s)"
        ),
        f"- **NaN / non-finite tagged failures:** {nan_count}",
        "",
    ])

    if summary.failed_cases:
        lines.extend([
            "| Test | Kind | Status | NaN/non-finite | Summary |",
            "|---|---|---|---|---|",
        ])
        for case in summary.failed_cases:
            flag = "yes" if case.nan_related else "no"
            if case.kind == "error":
                status = "hard-error"
            elif case.node in expected_map:
                status = f"allowlisted ({expected_map[case.node].issue})"
            else:
                status = "unexpected"
            safe_summary = case.summary.replace("|", "\\|")
            lines.append(
                f"| `{case.node}` | {case.kind} | {status} | {flag} | {safe_summary} |"
            )
        lines.extend([
            "",
            "Known Wave 4 tracked items exposed by sNaN initialisation: "
            "`#2105` (`MomentumRoughnessMethod=5`, `z0m_zh`/`zdm_zh`) and "
            "`#2106` (`SnowUse=1` + `NetRadiationMethod` 13/1003, `tsurf_0_K`).",
        ])
    elif summary.executed == 0:
        lines.append(
            "[X] No physics tests executed under no-trap `-finit-real=snan` "
            f"({summary.tests} recorded, {summary.skipped} skipped/xfailed)."
        )
    else:
        lines.append(
            "[OK] All `-m physics` tests passed under no-trap `-finit-real=snan`."
        )

    if resolved and summary.executed > 0 and not summary.has_any_error:
        lines.append("")
        for entry in resolved:
            lines.append(
                f"[!] Allowlisted failure `{entry.node}` ({entry.issue}) did not "
                "fail in this run; consider shrinking the allowlist or enabling `--strict`."
            )

    return "\n".join(lines) + "\n"


def _evaluate_hard_failure(
    summary: SuiteSummary,
    xml_path: Path,
    pytest_exit_code: int | None,
    unexpected_failures: list[FailedCase],
) -> str | None:
    """Return an error reason if the run must fail hard even in advisory mode."""
    recorded_failures = [c for c in summary.failed_cases if c.kind == "failure"]
    reason: str | None = None
    if pytest_exit_code is not None and pytest_exit_code not in {0, 1}:
        reason = (
            f"pytest exited abnormally with code {pytest_exit_code} "
            "(only 0 or 1 is admissible in advisory mode)"
        )
    elif summary.tests == 0:
        reason = f"JUnit XML {xml_path} recorded zero tests"
    elif summary.has_collection_error:
        reason = f"pytest reported collection error(s) in {xml_path}"
    elif summary.has_any_error:
        error_count = max(
            summary.errors,
            sum(1 for case in summary.failed_cases if case.kind == "error"),
        )
        reason = (
            f"pytest recorded {error_count} JUnit <error> record(s) "
            f"(infrastructure/setup/xdist worker crash) in {xml_path}"
        )
    elif summary.executed == 0:
        reason = (
            f"No physics tests executed in {xml_path} "
            f"({summary.tests} recorded, {summary.errors} errors, "
            f"{summary.skipped} skipped/xfailed)"
        )
    elif pytest_exit_code == 1 and summary.failures == 0 and summary.errors == 0:
        reason = (
            f"pytest exited with code 1 but {xml_path} recorded 0 failures and 0 errors"
        )
    elif summary.failures > 0 and not recorded_failures:
        reason = (
            f"{xml_path} header reported {summary.failures} failure(s) without "
            "<failure> testcase entries to match against the allowlist"
        )
    elif unexpected_failures:
        nodes = ", ".join(case.node for case in unexpected_failures)
        reason = (
            f"{len(unexpected_failures)} unexpected physics failure(s) outside "
            f"the sNaN allowlist: {nodes}"
        )
    return reason


def _build_parser() -> argparse.ArgumentParser:
    """Create the CLI argument parser."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("junit_xml", help="path to pytest JUnit XML file")
    parser.add_argument(
        "--expected-failures",
        default=None,
        help=(
            "path to JSON allowlist of known advisory failures "
            f"(default: {DEFAULT_EXPECTED_FAILURES_PATH.name} beside this script)"
        ),
    )
    parser.add_argument(
        "--markdown",
        action="append",
        default=[],
        help="write Markdown summary to this path (may be passed multiple times)",
    )
    parser.add_argument(
        "--append-summary",
        default=None,
        help="append Markdown summary to this file (e.g. $GITHUB_STEP_SUMMARY)",
    )
    parser.add_argument("--compiler", default="", help="compiler version line")
    parser.add_argument(
        "--build-profile", default="release", help="SUEWS_BUILD_PROFILE used"
    )
    parser.add_argument(
        "--pytest-exit-code",
        type=int,
        default=None,
        help="raw exit code returned by pytest (non-0/1 always fails hard)",
    )
    parser.add_argument(
        "--github-annotations",
        action="store_true",
        help="emit GitHub Actions ::warning/::error annotations for each failure",
    )
    parser.add_argument(
        "--strict",
        action="store_true",
        help="exit 1 if any test failed or errored (including allowlisted entries)",
    )
    return parser


def _load_inputs(
    xml_path: Path, expected_path: Path
) -> tuple[SuiteSummary, dict[str, ExpectedFailure]]:
    """Load and validate both the JUnit XML report and the expected-failures allowlist."""
    if not xml_path.is_file():
        raise FileNotFoundError(f"JUnit XML not found: {xml_path}")
    if not expected_path.is_file():
        raise FileNotFoundError(
            f"expected-failures allowlist not found: {expected_path}"
        )
    expected_failures = load_expected_failures(expected_path)
    summary = parse_junit_xml(xml_path)
    return summary, expected_failures


def main(argv: list[str] | None = None) -> int:
    """Parse JUnit XML, write summary Markdown, and apply hard/allowlist/strict gates."""
    args = _build_parser().parse_args(sys.argv[1:] if argv is None else argv[1:])

    xml_path = Path(args.junit_xml)
    expected_path = (
        Path(args.expected_failures)
        if args.expected_failures is not None
        else DEFAULT_EXPECTED_FAILURES_PATH
    )
    try:
        summary, expected_failures = _load_inputs(xml_path, expected_path)
    except (OSError, ValueError, json.JSONDecodeError, ET.ParseError) as exc:
        print(f"[X] {exc}", file=sys.stderr)
        return 1

    _allowlisted, unexpected, _resolved = classify_failures(summary, expected_failures)

    markdown = render_markdown(
        summary,
        compiler=args.compiler,
        build_profile=args.build_profile,
        pytest_exit_code=args.pytest_exit_code,
        expected_failures=expected_failures,
    )
    print(markdown, end="")

    for out_path in args.markdown:
        target = Path(out_path)
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(markdown, encoding="utf-8")

    if args.append_summary:
        with Path(args.append_summary).open("a", encoding="utf-8") as handle:
            handle.write(markdown)

    if args.github_annotations:
        for case in summary.failed_cases:
            tag = "NaN/non-finite" if case.nan_related else case.kind
            is_allowlisted = case.kind == "failure" and case.node in expected_failures
            level = "warning" if is_allowlisted and not args.strict else "error"
            print(
                f"::{level} title=Fortran sNaN pilot ({tag})::"
                f"{case.node}: {case.summary}"
            )

    hard_failure = _evaluate_hard_failure(
        summary, xml_path, args.pytest_exit_code, unexpected
    )
    if hard_failure is not None:
        print(f"[X] {hard_failure}", file=sys.stderr)
        return 1

    if args.strict and (summary.failures > 0 or summary.errors > 0):
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
