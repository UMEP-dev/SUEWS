#!/usr/bin/env python3
"""Summarise pytest JUnit XML from the nightly no-trap -finit-real=snan pilot.

Parses the JUnit XML written by ``pytest -m physics`` under a no-trap
``SUEWS_INIT_REAL=snan`` build, flags failures whose traceback mentions NaN or
non-finite values, writes an ASCII Markdown summary for ``$GITHUB_STEP_SUMMARY``
(and report artefacts), and optionally emits GitHub Actions warning annotations.

Exits 1 if the JUnit XML is missing, malformed, or recorded zero tests (so a
broken build or aborted pytest run never reads as clean). When tests ran to
completion, exits 0 in advisory pilot mode and 1 if ``--strict`` is passed with
any failure or error.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
import re
import sys
import xml.etree.ElementTree as ET

_NAN_SIGNAL_RE = re.compile(
    r"\b(?:nan|snan|qnan|non-finite|not finite|isfinite)\b|code 110",
    re.I,
)


@dataclass(frozen=True)
class FailedCase:
    """One failing or errored pytest case from JUnit XML."""

    node: str
    kind: str
    nan_related: bool
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
            name = case.attrib.get("name", "<unknown>")
            node = f"{classname}::{name}" if classname else name
            message = element.attrib.get("message", "")
            body = element.text or ""
            cases.append(
                FailedCase(
                    node=node,
                    kind=tag,
                    nan_related=bool(_NAN_SIGNAL_RE.search(f"{message}\n{body}")),
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


def render_markdown(
    summary: SuiteSummary,
    *,
    compiler: str = "",
    build_profile: str = "release",
) -> str:
    """Render an ASCII Markdown report suitable for ``$GITHUB_STEP_SUMMARY``."""
    nan_count = sum(1 for case in summary.failed_cases if case.nan_related)
    lines = [
        "## Fortran no-trap `-finit-real=snan` physics pilot",
        "",
        f"- **Build profile:** `{build_profile}` with `SUEWS_INIT_REAL=snan` (no `-ffpe-trap=invalid`)",
    ]
    if compiler:
        lines.append(f"- **Compiler:** `{compiler}`")
    lines.extend([
        (
            f"- **Results:** {summary.tests} tests -> "
            f"{summary.passed} passed, {summary.failures} failed, "
            f"{summary.errors} errors, {summary.skipped} skipped/xfailed "
            f"({summary.elapsed_s:.1f}s)"
        ),
        f"- **NaN / non-finite tagged failures:** {nan_count}",
        "",
    ])

    if summary.failed_cases:
        lines.extend([
            "| Test | Kind | NaN/non-finite | Summary |",
            "|---|---|---|---|",
        ])
        for case in summary.failed_cases:
            flag = "yes" if case.nan_related else "no"
            safe_summary = case.summary.replace("|", "\\|")
            lines.append(f"| `{case.node}` | {case.kind} | {flag} | {safe_summary} |")
        lines.extend([
            "",
            "Known Wave 4 tracked items exposed by sNaN initialisation: "
            "`#2105` (`MomentumRoughnessMethod=5`, `z0m_zh`/`zdm_zh`) and "
            "`#2106` (`SnowUse=1` + `NetRadiationMethod` 13/1003, `tsurf_0_K`).",
        ])
    else:
        lines.append(
            "[OK] All `-m physics` tests passed under no-trap `-finit-real=snan`."
        )

    return "\n".join(lines) + "\n"


def main(argv: list[str] | None = None) -> int:
    """Parse JUnit XML, write summary Markdown, and apply optional strict gate."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("junit_xml", help="path to pytest JUnit XML file")
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
        "--github-annotations",
        action="store_true",
        help="emit GitHub Actions ::warning annotations for each failure",
    )
    parser.add_argument(
        "--strict",
        action="store_true",
        help="exit 1 if any test failed or errored",
    )
    args = parser.parse_args(sys.argv[1:] if argv is None else argv[1:])

    xml_path = Path(args.junit_xml)
    if not xml_path.is_file():
        print(f"[X] JUnit XML not found: {xml_path}", file=sys.stderr)
        return 1

    try:
        summary = parse_junit_xml(xml_path)
    except (ET.ParseError, ValueError) as exc:
        print(f"[X] failed to parse {xml_path}: {exc}", file=sys.stderr)
        return 1

    if summary.tests == 0:
        print(f"[X] JUnit XML {xml_path} recorded zero tests", file=sys.stderr)
        return 1

    markdown = render_markdown(
        summary,
        compiler=args.compiler,
        build_profile=args.build_profile,
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
            print(
                f"::warning title=Fortran sNaN pilot ({tag})::"
                f"{case.node}: {case.summary}"
            )

    if args.strict and (summary.failures > 0 or summary.errors > 0):
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
