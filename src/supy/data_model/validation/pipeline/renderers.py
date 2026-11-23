"""Pure text-report rendering from structured ``ValidationReport`` objects.

Every consolidated text report (``report_<name>.txt``) is rendered directly
from structured :class:`PhaseReport` / :class:`ValidationReport` issues without
scraping intermediate ``.txt`` files via regular expressions or line parsers.
"""

from __future__ import annotations

from collections.abc import Sequence

from .report_schema import (
    SEVERITY_APPLIED_FIX,
    SEVERITY_ERROR,
    SEVERITY_INFO,
    SEVERITY_SUGGESTION,
    SEVERITY_WARNING,
    Issue,
    PhaseReport,
    ValidationReport,
)
from .report_writer import STOPPING_PHASE_PREFIX, VALIDATION_PHASE_NAMES

REPORT_TITLE = "SUEWS Validation Report"
_BORDER = "# " + "=" * 50

_PHASE_PASS_MESSAGES: dict[str, str] = {
    "A": "Configuration structure check passed",
    "B": "Physics validation check passed",
    "C": "Configuration consistency check passed",
    "AB": "Configuration structure check and Physics validation check passed",
    "AC": "Configuration structure check and Configuration consistency check passed",
    "BC": "Physics validation check and Configuration consistency check passed",
    "ABC": "All validation checks passed",
}


def _normalise_phases(
    report: ValidationReport | PhaseReport | Sequence[PhaseReport],
) -> list[PhaseReport]:
    if isinstance(report, ValidationReport):
        return list(report.phases)
    if isinstance(report, PhaseReport):
        return [report]
    return list(report)


def _site_suffix(issue: Issue) -> str:
    if issue.site_gridid is not None:
        return f" at site [{issue.site_gridid}]"
    return ""


def _issue_label(issue: Issue) -> str:
    if issue.yaml_path:
        return issue.yaml_path
    if issue.code:
        return issue.code
    return "configuration"


def _format_rename_detail(issue: Issue) -> str:
    """Render an ``A.RENAMED_PARAM`` issue as ``-- old changed to new``."""
    msg = issue.message
    if msg.startswith("Renamed ") and " -> " in msg:
        old_key, new_key = msg.removeprefix("Renamed ").split(" -> ", 1)
        return f"-- {old_key} changed to {new_key}"
    return f"-- {msg}"


def _render_action_needed(issues: list[Issue]) -> list[str]:
    """Render all blocking ``ERROR`` issues grouped by phase."""
    lines: list[str] = []
    phase_a = [i for i in issues if i.phase == "A"]
    phase_b = [i for i in issues if i.phase == "B"]
    phase_c = [i for i in issues if i.phase == "C"]

    if phase_a:
        lines.append(
            f"- Found ({len(phase_a)}) configuration structure error(s):"
        )
        for issue in phase_a:
            lines.append(f"-- {issue.message}")
            if issue.yaml_path and issue.yaml_path not in issue.message:
                lines.append(f"   Location: {issue.yaml_path}")

    if phase_b:
        lines.append(
            f"- Found ({len(phase_b)}) critical scientific parameter error(s):"
        )
        for issue in phase_b:
            label = _issue_label(issue)
            lines.append(f"-- {label}{_site_suffix(issue)}: {issue.message}")
            if issue.suggested_value is not None:
                lines.append(f"   Suggested fix: {issue.suggested_value}")

    if phase_c:
        lines.append(
            f"- Found ({len(phase_c)}) critical configuration consistency error(s):"
        )
        for issue in phase_c:
            path = issue.yaml_path or ""
            field_name = path.split(".")[-1] if path else (issue.code or "configuration")
            lines.append(f"-- {field_name}: {issue.message}")
            if path and path != "configuration":
                lines.append(f"   Location: {path}")
            if issue.suggested_value is not None:
                lines.append(f"   Suggested fix: {issue.suggested_value}")

    return lines


def _render_review_advised(issues: list[Issue]) -> list[str]:
    """Render all non-blocking ``WARNING`` issues grouped by phase."""
    lines: list[str] = []
    a_missing = [
        i for i in issues if i.phase == "A" and i.code == "A.MISSING_PARAM"
    ]
    a_extra = [
        i for i in issues if i.phase == "A" and i.code == "A.EXTRA_PARAM"
    ]
    a_other = [
        i
        for i in issues
        if i.phase == "A" and i.code not in {"A.MISSING_PARAM", "A.EXTRA_PARAM"}
    ]
    b_warnings = [i for i in issues if i.phase == "B"]
    c_warnings = [i for i in issues if i.phase == "C"]

    if a_missing:
        lines.append(
            f"- Found ({len(a_missing)}) optional missing parameter(s):"
        )
        for issue in a_missing:
            lines.append(f"-- {issue.yaml_path or issue.message}")

    if a_extra:
        lines.append(
            f"- Found ({len(a_extra)}) parameter(s) not in standard:"
        )
        for issue in a_extra:
            lines.append(f"-- {issue.yaml_path or issue.message}")

    if a_other:
        lines.append(
            f"- Review ({len(a_other)}) configuration warning(s):"
        )
        for issue in a_other:
            if issue.yaml_path and issue.yaml_path not in issue.message:
                lines.append(f"-- {issue.yaml_path}: {issue.message}")
            else:
                lines.append(f"-- {issue.message}")

    if b_warnings:
        lines.append(f"- Review ({len(b_warnings)}) scientific warning(s):")
        for issue in b_warnings:
            label = _issue_label(issue)
            lines.append(f"-- {label}{_site_suffix(issue)}: {issue.message}")

    if c_warnings:
        lines.append(
            f"- Review ({len(c_warnings)}) consistency warning(s):"
        )
        for issue in c_warnings:
            label = _issue_label(issue)
            lines.append(f"-- {label}{_site_suffix(issue)}: {issue.message}")

    return lines


def _render_suggested_updates(
    issues: list[Issue], *, science_fixes: str
) -> list[str]:
    """Render Phase B scientific initialisation suggestions."""
    if not issues:
        return []
    lines = [
        f"- Suggested ({len(issues)}) scientific initialisation update(s).",
        (
            "- These suggestions were not written to the updated YAML. They may be "
            "inappropriate for observed initial states, spin-up workflows, historical "
            "timezone settings, or specialist case studies."
        ),
    ]
    if science_fixes == "suggest":
        lines.append(
            "- Re-run with --science-fixes apply to apply them to the updated YAML, "
            "or --science-fixes off to suppress scientific transformation suggestions."
        )
    for issue in issues:
        label = _issue_label(issue)
        lines.append(f"-- {label}{_site_suffix(issue)}: {issue.message}")
    return lines


def _render_applied_updates(
    applied_issues: list[Issue], renamed_issues: list[Issue]
) -> list[str]:
    """Render applied scientific updates and mechanical parameter renames."""
    lines: list[str] = []
    if applied_issues:
        lines.append(f"- Applied ({len(applied_issues)}) scientific update(s):")
        for issue in applied_issues:
            label = _issue_label(issue)
            lines.append(f"-- {label}{_site_suffix(issue)}: {issue.message}")
    if renamed_issues:
        lines.append(
            f"- Updated ({len(renamed_issues)}) renamed parameter(s) to current standards:"
        )
        for issue in renamed_issues:
            lines.append(_format_rename_detail(issue))
    return lines


def _render_info(
    info_issues: list[Issue],
    renamed_issues: list[Issue],
    *,
    include_rename_summary: bool,
) -> list[str]:
    """Render informational notes, applied defaults, and rename summary."""
    lines: list[str] = []
    if include_rename_summary and renamed_issues:
        lines.append(f"- Updated ({len(renamed_issues)}) renamed parameter(s):")
        for issue in renamed_issues:
            lines.append(_format_rename_detail(issue))

    c_infos = [i for i in info_issues if i.phase == "C"]
    other_infos = [i for i in info_issues if i.phase != "C"]

    for issue in c_infos:
        lines.append(f"- {issue.message}")

    if other_infos:
        lines.append(f"- Note ({len(other_infos)}):")
        for issue in other_infos:
            label = _issue_label(issue)
            lines.append(f"-- {label}{_site_suffix(issue)}: {issue.message}")

    return lines


def render_text_report(
    report: ValidationReport | PhaseReport | Sequence[PhaseReport],
    *,
    mode: str = "public",
    science_fixes: str = "suggest",
) -> str:
    """Render a canonical human-readable text report from structured issues.

    Parameters
    ----------
    report : ValidationReport, PhaseReport, or sequence of PhaseReport
        Structured phase findings to render.
    mode : str, default "public"
        Validation mode label shown in the report header (``"public"`` or
        ``"dev"``).
    science_fixes : str, default "suggest"
        Scientific fix policy used during Phase B (controls the CLI hint shown
        under ``## SUGGESTED UPDATES``).

    Returns
    -------
    str
        Complete text report ready to write via ``REPORT_WRITER``.
    """
    phases = _normalise_phases(report)
    all_issues = [issue for phase in phases for issue in phase.issues]

    mode_title = "Public" if mode.lower() == "public" else mode.title()
    failed_phase = next((p.phase for p in phases if p.has_errors), None)
    stopping_stage = VALIDATION_PHASE_NAMES.get(failed_phase)

    lines: list[str] = [
        f"# {REPORT_TITLE}",
        _BORDER,
        f"# Mode: {mode_title}",
    ]
    if stopping_stage is not None:
        lines.append(f"{STOPPING_PHASE_PREFIX} {stopping_stage}")
    lines.append(_BORDER)

    errors = [i for i in all_issues if i.severity == SEVERITY_ERROR]
    warnings = [i for i in all_issues if i.severity == SEVERITY_WARNING]
    suggestions = [i for i in all_issues if i.severity == SEVERITY_SUGGESTION]
    renamed = [
        i
        for i in all_issues
        if i.severity == SEVERITY_APPLIED_FIX and i.code == "A.RENAMED_PARAM"
    ]
    applied_science = [
        i
        for i in all_issues
        if i.severity == SEVERITY_APPLIED_FIX and i.code != "A.RENAMED_PARAM"
    ]
    infos = [i for i in all_issues if i.severity == SEVERITY_INFO]

    sections: list[tuple[str, list[str]]] = [
        ("ACTION NEEDED", _render_action_needed(errors)),
        ("REVIEW ADVISED", _render_review_advised(warnings)),
        (
            "SUGGESTED UPDATES",
            _render_suggested_updates(suggestions, science_fixes=science_fixes),
        ),
        ("APPLIED UPDATES", _render_applied_updates(applied_science, renamed)),
        (
            "INFO",
            _render_info(
                infos,
                renamed,
                include_rename_summary=bool(renamed) and len(phases) > 1,
            ),
        ),
    ]

    has_sections = False
    for heading, body in sections:
        if not body:
            continue
        has_sections = True
        lines.append("")
        lines.append(f"## {heading}")
        lines.extend(body)

    if not has_sections:
        phase_str = "".join(p.phase for p in phases) or "C"
        pass_msg = _PHASE_PASS_MESSAGES.get(phase_str, f"Phase {phase_str} passed")
        lines.append("")
        lines.append(pass_msg)

    lines.append("")
    lines.append(_BORDER)
    return "\n".join(lines) + "\n"
