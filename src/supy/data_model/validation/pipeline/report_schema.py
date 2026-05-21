"""Canonical machine-readable schema for validator phase output.

Every phase of the validation pipeline (A: structure, B: physics,
C: Pydantic consistency) emits issues that conform to ``Issue``.
The orchestrator aggregates per-phase ``PhaseReport`` objects into a
single ``ValidationReport`` which is also what the CLI envelope's
``data.phases`` field carries.

Severity vocabulary matches the legacy ``ValidationResult`` used by
Phase B (see ``phase_b_rules/rules_core.py``); both share the same
``Issue.severity`` strings so consumers do not need to special-case
which phase produced the issue.

The text reports written by each phase are unchanged; this module
adds a JSON sidecar (``<report>.json``) next to every ``<report>.txt``
so downstream tooling (MCP, agents, CI) can consume the validator
output without parsing the human-readable form.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field, asdict
from pathlib import Path
from typing import Any, List, Optional, Union

PathLike = Union[str, Path]

SEVERITY_ERROR = "ERROR"
SEVERITY_WARNING = "WARNING"
SEVERITY_INFO = "INFO"
SEVERITY_SUGGESTION = "SUGGESTION"
SEVERITY_APPLIED_FIX = "APPLIED_FIX"
SEVERITY_PASS = "PASS"

VALID_SEVERITIES = frozenset({
    SEVERITY_ERROR,
    SEVERITY_WARNING,
    SEVERITY_INFO,
    SEVERITY_SUGGESTION,
    SEVERITY_APPLIED_FIX,
    SEVERITY_PASS,
})

VALID_PHASES = frozenset({"A", "B", "C"})


@dataclass
class Issue:
    """One discrete validator finding.

    ``code`` is a stable dotted slug ``<phase>.<category>.<parameter>``
    that downstream tooling can match on without reading ``message``.
    """

    phase: str
    severity: str
    code: str
    message: str
    yaml_path: Optional[str] = None
    site_gridid: Optional[int] = None
    site_index: Optional[int] = None
    category: Optional[str] = None
    suggested_value: Any = None
    applied_fix: bool = False

    def __post_init__(self) -> None:
        if self.phase not in VALID_PHASES:
            raise ValueError(
                f"Invalid phase {self.phase!r}; expected one of {sorted(VALID_PHASES)}"
            )
        self.severity = (self.severity or SEVERITY_INFO).upper()
        if self.severity not in VALID_SEVERITIES:
            raise ValueError(
                f"Invalid severity {self.severity!r}; expected one of {sorted(VALID_SEVERITIES)}"
            )

    def to_dict(self) -> dict:
        return asdict(self)


@dataclass
class PhaseReport:
    """Aggregate of issues produced by one phase of the validator."""

    phase: str
    issues: List[Issue] = field(default_factory=list)
    yaml_in: Optional[str] = None
    yaml_out: Optional[str] = None
    text_report_path: Optional[str] = None
    json_report_path: Optional[str] = None
    extra: dict = field(default_factory=dict)

    @property
    def has_errors(self) -> bool:
        return any(i.severity == SEVERITY_ERROR for i in self.issues)

    @property
    def has_warnings(self) -> bool:
        return any(i.severity == SEVERITY_WARNING for i in self.issues)

    @property
    def status(self) -> str:
        if self.has_errors:
            return "FAILED"
        if self.has_warnings:
            return "WARNING"
        return "PASSED"

    def to_dict(self) -> dict:
        return {
            "phase": self.phase,
            "status": self.status,
            "issues": [i.to_dict() for i in self.issues],
            "yaml_in": self.yaml_in,
            "yaml_out": self.yaml_out,
            "text_report_path": self.text_report_path,
            "json_report_path": self.json_report_path,
            "extra": self.extra,
        }


@dataclass
class ValidationReport:
    """Aggregate of every phase run for a single config."""

    phases: List[PhaseReport] = field(default_factory=list)

    @property
    def overall_status(self) -> str:
        if any(p.has_errors for p in self.phases):
            return "FAILED"
        if any(p.has_warnings for p in self.phases):
            return "WARNING"
        return "PASSED"

    def to_dict(self) -> dict:
        return {
            "overall_status": self.overall_status,
            "phases": [p.to_dict() for p in self.phases],
        }


@dataclass(frozen=True)
class JSONReportWriter:
    """Writes a ``PhaseReport`` to a JSON sidecar file."""

    encoding: str = "utf-8"
    indent: int = 2

    def write(self, filepath: PathLike, report: PhaseReport) -> None:
        path = Path(filepath)
        text = json.dumps(report.to_dict(), indent=self.indent, ensure_ascii=False)
        with path.open("w", encoding=self.encoding, newline="\n") as handle:
            handle.write(text)
            handle.write("\n")


JSON_REPORT_WRITER = JSONReportWriter()
