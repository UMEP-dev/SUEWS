"""Centralised report I/O utilities for validation pipeline output."""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Union

if TYPE_CHECKING:
    from .report_schema import PhaseReport

PathLike = Union[str, Path]

VALIDATION_PHASE_NAMES = {
    "A": "Completeness Check",
    "B": "Scientific Validation",
    "C": "Model Compatibility",
}
STOPPING_PHASE_PREFIX = "# Validation stopped at:"


@dataclass(frozen=True)
class ValidationReportWriter:
    """Centralised report writer with consistent encoding and newlines."""

    encoding: str = "utf-8"
    newline: str = "\n"

    def write(self, filepath: PathLike, content: str) -> None:
        """Write report content with consistent encoding and line endings."""
        path = Path(filepath)
        with path.open("w", encoding=self.encoding, newline=self.newline) as handle:
            handle.write(content)

    def read(self, filepath: PathLike) -> str:
        """Read report content with consistent encoding."""
        path = Path(filepath)
        with path.open("r", encoding=self.encoding, errors="replace") as handle:
            return handle.read()


REPORT_WRITER = ValidationReportWriter()


def format_report_stopping_phase(
    content: str, phase_reports: Iterable[PhaseReport]
) -> str:
    """Synchronise a text-report header with structured phase results.

    The first failed ``PhaseReport`` is the sole source of the public stage
    name. Successful and warning-only results do not carry a stopping-stage
    line.
    """
    failed_phase = next(
        (report.phase for report in phase_reports if report.has_errors), None
    )
    stage_name = VALIDATION_PHASE_NAMES.get(failed_phase)

    lines = content.splitlines(keepends=True)
    lines = [
        line
        for line in lines
        if not line.rstrip("\r\n").startswith(STOPPING_PHASE_PREFIX)
    ]
    if stage_name is None:
        return "".join(lines)

    for index, line in enumerate(lines):
        if line.rstrip("\r\n").startswith("# Mode:"):
            newline = "\r\n" if line.endswith("\r\n") else "\n"
            if not line.endswith(("\r", "\n")):
                lines[index] = f"{line}{newline}"
            lines.insert(
                index + 1,
                f"{STOPPING_PHASE_PREFIX} {stage_name}{newline}",
            )
            break

    return "".join(lines)


def sync_text_report_stopping_phase(
    report_path: PathLike, phase_reports: Iterable[PhaseReport]
) -> None:
    """Apply the structured stopping phase to an existing text report.

    Report I/O must not change validation exit behaviour. Missing or
    unwritable reports remain the responsibility of the existing pipeline
    diagnostics.
    """
    path = Path(report_path)
    if not path.exists():
        return

    try:
        content = REPORT_WRITER.read(path)
        updated = format_report_stopping_phase(content, phase_reports)
        if updated != content:
            REPORT_WRITER.write(path, updated)
    except OSError:
        return
