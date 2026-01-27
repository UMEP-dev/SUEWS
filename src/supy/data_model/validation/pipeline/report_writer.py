"""Centralised report I/O utilities for validation pipeline output."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Union

PathLike = Union[str, Path]


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
