"""Abstract backend contracts for SUEWS MCP."""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path
from typing import Any


@dataclass(slots=True)
class SimulationResult:
    """Result of a `suews run` invocation."""

    command: list[str]
    returncode: int
    stdout: str
    stderr: str
    output_file: Path | None


class BackendError(RuntimeError):
    """Base backend exception."""


class CommandExecutionError(BackendError):
    """Raised when a backend command exits with an error."""

    def __init__(
        self,
        *,
        command: list[str],
        returncode: int,
        stdout: str,
        stderr: str,
    ) -> None:
        self.command = command
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr
        command_repr = " ".join(command)
        message = (
            f"command failed with exit code {returncode}: {command_repr}\n"
            f"stdout:\n{stdout}\n"
            f"stderr:\n{stderr}"
        )
        super().__init__(message)


class SUEWSBackend(ABC):
    """Backend interface for local or remote SUEWS execution."""

    @abstractmethod
    async def list_types(self, query: str = "") -> dict[str, Any]:
        """Return available SUEWS types."""

    @abstractmethod
    async def get_schema(self, type_name: str, detail_level: str) -> dict[str, Any]:
        """Return type-level details from schema tooling."""

    @abstractmethod
    async def run_simulation(self, config_yaml: str, work_dir: Path) -> SimulationResult:
        """Run a SUEWS simulation."""

    @abstractmethod
    async def get_knowledge(self, topic: str) -> dict[str, Any]:
        """Lookup explanatory knowledge content."""

    @abstractmethod
    async def get_source_index(self, query: str) -> dict[str, Any]:
        """Search indexed Fortran source metadata."""

    @abstractmethod
    async def get_source_excerpt(
        self,
        file_name: str,
        subroutine: str | None = None,
    ) -> dict[str, Any]:
        """Return source code excerpt for a file or subroutine."""

    @abstractmethod
    async def get_diagnostic(
        self,
        variable: str,
        symptom: str | None = None,
        context: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """Return diagnostic guidance for simulation behaviour."""

    @abstractmethod
    async def search_catalogue(self, query: str) -> dict[str, Any]:
        """Search the data catalogue for forcing, samples, and external sources."""
