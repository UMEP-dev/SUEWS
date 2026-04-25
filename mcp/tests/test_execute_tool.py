from __future__ import annotations

import asyncio
from pathlib import Path
from typing import Any

from suews_mcp.backend.base import SimulationResult, SUEWSBackend
from suews_mcp.tools.execute import handle_execute


class StubBackend(SUEWSBackend):
    def __init__(self) -> None:
        self.last_work_dir: Path | None = None
        self.last_config: str | None = None
        self.sample_forcing_present = False

    async def list_types(self, query: str = "") -> dict[str, Any]:
        raise NotImplementedError

    async def get_schema(self, type_name: str, detail_level: str) -> dict[str, Any]:
        raise NotImplementedError

    async def run_simulation(self, config_yaml: str, work_dir: Path) -> SimulationResult:
        self.last_work_dir = work_dir
        self.last_config = config_yaml
        self.sample_forcing_present = (work_dir / "Kc_2012_data_60.txt").exists()
        return SimulationResult(
            command=["suews", "run", "config.yml"],
            returncode=0,
            stdout="output_file=missing.arrow\n",
            stderr="",
            output_file=work_dir / "missing.arrow",
        )

    async def get_knowledge(self, topic: str) -> dict[str, Any]:
        raise NotImplementedError

    async def get_source_index(self, query: str) -> dict[str, Any]:
        raise NotImplementedError

    async def get_source_excerpt(
        self, file_name: str, subroutine: str | None = None
    ) -> dict[str, Any]:
        raise NotImplementedError

    async def get_diagnostic(
        self,
        variable: str,
        symptom: str | None = None,
        context: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        raise NotImplementedError

    async def search_catalogue(self, query: str) -> dict[str, Any]:
        raise NotImplementedError


def test_execute_rejects_empty_config() -> None:
    backend = StubBackend()
    result = asyncio.run(handle_execute(backend=backend, config_yaml="", description=None))

    assert result["used_sample_config"] is False
    assert "empty" in result["stderr"].lower()


def test_execute_uses_sample_keyword() -> None:
    backend = StubBackend()
    result = asyncio.run(handle_execute(backend=backend, config_yaml="sample", description=None))

    assert result["used_sample_config"] is True
    assert backend.sample_forcing_present is True


def test_execute_handles_missing_output_file_gracefully() -> None:
    backend = StubBackend()
    result = asyncio.run(handle_execute(backend=backend, config_yaml="name: test\n"))

    assert result["returncode"] == 0
    assert result["timesteps"] is None
    assert result["warnings"]
