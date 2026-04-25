from __future__ import annotations

import asyncio
from pathlib import Path

from suews_mcp.backend.local import LocalBackend


def _make_backend(monkeypatch) -> LocalBackend:
    monkeypatch.setattr("suews_mcp.backend.local.shutil.which", lambda _: "/usr/local/bin/suews")
    return LocalBackend()


def test_get_schema_returns_index_for_known_type(monkeypatch) -> None:
    backend = _make_backend(monkeypatch)
    result = asyncio.run(backend.get_schema(type_name="ohm-state", detail_level="index"))
    assert result["detail_level"] == "index"
    assert result["type"]["type_name"] == "ohm-state"


def test_get_schema_returns_message_for_unmapped_type(monkeypatch) -> None:
    """State/output types without a JSON Schema mapping return a helpful message."""
    backend = _make_backend(monkeypatch)
    result = asyncio.run(backend.get_schema(type_name="ohm-state", detail_level="schema"))
    assert result["detail_level"] == "schema"
    assert result["payload"] is None
    assert "No JSON schema" in result["message"]


def test_run_simulation_parses_output_file(monkeypatch, tmp_path: Path) -> None:
    backend = _make_backend(monkeypatch)
    backend.suews_path = "/usr/local/bin/suews"

    async def fake_run(command: list[str], cwd: Path | None = None) -> tuple[int, str, str]:
        await asyncio.sleep(0)
        assert command == ["/usr/local/bin/suews", "run", "config.yml"]
        assert cwd == tmp_path
        return 0, "start\noutput_file=Output/result.arrow\n", ""

    monkeypatch.setattr(backend, "_run_command", fake_run)

    result = asyncio.run(backend.run_simulation("name: demo\n", tmp_path))

    assert (tmp_path / "config.yml").exists()
    assert result.output_file == tmp_path / "Output" / "result.arrow"


def test_run_simulation_parses_output_from_listing(monkeypatch, tmp_path: Path) -> None:
    """Fallback: parse 'files have been written out:' listing from suews-run stdout."""
    backend = _make_backend(monkeypatch)

    async def fake_run(command: list[str], cwd: Path | None = None) -> tuple[int, str, str]:
        await asyncio.sleep(0)
        del command, cwd
        return 0, (
            "SUEWS version: 2026.1.28\n"
            "The following files have been written out:\n"
            "Output/KCL1_2012_SUEWS_60.txt\n"
            "Output/df_state_KCL.csv\n"
            "\n"
            "SUEWS run successfully done!\n"
        ), ""

    monkeypatch.setattr(backend, "_run_command", fake_run)

    result = asyncio.run(backend.run_simulation("name: demo\n", tmp_path))
    assert result.output_file == tmp_path / "Output" / "KCL1_2012_SUEWS_60.txt"


def test_run_simulation_raises_on_failure(monkeypatch, tmp_path: Path) -> None:
    backend = _make_backend(monkeypatch)
    backend.suews_path = "/usr/local/bin/suews"

    async def fake_run(command: list[str], cwd: Path | None = None) -> tuple[int, str, str]:
        await asyncio.sleep(0)
        del command, cwd
        return 2, "", "bad config"

    monkeypatch.setattr(backend, "_run_command", fake_run)

    try:
        asyncio.run(backend.run_simulation("name: bad\n", tmp_path))
    except Exception as exc:
        assert "exit code 2" in str(exc)
        assert "bad config" in str(exc)
    else:
        raise AssertionError("Expected command failure")
