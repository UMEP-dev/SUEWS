from __future__ import annotations

import pytest
from suews_mcp import server

pytestmark = pytest.mark.api


class FakeMCP:
    def __init__(self):
        self.resources = {}
        self.tools = {}
        self.prompts = {}

    def resource(self, uri):
        def decorator(func):
            self.resources[uri] = func
            return func

        return decorator

    def tool(self, **kwargs):
        def decorator(func):
            self.tools[func.__name__] = func
            return func

        return decorator

    def prompt(self):
        def decorator(func):
            self.prompts[func.__name__] = func
            return func

        return decorator


def test_create_server_registers_v1_capabilities(tmp_path, monkeypatch):
    fake = FakeMCP()
    monkeypatch.setattr(server, "_make_fastmcp", lambda: fake)

    mcp = server.create_server(tmp_path)

    assert mcp is fake
    assert set(fake.resources) == {
        "suews://schema/current",
        "suews://schema/{version}",
        "suews://examples/sample-config",
        "suews://docs/yaml-config",
        "suews://docs/forcing-data",
        "suews://cli/help/{command}",
    }
    assert set(fake.tools) == {"validate_config"}
    assert set(fake.prompts) == set(server.prompts.PROMPTS)


def test_validate_tool_delegates_to_cli_helper(tmp_path, monkeypatch):
    fake = FakeMCP()
    monkeypatch.setattr(server, "_make_fastmcp", lambda: fake)
    calls = {}

    def fake_validate(root, config_path, *, schema_version, timeout_s, logger):
        calls["root"] = root
        calls["config_path"] = config_path
        calls["schema_version"] = schema_version
        calls["timeout_s"] = timeout_s
        calls["logger"] = logger
        return {"status": "success", "isError": False}

    monkeypatch.setattr(server.tools, "validate_config", fake_validate)
    server.create_server(tmp_path)

    result = fake.tools["validate_config"](
        "config.yml",
        schema_version="2026.5",
        timeout_s=7,
    )

    assert result == {"status": "success", "isError": False}
    assert calls["root"] == tmp_path.resolve()
    assert calls["config_path"] == "config.yml"
    assert calls["schema_version"] == "2026.5"
    assert calls["timeout_s"] == 7


def test_registered_prompts_return_text(tmp_path, monkeypatch):
    fake = FakeMCP()
    monkeypatch.setattr(server, "_make_fastmcp", lambda: fake)
    server.create_server(tmp_path)

    text = fake.prompts["review_config_before_run"]()

    assert "Review a SUEWS configuration" in text
    assert "Never overwrite files" in text
