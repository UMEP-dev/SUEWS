# suews-mcp

Model Context Protocol (MCP) server for SUEWS.

This is the **AI-facing adaptor** layer of the SUEWS canonical AI architecture
(CLI + Skill + MCP). It is a thin, controlled bridge that lets MCP clients —
Claude Desktop, Claude Code, Cursor, VS Code, etc. — call SUEWS through a fixed
allow-list of safe, structured tools. It never reimplements physics or
validation logic; every tool delegates to the unified `suews` CLI or to a
`supy` public function.

## Architecture

```
AI client  →  suews-mcp tools/resources  →  suews <subcmd>  →  supy → SUEWS
```

Hard rules:

- No arbitrary shell execution. The backend's allow-list permits only
  `suews <validate|schema|convert|run|inspect|diagnose|summarise|compare|init>`.
- Stdio transport only in the current phase. HTTP/SSE is deferred.
- Every tool emits the standard JSON envelope
  `{status, data, errors, warnings, meta}` defined once in
  `supy.cmd.json_envelope.Envelope`.
- Project root is set per-session via `SUEWS_MCP_PROJECT_ROOT`; tool
  arguments cannot escape it.

## Phase 1 surface (read-only)

Tools:

- `validate_config(config_path, project_root=None, explain=True)`
- `inspect_config(config_path, project_root=None, view="all")`
- `search_schema(query, version="current")`
- `list_examples()`
- `read_example(name)`

Resources:

- `suews://schema/current`, `suews://schema/{version}`
- `suews://examples/{name}`
- `suews://docs/{slug}`
- `suews://runs/{run_id}/{provenance|diagnostics}`

## Install (development)

```bash
cd mcp
uv pip install -e '.[dev]'
```

## Launch

```bash
SUEWS_MCP_PROJECT_ROOT=/path/to/case suews-mcp
```

The server reads JSON-RPC over stdio. Configure your MCP client to launch
`suews-mcp` as a stdio server.

## Testing

```bash
cd mcp
pytest tests/
```

Tests mock the subprocess layer; no real `suews` invocation runs in CI.
