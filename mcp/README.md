# suews-mcp

Model Context Protocol (MCP) server for SUEWS.

This is the **AI-facing adaptor** layer of the SUEWS canonical AI architecture
(CLI + Skill + MCP). It is a thin, controlled bridge that lets MCP clients —
Claude Desktop, Claude Code, Codex, Cursor, VS Code, etc. — call SUEWS through
a fixed allow-list of safe, structured tools. It never reimplements physics or
validation logic; every tool delegates to the unified `suews` CLI or to a
`supy` public function.

## Architecture

```
AI client  ->  suews-mcp tools/resources  ->  suews <subcmd>  ->  supy -> SUEWS
```

Hard rules:

- No arbitrary shell execution. The backend's allow-list permits only
  `suews <validate|schema|convert|run|inspect|diagnose|summarise|compare|init|knowledge>`.
- Stdio transport only in the current phase. HTTP/SSE is deferred.
- Every tool emits the standard JSON envelope
  `{status, data, errors, warnings, meta}` defined once in
  `supy.cmd.json_envelope.Envelope`.
- Project root is set per-session via `SUEWS_MCP_PROJECT_ROOT`; tool
  arguments cannot escape it.

## Tool surface

Configuration & schema (read-only):

- `validate_config(config_path, project_root=None, explain=True)`
- `inspect_config(config_path, project_root=None)`
- `search_schema(query, version="current")`
- `list_examples()`
- `read_example(name)`

Workflow:

- `init_case(target_dir, project_root=None, template="simple-urban")`
- `convert_config(input_path, output_path, project_root=None, from_version=None, debug_dir=None, no_profile_validation=False)`

Post-run analysis:

- `summarise_run(run_dir, project_root=None, variables=None)`
- `compare_runs(run_a, run_b_or_obs, project_root=None, metrics=None, variables=None)`
- `diagnose_run(run_dir, project_root=None)`

Versioned knowledge pack:

- `query_knowledge(question, project_root=None, pack=None, limit=5, scope=None)`
- `read_knowledge_manifest(project_root=None, pack=None)`

Each match in `query_knowledge` carries `git_sha`, `github_url`, `repo_path`
and `line_start`/`line_end` so downstream answers cite the exact revision.
The manifest also exposes `pack_version` and `schema_version` for cache-key
discipline.

## Resources

- `suews://schema/current`, `suews://schema/{version}`
- `suews://examples/{name}`
- `suews://docs/{slug}`
- `suews://runs/{run_id}/{provenance|diagnostics}`
- `suews://knowledge/manifest`
- `suews://knowledge/query/{question}`

## Install

The package needs a working `supy` install (the unified `suews` CLI must be
on `PATH`). In an editable supy checkout this is automatic; in a wheel-only
install it pulls `supy` from PyPI.

```bash
# from the repo root
uv pip install -e mcp/
# or, once published:
# pip install suews-mcp
```

## Recommended path: install via the SUEWS plugin

The `suews` plugin in `.claude-plugin/marketplace.json` (Claude Code) and
`.codex-plugin/plugin.json` (Codex) **already references this MCP server**
via the bundled `.mcp.json` files. When a user installs the SUEWS plugin
from the marketplace, the host registers `suews-mcp` automatically — no
hand-editing of `~/.claude/settings.json` or `~/.codex/config.toml` is
required, provided `suews-mcp` is on `PATH`.

### Claude Code

In Claude Code, install the `suews` plugin from the marketplace
(`/plugin install suews`). The plugin's `mcpServers` declaration points at
the top-level `.mcp.json`, which spawns the `suews-mcp` console script. If
the script lives in a project venv, ensure that venv is the active one
when Claude Code launches.

### Codex

In Codex, install the `suews` plugin via the plugin manager. The bundled
`plugins/suews/.mcp.json` (referenced from
`plugins/suews/.codex-plugin/plugin.json`) declares the same `suews-mcp`
stdio server, so the MCP tools appear automatically once the plugin is
enabled.

### What the plugin install gives you

After installing the plugin, the host exposes:

- the SUEWS workflow Skill (rules, references, recipes), and
- the `suews-mcp` MCP server with all twelve tools and resources listed
  above.

Invoke `read_knowledge_manifest` first to confirm the pack version your
session is bound to.

## Fallback: raw stdio configuration

Use this only when the host doesn't yet support plugin manifests (some
Cursor / VS Code / Claude Desktop builds), or when you want to point at a
custom `suews-mcp` build.

### Claude Code raw stanza

Add to `~/.claude/settings.json`:

```json
{
  "mcpServers": {
    "suews": {
      "command": "suews-mcp",
      "env": {
        "SUEWS_MCP_PROJECT_ROOT": "/absolute/path/to/your/suews/case"
      }
    }
  }
}
```

If `suews-mcp` lives inside a project venv, point `command` at the absolute
interpreter path, e.g. `/abs/path/.venv/bin/suews-mcp`.

### Codex raw TOML

Add to `~/.codex/config.toml`:

```toml
[mcp_servers.suews]
command = "suews-mcp"
env = { SUEWS_MCP_PROJECT_ROOT = "/absolute/path/to/your/suews/case" }
```

### Other clients

Any host that speaks stdio MCP (Cursor, VS Code's MCP extension, Claude
Desktop) launches it the same way: command `suews-mcp`, optional single
environment variable `SUEWS_MCP_PROJECT_ROOT` (defaults to the host's
working directory).

## Manual launch (for debugging)

```bash
SUEWS_MCP_PROJECT_ROOT=/path/to/case suews-mcp
```

The server reads JSON-RPC over stdio.

## Testing

Tests live under `test/mcp/` (alongside the rest of the SUEWS test
suite — same convention as the other sibling wheel `src/suews_bridge/`).
Run from the repo root:

```bash
pytest test/mcp -v
# or, as part of the standard suite:
make test
```

Tests mock the subprocess layer; no real `suews` invocation runs in CI.
