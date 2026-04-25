# suews-mcp

MCP server for conversational SUEWS discovery, configuration, and simulation.

<!-- mcp-name: io.github.umep-dev/suews -->

## What is this?

`suews-mcp` exposes SUEWS capabilities to MCP clients so you can search supported type definitions, execute runs from YAML, and ask for explainers on core modelling concepts.

## Quick install

### Claude Code

```bash
claude mcp add suews -- uvx suews-mcp
```

### Codex

```bash
codex mcp add suews -- uvx suews-mcp
```

### Claude Desktop

Add this to your MCP config:

```json
{
  "mcpServers": {
    "suews": {
      "command": "uvx",
      "args": ["suews-mcp"]
    }
  }
}
```

## Tools

- `search(query, type_name?, detail_level?)`
  - type catalogue lookup (index mode)
  - schema JSON retrieval for a selected type
  - sample JSON retrieval for a selected type
- `execute(config_yaml, description?)`
  - runs `suews run config.yml`
  - reports status and output artefact information
  - optionally summarises Arrow output when `pyarrow` is installed
- `explain(topic)`
  - returns curated, topic-matched SUEWS knowledge notes

## Optional extras

Install with simulation summaries enabled:

```bash
uvx "suews-mcp[simulation]"
```

## Development mode (from a local clone)

```bash
python3 mcp/scripts/install_mcp_clients.py --mode local
```

## Documentation

- SUEWS repository: <https://github.com/UMEP-dev/SUEWS>
- SUEWS docs: <https://suews.readthedocs.io/>
