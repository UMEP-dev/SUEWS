# SUEWS MCP

SUEWS urban climate model integration for Claude Desktop.

## Installation

**For AI users (Recommended):**

1. Download `suews-mcp.mcpb` from [GitHub Releases](https://github.com/UMEP-dev/SUEWS/releases)
2. Double-click the file (or drag to Claude Desktop)
3. First use: automatic setup (30-60 seconds)
4. Ready to use!

**For Python developers:**

```bash
pip install suews-mcp
```

Then configure manually in `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "suews": {
      "command": "suews-mcp"
    }
  }
}
```

## Usage

Ask Claude about urban climate modeling:

> "Create a SUEWS configuration for London with 60% buildings"

> "Run a simulation and show the energy balance"

> "Plot diurnal cycle of sensible heat flux"

## Available Tools

- **Configuration**: validate, create, update SUEWS configurations
- **Simulation**: run models, estimate runtime
- **Analysis**: load results, compute statistics, create plots, export data

## Requirements

- **For MCPB**: Claude Desktop (macOS or Windows), Internet (first-time setup)
- **For pip**: Python >=3.9, supy >=2025.10.15

## Documentation

Full documentation: https://suews.readthedocs.io/en/latest/mcp-integration.html

## License

GPL-V3.0
