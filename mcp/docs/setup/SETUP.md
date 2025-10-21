# SUEWS MCP Server - Setup Guide

Quick reference for setting up the SUEWS MCP server for Claude Desktop.

## Installation

```bash
# Install with MCP support
pip install supy[mcp]

# Or for development
pip install -e .[mcp]
```

## Claude Desktop Configuration

### Config File Location

- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Windows**: `%APPDATA%\Claude\claude_desktop_config.json`
- **Linux**: `~/.config/Claude/claude_desktop_config.json`

### Basic Configuration

```json
{
  "mcpServers": {
    "suews": {
      "command": "supy-mcp"
    }
  }
}
```

### With Virtual Environment

```json
{
  "mcpServers": {
    "suews": {
      "command": "/absolute/path/to/venv/bin/supy-mcp"
    }
  }
}
```

## Verification

After restarting Claude Desktop, the SUEWS tools should be available:

```
Tools available: 10
- validate_config
- create_config
- get_config_info
- update_config
- run_simulation
- estimate_runtime
- load_results
- compute_statistics
- create_plot
- export_results
```

## Testing

Test tool availability:

```python
from suews_mcp.server import list_tools
import asyncio

tools = asyncio.run(list_tools())
print(f"Available tools: {len(tools)}")
for tool in tools:
    print(f"  - {tool.name}")
```

## Troubleshooting

### Server doesn't appear in Claude Desktop

1. Check JSON syntax in config file
2. Verify `supy-mcp` is in PATH: `which supy-mcp`
3. Check Claude Desktop logs (Help → View Logs)
4. Restart Claude Desktop after config changes

### Import errors

```bash
# Check MCP SDK is installed
pip list | grep mcp

# Should show: mcp x.x.x

# Reinstall if needed
pip install --upgrade 'mcp>=0.9.0'
```

### Tool execution fails

- Use absolute paths for all file arguments
- Ensure YAML configs are valid
- Check write permissions for output directories

## Development

Running the server standalone for testing:

```bash
# Interactive mode (for debugging)
python -m suews_mcp.server

# With example config
echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | python -m suews_mcp.server
```

## See Also

- Full documentation: `docs/source/mcp-integration.md`
- MCP Protocol: https://modelcontextprotocol.org
- Issue #751: https://github.com/UMEP-dev/SUEWS/issues/751
