# SUEWS MCP Server Quick Start

## Testing Outside Claude Desktop

### 1. Using UV (Recommended)
```bash
cd mcp-server
uv venv
source .venv/bin/activate
uv pip install -e .
```

### 2. Test with CLI
```bash
# List all tools
python mcp_cli.py list

# Test specific tools
python mcp_cli.py explain albedo_deciduous_summer
python mcp_cli.py template suburban energy_balance
```

### 3. Check dependencies
```bash
python -m suews_mcp.check_supy
```

## Installing in Claude Desktop

### 1. Install SuPy for system Python
```bash
# Check which Python Claude uses
/usr/bin/python3 --version

# Install SuPy
/usr/bin/python3 -m pip install --user supy==2025.6.2.dev
```

### 2. Install Extension
1. Open Claude Desktop
2. Settings → Developer → Extensions
3. Install `dist/suews-assistant-20250630.dxt`

### 3. Troubleshooting
If extension fails, it will show clear error messages about missing dependencies.

## Key Files
- `mcp_cli.py` - Command-line testing tool
- `check_and_run.py` - Dependency checker (runs before server)
- `test_as_claude.py` - Simulate Claude Desktop environment
- `test_client.py` - Async client test
- `test_all_tools.py` - Comprehensive tool testing

## Current Status
✅ All 10 MCP tools working
✅ Dependency checking with clear instructions
✅ CLI testing interface
✅ Desktop extension package ready
⚠️ Requires SuPy installation for target Python