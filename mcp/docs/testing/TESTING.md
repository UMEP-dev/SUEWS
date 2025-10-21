# Local MCP Testing Guide

Quick testing of SUEWS MCP tools without Claude Desktop.

## Quick Start

```bash
# Activate environment
source ../.venv/bin/activate

# Interactive mode (recommended for exploration)
cd mcp
python test_mcp_local.py

# Direct command mode (fast iteration)
python test_mcp_local.py list_physics_schemes
python test_mcp_local.py get_variable_info variable_name=QH
python test_mcp_local.py get_model_docs model_name=Site
```

## Usage Patterns

### 1. Test Single Tool
```bash
python test_mcp_local.py <tool_name> [key=value ...]
```

Examples:
```bash
# No arguments
python test_mcp_local.py get_config_schema
python test_mcp_local.py list_available_models

# With arguments
python test_mcp_local.py get_variable_info variable_name=QE
python test_mcp_local.py get_model_docs model_name=SurfaceProperties
python test_mcp_local.py get_physics_implementation scheme_name=water_balance
```

### 2. Interactive Mode
```bash
python test_mcp_local.py
```

Provides menu with:
- Numbered list of all tools
- Quick test shortcuts (a-d)
- Interactive prompts for arguments

### 3. Development Workflow

**Typical iteration cycle:**

1. Edit tool in `src/suews_mcp/tools/knowledge.py`
2. Test immediately: `python test_mcp_local.py get_variable_info`
3. Fix issues
4. Repeat until working
5. Run proper tests: `pytest tests/test_knowledge.py`
6. Rebuild MCPB when ready

**No need to**:
- Restart Claude Desktop
- Rebuild MCPB
- Reinstall anything

Just edit and test!

## Available Tools

### Knowledge Tools (authentic SUEWS info)
- `get_config_schema` - JSON Schema from Pydantic models
- `get_model_docs` - Parameter documentation (e.g., model_name=Site)
- `list_available_models` - All Pydantic models
- `get_variable_info` - Output variables (variable_name=QH or empty for all)
- `list_physics_schemes` - Available physics schemes
- `get_physics_implementation` - Fortran source code (scheme_name=OHM)

### Utility Tools (SUEWS calculations)
- `calculate_ohm_coefficients` - OHM calibration (results_path=...)
- `calculate_surface_conductance` - Vegetation params (results_path=...)
- `calculate_roughness` - Urban morphology (building_height=10.0 plan_area_fraction=0.4)

## Tips

### Debugging Output
The test script limits depth and length for readability:
- Dict/List: Max 3 levels deep
- Strings: Truncated at 300 chars
- Lists: First 10 items shown

To see full output, modify `max_depth` in the script or use `json.dumps()`.

### Testing New Tools

1. Add function to `src/suews_mcp/tools/knowledge.py`
2. Add to `TOOLS` dict in `test_mcp_local.py`:
   ```python
   TOOLS = {
       ...
       "my_new_tool": knowledge.my_new_tool,
   }
   ```
3. Test: `python test_mcp_local.py my_new_tool`

### Comparing with Claude Desktop

If behaviour differs between local test and Claude Desktop:
1. Check MCP bundle is updated: `ls -l ~/Library/Application\ Support/Claude/mcp-servers/suews-mcp/src/suews_mcp/tools/`
2. Restart Claude Desktop (tools loaded at startup)
3. Check for Python caching: `rm -rf src/suews_mcp/**/__pycache__`

## Advanced: MCP Protocol Testing

For testing actual MCP protocol (messages, errors, etc.):

```bash
# Test MCP server directly via stdio
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
  python -m suews_mcp.server
```

This tests:
- JSON-RPC message handling
- Tool registration
- Error handling
- Protocol compliance

## Common Issues

### ImportError: No module named 'suews_mcp'
- Run from `mcp/` directory
- Or install in editable mode: `uv pip install -e .`

### Tool not found
- Check tool is in `TOOLS` dict
- Check import is correct
- Restart test script (Python caching)

### Different results than Claude Desktop
- Check Claude Desktop is using latest MCPB
- Clear `.venv` cache: `find . -name __pycache__ -type d -exec rm -rf {} +`
- Reinstall: `uv pip install -e . --force-reinstall`
