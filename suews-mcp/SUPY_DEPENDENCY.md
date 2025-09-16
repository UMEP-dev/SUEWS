# SuPy Dependency in SUEWS MCP Server

## Overview

The SUEWS MCP Server **does depend on SuPy**, but is designed with **graceful fallback** mechanisms to work even when SuPy is not installed.

## Dependency Architecture

### 1. Primary Mode (With SuPy)
When SuPy is installed, the MCP server provides:
- **Full functionality** with actual SUEWS model execution
- **Real simulations** using the SuPy Python wrapper
- **Actual data processing** and analysis
- **Complete validation** using SuPy's built-in validators

### 2. Fallback Mode (Without SuPy)
When SuPy is NOT installed, the MCP server:
- **Still runs** without crashing
- Provides **placeholder implementations** for testing
- Returns **simulated responses** for development
- Maintains **protocol compliance** for MCP operations
- Logs warnings about missing functionality

## How It Works

### Dependency Declaration
In `pyproject.toml`:
```toml
dependencies = [
    "mcp>=1.0.0",
    "supy>=2024.12.16",  # Required for full functionality
    "pydantic>=2.0.0,<3.0.0",
    "typing-extensions>=4.0.0"
]
```

### Import Handling
In `handlers.py`:
```python
# Try to import SuPy MCP tools
try:
    from supy.mcp.tools.configure import ConfigureSimulationTool
    from supy.mcp.tools.run import RunSimulationTool
    from supy.mcp.tools.analyze import AnalyzeResultsTool
    SUPY_MCP_TOOLS_AVAILABLE = True
except ImportError:
    SUPY_MCP_TOOLS_AVAILABLE = False
    # Fallback to placeholder implementations
```

### Runtime Behavior

#### With SuPy Installed:
```python
if SUPY_MCP_TOOLS_AVAILABLE:
    # Use actual SuPy tools
    result = await self._run_tool.execute(arguments)
    # Real SUEWS simulation runs
```

#### Without SuPy (Fallback):
```python
else:
    # Use placeholder implementation
    # Returns mock results for testing
    result_text = f"SUEWS simulation completed successfully for {config_file}"
    # No actual simulation runs
```

## Installation Scenarios

### 1. Production Use (SuPy Required)
```bash
pip install suews-mcp  # Automatically installs SuPy
```

### 2. Development/Testing (SuPy Optional)
```bash
pip install mcp pydantic typing-extensions  # Core dependencies only
# Server runs in fallback mode for protocol testing
```

### 3. Claude Desktop Installation
```bash
# Recommended: Install with SuPy for full functionality
pip install suews-mcp
pip install supy  # Explicitly ensure SuPy is installed
```

## Functionality Matrix

| Feature | With SuPy | Without SuPy |
|---------|-----------|--------------|
| MCP Server Starts | ✅ | ✅ |
| Protocol Compliance | ✅ | ✅ |
| Tool Discovery | ✅ | ✅ |
| Configuration Validation | ✅ Real | ⚠️ Simulated |
| Run Simulations | ✅ Real | ⚠️ Mock |
| Analyze Results | ✅ Real | ⚠️ Placeholder |
| Data Preprocessing | ✅ | ✅ |
| Resource Management | ✅ | ✅ |
| Error Handling | ✅ | ✅ |

## Why This Design?

1. **Development Flexibility**: Allows testing MCP protocol without full SUEWS stack
2. **Gradual Adoption**: Users can test the MCP interface before installing SuPy
3. **CI/CD Friendly**: Tests can run without heavy dependencies
4. **Debugging**: Easier to isolate MCP issues from SUEWS issues

## Checking SuPy Status

### In Python:
```python
import suews_mcp.handlers as h
print(f"SuPy available: {h.SUPY_MCP_TOOLS_AVAILABLE}")
```

### In Claude Desktop:
Ask: "Is SuPy available in the SUEWS MCP server?"

### Server Logs:
```
WARNING: SuPy MCP tools not available, using fallback implementations
```

## Recommendations

### For Production Use:
**Always install SuPy** to get full SUEWS functionality:
```bash
pip install suews-mcp[full]  # Includes all dependencies
```

### For Development:
- Start with core dependencies for protocol work
- Add SuPy when testing actual simulations
- Use fallback mode for rapid iteration

### For Claude Desktop:
- **Strongly recommend** installing SuPy
- Without it, you can only test the interface, not run real simulations
- The server will warn if SuPy is missing

## Troubleshooting

### "SuPy not found" Warning:
```bash
# Fix by installing SuPy
pip install supy
```

### "ImportError: cannot import name '_supy_driver'"
```bash
# SuPy installation issue - reinstall
pip uninstall supy
pip install supy --no-cache-dir
```

### Testing Without SuPy:
```python
# The server still works for testing
# You'll see placeholder responses like:
"SUEWS simulation completed successfully for config.yml"
# Instead of actual simulation results
```

## Summary

- **SuPy IS required** for actual SUEWS simulations
- **SuPy is NOT required** for MCP protocol testing
- The server **gracefully degrades** without SuPy
- **Production deployments** should always include SuPy
- **Development/testing** can work without SuPy initially

This design ensures the MCP server is:
1. **Robust**: Doesn't crash if dependencies are missing
2. **Flexible**: Works for different use cases
3. **Clear**: Warns when functionality is limited
4. **Practical**: Allows incremental adoption