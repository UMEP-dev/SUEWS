# DXT (Desktop Extension) Build Guide for SUEWS MCP

This guide follows the official DXT specifications for building Claude Desktop extensions.

## Official Resources
- **DXT Architecture**: https://github.com/anthropics/dxt/blob/main/README.md
- **Manifest Specification**: https://github.com/anthropics/dxt/blob/main/MANIFEST.md
- **Examples**: https://github.com/anthropics/dxt/tree/main/examples

## DXT Architecture Overview

DXT extensions are ZIP archives containing:
1. `manifest.json` - Extension metadata and configuration
2. Server implementation (Python/Node.js/Binary)
3. Dependencies and resources
4. Optional: icons, screenshots, documentation

## Building a DXT Extension

### 1. Manifest Structure (manifest.json)

#### Required Fields
```json
{
  "dxt_version": "0.1",
  "name": "suews-assistant",
  "version": "1.0.0",
  "description": "SUEWS parameter explanations",
  "author": {
    "name": "Author Name"
  },
  "server": {
    "type": "python",
    "entry_point": "run_server.py"
  }
}
```

#### Complete Manifest Example
```json
{
  "dxt_version": "0.1",
  "name": "suews-assistant",
  "version": "1.0.0",
  "display_name": "SUEWS Assistant",
  "description": "Get detailed explanations of SUEWS urban climate model parameters",
  "long_description": "Comprehensive parameter guide with scientific context...",
  "author": {
    "name": "Ting Sun & Claude Code",
    "email": "suews@urban-climate.net",
    "url": "https://github.com/UMEP-dev/SUEWS"
  },
  "server": {
    "type": "python",
    "entry_point": "run_server.py",
    "mcp_config": {
      "command": "python3",
      "args": ["${__dirname}/run_server.py"],
      "env": {
        "PYTHONPATH": "${__dirname}/src:${__dirname}"
      }
    }
  },
  "tools": [
    {
      "name": "explain_suews_parameter",
      "description": "Get detailed explanation of any SUEWS parameter"
    }
  ],
  "prompts": [
    {
      "name": "parameter_guide",
      "description": "Guide through SUEWS parameters",
      "text": "I need help understanding SUEWS parameters..."
    }
  ],
  "keywords": ["suews", "urban-climate", "parameters"],
  "license": "MIT",
  "repository": {
    "type": "git",
    "url": "https://github.com/UMEP-dev/SUEWS"
  },
  "compatibility": {
    "minimum_dxt_version": "0.1",
    "runtime": {
      "python": ">=3.9"
    }
  }
}
```

### 2. Server Implementation

#### Entry Point (run_server.py)
```python
#!/usr/bin/env python3
"""Entry point for DXT extension."""

import sys
import os
import logging

# Configure logging for debugging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[logging.StreamHandler(sys.stderr)]
)

# Add source to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

try:
    from suews_mcp.server import main
    main()
except Exception as e:
    logging.error(f"Failed to start server: {e}")
    sys.exit(1)
```

#### MCP Server (src/suews_mcp/server.py)
```python
"""MCP server with proper error handling and timeout management."""

import logging
import asyncio
from typing import Any
from mcp.server import FastMCP
from mcp.server.exceptions import McpError

logger = logging.getLogger(__name__)

# Create server with timeout configuration
mcp = FastMCP(
    name="suews-assistant",
    instructions="SUEWS parameter explanation service"
)

# Configure timeout for long operations
TOOL_TIMEOUT = 30.0  # seconds

@mcp.tool(description="Get detailed explanation of SUEWS parameter")
async def explain_suews_parameter(
    parameter_name: str,
    include_examples: bool = True
) -> str:
    """
    Tool with proper error handling and validation.
    """
    # Input validation
    if not parameter_name or not isinstance(parameter_name, str):
        raise McpError("Invalid parameter name")
    
    if len(parameter_name) > 100:
        raise McpError("Parameter name too long")
    
    try:
        # Apply timeout to prevent hanging
        result = await asyncio.wait_for(
            explain_parameter(parameter_name, include_examples),
            timeout=TOOL_TIMEOUT
        )
        return result
    except asyncio.TimeoutError:
        logger.error(f"Timeout explaining parameter: {parameter_name}")
        return "Operation timed out. Please try again."
    except Exception as e:
        logger.error(f"Error explaining parameter: {e}")
        return f"Error: Unable to explain parameter. {str(e)}"

def main():
    """Main entry point with error handling."""
    try:
        mcp.run(transport="stdio")
    except KeyboardInterrupt:
        logger.info("Server shutdown requested")
    except Exception as e:
        logger.error(f"Server error: {e}")
        raise
```

### 3. Build Script Best Practices

```python
#!/usr/bin/env python3
"""DXT extension builder following official spec."""

import json
import zipfile
from pathlib import Path

def validate_manifest(manifest_path):
    """Validate manifest against DXT spec."""
    with open(manifest_path) as f:
        manifest = json.load(f)
    
    # Required fields
    required = ["dxt_version", "name", "version", "description", "author", "server"]
    for field in required:
        if field not in manifest:
            raise ValueError(f"Missing required field: {field}")
    
    # Validate version format
    import re
    if not re.match(r'^\d+\.\d+\.\d+', manifest['version']):
        raise ValueError("Version must follow semantic versioning")
    
    return manifest

def create_dxt_extension():
    """Create DXT following official structure."""
    # Validate before building
    manifest = validate_manifest("manifest.json")
    
    # Create clean build
    build_dir = Path("build/extension")
    if build_dir.exists():
        shutil.rmtree(build_dir)
    
    # ... rest of build process
```

### 4. Testing Considerations

#### Manifest Validation
```bash
# Validate JSON structure
python -m json.tool manifest.json

# Check required fields
python -c "import json; m = json.load(open('manifest.json')); assert all(k in m for k in ['dxt_version', 'name', 'version', 'description', 'author', 'server'])"
```

#### Tool Response Validation
```python
async def test_tool_responses():
    """Validate tool returns proper JSON responses."""
    # Test successful response
    result = await explain_suews_parameter("albedo")
    assert isinstance(result, str)
    assert len(result) > 0
    
    # Test error handling
    result = await explain_suews_parameter("")
    assert "Error" in result or "Invalid" in result
    
    # Test timeout handling
    # ... implement timeout test
```

### 5. Security Best Practices

1. **Input Validation**: Always validate and sanitize inputs
2. **Timeout Management**: Prevent hanging operations
3. **Error Messages**: Don't expose internal details
4. **Resource Limits**: Limit memory and CPU usage
5. **Logging**: Log errors to stderr, not stdout

### 6. Directory Structure

```
suews-assistant/
├── manifest.json          # DXT manifest
├── run_server.py         # Entry point
├── requirements.txt      # Python dependencies
├── setup.py             # Python package setup
├── README.md            # User documentation
├── src/
│   └── suews_mcp/
│       ├── __init__.py
│       ├── server.py    # MCP server
│       └── tools/       # Tool implementations
└── tests/              # Test suite
```

### 7. Building for Production

```bash
# 1. Run tests
python -m pytest tests/

# 2. Validate manifest
python validate_manifest.py

# 3. Build extension
python build_extension.py

# 4. Test the DXT
unzip -t dist/suews-assistant-*.dxt

# 5. Manual test in Claude Desktop
# Install and verify all tools work
```

## Common Issues and Solutions

### Issue: Extension doesn't load
- Check manifest.json syntax
- Verify entry_point exists
- Check Python path configuration

### Issue: Tools don't appear
- Ensure tools are defined in manifest
- Verify tool names match implementation
- Check for import errors

### Issue: Server crashes
- Add comprehensive error handling
- Check stderr logs
- Validate all inputs

## References
- MCP SDK: https://github.com/modelcontextprotocol/python-sdk
- DXT Examples: https://github.com/anthropics/dxt/tree/main/examples
- Claude Desktop Docs: https://claude.ai/desktop