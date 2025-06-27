# SUEWS MCP Server

A Model Context Protocol (MCP) server for the Surface Urban Energy and Water balance Scheme (SUEWS), providing intelligent configuration guidance and result interpretation.

## Overview

This MCP server helps users:
- Create scientifically sound SUEWS configurations
- Validate input parameters against physical constraints
- Understand model outputs through intelligent interpretation
- Detect and resolve common configuration issues
- Compare results against typical urban climate patterns

## Features

### Configuration Guidance
- **Real-time validation**: Catch configuration errors before running simulations
- **Smart suggestions**: Context-aware parameter recommendations
- **Physics compatibility**: Ensure selected methods work together
- **Template generation**: Quick-start configurations for different urban contexts

### Result Interpretation
- **Energy balance analysis**: Diagnose closure issues and identify causes
- **Thermal comfort assessment**: Calculate comfort indices and heat stress
- **Urban effects quantification**: Analyse heat island intensity and drivers
- **Performance validation**: Compare against observations with insights
- **Narrative reports**: Plain-language explanations of results

## Installation

```bash
# From the mcp-server directory
pip install -e .

# Install development dependencies
pip install -e ".[dev]"
```

## Usage with Claude Desktop

Add to your Claude Desktop configuration:

```json
{
  "mcpServers": {
    "suews": {
      "command": "python",
      "args": ["-m", "suews_mcp.server"],
      "cwd": "/path/to/mcp-server"
    }
  }
}
```

## Development

```bash
# Run tests
pytest

# Format code
black src tests
ruff check src tests

# Type checking
mypy src
```

## Architecture

- `src/suews_mcp/server.py`: Main MCP server implementation
- `src/suews_mcp/tools/`: Individual MCP tools for configuration and analysis
- `src/suews_mcp/utils/`: Utilities including SUEWS model bridge
- `src/suews_mcp/knowledge/`: Scientific knowledge base and rules
- `src/suews_mcp/resources/`: Static resources and documentation

## Contributing

Please see the main SUEWS repository for contribution guidelines.