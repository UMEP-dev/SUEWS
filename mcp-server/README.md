# SUEWS MCP Server

A Model Context Protocol (MCP) server for the Surface Urban Energy and Water balance Scheme (SUEWS), providing comprehensive parameter explanations with scientific context.

## Overview

This MCP server helps users understand SUEWS parameters by providing:
- Detailed parameter descriptions with units
- Typical values and ranges for different urban contexts
- Scientific context explaining the physical meaning
- Usage examples for common scenarios
- Related parameters to consider
- Integration with SuPy data models for complete parameter coverage

## Features

- **Comprehensive Knowledge Base**: Covers all major SUEWS parameters
- **Scientific Context**: Explains the physics behind each parameter
- **Typical Values**: Provides ranges for different urban environments
- **SuPy Integration**: Enhanced with data model validation
- **Examples**: Real-world usage scenarios

## Installation

### Desktop Extension (Recommended)

The easiest way to use SUEWS MCP is via the desktop extension:

1. Download `suews-assistant-YYYYMMDD.dxt` from the `dist/` folder
2. Open Claude Desktop
3. Go to Extensions
4. Install the .dxt file

### Manual Installation

```bash
# Install dependencies
pip install -r requirements.txt

# Or install directly
pip install mcp pydantic supy==2025.7.6
```

### Dependencies

- **mcp**: Model Context Protocol implementation
- **pydantic**: Data validation
- **supy==2025.7.6**: SUEWS Python wrapper (pre-built wheels available for all platforms)

## Usage with Claude Desktop

If not using the desktop extension, add to your Claude Desktop configuration:

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

## Usage Examples

Ask the assistant about any SUEWS parameter:

- "Explain the albedo parameter"
- "What is tstep and what values should I use?"
- "Tell me about NetRadiationMethod"
- "What's the typical range for lai_max in urban areas?"
- "How does bldgh affect the model?"

## Testing

```bash
# Simple test to verify the server works
python test_simple.py

# Run the server directly
python run_server.py
```

## Building the Extension

```bash
# Build a new .dxt file
python build_extension.py
```

## Architecture

- `src/suews_mcp/server.py`: MCP server with parameter explanation tool
- `src/suews_mcp/tools/parameter_explainer.py`: Parameter knowledge base and formatter
- `manifest.json`: Desktop extension configuration
- `build_extension.py`: Build script for creating .dxt files

## Contributing

Please see the main SUEWS repository for contribution guidelines.

## Desktop Extension Development

For desktop extension development and manifest specification, see:
- https://github.com/anthropics/dxt
- [DXT Manifest Specification](https://github.com/anthropics/dxt/blob/main/MANIFEST.md)