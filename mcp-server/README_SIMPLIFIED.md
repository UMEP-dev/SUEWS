# SUEWS Assistant MCP Server (Simplified)

A focused MCP server that provides detailed explanations of SUEWS urban climate model parameters.

## What it does

The SUEWS Assistant helps users understand SUEWS parameters by providing:
- Detailed parameter descriptions
- Units and typical value ranges
- Scientific context and physical meaning
- Usage examples
- Related parameters

## Installation

### As Desktop Extension (Recommended)

1. Download `suews-assistant-YYYYMMDD.dxt` from the `dist/` folder
2. Open Claude Desktop
3. Go to Extensions
4. Install the .dxt file

### Manual Installation

```bash
# Install dependencies
pip install mcp pydantic

# Run the server
python run_server.py
```

## Usage

Simply ask about any SUEWS parameter:

- "Explain the albedo parameter"
- "What is tstep?"
- "Tell me about NetRadiationMethod"
- "What's the typical range for lai_max?"

## Available Parameters

The knowledge base includes explanations for:
- Model control parameters (tstep, etc.)
- Physics method parameters (NetRadiationMethod, StorageHeatMethod, etc.)
- Site parameters (latitude, z, bldgh, etc.)
- Surface parameters (albedo, emissivity, etc.)
- Vegetation parameters (lai_max, etc.)
- Water balance parameters (soilstore_capacity, etc.)
- Anthropogenic parameters (population_density, qf_a, etc.)

## Building the Extension

To build a new desktop extension:

```bash
python build_extension_simple.py
```

This creates a .dxt file in the `dist/` folder.

## Development

The simplified server consists of:
- `src/suews_mcp/server.py` - Main MCP server with single tool
- `src/suews_mcp/tools/parameter_explainer.py` - Parameter knowledge base
- `manifest.json` - Desktop extension manifest
- `run_server.py` - Server entry point

To add more parameters, edit the `PARAMETER_KNOWLEDGE` dictionary in `parameter_explainer.py`.