# SuPy MCP (Model Context Protocol) Server

This module provides an MCP server implementation for SuPy, exposing SUEWS urban climate model functionality through standardized tools that AI assistants can use.

## Overview

The MCP server wraps core SuPy functionality into three essential tools:

1. **configure_simulation** - Load, validate, modify and save SUEWS configurations
2. **run_simulation** - Execute SUEWS simulations with forcing data
3. **analyze_results** - Analyze and visualize simulation outputs

## Installation

The MCP server is included with SuPy. No additional installation is required beyond the standard SuPy setup.

## Usage

### Starting the MCP Server

```python
from supy.mcp.server import SUPYMCPServer

server = SUPYMCPServer()
# Run in stdio mode for MCP communication
asyncio.run(server.run_stdio())
```

### Tool Reference

#### configure_simulation

Manages SUEWS simulation configurations.

**Parameters:**
- `config_path` (string, optional): Path to YAML configuration file
- `config_updates` (object, optional): Configuration updates as JSON
- `validate_only` (boolean, optional): Only validate without creating simulation
- `site_name` (string, optional): Site identifier
- `save_path` (string, optional): Path to save configuration
- `save_format` (string, optional): Format for saving ('yaml' or 'json')

**Example:**
```json
{
  "config_path": "config/london.yaml",
  "site_name": "London_KCL",
  "config_updates": {
    "surface": {
      "fractions": {
        "building": 0.4,
        "paved": 0.2,
        "vegetation": 0.3,
        "water": 0.1
      }
    }
  },
  "save_path": "output/london_updated.yaml"
}
```

#### run_simulation

Executes SUEWS simulations with various data sources.

**Parameters:**
- `forcing_path` (string, optional): Path to forcing data file
- `config_path` (string, optional): Path to configuration file
- `use_sample_data` (boolean, optional): Use built-in sample data
- `start_time` (string, optional): Start time (ISO format)
- `end_time` (string, optional): End time (ISO format)
- `time_step` (integer, optional): Time step in seconds
- `save_state` (boolean, optional): Save final model state
- `parallel` (boolean, optional): Use parallel processing

**Example:**
```json
{
  "forcing_path": "data/kc1_2012_forcing.txt",
  "config_path": "config/kc1.yaml",
  "start_time": "2012-06-01T00:00:00",
  "end_time": "2012-08-31T23:00:00",
  "save_state": true
}
```

#### analyze_results

Analyzes SUEWS simulation outputs with various methods.

**Parameters:**
- `results_path` (string, required): Path to results file
- `analysis_type` (string, optional): Type of analysis
  - `summary`: Basic statistics and overview
  - `statistics`: Detailed statistical analysis
  - `energy_balance`: Energy flux analysis
  - `water_balance`: Water balance components
  - `temporal`: Temporal patterns (diurnal, seasonal)
- `variables` (array, optional): Specific variables to analyze
- `time_period` (string, optional): Aggregation period
- `start_time` (string, optional): Analysis start time
- `end_time` (string, optional): Analysis end time
- `comparison_path` (string, optional): Path to comparison data
- `output_format` (string, optional): Output format

**Example:**
```json
{
  "results_path": "output/simulation_results.csv",
  "analysis_type": "energy_balance",
  "variables": ["QH", "QE", "QS", "QN"],
  "time_period": "monthly",
  "output_format": "json"
}
```

## Response Format

All tools return structured responses:

```json
{
  "success": true,
  "timestamp": "2024-01-15T10:30:00",
  "data": {
    // Tool-specific data
  },
  "message": "Operation completed successfully",
  "errors": [],
  "warnings": []
}
```

## Error Handling

The MCP tools provide comprehensive error handling:

- **File validation**: Checks for file existence and format
- **Parameter validation**: Type checking and range validation
- **Time range validation**: Ensures valid time periods
- **Physics validation**: Checks surface fractions, energy balance
- **Clear error messages**: Detailed descriptions of what went wrong

## Examples

See `examples.py` for complete usage examples including:
- Basic tool usage
- Complete workflow (configure → run → analyze)
- Error handling demonstrations
- Parameter validation examples

## Testing

Run the test suite:

```bash
pytest test/test_mcp_tools.py -v
```

## Architecture

```
mcp/
├── server.py           # Main MCP server implementation
├── tools/
│   ├── base.py        # Base tool class with common functionality
│   ├── configure.py   # Configuration management tool
│   ├── run.py        # Simulation execution tool
│   └── analyze.py    # Results analysis tool
└── utils/
    └── translator.py  # Parameter translation and validation
```

## Key Features

### Configuration Management
- Load configurations from YAML files
- Create default configurations
- Apply nested configuration updates
- Validate configurations before use
- Save configurations in multiple formats

### Simulation Execution
- Support for multiple data sources (files, sample data)
- Time range filtering
- Configurable time steps
- State saving for continuation runs
- Parallel processing support

### Results Analysis
- Multiple analysis types (summary, statistics, energy/water balance, temporal)
- Variable selection and filtering
- Time period aggregation
- Comparison with reference data
- Multiple output formats

### Parameter Translation
- Automatic type conversion
- Comprehensive validation
- Clear error messages
- Support for nested parameters
- Mapping between MCP and SuPy conventions

## Integration with AI Assistants

The MCP server is designed to be used by AI assistants for:

1. **Interactive configuration**: AI can help users configure simulations through conversation
2. **Guided execution**: AI can manage simulation runs with appropriate parameters
3. **Results interpretation**: AI can analyze and explain simulation outputs
4. **Error resolution**: AI can help diagnose and fix configuration or execution issues
5. **Workflow automation**: AI can orchestrate complete modeling workflows

## Limitations

- File I/O operations require appropriate permissions
- Large simulations may require significant memory
- Parallel processing depends on system capabilities
- Some advanced SuPy features may not be exposed through MCP

## Contributing

Contributions are welcome! Please ensure:
- All tests pass
- New features include tests
- Documentation is updated
- Error handling is comprehensive
- Responses follow the structured format

## License

This module is part of SuPy and follows the same license terms.