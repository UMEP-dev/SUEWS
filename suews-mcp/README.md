# SUEWS MCP Server

A Model Context Protocol (MCP) server for the SUEWS (Surface Urban Energy and Water Balance Scheme) urban climate model. This server enables AI assistants and other tools to interact with SUEWS through a standardised interface, making urban climate modelling more accessible and automated.

## Overview

The SUEWS MCP server provides intelligent access to the SUEWS urban climate model through the Model Context Protocol. SUEWS is a physically-based model that simulates energy and water balance in urban areas, widely used for:

- Urban heat island analysis
- Building energy assessments  
- Stormwater management studies
- Climate adaptation planning
- Urban planning and design

This MCP server wraps SUEWS functionality into standardised tools that can be used by AI assistants, allowing for:
- Natural language interaction with complex urban climate models
- Automated parameter optimisation and sensitivity analysis
- Intelligent troubleshooting and configuration guidance
- Streamlined workflows from data preparation to results analysis

## Key Features

### 🛠️ **Model Configuration & Validation**
- Generate and validate SUEWS configuration files
- Comprehensive parameter validation with detailed diagnostics  
- Template-based configuration for different urban types (residential, commercial, industrial, parks)
- Physics option compatibility checks

### 🚀 **Simulation Management**
- Execute SUEWS simulations with custom parameters
- Progress tracking and resource management
- Support for both quick tests and long-term simulations
- Parallel simulation capabilities

### 📊 **Results Analysis & Visualisation**
- Process and interpret model outputs
- Energy balance analysis and validation
- Temporal pattern analysis (diurnal, seasonal, weekly)
- Comparison with observational data

### 🔄 **Data Processing Pipeline**
- Quality check and validate meteorological forcing data
- Format conversion (CSV, TXT, Excel, NetCDF)
- Automatic detection of data issues with auto-fix capabilities  
- Time series validation and energy balance closure checks

### 🧠 **Expert Knowledge Integration**
- Parameter guidance based on urban type and climate
- Intelligent error diagnosis and troubleshooting
- Best practices for model setup and calibration
- Automated sensitivity analysis workflows

## Installation

### Prerequisites

- Python 3.9 or higher
- SUEWS/SuPy installed and configured

### Method 1: Install from Source (Recommended)

```bash
# Clone the SUEWS repository
git clone https://github.com/UMEP-dev/SUEWS.git
cd SUEWS/suews-mcp

# Create virtual environment (recommended)
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install in development mode
pip install -e .

# Or install with development dependencies for contributing
pip install -e ".[dev]"
```

### Method 2: Install from PyPI

```bash
# When published to PyPI (coming soon)
pip install suews-mcp
```

### Verify Installation

```bash
# Check if the server starts correctly
suews-mcp --version

# Test basic functionality
python -c "import suews_mcp; print('✓ Installation successful')"
```

## Quick Start

### 1. Start the MCP Server

```bash
# Start the MCP server (default configuration)
suews-mcp

# Or run directly with Python
python -m suews_mcp.server

# Start with custom configuration
suews-mcp --config /path/to/config.yaml
```

### 2. Basic Usage with MCP Client

```python
# Example using the MCP client
import asyncio
from mcp import create_client

async def run_simulation_example():
    async with create_client("suews-mcp") as client:
        # List available tools
        tools = await client.list_tools()
        print(f"Available tools: {[t.name for t in tools.tools]}")
        
        # Run a basic simulation
        result = await client.call_tool(
            "run_simulation",
            {
                "forcing_path": "data/forcing.txt",
                "config_path": "configs/residential.yml",
                "use_sample_data": True
            }
        )
        print(result.content[0].text)

# Run the example
asyncio.run(run_simulation_example())
```

### 3. Using Built-in Templates

```bash
# List available configuration templates
curl -X POST http://localhost:8000/tools/list_resources \
  -d '{"resource_type": "config_template"}'

# Get a residential template
curl -X POST http://localhost:8000/tools/get_resource \
  -d '{"resource_path": "templates/configs/residential.yml"}'
```

### 4. Complete Workflow Example

```python
# Complete workflow: data preparation → configuration → simulation → analysis
async def complete_workflow():
    async with create_client("suews-mcp") as client:
        # Step 1: Preprocess meteorological data
        preprocessing = await client.call_tool(
            "preprocess_forcing",
            {
                "input_file": "raw_data/weather.csv",
                "output_file": "processed_data/forcing.txt",
                "validate_energy_balance": True,
                "auto_fix_issues": True
            }
        )
        
        # Step 2: Configure simulation
        config = await client.call_tool(
            "configure_simulation",
            {
                "config_path": "templates/configs/residential.yml",
                "site_name": "MyCity_Residential",
                "config_updates": {
                    "site": {
                        "lat": 51.5074,
                        "lon": -0.1278,
                        "elevation": 11
                    }
                },
                "save_path": "configs/my_simulation.yml"
            }
        )
        
        # Step 3: Validate configuration
        validation = await client.call_tool(
            "validate_config",
            {
                "config_file": "configs/my_simulation.yml",
                "strict_mode": False
            }
        )
        
        # Step 4: Run simulation
        simulation = await client.call_tool(
            "run_simulation",
            {
                "config_path": "configs/my_simulation.yml",
                "forcing_path": "processed_data/forcing.txt",
                "start_time": "2023-01-01T00:00:00",
                "end_time": "2023-12-31T23:00:00"
            }
        )
        
        # Step 5: Analyse results
        analysis = await client.call_tool(
            "analyze_results",
            {
                "results_path": "outputs/results.csv",
                "analysis_type": "energy_balance",
                "variables": ["QH", "QE", "QN", "QS"]
            }
        )
        
        return simulation, analysis
```

## Configuration

The server can be configured through:

### Environment Variables
```bash
export SUEWS_MCP_HOST="localhost"
export SUEWS_MCP_PORT="8000"
export SUEWS_MCP_MAX_CONCURRENT_SIMS="4"
export SUEWS_MCP_LOG_LEVEL="INFO"
```

### Configuration File
```yaml
# config.yaml
server:
  host: "localhost"
  port: 8000
  name: "SUEWS MCP Server"
  version: "1.0.0"

simulation:
  max_concurrent: 4
  timeout: 3600

tools:
  enable_simulation: true
  enable_validation: true
  enable_analysis: true

logging:
  level: "INFO"
  file: "suews_mcp.log"
```

## Current Status

**✅ Task #637 Implementation Complete**

The core SuPy tools for SUEWS MCP server integration have been successfully implemented. The server provides comprehensive functionality for running SUEWS urban climate simulations through the Model Context Protocol.

### Implementation Highlights
- ✅ **Core SuPy simulation tools integration** - Complete wrapper tools for SuPy functionality
- ✅ **SUEWS configuration validation tools** - Comprehensive config loading and validation
- ✅ **Result analysis and visualization tools** - Advanced analysis capabilities  
- ✅ **Integration tests with real SUEWS benchmark data** - Validated with Ward et al. (2016) model configuration
- ✅ **Comprehensive error handling and logging** - Robust production-ready implementation
- ✅ **Parameter translation between MCP JSON and SuPy Python** - Seamless data conversion

## MCP Tools Available

The server provides the following MCP tools:

### 🔍 `preprocess_forcing`
**Preprocess and validate meteorological forcing data for SUEWS simulations**

- Comprehensive data quality assessment with detailed issue reporting
- Automatic detection of missing data, out-of-range values, and temporal inconsistencies
- Energy balance validation (QN = QH + QE + QS) with closure checks
- Time series validation and timestep detection
- Auto-fix capabilities for common data issues
- Unit conversion and format standardisation

```json
{
  "input_file": "path/to/forcing_data.txt",
  "output_file": "path/to/processed_data.txt", 
  "validate_energy_balance": true,
  "auto_fix_issues": true,
  "target_timestep": 300
}
```

### ✅ `validate_config`  
**Comprehensive validation of SUEWS configuration files**

- Structural validation of YAML configuration format
- Required field validation with detailed error reporting
- Value range validation for all parameters
- Surface fraction validation (must sum to 1.0)
- Physics option compatibility checks
- File path validation for referenced forcing files
- Geographic coordinate validation

```json
{
  "config_file": "path/to/config.yml",
  "strict_mode": false,
  "check_file_paths": true
}
```

### 🔄 `convert_data_format`
**Convert meteorological data between different formats**

- Support for CSV, TXT, Excel, NetCDF, and SUEWS-specific formats
- Column mapping for variable name conversion
- Format-specific transformations and optimisations
- Preservation of data integrity during conversion
- Direct conversion to SUEWS-ready format

```json
{
  "input_file": "path/to/data.csv",
  "output_file": "path/to/suews_data.txt",
  "input_format": "csv",
  "output_format": "suews_txt",
  "column_mapping": {
    "temperature": "Tair",
    "humidity": "RH",
    "wind_speed": "U"
  }
}
```

### 🔧 `configure_simulation`
**Load, configure, validate and save SUEWS simulation parameters**

- Load configurations from YAML files or create defaults
- Apply configuration updates through JSON objects  
- Comprehensive validation with detailed error reporting
- Save configurations in YAML or JSON format
- Support for site-specific parameters and physics options

```json
{
  "config_path": "path/to/config.yml",
  "config_updates": {"model": {"control": {"tstep": 300}}},
  "site_name": "MyTestSite", 
  "save_path": "path/to/output.yml",
  "validate_only": false
}
```

### 🚀 `run_simulation`  
**Execute SUEWS urban climate model simulations with forcing data**

- Run with custom forcing data or built-in sample data
- Support for time filtering and parameter customization
- Progress tracking and comprehensive statistics
- Integration with both run_supy and SUEWSSimulation APIs
- Automatic calculation of energy balance variables

```json
{
  "forcing_path": "path/to/forcing.txt",
  "config_path": "path/to/config.yml", 
  "use_sample_data": false,
  "start_time": "2012-01-01T00:00:00",
  "end_time": "2012-12-31T23:00:00",
  "save_state": true
}
```

### 📊 `analyze_results`
**Analyze SUEWS simulation results with statistics, resampling, and visualization**

- Load results from multiple formats (CSV, TXT, Parquet, Pickle)
- Multiple analysis types: summary, statistics, energy_balance, water_balance, temporal
- Advanced temporal pattern analysis (diurnal, seasonal, weekly patterns)
- Energy balance closure validation and diagnostics
- Comparison capabilities with reference data

```json
{
  "results_path": "path/to/results.csv",
  "analysis_type": "energy_balance",
  "variables": ["QH", "QE", "QN", "QS"],
  "time_period": "monthly",
  "comparison_path": "path/to/reference.csv"
}
```

## Development

### Setting up Development Environment

```bash
# Clone the repository
git clone https://github.com/UMEP-dev/SUEWS.git
cd SUEWS/suews-mcp

# Create virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install development dependencies
pip install -e ".[dev]"

# Run tests
pytest
```

### Code Quality

This project uses:
- **Black** for code formatting
- **Ruff** for linting
- **MyPy** for type checking
- **pytest** for testing

```bash
# Format code
black src/ tests/

# Lint code
ruff check src/ tests/

# Type check
mypy src/

# Run tests
pytest
```

## Contributing

Contributions are welcome! Please see the main SUEWS project guidelines for contributing standards and processes.

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](../LICENSE) file for details.

## Related Projects

- [SUEWS](https://github.com/UMEP-dev/SUEWS) - The main SUEWS model
- [SuPy](https://supy.readthedocs.io/) - Python interface for SUEWS
- [UMEP](https://umep-docs.readthedocs.io/) - Urban Multi-scale Environmental Predictor

## Documentation

- **[Quick Start Guide](docs/quickstart.md)**: Step-by-step tutorial for new users
- **[API Reference](docs/api_reference.md)**: Complete documentation of all MCP tools
- **[Examples](docs/examples/)**: Practical examples for common use cases
- **[FAQ & Troubleshooting](docs/faq.md)**: Solutions to common issues
- **[Jupyter Notebooks](examples/notebooks/)**: Interactive tutorials

## Performance & Scalability

- **Concurrent Simulations**: Run multiple simulations in parallel
- **Resource Management**: Automatic memory and CPU monitoring
- **Caching**: Intelligent caching of configuration and results
- **Error Recovery**: Robust error handling with detailed diagnostics

## Troubleshooting

### Common Issues

**Server won't start:**
```bash
# Check if port is available
lsof -i :8000

# Try different port
suews-mcp --port 8001
```

**Import errors:**
```bash
# Verify SUEWS/SuPy installation
python -c "import supy; print('✓ SuPy available')"

# Check Python path
echo $PYTHONPATH
```

**Simulation failures:**
```bash
# Check logs
tail -f suews_mcp.log

# Validate configuration first
curl -X POST http://localhost:8000/tools/validate_config \
  -d '{"config_file": "config.yml"}'
```

### Getting Help

1. **Check the logs**: Most issues are explained in the server logs
2. **Use validation tools**: Always validate configurations before running
3. **Start simple**: Use provided templates and sample data
4. **Check documentation**: See [docs/](docs/) for detailed guides

For more troubleshooting help, see [docs/faq.md](docs/faq.md)

## API Overview

The server provides **10+ MCP tools** organised into categories:

### Data Processing Tools
- `preprocess_forcing` - Quality check meteorological data
- `convert_data_format` - Convert between data formats
- `validate_config` - Comprehensive configuration validation

### Simulation Tools  
- `configure_simulation` - Load and configure SUEWS parameters
- `run_simulation` - Execute SUEWS urban climate simulations
- `analyze_results` - Analyse simulation outputs

### Resource Tools
- `list_resources` - Browse available templates and examples
- `get_resource` - Retrieve specific templates or documentation
- `health_check` - Monitor server status

For complete API documentation, see [docs/api_reference.md](docs/api_reference.md)

## Examples

### Urban Heat Island Study
```python
# Compare residential vs commercial areas
for urban_type in ["residential", "commercial"]:
    config = await client.call_tool(
        "configure_simulation",
        {"config_path": f"templates/configs/{urban_type}.yml"}
    )
    
    results = await client.call_tool(
        "run_simulation", 
        {"config_path": config, "forcing_path": "data/heatwave.txt"}
    )
```

### Parameter Sensitivity Analysis
```python
# Test impact of albedo changes
for albedo in [0.1, 0.15, 0.2, 0.25]:
    config_updates = {"surface": {"albedo_paved": albedo}}
    
    result = await client.call_tool(
        "run_simulation",
        {
            "config_path": "base_config.yml",
            "config_updates": config_updates
        }
    )
```

More examples available in [docs/examples/](docs/examples/) and [examples/notebooks/](examples/notebooks/)

## Contributing

Contributions are welcome! Please see the [CONTRIBUTING.md](CONTRIBUTING.md) file for:
- Development setup instructions
- Code style guidelines
- Testing requirements
- Pull request process

### Development Setup
```bash
# Fork and clone
git clone https://github.com/yourusername/SUEWS.git
cd SUEWS/suews-mcp

# Install development dependencies
pip install -e ".[dev]"

# Run tests
pytest

# Run code quality checks
black src/ tests/
ruff check src/ tests/
mypy src/
```

## Support

For support and questions:
- **📖 Documentation**: Check [docs/](docs/) first
- **🐛 Issues**: Open an [issue](https://github.com/UMEP-dev/SUEWS/issues) on GitHub
- **💬 Discussions**: Join [SUEWS community discussions](https://github.com/UMEP-dev/SUEWS/discussions)
- **📧 Email**: Contact the SUEWS development team

### Related Resources
- **[SUEWS Documentation](https://suews.readthedocs.io/)**: Complete model documentation
- **[SuPy Documentation](https://supy.readthedocs.io/)**: Python interface documentation
- **[UMEP](https://umep-docs.readthedocs.io/)**: Urban Multi-scale Environmental Predictor
