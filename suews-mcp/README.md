# SUEWS MCP Server

A Model Context Protocol (MCP) server for the SUEWS (Surface Urban Energy and Water Balance Scheme) urban climate model.

## Overview

This MCP server provides intelligent access to SUEWS model capabilities through a standardised interface. It enables AI assistants and other tools to interact with urban climate modelling workflows, including model configuration, simulation execution, and results analysis.

## Features

- **Model Configuration**: Generate and validate SUEWS configuration files
- **Simulation Management**: Run SUEWS simulations with various parameters
- **Results Analysis**: Process and interpret model outputs
- **Data Processing**: Handle forcing data and model inputs
- **Data Preprocessing**: Quality check and validate meteorological forcing data with comprehensive error reporting
- **Format Conversion**: Convert between CSV, TXT, Excel, and NetCDF meteorological data formats
- **Configuration Validation**: Comprehensive validation of SUEWS configurations with detailed diagnostics
- **Parameter Guidance**: Provide expert knowledge on model parameterisation

## Installation

### Prerequisites

- Python 3.9 or higher
- SUEWS/SuPy installed and configured

### Install from Source

```bash
# Clone the repository (when available)
git clone https://github.com/UMEP-dev/SUEWS.git
cd SUEWS/suews-mcp

# Install in development mode
pip install -e .

# Or install with development dependencies
pip install -e ".[dev]"
```

### Install from PyPI

```bash
# When published to PyPI
pip install suews-mcp
```

## Quick Start

### Running the Server

```bash
# Start the MCP server
suews-mcp

# Or run directly with Python
python -m suews_mcp.server
```

### Configuration

The server can be configured through environment variables or configuration files. Details on configuration options will be provided in future updates.

## Usage Examples

*Usage examples and integration guides will be provided as the project develops.*

### Basic Model Run

*Example code for running SUEWS through the MCP interface will be added.*

### Parameter Optimisation

*Example of using the MCP server for parameter sensitivity analysis.*

### Multi-site Analysis

*Example of batch processing multiple urban sites.*

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

## Support

For support and questions:
- Check the [SUEWS documentation](https://suews.readthedocs.io/)
- Open an [issue](https://github.com/UMEP-dev/SUEWS/issues) on GitHub
- Join the SUEWS community discussions
