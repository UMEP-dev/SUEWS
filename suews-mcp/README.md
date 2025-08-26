# SUEWS MCP Server

A Model Context Protocol (MCP) server for the SUEWS (Surface Urban Energy and Water Balance Scheme) urban climate model.

## Overview

This MCP server provides intelligent access to SUEWS model capabilities through a standardised interface. It enables AI assistants and other tools to interact with urban climate modelling workflows, including model configuration, simulation execution, and results analysis.

## Features

- **Model Configuration**: Generate and validate SUEWS configuration files
- **Simulation Management**: Run SUEWS simulations with various parameters
- **Results Analysis**: Process and interpret model outputs
- **Data Processing**: Handle forcing data and model inputs
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

## MCP Tools Available

The server provides the following MCP tools:

*Tool documentation will be added as development progresses.*

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
