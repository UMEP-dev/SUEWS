# SUEWS MCP Server

SUEWS urban climate model integration for Claude Desktop and Claude Code via Model Context Protocol (MCP).

## Quick Start

### For AI Users (Recommended)

1. Download `suews-mcp.mcpb` from [GitHub Releases](https://github.com/UMEP-dev/SUEWS/releases)
2. Double-click the file (or drag to Claude Desktop)
3. First use: automatic setup (30-60 seconds)
4. Ready to use!

### For Python Developers

```bash
pip install suews-mcp
```

Then configure manually in `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "suews": {
      "command": "suews-mcp"
    }
  }
}
```

### For Development

```bash
cd mcp
uv venv
source .venv/bin/activate
uv pip install -e .
```

## Project Structure

```
mcp/
├── src/suews_mcp/          # Source code
│   ├── server.py           # MCP server main
│   ├── tools/              # Tool implementations
│   └── utils/              # Helper functions
├── docs/                    # Documentation
│   ├── USE_CASES.md        # Concrete use cases and scenarios
│   ├── testing/            # Testing documentation
│   ├── evaluation/         # Evaluation framework docs
│   └── setup/              # Setup and configuration guides
├── tests/                   # Unit and integration tests
├── scripts/                 # Testing and utility scripts
├── evaluation/              # Evaluation framework and results
│   ├── question_bank.json  # Test questions
│   ├── results/            # Evaluation results
│   └── *.py                # Evaluation scripts
├── dist/                    # Build artifacts (.mcpb packages)
└── pyproject.toml          # Package configuration
```

## Documentation

### Getting Started
- [Setup Guide](docs/setup/SETUP.md) - Installation and configuration
- [API Test Setup](docs/setup/API_TEST_SETUP.md) - Testing with Claude API

### Understanding the Project
- **[Use Cases](docs/USE_CASES.md)** - ⭐ **Start here!** Concrete scenarios and workflows
- [Testing Guide](docs/testing/TESTING.md) - How to test the MCP server
- [Testing Issues](docs/testing/MCP_TESTING_ISSUES.md) - Current implementation status

### Development
- [Evaluation Framework](docs/evaluation/EVALUATION_FRAMEWORK.md) - Quality assurance
- [QA Review Workflow](docs/evaluation/QA_REVIEW_WORKFLOW.md) - Review process
- [Questions List](docs/evaluation/QUESTIONS_LIST.md) - Test question bank

## Available Tools (16 total)

### Configuration Management
- `validate_config` - Validate YAML configuration files
- `create_config` - Create new configurations
- `get_config_info` - Get configuration metadata
- `update_config` - Update existing configurations
- `get_config_schema` - Get data model schema

### Simulation
- `run_simulation` - Execute SUEWS simulations

### Knowledge Base
- `get_model_docs` - Access model documentation
- `list_available_models` - List available surface types (57 models)
- `get_variable_info` - Get variable metadata (16 variables)
- `list_physics_schemes` - List physics schemes (8 schemes)
- `get_physics_implementation` - View Fortran physics code

### Utility Calculations
- `calculate_ohm_coefficients` - OHM coefficient calculations
- `calculate_surface_conductance` - Surface conductance calculations
- `calculate_roughness` - Roughness parameter calculations

### Data Analysis
- `load_results` - Load simulation results
- `export_results` - Export data to various formats

## Usage Examples

### Ask Natural Questions

```
"Create a SUEWS configuration for London with 60% buildings"
"Show me how the OHM scheme calculates storage heat flux"
"Run a simulation and plot the energy balance"
"What does SMD mean and why is it increasing?"
```

### Interactive Workflows

- **New User**: Set up first simulation in minutes
- **Researcher**: Calibrate model parameters from observations
- **Planner**: Compare green infrastructure scenarios
- **Student**: Learn urban climate interactively
- **Developer**: Validate configurations before HPC runs

See [Use Cases](docs/USE_CASES.md) for detailed scenarios.

## Current Status

**Testing Results** (as of 2025-01-21):
- **13/16 tools tested** (3 blocked by dependencies)
- **7 tools working** (54%)
- **6 tools failing** (46%)

**⚠️ Testing Caveat**: All tests run in editable mode with dev tree access. Results need validation in isolated environment. See [Isolated Testing](docs/testing/ISOLATED_TESTING.md).

See [Testing Issues](docs/testing/MCP_TESTING_ISSUES.md) for details.

### Working Tools ✓
- Configuration validation and info
- Physics scheme listing and code viewing
- Variable documentation
- Model listing

### Known Issues ✗
- `run_simulation` - Missing initial state argument
- `load_results` - Doesn't support .pkl format
- `get_model_docs` - JSON serialization error
- `calculate_roughness` - Missing met arguments
- `get_config_schema` - Response too large (92k tokens)

## Requirements

- **For MCPB**: Claude Desktop (macOS or Windows), Internet (first-time setup)
- **For pip**: Python >=3.9, supy >=2025.10.15

## Development

### Running Tests

```bash
# Unit tests
pytest tests/

# MCP tool tests
python scripts/test_mcp_local.py

# Evaluation framework
python evaluation/evaluate_mcp.py
```

### Building Distribution

```bash
# Build .mcpb package (requires Claude Code)
# Package is created in dist/
```

## Links

- **Documentation**: https://suews.readthedocs.io/en/latest/mcp-integration.html
- **Repository**: https://github.com/UMEP-dev/SUEWS
- **Issues**: https://github.com/UMEP-dev/SUEWS/issues
- **License**: GPL-V3.0

## Contributing

Issues and pull requests welcome! See main SUEWS repository for contribution guidelines.

---

**Quick Navigation**:
- 🚀 [Use Cases](docs/USE_CASES.md) - See what you can do
- 🔧 [Setup](docs/setup/SETUP.md) - Get started
- 🧪 [Testing](docs/testing/TESTING.md) - Run tests
- 📊 [Evaluation](docs/evaluation/EVALUATION_FRAMEWORK.md) - Quality assurance
- 🐛 [Known Issues](docs/testing/MCP_TESTING_ISSUES.md) - Current bugs
