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
- **[Skills Integration Strategy](docs/SKILLS_INTEGRATION_STRATEGY.md)** - 🚀 **Future direction!** Workflow automation plans
- [Testing Guide](docs/testing/TESTING.md) - How to test the MCP server
- [Token Limit Solutions](docs/testing/TOKEN_LIMIT_SOLUTIONS.md) - How we handle large responses

### Development
- [Evaluation Framework](docs/evaluation/EVALUATION_FRAMEWORK.md) - Quality assurance
- [QA Review Workflow](docs/evaluation/QA_REVIEW_WORKFLOW.md) - Review process
- [Questions List](docs/evaluation/QUESTIONS_LIST.md) - Test question bank

## Available Tools (17 total, 100% working ✅)

### Configuration Management (4 tools)
- `validate_config` - Validate YAML configuration files
- `create_config` - Create new configurations (with nested updates support)
- `get_config_info` - Get configuration metadata
- `update_config` - Update existing configurations (supports nested structures)

### Simulation (1 tool)
- `run_simulation` - Execute SUEWS simulations (with date validation)

### Knowledge Base (7 tools)
- `get_config_schema` - Get configuration schema overview (navigation guide)
- `get_config_docs` - Access configuration parameter documentation (57 models)
- `list_available_models` - List available Pydantic models
- `get_variable_info` - Get variable metadata (16 variables)
- `list_physics_schemes` - List physics schemes (24 modules: 18 physics + 4 control + 2 utility)
- `get_physics_implementation` - View Fortran physics code (smart size handling)
- `get_forcing_format_guide` - SUEWS forcing file format documentation

### Forcing Data (1 tool)
- `get_era5_forcing` - Download and convert ERA5 data to SUEWS format (global coverage)

### Utility Calculations (2 tools)
- `calculate_ohm_coefficients` - OHM coefficient calculations
- `calculate_surface_conductance` - Surface conductance calculations

### Data Access (2 tools)
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

**Production Ready** (as of 2025-10-22):
- **15/15 tools working** (100% success rate) ✅
- **Token limit issues resolved** (Bug #2 and #7 fixed)
- **Complete Fortran coverage** (24 modules accessible)
- **Naming clarified** (config vs physics distinction)

### Recent Improvements

**Token Limit Fixes** (Build 2025-10-22):
- `get_config_schema`: Returns navigation guide (~500 tokens) instead of full schema (92k tokens)
- `get_physics_implementation`: Smart size handling - full code for small schemes (<20k tokens), structured summary with subroutines for large schemes (≥20k tokens)

**Naming Clarification**:
- `get_model_docs` → `get_config_docs` (configuration parameters, not physics models)
- Clear distinction: "config" for configuration, "model" for physics/SUEWS logic

**Complete Fortran Code Access** (8 → 24 modules):
- **Physics schemes** (18): OHM, water_balance, evaporation, LUMPS, NARP, anthropogenic_heat, snow, SPARTACUS, ESTM, BEERS, SOLWEIG, STEBBS, resistance, RSL, biogenic_CO2, atmospheric_stability, daily_state, element_heat_capacity
- **Control modules** (4): driver, constants, types, output
- **Utility modules** (2): meteorology, time_utilities

### Next Phase: Claude Skills Integration

See [Skills Integration Strategy](docs/SKILLS_INTEGRATION_STRATEGY.md) for plans to build high-level workflow automation on top of MCP tools.

**Priority**: Implement 4 analysis tools (energy_balance, statistics, water_balance, performance) to enable workflow orchestration.

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
