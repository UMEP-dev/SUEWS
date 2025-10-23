# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Setup

**Quick start**:
```bash
cd mcp
uv venv
source .venv/bin/activate
uv pip install -e .
```

**IMPORTANT**: This MCP server depends on `supy>=2025.10.15`. For development:
- Use prebuilt `supy` from PyPI (not editable install from parent SUEWS repo)
- Editable supy uses meson-python which causes import failures when Claude Desktop launches MCP server
- Install with: `uv pip install supy==2025.10.15`

## Testing

```bash
# Unit tests
pytest tests/

# Test specific tool
pytest tests/test_configure.py -v

# Local MCP tool testing (without Claude Desktop)
python scripts/test_mcp_local.py

# Evaluation framework (quality assurance)
python evaluation/evaluate_mcp.py
```

## Architecture

### Core Structure

**MCP Server** (`src/suews_mcp/server.py`):
- Defines 17 MCP tools using `@app.list_tools()` decorator
- Routes tool calls to appropriate modules via `@app.call_tool()` handler
- Server named "supy-mcp" (line 16)

**Tool Categories** (src/suews_mcp/tools/):
- `configure.py`: Config validation, creation, updates (uses SUEWSConfig data model)
- `simulate.py`: Run SUEWS simulations
- `knowledge.py`: Access schemas, docs, physics code (57 Pydantic models, 24 Fortran modules)
- `forcing.py`: ERA5 data download and conversion
- `utilities.py`: OHM coefficients, surface conductance calculations
- `analyze.py`: Load and export simulation results

**Helper Functions** (`src/suews_mcp/utils/helpers.py`):
- YAML file I/O with validation error formatting
- All tools use these for consistent error handling

### Key Design Patterns

**Async by default**: All tool functions are `async` (required by MCP protocol)

**Validation**: Use Pydantic models from `supy.data_model.core` for configuration validation

**Nested config updates**: `update_config()` uses `_recursive_update()` helper to handle nested dictionary updates without flattening (matches SUEWSSimulation behavior)

**Token limit handling**:
- `get_config_schema`: Returns navigation guide (~500 tokens) instead of full schema (92k tokens)
- `get_physics_implementation`: Smart size handling - full code for small schemes (<20k tokens), structured summary for large schemes (≥20k tokens)

## Building Distribution

**Build wheel** (for pip installation):
```bash
uv build --wheel --out-dir dist/
```

**Build .mcpb package** (for Claude Desktop):
- Requires Claude Code MCP bundler
- Creates self-contained package in `dist/`
- Includes bootstrap.js and manifest.json

## Common Tasks

**Add new MCP tool**:
1. Implement async function in appropriate `tools/*.py` module
2. Add Tool definition to `list_tools()` in `server.py`
3. Add routing case to `call_tool()` handler in `server.py`
4. Write tests in `tests/test_*.py`

**Update dependencies**:
- Edit `pyproject.toml` dependencies
- Rebuild: `uv pip install -e .`

**Debug tool failures**:
- Check Claude Desktop logs: `~/Library/Logs/Claude/mcp-server-*.log`
- Test locally: `python scripts/test_mcp_local.py`
- Enable stderr output: `print('...', file=sys.stderr)` appears in Claude Desktop logs

## Important Files

**Package metadata**:
- `src/suews_mcp/data/variables_metadata.json`: SUEWS output variable definitions (16 variables)
- `src/suews_mcp/physics_code/*.f95`: Fortran physics source code (24 modules, read-only access for AI)

**Documentation** (extensive):
- `docs/USE_CASES.md`: Concrete workflow scenarios
- `docs/testing/TESTING.md`: Testing guide
- `docs/evaluation/EVALUATION_FRAMEWORK.md`: QA framework
- `evaluation/question_bank.json`: Test questions for evaluation

## Naming Conventions

**"Config" vs "Physics/Model"**:
- Use "config" for configuration parameters (what users set)
- Use "physics" or "scheme" for implementation details (Fortran code)
- Example: `get_config_docs` (parameters), `get_physics_implementation` (Fortran code)

## Known Issues

**Claude Desktop Integration**:
- Use prebuilt supy (not editable) to avoid meson-python rebuild failures
- MCP server name "supy-mcp" creates log file `mcp-server-suews.log` (name mismatch is normal)
- First launch may take 30-60s (dependency installation)

**Testing Limitations**:
- Some tests require sample config files from installed supy package
- Evaluation framework tests require Claude API access
