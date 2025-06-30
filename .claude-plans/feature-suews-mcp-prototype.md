# Feature: SUEWS MCP Server Prototype

## Context
Build a Model Context Protocol (MCP) server for SUEWS that provides AI-powered configuration guidance and result interpretation through Claude Desktop. This enables users to get intelligent help with urban climate modeling directly in Claude.

## GitHub Issues
- #431 - MCP server for SUEWS configuration guidance (PRIMARY)

## Progress Tracking

### Phase 1: Core Infrastructure ✅
- [x] Set up MCP server structure
- [x] Create FastMCP-based server
- [x] Implement SuPy integration
- [x] Build desktop extension system

### Phase 2: Tool Implementation ✅
- [x] validate_suews_config
- [x] suggest_suews_parameters
- [x] check_suews_physics_compatibility
- [x] generate_suews_template
- [x] explain_suews_parameter
- [x] diagnose_suews_energy_balance
- [x] interpret_suews_thermal_comfort
- [x] analyze_suews_urban_effects
- [x] validate_suews_against_observations
- [x] generate_suews_insights_report

### Phase 3: Testing & Deployment ✅
- [x] Create CLI testing interface (mcp_cli.py)
- [x] Add dependency checking
- [x] Build desktop extension (.dxt)
- [x] Test with UV environment
- [x] Document installation process

### Phase 4: Enhancement (TODO)
- [ ] Complete parameter database
- [ ] Add visualization capabilities
- [ ] Create configuration wizard
- [ ] Implement result dashboard

## Key Decisions
1. **Require SuPy**: Made SuPy a hard dependency rather than optional
2. **FastMCP Framework**: Used FastMCP for simpler tool registration
3. **Desktop Extension**: Created .dxt package for easy installation
4. **CLI Testing**: Built mcp_cli.py for command-line testing
5. **UV Support**: Optimized for UV package manager

## Implementation Notes

### Technical Stack
- FastMCP for server implementation
- SuPy v2025.6.2.dev for data models
- UV for fast environment setup
- Desktop extension (.dxt) packaging

### Project Structure
```
mcp-server/
├── src/suews_mcp/      # Main package
│   ├── server.py       # 10 MCP tools
│   ├── tools/          # Tool implementations
│   └── utils/          # SuPy bridge
├── mcp_cli.py          # CLI testing
├── build_extension.py  # DXT builder
└── manifest.json       # Extension config
```

### Testing Strategy
1. UV virtual environment for development
2. mcp_cli.py for command-line testing
3. Multiple test scripts for different scenarios
4. check_and_run.py for dependency validation

## Files Modified/Created
- `mcp-server/src/suews_mcp/server.py` - Main server with 10 tools
- `mcp-server/src/suews_mcp/tools/*.py` - Individual tool implementations
- `mcp-server/src/suews_mcp/utils/suews_bridge.py` - SuPy integration
- `mcp-server/manifest.json` - Desktop extension configuration
- `mcp-server/mcp_cli.py` - Command-line testing interface
- `mcp-server/build_extension.py` - Extension packaging script
- `mcp-server/check_and_run.py` - Dependency checker
- `mcp-server/pyproject.toml` - Package configuration
- `mcp-server/DEVELOPMENT_PLAN.md` - Future development roadmap

## Current Status
**COMPLETED** - All 10 tools implemented and working. Desktop extension ready for testing. Requires SuPy installation for target Python environment.

## Next Steps
1. Install SuPy for system Python: `/usr/bin/python3 -m pip install --user supy==2025.6.2.dev`
2. Test desktop extension in Claude Desktop
3. Begin Phase 4 enhancements (parameter database, visualization, etc.)

## Testing Commands
```bash
# Set up development environment
cd mcp-server
uv venv && source .venv/bin/activate
uv pip install -e .

# Test tools
python mcp_cli.py list
python mcp_cli.py explain albedo_deciduous_summer
python mcp_cli.py template suburban energy_balance

# Build extension
python build_extension.py
# Creates: dist/suews-assistant-YYYYMMDD.dxt
```