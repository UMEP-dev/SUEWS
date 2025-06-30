# SUEWS MCP Server Development Plan

## Project Overview
AI-powered Model Context Protocol (MCP) server for SUEWS (Surface Urban Energy and Water balance Scheme) that provides intelligent configuration guidance and result interpretation through Claude Desktop.

## Current Status (2025-06-30)

### ✅ Phase 1: Core Infrastructure (COMPLETED)
- [x] Set up MCP server with FastMCP framework
- [x] Create project structure with proper packaging
- [x] Implement SuPy data model integration
- [x] Build desktop extension packaging system
- [x] Add dependency checking mechanisms

### ✅ Phase 2: Tool Implementation (COMPLETED)
Implemented all 10 core tools:

1. **Configuration Tools**
   - [x] `validate_suews_config` - Validate YAML configurations
   - [x] `suggest_suews_parameters` - Context-aware parameter suggestions
   - [x] `check_suews_physics_compatibility` - Physics method compatibility
   - [x] `generate_suews_template` - Site-specific templates
   - [x] `explain_suews_parameter` - Parameter documentation

2. **Analysis Tools**
   - [x] `diagnose_suews_energy_balance` - Energy balance analysis
   - [x] `interpret_suews_thermal_comfort` - Comfort indices interpretation
   - [x] `analyze_suews_urban_effects` - Urban climate effects
   - [x] `validate_suews_against_observations` - Model validation
   - [x] `generate_suews_insights_report` - Comprehensive reports

### ✅ Phase 3: Testing & Deployment (COMPLETED)
- [x] Command-line testing interface (`mcp_cli.py`)
- [x] Async client tests
- [x] Desktop extension build system
- [x] Dependency checking with helpful errors
- [x] UV environment support

## Technical Achievements

### Architecture
```
mcp-server/
├── src/suews_mcp/
│   ├── server.py          # FastMCP server (10 tools)
│   ├── tools/             # Tool implementations
│   └── utils/             # SuPy bridge
├── manifest.json          # Desktop extension config
├── mcp_cli.py            # CLI testing interface
└── build_extension.py    # DXT packaging
```

### Key Features
- **Dependency Management**: Requires `supy==2025.6.2.dev`
- **Error Handling**: Clear messages when SuPy is missing
- **Testing**: Multiple test scripts for different scenarios
- **Desktop Integration**: 152KB .dxt package ready

### Testing Tools
- `mcp_cli.py` - Command-line interface for all tools
- `test_as_claude.py` - Simulates Claude Desktop environment
- `check_supy.py` - Detailed dependency checking
- UV virtual environment for fast development

## 🚧 Phase 4: Enhancement & Expansion (TODO)

### 4.1 Knowledge Base Development
- [ ] Complete parameter database with all SUEWS parameters
- [ ] Add typical values by climate zone
- [ ] Include scientific references
- [ ] Create example configuration library

### 4.2 Advanced Validation
- [ ] Deep physics compatibility matrix
- [ ] Cross-parameter validation rules
- [ ] Data quality checks for forcing files
- [ ] Common error pattern detection

### 4.3 Result Processing
- [ ] Parse SUEWS output file formats
- [ ] Generate time series plots
- [ ] Calculate derived metrics
- [ ] Export reports in multiple formats

### 4.4 Interactive Features
- [ ] Configuration wizard (guided setup)
- [ ] Parameter sensitivity analysis
- [ ] Scenario comparison tools
- [ ] Real-time validation feedback

### 4.5 Integration & Connectivity
- [ ] UMEP project import/export
- [ ] ERA5 data downloader
- [ ] WRF coupling guidance
- [ ] Database of urban morphology

## 📋 Known Issues & Limitations

1. **SuPy Version Dependency**
   - Locked to specific dev version
   - Need to handle version compatibility
   - Desktop extension requires system Python installation

2. **Knowledge Gaps**
   - Parameter database incomplete
   - Limited physics compatibility rules
   - No visualization capabilities yet

3. **User Experience**
   - Manual SuPy installation required
   - No progress indicators for long operations
   - Limited error recovery options

## 🎯 Next Development Steps

### Immediate (Week 1)
1. Create comprehensive parameter database
2. Add more example configurations
3. Improve error messages and recovery

### Short-term (Month 1)
1. Implement configuration wizard
2. Add basic plotting capabilities
3. Create tutorial documentation
4. Expand physics compatibility rules

### Medium-term (Month 2-3)
1. Build result analysis dashboard
2. Add data source connectors
3. Implement caching for performance
4. Create video tutorials

### Long-term (Month 4-6)
1. Machine learning for parameter optimization
2. Integration with QGIS/UMEP
3. Multi-site comparison tools
4. Cloud deployment options

## 📊 Success Metrics

- **Functionality**: All 10 tools respond correctly
- **Performance**: <1s response time for most operations
- **Reliability**: Clear errors, no crashes
- **Usability**: Works in Claude Desktop with minimal setup
- **Coverage**: Handles common SUEWS use cases

## 🔧 Development Guidelines

### Adding New Tools
1. Implement in `src/suews_mcp/tools/`
2. Register in `server.py` with `@mcp.tool`
3. Update `manifest.json`
4. Add tests in `test_all_tools.py`
5. Document in README

### Testing Protocol
1. Test with `mcp_cli.py` first
2. Run `test_all_tools.py`
3. Build extension with `build_extension.py`
4. Test in Claude Desktop

### Code Standards
- Async functions for all tools
- Type hints for parameters
- Descriptive error messages
- British English in documentation

## 📚 Resources

- **SUEWS Documentation**: https://suews.readthedocs.io
- **MCP Specification**: https://modelcontextprotocol.io
- **SuPy Repository**: https://github.com/UMEP-dev/SUEWS
- **Desktop Extensions**: https://github.com/anthropics/dxt

## Version History

- **v1.0.0** (2025-06-30): Initial release with 10 tools
  - Core validation and guidance tools
  - Desktop extension support
  - CLI testing interface

---

*Last Updated: 2025-06-30*
*Author: Ting Sun & Claude Code*