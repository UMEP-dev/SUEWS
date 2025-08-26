---
name: suews-mcp
status: in-progress
created: 2025-08-26T14:33:40Z
progress: 14%
prd: .claude/prds/suews-mcp.md
github: https://github.com/UMEP-dev/SUEWS/issues/635
updated: 2025-08-26T15:32:00Z
last_sync: 2025-08-26T15:32:00Z
---

# Epic: suews-mcp

## Overview
Implement a lightweight MCP server that wraps the existing SuPy Python interface to provide AI-assisted access to SUEWS urban climate modeling. The server will leverage SuPy's existing functionality rather than reimplementing features, focusing on translating between natural language requests and SuPy API calls while providing intelligent assistance for configuration, execution, and analysis.

## Architecture Decisions

### Core Simplifications
- **Leverage SuPy entirely**: No direct Fortran interaction, use SuPy's proven API
- **Minimal new code**: Wrap existing functionality, don't recreate it
- **File-based workflows**: Use SuPy's existing file I/O rather than custom formats
- **Stateless design**: Each MCP request maps to discrete SuPy operations

### Technology Stack
- **MCP Python SDK**: Official SDK for protocol implementation
- **SuPy**: Existing Python wrapper for SUEWS (no modifications needed)
- **Standard library**: asyncio for concurrent operations
- **Minimal dependencies**: Only add what MCP specifically requires

### Design Patterns
- **Facade pattern**: MCP tools as thin wrappers around SuPy functions
- **Command pattern**: Each MCP tool maps to specific SuPy workflow
- **Template method**: Reusable patterns for common operations

## Technical Approach

### MCP Server Core
- Single `server.py` implementing MCP protocol
- Tool registry mapping MCP functions to SuPy calls
- Resource server for templates and examples
- Simple prompt templates for guided workflows

### Tool Implementations
- **Direct SuPy mappings**: Most tools just call SuPy functions with parameter translation
- **Intelligent wrappers**: Add validation and helpful error messages
- **Batch operations**: Use SuPy's existing multi-grid support
- **Results formatting**: Transform SuPy DataFrames to AI-friendly summaries

### Resource Management
- Leverage SuPy's built-in sample data
- Use SuPy's configuration templates
- Provide minimal additional examples
- Link to existing SUEWS documentation

## Implementation Strategy

### Development Approach
- Start with minimal viable server
- Add tools incrementally, testing each
- Reuse SuPy examples as test cases
- Focus on user experience over features

### Risk Mitigation
- Pin MCP SDK version for stability
- Comprehensive error handling
- Fallback to SuPy defaults
- Clear documentation of limitations

### Testing Strategy
- Unit tests for MCP protocol compliance
- Integration tests with SuPy
- End-to-end tests with sample workflows
- Performance benchmarks for response times

## Task Breakdown Preview

High-level task categories that will be created:
- [x] **Setup**: Initialize MCP server structure and basic protocol implementation
- [ ] **Core Tools**: Implement essential SuPy wrapper tools (config, run, analyze)
- [ ] **Data Handling**: Add preprocessing and validation utilities
- [ ] **Resources**: Create minimal templates and examples
- [ ] **Intelligence**: Add smart suggestions and error diagnosis
- [ ] **Testing**: Comprehensive test suite and documentation
- [ ] **Packaging**: Distribution setup and installation guide

## Dependencies

### External Dependencies
- MCP Python SDK (latest stable)
- SuPy (existing installation required)
- Python 3.9+ (match SuPy requirements)

### Internal Dependencies
- No modifications to SuPy needed
- Use existing SUEWS test data
- Leverage SuPy documentation

### Prerequisite Work
- User must have working SuPy installation
- Python environment properly configured
- Basic understanding of SUEWS concepts

## Success Criteria (Technical)

### Performance Benchmarks
- Tool response < 5 seconds for all operations
- Support 10 concurrent grid simulations
- Handle multi-year runs without timeout
- Memory usage < 2GB for typical workflows

### Quality Gates
- 100% MCP protocol compliance
- 90% code coverage in tests
- All SuPy errors gracefully handled
- Self-documenting tool descriptions

### Acceptance Criteria
- Successfully run benchmark SUEWS simulation via MCP
- AI can configure simulation from natural language
- Clear error messages guide users to solutions
- Works on Windows, macOS, and Linux

## Estimated Effort

### Overall Timeline
- **Total Duration**: 3-4 weeks
- **Development**: 2 weeks core implementation
- **Testing & Polish**: 1 week
- **Documentation**: Throughout development

### Resource Requirements
- Single developer familiar with Python and SuPy
- Access to SUEWS test cases and documentation
- Testing environment with SuPy installed

### Critical Path Items
1. MCP server initialization (Day 1-2) ✅
2. Core SuPy tool wrappers (Day 3-7)
3. Testing and refinement (Week 2)
4. Documentation and packaging (Week 3)

## Implementation Notes

### Key Simplifications from PRD
- No custom parameter optimization (use SuPy defaults)
- No complex visualization (return data for AI to describe)
- No database integration (file-based only)
- Minimal templates (leverage SuPy examples)
- No GUI or web interface
- Focus on wrapping, not extending SuPy

### Leverage Points
- SuPy handles all SUEWS complexity
- MCP SDK provides protocol implementation
- Existing SUEWS documentation and tutorials
- SuPy's robust error handling
- Standard Python packaging tools

## Tasks Created
- [x] #636 - MCP Server Setup (parallel: true)
- [ ] #637 - Core SuPy Tools (parallel: false)
- [ ] #638 - Data Preprocessing Tools (parallel: true)
- [ ] #639 - Resource Management (parallel: true)
- [ ] #640 - Error Handling & Intelligence (parallel: false)
- [ ] #641 - Testing Suite (parallel: false)
- [ ] #642 - Packaging & Documentation (parallel: false)

Total tasks: 7
Parallel tasks: 3
Sequential tasks: 4
Estimated total effort: 64 hours
Progress: 1/7 tasks completed (14%)