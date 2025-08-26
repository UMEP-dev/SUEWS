---
name: suews-mcp
description: MCP server to expose SUEWS urban climate modeling capabilities to AI assistants
status: backlog
created: 2025-08-26T14:26:13Z
---

# PRD: suews-mcp

## Executive Summary

The SUEWS MCP Server will provide a standardized interface between the SUEWS (Surface Urban Energy and Water Balance Scheme) urban climate model and AI assistants like Claude through the Model Context Protocol. This integration will democratize access to sophisticated urban climate modeling by enabling natural language interaction, automated configuration, and intelligent analysis of simulation results. The server will transform how researchers, planners, and students interact with SUEWS, reducing the learning curve and improving productivity by enabling AI-assisted modeling workflows.

## Problem Statement

### Current Challenges
Urban climate modeling with SUEWS requires significant expertise in:
- Understanding complex parameter configurations with hundreds of variables
- Preparing meteorological forcing data in specific formats
- Interpreting multi-dimensional output data
- Diagnosing model errors and convergence issues
- Optimizing parameters for specific locations and conditions

These challenges create barriers for:
- **New users**: Steep learning curve prevents adoption
- **Occasional users**: Forget syntax and parameters between uses
- **Non-experts**: City planners and policymakers struggle with technical complexity
- **Researchers**: Spend excessive time on configuration rather than analysis
- **Students**: Difficulty learning without constant guidance

### Why Now?
- MCP standard (released November 2024) provides mature protocol for AI-tool integration
- Growing demand for urban climate assessments due to climate change
- Increasing need for rapid urban heat island mitigation strategies
- AI assistants now sophisticated enough to understand scientific workflows
- SuPy Python interface makes programmatic access feasible

## User Stories

### Primary Persona: Urban Climate Researcher
**As a** researcher studying urban heat islands  
**I want to** configure and run SUEWS simulations through natural language  
**So that** I can focus on scientific questions rather than technical implementation  

**Acceptance Criteria:**
- Can describe study area and have AI generate appropriate configuration
- Receives intelligent suggestions for parameter values based on location
- Gets clear explanations of model outputs and uncertainties

### Secondary Persona: City Planner
**As a** city planner evaluating heat mitigation strategies  
**I want to** compare different urban greening scenarios easily  
**So that** I can make evidence-based policy recommendations  

**Acceptance Criteria:**
- Can describe scenarios in plain language (e.g., "increase tree cover by 20%")
- Receives visualizations comparing scenario outcomes
- Gets non-technical summaries of results

### Tertiary Persona: Graduate Student
**As a** student learning urban meteorology  
**I want to** understand SUEWS through interactive exploration  
**So that** I can build expertise progressively  

**Acceptance Criteria:**
- Can ask questions about model physics and get clear explanations
- Receives guidance on appropriate parameter ranges
- Gets feedback on configuration errors with educational context

## Requirements

### Functional Requirements

#### Core Model Operations
1. **Configuration Management**
   - Parse and validate SUEWS configuration files
   - Generate configurations from natural language descriptions
   - Suggest parameter values based on location/climate zone
   - Validate parameter combinations for physical consistency

2. **Simulation Execution**
   - Run single-grid simulations with SuPy
   - Support multi-grid simulations (up to 10 grids simultaneously)
   - Handle multi-year simulations efficiently
   - Provide progress updates during long runs
   - Support simulation interruption and resumption

3. **Data Preprocessing**
   - Convert meteorological data from common formats (CSV, NetCDF)
   - Fill missing data with appropriate methods
   - Validate forcing data quality
   - Generate synthetic data for testing scenarios

4. **Results Analysis**
   - Extract key metrics (energy balance, water balance, temperatures)
   - Generate standard plots and visualizations
   - Compare simulation outputs across scenarios
   - Calculate derived statistics and indicators

5. **Error Diagnosis**
   - Interpret error messages into actionable guidance
   - Suggest parameter adjustments for convergence issues
   - Validate input data consistency
   - Provide debugging workflows for common problems

### MCP Protocol Implementation

#### Resources (Exposed Data)
- Configuration templates for different urban types
- Parameter databases by climate zone
- Example forcing data files
- Validation datasets
- Documentation and tutorials

#### Tools (Executable Functions)
- `configure_simulation`: Generate/modify configuration
- `validate_config`: Check configuration validity
- `preprocess_forcing`: Prepare meteorological data
- `run_simulation`: Execute SUEWS via SuPy
- `analyze_results`: Process and visualize outputs
- `compare_scenarios`: Multi-scenario analysis
- `optimize_parameters`: Automated calibration
- `diagnose_errors`: Troubleshoot issues

#### Prompts (Guided Interactions)
- "Setup urban climate simulation"
- "Analyze heat mitigation scenario"
- "Calibrate model for location"
- "Diagnose simulation errors"
- "Compare surface configurations"

### Non-Functional Requirements

#### Performance
- Query response time: < 5 seconds for configuration/analysis
- Simulation startup: < 10 seconds
- Support for multi-year simulations without timeout
- Efficient memory usage for 10 concurrent grids
- Streaming updates for long-running operations

#### Reliability
- Graceful handling of invalid configurations
- Recovery from simulation failures
- Consistent state management across sessions
- Audit logging of all operations

#### Usability
- Self-documenting API with clear function descriptions
- Intelligent error messages with suggested fixes
- Progressive disclosure of complexity
- Context-aware help and examples

#### Compatibility
- Python version: Match SuPy requirements
- Operating systems: Windows, macOS, Linux
- File formats: CSV, TXT, NetCDF, HDF5
- Preserve compatibility with existing SUEWS workflows

## Success Criteria

### Quantitative Metrics
1. **Efficiency Gains**
   - 50% reduction in time to configure first simulation
   - 75% reduction in configuration errors
   - 10x increase in scenario comparisons per day

2. **Adoption Metrics**
   - 100+ simulations run via MCP in first month
   - 20+ unique users within 3 months
   - 80% of users successfully complete first simulation

3. **Quality Metrics**
   - 95% of AI-generated configurations pass validation
   - < 5% of simulations fail due to configuration issues
   - 90% of error diagnoses lead to successful resolution

### Qualitative Metrics
- Positive feedback from non-expert users on accessibility
- Reduced support requests for basic configuration
- Increased exploration of parameter space
- Better understanding of model sensitivity

## Constraints & Assumptions

### Technical Constraints
- Must work within MCP protocol specifications
- Limited to SuPy-accessible functionality
- Memory constraints for large grid simulations
- Python Global Interpreter Lock affects parallelization

### Assumptions
- Users have SuPy installed and configured
- Basic Python environment is available
- Users understand urban climate concepts
- AI assistant has scientific domain knowledge

### Dependencies
- SuPy package and its dependencies
- MCP Python SDK
- NumPy, Pandas for data manipulation
- Matplotlib for basic visualizations
- Local file system access for data I/O

## Out of Scope

1. **Installation and Environment Setup**
   - Python installation
   - SuPy compilation
   - Operating system configuration
   - Dependency management

2. **Advanced Features**
   - Direct Fortran code modification
   - Custom physics implementations
   - Real-time data assimilation
   - Web-based GUI development

3. **External Integrations**
   - UMEP plugin functionality
   - GIS software connections
   - Cloud compute orchestration
   - Database management systems

4. **Model Development**
   - New physics schemes
   - Algorithm optimization
   - Fortran code debugging
   - Model coupling with other systems

## Dependencies

### External Dependencies
- **SuPy**: Python wrapper for SUEWS model
- **MCP Python SDK**: Protocol implementation
- **Scientific Python Stack**: NumPy, Pandas, Matplotlib
- **AI Assistant**: Claude or compatible MCP client

### Internal Dependencies
- SUEWS configuration templates
- Parameter lookup tables
- Validation rule sets
- Example datasets

## Implementation Approach

### Phase 1: Core Infrastructure (Week 1)
- Set up MCP server skeleton
- Implement basic configuration tools
- Create simulation runner
- Add minimal error handling

### Phase 2: Data Operations (Week 2)
- Preprocessing utilities
- Results extraction
- Basic visualizations
- Scenario comparison

### Phase 3: Intelligence Layer (Week 3)
- Parameter optimization
- Error diagnosis system
- Configuration validation
- Smart suggestions

### Phase 4: Polish & Documentation (Week 4)
- Comprehensive testing
- Performance optimization
- Self-documentation system
- Example workflows

## Risks & Mitigation

### Technical Risks
| Risk | Likelihood | Impact | Mitigation |
|------|------------|---------|------------|
| MCP protocol changes | Low | High | Version pin SDK, abstract interface |
| SuPy API changes | Low | High | Version compatibility layer |
| Performance bottlenecks | Medium | Medium | Implement caching, async operations |
| Memory issues with large grids | Medium | Medium | Streaming processing, chunking |

### User Risks
| Risk | Likelihood | Impact | Mitigation |
|------|------------|---------|------------|
| Over-reliance on AI suggestions | High | Medium | Include uncertainty estimates |
| Misinterpretation of results | Medium | High | Clear caveats and limitations |
| Invalid scientific conclusions | Low | High | Validation against benchmarks |

## Future Enhancements

### Version 2.0 Considerations
- Cloud deployment capabilities
- Web API endpoints
- Real-time weather data integration
- Machine learning parameter estimation
- Multi-model ensemble runs
- Integration with other urban models

### Long-term Vision
- Comprehensive urban climate modeling platform
- AI-assisted research workflows
- Automated report generation
- Policy impact assessment tools
- Educational curriculum integration

## Appendix: Technical Architecture

### Component Structure
```
suews-mcp/
├── server.py           # Main MCP server
├── tools/              # MCP tool implementations
│   ├── configuration.py
│   ├── simulation.py
│   ├── preprocessing.py
│   └── analysis.py
├── resources/          # Static resources
│   ├── templates/
│   ├── parameters/
│   └── examples/
├── prompts/           # Guided workflows
└── utils/             # Helper functions
```

### Data Flow
1. User query → AI Assistant
2. AI Assistant → MCP Client
3. MCP Client → SUEWS MCP Server
4. MCP Server → SuPy
5. SuPy → SUEWS Fortran
6. Results → MCP Server → AI → User

### Key Design Decisions
- **Stateless operations**: Each request independent
- **File-based data exchange**: Leverage existing formats
- **Lazy loading**: Resources loaded on demand
- **Comprehensive logging**: Full audit trail
- **Fail-safe defaults**: Conservative parameter choices