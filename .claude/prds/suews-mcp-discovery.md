# SUEWS MCP Server - Discovery Questions

## Context
Creating an MCP (Model Context Protocol) server to expose SUEWS (Surface Urban Energy and Water Balance Scheme) capabilities to AI assistants like Claude.

## Discovery Questions

### 1. Primary Use Cases
Which of these potential use cases are most important for your MCP server? (Rank or select top 3)

- [x ] **Interactive Model Configuration**: Allow AI to help configure SUEWS simulations through natural language
- [ x] **Real-time Analysis**: Enable AI to query running simulations and analyze results
- [ x] **Parameter Optimization**: Use AI to suggest optimal parameters based on location/climate
- [ x] **Batch Processing**: Allow AI to orchestrate multiple simulation runs
- [ x] **Educational Interface**: Help students/researchers learn SUEWS through conversational AI
- [ ] **Other**: _________________________________

### 2. Target Users
Who would primarily use this MCP-enabled SUEWS interface? (Select all that apply)

- [ x] Urban climate researchers
- [ x] City planners and policymakers
- [ x] Students learning urban meteorology
- [ x] Engineers working on urban heat mitigation
- [ x] Environmental consultants
- [ ] Other: _________________________________

### 3. Core Functionalities
Which SUEWS capabilities should be exposed through MCP? (Rank by priority: 1=highest)

- [ x] Model configuration (surface types, parameters)
- [ x] Running simulations (single/batch)
- [ x] Data preprocessing (meteorological inputs)
- [ x] Results visualization and export
- [ x] Parameter sensitivity analysis
- [ x] Model validation against observations
- [ x] Scenario comparison (what-if analysis)
- [ x] Error diagnosis and troubleshooting
- [ ] Other: _________________________________

### 4. Data Integration
What data sources should the MCP server connect to? (Select all that apply)

- [ x] Local meteorological data files (CSV, NetCDF, etc.)
- [ ] Online weather APIs (specify: _______________)
- [ x] Urban morphology databases
- [ x] Land cover datasets (e.g., from remote sensing)
- [ x] Historical simulation results database
- [ ] GitHub repositories with SUEWS configurations
- [ ] Other: _________________________________

### 5. Technical Requirements

**Python Integration:**
- Should this work with SuPy (Python interface) or directly with SUEWS Fortran? ___________
supy
- Preferred Python version: ___________
same as supy
- Required Python packages beyond SuPy: ___________
dont know yet - you decide


**Deployment:**
- [ x] Local development environment only
- [ ] Cloud deployment support needed
- [ ] Docker containerization required
- [ ] Other: _________________________________

**Performance Requirements:**
- Maximum acceptable response time for queries: ___________
5s
- Expected number of concurrent users: ___________
na
- Maximum simulation size/duration to support: ___________
10 grids at a time
several years at a time


### 6. Success Metrics
What would make this MCP server successful? (Rank by importance)

- [ x] Number of simulations run through AI interface
- [ x] Time saved in model configuration (target: ___% reduction)
- [ x] Improved accessibility for non-experts
- [ x] Better parameter optimization results
- [ x] Reduced errors in model setup
- [ x] Faster learning curve for new users
- [ ] Other: _________________________________

### 7. Specific Features

**Must-Have Features:**
1. _help with model configuration_
3. _help with data preprocessing_
2. _help with running simulations_

**Nice-to-Have Features:**
1. __parameter optimization__________
2. __data post-processing__________
3. __results interpretation__________

**Out of Scope (explicitly NOT doing):**
1. ___installation of python__________
2. ___environment setup__________

### 8. Integration Points

**Existing Tools/Workflows:**
- Current SUEWS workflow tools used: _______suews-conlidate, suews-convert, suews-run_____________
- UMEP integration needed? ___no_
- Other tools to integrate with: ___no_

### 9. Security & Access Control

- [ x] No authentication needed (local use only)
- [ ] Basic API key authentication
- [ ] User roles and permissions needed
- [ x] Audit logging required
- [ ] Other: _________________________________

### 10. Timeline & Resources

**Development Timeline:**
- Preferred completion date: _2025-09-01_
- Available for testing/feedback: _2025-08-31_

**Resources:**
- Who will maintain this after initial development? ___me_____
- Documentation requirements: __except for installation of mcp, the mcp should be self-documenting and self-explanatory_

## Additional Notes
_Please add any other requirements, constraints, or ideas here:_

---

**Instructions**: Please fill out this discovery document with your responses. You can:
- Check boxes with [x]
- Fill in blanks with your answers
- Add additional items where "Other" is specified
- Rank items by adding numbers (1, 2, 3...)

Once complete, we'll use this to create a comprehensive PRD for the SUEWS MCP server.