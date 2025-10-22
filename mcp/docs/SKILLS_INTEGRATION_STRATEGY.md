# Claude Skills + SUEWS MCP Integration Strategy

**Date**: 2025-10-22
**Status**: Planning phase
**Context**: Following completion of token limit fixes and full Fortran code integration

---

## Executive Summary

Claude Skills provide workflow orchestration capabilities that can transform the SUEWS MCP server from 15 atomic tools into high-level, domain-specific workflows. This document outlines the integration strategy, identifies priority skills, and defines implementation phases.

## What Are Claude Skills?

**Claude Skills** are composable folders containing:
- Markdown instructions with YAML frontmatter
- Optional executable scripts
- Resources and examples

**Key characteristics**:
- **Auto-detection**: Claude identifies relevant skills based on task metadata
- **Composability**: Skills can invoke other skills and MCP tools
- **Portability**: Same skills work across Claude.ai, Claude Code, and API
- **Progressive disclosure**: Load information only as needed (critical for large codebases)

**Availability**: Pro, Max, Team, Enterprise users across all Claude platforms

## Skills vs MCP: Architecture

```
User Request
    ↓
Claude Skills Layer ← High-level workflow orchestration
    ↓
MCP Tools Layer ← Atomic operations (15 tools)
    ↓
SUEWS/SuPy ← Underlying model
```

**Relationship**:
- **MCP**: Secure, governed integration fabric ("USB-C for AI")
- **Skills**: Domain expertise packages that orchestrate MCP tools
- **Synergy**: Skills provide the "how" (workflow), MCP provides the "what" (operations)

## Current SUEWS MCP Status

### Available Tools (15 total, 100% working)

**Configuration** (5 tools):
- validate_config, create_config, get_config_info, update_config
- get_config_schema (navigation guide, not raw schema)

**Knowledge** (5 tools):
- get_config_docs (renamed from get_model_docs for clarity)
- list_available_models (57 configuration models)
- get_variable_info (16 output variables)
- list_physics_schemes (24 modules: 18 physics + 4 control + 2 utility)
- get_physics_implementation (smart size handling: full code <20k tokens, structured summary ≥20k)

**Simulation** (1 tool):
- run_simulation

**Utilities** (2 tools):
- calculate_ohm_coefficients
- calculate_surface_conductance

**Data Access** (2 tools):
- load_results
- export_results

### Critical Gap: Analysis Tools

**Current limitation**: "Analysis" tools only provide data I/O, not insights generation.

**Blocker for skills**: Many workflow patterns require:
- analyze_energy_balance() - Closure, component breakdown, diagnostics
- calculate_statistics() - Temporal/spatial statistics
- analyze_water_balance() - Runoff, evaporation, storage analysis
- calculate_performance() - Model-observation comparison metrics

**Impact**: Skills requiring analysis will be incomplete without these tools.

## High-Priority Skills for SUEWS

### 1. Urban Heat Island Analysis Skill

**Purpose**: End-to-end UHI assessment from configuration to report

**YAML Metadata**:
```yaml
name: urban-heat-island-analysis
description: Analyse urban heat island effects with SUEWS
category: research-workflow
tools: [validate_config, run_simulation, analyze_energy_balance, get_variable_info]
outputs: [plots, statistics, report]
```

**Workflow**:
1. Validate urban and rural configurations (validate_config)
2. Run baseline urban simulation (run_simulation)
3. Run reference rural simulation (run_simulation)
4. Calculate UHI intensity (ΔT analysis) - **requires analyze_energy_balance**
5. Analyse energy balance differences - **requires analyze_energy_balance**
6. Generate comparative visualisation
7. Produce summary report with guidance

**User experience**:
```
User: "Analyse the UHI effect for this London configuration"
Skill: [automatically orchestrates 7-step workflow]
Output: Report with UHI intensity, energy drivers, visualisations
```

**Value**: Compresses expert workflow into single request

### 2. Model Calibration Skill

**Purpose**: Iterative parameter tuning with performance feedback

**YAML Metadata**:
```yaml
name: suews-calibration
description: Calibrate SUEWS parameters against observations
category: model-setup
tools: [get_config_info, update_config, run_simulation, calculate_performance, calculate_ohm_coefficients, calculate_surface_conductance]
iterative: true
```

**Workflow**:
1. Load baseline configuration (get_config_info)
2. Run simulation with current parameters (run_simulation)
3. Calculate performance metrics - **requires calculate_performance**
4. Identify poor-performing components:
   - Energy balance → calculate_ohm_coefficients
   - Evaporation → calculate_surface_conductance
5. Update configuration with suggested values (update_config)
6. Re-run simulation and validate improvement
7. Report calibration summary (before/after metrics)

**User experience**:
```
User: "Calibrate SUEWS for this site with these observations"
Skill: [iteratively tunes parameters, reports progress]
Output: Optimised configuration + performance summary
```

**Value**: Automates what currently requires expert knowledge and trial-and-error

### 3. Physics Investigation Skill

**Purpose**: Navigate SUEWS source code to understand calculations

**YAML Metadata**:
```yaml
name: suews-physics-navigator
description: Explore SUEWS physics implementation and algorithms
category: learning
tools: [list_physics_schemes, get_physics_implementation, get_config_docs, get_variable_info]
educational: true
```

**Workflow**:
1. User asks: "How does SUEWS calculate sensible heat flux?"
2. Identify relevant schemes (list_physics_schemes → LUMPS)
3. Retrieve implementation (get_physics_implementation → LUMPS Fortran code)
4. Parse algorithm from source code
5. Cross-reference configuration parameters (get_config_docs)
6. Link to output variables (get_variable_info)
7. Provide conceptual summary with code citations (line numbers)

**User experience**:
```
User: "Explain the snow scheme algorithm"
Skill: [retrieves snow.f95 (1558 lines), extracts 9 subroutines]
Output: Narrative explanation + code references + physics concepts
```

**Value**: Makes 24 Fortran modules (8 → 24 after expansion) accessible without Fortran expertise

### 4. Scenario Comparison Skill

**Purpose**: Compare multiple SUEWS configurations systematically

**YAML Metadata**:
```yaml
name: scenario-comparison
description: Run and compare multiple SUEWS scenarios
category: planning
tools: [validate_config, run_simulation, analyze_energy_balance, analyze_water_balance, export_results]
parallel: true
```

**Workflow**:
1. Accept multiple configuration files (e.g., baseline, greening, cool roofs)
2. Validate all scenarios (validate_config)
3. Run simulations in sequence (run_simulation × N)
4. Calculate comparative statistics - **requires analyze_energy_balance, analyze_water_balance**
5. Generate difference plots (ΔT, ΔQE, ΔRunoff)
6. Export results table (export_results)
7. Produce executive summary (winner analysis)

**User experience**:
```
User: "Compare baseline vs 30% green roofs vs street trees"
Skill: [runs 3 scenarios, compares results]
Output: Comparative analysis + recommendation
```

**Value**: What-if analysis without manual orchestration

### 5. Documentation Generator Skill

**Purpose**: Auto-generate configuration documentation for reproducibility

**YAML Metadata**:
```yaml
name: config-documentation
description: Generate human-readable configuration documentation
category: documentation
tools: [get_config_info, list_available_models, get_config_docs, get_variable_info]
output_format: markdown
```

**Workflow**:
1. Load user's configuration (get_config_info)
2. For each model used: get parameter docs (get_config_docs)
3. For each output variable: get metadata (get_variable_info)
4. Generate structured documentation (Markdown/HTML/PDF)
5. Include physics background for key parameters
6. Add validation rules and default values
7. Generate citations for physics schemes

**User experience**:
```
User: "Document this configuration for publication"
Skill: [generates comprehensive documentation]
Output: Paper-ready methods section + supplementary material
```

**Value**: Self-documenting configurations for research reproducibility

## Implementation Roadmap

### Phase 1: Foundation (Immediate - Current Sprint)

**Goal**: Implement critical analysis tools to unblock skill development

**Tasks**:
1. Implement `analyze_energy_balance()` tool
   - Energy balance closure calculation
   - Component breakdown (QN, QF, QH, QE, QS)
   - Temporal analysis (diurnal, seasonal)
   - Diagnostic output (imbalance > 10% warning)

2. Implement `calculate_statistics()` tool
   - Descriptive statistics (mean, std, min, max, percentiles)
   - Temporal aggregation (hourly → daily → monthly)
   - Spatial aggregation (multi-grid support)
   - Missing data handling

3. Implement `analyze_water_balance()` tool
   - Water balance closure (P = E + R + ΔS)
   - Component breakdown (runoff, evaporation, storage change)
   - Seasonal patterns
   - Drainage diagnostics

4. Implement `calculate_performance()` tool
   - Model-observation metrics (RMSE, MAE, R², bias)
   - Variable-specific performance
   - Time-period filtering
   - Performance targets (what's "good"?)

**Testing**: Validate each tool independently before proceeding

**Deliverable**: 4 new MCP tools (15 → 19 total), all with unit tests

### Phase 2: Core Skills (Next Sprint)

**Goal**: Build and test foundational skills

**Tasks**:
1. **Physics Investigation Skill**
   - Reason: Lowest complexity, no analysis tools required
   - Creates template for skill structure
   - Tests progressive disclosure with large Fortran files

2. **Documentation Generator Skill**
   - Reason: No simulation required, uses existing tools only
   - Provides immediate value for researchers
   - Tests skill composability (multiple tool calls)

3. **Urban Heat Island Analysis Skill**
   - Reason: Common use case, tests simulation workflow
   - Requires Phase 1 analysis tools
   - Tests iterative workflows

**Testing**: Each skill validated with real SUEWS configurations

**Deliverable**: 3 working skills with examples and documentation

### Phase 3: Advanced Skills (Future)

**Goal**: Complex, multi-step workflows

**Tasks**:
1. **Model Calibration Skill**
   - Iterative parameter optimisation
   - Performance-guided updates
   - Requires all Phase 1 tools

2. **Scenario Comparison Skill**
   - Parallel simulation handling
   - Statistical comparison
   - Visualisation generation

3. **Research Workflow Automation**
   - Sensitivity analysis
   - Uncertainty quantification
   - Publication-ready outputs

**Deliverable**: Complete skill library for SUEWS research workflows

## Skills + MCP Synergy Benefits

### 1. Progressive Disclosure at Scale

**Problem**: SUEWS has 24 Fortran files (330k+ tokens total)
**Solution**: Skills load modules only when needed via get_physics_implementation()

**Example**:
```
User: "Explain snow algorithm"
Skill loads: snow.f95 only (39k tokens as structured summary)
Not loaded: Other 23 modules (291k tokens)
```

### 2. Domain Expertise Packaging

**Problem**: SUEWS requires urban climate expertise
**Solution**: Skills encode expert workflows and best practices

**Example**:
```
Calibration skill knows:
- Energy balance must close within 10%
- Check OHM first for QS issues
- Then tune surface conductance for QE
- Validate with independent observations
```

### 3. Composable Workflows

**Problem**: Complex tasks require orchestrating many tools
**Solution**: Skills can invoke other skills

**Example**:
```
Scenario comparison skill invokes:
→ Physics investigation skill (for explaining differences)
→ Documentation generator skill (for methods section)
→ Analysis tools (for metrics)
```

### 4. Portable Expertise

**Problem**: Workflows differ across Claude platforms
**Solution**: Same skills work everywhere

**Example**:
```
UHI analysis skill works in:
- Claude.ai (web interface, interactive)
- Claude Code (development, reproducible)
- API (automated, batch processing)
```

## Technical Considerations

### Skill Structure Template

```
suews-skills/
├── urban-heat-island-analysis/
│   ├── skill.md              # Instructions + YAML frontmatter
│   ├── examples/
│   │   ├── london.yaml       # Example configuration
│   │   └── expected_output.md
│   └── scripts/
│       └── plot_uhi.py       # Optional helper script
├── model-calibration/
│   └── ...
└── physics-navigator/
    └── ...
```

### Skill Metadata Best Practices

```yaml
name: urban-heat-island-analysis
version: 1.0.0
description: Analyse urban heat island effects with SUEWS
author: SUEWS Development Team
category: research-workflow
requires:
  mcp_server: suews
  min_tools: [validate_config, run_simulation, analyze_energy_balance]
  python_packages: [matplotlib, numpy]
outputs:
  - type: plot
    format: png
  - type: statistics
    format: json
  - type: report
    format: markdown
educational: true  # Explain steps as you go
iterative: false   # Run once vs multiple iterations
parallel: false    # Sequential workflow
estimated_time: "2-5 minutes"
```

### Error Handling in Skills

**Principle**: Graceful degradation with informative fallbacks

**Example**:
```markdown
If analyze_energy_balance() fails:
1. Run simulation anyway
2. Load results with load_results()
3. Manually calculate closure (QN + QF) - (QH + QE + QS)
4. Warn user: "Energy balance analysis unavailable, using basic calculation"
5. Suggest: "Install updated SUEWS MCP for full analysis"
```

## Success Metrics

### Phase 1 Success Criteria
- [ ] All 4 analysis tools pass unit tests
- [ ] Tools work with real SUEWS output
- [ ] Performance benchmarks documented
- [ ] Error handling comprehensive

### Phase 2 Success Criteria
- [ ] 3 skills successfully deployed
- [ ] Each skill tested with ≥2 real configurations
- [ ] User documentation complete
- [ ] Example workflows published

### Phase 3 Success Criteria
- [ ] All 5 priority skills operational
- [ ] Skills successfully compose (calibration → documentation)
- [ ] User feedback collected and incorporated
- [ ] Published skill library

## Research Impact Opportunities

### For SUEWS Users
- **Learning curve reduction**: Skills guide new users through complex workflows
- **Best practices enforcement**: Skills encode expert knowledge
- **Reproducibility**: Documentation generator skill automates methods sections

### For Urban Climate Research
- **Accelerated UHI analysis**: 7-step workflow → 1 request
- **Faster calibration**: Expert-guided parameter tuning
- **Scenario exploration**: Easy what-if comparisons

### For Model Development
- **Code understanding**: Physics navigator makes Fortran accessible
- **Validation workflows**: Automated performance assessment
- **Documentation**: Self-generating configuration docs

## Next Steps

1. **Immediate**: Implement Phase 1 analysis tools (current blocker)
2. **Next sprint**: Build physics investigation skill (prototype)
3. **Documentation**: Create skill development guide
4. **Community**: Gather user feedback on priority workflows

---

## References

- Claude Skills Announcement: https://www.anthropic.com/news/skills
- MCP Documentation: https://modelcontextprotocol.io/
- SUEWS Documentation: https://suews.readthedocs.io/
- Agent Skills Engineering: https://www.anthropic.com/engineering/equipping-agents-for-the-real-world-with-agent-skills

---

**Status**: This document defines the strategy. Implementation pending Phase 1 completion.

**Last Updated**: 2025-10-22
**Next Review**: After Phase 1 analysis tools implemented
