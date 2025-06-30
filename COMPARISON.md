# MCP Server Approaches: Complex vs Simple

## Complex Approach (mcp-server/)
**10 tools, 150+ KB, comprehensive features**

### Pros:
- Complete parameter documentation
- Physics compatibility checking  
- Energy balance analysis
- Thermal comfort interpretation
- Observation validation
- Comprehensive reports

### Cons:
- Requires SuPy installation
- Complex dependency management
- Harder to understand/modify
- Overkill for basic validation

### Tools:
```
validate_suews_config       # Full validation with SuPy models
suggest_suews_parameters    # AI-powered suggestions
check_suews_physics_compatibility
generate_suews_template
explain_suews_parameter
diagnose_suews_energy_balance
interpret_suews_thermal_comfort
analyze_suews_urban_effects
validate_suews_against_observations
generate_suews_insights_report
```

## Simple Approach (mcp-server-simple/)
**3 tools, ~10 KB, focused on settings**

### Pros:
- No SuPy dependency
- Clear, minimal code (~200 lines)
- Easy to understand/extend
- Fast to deploy
- Focused purpose

### Cons:
- Limited validation
- No analysis features
- Basic parameter set only

### Tools:
```
create_settings    # Validate and save
get_settings      # Retrieve by ID
list_settings     # Show all saved
```

## Key Differences

| Aspect | Complex | Simple |
|--------|---------|---------|
| Dependencies | SuPy, pandas, numpy, xarray | Just pydantic, mcp |
| Code size | ~2000 lines | ~200 lines |
| Validation | Full SuPy models | Basic Pydantic |
| Storage | Not implemented | JSON files |
| Focus | Full analysis | Just settings |

## When to Use Which?

**Use Simple when:**
- Just need to validate settings
- Want minimal dependencies
- Building a larger workflow
- Learning MCP basics

**Use Complex when:**
- Need comprehensive validation
- Want result analysis
- Need parameter documentation
- Building complete SUEWS assistant

## Migration Path

Start with Simple, then:
1. Add more parameters to `SuPySettings`
2. Add `run_simulation` tool
3. Add result parsing tools
4. Gradually add analysis features

The Simple approach is a focused building block, while Complex is a complete solution.