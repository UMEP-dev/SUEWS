# SUEWS MCP Templates

This directory contains templates and examples for the SUEWS MCP server to help users get started with urban climate modelling workflows.

## Directory Structure

### `/configs/` - Configuration Templates
Pre-configured YAML files for common urban area types:
- `residential.yml` - Mixed residential areas (25% paved, 35% buildings, 30% grass)
- `commercial.yml` - Dense downtown areas (40% paved, 55% buildings, minimal vegetation)
- `industrial.yml` - Industrial zones (50% paved, 35% buildings, minimal vegetation)
- `park.yml` - Urban parks/green spaces (45% grass, 35% trees, minimal built area)

### `/workflows/` - Workflow Guides
Step-by-step guides for common SUEWS workflows:
- `quick_start.md` - Your first SUEWS simulation
- `sensitivity_analysis.md` - Parameter sensitivity testing
- `validation_workflow.md` - Model validation with observations

### `/examples/` - Working Examples
Complete working examples for different use cases:
- `basic_simulation/` - Minimal working simulation
- `urban_park_study/` - Green infrastructure analysis
- `building_energy/` - Urban energy balance study
- `multi_site_analysis/` - Comparative studies across locations

### `/prompts/` - Guided Prompt Templates
Templates for AI-assisted workflow guidance:
- `model_setup/` - Configuration assistance prompts
- `data_preparation/` - Data processing guidance
- `analysis/` - Results interpretation prompts

### `/data/` - Sample Data and Sources
Sample datasets and data source information:
- `sample_forcing.txt` - Example meteorological data
- `sample_initial.txt` - Example initial conditions
- `data_sources.md` - Links to real-world data sources

## Usage with MCP Server

These templates are accessed through the MCP server tools:

```python
# List available templates
templates = await client.call_tool("list_resources", {
    "resource_type": "template"
})

# Get a specific template
template = await client.call_tool("get_resource", {
    "resource_path": "templates/configs/residential.yml"
})

# Get workflow guidance
workflow = await client.call_tool("get_resource", {
    "resource_path": "templates/workflows/quick_start.md"
})
```

## Contributing

When adding new templates:
1. Follow the existing naming conventions
2. Include comprehensive documentation
3. Test templates with actual workflows
4. Link to relevant SuPy documentation
5. Keep examples minimal and focused

## Links to Documentation

- [SuPy Documentation](https://supy.readthedocs.io/)
- [SUEWS Manual](https://suews.readthedocs.io/)
- [SuPy Quick Start Tutorial](https://suews.readthedocs.io/en/latest/tutorials/python/quick-start.html)
- [Configuration Guide](https://suews.readthedocs.io/en/latest/inputs/yaml/index.html)