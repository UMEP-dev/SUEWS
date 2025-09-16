# SUEWS MCP Resource Management

This document describes the resource management system for the SUEWS MCP server, providing templates, examples, and guidance for urban climate modelling workflows.

## Overview

The resource management system provides:
- **Configuration templates** for common urban area types
- **Workflow guides** for step-by-step SUEWS processes
- **Working examples** for specific use cases
- **Prompt templates** for AI-assisted guidance
- **Sample data** and documentation links

## Architecture

### Core Components

1. **ResourceManager** (`src/suews_mcp/resources.py`)
   - Manages discovery and loading of resources
   - Provides search and filtering capabilities
   - Validates resource availability

2. **Resource Catalog** (`templates/resource_catalog.json`)
   - Central registry of all available resources
   - Metadata including descriptions, tags, and usage patterns
   - Search indices for efficient discovery

3. **Templates Directory** (`templates/`)
   - Organised directory structure for different resource types
   - Consistent naming conventions
   - Version-controlled content

### Directory Structure

```
templates/
├── README.md                    # Overview and usage guide
├── resource_catalog.json       # Central resource registry
├── configs/                     # YAML configuration templates
│   ├── residential.yml          # Mixed residential areas
│   ├── commercial.yml           # Dense downtown/commercial
│   ├── industrial.yml           # Industrial zones
│   └── park.yml                 # Urban parks/green spaces
├── workflows/                   # Step-by-step workflow guides
│   ├── quick_start.md           # First SUEWS simulation
│   ├── sensitivity_analysis.md # Parameter sensitivity testing
│   └── validation_workflow.md  # Model validation process
├── examples/                    # Complete working examples
│   ├── basic_simulation/        # Minimal working example
│   ├── urban_park_study/        # Green infrastructure analysis
│   ├── building_energy/         # Urban energy studies
│   └── multi_site_analysis/     # Comparative studies
├── prompts/                     # AI-assisted guidance templates
│   ├── model_setup/             # Configuration assistance
│   ├── data_preparation/        # Data processing guidance
│   └── analysis/                # Results interpretation
└── data/                        # Sample data and sources
    ├── sample_forcing.txt       # Example meteorological data
    ├── sample_initial.txt       # Example initial conditions
    └── data_sources.md          # External data source links
```

## Usage

### Basic Resource Loading

```python
from suews_mcp.resources import ResourceManager

# Initialize resource manager
manager = ResourceManager()

# List all available resources
all_resources = manager.list_resources()

# Get specific resource content
residential_config = manager.get_resource("configs/residential.yml")
quick_start_guide = manager.get_resource("workflows/quick_start.md")
```

### Resource Discovery

```python
# Search by tags
park_resources = manager.find_resources(tags=["park", "vegetation"])

# Filter by difficulty level
beginner_resources = manager.find_resources(difficulty="beginner")

# Find resources for specific domain
green_infra_resources = manager.find_resources(domain="green_infrastructure")

# Get usage patterns
beginner_workflow = manager.get_usage_pattern("beginner_workflow")
```

### MCP Server Integration

The resource manager integrates with MCP server tools:

```python
# In MCP server handler
async def handle_get_resource(self, arguments):
    resource_path = arguments.get("resource_path")
    content = self.resource_manager.get_resource(resource_path)
    
    if content:
        return {"success": True, "content": content}
    else:
        return {"success": False, "error": "Resource not found"}

async def handle_list_resources(self, arguments):
    resource_type = arguments.get("resource_type")
    resources = self.resource_manager.list_resources(resource_type)
    
    return {"success": True, "resources": resources}
```

## Resource Types

### 1. Configuration Templates

Pre-configured YAML files for common urban area types:

- **Residential** (`configs/residential.yml`): Mixed residential areas with moderate building density
- **Commercial** (`configs/commercial.yml`): Dense downtown areas with high building coverage  
- **Industrial** (`configs/industrial.yml`): Industrial zones with large paved areas
- **Park** (`configs/park.yml`): Urban green spaces with high vegetation coverage

Each template includes:
- Complete SUEWS model configuration
- Realistic surface fraction values
- Appropriate physics parameter settings
- Initial conditions for temperate climates

### 2. Workflow Guides

Step-by-step markdown guides for common SUEWS workflows:

- **Quick Start** (`workflows/quick_start.md`): First SUEWS simulation from setup to results
- **Sensitivity Analysis** (`workflows/sensitivity_analysis.md`): Testing parameter impacts
- **Validation Workflow** (`workflows/validation_workflow.md`): Comparing with observations

Each workflow includes:
- Clear learning objectives
- Prerequisites and difficulty level
- Step-by-step instructions
- Troubleshooting guidance

### 3. Working Examples

Complete, executable examples for specific use cases:

- **Basic Simulation** (`examples/basic_simulation/`): Minimal working example
- **Urban Park Study** (`examples/urban_park_study/`): Green infrastructure benefits
- **Building Energy** (`examples/building_energy/`): Urban energy integration
- **Multi-site Analysis** (`examples/multi_site_analysis/`): Comparative studies

Each example includes:
- README with overview and instructions
- Python scripts with complete implementations
- Expected results and interpretation guidance

### 4. Prompt Templates

AI-assisted guidance templates for interactive help:

- **Site Configuration** (`prompts/model_setup/configure_site.md`): Interactive site setup
- **Data Preparation** (`prompts/data_preparation/meteorological_data.md`): Data processing help
- **Results Interpretation** (`prompts/analysis/energy_balance_interpretation.md`): Analysis guidance

Templates provide:
- Structured question sequences
- Validation and error handling
- Contextual help and recommendations

### 5. Sample Data

Example datasets and data source information:

- **Sample Forcing Data**: Example meteorological time series
- **Sample Initial Conditions**: Typical urban site parameters  
- **Data Sources Guide**: Links to real-world data sources

## Usage Patterns

Pre-defined resource sequences for common workflows:

### Beginner Workflow
1. `configs/residential.yml` - Start with residential template
2. `data/sample_forcing.txt` - Use sample meteorological data  
3. `workflows/quick_start.md` - Follow step-by-step guide
4. `examples/basic_simulation/` - Run complete example
5. `prompts/analysis/energy_balance_interpretation.md` - Interpret results

### Green Infrastructure Study
1. `configs/park.yml` - Park configuration
2. `configs/residential.yml` - Comparison baseline
3. `examples/urban_park_study/` - Complete analysis example
4. `prompts/analysis/energy_balance_interpretation.md` - Result interpretation

### Building Energy Study  
1. `configs/commercial.yml` - Dense urban configuration
2. `examples/building_energy/` - Energy integration example
3. `workflows/sensitivity_analysis.md` - Parameter testing

## Search and Discovery

The system provides multiple ways to find relevant resources:

### By Tags
Resources are tagged with relevant keywords:
- Surface types: `park`, `residential`, `commercial`, `industrial`
- Applications: `green_infrastructure`, `building_energy`, `validation`
- Difficulty: `beginner`, `intermediate`, `advanced`

### By Domain
Resources are categorised by research domain:
- `urban_planning`: Planning and policy applications
- `building_energy`: Building and energy studies
- `green_infrastructure`: Green space and ecosystem services
- `climate_adaptation`: Climate change and adaptation

### By Difficulty
Resources are ranked by complexity:
- **Beginner**: Basic tutorials and simple examples
- **Intermediate**: Parameter studies and comparative analysis
- **Advanced**: Validation, calibration, and complex integrations

## Testing and Validation

The resource management system includes comprehensive testing:

```bash
# Run resource management tests
python3 test_resources_fixed.py
```

Tests verify:
- Resource catalog loading
- Resource content accessibility
- Search and filtering functionality
- File system validation
- Integration patterns
- Documentation links

## Integration with MCP Server

The resource management integrates seamlessly with the MCP server:

### Available MCP Tools

1. **get_resource**: Load specific resource content
2. **list_resources**: Browse available resources by type
3. **find_resources**: Search resources by criteria
4. **get_usage_pattern**: Get recommended workflow sequences

### Example MCP Requests

```json
{
  "method": "tools/call",
  "params": {
    "name": "get_resource",
    "arguments": {
      "resource_path": "configs/residential.yml"
    }
  }
}
```

```json
{
  "method": "tools/call", 
  "params": {
    "name": "find_resources",
    "arguments": {
      "tags": ["park"],
      "difficulty": "beginner"
    }
  }
}
```

## Extending the System

### Adding New Resources

1. **Create the resource file** in appropriate directory
2. **Update resource_catalog.json** with metadata
3. **Add to search indices** with relevant tags
4. **Test accessibility** with test suite
5. **Document integration** in usage patterns

### Template Guidelines

When creating new resources:
- Follow existing naming conventions
- Include comprehensive metadata
- Provide clear descriptions and examples
- Link to relevant SuPy documentation
- Test with actual workflows

### Metadata Standards

Each resource should include:
- Unique ID and descriptive name
- Clear description and use cases
- Appropriate tags and categories
- Difficulty level and prerequisites
- Expected outputs and learning outcomes

## Documentation Links

The system maintains links to external documentation:
- [SuPy Documentation](https://supy.readthedocs.io/)
- [SUEWS Manual](https://suews.readthedocs.io/)
- [Quick Start Tutorial](https://suews.readthedocs.io/en/latest/tutorials/python/quick-start.html)
- [Configuration Guide](https://suews.readthedocs.io/en/latest/inputs/yaml/index.html)
- [Parameter Reference](https://suews.readthedocs.io/en/latest/inputs/tables/index.html)
- [Troubleshooting Guide](https://suews.readthedocs.io/en/latest/troubleshooting.html)

## Performance Considerations

The resource management system is designed for efficiency:
- **Lazy loading**: Catalog loaded once, cached in memory
- **File-based storage**: Simple filesystem operations
- **Indexed search**: Pre-computed search indices
- **Minimal dependencies**: Pure Python implementation

## Security Considerations

Resource access includes basic security measures:
- **Path validation**: Prevents directory traversal
- **Content filtering**: Text-based resources only  
- **Error handling**: Graceful failure modes
- **Input sanitisation**: Safe resource path handling

## Future Enhancements

Planned improvements include:
- **Version control**: Resource versioning and updates
- **User contributions**: Community-contributed templates
- **Dynamic generation**: AI-generated resource content
- **Performance metrics**: Usage analytics and optimisation
- **Multi-language support**: Internationalisation capabilities

## Conclusion

The SUEWS MCP resource management system provides a comprehensive, well-organised collection of templates, examples, and guidance for urban climate modelling workflows. It enables both beginners and advanced users to effectively leverage SUEWS capabilities through structured, discoverable resources integrated seamlessly with the MCP server architecture.