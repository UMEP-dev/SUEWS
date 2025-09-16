# SUEWS MCP Server Quick Start Guide

Welcome to the SUEWS MCP Server! This guide will walk you through setting up and running your first urban climate simulation using the Model Context Protocol interface.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Installation](#installation)
3. [Starting the Server](#starting-the-server)
4. [Your First Simulation](#your-first-simulation)
5. [Working with Templates](#working-with-templates)
6. [Data Processing Workflow](#data-processing-workflow)
7. [Advanced Examples](#advanced-examples)
8. [Next Steps](#next-steps)

## Prerequisites

Before starting, ensure you have:

- **Python 3.9+** installed
- **SUEWS/SuPy** installed and working
- Basic understanding of urban climate modelling concepts
- Meteorological forcing data (or use our sample data)

### Verify SuPy Installation

```bash
python -c "import supy; print(f'✓ SuPy version: {supy.__version__}')"
```

If this fails, install SuPy first:
```bash
pip install supy
```

## Installation

### 1. Clone and Install

```bash
# Clone the repository
git clone https://github.com/UMEP-dev/SUEWS.git
cd SUEWS/suews-mcp

# Create virtual environment (recommended)
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install the SUEWS MCP server
pip install -e .
```

### 2. Verify Installation

```bash
# Check installation
python -c "import suews_mcp; print('✓ SUEWS MCP Server installed successfully')"

# Check available tools
python -c "from suews_mcp.handlers import SUEWSMCPHandlers; print('✓ Handlers available')"
```

## Starting the Server

### Basic Server Start

```bash
# Start with default settings
suews-mcp

# Or with Python module
python -m suews_mcp.server
```

You should see output like:
```
INFO - Starting SUEWS MCP Server v1.0.0
INFO - Server listening on localhost:8000
INFO - Available tools: 10
INFO - Ready for connections
```

### Custom Configuration

Create a config file `config.yaml`:

```yaml
server:
  host: "localhost"
  port: 8001
  name: "My SUEWS Server"

simulation:
  max_concurrent: 2
  timeout: 1800

tools:
  enable_simulation: true
  enable_validation: true
  enable_analysis: true

logging:
  level: "DEBUG"
  file: "my_suews_server.log"
```

Then start with:
```bash
suews-mcp --config config.yaml
```

## Your First Simulation

Let's run a complete simulation workflow using the MCP tools.

### Step 1: List Available Tools

```python
import asyncio
from mcp import create_client

async def list_tools():
    async with create_client("suews-mcp") as client:
        tools = await client.list_tools()
        print("Available MCP Tools:")
        for tool in tools.tools:
            print(f"  - {tool.name}: {tool.description}")

asyncio.run(list_tools())
```

### Step 2: Get a Configuration Template

```python
async def get_template():
    async with create_client("suews-mcp") as client:
        # List available templates
        resources = await client.call_tool("list_resources", {
            "resource_type": "config_template"
        })
        print(resources.content[0].text)
        
        # Get a specific template
        template = await client.call_tool("get_resource", {
            "resource_path": "templates/configs/residential.yml"
        })
        print("\nResidential Template:")
        print(template.content[0].text[:500] + "...")

asyncio.run(get_template())
```

### Step 3: Configure Your Simulation

```python
async def configure_simulation():
    async with create_client("suews-mcp") as client:
        # Configure simulation for London residential area
        config = await client.call_tool("configure_simulation", {
            "config_path": "templates/configs/residential.yml",
            "site_name": "London_Residential_Demo",
            "config_updates": {
                "site": {
                    "lat": 51.5074,      # London latitude
                    "lon": -0.1278,      # London longitude  
                    "elevation": 11,     # London elevation (m)
                    "timezone": 0        # UTC offset
                },
                "surface": {
                    "frac_paved": 0.35,  # 35% paved surfaces
                    "frac_bldgs": 0.25,  # 25% buildings
                    "frac_grass": 0.30,  # 30% grass
                    "frac_trees": 0.10   # 10% trees
                }
            },
            "save_path": "my_london_config.yml"
        })
        print("Configuration Result:")
        print(config.content[0].text)

asyncio.run(configure_simulation())
```

### Step 4: Validate Configuration

```python
async def validate_config():
    async with create_client("suews-mcp") as client:
        validation = await client.call_tool("validate_config", {
            "config_file": "my_london_config.yml",
            "strict_mode": False,
            "check_file_paths": True
        })
        print("Validation Results:")
        print(validation.content[0].text)

asyncio.run(validate_config())
```

### Step 5: Run the Simulation

```python
async def run_simulation():
    async with create_client("suews-mcp") as client:
        # Run simulation with sample data
        simulation = await client.call_tool("run_simulation", {
            "config_path": "my_london_config.yml",
            "use_sample_data": True,  # Use built-in sample data
            "start_time": "2012-01-01T00:00:00",
            "end_time": "2012-01-07T23:00:00",  # One week simulation
            "save_state": True
        })
        print("Simulation Results:")
        print(simulation.content[0].text)

asyncio.run(run_simulation())
```

### Step 6: Analyse Results

```python
async def analyse_results():
    async with create_client("suews-mcp") as client:
        analysis = await client.call_tool("analyze_results", {
            "results_path": "outputs/London_Residential_Demo_SUEWS.csv",
            "analysis_type": "energy_balance",
            "variables": ["QH", "QE", "QN", "QS", "T2"],
            "time_period": "daily"
        })
        print("Analysis Results:")
        print(analysis.content[0].text)

asyncio.run(analyse_results())
```

## Working with Templates

The server provides several built-in templates for different urban environments:

### Available Templates

1. **Residential**: Suburban residential areas with mixed buildings and vegetation
2. **Commercial**: Dense urban commercial/business districts
3. **Industrial**: Industrial areas with large buildings and paved surfaces
4. **Park**: Urban parks and green spaces

### Customising Templates

```python
async def customize_template():
    async with create_client("suews-mcp") as client:
        # Start with commercial template
        config = await client.call_tool("configure_simulation", {
            "config_path": "templates/configs/commercial.yml",
            "site_name": "MyCity_Downtown",
            "config_updates": {
                "site": {
                    "lat": 40.7128,    # New York City
                    "lon": -74.0060,
                    "elevation": 10
                },
                "surface": {
                    "frac_bldgs": 0.45,  # Higher building density
                    "height_bldgs": 25.0 # Taller buildings
                },
                "anthropogenic": {
                    "qf0_beu": 45.0     # Higher energy use
                },
                "model": {
                    "control": {
                        "tstep": 300    # 5-minute timesteps
                    }
                }
            },
            "save_path": "nyc_downtown.yml"
        })
        print(config.content[0].text)

asyncio.run(customize_template())
```

## Data Processing Workflow

### Preprocessing Your Own Meteorological Data

If you have your own weather data, process it first:

```python
async def preprocess_data():
    async with create_client("suews-mcp") as client:
        # Preprocess CSV weather data
        preprocessing = await client.call_tool("preprocess_forcing", {
            "input_file": "my_weather_data.csv",
            "output_file": "processed_forcing.txt",
            "validate_energy_balance": True,
            "auto_fix_issues": True,
            "target_timestep": 3600  # Hourly data
        })
        print("Preprocessing Results:")
        print(preprocessing.content[0].text)

asyncio.run(preprocess_data())
```

### Converting Data Formats

```python
async def convert_format():
    async with create_client("suews-mcp") as client:
        conversion = await client.call_tool("convert_data_format", {
            "input_file": "weather_data.xlsx",
            "output_file": "suews_forcing.txt",
            "input_format": "excel",
            "output_format": "suews_txt",
            "column_mapping": {
                "Temperature": "Tair",
                "Relative_Humidity": "RH", 
                "Wind_Speed": "U",
                "Net_Radiation": "Kdown"
            }
        })
        print("Conversion Results:")
        print(conversion.content[0].text)

asyncio.run(convert_format())
```

## Advanced Examples

### Parameter Sensitivity Analysis

```python
async def parameter_sensitivity():
    async with create_client("suews-mcp") as client:
        results = {}
        
        # Test different albedo values
        for albedo in [0.1, 0.15, 0.2, 0.25, 0.3]:
            print(f"Testing albedo = {albedo}")
            
            # Configure simulation
            config_updates = {
                "surface": {
                    "albedo_paved": albedo,
                    "albedo_bldgs": albedo * 1.2  # Buildings slightly higher
                }
            }
            
            # Run simulation
            sim_result = await client.call_tool("run_simulation", {
                "config_path": "templates/configs/residential.yml",
                "config_updates": config_updates,
                "use_sample_data": True,
                "start_time": "2012-07-01T00:00:00",
                "end_time": "2012-07-07T23:00:00"  # Summer week
            })
            
            # Store results
            results[f"albedo_{albedo}"] = sim_result.content[0].text
            
        print(f"Completed sensitivity analysis for {len(results)} scenarios")

asyncio.run(parameter_sensitivity())
```

### Multi-Site Comparison

```python
async def multi_site_comparison():
    async with create_client("suews-mcp") as client:
        sites = {
            "residential": {
                "template": "templates/configs/residential.yml",
                "name": "Suburban_Residential"
            },
            "commercial": {
                "template": "templates/configs/commercial.yml", 
                "name": "Downtown_Commercial"
            },
            "park": {
                "template": "templates/configs/park.yml",
                "name": "Central_Park"
            }
        }
        
        site_results = {}
        
        for site_type, site_info in sites.items():
            print(f"Running simulation for {site_type}...")
            
            # Run simulation for each site type
            result = await client.call_tool("run_simulation", {
                "config_path": site_info["template"],
                "use_sample_data": True,
                "start_time": "2012-06-21T00:00:00",  # Summer solstice
                "end_time": "2012-06-27T23:00:00"
            })
            
            # Analyse results
            analysis = await client.call_tool("analyze_results", {
                "results_path": f"outputs/{site_info['name']}_SUEWS.csv",
                "analysis_type": "summary",
                "variables": ["QH", "QE", "T2"]
            })
            
            site_results[site_type] = {
                "simulation": result.content[0].text,
                "analysis": analysis.content[0].text
            }
        
        print(f"Completed comparison for {len(site_results)} site types")

asyncio.run(multi_site_comparison())
```

### Health Monitoring

```python
async def monitor_server():
    async with create_client("suews-mcp") as client:
        health = await client.call_tool("health_check", {})
        print("Server Health Status:")
        print(health.content[0].text)

asyncio.run(monitor_server())
```

## Troubleshooting Common Issues

### Server Connection Issues

```bash
# Check if server is running
ps aux | grep suews-mcp

# Check port availability
lsof -i :8000

# Try different port
suews-mcp --port 8001
```

### Configuration Validation Errors

1. **Surface fractions don't sum to 1.0**:
   ```python
   # Fix in config_updates
   config_updates = {
       "surface": {
           "frac_paved": 0.35,
           "frac_bldgs": 0.25, 
           "frac_grass": 0.25,
           "frac_trees": 0.15
           # Total = 1.00
       }
   }
   ```

2. **Invalid date ranges**:
   ```python
   # Use ISO format dates
   "start_time": "2012-01-01T00:00:00",
   "end_time": "2012-12-31T23:00:00"
   ```

3. **Missing forcing data**:
   ```python
   # Use sample data for testing
   "use_sample_data": True
   ```

### Simulation Failures

1. **Check logs**:
   ```bash
   tail -f suews_mcp.log
   ```

2. **Validate configuration first**:
   ```python
   await client.call_tool("validate_config", {
       "config_file": "my_config.yml",
       "strict_mode": True
   })
   ```

3. **Start with short simulations**:
   ```python
   # Test with one day first
   "start_time": "2012-07-01T00:00:00",
   "end_time": "2012-07-01T23:00:00"
   ```

## Next Steps

Congratulations! You've successfully run your first SUEWS simulations using the MCP server. Here's what to explore next:

### 1. Learn More About SUEWS
- Read the [SUEWS documentation](https://suews.readthedocs.io/)
- Understand energy balance components (QH, QE, QN, QS)
- Learn about urban surface parameterisation

### 2. Explore More Examples
- Check `docs/examples/` for specific use cases
- Try the Jupyter notebooks in `examples/notebooks/`
- Review the complete API reference

### 3. Use Real Data
- Process your own meteorological data
- Calibrate model parameters for your study area
- Compare results with observations

### 4. Advanced Workflows
- Set up automated parameter optimisation
- Create custom analysis pipelines
- Integrate with other urban modelling tools

### 5. Get Help
- Join the [SUEWS community discussions](https://github.com/UMEP-dev/SUEWS/discussions)
- Report issues on [GitHub](https://github.com/UMEP-dev/SUEWS/issues)
- Check the FAQ for common questions

## Additional Resources

- **[API Reference](api_reference.md)**: Complete tool documentation
- **[Examples](examples/)**: Practical use case examples  
- **[FAQ](faq.md)**: Frequently asked questions
- **[SUEWS Manual](https://suews.readthedocs.io/)**: Complete model documentation
- **[SuPy Documentation](https://supy.readthedocs.io/)**: Python interface docs

Happy modelling! 🏙️🌡️