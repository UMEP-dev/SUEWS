"""
Example usage of SuPy MCP tools.

This module demonstrates how to use the three core MCP tools for SUEWS simulations.
"""

import asyncio
import json
from pathlib import Path

from src.supy.mcp.server import SUPYMCPServer


async def example_configure_simulation():
    """Example: Configure a SUEWS simulation."""
    server = SUPYMCPServer()
    
    # Example 1: Load configuration from YAML file
    result = await server.call_tool("configure_simulation", {
        "config_path": "path/to/config.yaml",
        "site_name": "London",
        "validate_only": False
    })
    print("Configuration loaded:", result)
    
    # Example 2: Create default configuration with updates
    result = await server.call_tool("configure_simulation", {
        "config_updates": {
            "site": {
                "name": "London",
                "latitude": 51.5,
                "longitude": -0.1
            },
            "surface": {
                "fractions": {
                    "building": 0.4,
                    "paved": 0.2,
                    "vegetation": 0.3,
                    "water": 0.1
                }
            }
        },
        "save_path": "output/london_config.yaml"
    })
    print("Configuration created and saved:", result)
    
    return result


async def example_run_simulation():
    """Example: Run a SUEWS simulation."""
    server = SUPYMCPServer()
    
    # Example 1: Run with sample data
    result = await server.call_tool("run_simulation", {
        "use_sample_data": True,
        "save_state": True,
        "time_step": 3600
    })
    print("Simulation with sample data:", result)
    
    # Example 2: Run with custom forcing data and time filtering
    result = await server.call_tool("run_simulation", {
        "forcing_path": "data/forcing_2024.txt",
        "config_path": "config/london.yaml",
        "start_time": "2024-06-01T00:00:00",
        "end_time": "2024-08-31T23:00:00",
        "save_state": True
    })
    print("Simulation with custom data:", result)
    
    return result


async def example_analyze_results():
    """Example: Analyze simulation results."""
    server = SUPYMCPServer()
    
    # Example 1: Summary analysis
    result = await server.call_tool("analyze_results", {
        "results_path": "output/simulation_results.csv",
        "analysis_type": "summary"
    })
    print("Summary analysis:", result)
    
    # Example 2: Energy balance analysis with specific variables
    result = await server.call_tool("analyze_results", {
        "results_path": "output/simulation_results.csv",
        "analysis_type": "energy_balance",
        "variables": ["QH", "QE", "QS", "QN", "QF"],
        "time_period": "monthly"
    })
    print("Energy balance analysis:", result)
    
    # Example 3: Comparison with observations
    result = await server.call_tool("analyze_results", {
        "results_path": "output/simulation_results.csv",
        "comparison_path": "data/observations.csv",
        "analysis_type": "statistics",
        "start_time": "2024-06-01T00:00:00",
        "end_time": "2024-08-31T23:00:00"
    })
    print("Comparison analysis:", result)
    
    return result


async def example_complete_workflow():
    """Example: Complete workflow from configuration to analysis."""
    server = SUPYMCPServer()
    
    print("=" * 60)
    print("SUEWS MCP Tools - Complete Workflow Example")
    print("=" * 60)
    
    # Step 1: Configure simulation
    print("\n1. Configuring simulation...")
    config_result = await server.call_tool("configure_simulation", {
        "config_updates": {
            "site": {
                "name": "UrbanTestSite",
                "latitude": 51.5,
                "longitude": -0.1
            },
            "model": {
                "name": "SUEWS",
                "version": "2024.1"
            },
            "surface": {
                "fractions": {
                    "building": 0.35,
                    "paved": 0.25,
                    "vegetation": 0.35,
                    "water": 0.05
                }
            }
        },
        "save_path": "temp/workflow_config.yaml",
        "validate_only": False
    })
    
    if config_result["success"]:
        print("✓ Configuration created successfully")
        print(f"  Site: {config_result['data'].get('site_name', 'N/A')}")
        print(f"  Validation: {config_result['data'].get('valid', False)}")
    else:
        print("✗ Configuration failed:", config_result.get("errors"))
        return
    
    # Step 2: Run simulation
    print("\n2. Running simulation...")
    run_result = await server.call_tool("run_simulation", {
        "use_sample_data": True,  # Using sample data for demo
        "save_state": True,
        "time_step": 3600
    })
    
    if run_result["success"]:
        print("✓ Simulation completed successfully")
        data = run_result.get("data", {})
        print(f"  Data source: {data.get('data_source', 'N/A')}")
        print(f"  Time steps: {data.get('output', {}).get('shape', [0])[0]}")
        if "statistics" in data:
            print("  Key statistics calculated")
    else:
        print("✗ Simulation failed:", run_result.get("errors"))
        return
    
    # Step 3: Analyze results
    print("\n3. Analyzing results...")
    
    # For demo, we would normally save the results first and then analyze
    # Here we'll show the analysis structure
    
    analysis_types = ["summary", "energy_balance", "temporal"]
    
    for analysis_type in analysis_types:
        print(f"\n  Performing {analysis_type} analysis...")
        
        # In a real scenario, you would pass the actual results file
        # This is a demonstration of the API
        analysis_params = {
            "results_path": "temp/simulation_output.csv",  # Would be actual output
            "analysis_type": analysis_type,
            "output_format": "json"
        }
        
        if analysis_type == "energy_balance":
            analysis_params["variables"] = ["QH", "QE", "QS", "QN", "QF"]
        elif analysis_type == "temporal":
            analysis_params["time_period"] = "daily"
        
        # Note: This would fail without actual results file
        # Showing the structure for documentation
        print(f"    Parameters: {json.dumps(analysis_params, indent=6)}")
    
    print("\n" + "=" * 60)
    print("Workflow complete!")
    print("=" * 60)


async def example_error_handling():
    """Example: Error handling in MCP tools."""
    server = SUPYMCPServer()
    
    print("\nDemonstrating error handling:")
    print("-" * 40)
    
    # Example 1: Invalid file path
    result = await server.call_tool("configure_simulation", {
        "config_path": "/non/existent/file.yaml"
    })
    if not result["success"]:
        print("Expected error - file not found:")
        print(f"  {result.get('errors', ['No error message'])[0]}")
    
    # Example 2: Invalid time range
    result = await server.call_tool("run_simulation", {
        "forcing_path": "data/forcing.txt",
        "start_time": "2024-12-31T23:00:00",
        "end_time": "2024-01-01T00:00:00"  # End before start
    })
    if not result["success"]:
        print("\nExpected error - invalid time range:")
        print(f"  {result.get('errors', ['No error message'])[0]}")
    
    # Example 3: Invalid analysis type
    result = await server.call_tool("analyze_results", {
        "results_path": "output/results.csv",
        "analysis_type": "invalid_type"
    })
    if not result["success"]:
        print("\nExpected error - invalid analysis type:")
        print(f"  {result.get('errors', ['No error message'])[0]}")


def main():
    """Run all examples."""
    # Run async examples
    loop = asyncio.get_event_loop()
    
    # Run complete workflow
    loop.run_until_complete(example_complete_workflow())
    
    # Run error handling examples
    loop.run_until_complete(example_error_handling())
    
    print("\nExamples completed!")


if __name__ == "__main__":
    main()