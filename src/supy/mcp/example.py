#!/usr/bin/env python3
"""
Example usage of SuPy MCP server.

This script demonstrates how to use the MCP server both programmatically
and as a standalone server for AI assistant integration.
"""

import asyncio
import json
import sys
from pathlib import Path

from . import SUPYMCPServer


async def example_programmatic_usage():
    """Example of using MCP server programmatically."""
    
    print("=== SuPy MCP Server - Programmatic Usage Example ===")
    
    # Create server instance
    server = SUPYMCPServer()
    
    # List available tools
    print("\n1. Available Tools:")
    tools = server.list_tools()
    for tool in tools:
        print(f"   - {tool['name']}: {tool['description']}")
    
    # Example 1: Configure simulation (validation only)
    print("\n2. Configure Simulation (validation only):")
    try:
        result = await server.call_tool("configure_simulation", {
            "validate_only": True
        })
        
        print(f"   Success: {result['success']}")
        if result['success']:
            print(f"   Message: {result.get('message', 'No message')}")
        else:
            print(f"   Errors: {result.get('errors', ['Unknown error'])}")
            
    except Exception as e:
        print(f"   Error: {e}")
    
    # Example 2: Run simulation with sample data
    print("\n3. Run Simulation with Sample Data:")
    try:
        result = await server.call_tool("run_simulation", {
            "use_sample_data": True,
            "save_state": False
        })
        
        print(f"   Success: {result['success']}")
        if result['success'] and 'data' in result:
            data = result['data']
            if 'simulation_completed' in data:
                print(f"   Simulation completed: {data['simulation_completed']}")
            if 'output' in data:
                output_info = data['output']
                print(f"   Output shape: {output_info['shape']}")
                print(f"   Variables: {output_info['columns'][:5]}{'...' if len(output_info['columns']) > 5 else ''}")
        else:
            print(f"   Errors: {result.get('errors', ['Unknown error'])}")
            
    except Exception as e:
        print(f"   Error: {e}")
    
    # Example 3: Analyze results (if we have sample data available)
    print("\n4. Analyze Sample Results:")
    
    # Create a sample results file for demonstration
    try:
        import pandas as pd
        import tempfile
        
        # Create sample data
        sample_data = pd.DataFrame({
            'QH': [100.0, 110.0, 120.0, 115.0, 105.0],
            'QE': [50.0, 55.0, 60.0, 58.0, 52.0],
            'T2': [20.0, 21.0, 22.0, 21.5, 20.5]
        }, index=pd.date_range('2012-01-01', periods=5, freq='H'))
        
        # Write to temporary file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False) as tf:
            sample_data.to_csv(tf.name)
            temp_results_path = tf.name
        
        # Analyze the sample data
        result = await server.call_tool("analyze_results", {
            "results_path": temp_results_path,
            "analysis_type": "summary"
        })
        
        print(f"   Success: {result['success']}")
        if result['success'] and 'data' in result:
            analysis_data = result['data']
            if 'analysis_results' in analysis_data:
                analysis = analysis_data['analysis_results']
                if 'data_overview' in analysis:
                    overview = analysis['data_overview']
                    print(f"   Timesteps: {overview['n_timesteps']}")
                    print(f"   Variables: {overview['n_variables']}")
                    print(f"   Frequency: {overview.get('frequency', 'unknown')}")
        else:
            print(f"   Errors: {result.get('errors', ['Unknown error'])}")
        
        # Clean up
        Path(temp_results_path).unlink()
        
    except Exception as e:
        print(f"   Error: {e}")


def example_mcp_protocol_requests():
    """Example MCP protocol requests for testing with AI assistants."""
    
    print("\n=== Example MCP Protocol Requests ===")
    
    examples = [
        {
            "name": "Initialize Server",
            "request": {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": {}
            }
        },
        {
            "name": "List Available Tools", 
            "request": {
                "jsonrpc": "2.0",
                "id": 2,
                "method": "tools/list",
                "params": {}
            }
        },
        {
            "name": "Configure Simulation",
            "request": {
                "jsonrpc": "2.0",
                "id": 3,
                "method": "tools/call",
                "params": {
                    "name": "configure_simulation",
                    "arguments": {
                        "validate_only": True
                    }
                }
            }
        },
        {
            "name": "Run Simulation with Sample Data",
            "request": {
                "jsonrpc": "2.0", 
                "id": 4,
                "method": "tools/call",
                "params": {
                    "name": "run_simulation",
                    "arguments": {
                        "use_sample_data": True,
                        "save_state": False,
                        "time_step": 3600
                    }
                }
            }
        }
    ]
    
    for example in examples:
        print(f"\n{example['name']}:")
        print(json.dumps(example['request'], indent=2))


async def main():
    """Main example function."""
    
    if len(sys.argv) > 1 and sys.argv[1] == "--server":
        # Run as MCP server
        print("Starting SuPy MCP Server...")
        server = SUPYMCPServer()
        await server.run_stdio()
    
    elif len(sys.argv) > 1 and sys.argv[1] == "--examples":
        # Show example requests
        example_mcp_protocol_requests()
    
    else:
        # Run programmatic examples
        await example_programmatic_usage()
        example_mcp_protocol_requests()


if __name__ == "__main__":
    print("SuPy MCP Server Example")
    print("Usage:")
    print("  python example.py              # Run programmatic examples")
    print("  python example.py --server     # Start MCP server (stdio mode)")
    print("  python example.py --examples   # Show example MCP requests")
    print()
    
    asyncio.run(main())