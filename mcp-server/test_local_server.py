#!/usr/bin/env python3
"""Test the parameter explainer server locally."""

import asyncio
import sys
import os

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from suews_mcp.tools.parameter_explainer import explain_parameter


async def test_parameters():
    """Test parameter explanations."""
    
    test_params = [
        "tstep",
        "albedo", 
        "lai_max",
        "NetRadiationMethod",
        "bldgh"
    ]
    
    print("Testing SUEWS Parameter Explainer")
    print("=" * 50)
    
    for param in test_params:
        print(f"\n\nParameter: {param}")
        print("-" * 40)
        
        try:
            result = await explain_parameter(param, include_examples=True)
            # Print first 300 chars
            if len(result) > 300:
                print(result[:300] + "...")
            else:
                print(result)
        except Exception as e:
            print(f"Error: {e}")


if __name__ == "__main__":
    asyncio.run(test_parameters())