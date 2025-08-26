#!/usr/bin/env python3
"""
Basic SUEWS Simulation Example using MCP Tools

This script demonstrates a complete SUEWS urban climate simulation workflow
using the MCP server tools for guided assistance.
"""

import asyncio
import json
from pathlib import Path
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client


async def run_basic_simulation():
    """
    Run a basic SUEWS simulation using MCP tools for guidance.
    
    This example demonstrates:
    1. Getting configuration templates
    2. Running simulations with sample data
    3. Analysing results with MCP tools
    4. Saving outputs for further analysis
    """
    # Connect to SUEWS MCP server
    server_params = StdioServerParameters(
        command="python", 
        args=["-m", "suews_mcp.server"]
    )
    
    try:
        async with stdio_client(server_params) as (read, write):
            async with ClientSession(read, write) as session:
                print("🔄 Initializing MCP session...")
                await session.initialize()
                print("✅ MCP server connected")
                
                # Step 1: Get residential area template
                print("\n📋 Loading residential area template...")
                template_result = await session.call_tool(
                    "get_resource", 
                    {"resource_path": "templates/configs/residential.yml"}
                )
                
                if not template_result.isError:
                    print("✅ Template loaded successfully")
                    config_content = template_result.content[0].text
                else:
                    print("❌ Failed to load template")
                    return
                
                # Step 2: Validate configuration
                print("\n🔍 Validating configuration...")
                validation_result = await session.call_tool(
                    "validate_suews_config",
                    {"config_data": config_content, "strict": True}
                )
                
                validation_data = json.loads(validation_result.content[0].text)
                if validation_data.get("success", False):
                    print("✅ Configuration is valid")
                else:
                    print("❌ Configuration validation failed:")
                    for error in validation_data.get("errors", []):
                        print(f"   - {error}")
                    return
                
                # Step 3: Run simulation with sample data
                print("\n🏙️  Running SUEWS simulation...")
                sim_result = await session.call_tool(
                    "run_suews_simulation",
                    {
                        "config_data": config_content,
                        "use_sample_data": True,
                        "simulation_id": "basic_residential_example",
                        "duration_months": 12,
                        "output_frequency": "hourly"
                    }
                )
                
                sim_data = json.loads(sim_result.content[0].text)
                if sim_data.get("success", False):
                    print("✅ Simulation completed successfully!")
                    print(f"   Duration: {sim_data.get('duration_seconds', 'N/A')} seconds")
                    print(f"   Time steps: {sim_data.get('time_steps', 'N/A')}")
                else:
                    print("❌ Simulation failed:")
                    for error in sim_data.get("errors", []):
                        print(f"   - {error}")
                    return
                
                # Step 4: Analyse results
                print("\n📊 Analysing simulation results...")
                analysis_result = await session.call_tool(
                    "analyze_suews_output",
                    {
                        "simulation_id": "basic_residential_example",
                        "metrics": ["QH", "QE", "QN", "QS", "QF", "T2", "RH2"],
                        "time_aggregation": "monthly",
                        "include_statistics": True,
                        "create_plots": True
                    }
                )
                
                analysis_data = json.loads(analysis_result.content[0].text)
                if analysis_data.get("success", False):
                    print("✅ Analysis completed successfully!")
                    
                    # Display key results
                    stats = analysis_data.get("statistics", {})
                    if stats:
                        print("\n📈 Key Results Summary:")
                        for var, var_stats in stats.items():
                            mean_val = var_stats.get("mean", 0)
                            print(f"   {var}: Mean = {mean_val:.2f} W/m²" 
                                  if var.startswith("Q") else 
                                  f"   {var}: Mean = {mean_val:.2f}")
                    
                    # List output files
                    outputs = analysis_data.get("output_files", [])
                    if outputs:
                        print("\n💾 Generated output files:")
                        for output_file in outputs:
                            print(f"   - {output_file}")
                
                else:
                    print("❌ Analysis failed:")
                    for error in analysis_data.get("errors", []):
                        print(f"   - {error}")
                
                # Step 5: Get interpretation guidance
                print("\n💡 Getting result interpretation guidance...")
                guidance_result = await session.call_tool(
                    "get_resource",
                    {"resource_path": "templates/prompts/analysis/energy_balance_interpretation.md"}
                )
                
                if not guidance_result.isError:
                    print("✅ Interpretation guidance available")
                    print("\n📚 Next steps and interpretation tips:")
                    print(guidance_result.content[0].text[:500] + "...")
                else:
                    print("ℹ️  Basic interpretation: Check seasonal patterns in QN, QH, QE")
                
                print("\n🎉 Basic simulation workflow completed successfully!")
                print("📁 Check the generated files for detailed results")
                print("🚀 Ready to try more advanced examples!")
                
                return analysis_data
                
    except Exception as e:
        print(f"❌ Error during simulation: {e}")
        print("💡 Try running with direct SuPy if MCP server is not available")
        return None


async def run_fallback_supy():
    """
    Fallback to direct SuPy usage if MCP server is not available.
    """
    try:
        import supy as sp
        import matplotlib.pyplot as plt
        
        print("🔄 Running fallback simulation with SuPy directly...")
        
        # Load sample data
        df_state_init, df_forcing = sp.load_sample_data()
        df_forcing_year = df_forcing.loc["2012"].iloc[1:]
        grid = df_state_init.index[0]
        
        # Run simulation
        print("🏙️  Running SUEWS simulation...")
        df_output, df_state_final = sp.run_supy(df_forcing_year, df_state_init)
        print("✅ Simulation completed!")
        
        # Basic analysis
        monthly_data = df_output["SUEWS"].loc[grid].resample("1M").mean()
        energy_vars = ["QN", "QH", "QE", "QS", "QF"]
        
        print("📊 Monthly energy balance statistics:")
        for var in energy_vars:
            mean_val = monthly_data[var].mean()
            print(f"   {var}: Mean = {mean_val:.2f} W/m²")
        
        # Save results
        output_files = sp.save_supy(df_output, df_state_final)
        print(f"💾 Results saved: {len(output_files)} files created")
        
        # Create basic plot
        fig, ax = plt.subplots(figsize=(10, 6))
        monthly_data[energy_vars].plot(ax=ax, kind="bar")
        ax.set_title("Monthly Average Surface Energy Balance")
        ax.set_ylabel("Flux (W/m²)")
        ax.set_xlabel("Month")
        ax.legend()
        plt.xticks(rotation=45)
        plt.tight_layout()
        plt.savefig("basic_simulation_energy_balance.png", dpi=150, bbox_inches="tight")
        print("📈 Plot saved to basic_simulation_energy_balance.png")
        
        return True
        
    except ImportError:
        print("❌ SuPy not available. Please install supy: pip install supy")
        return False
    except Exception as e:
        print(f"❌ Error in fallback simulation: {e}")
        return False


def main():
    """
    Main function to run the basic simulation example.
    """
    print("🏙️  SUEWS Basic Simulation Example")
    print("=" * 50)
    
    # Try MCP approach first
    try:
        result = asyncio.run(run_basic_simulation())
        if result is None:
            print("\n🔄 Falling back to direct SuPy approach...")
            asyncio.run(run_fallback_supy())
    except Exception as e:
        print(f"❌ MCP approach failed: {e}")
        print("🔄 Falling back to direct SuPy approach...")
        asyncio.run(run_fallback_supy())


if __name__ == "__main__":
    main()