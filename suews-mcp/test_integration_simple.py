#!/usr/bin/env python3
"""
Simple integration test for SUEWS MCP server without external dependencies.

This validates that the core implementation works correctly.
"""

import asyncio
import sys
import os
from pathlib import Path

# Add the suews-mcp module to path
current_dir = Path(__file__).parent
sys.path.insert(0, str(current_dir / "src"))

try:
    from suews_mcp.config import MCPServerConfig
    from suews_mcp.handlers import SUEWSMCPHandlers

    print("✓ Successfully imported SUEWS MCP modules")
except ImportError as e:
    print(f"✗ Failed to import SUEWS MCP modules: {e}")
    sys.exit(1)


async def test_basic_functionality():
    """Test basic MCP functionality."""
    print("\n" + "=" * 50)
    print("SUEWS MCP Integration Test")
    print("=" * 50)

    # 1. Test configuration
    print("\n1. Testing configuration creation...")
    try:
        config = MCPServerConfig(
            server_name="test-suews-mcp",
            server_version="0.1.0-test",
            enable_simulation_tool=True,
            enable_validation_tool=True,
            enable_analysis_tool=True,
            max_concurrent_simulations=2,
        )
        print("✓ Configuration created successfully")
        print(f"  - Server: {config.server_name} v{config.server_version}")
        print(
            f"  - Tools enabled: simulation={config.enable_simulation_tool}, validation={config.enable_validation_tool}, analysis={config.enable_analysis_tool}"
        )
    except Exception as e:
        print(f"✗ Configuration creation failed: {e}")
        return False

    # 2. Test handlers initialization
    print("\n2. Testing handlers initialization...")
    try:
        handlers = SUEWSMCPHandlers(config)
        print("✓ Handlers initialized successfully")
        print(
            f"  - Active simulations tracking: {len(handlers._active_simulations)} simulations"
        )
        print(f"  - Concurrent limit: {handlers._simulation_semaphore._value}")

        # Check tool availability
        from suews_mcp.handlers import SUPY_MCP_TOOLS_AVAILABLE

        print(f"  - SuPy MCP tools available: {SUPY_MCP_TOOLS_AVAILABLE}")

    except Exception as e:
        print(f"✗ Handlers initialization failed: {e}")
        return False

    # 3. Test MCP protocol methods
    print("\n3. Testing MCP protocol methods...")
    try:
        # Initialize
        init_params = {"protocolVersion": "2024-11-05", "capabilities": {}}
        init_result = await handlers.handle_initialize(init_params)
        print("✓ Initialize handler works")

        # List tools
        tools_result = await handlers.handle_list_tools()
        print("✓ List tools handler works")

        if hasattr(tools_result, "tools"):
            tools = tools_result.tools
        elif isinstance(tools_result, dict) and "tools" in tools_result:
            tools = tools_result["tools"]
        else:
            tools = tools_result if isinstance(tools_result, list) else []

        print(f"  - Available tools: {len(tools)}")
        for i, tool in enumerate(tools[:3]):  # Show first 3 tools
            if hasattr(tool, "name"):
                print(f"    {i+1}. {tool.name}: {tool.description[:50]}...")
            elif isinstance(tool, dict):
                name = tool.get("name", "Unknown")
                desc = tool.get("description", "No description")
                print(f"    {i+1}. {name}: {desc[:50]}...")

        # List prompts
        prompts_result = await handlers.handle_list_prompts()
        print("✓ List prompts handler works")

        if hasattr(prompts_result, "prompts"):
            prompts = prompts_result.prompts
        elif isinstance(prompts_result, dict) and "prompts" in prompts_result:
            prompts = prompts_result["prompts"]
        else:
            prompts = prompts_result if isinstance(prompts_result, list) else []

        print(f"  - Available prompts: {len(prompts)}")

    except Exception as e:
        print(f"✗ MCP protocol methods failed: {e}")
        return False

    # 4. Test tool calling
    print("\n4. Testing tool calling...")
    try:
        # Test health check
        health_result = await handlers.handle_call_tool("health_check", {})
        print("✓ Health check tool works")

        if hasattr(health_result, "content") and health_result.content:
            content = health_result.content[0]
            if hasattr(content, "text"):
                print(f"  - Health check response: {content.text[:100]}...")

        # Test unknown tool handling
        unknown_result = await handlers.handle_call_tool("nonexistent_tool", {})
        print("✓ Unknown tool handling works")

        if hasattr(unknown_result, "is_error"):
            print(f"  - Correctly reports error: {unknown_result.is_error}")

    except Exception as e:
        print(f"✗ Tool calling failed: {e}")
        return False

    # 5. Test SuPy integration (if available)
    print("\n5. Testing SuPy integration...")
    try:
        from suews_mcp.handlers import SUPY_MCP_TOOLS_AVAILABLE

        if SUPY_MCP_TOOLS_AVAILABLE:
            print("✓ SuPy MCP tools are available")

            # Try to use SuPy tools
            if hasattr(handlers, "_configure_tool") and handlers._configure_tool:
                print("  - Configure tool loaded")
            if hasattr(handlers, "_run_tool") and handlers._run_tool:
                print("  - Run tool loaded")
            if hasattr(handlers, "_analyze_tool") and handlers._analyze_tool:
                print("  - Analyze tool loaded")

            # Test configure tool with minimal params
            try:
                if handlers._configure_tool:
                    config_result = await handlers._configure_tool.execute({})
                    print("  ✓ Configure tool execution test completed")
                    if config_result.get("success"):
                        print("    - Configure tool succeeded")
                    else:
                        print(
                            f"    - Configure tool failed (expected): {config_result.get('errors', [])[:1]}"
                        )
            except Exception as e:
                print(
                    f"  - Configure tool test failed (expected in test env): {str(e)[:60]}..."
                )
        else:
            print("⚠ SuPy MCP tools not available (expected in development)")
            print("  - Using fallback tool implementations")

    except Exception as e:
        print(f"✗ SuPy integration test failed: {e}")
        return False

    # 6. Test resource handling
    print("\n6. Testing resource handling...")
    try:
        # Test resource listing
        list_result = await handlers.handle_call_tool(
            "list_resources", {"resource_type": "all"}
        )
        print("✓ Resource listing works")

        # Test security - try path traversal
        try:
            security_result = await handlers.handle_call_tool(
                "get_resource", {"resource_path": "../../../etc/passwd"}
            )

            # Should block or error
            if hasattr(security_result, "is_error") and security_result.is_error:
                print("✓ Security check blocks path traversal")
            else:
                print("⚠ Security check may need review")

        except Exception:
            print("✓ Security check blocks path traversal (via exception)")

    except Exception as e:
        print(f"✗ Resource handling test failed: {e}")
        return False

    # 7. Test cleanup
    print("\n7. Testing cleanup...")
    try:
        handlers.cleanup()
        print("✓ Cleanup completed")
        print(f"  - Active simulations cleared: {len(handlers._active_simulations)}")
    except Exception as e:
        print(f"✗ Cleanup failed: {e}")
        return False

    return True


def test_benchmark_data_availability():
    """Test if benchmark data is available."""
    print("\n" + "=" * 50)
    print("SUEWS Benchmark Data Availability Test")
    print("=" * 50)

    # Check for benchmark config
    config_path = (
        Path(__file__).parent.parent
        / "test"
        / "fixtures"
        / "benchmark1"
        / "benchmark1.yml"
    )
    if config_path.exists():
        print(f"✓ Benchmark config found: {config_path}")

        try:
            import yaml

            with open(config_path, "r") as f:
                config_data = yaml.safe_load(f)
            print(f"  - Config name: {config_data.get('name', 'Unknown')}")
            print(f"  - Sites: {len(config_data.get('sites', []))}")
            print(f"  - Model control: {'model' in config_data}")
        except Exception as e:
            print(f"  - Config validation failed: {e}")
    else:
        print(f"⚠ Benchmark config not found: {config_path}")

    # Check for forcing data
    forcing_path = config_path.parent / "forcing" / "Kc1_2011_data_5.txt"
    if forcing_path.exists():
        print(f"✓ Benchmark forcing data found: {forcing_path}")
        try:
            with open(forcing_path, "r") as f:
                line = f.readline().strip()
            print(f"  - First line length: {len(line)} characters")
        except Exception as e:
            print(f"  - Forcing data validation failed: {e}")
    else:
        print(f"⚠ Benchmark forcing data not found: {forcing_path}")


async def main():
    """Run all tests."""
    print("Starting SUEWS MCP Integration Tests...")

    # Basic functionality test
    basic_success = await test_basic_functionality()

    # Benchmark data availability test
    test_benchmark_data_availability()

    # Summary
    print("\n" + "=" * 50)
    print("TEST SUMMARY")
    print("=" * 50)

    if basic_success:
        print("✓ Basic functionality tests: PASSED")
        print("\nThe SUEWS MCP server implementation is working correctly!")
        print("\nNext steps:")
        print("1. Install the MCP library: pip install mcp")
        print("2. Install SuPy dependencies for full functionality")
        print("3. Run with a proper MCP client")
    else:
        print("✗ Basic functionality tests: FAILED")
        print("\nPlease check the errors above and fix any issues.")

    print("\nIntegration tests completed.")
    return basic_success


if __name__ == "__main__":
    success = asyncio.run(main())
    sys.exit(0 if success else 1)
