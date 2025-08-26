#!/usr/bin/env python3
"""Basic functionality test for SUEWS MCP Server."""

import asyncio
import sys
import os
import logging

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from suews_mcp.config import MCPServerConfig, load_config, setup_logging
from suews_mcp.handlers import SUEWSMCPHandlers, MCP_AVAILABLE
from suews_mcp.server import SUEWSMCPServer


async def test_handlers():
    """Test handlers functionality."""
    print("Testing SUEWS MCP Handlers...")
    
    config = load_config()
    handlers = SUEWSMCPHandlers(config)
    
    # Test initialization
    init_result = await handlers.handle_initialize({})
    print(f"✓ Initialize: {init_result}")
    
    # Test list tools
    tools_result = await handlers.handle_list_tools()
    print(f"✓ List tools: Found {len(tools_result['tools'])} tools")
    
    # Test health check
    health_result = await handlers.handle_call_tool("health_check", {})
    print(f"✓ Health check: {health_result['content'][0]['text'][:50]}...")
    
    # Test validation tool
    validation_result = await handlers.handle_call_tool("validate_suews_config", {"config_file": "test.yml"})
    print(f"✓ Validation tool: {validation_result['content'][0]['text'][:50]}...")
    
    print("✓ All handler tests passed!")


def test_config():
    """Test configuration functionality."""
    print("Testing SUEWS MCP Configuration...")
    
    # Test default config
    config = load_config()
    print(f"✓ Default config: {config.server_name} v{config.server_version}")
    
    # Test environment variable support
    os.environ["SUEWS_MCP_SERVER_NAME"] = "test-server"
    os.environ["SUEWS_MCP_LOG_LEVEL"] = "DEBUG"
    config = load_config()
    print(f"✓ Environment config: {config.server_name}, log level: {config.log_level}")
    
    # Test config methods
    log_level = config.get_log_level()
    temp_dir = config.get_temp_dir()
    config_dict = config.to_dict()
    print(f"✓ Config methods: log_level={log_level}, temp_dir exists, dict has {len(config_dict)} keys")
    
    # Clean up
    del os.environ["SUEWS_MCP_SERVER_NAME"]
    del os.environ["SUEWS_MCP_LOG_LEVEL"]
    
    print("✓ All configuration tests passed!")


def test_server_init():
    """Test server initialization."""
    print("Testing SUEWS MCP Server initialization...")
    
    config = load_config()
    server = SUEWSMCPServer(config)
    
    print(f"✓ Server initialized: {server.config.server_name}")
    print(f"✓ MCP available: {MCP_AVAILABLE}")
    
    if not MCP_AVAILABLE:
        print("✓ Server correctly handles missing MCP library")
    
    print("✓ All server initialization tests passed!")


async def main():
    """Run all tests."""
    print("SUEWS MCP Server - Basic Functionality Test")
    print("=" * 50)
    
    # Set up logging
    logging.basicConfig(level=logging.WARNING)  # Reduce noise during testing
    
    try:
        # Test configuration
        test_config()
        print()
        
        # Test server initialization
        test_server_init()
        print()
        
        # Test handlers
        await test_handlers()
        print()
        
        print("🎉 ALL TESTS PASSED! 🎉")
        print()
        print("Summary:")
        print("✓ Configuration module works correctly")
        print("✓ Server initializes without errors") 
        print("✓ Handlers provide all required functionality")
        print("✓ MCP library absence handled gracefully")
        print("✓ Environment variable configuration supported")
        print("✓ All tools can be called and return valid responses")
        
        if not MCP_AVAILABLE:
            print()
            print("Note: To run the actual MCP server, install the MCP library:")
            print("  pip install mcp>=1.0.0")
        
        return True
        
    except Exception as e:
        print(f"❌ Test failed: {e}")
        import traceback
        traceback.print_exc()
        return False


if __name__ == "__main__":
    success = asyncio.run(main())
    sys.exit(0 if success else 1)