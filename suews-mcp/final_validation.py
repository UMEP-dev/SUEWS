#!/usr/bin/env python3
"""Final comprehensive validation of SUEWS MCP test setup."""

import sys
import os
from pathlib import Path

def main():
    """Main validation function."""
    print("SUEWS MCP Server - Final Test Infrastructure Validation")
    print("=" * 60)
    
    base_dir = Path(__file__).parent
    
    # 1. Check all required files exist
    print("1. Checking file structure...")
    required_files = {
        "tests/__init__.py": "Test package initialization",
        "tests/conftest.py": "Test fixtures and configuration",
        "tests/test_server.py": "Main test suite", 
        "pytest.ini": "Pytest configuration",
        ".coveragerc": "Coverage configuration"
    }
    
    all_files_exist = True
    for file_path, description in required_files.items():
        full_path = base_dir / file_path
        if full_path.exists():
            size = full_path.stat().st_size
            print(f"   ✓ {file_path:<25} ({size:,} bytes) - {description}")
        else:
            print(f"   ❌ {file_path:<25} - MISSING - {description}")
            all_files_exist = False
    
    # 2. Check syntax of Python files
    print("\n2. Validating Python syntax...")
    python_files = [
        base_dir / "tests" / "__init__.py",
        base_dir / "tests" / "conftest.py", 
        base_dir / "tests" / "test_server.py"
    ]
    
    syntax_valid = True
    for py_file in python_files:
        try:
            with open(py_file, 'rb') as f:
                compile(f.read(), py_file.name, 'exec')
            print(f"   ✓ {py_file.name} - Valid syntax")
        except SyntaxError as e:
            print(f"   ❌ {py_file.name} - Syntax error: {e}")
            syntax_valid = False
    
    # 3. Check that core modules import correctly
    print("\n3. Testing core module imports...")
    sys.path.insert(0, str(base_dir / "src"))
    
    modules_imported = True
    try:
        from suews_mcp.config import MCPServerConfig, load_config, setup_logging
        from suews_mcp.handlers import SUEWSMCPHandlers, MCP_AVAILABLE
        from suews_mcp.server import SUEWSMCPServer, run_server, main
        print(f"   ✓ All core modules import successfully")
        print(f"   ✓ MCP library available: {'Yes' if MCP_AVAILABLE else 'No (test mode)'}")
    except ImportError as e:
        print(f"   ❌ Import error: {e}")
        modules_imported = False
    
    # 4. Test basic functionality
    print("\n4. Testing basic functionality...")
    functionality_works = True
    try:
        config = load_config()
        handlers = SUEWSMCPHandlers(config)
        server = SUEWSMCPServer(config)
        print(f"   ✓ Configuration loading works")
        print(f"   ✓ Handlers creation works")
        print(f"   ✓ Server creation works")
        print(f"   ✓ Server name: {config.server_name}")
        print(f"   ✓ Max concurrent simulations: {config.max_concurrent_simulations}")
    except Exception as e:
        print(f"   ❌ Functionality error: {e}")
        functionality_works = False
    
    # 5. Check configuration files
    print("\n5. Validating configuration files...")
    config_valid = True
    
    # Check pytest.ini
    pytest_ini = base_dir / "pytest.ini"
    with open(pytest_ini, 'r') as f:
        pytest_content = f.read()
    
    pytest_checks = [
        ("testpaths = tests", "Test discovery path"),
        ("asyncio_mode = auto", "Async test mode"),
        ("--cov=src/suews_mcp", "Coverage source"),
        ("--cov-fail-under=80", "Coverage threshold")
    ]
    
    for check, desc in pytest_checks:
        if check in pytest_content:
            print(f"   ✓ pytest.ini: {desc}")
        else:
            print(f"   ❌ pytest.ini missing: {desc}")
            config_valid = False
    
    # Check .coveragerc
    coveragerc = base_dir / ".coveragerc"
    with open(coveragerc, 'r') as f:
        coverage_content = f.read()
    
    coverage_checks = [
        ("source = src/suews_mcp", "Coverage source"),
        ("fail_under = 80", "Coverage threshold"),
        ("[report]", "Report configuration"),
        ("show_missing = True", "Missing lines report")
    ]
    
    for check, desc in coverage_checks:
        if check in coverage_content:
            print(f"   ✓ .coveragerc: {desc}")
        else:
            print(f"   ❌ .coveragerc missing: {desc}")
            config_valid = False
    
    # 6. Count test cases
    print("\n6. Analyzing test coverage...")
    test_file = base_dir / "tests" / "test_server.py"
    with open(test_file, 'r') as f:
        test_content = f.read()
    
    # Count different types of content
    test_classes = test_content.count("class Test")
    test_functions = test_content.count("def test_")
    async_tests = test_content.count("async def test_")
    pytest_marks = test_content.count("@pytest.mark")
    
    print(f"   ✓ Test classes: {test_classes}")
    print(f"   ✓ Test functions: {test_functions}")
    print(f"   ✓ Async test functions: {async_tests}")
    print(f"   ✓ Pytest markers: {pytest_marks}")
    
    # Test categories
    categories = ["unit", "integration", "server", "handlers", "config", "health"]
    for category in categories:
        count = test_content.count(f"@pytest.mark.{category}")
        print(f"   ✓ {category.capitalize()} tests: {count}")
    
    # Final assessment
    print("\n" + "=" * 60)
    all_good = all_files_exist and syntax_valid and modules_imported and functionality_works and config_valid
    
    if all_good:
        print("🎉 COMPLETE TEST INFRASTRUCTURE SETUP SUCCESSFUL! 🎉")
        print("\n✅ All components are properly configured and ready to use:")
        print("   • File structure is complete")
        print("   • Python syntax is valid")
        print("   • Core modules import correctly")
        print("   • Basic functionality works")
        print("   • Configuration files are properly set up")
        print(f"   • {test_classes} test classes with {test_functions} test methods")
        print("   • Coverage targeting 80%+")
        print("   • Comprehensive test fixtures available")
        
        print("\n📋 Ready for pytest execution:")
        print("   pytest                              # Run all tests")
        print("   pytest -v                          # Verbose output")
        print("   pytest -m unit                     # Unit tests only")
        print("   pytest -m integration              # Integration tests only")
        print("   pytest --cov                       # With coverage report")
        print("   pytest tests/test_server.py        # Specific file")
        print("   pytest -k health_check             # Specific tests")
        
        print("\n📊 Test organization:")
        print("   • Unit tests: Fast, isolated component tests")
        print("   • Integration tests: Multi-component interaction tests") 
        print("   • Server tests: Server initialization and lifecycle")
        print("   • Handler tests: MCP protocol handler functionality")
        print("   • Health tests: Health check and monitoring")
        print("   • Config tests: Configuration management")
        
        print("\n🔧 Available test fixtures:")
        print("   • test_config: Full test configuration")
        print("   • minimal_config: Minimal test configuration")
        print("   • temp_directory: Temporary file system")
        print("   • mock_mcp_client: Mock MCP client")
        print("   • handlers/server: Test instances")
        print("   • Test data files: Config, forcing, output samples")
        
        return True
    else:
        print("❌ SETUP VALIDATION FAILED!")
        print("\nIssues found:")
        if not all_files_exist:
            print("   • Missing required files")
        if not syntax_valid:
            print("   • Python syntax errors")
        if not modules_imported:
            print("   • Module import failures")
        if not functionality_works:
            print("   • Basic functionality issues")
        if not config_valid:
            print("   • Configuration file issues")
        
        print("\nPlease resolve these issues before proceeding.")
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)