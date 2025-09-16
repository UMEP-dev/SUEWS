#!/usr/bin/env python3
"""Validate the test setup without running pytest."""

import sys
import os
import importlib.util
from pathlib import Path


def validate_file_structure():
    """Validate that all test files are in place."""
    base_dir = Path(__file__).parent
    expected_files = [
        "tests/__init__.py",
        "tests/conftest.py",
        "tests/test_server.py",
        "pytest.ini",
        ".coveragerc",
    ]

    print("Validating file structure...")
    missing_files = []

    for file_path in expected_files:
        full_path = base_dir / file_path
        if full_path.exists():
            print(f"✓ {file_path}")
        else:
            print(f"❌ {file_path} - MISSING")
            missing_files.append(file_path)

    return len(missing_files) == 0


def validate_imports():
    """Validate that imports work correctly."""
    print("\nValidating imports...")

    # Add src to path
    src_path = Path(__file__).parent / "src"
    sys.path.insert(0, str(src_path))

    try:
        # Test core imports
        from suews_mcp.config import MCPServerConfig, load_config, setup_logging

        print("✓ Config imports work")

        from suews_mcp.handlers import SUEWSMCPHandlers, MCP_AVAILABLE

        print("✓ Handlers imports work")

        from suews_mcp.server import SUEWSMCPServer, run_server, main

        print("✓ Server imports work")

        # Test config creation
        config = load_config()
        assert config.server_name == "suews-mcp"
        print("✓ Config creation works")

        # Test handlers creation
        handlers = SUEWSMCPHandlers(config)
        assert handlers.config == config
        print("✓ Handlers creation works")

        # Test server creation
        server = SUEWSMCPServer(config)
        assert server.config == config
        print("✓ Server creation works")

        return True

    except Exception as e:
        print(f"❌ Import/creation error: {e}")
        return False


def validate_test_files():
    """Validate that test files can be imported."""
    print("\nValidating test files...")

    # Add tests to path
    tests_path = Path(__file__).parent / "tests"
    sys.path.insert(0, str(tests_path))

    try:
        # Load conftest module
        spec = importlib.util.spec_from_file_location(
            "conftest", tests_path / "conftest.py"
        )
        conftest = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(conftest)
        print("✓ conftest.py loads correctly")

        # Load test_server module
        spec = importlib.util.spec_from_file_location(
            "test_server", tests_path / "test_server.py"
        )
        test_server = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(test_server)
        print("✓ test_server.py loads correctly")

        return True

    except Exception as e:
        print(f"❌ Test file error: {e}")
        return False


def validate_pytest_config():
    """Validate pytest configuration."""
    print("\nValidating pytest configuration...")

    pytest_ini = Path(__file__).parent / "pytest.ini"
    coveragerc = Path(__file__).parent / ".coveragerc"

    try:
        # Check pytest.ini
        with open(pytest_ini, "r") as f:
            pytest_content = f.read()

        required_pytest_sections = [
            "[tool:pytest]",
            "testpaths = tests",
            "asyncio_mode = auto",
            "--cov=src/suews_mcp",
            "--cov-fail-under=80",
        ]

        for section in required_pytest_sections:
            if section in pytest_content:
                print(f"✓ pytest.ini contains: {section}")
            else:
                print(f"❌ pytest.ini missing: {section}")
                return False

        # Check .coveragerc
        with open(coveragerc, "r") as f:
            coverage_content = f.read()

        required_coverage_sections = [
            "[run]",
            "source = src/suews_mcp",
            "[report]",
            "fail_under = 80",
        ]

        for section in required_coverage_sections:
            if section in coverage_content:
                print(f"✓ .coveragerc contains: {section}")
            else:
                print(f"❌ .coveragerc missing: {section}")
                return False

        return True

    except Exception as e:
        print(f"❌ Config validation error: {e}")
        return False


def main():
    """Main validation function."""
    print("SUEWS MCP Server - Test Setup Validation")
    print("=" * 50)

    all_good = True

    # Validate file structure
    if not validate_file_structure():
        all_good = False

    # Validate imports
    if not validate_imports():
        all_good = False

    # Validate test files
    if not validate_test_files():
        all_good = False

    # Validate configurations
    if not validate_pytest_config():
        all_good = False

    print("\n" + "=" * 50)
    if all_good:
        print("🎉 ALL VALIDATIONS PASSED! 🎉")
        print("\nTest setup is complete and ready to use!")
        print("\nTo run tests (when pytest is available):")
        print("  pytest")
        print("  pytest --cov=src/suews_mcp")
        print("  pytest -v tests/test_server.py::TestSUEWSMCPServer")
        print("\nTest organization:")
        print("  - Unit tests: pytest -m unit")
        print("  - Integration tests: pytest -m integration")
        print("  - Server tests: pytest -m server")
        print("  - Handler tests: pytest -m handlers")
        print("  - Health check tests: pytest -m health")
        print("  - Config tests: pytest -m config")
    else:
        print("❌ VALIDATION FAILED!")
        print("Please fix the issues above before running tests.")

    return all_good


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
