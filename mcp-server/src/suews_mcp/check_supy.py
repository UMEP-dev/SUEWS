"""Check SuPy installation and version."""

import sys
import subprocess
from pathlib import Path


def check_supy_installation():
    """Check if SuPy is installed and get version info."""
    print("=== SuPy Installation Check ===\n")

    # Check Python info
    print(f"Python executable: {sys.executable}")
    print(f"Python version: {sys.version}")
    print(f"Python path: {sys.path[:3]}...\n")

    # Try to import SuPy
    try:
        import supy

        print(f"✓ SuPy is installed")
        print(f"  Version: {supy.__version__}")
        print(f"  Location: {supy.__file__}")

        # Check supy.check_version() if available
        if hasattr(supy, "check_version"):
            print("\nRunning supy.check_version():")
            supy.check_version()

        # Check data models
        try:
            from supy.data_model.core import SUEWSConfig

            print("\n✓ SuPy data models are available")

            # List available models
            from supy.data_model import model

            models = [attr for attr in dir(model) if not attr.startswith("_") and attr[0].isupper()]
            print(f"  Available models: {', '.join(models[:5])}...")

        except ImportError as e:
            print(f"\n✗ SuPy data models not available: {e}")

        return True

    except ImportError as e:
        print(f"✗ SuPy is NOT installed: {e}")
        print("\nTo install SuPy:")
        print("  pip install supy==2025.6.2.dev")
        print("  or")
        print("  pip install git+https://github.com/UMEP-dev/SUEWS.git@master")
        return False


def check_mcp_dependencies():
    """Check if MCP and other dependencies are installed."""
    print("\n=== MCP Dependencies Check ===\n")

    deps = ["mcp", "pydantic", "pyyaml", "numpy", "pandas", "xarray"]
    missing = []

    for dep in deps:
        try:
            __import__(dep)
            print(f"✓ {dep} is installed")
        except ImportError:
            print(f"✗ {dep} is NOT installed")
            missing.append(dep)

    if missing:
        print(f"\nTo install missing dependencies:")
        print(f"  pip install {' '.join(missing)}")

    return len(missing) == 0


if __name__ == "__main__":
    supy_ok = check_supy_installation()
    deps_ok = check_mcp_dependencies()

    print("\n=== Summary ===")
    if supy_ok and deps_ok:
        print("✓ All dependencies are installed. MCP server should work.")
    else:
        print("✗ Missing dependencies. Please install them before running MCP server.")
        sys.exit(1)
