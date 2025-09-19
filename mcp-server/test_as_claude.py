#!/usr/bin/env python3
"""Test MCP server as Claude Desktop would run it."""

import subprocess
import sys
import os


def test_with_python(python_path):
    """Test MCP server with specific Python."""
    print(f"\n=== Testing with {python_path} ===")

    # Check Python version
    result = subprocess.run([python_path, "--version"], capture_output=True, text=True)
    print(f"Version: {result.stdout.strip()}")

    # Check if SuPy is installed
    result = subprocess.run(
        [python_path, "-c", "import supy; print(f'SuPy {supy.__version__} installed')"],
        capture_output=True,
        text=True,
    )
    if result.returncode == 0:
        print(f"✓ {result.stdout.strip()}")
    else:
        print(f"✗ SuPy not installed: {result.stderr.strip()}")
        print(f"  Install with: {python_path} -m pip install supy==2025.6.2.dev")
        return False

    # Test running the server
    print("\nTesting server startup...")
    env = os.environ.copy()
    env["PYTHONPATH"] = "src"

    process = subprocess.Popen(
        [python_path, "run_server.py"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        env=env,
    )

    # Wait a bit and check if it started
    import time

    time.sleep(2)

    if process.poll() is None:
        print("✓ Server started successfully")
        process.terminate()
        return True
    else:
        stdout, stderr = process.communicate()
        print("✗ Server failed to start:")
        print(stderr)
        return False


# Test different Python installations
pythons_to_test = [
    "/usr/bin/python3",  # System Python (Claude Desktop likely uses this)
    "/opt/homebrew/bin/python3",  # Homebrew Python
    sys.executable,  # Current Python
]

print("Testing MCP server with different Python installations...")
print("This simulates how Claude Desktop would run the extension.")

for python_path in pythons_to_test:
    if os.path.exists(python_path):
        test_with_python(python_path)
    else:
        print(f"\n{python_path} not found, skipping...")

print("\n=== Recommendation ===")
print("Install SuPy for the Python that Claude Desktop uses:")
print("  /usr/bin/python3 -m pip install --user supy==2025.6.2.dev")
