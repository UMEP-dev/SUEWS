#!/usr/bin/env python
"""
Quick test runner to verify validator changes don't break existing tests.
"""

import subprocess
import sys


def run_tests():
    """Run the migrated validator tests."""
    print("Running migrated validator tests...")

    # Run the main migrated validator tests
    cmd = [
        sys.executable,
        "-m",
        "pytest",
        "test/data_model/test_migrated_validators.py",
        "-v",
        "--tb=short",
    ]
    result = subprocess.run(cmd, capture_output=True, text=True)

    print(result.stdout)
    if result.stderr:
        print("STDERR:", result.stderr)

    if result.returncode != 0:
        print(f"\nTests failed with return code: {result.returncode}")
        return False
    else:
        print("\nAll migrated validator tests passed!")
        return True


if __name__ == "__main__":
    success = run_tests()
    sys.exit(0 if success else 1)
