#!/usr/bin/env python3
"""Test that all imports work after reorganization."""

import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / "src"))

print("Testing reorganized imports...")

try:
    # Test schema version import
    from supy.data_model._schema_version import CURRENT_SCHEMA_VERSION
    print(f"✓ Schema version import works: {CURRENT_SCHEMA_VERSION}")
except ImportError as e:
    print(f"✗ Schema version import failed: {e}")

try:
    # Test validation controller import
    from supy.data_model.validation.controller import ValidationController
    print("✓ ValidationController import works")
except ImportError as e:
    print(f"✗ ValidationController import failed: {e}")

try:
    # Test validation utils import
    from supy.data_model.validation.utils import check_missing_params
    print("✓ Validation utils import works")
except ImportError as e:
    print(f"✗ Validation utils import failed: {e}")

try:
    # Test yaml helpers import
    from supy.data_model.validation.yaml_helpers import run_precheck
    print("✓ YAML helpers import works")
except ImportError as e:
    print(f"✗ YAML helpers import failed: {e}")

try:
    # Test validation module import
    from supy.data_model.validation import (
        ValidationController as VC,
        check_missing_params as cmp,
        run_precheck as rp,
    )
    print("✓ Validation module import works")
except ImportError as e:
    print(f"✗ Validation module import failed: {e}")

try:
    # Test data_model module import
    from supy.data_model import (
        ValidationController as VC2,
        run_precheck as rp2,
    )
    print("✓ Data model module import works")
except ImportError as e:
    print(f"✗ Data model module import failed: {e}")

print("\n✅ All imports successful!" if all([
    'CURRENT_SCHEMA_VERSION' in locals(),
    'ValidationController' in locals(),
    'check_missing_params' in locals(),
    'run_precheck' in locals(),
]) else "\n⚠️ Some imports failed")