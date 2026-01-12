#!/usr/bin/env python3
"""
Generate JSON Schema for SUEWS.

This is a convenience wrapper around the package's schema exporter.
Requires SUEWS to be installed (including Fortran components).

Usage:
    python .github/scripts/generate_schema.py [--preview --pr-number N]

For CI, prefer using the module directly:
    python -m supy.data_model.schema.exporter
"""

import sys


def main():
    """Run the schema exporter."""
    try:
        from supy.data_model.schema.exporter import main as exporter_main
    except ImportError as e:
        print(
            f"Error: Could not import supy. Make sure SUEWS is installed.\n"
            f"  pip install .  # or: uv pip install .\n"
            f"Detail: {e}",
            file=sys.stderr,
        )
        sys.exit(1)

    exporter_main()


if __name__ == "__main__":
    main()
