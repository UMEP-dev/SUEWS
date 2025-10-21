#!/usr/bin/env python3
"""Replace manual DATA statements with INCLUDE directive for generated file.

This script modifies suews_ctrl_output.f95 to use the auto-generated
varlist_generated.f95 file instead of manual DATA statements.
"""

from pathlib import Path

def replace_data_statements():
    """Replace DATA statements in Fortran file with INCLUDE directive."""

    fortran_file = Path(__file__).parent / "suews_ctrl_output.f95"

    # Read the file
    with open(fortran_file, 'r') as f:
        lines = f.readlines()

    # Find the start and end of DATA statements
    # Start: after "TYPE(varAttr) :: varListAll(1400)"
    # End: before "CONTAINS"

    start_idx = None
    end_idx = None
    contains_idx = None

    for i, line in enumerate(lines):
        if 'TYPE(varAttr) :: varListAll(1400)' in line:
            start_idx = i + 1  # Start after this line
        elif line.strip() == 'CONTAINS' and start_idx is not None:
            contains_idx = i
            end_idx = i  # End at CONTAINS
            break

    if start_idx is None or end_idx is None:
        print("❌ Could not find DATA statement boundaries")
        return 1

    print(f"Found DATA statements: lines {start_idx+1} to {end_idx}")

    # Create replacement lines
    replacement = [
        '\n',
        '   ! =========================================================================\n',
        '   ! Variable definitions now auto-generated from Python OUTPUT_REGISTRY\n',
        '   ! Source: src/supy/data_model/output/__init__.py\n',
        '   ! Generator: src/suews/src/generate_varlist.py\n',
        '   ! =========================================================================\n',
        '   ! NOTE: The following groups are not yet migrated to Python:\n',
        '   !   - SPARTACUS (experimental radiation model)\n',
        '   !   - EHC (experimental heat capacity model)\n',
        '   !   - STEBBS (experimental energy balance model)\n',
        '   !   - NHood (neighbourhood iteration diagnostics)\n',
        '   ! These will continue to use Fortran-only definitions until migrated.\n',
        '   ! =========================================================================\n',
        '\n',
        '   INCLUDE \'varlist_generated.f95\'\n',
        '\n',
    ]

    # Replace the DATA statements with the INCLUDE directive
    new_lines = lines[:start_idx] + replacement + lines[end_idx:]

    # Write back to file
    with open(fortran_file, 'w') as f:
        f.writelines(new_lines)

    print(f"✓ Replaced {end_idx - start_idx} lines with INCLUDE directive")
    print(f"✓ Modified: {fortran_file}")
    print(f"  Old DATA statements: {end_idx - start_idx} lines")
    print(f"  New INCLUDE directive: {len(replacement)} lines")
    print(f"  Net reduction: {end_idx - start_idx - len(replacement)} lines")

    return 0

if __name__ == "__main__":
    import sys
    sys.exit(replace_data_statements())
