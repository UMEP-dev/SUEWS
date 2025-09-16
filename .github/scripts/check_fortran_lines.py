#!/usr/bin/env python3
"""
Check Fortran files for lines exceeding 132 characters.
Exit with error if any are found.
"""
import sys
from pathlib import Path

def check_fortran_lines(max_length=132):
    """Check all Fortran files for long lines."""
    src_dir = Path('src')
    if not src_dir.exists():
        src_dir = Path('.')
    
    fortran_files = list(src_dir.rglob('*.f95')) + list(src_dir.rglob('*.f90'))
    
    problems = []
    for filepath in fortran_files:
        with open(filepath, 'r') as f:
            lines = f.readlines()
        
        for i, line in enumerate(lines, 1):
            # Skip comments
            if line.lstrip().startswith('!'):
                continue
            
            if len(line.rstrip()) > max_length:
                problems.append(f"{filepath}:{i}: Line has {len(line.rstrip())} characters (max: {max_length})")
    
    if problems:
        print("ERROR: Found Fortran lines exceeding 132 characters:")
        print("=" * 60)
        for problem in problems[:20]:  # Show first 20 problems
            print(problem)
        if len(problems) > 20:
            print(f"... and {len(problems) - 20} more")
        print("=" * 60)
        print("\nTo fix: Run 'make format' or manually break long lines with '&' continuation")
        return 1
    else:
        print("✓ All Fortran lines are within 132 character limit")
        return 0

if __name__ == '__main__':
    sys.exit(check_fortran_lines())