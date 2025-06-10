#!/usr/bin/env python
"""
Patch f2py-generated C files to fix _setjmpex issue on Windows x64 with MinGW.

This is a standard workaround for the _setjmpex undefined reference issue
when building f2py modules with MinGW-w64 on 64-bit Windows.

The issue occurs because:
1. f2py generates code that includes <setjmp.h>
2. On Windows x64 with MinGW, setjmp.h defines setjmp as a macro that expands to _setjmpex
3. But MinGW doesn't provide _setjmpex, it's MSVC-specific
4. The solution is to add a compatibility layer before including setjmp.h

This approach is based on solutions used by numpy and other scientific Python packages.
"""

import sys
import platform
import shutil
from pathlib import Path


def patch_setjmp_header(input_file, output_file):
    """Add setjmp compatibility layer for MinGW in f2py-generated C files."""
    with open(input_file, 'r') as f:
        content = f.read()
    
    # Add compatibility definitions that redirect _setjmpex to __builtin_setjmp
    setjmp_compat = """
/* MinGW setjmp compatibility for f2py on Windows x64 */
#ifdef _WIN64
#ifdef __MINGW64__
/* Redirect _setjmpex to GCC's builtin setjmp */
#define _setjmpex __builtin_setjmp
/* Also ensure regular setjmp uses builtin */
#define setjmp(x) __builtin_setjmp(x)
#define longjmp(x,y) __builtin_longjmp(x,y)
#endif
#endif

"""
    
    # Insert the compatibility code before the first include
    if '#include' in content:
        first_include_pos = content.find('#include')
        content = content[:first_include_pos] + setjmp_compat + content[first_include_pos:]
    
    with open(output_file, 'w') as f:
        f.write(content)


def main():
    if len(sys.argv) != 3:
        print("Usage: patch_setjmp_windows.py <input_c_file> <output_c_file>")
        sys.exit(1)
    
    input_file = Path(sys.argv[1])
    output_file = Path(sys.argv[2])
    
    # Only apply patch on Windows x64
    if platform.system() == 'Windows' and platform.machine() in ['AMD64', 'x86_64']:
        print(f"Patching {input_file} for Windows x64...")
        patch_setjmp_header(input_file, output_file)
        print(f"Patched file written to {output_file}")
    else:
        # On other platforms, just copy the file
        shutil.copy2(input_file, output_file)
        print(f"No patching needed, copied {input_file} to {output_file}")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())