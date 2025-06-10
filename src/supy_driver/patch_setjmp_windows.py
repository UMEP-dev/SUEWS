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
    """Fix setjmp issues in f2py-generated C files for MinGW on Windows."""
    with open(input_file, 'r') as f:
        content = f.read()
    
    # The core issue is that f2py generates code that uses setjmp, which
    # on Windows x64 with certain headers expands to _setjmpex (MSVC-specific).
    # We need to provide a working implementation for MinGW.
    
    # First, find where setjmp.h is included and add our fix before it
    setjmp_include_pos = content.find('#include <setjmp.h>')
    
    if setjmp_include_pos != -1:
        # Insert our compatibility layer before the setjmp.h include
        setjmp_fix = """
/* Fix for MinGW setjmp issues with f2py on Windows x64 */
#if defined(_WIN64) && defined(__MINGW32__)
  /* Define this to use the two-argument setjmp */
  #define __USE_MINGW_SETJMP_TWO_ARGS 1
#endif

"""
        content = (content[:setjmp_include_pos] + 
                  setjmp_fix + 
                  content[setjmp_include_pos:])
    
    # Also add a fallback definition for _setjmpex after all includes
    # Find a good position after includes but before function definitions
    last_include = content.rfind('#include')
    if last_include != -1:
        # Find the end of the last include line
        end_of_line = content.find('\n', last_include)
        if end_of_line != -1:
            setjmpex_fix = """

/* Provide _setjmpex compatibility for MinGW if needed */
#if defined(_WIN64) && defined(__MINGW32__) && !defined(_setjmpex)
  #define _setjmpex(buf) _setjmp(buf)
#endif

"""
            content = (content[:end_of_line + 1] + 
                      setjmpex_fix + 
                      content[end_of_line + 1:])
    
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