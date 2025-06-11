# Windows CI Build Failure on windows-2022 with setjmp Symbol Issues

## Summary
The Windows CI build was failing consistently on the `windows-2022` GitHub Actions runner due to setjmp symbol conflicts, while builds worked fine on the older `windows-2019` runner. The issue stemmed from cibuildwheel not properly accessing the UCRT64 toolchain installed by the msys2/setup-msys2 action.

## Environment
- **Platform**: Windows Server 2022 (GitHub Actions)
- **Build Tool**: cibuildwheel v2.23.3
- **Compiler**: MinGW-w64 UCRT64 toolchain
- **Python Versions**: 3.9-3.13

## Problem Description
The build was failing with setjmp-related symbol errors during the compilation of Fortran extensions. Investigation revealed that:

1. The msys2/setup-msys2 action was installing the UCRT64 toolchain correctly
2. However, cibuildwheel was running in an isolated environment and couldn't access these tools
3. Instead, cibuildwheel was picking up incorrect compilers from other locations:
   - `C:\mingw64\bin\gcc.exe`
   - `C:\Strawberry\c\bin\gcc.exe`
4. The UCRT64 installation directory was empty within the cibuildwheel environment

## Root Cause
The issue occurred because:
- cibuildwheel creates an isolated build environment
- Environment variables and PATH modifications from the GitHub Actions runner don't propagate into cibuildwheel
- The msys2/setup-msys2 action installs tools outside of cibuildwheel's environment

## Solution
Fixed by installing MSYS2 and the UCRT64 toolchain directly within the cibuildwheel environment:

1. **Removed external MSYS2 setup**: Eliminated the msys2/setup-msys2 action that was running outside cibuildwheel
2. **Added CIBW_BEFORE_ALL_WINDOWS**: Installed MSYS2 via Chocolatey within cibuildwheel
3. **Configured proper toolchain**: Installed specific UCRT64 packages including openblas
4. **Set correct environment variables**: Used proper YAML multiline format for environment configuration
5. **Added static linking flags**: Included `-static-libgcc -static-libgfortran` to avoid runtime conflicts

## Key Changes Made

### Before (broken):
```yaml
- name: Set up UCRT-based MinGW-w64
  if: runner.os == 'Windows'
  uses: msys2/setup-msys2@v2
  # ... toolchain installed outside cibuildwheel

CIBW_ENVIRONMENT_WINDOWS: 'PATH=/c/msys64/ucrt64/bin:$PATH ...'
```

### After (working):
```yaml
CIBW_BEFORE_ALL_WINDOWS: >
  choco install msys2 -y --no-progress &&
  C:\tools\msys64\usr\bin\bash.exe -lc "pacman -Syu --noconfirm" &&
  C:\tools\msys64\usr\bin\bash.exe -lc "pacman -S --needed --noconfirm mingw-w64-ucrt-x86_64-toolchain mingw-w64-ucrt-x86_64-gcc mingw-w64-ucrt-x86_64-gcc-fortran mingw-w64-ucrt-x86_64-openblas"

CIBW_ENVIRONMENT_WINDOWS: >
  PATH="C:\tools\msys64\ucrt64\bin;$PATH"
  CC=gcc
  CXX=g++
  FC=gfortran
  CFLAGS="-D__USE_MINGW_SETJMP_NON_SEH -D__USE_MINGW_ANSI_STDIO=1 -mlong-double-64"
  FCFLAGS="-fno-optimize-sibling-calls"
  LDFLAGS="-lucrt -static-libgcc -static-libgfortran"
```

## Lessons Learned
1. **cibuildwheel isolation**: External tool installations don't carry over into cibuildwheel's environment
2. **YAML formatting**: Environment variables must use proper YAML multiline syntax
3. **Toolchain consistency**: Ensuring the same compiler toolchain is used throughout the build process is critical
4. **Static linking**: Using static linking flags helps avoid runtime library conflicts

## Additional Issues Encountered

### YAML Formatting Problems
Multiple iterations were needed to get the cibuildwheel environment configuration correct:

1. **Multiline string parsing**: Initial attempt with backslash line continuations failed
   ```
   cibuildwheel: Malformed environment option 'PATH=C:\\tools\\msys64\\ucrt64\\bin;$PATH \\ CC=gcc...'
   ```

2. **Environment variable format**: Had to use proper YAML multiline syntax with `>` indicator

### Command Shell Mismatch
The `CIBW_BEFORE_BUILD_WINDOWS` commands were initially written in bash syntax but needed Windows CMD:

**Problem**: Commands like `echo "PATH: $PATH"` were outputting literal strings
**Solution**: Used Windows CMD syntax:
- `%PATH%` instead of `$PATH`
- `dir` instead of `ls`
- `where` instead of `which`
- `&` instead of `&&` for command chaining

## Related Commits
- Fix implemented in branch: `fix-windows-gcc-scipy`
- Initial toolchain fix: `4ef00cc8`
- YAML formatting fix: `abf21099` 
- Environment variable format: `0c7166bf`
- CMD syntax fix: `af556e27`

## Impact
This fix ensures that Windows wheel builds work reliably on modern GitHub Actions runners (windows-2022) and should be more robust against future platform updates. The solution also provides a template for other projects facing similar cibuildwheel toolchain issues on Windows.