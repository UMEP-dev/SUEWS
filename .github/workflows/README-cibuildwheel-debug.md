# cibuildwheel Debug Workflow

This workflow provides an interactive debugging environment for troubleshooting cibuildwheel build issues.

## Features

- **Manual dispatch** with customizable parameters
- **SSH access** to the build environment
- **Claude Code CLI** pre-installed for AI-assisted debugging
- **Verbose logging** and artifact collection
- **Platform-specific debugging** for Linux, macOS, and Windows

## Usage

### 1. Trigger the Workflow

1. Go to the [Actions tab](../../actions) in GitHub
2. Select "Debug cibuildwheel with SSH" from the workflow list
3. Click "Run workflow"
4. Configure parameters:
   - **Platform**: Choose the OS to debug on
   - **Python version**: Select the Python version (cp39-cp313)
   - **Architecture**: Choose the target architecture
   - **Debug mode**: Select when to enable SSH access

### 2. Connect via SSH

When the workflow reaches a debug point, it will display SSH connection instructions:

```bash
# Example connection command (will be shown in workflow logs)
ssh -i <key> runner@<host>
```

### 3. Debug with Claude Code

Once connected via SSH, you can use Claude Code to help debug:

```bash
# Check Claude Code is available
claude --version

# Get help debugging build errors
claude -p "I'm debugging a cibuildwheel build failure. Here's the error: [paste error]"

# Analyze build logs
claude -p "analyze this build log and suggest fixes" < cibuildwheel.log

# Interactive debugging session
claude
```

### 4. Common Debugging Commands

```bash
# Check environment
env | grep CIBW
python --version
gcc --version

# Examine build directory
ls -la
find . -name "*.log"

# Test compilation manually
cd src/suews
gfortran -c *.f95

# Check Python environment
pip list
python -c "import sys; print(sys.path)"
```

## Debug Modes

- **before-build**: SSH session before cibuildwheel starts (useful for environment setup issues)
- **after-failure**: SSH session only if build fails (default for debugging failures)
- **always**: SSH sessions both before and after build
- **disabled**: No SSH sessions (just run with verbose logging)

## Platform-Specific Notes

### Linux (manylinux)
- Uses manylinux_2_28 image
- Check gfortran availability in container

### macOS
- Separate workflows for Intel (macos-13) and Apple Silicon (macos-latest)
- Homebrew gfortran installation included

### Windows
- Uses MSYS2/MinGW-w64 toolchain
- Special handling for setjmp compatibility
- Check paths and compiler locations

## Artifacts

The workflow uploads several artifacts:
- **debug-logs-***: Build logs and output
- **wheels-***: Successfully built wheels (if any)

## Tips

1. **Use verbose mode**: The workflow sets `CIBW_BUILD_VERBOSITY: 3` for detailed output
2. **Check compiler paths**: Especially important on Windows
3. **Test manually**: Try building without cibuildwheel to isolate issues
4. **Compare with CI**: Check successful builds in main workflow for differences