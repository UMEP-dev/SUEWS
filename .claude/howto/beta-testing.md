# Beta Testing with Development Versions of SuPy

This guide helps beta testers install and use development versions of SuPy from test.pypi.org.

## Quick Start with `uv`

The fastest way to test development versions is using `uv` (10-100x faster than pip):

```bash
# Install uv
curl -LsSf https://astral.sh/uv/install.sh | sh

# Create environment
uv venv .venv
source .venv/bin/activate  # On Windows: .venv\Scripts\activate

# Install development version
uv pip install --index-url https://test.pypi.org/simple/ \
              --extra-index-url https://pypi.org/simple/ \
              supy
```

## Why Use `uv` for Beta Testing?

The error shown in issue #652 occurs because:
- Test PyPI doesn't mirror all dependencies
- pip struggles with complex dependency resolution from multiple indices
- Some packages have different names on test.pypi.org

`uv` solves these issues with:
- Better dependency resolution algorithms
- Efficient handling of multiple package indices
- Faster installation (seconds vs minutes)

## Detailed Installation Steps

### 1. Install uv

```bash
# macOS/Linux
curl -LsSf https://astral.sh/uv/install.sh | sh

# Windows (PowerShell)
irm https://astral.sh/uv/install.ps1 | iex
```

### 2. Create Isolated Environment

```bash
# Create new environment for beta testing
uv venv .venv-beta

# Activate environment
# macOS/Linux:
source .venv-beta/bin/activate
# Windows:
.venv-beta\Scripts\activate
```

### 3. Install Development Version

```bash
# Install with proper index configuration
uv pip install --index-url https://test.pypi.org/simple/ \
              --extra-index-url https://pypi.org/simple/ \
              supy
```

The `--extra-index-url` ensures dependencies not on test.pypi.org are fetched from the main PyPI.

### 4. Verify Installation

```python
import supy as sp
print(sp.__version__)  # Should show dev version like 2025.9.16.dev0
```

## Switching Between Versions

### Install Specific Dev Version

```bash
uv pip install --index-url https://test.pypi.org/simple/ \
              --extra-index-url https://pypi.org/simple/ \
              supy==2025.9.16.dev0
```

### Switch to Stable Version

```bash
uv pip uninstall supy
uv pip install supy  # Installs from main PyPI
```

## Common Issues

### Issue: Package Not Found

If you see errors about missing packages:
```bash
# Use --extra-index-url to fetch from main PyPI
uv pip install --index-url https://test.pypi.org/simple/ \
              --extra-index-url https://pypi.org/simple/ \
              supy
```

### Issue: Wrong Version Installed

Always check the version after installation:
```python
import supy as sp
print(f"SuPy version: {sp.__version__}")
```

### Issue: Import Errors

Development versions may have different dependencies:
```bash
# Install all optional dependencies
uv pip install --index-url https://test.pypi.org/simple/ \
              --extra-index-url https://pypi.org/simple/ \
              "supy[all]"
```

## Reporting Beta Issues

When reporting issues with development versions:

1. **Include Version Info**
   ```python
   import supy as sp
   import sys
   print(f"SuPy: {sp.__version__}")
   print(f"Python: {sys.version}")
   ```

2. **Describe Installation Method**
   - Which indices were used
   - Any special flags or options

3. **Include Full Error Messages**
   - Complete stack traces
   - Import errors
   - Dependency conflicts

4. **Create GitHub Issue**
   - Use label: `beta-testing`
   - Reference the dev version number
   - Link to test.pypi.org package page

## Automated Setup Script

Use the provided script for quick setup:
```bash
bash scripts/setup-beta.sh
```

This script:
- Installs uv if not present
- Creates isolated environment
- Installs latest dev version
- Verifies installation

## See Also

- User documentation: `docs/source/installation/beta-testing.rst`
- Package versions: https://test.pypi.org/project/supy/
- GitHub issues: https://github.com/UMEP-dev/SUEWS/issues