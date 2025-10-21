# Isolated MCP Testing

## Critical Issue Identified

**Date**: 2025-01-21

### The Problem

Current testing setup installs MCP in editable mode within the SUEWS development tree:
```bash
cd mcp
uv pip install -e .    # Editable install
```

**This gives unrealistic access to**:
- Development source code (`../../src/supy/`)
- Uncommitted files
- Development-only resources
- The entire repository structure

**But real users install from**:
- PyPI: `pip install suews-mcp` (isolated)
- MCPB: `suews-mcp.mcpb` (isolated)

They get **only** the packaged code + declared dependencies.

### The Risk

Tests might pass in development but **fail for real users** because:
1. MCP code accidentally imports from dev tree
2. Hardcoded paths that only exist in dev
3. Dependencies on files not included in package
4. Missing package_data declarations

## Proper Testing Strategy

### Level 1: Development Testing (Current)

**Location**: `/path/to/suews/mcp/.venv/`

**Installation**:
```bash
cd mcp
uv venv
source .venv/bin/activate
uv pip install -e .
```

**Purpose**: Rapid iteration during development

**Limitations**:
- ⚠️ Has access to development tree
- ⚠️ Not representative of user experience
- ⚠️ Can't catch packaging issues

**Use for**: Quick development cycles only

---

### Level 2: Isolated Package Testing (REQUIRED)

**Location**: Separate directory, no SUEWS source access

**Setup**:
```bash
# Create isolated test environment OUTSIDE the SUEWS repo
mkdir -p ~/mcp-testing-isolated
cd ~/mcp-testing-isolated

# Create fresh environment
uv venv
source .venv/bin/activate

# Install from built package (not editable!)
cd /path/to/suews/mcp
python -m build  # Creates dist/suews_mcp-1.0.0-py3-none-any.whl

cd ~/mcp-testing-isolated
uv pip install /path/to/suews/mcp/dist/suews_mcp-1.0.0-py3-none-any.whl

# Verify installation
which suews-mcp
# Should be: ~/mcp-testing-isolated/.venv/bin/suews-mcp
```

**Critical checks**:
```bash
# Check it can't access dev tree
python -c "import sys; print([p for p in sys.path if 'suews' in p])"
# Should NOT show development paths

# Check only packaged dependencies
pip list
# Should show: suews-mcp, supy, mcp, (and their deps)
# Should NOT show: dev packages

# Test MCP server
echo '{"jsonrpc":"2.0","id":1,"method":"initialize",...}' | suews-mcp
# Should work without dev tree access
```

**Purpose**: Validate packaging is correct

**Use for**:
- Pre-release testing
- CI/CD pipelines
- Reproducing user issues

---

### Level 3: MCPB Testing (User Simulation)

**Location**: Clean system or VM

**Setup**:
```bash
# Build MCPB package
cd /path/to/suews/mcp
# (Use Claude Code to build .mcpb)

# On clean system (or new user account):
# 1. Install Claude Desktop
# 2. Double-click suews-mcp.mcpb
# 3. Test via Claude Desktop interface
```

**Purpose**: Exactly mimics user experience

**Use for**: Final validation before release

---

## Testing Checklist

### Before Each Release

- [ ] **Level 1: Development tests pass**
  ```bash
  cd mcp
  source .venv/bin/activate
  pytest tests/
  python scripts/test_mcp_local.py
  ```

- [ ] **Level 2: Build package**
  ```bash
  cd mcp
  rm -rf dist/ build/
  python -m build
  ```

- [ ] **Level 2: Isolated installation**
  ```bash
  # In separate directory
  uv venv
  source .venv/bin/activate
  uv pip install /path/to/dist/suews_mcp-*.whl
  ```

- [ ] **Level 2: Verify isolation**
  ```bash
  # Should NOT have access to dev tree
  python -c "import suews_mcp; print(suews_mcp.__file__)"
  # Should point to .venv, not dev tree
  ```

- [ ] **Level 2: Test all tools**
  ```bash
  # Re-run all 16 MCP tool tests
  # Without dev tree access!
  ```

- [ ] **Level 2: Check for import errors**
  ```bash
  python -c "from suews_mcp.tools import simulate"
  # Should work without relative imports to dev tree
  ```

- [ ] **Level 3: MCPB packaging**
  ```bash
  # Build .mcpb
  # Test on clean system
  ```

- [ ] **Level 3: End-to-end user workflow**
  ```bash
  # Via Claude Desktop
  # Test all 10 use cases from USE_CASES.md
  ```

---

## Common Packaging Issues to Check

### Issue 1: Missing package_data

**Symptom**: ImportError or FileNotFoundError in isolated install

**Check**: `pyproject.toml`
```toml
[tool.setuptools.package-data]
suews_mcp = [
    "data/*.json",           # Are all data files listed?
    "physics_code/*.f95",    # Are physics codes included?
]
```

**Test**:
```python
import suews_mcp
import pkg_resources
# Check if data files are accessible
```

---

### Issue 2: Hardcoded paths

**Symptom**: FileNotFoundError for development paths

**Bad code**:
```python
# In suews_mcp/tools/knowledge.py
PHYSICS_DIR = "../../src/suews/src/"  # ❌ Assumes dev tree
```

**Good code**:
```python
# Use pkg_resources or importlib.resources
from importlib import resources
PHYSICS_DIR = resources.files('suews_mcp') / 'physics_code'
```

**Test**: Grep for hardcoded paths
```bash
cd mcp/src
grep -r "\.\./\.\." .
grep -r "src/suews" .
```

---

### Issue 3: Import from supy internals

**Bad**:
```python
from supy.src.suews_driver import run_suews  # ❌ Internal API
```

**Good**:
```python
from supy import run_supy  # ✅ Public API
```

**Test**:
```bash
cd mcp/src
grep -r "from supy\." . | grep -v "from supy import"
# Should find no internal imports
```

---

### Issue 4: Missing dependencies

**Symptom**: ImportError for packages used but not declared

**Check**: `pyproject.toml`
```toml
dependencies = [
    "supy>=2025.10.15",
    "mcp>=0.9.0",
    # All other deps listed?
]
```

**Test**: In isolated env
```bash
pip list
# Compare with imports in code
```

---

## Automated Testing Setup

Create `.github/workflows/test-package.yml`:

```yaml
name: Test Package Isolation

on: [push, pull_request]

jobs:
  test-isolated:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Build package
        run: |
          cd mcp
          python -m build

      - name: Create isolated environment
        run: |
          mkdir ~/isolated-test
          cd ~/isolated-test
          python -m venv .venv
          source .venv/bin/activate

      - name: Install from wheel (NOT editable)
        run: |
          source ~/isolated-test/.venv/bin/activate
          pip install mcp/dist/*.whl

      - name: Verify isolation
        run: |
          source ~/isolated-test/.venv/bin/activate
          python -c "
          import sys
          import suews_mcp
          # Check no dev paths in sys.path
          dev_paths = [p for p in sys.path if 'suews' in p and 'site-packages' not in p]
          assert len(dev_paths) == 0, f'Dev paths found: {dev_paths}'
          "

      - name: Run MCP tests
        run: |
          source ~/isolated-test/.venv/bin/activate
          # Run all tool tests
          pytest mcp/tests/ --import-mode=importlib
```

---

## Quick Reference

### Development (Fast iteration)
```bash
cd mcp && source .venv/bin/activate
# Edit code, test immediately
```

### Pre-commit (Required!)
```bash
cd mcp
python -m build
cd ~/isolated-test
pip install --force-reinstall /path/to/mcp/dist/*.whl
# Test all tools
```

### Pre-release (Required!)
```bash
# Full isolated testing
# MCPB testing on clean system
# All 10 use cases verified
```

---

## Recovery from Current State

Since we've been testing in editable mode, we need to:

1. **Build the package**:
   ```bash
   cd mcp
   python -m build
   ```

2. **Create isolated test environment**:
   ```bash
   mkdir ~/suews-mcp-isolated-test
   cd ~/suews-mcp-isolated-test
   uv venv
   source .venv/bin/activate
   ```

3. **Install from wheel**:
   ```bash
   uv pip install /path/to/suews/mcp/dist/suews_mcp-1.0.0-py3-none-any.whl
   ```

4. **Re-run all 16 tool tests**:
   ```bash
   # Test with NO access to dev tree
   # Document any failures
   ```

5. **Fix packaging issues**:
   ```bash
   # Update pyproject.toml
   # Fix import statements
   # Add missing package_data
   ```

6. **Rebuild and retest**:
   ```bash
   # Iterate until all tests pass in isolation
   ```

---

## Summary

**Never trust tests run in editable mode for MCP servers!**

Always validate with:
1. ✅ Build package (`python -m build`)
2. ✅ Install in isolated environment
3. ✅ Test without dev tree access
4. ✅ Verify packaging completeness

This is the **only** way to ensure real users won't hit issues.
