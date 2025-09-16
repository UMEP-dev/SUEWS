# Installing SUEWS MCP Server in Claude Desktop

This guide walks you through installing and configuring the SUEWS MCP server for use with Claude Desktop.

## Prerequisites

1. **Claude Desktop** installed (version 0.7.0 or later)
2. **Python 3.9+** installed on your system
3. **Git** installed (for cloning the repository)

## Installation Steps

### Step 1: Clone and Install SUEWS MCP

```bash
# 1. Clone the SUEWS repository
git clone https://github.com/UMEP-dev/SUEWS.git
cd SUEWS

# 2. Check out the feature branch (until merged to master)
git checkout feature/suews-mcp
cd suews-mcp

# 3. Create a virtual environment (recommended)
python3 -m venv suews-mcp-env
source suews-mcp-env/bin/activate  # On Windows: suews-mcp-env\Scripts\activate

# 4. Install the package
pip install -e .

# 5. Install SuPy (if not already installed)
pip install supy

# 6. Verify installation
python -c "import suews_mcp; print('✓ SUEWS MCP installed')"
```

### Step 2: Configure Claude Desktop

1. **Open Claude Desktop settings**:
   - On macOS: Claude Desktop → Settings → Developer → Edit Config
   - On Windows: File → Settings → Developer → Edit Config
   - On Linux: Edit → Preferences → Developer → Edit Config

2. **Add the SUEWS MCP server configuration**:

```json
{
  "mcpServers": {
    "suews": {
      "command": "python",
      "args": ["-m", "suews_mcp.server"],
      "cwd": "/path/to/SUEWS/suews-mcp",
      "env": {
        "PYTHONPATH": "/path/to/SUEWS/suews-mcp/src",
        "VIRTUAL_ENV": "/path/to/SUEWS/suews-mcp/suews-mcp-env"
      }
    }
  }
}
```

**Important**: Replace `/path/to/SUEWS` with your actual path, for example:
- macOS/Linux: `/Users/username/projects/SUEWS`
- Windows: `C:\\Users\\username\\projects\\SUEWS`

### Step 3: Alternative Configuration (using activation script)

If the virtual environment doesn't activate properly, create a wrapper script:

**On macOS/Linux** - Create `run-suews-mcp.sh`:
```bash
#!/bin/bash
source /path/to/SUEWS/suews-mcp/suews-mcp-env/bin/activate
python -m suews_mcp.server
```

Make it executable:
```bash
chmod +x run-suews-mcp.sh
```

**On Windows** - Create `run-suews-mcp.bat`:
```batch
@echo off
call C:\path\to\SUEWS\suews-mcp\suews-mcp-env\Scripts\activate
python -m suews_mcp.server
```

Then update Claude Desktop config to use the wrapper:

```json
{
  "mcpServers": {
    "suews": {
      "command": "/path/to/run-suews-mcp.sh",  // or .bat on Windows
      "args": [],
      "cwd": "/path/to/SUEWS/suews-mcp"
    }
  }
}
```

### Step 4: Test the Installation

1. **Restart Claude Desktop** after saving the configuration

2. **Open a new conversation** and test with:
   ```
   Can you list the available SUEWS tools?
   ```

3. **Expected response**: Claude should list the available SUEWS MCP tools like:
   - `run_suews_simulation`
   - `validate_suews_config`
   - `analyze_suews_output`
   - `preprocess_forcing`
   - etc.

## Troubleshooting

### Issue: "MCP server not found"

**Solution**: Check the path in your config file:
```bash
# Verify the path exists
ls -la /path/to/SUEWS/suews-mcp/src/suews_mcp/server.py
```

### Issue: "ImportError: No module named suews_mcp"

**Solution**: Ensure the virtual environment is activated:
```bash
# Re-install in the virtual environment
cd /path/to/SUEWS/suews-mcp
source suews-mcp-env/bin/activate
pip install -e .
```

### Issue: "SuPy not found"

**Solution**: Install SuPy in the virtual environment:
```bash
source suews-mcp-env/bin/activate
pip install supy
```

### Issue: Server starts but tools don't work

**Solution**: Check server logs:
1. Enable debug mode in the config:
```json
{
  "mcpServers": {
    "suews": {
      "command": "python",
      "args": ["-m", "suews_mcp.server", "--debug"],
      "env": {
        "SUEWS_MCP_DEBUG": "true"
      }
    }
  }
}
```

2. Check Claude Desktop logs:
   - macOS: `~/Library/Logs/Claude/`
   - Windows: `%APPDATA%\Claude\logs\`
   - Linux: `~/.config/Claude/logs/`

## Usage Examples

Once installed, you can use SUEWS MCP in Claude Desktop:

### Example 1: Run a Simple Simulation
```
Can you help me run a SUEWS simulation for a residential area in London with default parameters?
```

### Example 2: Validate Configuration
```
I have a SUEWS configuration file. Can you check if it's valid and identify any issues?
```

### Example 3: Analyze Results
```
Can you analyze the energy balance from my SUEWS simulation output and identify any problems?
```

### Example 4: Preprocess Data
```
I have meteorological data in CSV format. Can you help me prepare it for SUEWS?
```

## Development Mode

For development and testing:

1. **Install development dependencies**:
```bash
cd /path/to/SUEWS/suews-mcp
source suews-mcp-env/bin/activate
pip install -e ".[dev,test]"
```

2. **Run tests**:
```bash
pytest tests/
```

3. **Enable verbose logging** in Claude Desktop config:
```json
{
  "mcpServers": {
    "suews": {
      "command": "python",
      "args": ["-m", "suews_mcp.server", "--verbose"],
      "env": {
        "SUEWS_MCP_LOG_LEVEL": "DEBUG"
      }
    }
  }
}
```

## Next Steps

After successful installation:

1. **Explore templates**: Ask Claude to show available SUEWS configuration templates
2. **Try the quickstart**: Request a guided walkthrough of running your first simulation
3. **Read documentation**: Ask for the API reference or specific tool documentation
4. **Run examples**: Try the urban heat island or parameter sensitivity examples

## Support

- **GitHub Issues**: https://github.com/UMEP-dev/SUEWS/issues
- **Documentation**: Available through Claude by asking "Show me the SUEWS MCP documentation"
- **Examples**: Ask Claude for "SUEWS MCP examples" to see practical workflows

## Version Compatibility

- Claude Desktop: 0.7.0+
- Python: 3.9-3.12
- SuPy: Latest version
- MCP Protocol: 2024-11-05

---

Remember to restart Claude Desktop after any configuration changes!