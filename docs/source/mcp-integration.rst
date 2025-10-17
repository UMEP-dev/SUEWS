.. _mcp_integration:

MCP Server Integration
======================

SUEWS includes built-in support for the Model Context Protocol (MCP), enabling AI assistants like Claude to interact with SUEWS simulations through natural language.

Installation
------------

**For AI Users (Recommended)**

Download the MCPB bundle from `GitHub Releases <https://github.com/UMEP-dev/SUEWS/releases>`_:

1. Download ``suews-mcp.mcpb`` from the latest release
2. Double-click the file (or drag to Claude Desktop)
3. First use: automatic setup (30-60 seconds)
4. Ready to use!

The MCPB bundle includes everything needed to run the SUEWS MCP server, including automatic Python environment setup.

**For Python Developers**

Install via pip::

    pip install suews-mcp

Then configure manually in Claude Desktop (see below).

Requirements
~~~~~~~~~~~~

- **For MCPB**: Claude Desktop (macOS or Windows), Internet (first-time setup)
- **For pip**: Python >=3.9, supy >=2025.10.15

Available Tools
---------------

The SUEWS MCP server provides 10 tools organised into three categories:

Configuration Tools
~~~~~~~~~~~~~~~~~~~

- **validate_config**: Validate a SUEWS YAML configuration file using the data model
- **create_config**: Create a new SUEWS configuration file (optionally from a template)
- **get_config_info**: Get information about a configuration file (sites, parameters, etc.)
- **update_config**: Update an existing configuration file

Simulation Tools
~~~~~~~~~~~~~~~~

- **run_simulation**: Run a SUEWS simulation from a configuration file
- **estimate_runtime**: Estimate simulation runtime based on configuration

Analysis Tools
~~~~~~~~~~~~~~

- **load_results**: Load simulation results from file (CSV, NetCDF)
- **compute_statistics**: Compute statistics on simulation results (mean, sum, min, max, std)
- **create_plot**: Create plots from simulation results (timeseries, scatter, histogram)
- **export_results**: Export results to different formats (CSV, JSON, NetCDF)

Claude Desktop Setup
---------------------

MCPB Installation (Recommended)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you installed via MCPB (recommended for most users):

1. Download ``suews-mcp.mcpb`` from `GitHub Releases <https://github.com/UMEP-dev/SUEWS/releases>`_
2. Double-click the file or drag it to Claude Desktop
3. Claude Desktop will automatically install and configure the server
4. Restart Claude Desktop

No manual configuration required!

Manual Installation (pip)
~~~~~~~~~~~~~~~~~~~~~~~~~~

If you installed via pip, add the SUEWS MCP server to your Claude Desktop configuration file.

**Configuration File Location:**

- **macOS**: ``~/Library/Application Support/Claude/claude_desktop_config.json``
- **Windows**: ``%APPDATA%\Claude\claude_desktop_config.json``

**Configuration:**

.. code-block:: json

    {
      "mcpServers": {
        "suews": {
          "command": "suews-mcp"
        }
      }
    }

If using a virtual environment, use the full path:

.. code-block:: json

    {
      "mcpServers": {
        "suews": {
          "command": "/path/to/venv/bin/suews-mcp"
        }
      }
    }

After updating the configuration, restart Claude Desktop.

Verify Installation
~~~~~~~~~~~~~~~~~~~

In Claude Desktop, you should see the SUEWS tools available. Try asking:

    *"What SUEWS MCP tools are available?"*

Claude should list all 10 available tools.

Example Usage
-------------

Once configured, you can interact with SUEWS through natural language:

Validate a Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~

    *"Validate the SUEWS configuration at /path/to/config.yml"*

Claude will use the ``validate_config`` tool to check the configuration and report any errors.

Run a Simulation
~~~~~~~~~~~~~~~~

    *"Run a SUEWS simulation using the configuration at /path/to/config.yml and save results to /path/to/output"*

Claude will execute the simulation and provide a summary of the results.

Analyse Results
~~~~~~~~~~~~~~~

    *"Load the SUEWS results from /path/to/output/results.csv and compute the mean temperature"*

Claude will use the analysis tools to process and summarise the results.

Create Visualisations
~~~~~~~~~~~~~~~~~~~~~~

    *"Create a timeseries plot of temperature and humidity from /path/to/results.csv"*

Claude will generate and save a plot of the requested variables.

Workflow Example
----------------

A typical SUEWS workflow with MCP might look like:

1. **Configuration**: *"Create a new SUEWS configuration based on the template at examples/sample.yml with name 'london_test'"*
2. **Validation**: *"Validate the configuration file I just created"*
3. **Simulation**: *"Run a simulation with this configuration"*
4. **Analysis**: *"Compute daily statistics for energy fluxes from the results"*
5. **Visualisation**: *"Plot the diurnal cycle of sensible heat flux"*

Advanced Usage
--------------

Custom Configuration Updates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can ask Claude to make specific changes to configurations:

    *"Update the configuration to change the timestep to 30 minutes and enable snow modelling"*

Batch Analysis
~~~~~~~~~~~~~~

Process multiple result files:

    *"Load all CSV files in /path/to/results/ and compute monthly averages for each"*

Comparative Analysis
~~~~~~~~~~~~~~~~~~~~

    *"Compare the temperature predictions between /path/to/sim1/results.csv and /path/to/sim2/results.csv"*

Troubleshooting
---------------

Server Not Appearing
~~~~~~~~~~~~~~~~~~~~

**For MCPB users:**

- Ensure you dragged the ``.mcpb`` file to Claude Desktop
- Check Claude Desktop logs (Help → View Logs)
- Try restarting Claude Desktop

**For pip users:**

- Check the configuration file syntax (valid JSON)
- Ensure ``suews-mcp`` is in your PATH or use the full path
- Check Claude Desktop logs (Help → View Logs)

Tool Execution Failures
~~~~~~~~~~~~~~~~~~~~~~~~

- Verify file paths are absolute and accessible
- Check that configuration files are valid YAML
- Ensure sufficient disk space for outputs

Import Errors
~~~~~~~~~~~~~

**For pip users:**

- Verify that ``suews-mcp`` is installed: ``pip list | grep suews-mcp``
- Check that ``supy`` version is >= 2025.10.15
- Try reinstalling: ``pip install --upgrade --force-reinstall suews-mcp``

**For MCPB users:**

- Delete the ``.venv`` folder in the MCPB installation directory
- Reinstall by double-clicking the ``.mcpb`` file again

Technical Details
-----------------

Architecture
~~~~~~~~~~~~

The SUEWS MCP server is implemented as a separate package ``suews-mcp``:

- **Location**: Separate repository at ``mcp/src/suews_mcp/``
- **Server**: Async MCP server using the MCP Python SDK
- **Tools**: Direct access to SuPy internals and data models
- **Validation**: Uses SUEWSConfig Pydantic models
- **Distribution**: MCPB bundle with embedded UV for automatic setup

MCPB Bundle Contents
~~~~~~~~~~~~~~~~~~~~

The MCPB bundle includes:

- Bootstrap script (Node.js) for automatic setup
- UV package manager binaries (macOS arm64/x64, Windows x64)
- MCP server source code
- Manifest file for Claude Desktop integration

On first run, the bootstrap script:

1. Detects platform and architecture
2. Creates a Python virtual environment
3. Installs ``supy`` and dependencies using UV
4. Launches the MCP server

Data Flow
~~~~~~~~~

1. Claude sends tool request to MCP server via stdio
2. MCP server validates input schema
3. Tool function executes using SuPy modules
4. Results formatted as JSON and returned to Claude
5. Claude presents results in natural language

Security Considerations
~~~~~~~~~~~~~~~~~~~~~~~

- The MCP server has full filesystem access in the user's context
- Configuration files are validated before execution
- No network access is required (stdio communication only)
- Simulation outputs are written with user permissions
- MCPB bundles are verified by Claude Desktop before installation

See Also
--------

- :doc:`YAML Configuration Guide <inputs/yaml/index>`
- :doc:`SuPy Python API <api>`
- `Model Context Protocol Documentation <https://modelcontextprotocol.org>`_
