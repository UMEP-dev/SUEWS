.. _api_command_line:

Command-Line Tools
==================

SUEWS provides command-line tools for common operations without requiring Python scripting.

Available Commands
------------------

suews-run
~~~~~~~~~

Execute SUEWS simulations from the command line with YAML or namelist configuration files.

**YAML Configuration (Recommended)**

.. code-block:: bash

    # Run with YAML configuration file
    suews-run config.yml

    # Or specify full path
    suews-run /path/to/config.yml

    # Use default config.yml in current directory
    suews-run

**Namelist Configuration (Deprecated)**

Legacy namelist format is still supported but deprecated:

.. code-block:: bash

    # Legacy format with deprecation warning
    suews-run -p RunControl.nml

**Migration from Namelist to YAML**

To migrate from the deprecated namelist format to modern YAML:

.. code-block:: bash

    # Step 1: Convert namelist to YAML
    suews-convert -i RunControl.nml -o config.yml

    # Step 2: Run with YAML configuration
    suews-run config.yml

**Format Auto-Detection**

The tool automatically detects the configuration format based on file extension:

- ``.yml``, ``.yaml`` → YAML format (modern, recommended)
- ``.nml`` → Namelist format (legacy, shows deprecation warning)

For detailed usage and examples, see the :doc:`/workflow` guide.

suews-convert
~~~~~~~~~~~~~

Convert between SUEWS input formats and versions.

.. code-block:: bash

    suews-convert -i input_dir -o output.yml

**Documentation**:

- **CLI usage**: See :doc:`/inputs/converter` for command-line options
- **Python API**: See :doc:`converter` for programmatic usage

suews-validate
~~~~~~~~~~~~~~

Validate SUEWS YAML configuration files against the schema.

.. code-block:: bash

    suews-validate config.yml

suews-schema
~~~~~~~~~~~~

Display and export SUEWS configuration schema for validation and tooling.

.. code-block:: bash

    suews-schema --help

Related Documentation
---------------------

- :doc:`python-cli-equivalents` - Python API equivalents for all CLI commands
- :doc:`/workflow` - Complete workflow guide
- :doc:`/inputs/converter` - Configuration converter guide
- :doc:`/inputs/yaml/index` - YAML configuration reference
