.. _api_command_line:

Command-Line Tools
==================

SUEWS provides command-line tools for common operations without requiring Python scripting.

Available Commands
------------------

suews-run
~~~~~~~~~

Execute SUEWS simulations from the command line with YAML configuration files.

.. code-block:: bash

    suews-run config.yml

For detailed usage, see the :doc:`/workflow` guide.

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
