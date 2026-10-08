.. _api_command_line:

Command-Line Tools
==================

SUEWS provides command-line tools for common operations without requiring Python scripting.

All tools are subcommands of a single ``suews`` command. Run ``suews --help`` for the list, and ``suews <subcommand> --help`` for the options of each one. Most subcommands accept ``--format json``, which writes the standard SUEWS JSON envelope to stdout (see :doc:`/contributing/json-output-integration`).

.. note::

   The hyphenated commands ``suews-run``, ``suews-convert``, ``suews-validate``, ``suews-schema`` and ``suews-inspect`` are deprecated aliases of ``suews run``, ``suews convert``, ``suews validate``, ``suews schema`` and ``suews inspect``. They still work but print a ``DEPRECATED:`` notice to stderr and will be removed in a future release.

Available Commands
------------------

suews init
~~~~~~~~~~

Create a new case directory from a packaged template. The directory is created if missing; existing config files are not overwritten.

.. code-block:: bash

    suews init my_case
    suews init my_case --template simple-urban

Only the ``simple-urban`` template is shipped at present; the other template names (``multi-site``, ``teaching-demo``, ``spartacus``) are reserved and rejected with an error.

suews run
~~~~~~~~~

Execute SUEWS simulations from the command line with YAML or namelist configuration files.

**YAML Configuration (Recommended)**

.. code-block:: bash

    # Run with YAML configuration file
    suews run config.yml

    # Or specify full path
    suews run /path/to/config.yml

    # Use default config.yml in current directory
    suews run

**Namelist Configuration (Deprecated)**

Legacy namelist format is still supported but deprecated:

.. code-block:: bash

    # Legacy format with deprecation warning
    suews run RunControl.nml

**Migration from Namelist to YAML**

To migrate from the deprecated namelist format to modern YAML:

.. code-block:: bash

    # Step 1: Convert namelist to YAML
    suews convert -i RunControl.nml -o config.yml

    # Step 2: Run with YAML configuration
    suews run config.yml

**Format Auto-Detection**

The tool automatically detects the configuration format based on file extension:

- ``.yml``, ``.yaml`` -> YAML format (modern, recommended)
- ``.nml`` -> Namelist format (legacy, shows deprecation warning)

For detailed usage and examples, see the :doc:`/workflow` guide.

suews convert
~~~~~~~~~~~~~

Convert a legacy table set (``RunControl.nml``), a df_state snapshot (``.csv``/``.pkl``) or an older YAML configuration into a current-schema YAML file.

.. code-block:: bash

    suews convert -i RunControl.nml -o config.yml

**Documentation**:

- **CLI usage**: See :doc:`/inputs/converter` for command-line options
- **Python API**: See :doc:`converter` for programmatic usage

suews validate
~~~~~~~~~~~~~~

Validate SUEWS YAML configuration files against the schema and run the validation pipeline.

.. code-block:: bash

    suews validate config.yml

See :doc:`/inputs/yaml/validation` for the pipeline phases and options.

suews inspect
~~~~~~~~~~~~~

Show a compact, read-only overview of a YAML configuration: per-site coordinates, surface-cover fractions and a forcing file summary.

.. code-block:: bash

    suews inspect config.yml
    suews inspect config.yml --format json

suews summarise
~~~~~~~~~~~~~~~

Print a per-variable summary (mean, minimum, maximum and percentage of missing values) of the output in a run directory.

.. code-block:: bash

    suews summarise path/to/run_dir
    suews summarise path/to/run_dir --variables QH,QE,QN

suews compare
~~~~~~~~~~~~~

Compare two run directories, or a run directory and an observations CSV file, by computing per-variable RMSE, bias and Pearson correlation over their shared timestamps.

.. code-block:: bash

    suews compare run_a run_b
    suews compare run_a observations.csv --variables QH,QE --metrics rmse,bias

Use ``--grid`` to choose a grid when an input holds several, and ``--align positional`` for inputs without a recoverable time axis.

suews diagnose
~~~~~~~~~~~~~~

Run a battery of checks on a run directory: provenance present, output files present, proportion of missing values in QH, QE and QN, and energy-balance closure.

.. code-block:: bash

    suews diagnose path/to/run_dir
    suews diagnose path/to/run_dir --format json

suews schema
~~~~~~~~~~~~

Display, check, migrate and export the SUEWS configuration schema (subcommands ``info``, ``version``, ``migrate`` and ``export``).

.. code-block:: bash

    suews schema --help

See :doc:`/contributing/schema/schema_cli` for details.

suews knowledge
~~~~~~~~~~~~~~~

Query the packaged source-evidence knowledge pack. The pack is generated from
the Git checkout used to build the installed SuPy wheel and carries citations
back to the exact repository paths and line spans.

.. code-block:: bash

    suews knowledge manifest --format json
    suews knowledge query "How is runoff routed?" --format json

See :doc:`knowledge-pack` for the pack format and source policy.

Related Documentation
---------------------

- :doc:`python-cli-equivalents` - Python API equivalents for all CLI commands
- :doc:`/workflow` - Complete workflow guide
- :doc:`/inputs/converter` - Configuration converter guide
- :doc:`/inputs/yaml/index` - YAML configuration reference
