.. _transition_guide:

Transitioning to YAML-based Configuration
=========================================

As of 2025, SUEWS has adopted a new YAML-based format for input files to enhance readability, maintainability, and user experience. To help users migrate their existing table-based input files to this new format, a transition tool is provided.

This guide explains how to use the ``to_yaml`` command-line tool to automate the conversion process.

The transition process involves two main steps, which are handled automatically by the tool:

1.  **Table Version Update (Optional)**: If you are using input files from an older version of SUEWS, the tool first converts them to the latest available table-based format.
2.  **Conversion to YAML**: The tool then reads the complete set of (updated) table-based inputs and converts them into a single, comprehensive ``config_suews.yml`` file.

Prerequisites
-------------

Ensure that ``supy`` is installed in your Python environment. The transition tool is part of the ``supy`` package.

Using the Transition Tool
-------------------------

The transition tool, ``to_yaml.py``, is a command-line script. You can run it as a Python module.

.. code-block:: bash

   python -m supy.cmd.to_yaml [OPTIONS]

Command-Line Options
~~~~~~~~~~~~~~~~~~~~

The tool accepts the following arguments:

*   ``-i, --input-dir PATH``: **(Required)** The directory containing your existing SUEWS table-based input files. This directory must include a ``RunControl.nml`` file.
*   ``-o, --output-file PATH``: **(Required)** The full path for the output YAML file that will be created (e.g., ``/path/to/my/project/config_suews.yml``).
*   ``-f, --from-ver TEXT``: (Optional) The version of your source input files (e.g., ``2020a``). If specified, the tool will first convert the tables to the latest supported version before creating the YAML file.

Examples
--------

Example 1: Converting Up-to-Date Tables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If your table-based input files are already up-to-date (i.e. after v2025a), you can convert them directly to YAML without specifying version numbers.

Suppose your input files are located in a directory called ``/path/to/suews_run_london/Input``.

.. code-block:: bash

   python -m supy.cmd.to_yaml \
       --input-dir /path/to/suews_run_london/Input \
       --output-file /path/to/suews_run_london/config_london.yml

The tool will read the files from the input directory and create ``config_london.yml`` in the specified location.

Example 2: Converting Older Tables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have input files from an older SUEWS version (e.g., v2019b), you can specify the source version, and the tool will automatically convert them to the latest available table format before generating the YAML file.

.. code-block:: bash

   python -m supy.cmd.to_yaml \
       --input-dir /path/to/old_suews_run/Input_v2019b \
       --output-file /path/to/old_suews_run/config_new.yml \
       --from-ver 2019b

The tool will first run the table converter to update the files from ``v2019b`` to the latest version in a temporary location, and then use these updated files to generate ``config_new.yml``.