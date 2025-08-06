.. _transition_guide:

Transitioning to YAML-based Configuration
=========================================

As of 2025, SUEWS has adopted a new YAML-based format for input files to enhance readability, maintainability, and user experience. To help users migrate their existing table-based input files to this new format, a transition tool is provided.

This guide explains how to use the ``suews-convert`` command-line tool to automate the conversion process.

The ``suews-convert`` tool automatically determines the appropriate conversion based on the target version:

- **Versions before 2025** (e.g., 2024a): Performs table-to-table conversion
- **Version 2025a or later**: Converts to YAML format

When converting to YAML (2025+), the process involves two main steps:

1.  **Table Version Update (if needed)**: If you are using input files from an older version of SUEWS, the tool first converts them to the latest available table-based format.
2.  **Conversion to YAML**: The tool then reads the complete set of (updated) table-based inputs and converts them into a single, comprehensive YAML file.

Prerequisites
-------------

Ensure that ``supy`` is installed in your Python environment. The transition tool is part of the ``supy`` package.

Using the Transition Tool
-------------------------

The ``suews-convert`` command is installed with the ``supy`` package and can be run directly from the command line.

.. code-block:: bash

   suews-convert [OPTIONS]

Command-Line Options
~~~~~~~~~~~~~~~~~~~~

The tool accepts the following arguments:

*   ``-f, --from VERSION``: **(Required)** The version of your source input files (e.g., ``2020a``, ``2024a``).
*   ``-t, --to VERSION``: **(Required)** The target version. Use ``2025a`` or later versions to convert to YAML format.
*   ``-i, --input PATH``: **(Required)** The directory containing your existing SUEWS table-based input files. This directory must include a ``RunControl.nml`` file.
*   ``-o, --output PATH``: **(Required)** The output path:
    - For table conversion (pre-2025): Directory for the converted tables
    - For YAML conversion (2025+): Path for the output YAML file

Examples
--------

Example 1: Converting to YAML (2025a)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To convert table-based input files to the modern YAML format, use version ``2025a`` or later as the target.

Suppose your input files are from version ``2024a`` and located in ``/path/to/suews_run_london/Input``:

.. code-block:: bash

   suews-convert \
       -f 2024a \
       -t 2025a \
       -i /path/to/suews_run_london/Input \
       -o /path/to/suews_run_london/config_london.yml

The tool will read the files from the input directory and create ``config_london.yml`` in the specified location.

Example 2: Converting Older Tables to YAML
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have input files from an older SUEWS version (e.g., ``2019b``), you can convert them directly to YAML:

.. code-block:: bash

   suews-convert \
       -f 2019b \
       -t 2025a \
       -i /path/to/old_suews_run/Input_v2019b \
       -o /path/to/old_suews_run/config_new.yml

The tool will first update the tables from ``2019b`` to the latest table format in a temporary location, and then convert these to YAML.

Example 3: Table-to-Table Conversion (Pre-2025)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For converting between table versions without creating YAML, use a target version before 2025:

.. code-block:: bash

   suews-convert \
       -f 2020a \
       -t 2024a \
       -i /path/to/suews_run/Input_v2020a \
       -o /path/to/suews_run/Input_v2024a

This will convert the tables from ``2020a`` to ``2024a`` format, creating the updated tables in the specified output directory.