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

Required arguments:

*   ``-i, --input PATH``: The directory containing ``RunControl.nml``. The converter will read the ``FileInputPath`` parameter from this file to locate the actual table files (e.g., if ``FileInputPath="./Input/"``, it will look for tables in ``input_dir/Input/``).
*   ``-o, --output PATH``: The output path:
    - For table conversion (pre-2025): Directory for the converted tables
    - For YAML conversion (2025+): Path for the output YAML file

Optional arguments:

*   ``-f, --from VERSION``: The version of your source input files (e.g., ``2020a``, ``2024a``). If not specified, the tool will auto-detect the version.
*   ``-t, --to VERSION``: The target version. Options include:
    - Specific version (e.g., ``2024a`` for tables, ``2025a`` for YAML)
    - ``latest`` (default): Converts to the current YAML format
*   ``-d, --debug-dir PATH``: Directory to save intermediate conversion files for debugging.
*   ``--no-profile-validation``: Disable automatic profile validation and creation of missing profiles.
*   ``--force-table``: Force table output format even for 2025a (skip YAML conversion).

Examples
--------

Example 1: Auto-detect and Convert to Latest YAML
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The simplest way to convert your files to YAML format - let the tool detect the version automatically:

.. code-block:: bash

   suews-convert \
       -i /path/to/suews_run_london \
       -o /path/to/new_config/config.yml

The tool will:
1. Read ``RunControl.nml`` from the input directory
2. Auto-detect the version of your input files
3. Find table files using the path specified in ``FileInputPath`` (e.g., ``./Input/``)
4. Convert them to the latest YAML format
5. Create ``config.yml`` in the specified output location

Example 2: Converting to YAML with Explicit Versions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you know your source version and want to explicitly specify it:

.. code-block:: bash

   suews-convert \
       -f 2024a \
       -t 2025a \
       -i /path/to/old_runs/london_2024a \
       -o /path/to/yaml_configs/london.yml

Note: The input path should contain ``RunControl.nml``. The converter will read ``FileInputPath`` from it to locate the table files.

Example 3: Converting Older Tables to YAML
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have input files from an older SUEWS version (e.g., ``2019b``), you can convert them directly to YAML:

.. code-block:: bash

   suews-convert \
       -f 2019b \
       -t latest \
       -i /path/to/archive/2019_runs/site_v2019b \
       -o /path/to/updated_configs/site_2019.yml

The tool will:
1. Read ``RunControl.nml`` from the input directory
2. Find table files (typically in ``Input/`` subdirectory as specified by ``FileInputPath``)
3. Update the tables from ``2019b`` through intermediate versions
4. Convert to YAML format

Example 4: Table-to-Table Conversion (Pre-2025)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For converting between table versions without creating YAML, use a target version before 2025:

.. code-block:: bash

   suews-convert \
       -f 2020a \
       -t 2024a \
       -i /path/to/suews_run/Input_v2020a \
       -o /path/to/suews_run/Input_v2024a

This will convert the tables from ``2020a`` to ``2024a`` format, creating the updated tables in the specified output directory.

Example 5: Debugging Conversion Issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you encounter issues during conversion, use the debug directory option to inspect intermediate files:

.. code-block:: bash

   suews-convert \
       -f 2016a \
       -t latest \
       -i /path/to/legacy_runs/2016a_site \
       -o /path/to/yaml_output/site_config.yml \
       -d /tmp/suews_debug

This saves all intermediate conversion steps in the debug directory (``/tmp/suews_debug``), allowing you to identify where issues occur in the conversion chain. The input directory should contain ``RunControl.nml``.

Version Auto-Detection
~~~~~~~~~~~~~~~~~~~~~~

The converter can automatically detect the version of your input files by analysing:

- File naming patterns (e.g., ``SUEWS_AnthropogenicEmission.txt`` vs ``SUEWS_AnthropogenicHeat.txt``)
- Column headers in specific tables
- Parameters present in ``RunControl.nml``
- Presence of optional files like ``SUEWS_SPARTACUS.nml``

If auto-detection fails, you'll be prompted to specify the source version explicitly using the ``-f`` option.

Path Resolution and File Location
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The converter intelligently handles various directory structures by reading the ``FileInputPath`` parameter from your ``RunControl.nml`` file:

- **Configured paths**: The converter respects custom paths specified in ``RunControl.nml``
- **Absolute paths**: Used directly as specified (e.g., ``/home/user/data/inputs/``)
- **Relative paths**: Resolved relative to the input directory (e.g., ``./Input/`` becomes ``input_dir/Input/``)
- **Automatic fallback**: If files aren't found at the configured path, the converter automatically checks:
  
  1. The root input directory
  2. The path specified in ``FileInputPath``
  3. The ``Input/`` subdirectory
  
This ensures compatibility with various SUEWS installation structures while respecting user configurations.

Troubleshooting
~~~~~~~~~~~~~~~

**Common Issues and Solutions:**

1. **"Could not auto-detect version"**
   
   - Ensure your input directory contains ``RunControl.nml``
   - Check that your SUEWS table files are present
   - Specify the source version explicitly with ``-f``

2. **"Missing required files"**
   
   - Verify that all required SUEWS table files are present
   - Check the ``FileInputPath`` setting in ``RunControl.nml``
   - Ensure files are in the expected directory structure

3. **"Profile validation errors"**
   
   - The converter automatically creates missing profiles
   - Use ``--no-profile-validation`` to skip this step if needed
   - Check that profile IDs in tables match those in ``SUEWS_Profiles.txt``

4. **"Conversion chain failed"**
   
   - Use ``-d debug_dir`` to save intermediate files
   - Check the debug directory to identify which conversion step failed
   - Report issues with the specific version transition that failed