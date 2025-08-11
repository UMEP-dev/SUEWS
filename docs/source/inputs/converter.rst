.. _input_converter:

SUEWS Format Converter
======================

The ``suews-convert`` tool provides seamless conversion between different SUEWS input formats and versions. It supports:

- **Version migration**: Update older SUEWS input files to newer table formats
- **Format conversion**: Transform legacy table-based inputs to modern YAML format

.. note::
  From v2025a onwards, SUEWS uses YAML format as the primary configuration method. The converter automatically determines the output format based on the target version.

Usage
-----

.. code-block:: shell

   suews-convert -f FROM_VERSION -t TO_VERSION -i INPUT_PATH -o OUTPUT_PATH

The converter automatically determines the conversion type:

- **Versions before 2025** (e.g., 2024a): Table-to-table conversion
- **Version 2025a or later**: Converts to YAML format
- **Same version** (e.g., 2016a to 2016a): Reformats files only (removes inline comments, fixes whitespace, standardises delimiters, organises into correct directory structure)

Parameters
----------

Required Parameters:
~~~~~~~~~~~~~~~~~~~~

- ``-i, --input``: Input directory containing SUEWS files (must have RunControl.nml)
- ``-o, --output``: Output path (directory for table conversion, file path for YAML)

Optional Parameters:
~~~~~~~~~~~~~~~~~~~~

- ``-f, --from``: Source version (e.g., '2020a', '2024a'). If not specified, the converter will auto-detect the version
- ``-t, --to``: Target version. Options include:
  
  - Specific version (e.g., '2024a' for tables, '2025a' for YAML)
  - ``latest`` (default): Converts to the current YAML format
  
- ``-d, --debug-dir``: Directory to save intermediate conversion files for debugging
- ``--no-profile-validation``: Disable automatic profile validation and creation of missing profiles
- ``--force-table``: Force table output format even for 2025a (skip YAML conversion)

Examples
--------

**Auto-detect version and convert to latest YAML:**

.. code-block:: shell

   suews-convert -i your_suews_folder -o config.yml
   # Auto-detects source version and converts to latest YAML format

**Table-to-table conversion (pre-2025):**

.. code-block:: shell

   suews-convert -f 2018a -t 2024a -i your_2018a_folder -o your_2024a_folder

**Table-to-YAML conversion (2025+):**

.. code-block:: shell

   suews-convert -f 2024a -t 2025a -i your_2024a_folder -o config.yml


**Debug intermediate steps:**

.. code-block:: shell

   suews-convert -f 2016a -t latest -i old_data -o config.yml -d debug_output
   # Saves intermediate conversion files in debug_output directory

.. tip:: The converter uses the ``RunControl.nml`` file in your input folder to determine the location of input tables. This ensures that custom paths specified in ``FileInputPath`` are correctly handled.

Version Detection
-----------------

The converter can automatically detect the version of your input files by examining:

- File existence patterns (e.g., ``SUEWS_AnthropogenicEmission.txt`` vs ``SUEWS_AnthropogenicHeat.txt``)
- Column presence/absence in specific tables
- Parameters in ``RunControl.nml`` (for 2024a+)
- Optional files like ``SUEWS_SPARTACUS.nml``

If auto-detection fails, you can specify the source version explicitly with ``-f``.

Path Handling
-------------

The converter respects path configurations in ``RunControl.nml``:

- **Absolute paths**: Used directly as specified
- **Relative paths**: Resolved relative to the input directory
- **Automatic fallback**: If files aren't found at the configured path, the converter checks:
  
  1. The root input directory
  2. The path specified in ``FileInputPath`` 
  3. The ``Input/`` subdirectory

This flexible approach ensures the converter works with various directory structures while respecting user-configured paths.
