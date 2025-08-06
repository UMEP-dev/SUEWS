SUEWS Convert Command Guide
============================

The ``suews-convert`` command provides a unified interface for converting SUEWS input files between different formats.

Overview
--------

The command automatically determines the conversion type based on the target version:

- **Table-to-table conversion**: For target versions before 2025 (e.g., 2020a, 2024a)
- **Table-to-YAML conversion**: For target version 2025a or later

Usage
-----

.. code-block:: bash

   suews-convert -f FROM_VERSION -t TO_VERSION -i INPUT_PATH -o OUTPUT_PATH

Parameters
----------

- ``-f, --from``: Source version to convert from (e.g., '2020a', '2024a')
- ``-t, --to``: Target version (e.g., '2024a' for tables, '2025a' for YAML)
- ``-i, --input``: Input directory containing SUEWS files (must have RunControl.nml)
- ``-o, --output``: Output path (directory for table conversion, file path for YAML)

Examples
--------

Table-to-Table Conversion
~~~~~~~~~~~~~~~~~~~~~~~~~

Convert from 2020a to 2024a format:

.. code-block:: bash

   suews-convert -f 2020a -t 2024a -i /path/to/2020a/input -o /path/to/2024a/output

Table-to-YAML Conversion
~~~~~~~~~~~~~~~~~~~~~~~~

Convert from 2024a tables to 2025a (automatically converts to YAML):

.. code-block:: bash

   suews-convert -f 2024a -t 2025a -i /path/to/tables -o config.yml

Version History
---------------

- **2025a and later**: YAML format is the standard for version 2025a onwards
- **2024a and earlier**: Table-based format using multiple .txt and .nml files

Notes
-----

1. The input directory must contain a ``RunControl.nml`` file
2. For YAML output, if no file extension is provided, ``.yml`` will be added automatically
3. When converting to YAML, the command will:
   
   - First convert tables to the latest table version if needed
   - Then convert the latest table format to YAML
   
4. Table-to-table conversion preserves the directory structure but excludes forcing files and output folders