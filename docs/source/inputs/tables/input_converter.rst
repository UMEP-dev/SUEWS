.. _input_converter:

SUEWS input converter
********************************

.. note::
  The SUEWS converter has been integrated into SuPy as a command line tool :ref:`suews-convert` since v2020a.
  From v2025a onwards, SUEWS uses YAML format exclusively. The converter automatically determines the output format based on the target version.

Usage
-----

.. code-block:: shell

   suews-convert -f FROM_VERSION -t TO_VERSION -i INPUT_PATH -o OUTPUT_PATH

The converter automatically determines the conversion type:

- **Versions before 2025** (e.g., 2024a): Table-to-table conversion
- **Version 2025a or later**: Converts to YAML format

Parameters
----------

- ``-f, --from``: Source version (e.g., '2020a', '2024a')
- ``-t, --to``: Target version (e.g., '2024a' for tables, '2025a' for YAML)
- ``-i, --input``: Input directory containing SUEWS files (must have RunControl.nml)
- ``-o, --output``: Output path (directory for table conversion, file path for YAML)

Examples
--------

**Table-to-table conversion (pre-2025):**

.. code-block:: shell

   suews-convert -f 2018a -t 2024a -i your_2018a_folder -o your_2024a_folder

**Table-to-YAML conversion (2025+):**

.. code-block:: shell

   suews-convert -f 2024a -t 2025a -i your_2024a_folder -o config.yml

.. tip:: The converter will use the ``RunControl.nml`` file in your input folder to determine the location of input tables.
