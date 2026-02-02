.. _api_converter:

Configuration Converter API
============================

.. currentmodule:: supy.util.converter

The converter module provides Python functions for converting SUEWS configurations between different formats and versions.

Key Features
------------

- **Format Conversion**: Convert table-based inputs to YAML configuration
- **Version Migration**: Update table files between SUEWS versions
- **Auto-detection**: Automatically detect input format and version
- **Python Integration**: Direct Python API for tool integration (e.g., QGIS plugins)

For CLI usage, see :doc:`/inputs/converter`.

Functions
---------

Table to YAML Conversion
~~~~~~~~~~~~~~~~~~~~~~~~~

.. autofunction:: convert_to_yaml

Table Version Conversion
~~~~~~~~~~~~~~~~~~~~~~~~~

.. autofunction:: convert_table

Utility Functions
~~~~~~~~~~~~~~~~~

.. autofunction:: detect_input_type

.. autofunction:: detect_table_version

Quick Examples
--------------

**Convert table-based input to YAML**:

.. code-block:: python

    from supy.util.converter import convert_to_yaml

    # Auto-detect version and convert
    convert_to_yaml(
        input_file='path/to/RunControl.nml',
        output_file='config.yml'
    )

    # Specify version explicitly
    convert_to_yaml(
        input_file='path/to/RunControl.nml',
        output_file='config.yml',
        from_ver='2024a'
    )

**Convert between table versions**:

.. code-block:: python

    from supy.util.converter import convert_table

    # Convert from old to new version
    convert_table(
        fromDir='path/to/old_data',
        toDir='path/to/new_data',
        fromVer='2016a',
        toVer='2024a'
    )

**Auto-detect input format**:

.. code-block:: python

    from supy.util.converter import detect_input_type, detect_table_version
    from pathlib import Path

    # Detect input type
    input_path = Path('path/to/RunControl.nml')
    input_type = detect_input_type(input_path)  # Returns 'nml'

    # Detect table version
    if input_type == 'nml':
        version = detect_table_version(input_path.parent)
        print(f"Detected version: {version}")

Error Handling
--------------

The converter functions raise informative exceptions on errors:

.. code-block:: python

    from supy.util.converter import convert_to_yaml
    from pathlib import Path

    try:
        convert_to_yaml(
            input_file='path/to/RunControl.nml',
            output_file='config.yml'
        )
    except ValueError as e:
        # Input validation errors
        print(f"Input error: {e}")
    except FileNotFoundError as e:
        # Missing files
        print(f"File not found: {e}")
    except Exception as e:
        # Other conversion errors
        print(f"Conversion failed: {e}")

Related Documentation
---------------------

- :doc:`/inputs/converter` - Command-line usage guide
- :doc:`/inputs/transition_guide` - Transition guide to YAML format
- :doc:`/inputs/yaml/index` - YAML configuration reference
- :doc:`simulation` - SUEWSSimulation class for running simulations
