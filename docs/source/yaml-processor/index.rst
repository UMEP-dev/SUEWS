SUEWS Configuration Validation
===============================

The SUEWS validator ensures your configuration files are complete, scientifically valid, and ready to run.

Quick Start
-----------

**Validate and fix your configuration:**

.. code-block:: bash

    suews-validate config.yml

This creates a corrected configuration file that's ready to use with SUEWS.

How It Works
------------

The validator performs three checks on your configuration:

.. list-table::
   :header-rows: 1
   :widths: 20 30 50

   * - Phase
     - What It Does
     - Example Fixes
   * - **A: Completeness**
     - Adds missing parameters
     - Adds default soil depths, building heights
   * - **B: Scientific**
     - Fixes unrealistic values
     - Adjusts fractions to sum to 1.0, sets realistic temperatures
   * - **C: Compatibility**
     - Ensures settings work together
     - Checks physics options match, validates dependencies

Common Tasks
------------

Validate Without Changes
~~~~~~~~~~~~~~~~~~~~~~~~~

Check your configuration without modifying files:

.. code-block:: bash

    suews-validate validate config.yml

Fix Your Configuration
~~~~~~~~~~~~~~~~~~~~~~~

Create a corrected version with all issues fixed:

.. code-block:: bash

    suews-validate config.yml
    # Creates: updatedABC_config.yml (fixed version)
    # Creates: reportABC_config.txt (what changed)

Check Multiple Files
~~~~~~~~~~~~~~~~~~~~

Validate all configurations in a directory:

.. code-block:: bash

    suews-validate validate *.yml

Get JSON Output for Scripts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    suews-validate validate config.yml --format json

What Gets Fixed Automatically?
-------------------------------

**Automatic Fixes:**

- Missing parameters filled with appropriate defaults
- Surface cover fractions adjusted to sum to exactly 1.0
- Initial temperatures set based on your location and season (using climate data)
- Vegetation parameters set appropriately for surface types
- Physical parameters checked for reasonable ranges

**Manual Fixes Required:**

- Invalid latitude/longitude coordinates
- Physically impossible values (negative heights, fractions > 1)
- Incompatible model options

Understanding the Output
------------------------

After running validation, you'll see:

**Success:**
::

    ✓ Phase A completed - Structure validated
    ✓ Phase B completed - Science checks passed  
    ✓ Phase C completed - Model compatibility verified
    
    Output saved to: updatedABC_config.yml

Your configuration is ready to use!

**Issues Found:**
::

    ✓ Phase A completed - Added 5 missing parameters
    ✓ Phase B completed - Fixed 2 scientific issues
    ✓ Phase C completed - Configuration now valid
    
    See report for details: reportABC_config.txt

Check the report to understand what was changed.

Report Example
--------------

The report shows exactly what was changed:

::

    PHASE A - STRUCTURE CHECK
    ========================
    Added missing parameters:
    - soil.soil_depth: [0.1, 0.25, 0.5, 0.75] (default depths)
    - bldgs.bldgh: 10.0 (default building height)
    
    PHASE B - SCIENTIFIC VALIDATION
    ===============================
    Automatic corrections:
    - Surface fractions adjusted from 0.98 to 1.00
    - Initial temperatures set to 15.2°C (July average for London)
    
    PHASE C - COMPATIBILITY CHECK
    ============================
    ✓ All physics options compatible
    ✓ Configuration valid for SUEWS

Command Options
---------------

Basic Usage
~~~~~~~~~~~

.. code-block:: bash

    # Full validation with fixes
    suews-validate config.yml
    
    # Check only (no changes)
    suews-validate validate config.yml
    
    # Check without writing files
    suews-validate --dry-run config.yml

Output Formats
~~~~~~~~~~~~~~

.. code-block:: bash

    # Human-readable table (default)
    suews-validate validate config.yml
    
    # Machine-readable JSON
    suews-validate validate config.yml --format json
    
    # Quiet mode (summary only)
    suews-validate validate config.yml --quiet

Advanced Options
~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Run specific phases
    suews-validate config.yml --phase A    # Structure only
    suews-validate config.yml --phase B    # Science only
    suews-validate config.yml --phase AB   # Structure + Science
    
    # Migrate old configuration
    suews-validate migrate old.yml -o new.yml

Best Practices
--------------

1. **Start with the sample configuration** and modify it for your site
2. **Validate early and often** during configuration development  
3. **Review the reports** to understand what was changed
4. **Keep your original files** - the validator creates new versions

Getting Help
------------

- Run ``suews-validate --help`` for command options
- See :doc:`/cli/validation-guide` for detailed usage examples
- Check :doc:`/inputs/yaml/index` for parameter documentation

For Developers
--------------

If you need technical details about the validation system:

- :doc:`processor_detailed` - Implementation details
- :doc:`module_structure` - Code organization
- `GitHub repository <https://github.com/UMEP-dev/SUEWS>`_ - Source code

.. toctree::
   :maxdepth: 2
   :caption: Technical Documentation
   :hidden:

   processor_detailed
   module_structure
   index-detailed
   namelist_to_yaml_conversion
   ../cli/json-output