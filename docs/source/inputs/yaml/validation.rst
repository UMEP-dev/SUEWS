Validation Tool Reference
=========================

The ``suews-validate`` command is a comprehensive validation system that automatically checks, fixes, and optimises SUEWS YAML configuration files. It ensures your configuration is complete, scientifically valid, and compatible with the SUEWS model.

What the Validator Does
-----------------------

The validation system performs multiple checks on your configuration:

- **Completeness Check**: Detects missing parameters and updates deprecated parameter names to current standards
- **Scientific Validation**: Applies automatic scientific corrections and validates physics options compatibility
- **Model Compatibility**: Ensures configuration compatibility with SUEWS computational engine

Basic Usage
-----------

.. code-block:: bash

    # Validate and fix configuration (creates corrected file)
    suews-validate config.yml

    # Check configuration without making changes
    suews-validate validate config.yml

    # Check without writing files (read-only validation)
    suews-validate --dry-run config.yml

For complete usage options and advanced features, use:

.. code-block:: bash

    # View all available options and commands
    suews-validate --help

    # View help for specific subcommands
    suews-validate validate --help
    suews-validate migrate --help
    suews-validate version --help

Output Files
------------

When you run ``suews-validate config.yml``, it creates:

- ``updated_config.yml`` - Your corrected configuration (ready to use)
- ``report_config.txt`` - Detailed report of what was changed

Understanding Reports
---------------------

The validation report provides comprehensive details about every change made to your configuration:

.. code-block:: text

    # SUEWS Validation Report
    # =======================
    # Mode: Public
    # =======================

    ## NO ACTION NEEDED
    - Updated (3) optional missing parameter(s) with null values:
    -- holiday added to updated YAML and set to null
    -- wetthresh added to updated YAML and set to null
    -- roughlenmommethod added to updated YAML and set to null

    - Updated (2) renamed parameter(s):
    -- diagmethod changed to rslmethod
    -- cp changed to rho_cp

    - Updated (11) parameter(s):
    -- initial_states.paved: temperature, tsfc, tin → 12.4°C (Set from CRU data for coordinates (51.51, -0.13) for month 1)
    -- initial_states.bldgs: temperature, tsfc, tin → 12.4°C (Set from CRU data for coordinates (51.51, -0.13) for month 1)
    -- anthropogenic_emissions.startdls: 15.0 → 86 (Calculated DLS start for coordinates (51.51, -0.13))
    -- paved.sfr at site [0]: rounded to achieve sum of land cover fractions equal to 1.0

    ## ACTION NEEDED
    - Found (1) critical missing parameter(s):
    -- netradiationmethod has been added to updated YAML and set to null
       Suggested fix: Set appropriate value based on SUEWS documentation

    - Found (2) critical scientific parameter error(s):
    -- rslmethod-stabilitymethod: If rslmethod == 2, stabilitymethod must be 3
       Suggested fix: Set stabilitymethod to 3
    -- storageheatmethod-ohmincqf: StorageHeatMethod is set to 1 and OhmIncQf is set to 1. You should switch to OhmIncQf=0.
       Suggested fix: Set OhmIncQf to 0

    # =================================================

**Report Categories:**

- **ACTION NEEDED**: Critical issues requiring your attention
- **NO ACTION NEEDED**: Informational items automatically handled
- **VALIDATION RESULTS**: Final compatibility and consistency checks

Exit Codes
----------

For scripting and CI/CD:

- ``0`` - Configuration is valid (or was successfully fixed)
- ``1`` - Validation failed (manual fixes needed)
- ``2`` - Invalid command or file not found

CI/CD Integration
-----------------

GitHub Actions Example
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: yaml

    - name: Validate SUEWS Configuration
      run: |
        suews-validate validate config.yml --format json > results.json
        if [ $? -ne 0 ]; then
          echo "Configuration validation failed"
          exit 1
        fi

Batch Processing
~~~~~~~~~~~~~~~~

.. code-block:: bash

    #!/bin/bash
    # Validate all configurations
    for config in configs/*.yml; do
        if suews-validate validate "$config" --quiet; then
            echo "✓ $config"
        else
            echo "✗ $config - needs attention"
        fi
    done

Troubleshooting
---------------

**"Command not found"**
   Install SuPy: ``pip install supy``

**"File not found"**
   Check the file path and ensure the file exists

**"Validation failed after fixes"**
   Some issues need manual intervention. Check the report for details.

**"Unknown parameter"**
   You may have a typo or be using an outdated configuration format.

For more detailed usage examples and advanced options, always refer to:

.. code-block:: bash

    suews-validate --help