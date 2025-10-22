Validation Tool Reference
=========================

The ``suews-validate`` command is a comprehensive validation system that automatically checks and updates SUEWS YAML configuration files. It ensures your configuration is complete, scientifically valid, and compatible with the SUEWS model.

What the Validator Does
-----------------------

The validation system performs multiple checks on your configuration:

- **Completeness Check**: Detects missing parameters, updates deprecated parameter names to current standards and validates forcing data
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

**Final Files (ready to use):**
- ``updated_config.yml`` - Your corrected configuration (ready to use with SUEWS)
- ``report_config.txt`` - Consolidated validation report showing all changes

Understanding Reports
---------------------

The validation report provides comprehensive details about every change made to your configuration. 

.. code-block:: text

    # SUEWS Validation Report
    # ==================================================
    # Mode: Public
    # ==================================================

    ## ACTION NEEDED
    - Found (3) forcing data validation error(s):
    -- Wind speed (`U`) must be >= 0.01 m/s to avoid division by zero errors in atmospheric calculations. 1 values below 0.01 m/s found at line(s): [670]
    -- `rh` should be between [0.0001, 105] but 25 outliers are found at line(s): [5, 118, 156, 157, ...]
    -- `kdown` should be between [0, 1400] but 6 outliers are found at line(s): [176, 406, 655, 693, 847, 1558]
       Required fix: Review and correct forcing data file.
       Suggestion: You may want to plot the time series of your input data.

    Note: Line numbers refer to actual lines in the forcing .txt file (including header)

    - Found (1) critical missing parameter(s):
    -- netradiationmethod has been added to updated YAML and set to null
       Location: model.physics.netradiationmethod

    ## NO ACTION NEEDED
    - Updated (3) optional missing parameter(s) with null values:
    -- holiday added to updated YAML and set to null
    -- wetthresh added to updated YAML and set to null
    -- roughlenmommethod added to updated YAML and set to null

    - Updated (2) renamed parameter(s):
    -- diagmethod changed to rslmethod
    -- cp changed to rho_cp

    - Updated (7) parameter(s):
    -- initial_states.paved: temperature, tsfc, tin → 12.4°C (Set from CRU data for coordinates (51.51, -0.13) for month 1)
    -- initial_states.bldgs: temperature, tsfc, tin → 12.4°C (Set from CRU data for coordinates (51.51, -0.13) for month 1)
    -- anthropogenic_emissions.startdls: 15.0 → 86 (Calculated DLS start for coordinates (51.51, -0.13))

    # ==================================================

**Report Structure:**

The report is organised into two main sections:

- **NO ACTION NEEDED**: Changes that were automatically applied to your configuration and warnings. These are informational and require no further action from you. 

- **ACTION NEEDED**: Critical issues that require your attention before the configuration can be used. 


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
   Some issues need manual intervention. Check the **ACTION NEEDED** section in ``report_config.txt`` for specific issues requiring your attention.

**"Unknown parameter"**
   You may have a typo or be using an outdated configuration format. The validator will suggest corrections for renamed parameters.

For more detailed usage examples and advanced options, always refer to:

.. code-block:: bash

    suews-validate --help