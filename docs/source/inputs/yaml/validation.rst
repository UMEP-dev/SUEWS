Validation Tool Reference
=========================

The ``suews-validate`` command is a comprehensive three-phase validation system that automatically checks, fixes, and optimises SUEWS YAML configuration files. It ensures your configuration is complete, scientifically valid, and compatible with the SUEWS model.

## What the Validator Does

**Phase A: Completeness & Structure**
  - Detects missing parameters
  - Updates deprecated parameter names to current standards
  - Preserves your custom parameter values while ensuring completeness

**Phase B: Scientific Validation**
  - Performs automatic scientific corrections
  - Applies scientific constraints and physical relationships
  - Initialises temperatures using CRU climatological data based on location and season
  - Validates physics options compatibility

**Phase C: Model Compatibility**
  - Validates against Pydantic data models
  - Applies conditional validation rules based on physics settings
  - Ensures configuration compatibility with SUEWS computational engine
  - Performs final consistency checks

Command Reference
-----------------

Basic Commands
~~~~~~~~~~~~~~

.. code-block:: bash

    # Validate and fix (creates corrected file)
    suews-validate config.yml

    # Check only (no changes)
    suews-validate validate config.yml

    # Check without writing files (read-only validation)
    suews-validate --dry-run config.yml

Output Options
~~~~~~~~~~~~~~

.. code-block:: bash

    # Human-readable table (default)
    suews-validate validate config.yml
    
    # Machine-readable JSON for CI/CD
    suews-validate validate config.yml --format json
    
    # Quiet mode (summary only)
    suews-validate validate config.yml --quiet
    
    # Verbose mode (detailed errors)
    suews-validate validate config.yml --verbose

Advanced Usage
~~~~~~~~~~~~~~

.. code-block:: bash

    # Run specific validation phases
    suews-validate --pipeline A config.yml     # Phase A: Structure only
    suews-validate --pipeline B config.yml     # Phase B: Science only
    suews-validate --pipeline C config.yml     # Phase C: Compatibility only
    suews-validate --pipeline AB config.yml    # Phases A+B: Structure + Science
    suews-validate --pipeline ABC config.yml   # All phases (default)

    # Public mode (default)
    suews-validate --pipeline ABC config.yml
    suews-validate --mode public --pipeline ABC config.yml

    # Developer mode (enables experimental features)
    suews-validate --mode dev --pipeline ABC config.yml

    # Migrate old configuration format
    suews-validate migrate old.yml -o new.yml

    # Check schema version
    suews-validate version config.yml

Output Files
------------

When you run ``suews-validate config.yml``, it creates:

- ``updatedABC_config.yml`` - Your corrected configuration (ready to use)
- ``reportABC_config.txt`` - Detailed report of what was changed

Understanding Reports
---------------------

The validation report provides comprehensive details about every change made to your configuration, organised by phase:

**Phase A Report Structure**

.. code-block:: text

    # SUEWS - Phase A (Up-to-date YAML check) Report
    # ==============================================
    # Mode: Public
    # ==============================================

    ## NO ACTION NEEDED
    - Updated (29) optional missing parameter(s) with null values:
    -- forcing_file added to the updated YAML and set to null
    -- output_file added to the updated YAML and set to null
    -- lat added to the updated YAML and set to null
    -- lng added to the updated YAML and set to null
    -- timezone added to the updated YAML and set to null
    -- physics added to the updated YAML and set to null
    -- land_cover added to the updated YAML and set to null
    -- initial_states added to the updated YAML and set to null

    # =================================================

**Phase B Report Structure**

.. code-block:: text

    # SUEWS - Phase B (Scientific Validation) Report
    # ==================================================
    # Mode: Public
    # ==================================================

    ## NO ACTION NEEDED
    - Updated (24) parameter(s):
    -- initial_states.paved at site [0]: temperature, tsfc, tin → 4.8°C
       (Set from CRU data for coordinates (51.51, -0.12) for month 1)
    -- initial_states.bldgs at site [0]: temperature, tsfc, tin → 4.8°C
    -- initial_states.grass at site [0]: temperature, tsfc, tin → 4.8°C

    - Revise (2) warnings:
    -- land_cover.evetr at site [0]: Parameters not checked because 'evetr' surface fraction is 0
    -- land_cover.bsoil at site [0]: Parameters not checked because 'bsoil' surface fraction is 0

    # =================================================

**Phase C Report Structure**

.. code-block:: text

    # SUEWS - Phase C (Pydantic Validation) Report
    # ============================================
    # Mode: Public
    # ============================================

    Phase C passed

    # ==================================================

**Report Categories Explained**

- **ACTION NEEDED**: Critical issues requiring your attention (missing physics parameters, forbidden locations)
- **NO ACTION NEEDED**: Informational items automatically handled (optional parameters, allowed customizations)
- **AUTOMATIC CORRECTIONS**: Scientific adjustments applied (surface fractions, temperatures)
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
          cat results.json | jq '.results[].errors'
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

JSON Output Format
~~~~~~~~~~~~~~~~~~

.. code-block:: json

    {
      "status": "success",
      "summary": {
        "total_files": 1,
        "valid_files": 1,
        "total_errors": 0
      },
      "results": [{
        "file": "config.yml",
        "valid": true,
        "errors": []
      }]
    }

For detailed JSON output documentation including error codes and CI/CD examples, see :doc:`/contributing/json-output-integration`.

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