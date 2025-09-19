Validation Tool Reference
=========================

The ``suews-validate`` command checks and fixes YAML configuration issues automatically.

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

The report shows all changes made:

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