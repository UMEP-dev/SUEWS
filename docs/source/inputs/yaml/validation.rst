Configuration Validation
========================

The ``suews-validate`` command helps you check and fix your YAML configuration files before running simulations.

Quick Start
-----------

**Check if your configuration is valid:**

.. code-block:: bash

    suews-validate validate config.yml

**Fix common issues automatically:**

.. code-block:: bash

    suews-validate config.yml

This will create a corrected version of your configuration file.

What Does It Do?
----------------

The validator performs three types of checks:

1. **Completeness** - Adds any missing parameters with sensible defaults
2. **Scientific validity** - Fixes unrealistic values (e.g., ensures fractions sum to 1.0)
3. **Compatibility** - Ensures your settings work together

Common Use Cases
----------------

Check Multiple Files
~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Check all configs in a directory
    suews-validate validate configs/*.yml

    # Get results in JSON format for scripts
    suews-validate validate configs/*.yml --format json

Fix Your Configuration
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Run full validation and create fixed version
    suews-validate my_config.yml
    
    # Output: updatedABC_my_config.yml (ready to use)
    #         reportABC_my_config.txt (what was changed)

Quick Check Without Writing Files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Just check, don't create files
    suews-validate --dry-run config.yml

Migrate Old Configurations
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Update old config to latest format
    suews-validate migrate old_config.yml -o new_config.yml

Understanding the Output
------------------------

Success Example
~~~~~~~~~~~~~~~

::

    ✓ Phase A completed - Added 3 missing parameters
    ✓ Phase B completed - Fixed surface fractions (sum=1.0)
    ✓ Phase C completed - Configuration is valid
    
    Output: updatedABC_config.yml
    Report: reportABC_config.txt

The updated file is ready to use with SUEWS.

Failure Example
~~~~~~~~~~~~~~~

::

    ✓ Phase A completed
    ✗ Phase B failed - Invalid building height
    
    See reportB_config.txt for details

Check the report file to see what needs manual fixing.

What Gets Fixed Automatically?
-------------------------------

**Automatic fixes include:**

- Missing parameters get sensible defaults
- Surface fractions adjusted to sum to 1.0
- Initial temperatures set based on location and season
- Vegetation parameters set based on surface type

**Manual fixes needed for:**

- Invalid coordinates
- Physically impossible values (e.g., negative heights)
- Incompatible physics options

Command Reference
-----------------

Basic Commands
~~~~~~~~~~~~~~

.. code-block:: bash

    # Full validation and fixing
    suews-validate config.yml
    
    # Check only (no fixes)
    suews-validate validate config.yml
    
    # Migrate old format
    suews-validate migrate old.yml -o new.yml
    
    # Show schema version
    suews-validate schema info

Validation Options
~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Dry run (check without writing)
    suews-validate --dry-run config.yml
    
    # JSON output for scripts
    suews-validate validate config.yml --format json
    
    # Quiet mode (summary only)
    suews-validate validate config.yml --quiet
    
    # Verbose mode (detailed errors)
    suews-validate validate config.yml --verbose

Advanced Options
~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Run specific validation phases
    suews-validate config.yml --phase A   # Structure check only
    suews-validate config.yml --phase B   # Scientific check only
    suews-validate config.yml --phase C   # Compatibility check only
    
    # Skip scientific corrections
    suews-validate config.yml --phase AC

Tips for Success
----------------

1. **Start with the sample**: Copy ``sample_config.yml`` and modify it
2. **Run validation early**: Check your config before lengthy simulations
3. **Check the reports**: They explain what was changed and why
4. **Keep backups**: Original files are preserved when creating fixed versions

Getting Help
------------

.. code-block:: bash

    # Show help
    suews-validate --help
    
    # Show help for specific command
    suews-validate validate --help

For more details, see the :doc:`/inputs/yaml/index` documentation.

Examples
--------

London Configuration
~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Starting with a minimal config
    $ cat london.yml
    sites:
      - name: London
        lat: 51.5
        lng: -0.1
    
    # Run validation
    $ suews-validate london.yml
    ✓ Phase A completed - Added 47 missing parameters
    ✓ Phase B completed - Set temperatures from climate data
    ✓ Phase C completed - Configuration valid
    
    Created: updatedABC_london.yml (ready to use)

Batch Processing
~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Check all configurations
    for config in configs/*.yml; do
        if suews-validate validate "$config" --quiet; then
            echo "✓ $config"
        else
            echo "✗ $config - needs attention"
        fi
    done

CI/CD Integration
~~~~~~~~~~~~~~~~~

.. code-block:: bash

    # In GitHub Actions or CI pipeline
    suews-validate validate config.yml --format json > results.json
    
    # Check exit code
    if [ $? -eq 0 ]; then
        echo "Configuration valid"
    else
        echo "Configuration has issues"
        exit 1
    fi