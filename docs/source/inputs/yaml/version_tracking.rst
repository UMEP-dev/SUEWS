YAML Configuration Version Tracking
====================================

.. note::
   Version tracking was introduced in SUEWS v2025.8.15 to help manage configuration compatibility across different model versions.

Overview
--------

SUEWS YAML configurations now support version tracking to help identify and debug compatibility issues when using configuration files across different model versions. This feature provides:

- Clear identification of which model version a configuration was designed for
- Automatic compatibility checking with warnings when mismatches are detected
- Schema version tracking to manage configuration format evolution
- Tools for updating version information in configuration files

Version Fields
--------------

Two optional fields can be added to the root level of any SUEWS YAML configuration:

.. code-block:: yaml

   name: sample_config_v1.0
   version: 2025.8.15.dev325          # Model version this config is designed for
   config_version: v1.0                # Configuration schema version
   description: Sample config v1.0 designed for supy version 2025.8.15.dev325
   model:
     # ... model configuration ...
   sites:
     # ... site configuration ...

**version**
   The SUEWS/SuPy model version this configuration was designed for (e.g., ``2025.8.15.dev325``). 
   This helps identify potential compatibility issues when running with different model versions.

**config_version**
   The configuration schema version (e.g., ``v1.0``, ``v1.1``). This tracks the evolution of the 
   configuration format itself, independent of the model version.

Version Compatibility Checking
-------------------------------

When loading a YAML configuration, SUEWS automatically:

1. **Logs version information** if present in the configuration
2. **Checks compatibility** between the configuration's target version and the running model version
3. **Issues warnings** when version mismatches are detected

Example warning message:

.. code-block:: text

   WARNING: Version mismatch: Config designed for 2025.7.2.dev106, running with 2025.8.15.dev325. 
   This may cause compatibility issues.

These warnings help identify when a configuration might not work as expected due to version differences.

Managing Version Information
----------------------------

Automatic Version Updates
~~~~~~~~~~~~~~~~~~~~~~~~~

A utility script is provided to update version information in YAML files:

.. code-block:: bash

   # Update a single configuration file
   python -m supy.util.update_config_version config.yml --model-version 2025.8.15.dev325

   # Auto-increment the config version
   python -m supy.util.update_config_version config.yml --auto-increment

   # Update all sample configs in the repository
   python -m supy.util.update_config_version --all-sample-configs

Command-line Options
~~~~~~~~~~~~~~~~~~~~

``--model-version VERSION``
   Set the model version (uses current git version if not specified)

``--config-version VERSION``
   Set the configuration schema version (e.g., v1.0, v1.1)

``--auto-increment``
   Automatically increment the config version number

``--update-name``
   Update the name field to include version information

``--all-sample-configs``
   Update all sample configuration files in the repository

Best Practices
--------------

1. **Always include version fields** in production configurations to aid debugging
2. **Update the model version** when creating configurations for a new SUEWS release
3. **Increment config_version** when making structural changes to the configuration format
4. **Document version requirements** in your project documentation
5. **Test configurations** with the target model version before deployment

Version History
---------------

Configuration Schema Versions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**v1.0** (2025.8.15)
   Initial version with formal version tracking support

Migration Guide
---------------

For existing configurations without version fields:

1. Add the version fields manually:

   .. code-block:: yaml

      version: 2025.8.15.dev325  # Your target model version
      config_version: v1.0        # Start with v1.0 for existing configs

2. Or use the update utility:

   .. code-block:: bash

      python -m supy.util.update_config_version your_config.yml

3. Test the configuration with version checking enabled to ensure compatibility

Troubleshooting
---------------

**"Version mismatch" warnings**
   These warnings indicate the configuration was designed for a different model version.
   While the configuration may still work, review the changelog for breaking changes
   between versions.

**Missing version fields**
   Configurations without version fields will work normally but won't benefit from
   compatibility checking. Consider adding version fields for better maintainability.

**Finding the current model version**
   Run ``python -c "import supy; print(supy.__version__)"`` to see your installed version.