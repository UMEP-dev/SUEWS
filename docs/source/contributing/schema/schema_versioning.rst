YAML Configuration Schema Versioning
=====================================

.. note::
   Schema versioning was introduced in SUEWS v2025.8 to track configuration structure changes independently of model versions.

Schema Versioning Overview
--------------------------

SUEWS YAML configurations use a **schema version** to track the structure and format of configuration files. This versioning system:

- **Tracks structure only**: Schema versions change when fields are added, removed, or renamed
- **Independent of model versions**: SUEWS can be updated without changing the schema
- **Enables migration**: Provides clear paths for updating older configurations
- **Simplifies compatibility**: One version number instead of complex version matrices

Key Concept
-----------

**Schema version ≠ SUEWS version**

The schema version (e.g., ``1.0``) tracks the configuration structure, while SUEWS versions (e.g., ``2025.8.15``) track the model code. A single schema version may work with many SUEWS releases.

Schema Version Field
--------------------

Add the ``schema_version`` field to your configuration:

.. code-block:: yaml

   name: my_urban_config
   schema_version: "1.0"    # Configuration structure version
   description: Urban climate simulation for central London
   model:
     # ... model configuration ...
   sites:
     # ... site configuration ...

If no schema version is specified, SUEWS assumes the current schema version.

Schema Version Policy
---------------------

Schema versions follow a simple **major.minor** format:

**Minor Version Changes (1.0 → 1.1)**
   - New optional fields added
   - Backward compatible
   - Old configs work without modification
   - Example: Adding optional ``new_parameter`` field

**Major Version Changes (1.0 → 2.0)**
   - Breaking changes (field renames, restructuring)
   - Migration required
   - Clear migration documentation provided
   - Example: Renaming ``old_field`` to ``new_field``

.. important::
   Schema versions change rarely (perhaps once per year), unlike SUEWS versions which may have hundreds of development builds.

Compatibility Checking
----------------------

SUEWS automatically checks schema compatibility when loading configurations:

**Compatible Versions**
   No warnings - configuration loads normally

**Older Schema**
   .. code-block:: text

      WARNING: Configuration uses older schema 0.9, current is 1.0. 
      Consider updating your configuration.

**Newer Schema**
   .. code-block:: text

      WARNING: Configuration uses newer schema 2.0, this version supports 1.0. 
      Please update SUEWS or use an older configuration.

Migration
---------

When schema versions change, migration tools help update your configurations:

Command-Line Migration
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   # Migrate a single file
   python -m supy.util.schema_migration config.yml

   # Migrate to specific version
   python -m supy.util.schema_migration config.yml --to-version 1.0

Versioning Python API
~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from supy.util.schema_migration import migrate_config_file
   
   # Migrate to current schema
   migrate_config_file('old_config.yml', 'new_config.yml')
   
   # Check if migration needed
   from supy.util.schema_migration import check_migration_needed
   if check_migration_needed('config.yml'):
       print("Configuration needs migration")

Managing Schema Versions
------------------------

Update schema versions in your configurations:

.. code-block:: bash

   # Set to current schema version
   python -m supy.util.update_schema_version config.yml --current
   
   # Set specific version
   python -m supy.util.update_schema_version config.yml --schema-version 1.0
   
   # Update all configs in directory
   python -m supy.util.update_schema_version --directory ./configs --current

Version History
---------------

**Schema 1.0** (2025.8)
   Initial YAML schema with full Pydantic data model. Includes all parameters from the table-based format in a hierarchical structure.

Versioning Best Practices
-------------------------

1. **Let SUEWS handle it**: If you don't specify a schema version, SUEWS assumes the current version
2. **Check compatibility**: Use migration tools when updating old configurations
3. **Don't modify manually**: Use the provided tools to update schema versions
4. **Document your version**: When sharing configs, note the SUEWS version tested with

FAQ
---

**Q: Do I need to add schema_version to my configs?**
   No, it's optional. SUEWS assumes the current version if not specified.

**Q: How often do schema versions change?**
   Rarely - perhaps once per year for minor updates, less often for major changes.

**Q: What if I use the wrong schema version?**
   SUEWS will warn you and may still work if changes are minor. Use migration tools for major differences.

**Q: Is this the same as the SUEWS model version?**
   No, schema versions track configuration structure. Model versions track SUEWS code changes.