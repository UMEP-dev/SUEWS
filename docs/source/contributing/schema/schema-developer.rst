Schema Development Documentation
=================================

.. note::
   This documentation is for developers working on SUEWS schema implementation. 
   Users should refer to the main :doc:`index` for configuration guidance.

Schema Versioning
-----------------

SUEWS uses schema versioning to track configuration structure changes:

- Schema versions (e.g., ``1.0``) track configuration structure
- SUEWS versions (e.g., ``2025.8.15``) track model code
- Schema changes only when fields are added, removed, or renamed

Version Policy
~~~~~~~~~~~~~~

**Minor Version Changes (1.0 → 1.1)**
   - New optional fields added
   - Backward compatible
   - Old configs work without modification

**Major Version Changes (1.0 → 2.0)**
   - Breaking changes (renamed/removed fields)
   - Requires migration
   - Clear migration path provided

Schema Management CLI
---------------------

For developers, the ``suews-schema`` command provides schema management:

.. code-block:: bash

   suews-schema info                 # Display schema information
   suews-schema version files/*.yml  # Check schema versions
   suews-schema migrate old.yml      # Migrate between versions
   suews-schema export               # Export JSON Schema

Schema Publishing
-----------------

Schemas are published as JSON Schema files for:

- External validation tools
- IDE autocomplete
- API documentation generation
- CI/CD pipeline validation

Access schemas at:

- GitHub Pages: ``https://umep-dev.github.io/SUEWS/schemas/suews-config/``
- Raw GitHub: ``https://raw.githubusercontent.com/UMEP-dev/SUEWS/master/schemas/suews-config/latest.json``

Generate schemas locally:

.. code-block:: bash

   python .github/scripts/generate_schema.py

Python API
----------

.. code-block:: python

   from supy.data_model.schema.publisher import generate_json_schema
   from supy.data_model.schema.migration import SchemaMigrator
   
   # Generate schema
   schema = generate_json_schema(version='1.0')
   
   # Migrate configuration
   migrator = SchemaMigrator()
   new_config = migrator.migrate(old_config, to_version='1.0')

Implementation Details
----------------------

The schema system is implemented in:

- ``src/supy/data_model/schema/`` - Core schema implementation
- ``src/supy/data_model/validation/`` - Validation pipeline
- ``src/supy/cmd/validate_config.py`` - CLI interface

For technical details, see the source code and inline documentation.