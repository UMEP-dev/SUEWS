.. _schema_cli:

Schema Management CLI
=====================

The ``suews-schema`` command provides a comprehensive command-line interface for managing SUEWS YAML configuration schemas. This unified tool addresses the needs for schema version checking, validation, and migration between different schema versions.

.. note::
   This CLI consolidates functionality from GitHub issues #612 and #613, providing a single entry point for all schema-related operations that will integrate with the future suews-wizard (#544).

Installation
------------

The ``suews-schema`` command is automatically installed with SuPy:

.. code-block:: bash

   pip install supy

Once installed, the command is available system-wide.

Available Commands
------------------

The ``suews-schema`` CLI provides the following subcommands:

.. code-block:: bash

   suews-schema --help              # Show help and available commands
   suews-schema info                 # Display schema version information
   suews-schema version              # Check or update schema versions
   suews-schema validate             # Validate configuration files
   suews-schema migrate              # Migrate between schema versions
   suews-schema export               # Export JSON Schema

Command Reference
-----------------

info
~~~~

Display information about available schema versions and the current version.

.. code-block:: bash

   suews-schema info

This command shows:

- Current schema version
- Available schema versions with descriptions
- Quick reference for other CLI commands

version
~~~~~~~

Check or update schema versions in configuration files.

**Basic usage:**

.. code-block:: bash

   # Check schema version
   suews-schema version config.yml

   # Check multiple files
   suews-schema version configs/*.yml

**Update schema versions:**

.. code-block:: bash

   # Update to current version
   suews-schema version config.yml --update

   # Update to specific version
   suews-schema version config.yml --update --target-version 1.0

   # Update without backup (not recommended)
   suews-schema version config.yml --update --no-backup

**Options:**

- ``--update, -u``: Update schema version in files
- ``--target-version``: Target version for update (default: current)
- ``--backup, -b``: Create backup before updating (default: true)

validate
~~~~~~~~

Validate configuration files against their schema.

**Basic usage:**

.. code-block:: bash

   # Validate single file
   suews-schema validate config.yml

   # Validate multiple files
   suews-schema validate configs/*.yml

**Advanced options:**

.. code-block:: bash

   # Validate against specific schema version
   suews-schema validate config.yml --schema-version 1.0

   # Strict mode (exit with error code on failure)
   suews-schema validate config.yml --strict

   # Different output formats
   suews-schema validate config.yml --format json
   suews-schema validate config.yml --format yaml

**Options:**

- ``--schema-version``: Schema version to validate against
- ``--strict, -s``: Exit with error on validation failure
- ``--format``: Output format (table, json, yaml)

migrate
~~~~~~~

Migrate configuration files between schema versions.

**Basic usage:**

.. code-block:: bash

   # Migrate to current version
   suews-schema migrate old_config.yml

   # Migrate to specific version
   suews-schema migrate config.yml --target-version 2.0

**Batch migration:**

.. code-block:: bash

   # Migrate multiple files to output directory
   suews-schema migrate configs/*.yml --output-dir migrated/

   # Dry run to preview changes
   suews-schema migrate config.yml --dry-run

**Options:**

- ``--target-version``: Target schema version (default: current)
- ``--output-dir, -o``: Output directory for migrated files
- ``--backup, -b``: Keep original files (default: true)
- ``--dry-run, -n``: Show what would be done without doing it

export
~~~~~~

Export the JSON Schema for SUEWS configurations.

**Basic usage:**

.. code-block:: bash

   # Export to file
   suews-schema export -o schema.json

   # Export specific version
   suews-schema export --version 1.0 -o schema-v1.0.json

   # Export as YAML
   suews-schema export --format yaml -o schema.yaml

**Options:**

- ``--output, -o``: Output file for schema
- ``--version``: Schema version to export (default: current)
- ``--format``: Output format (json, yaml)

Global Options
--------------

All commands support these global options:

- ``--verbose, -v``: Enable verbose output for debugging
- ``--quiet, -q``: Suppress non-essential output

Examples
--------

**Example 1: Check and update schema versions**

.. code-block:: bash

   # Check current versions
   suews-schema version configs/*.yml

   # Update all to current version
   suews-schema version configs/*.yml --update

**Example 2: CI/CD validation**

.. code-block:: bash

   # Validate all configs in CI pipeline
   suews-schema validate configs/*.yml --strict

   # Output validation results as JSON for processing
   suews-schema validate configs/*.yml --format json > validation.json

**Example 3: Migration workflow**

.. code-block:: bash

   # Check what needs migration
   suews-schema version old_configs/*.yml

   # Dry run migration
   suews-schema migrate old_configs/*.yml --dry-run

   # Perform migration
   suews-schema migrate old_configs/*.yml --output-dir updated_configs/

   # Validate migrated files
   suews-schema validate updated_configs/*.yml --strict

**Example 4: Schema documentation**

.. code-block:: bash

   # Export current schema for documentation
   suews-schema export -o docs/schema.json

   # Generate human-readable YAML version
   suews-schema export --format yaml -o docs/schema.yaml

Integration with Other Tools
----------------------------

Using with Python
~~~~~~~~~~~~~~~~~

The schema CLI can be called from Python scripts:

.. code-block:: python

   import subprocess
   import json

   # Validate configuration
   result = subprocess.run(
       ['suews-schema', 'validate', 'config.yml', '--format', 'json'],
       capture_output=True, text=True
   )
   validation = json.loads(result.stdout)

   # Check schema version
   result = subprocess.run(
       ['suews-schema', 'version', 'config.yml', '--quiet'],
       capture_output=True, text=True
   )

Using in CI/CD
~~~~~~~~~~~~~~

**GitHub Actions example:**

.. code-block:: yaml

   - name: Validate SUEWS configs
     run: |
       suews-schema validate configs/*.yml --strict

   - name: Check schema versions
     run: |
       suews-schema version configs/*.yml

**Pre-commit hook example:**

.. code-block:: yaml

   repos:
     - repo: local
       hooks:
         - id: validate-suews-config
           name: Validate SUEWS configs
           entry: suews-schema validate
           language: system
           files: '\.yml$'
           pass_filenames: true
           args: ['--strict']

Future Integration with suews-wizard
------------------------------------

The ``suews-schema`` CLI is designed to integrate seamlessly with the upcoming ``suews-wizard`` (issue #544):

- The wizard will use the validation logic to ensure created configurations are valid
- Migration utilities can be called from within the wizard for upgrading existing configs
- Schema version checking will be embedded in the wizard workflow
- The export functionality will provide schema documentation within the wizard

Best Practices
--------------

1. **Always specify schema version**: Include ``schema_version`` in your YAML files
2. **Validate before running**: Use ``suews-schema validate`` before running simulations
3. **Keep backups**: Always backup configurations before migration
4. **Use CI/CD validation**: Integrate validation into your automated workflows
5. **Document versions**: Keep track of which schema version your configs use

Troubleshooting
---------------

**Issue: Command not found**

Ensure SuPy is installed:

.. code-block:: bash

   pip install supy

**Issue: Validation fails but config works**

Check if you're validating against the correct schema version:

.. code-block:: bash

   suews-schema version config.yml
   suews-schema validate config.yml --schema-version <correct_version>

**Issue: Migration fails**

Try step-by-step migration if jumping multiple versions:

.. code-block:: bash

   # Instead of 0.1 -> 2.0 directly
   suews-schema migrate config.yml --target-version 1.0
   suews-schema migrate config.migrated.yml --target-version 2.0

See Also
--------

- :ref:`yaml_config` - YAML configuration documentation
- :ref:`schema_versioning` - Schema versioning details
- :ref:`validation` - Configuration validation guide