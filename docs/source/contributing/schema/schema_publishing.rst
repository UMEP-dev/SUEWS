Schema Publishing and Validation
=================================

SUEWS publishes machine-readable JSON Schema files that enable:

- External validation tools
- IDE autocomplete and validation  
- API documentation generation
- Configuration linting in CI/CD pipelines

Schema Files
------------

Published schemas are available in the ``schemas/suews-config/`` directory and via GitHub Pages:

.. code-block:: text

   schemas/
   └── suews-config/
       ├── 0.1.json        # Version 0.1 schema
       ├── latest.json     # Always points to current version
       ├── registry.json   # Version registry metadata
       └── index.html      # Web interface for schema browsing

Access Methods
--------------

Online Access
~~~~~~~~~~~~~

Schemas are available via GitHub Pages:

- **Schema Registry**: ``https://umep-dev.github.io/SUEWS/schemas/suews-config/``
- **Latest Version**: ``https://umep-dev.github.io/SUEWS/schemas/suews-config/latest.json``
- **Version 0.1**: ``https://umep-dev.github.io/SUEWS/schemas/suews-config/0.1.json``

Or via raw GitHub:

- Latest: ``https://raw.githubusercontent.com/UMEP-dev/SUEWS/master/schemas/suews-config/latest.json``
- Version 0.1: ``https://raw.githubusercontent.com/UMEP-dev/SUEWS/master/schemas/suews-config/0.1.json``

Local Access
~~~~~~~~~~~~

After installing SUEWS:

.. code-block:: bash

   # Generate schema locally (standalone, no build required)
   python .github/scripts/generate_schema.py
   
   # Or using the data model directly
   python -m supy.data_model.schema.publisher generate schema.json

Validation Tools
----------------

Command-Line Validation
~~~~~~~~~~~~~~~~~~~~~~~

SUEWS provides a user-friendly validation tool:

.. code-block:: bash

   # Quick check with detailed feedback
   suews-validate check my_config.yml
   
   # Validate multiple files
   suews-validate validate *.yml
   
   # Check if migration needed
   suews-validate migrate old_config.yml --output new_config.yml

Publishing Python API
~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from supy.util.schema_publisher import validate_config_against_schema
   
   # Validate configuration
   is_valid = validate_config_against_schema('my_config.yml')
   
   # Or with specific schema version
   is_valid = validate_config_against_schema(
       'my_config.yml',
       version='0.1'
   )

External Tools
~~~~~~~~~~~~~~

Using ``jsonschema`` CLI:

.. code-block:: bash

   pip install jsonschema
   jsonschema -i config.yml schemas/suews-config/latest.json

Using ``yajsv`` (Yet Another JSON Schema Validator):

.. code-block:: bash

   # Install: https://github.com/neilpa/yajsv
   yajsv -s schemas/suews-config/latest.json config.yml

IDE Integration
---------------

Visual Studio Code
~~~~~~~~~~~~~~~~~~

1. Install the YAML extension by Red Hat
2. Add to workspace settings (``.vscode/settings.json``):

.. code-block:: json

   {
     "yaml.schemas": {
       "./schemas/suews-config/latest.json": ["*.yml", "*.yaml"],
       "https://umep-dev.github.io/SUEWS/schemas/suews-config/latest.json": ["suews-*.yml"],
       "$schema": "https://umep-dev.github.io/SUEWS/schemas/suews-config/0.1.json"
     }
   }

Features:
- Autocomplete for all fields
- Inline validation errors
- Hover documentation
- Schema-aware formatting

PyCharm / IntelliJ IDEA
~~~~~~~~~~~~~~~~~~~~~~~~

1. Go to **Settings** → **Languages & Frameworks** → **Schemas and DTDs** → **JSON Schema Mappings**
2. Click **+** to add new mapping:
   
   - Name: ``SUEWS Configuration``
   - Schema file: ``schemas/suews-config/latest.json``
   - File path pattern: ``*.yml``

Sublime Text
~~~~~~~~~~~~

Using the LSP-yaml package:

1. Install LSP and LSP-yaml packages
2. Configure in LSP-yaml settings:

.. code-block:: json

   {
     "settings": {
       "yaml.schemas": {
         "./schemas/suews-config/latest.json": "*.yml"
       }
     }
   }

Vim/Neovim
~~~~~~~~~~

Using coc-yaml:

1. Install coc.nvim and coc-yaml
2. Add to ``:CocConfig``:

.. code-block:: json

   {
     "yaml.schemas": {
       "./schemas/suews-config/latest.json": "*.yml"
     }
   }

Schema CI/CD Integration
------------------------

GitHub Actions
~~~~~~~~~~~~~~

.. code-block:: yaml

   name: Validate Configurations
   
   on: [push, pull_request]
   
   jobs:
     validate:
       runs-on: ubuntu-latest
       steps:
       - uses: actions/checkout@v4
       
       - name: Set up Python
         uses: actions/setup-python@v4
         with:
           python-version: '3.11'
       
       - name: Install SUEWS
         run: pip install supy
       
       - name: Validate configurations
         run: |
           suews-validate validate configs/*.yml

GitLab CI
~~~~~~~~~

.. code-block:: yaml

   validate-configs:
     image: python:3.11
     script:
       - pip install supy
       - suews-validate validate configs/*.yml
     only:
       changes:
         - configs/*.yml

Pre-commit Hook
~~~~~~~~~~~~~~~

Add to ``.pre-commit-config.yaml``:

.. code-block:: yaml

   repos:
   - repo: local
     hooks:
     - id: validate-suews-config
       name: Validate SUEWS configs
       entry: suews-validate validate
       language: system
       files: \.yml$

Schema Generation
-----------------

Automatic Generation
~~~~~~~~~~~~~~~~~~~~

Schemas are automatically generated and published when:

1. Changes are pushed to ``src/supy/data_model/``
2. New release tags are created
3. Manual workflow dispatch in GitHub Actions

Manual Generation
~~~~~~~~~~~~~~~~~

Generate schemas locally:

.. code-block:: python

   from supy.util.schema_publisher import generate_json_schema, save_schema
   
   # Generate current schema
   schema = generate_json_schema()
   
   # Save to file
   save_schema(
       'my-schema.json',
       version='0.1',
       include_internal=False  # Exclude internal fields
   )
   
   # Create complete bundle
   from supy.data_model.schema.publisher import create_schema_bundle
   create_schema_bundle('./my-schemas/', version='0.1')

Schema Contents
~~~~~~~~~~~~~~~

Generated schemas include:

- All configuration fields with types
- Field descriptions and constraints
- Required vs optional fields
- Default values
- Validation rules (min/max, patterns, etc.)
- Examples

Advanced Usage
--------------

Custom Validation Rules
~~~~~~~~~~~~~~~~~~~~~~~

Extend schema with custom rules:

.. code-block:: python

   import json
   from supy.util.schema_publisher import generate_json_schema
   
   # Generate base schema
   schema = generate_json_schema()
   
   # Add custom rules
   schema['properties']['sites']['minItems'] = 1
   schema['properties']['sites']['maxItems'] = 100
   
   # Add custom patterns
   schema['properties']['name']['pattern'] = '^[a-zA-Z0-9_-]+$'
   
   # Save extended schema
   with open('custom-schema.json', 'w') as f:
       json.dump(schema, f, indent=2)

Schema Composition
~~~~~~~~~~~~~~~~~~

Combine schemas for complex validations:

.. code-block:: json

   {
     "$schema": "https://json-schema.org/draft/2020-12/schema",
     "allOf": [
       {"$ref": "schemas/latest/schema.json"},
       {"$ref": "my-custom-rules.json"}
     ]
   }

Programmatic Validation
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   import jsonschema
   import yaml
   from supy.util.schema_publisher import generate_json_schema
   
   # Load configuration
   with open('config.yml') as f:
       config = yaml.safe_load(f)
   
   # Generate schema for specific version
   schema = generate_json_schema(version='0.1')
   
   # Create validator with format checking
   validator = jsonschema.Draft7Validator(
       schema,
       format_checker=jsonschema.FormatChecker()
   )
   
   # Validate and collect all errors
   errors = list(validator.iter_errors(config))
   
   for error in errors:
       print(f"Path: {' > '.join(str(p) for p in error.path)}")
       print(f"Error: {error.message}")

Publishing Best Practices
-------------------------

1. **Version Control**: Always specify ``schema_version`` in configurations
2. **CI/CD Validation**: Validate all configs in CI pipelines
3. **IDE Integration**: Set up schema validation in your editor
4. **Pre-commit Hooks**: Validate before committing changes
5. **Documentation**: Document any custom validation rules
6. **Migration Path**: Plan for schema evolution and migrations

Publishing Troubleshooting
--------------------------

Common Issues
~~~~~~~~~~~~~

**"No schema version specified"**
   Add ``schema_version: "0.1"`` to your configuration

**"Additional properties are not allowed"**
   Remove fields not defined in the schema or check for typos

**"Required field missing"**
   Check the schema documentation for required fields

**IDE not showing autocomplete**
   Ensure schema file path is correct in IDE settings

Schema Publishing Help
~~~~~~~~~~~~~~~~~~~~~~

- Schema documentation: :doc:`schema_versioning`
- GitHub issues: https://github.com/UMEP-dev/SUEWS/issues
- Schema files: ``schemas/`` directory