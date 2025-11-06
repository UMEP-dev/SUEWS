Schema Management
-----------------

Managing configuration schemas programmatically.

CLI Commands
~~~~~~~~~~~~

.. code-block:: bash

    suews-schema export -o schema.json
    suews-schema validate config.yml
    suews-schema migrate old_config.yml --target-version 2.0

Python Equivalent (Schema Export)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    from supy.data_model.schema.publisher import generate_json_schema, save_schema
    import json
    from pathlib import Path

    # Generate current schema
    schema = generate_json_schema()

    # Save as JSON
    with open("schema.json", "w") as f:
        json.dump(schema, f, indent=2)

    # Or use the built-in save function
    save_schema(schema, Path("suews_schema.json"))

    print("Schema exported successfully!")

Python Equivalent (Schema Validation)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    import yaml
    import jsonschema
    from supy.data_model.schema.publisher import generate_json_schema
    from supy.data_model.core.config import SUEWSConfig

    # Load configuration
    with open("config.yml", "r") as f:
        config = yaml.safe_load(f)

    # Generate schema and validate
    schema = generate_json_schema()

    try:
        # JSON Schema validation
        jsonschema.validate(config, schema)

        # Pydantic model validation (additional checks)
        suews_config = SUEWSConfig(**config)

        print("Configuration is valid!")

    except jsonschema.ValidationError as e:
        print(f"Schema validation error: {e.message}")
    except Exception as e:
        print(f"Model validation error: {e}")

Python Equivalent (Schema Migration)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    import yaml
    from supy.data_model.schema.migration import SchemaMigrator
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION

    # Load old configuration
    with open("old_config.yml", "r") as f:
        old_config = yaml.safe_load(f)

    # Migrate to current version
    migrator = SchemaMigrator()

    # Auto-detect current version
    current_version = migrator.auto_detect_version(old_config)
    print(f"Detected version: {current_version}")

    # Migrate to target version
    migrated_config = migrator.migrate(
        old_config,
        from_version=current_version,
        to_version=CURRENT_SCHEMA_VERSION
    )

    # Save migrated configuration
    with open("migrated_config.yml", "w") as f:
        yaml.dump(migrated_config, f, default_flow_style=False, sort_keys=False)

    print(f"Migration completed: {current_version} → {CURRENT_SCHEMA_VERSION}")

Batch Operations
~~~~~~~~~~~~~~~~

.. code-block:: python

    import yaml
    from pathlib import Path
    from supy.data_model.schema.publisher import generate_json_schema
    from supy.cmd.validate_config import validate_single_file

    # Process multiple configuration files
    config_dir = Path("configurations")
    schema = generate_json_schema()

    results = []
    for config_file in config_dir.glob("*.yml"):
        is_valid, errors = validate_single_file(config_file, schema, show_details=False)
        results.append({
            "file": config_file.name,
            "valid": is_valid,
            "error_count": len(errors)
        })

    # Summary report
    valid_count = sum(1 for r in results if r["valid"])
    total_count = len(results)

    print(f"Validation Summary: {valid_count}/{total_count} files valid")

    for result in results:
        status = "✓" if result["valid"] else "✗"
        print(f"  {status} {result['file']}")
