:doc-source: hand-written

Configuration Validation
-------------------------

Validating YAML configuration files programmatically.

CLI Command
~~~~~~~~~~~

.. code-block:: bash

    suews validate config.yml

Python Equivalent (Simple Validation)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Quick schema validation for checking configuration files:

.. code-block:: python

    from pathlib import Path
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.configuration.publisher import generate_json_schema

    # Generate the current schema
    schema = generate_json_schema()

    # Validate a single file
    is_valid, errors = validate_single_file(
        Path("config.yml"),
        schema,
        show_details=True
    )

    if not is_valid:
        for error in errors:
            print(f"Error: {error}")
    else:
        print("Configuration is valid!")

Python Equivalent (Full Validation Pipeline)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Complete in-memory validation across all phases (A, B, and C) with zero disk
writes using :func:`supy.validate_config`:

.. code-block:: python

    import supy

    # Validate a YAML file, dict, or SUEWSConfig purely in memory
    report = supy.validate_config("config.yml")

    if not report.is_valid:
        for error in report.errors:
            print(f"[{error.code}] {error.yaml_path}: {error.message}")
    else:
        print("Configuration is valid!")

    # Render the canonical text report
    print(report.to_text())

    # Pass write_outputs=True to also publish updated_config.yml and reports
    report = supy.validate_config("config.yml", write_outputs=True)
