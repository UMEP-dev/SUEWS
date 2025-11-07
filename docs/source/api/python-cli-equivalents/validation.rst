Configuration Validation
-------------------------

Validating YAML configuration files programmatically.

CLI Command
~~~~~~~~~~~

.. code-block:: bash

    suews-validate config.yml

Python Equivalent (Simple Validation)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Quick schema validation for checking configuration files:

.. code-block:: python

    from pathlib import Path
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.schema.publisher import generate_json_schema

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

Complete validation including all phases (A→B→C) for thorough checking:

.. code-block:: python

    import importlib.resources
    import supy
    from supy.data_model.validation.pipeline.orchestrator import (
        validate_input_file,
        setup_output_paths,
        run_phase_a,
        run_phase_b,
        run_phase_c
    )

    # Validate and get standard config
    user_yaml_file = validate_input_file("config.yml")
    sample_data = importlib.resources.files(supy) / "sample_data"
    with importlib.resources.as_file(sample_data / "sample_config.yml") as path:
        standard_yaml_file = str(path)

    # Setup output paths
    (uptodate_file, report_file,
     science_yaml_file, science_report_file,
     pydantic_yaml_file, pydantic_report_file,
     dirname) = setup_output_paths(user_yaml_file, pipeline="ABC")

    # Run phases A → B → C
    a_ok = run_phase_a(
        user_yaml_file, standard_yaml_file, uptodate_file,
        report_file, mode="public", phase="ABC", silent=False, forcing="on"
    )

    if not a_ok:
        print(f"✗ Phase A failed")
        print(f"  Report: {report_file}")
        print(f"  Updated YAML: {uptodate_file}")
    else:
        b_ok = run_phase_b(
            user_yaml_file, uptodate_file, standard_yaml_file,
            science_yaml_file, science_report_file, report_file,
            phase_a_performed=True, mode="public", phase="ABC", silent=False
        )

        if not b_ok:
            print(f"✗ Phase B failed")
            print(f"  Report: {science_report_file}")
            print(f"  Updated YAML: {uptodate_file}")
        else:
            c_ok = run_phase_c(
                science_yaml_file, pydantic_yaml_file, pydantic_report_file,
                mode="public", phases_run=["A", "B", "C"], silent=False
            )

            if not c_ok:
                print(f"✗ Phase C failed")
                print(f"  Report: {pydantic_report_file}")
                print(f"  Updated YAML: {science_yaml_file}")
            else:
                print(f"✓ All phases passed!")
                print(f"  Report: {pydantic_report_file}")
                print(f"  Updated YAML: {pydantic_yaml_file}")
