Configuration Conversion
-------------------------

Converting between SUEWS input formats programmatically.

CLI Command
~~~~~~~~~~~

.. code-block:: bash

    suews-convert -i input_dir/RunControl.nml -o config.yml
    suews-convert -i df_state.csv -o config.yml

Python Equivalent
~~~~~~~~~~~~~~~~~~

.. code-block:: python

    from supy.util.converter import convert_to_yaml
    from pathlib import Path

    # Convert table-based SUEWS input to YAML
    convert_to_yaml(
        input_file="input_dir/RunControl.nml",
        output_file="config.yml",
        from_ver=None,  # Auto-detect version
        debug_dir=None,  # No debug files
        validate_profiles=True  # Validate profiles automatically
    )

    # Convert df_state format to YAML
    convert_to_yaml(
        input_file="df_state.csv",
        output_file="config_from_state.yml",
        from_ver=None,  # Not used for df_state
        validate_profiles=True
    )

    print("Conversion completed successfully!")

Advanced Usage with Debugging
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    from supy.util.converter import (
        convert_to_yaml,
        detect_table_version,
        detect_input_type
    )
    from pathlib import Path
    import f90nml

    input_path = Path("RunControl.nml")

    # Detect input type and version
    input_type = detect_input_type(input_path)
    print(f"Input type: {input_type}")

    if input_type == "nml":
        # Read RunControl.nml to get the input directory path
        nml = f90nml.read(str(input_path))
        file_input_path = Path(nml['runcontrol']['fileinputpath'])

        # Handle both absolute and relative paths
        if file_input_path.is_absolute():
            input_dir = file_input_path
        else:
            input_dir = input_path.parent / file_input_path

        version = detect_table_version(input_dir)
        print(f"Detected table version: {version}")

    # Convert with debug directory to keep intermediate files
    convert_to_yaml(
        input_file=str(input_path),
        output_file="config.yml",
        debug_dir="./debug_conversion",  # Keep intermediate files
        validate_profiles=False  # Skip profile validation if needed
    )
